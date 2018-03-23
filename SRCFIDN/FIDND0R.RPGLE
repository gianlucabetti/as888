000100040930     H DECEDIT('0,') DATEDIT(*DMY.)
000200040930      *----------------------------------------------------*
000300040930      *  Statistica Richiesta Assistenza Clienti           *
000400040930      *----------------------------------------------------*
000500040930     fFidnd0d   cf   e             workstn
000600041014     fTabel00f  if   e           k disk
000700141204     f******Azute01l  if   e           k disk
000800041018     fAzorg01l  if   e           k disk
000900041014     fAzorg02l  if   e           k disk    rename(azorg:azorg02)
001000040930
001100041014      *---------------------------------------------------------------*
001200040930
001300041014      *
001400041014      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
001500041014      *
001600041014
001700050520     d msg             s             78    dim(25) ctdata perrcd(1)             msg video
001800041020     d l6              S              3  0 dim(30)
001900041020     d poa             S              3  0 dim(250)
002000040930
002100041014      *
002200041014      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002300041014      *
002400040930
002500040930     d Kpjba         e ds
002600050520     d ds5a2         e ds
002700140130     d dMAR          e ds                  inz
002800040930
002900041014      *
003000041014      * - Parametri x Controllo profilo utenti
003100041014     d TIBS34DS      e ds                  inz
003200041014      * - Ds di riferimento al file esterno AZUTE00F
003300041014     d AZUTEDS       e ds                  extname(AZUTE00F)
003400041014      * - Ds per dati organigramma
003500041014     d DDatiUte      e ds
003600041018      * - Tabelle
003700041018     d TIBS02ds      e ds                  inz
003800131018     d tntbMARds1    e ds                  inz
003900041019      * - Parametri per gestione reclami già esistenti
004000041019     d FIDND0DS      e ds
004100041019      * DS per TRUL06R - Caricamento £X
004200041019     d TRUL06DS      E DS
004300041019     d  LIN                    1     90  0
004400041019     d                                     DIM(30)
004500041020      * DS per TRUL31R - Caricamento P.O. abilitati
004600041020     d TRUL31DS      E DS
004700041020     d  OPG                   10    759  0
004800041020     d                                     DIM(250)
004900140925
005000140925      // - Tabella COR = Tipo comunicazione
005100140925     d dCOR          e ds                  inz
005200041014      *
005300041014     d WLBdat          ds                  inz
005400041014     d  G02dat                 1      8  0 inz
005500041014     d  G02inv                 9     16  0 inz
005600041014     d  G02err                17     17    inz('3')
005700041014     d  G02tgi                18     22  0 inz
005800050523     D WGIDAT          DS
005900050523     D  GIODAT                 1      8  0
006000050523     D  GIOINV                 9     16  0
006100050523     D  GIOTGI                17     21  0
006200041014      *
006300041014      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
006400041014      *
006500041014
006600041014      * - Campi di comodo
006700041014     d w0140           s             14  0
006800041014     d wData           s              8  0
006900041014     d DateU           s              8  0
007000041019     d codut           s              1  0 inz(1)
007100041015     d §Kut            s                   like(codut)
007200041015     d §Cod            s                   like(TBLcod)
007300041015     d §Key            s                   like(TBLkey)
007400041015     d Cod             s                   like(TBLcod)
007500041015     d Key             s                   like(TBLkey)
007600041019     d wfil            s                   like(ORGfil)
007700041015     d ware            s                   like(ORGcar)
007800041018     d wamgdcd         s                   like(V1cdcd)
007900041018     d wamgdca         s                   like(V1cdcd)
008000041018     d wamgdhd         s                   like(V1cdcd)
008100041018     d wamgdha         s                   like(V1cdcd)
008200041018     d wrk_cor         s                   like(v1ctc1)
008300041018     d wrk_mar         s                   like(v1cma1)
008400041018     d wrk_chr         s                   like(v1ccc1)
008500141204     d W_ute           s                   like(v1cugr1)
008600141204     d W_utemax        s                   like(v1cugr1)
008700141204     d Wrk_ute         s                   like(v1cugr1)
008800041018     d Wlute           s              3  0
008900141204     d W_utegen        s               n   inz(*off)
009000050523     d sav_tgi         s                   like(g02tgi)
009100141204
009200141204      //---------------------------------------------------------------
009300141204      //?Definizione Prototipi.
009400141204      //---------------------------------------------------------------
009500141204      /copy gaitrasrc/srcprotopr,TIBS02R
009600141204
009700041014      *------------------------------------------------------------------------*
009800041014      * INDICATORI
009900041014      *------------------------------------------------------------------------*
010000041014      *  01   - on - sono in sede    off - sono in filiale
010100041014      *  06   - 2 livello
010200041014      *  28   - emetto campo messaggio
010300041014      *  30/31- comodo
010400141204      *  40/71- errori posizionamento cursore sui campi
010500041014      *  90   - riemissione videata per errore o ricerca
010600041014      *---------------------------------------------------------------*
010700041014
010800041014      * imposto i campi iniziali
010900041014
011000041014      * sono in sede
011100041014     c                   If        *in01
011200041014     c                   exsr      sr_inzsede
011300041014
011400041014     c                   else
011500041014      * sono in filiale
011600041014     c                   exsr      sr_inzfilia
011700041014     c                   endif
011800041014      *
011900041014      * in sede
012000041014     c                   dow       *in01
012100041014      *
012200041014     c                   exfmt     dnd0d00
012300041019     c                   setoff                                       2890
012400041014      *
012500041014      * f3 fine lavoro
012600041014     c                   if        *inkc
012700041014     c                   leave
012800041014     c                   endif
012900041014      *
013000041014      * controllo dati del video
013100041014      *
013200041015     c                   exsr      sr_ctrd00
013300041014
013400041014     c                   if        *in90
013500041014     c                   iter
013600041014     c                   endif
013700041015      * imposto alcuni della seconda filiale
013800041015     c                   exsr      SR_inzfilia
013900041014      * gestione della seconda videata
014000041014     c                   exsr      sr_gesvidfil
014100041014      *
014200041015      * se viene dato F3 o F6 nella seconda videata vado a fine
014300041014      *
014400041025     c                   if        *inkc  or *inkf
014500041014     c                   leave
014600041014     c                   endif
014700041014      *
014800041014     c                   enddo
014900041014      *
015000041014      *
015100041014      * in filiale
015200041014     c                   if        not *in01
015300041014      *
015400041014     c                   exsr      sr_gesvidfil
015500041014     c                   endif
015600041014      *
015700041014     c                   seton                                        lr
015800041014      *
015900041014      *---------------------------------------------------------------*
016000041014      * Inizializzo i campi della videata di sola sede                *
016100041014      *---------------------------------------------------------------*
016200041014     c     SR_inzsede    BegSr
016300041014      *
016400041014      * imposto i campi del video richieste di sede
016500041014      *
016600041014     c                   eval      v1cord = '2'
016700041014     c                   eval      v1csap = ' '
016800041014     c                   eval      v1ctoi = 'N'
016900041014     c                   eval      v1ccrp = 'N'
017000121116     c                   eval      v1ccrptch = 'N'
017100041014     c                   clear                   v1care
017200041014     c                   clear                   v1dare
017300041014     c                   clear                   v1cdis
017400041014     c                   clear                   v1ddis
017500041014      *
017600041014     c                   endsr
017700041014      *---------------------------------------------------------------*
017800041014      * Inizializzo i campi della videata di sede / sola filiale      *
017900041014      *---------------------------------------------------------------*
018000041014     c     SR_inzfilia   BegSr
018100041014      *
018200041014      * imposto i campi del video richieste di sede / sola filiale
018300041014      *
018400041015      * se non sono in sede
018500041015     c                   If        not *in01
018600041015      *
018700041014     c                   eval      v1cord = '2'
018800041014     c                   eval      v1csap = ' '
018900041014     c                   eval      v1ctsn = 'N'
019000041015      *
019100041015     c                   endif
019200041015      *
019300041014     c                   clear                   v1cprs
019400041014     c                   clear                   v1dprs
019500041014     c                   clear                   v1caap
019600041014     c                   clear                   v1cfil
019700041014     c                   clear                   v1dfil
019800041014     c                   clear                   v1csre
019900041014     c                   clear                   v1cdcd
020000041014     c                   clear                   v1cdca
020100041014     c                   clear                   v1cdhd
020200041014     c                   clear                   v1cdha
020300041014     c                   clear                   v1ctc1
020400041014     c                   clear                   v1ctc2
020500041014     c                   clear                   v1ctc3
020600041014     c                   clear                   v1ctc4
020700041014     c                   clear                   v1cma1
020800041014     c                   clear                   v1cma2
020900041014     c                   clear                   v1cma3
021000041014     c                   clear                   v1cma4
021100041014     c                   clear                   v1cma5
021200041014     c                   clear                   v1cma6
021300041014     c                   clear                   v1cma7
021400041014     c                   clear                   v1cma8
021500041014     c                   clear                   v1ccc1
021600041014     c                   clear                   v1ccc2
021700041014     c                   clear                   v1ccc3
021800041014     c                   eval      v1csra = ' '
021900041014     c                   clear                   v1cxgg
022000141204     c                   clear                   v1cugr1
022100141204     c                   clear                   v1cugr2
022200141204     c                   clear                   v1cugr3
022300141204     c                   clear                   v1cugr4
022400141204     c                   clear                   v1cugr5
022500041014      *
022600041014     c                   endsr
022700041014      *---------------------------------------------------------------*
022800041014      * Controllo i campi della videata di sola sede                 *
022900041014      *---------------------------------------------------------------*
023000041015     c     SR_ctrd00     BegSr
023100041014      *
023200041015     c                   do
023300041019
023400041019     c                   setoff                                       4041
023500041015      *
023600041015      * Cod Distretto se elezionato
023700041015     c                   clear                   v1ddis
023800041015     c                   clear                   v2ddis
023900041015     c     '?'           scan      v1cdis                                 90
024000041015     c                   if        *in90
024100041015     c                   movel     codut         §kut
024200041015     c                   movel     '17'          §cod
024300041015     c                   clear                   §key
024400041015     c                   call      'X§TABER'
024500041015     c                   parm                    §kut
024600041015     c                   parm                    §cod
024700041015     c                   parm                    §key
024800041015     c                   parm                    §des             25
024900041015     c                   movel     §key          v1cdis
025000041015     c                   movel     §des          v1ddis
025100041015     c                   movel     §des          v2ddis
025200041015     c                   seton                                        40
025300041015     c                   leave
025400041015     c                   endif
025500041015      *
025600041015     c     v1cdis        ifne      *blanks
025700041015     c                   movel     '17'          cod
025800041015     c                   movel(p)  v1cdis        key
025900041015     c     ktab          chain     tabel00f
026000041015     c                   if        not %found(tabel00f) OR tblflg <> ' '
026100041015     c                   movel     msg(1)        v1cmsg
026200041015     c                   seton                                        289040
026300041015     c                   leave
026400041015     c                   endif
026500041015      *
026600041015     c                   movel     tbluni        v1ddis
026700041015     c                   movel     tbluni        v2ddis
026800041015      *
026900041015      * controllo se esiste in organigramma
027000041015      *
027100041015     c     v1cdis        chain     azorg02l
027200041015     c                   dow       %found(azorg02l) and orgfva<>' '
027300041015     c     v1cdis        reade     azorg02l
027400041015     c                   enddo
027500041015      *
027600041015     c                   if        not %found(azorg02l)
027700041015     c                   movel     msg(2)        v1cmsg
027800041015     c                   seton                                        289040
027900041015     c                   leave
028000041015     c                   endif
028100041015     c                   endif
028200041015      *
028300041015      * Cod Area      se elezionato
028400041015      *
028500041015     c                   clear                   v1dare
028600041015     c                   clear                   v2dare
028700041015     c     '?'           scan      v1care                                 90
028800041015     c                   if        *in90
028900041015     c                   movel     codut         §kut
029000041015     c                   movel     '05'          §cod
029100041015     c                   clear                   §key
029200041015     c                   call      'X§TABER'
029300041015     c                   parm                    §kut
029400041015     c                   parm                    §cod
029500041015     c                   parm                    §key
029600041015     c                   parm                    §des
029700041015     c                   movel     §key          v1care
029800041015     c                   movel     §des          v1dare
029900041015     c                   movel     §des          v2dare
030000041015     c                   seton                                        41
030100041015     c                   leave
030200041015     c                   endif
030300041015      *
030400041015     c     v1care        ifeq      *zeros
030500041015     c                   clear                   v1care
030600041015     c                   endif
030700041015      *
030800041015     c     v1care        ifne      *blanks
030900041015     c     v1care        andne     *zeros
031000041015     c                   movel     '05'          cod
031100041015     c                   movel(p)  v1care        key
031200041015     c     ktab          chain     tabel00f
031300041015     c                   if        not %found(tabel00f) OR tblflg<>' '
031400041015     c                   movel     msg(3)        v1cmsg
031500041015     c                   seton                                        289041
031600041015     c                   leave
031700041015     c                   endif
031800041015      *
031900041015     c                   movel     tbluni        v1dare
032000041015     c                   movel     tbluni        v2dare
032100041015      *
032200041015     c     v1care        ifle      *zeros
032300041015     c                   clear                   ware
032400041015     c                   else
032500041015     c                   movel     v1care        ware
032600041015     c                   endif
032700041015     c
032800041015     c                   if        v1cdis<>' '
032900041015     c     Korg          chain     azorg02l
033000041015     c                   dow       %found(azorg02l) and orgfva<>' '
033100041015     c     Korg          reade     azorg02l
033200041015     c                   enddo
033300041015      *
033400041015     c                   if        not %found(azorg02l)
033500041015     c                   movel     msg(4)        v1cmsg
033600041015     c                   seton                                        289041
033700041015     c                   leave
033800041015     c                   endif
033900041015     c
034000041015     c                   else
034100041015     c     *loval        setll     azorg02l
034200041015     c                   read      azorg02l
034300041015     c                   dow       not %eof(azorg02l)  and v1cdis=*blanks
034400041015     c                   if        orgcar<>ware
034500041015     c     korg1         setgt     azorg02l
034600041015     c                   else
034700041015     c     orgfva        ifeq      *blanks
034800041015     c                   movel     orgfl3        v1cdis
034900041015     c                   endif
035000041015     c                   endif
035100041015     c                   read      azorg02l
035200041015     c                   enddo
035300041015
035400041015      * non trovata area sull'organigramma
035500041015     c                   if        v1cdis=' '
035600041015     c                   clear                   v1cdis
035700041015     c                   movel     msg(5)        v1cmsg
035800041015     c                   seton                                        289041
035900041015     c                   leave
036000041015     c                   endif
036100041015     c
036200041015     c* Decodifico codice distretto
036300041015     c                   movel     '17'          cod
036400041015     c                   movel(p)  v1cdis        key
036500041015     c     ktab          chain     tabel00f
036600041015     c                   if        %found(tabel00f)
036700041015     c                   movel     tbluni        v1ddis
036800041015     c                   movel     tbluni        v2ddis
036900041015     c                   seton                                        90
037000041015     c                   endif
037100041015
037200041015     c                   endif
037300041015     c                   endif
037400041015
037500041015     c                   leave
037600041015
037700041015     c                   enddo
037800041015
037900041015     c                   endsr
038000041018      *---------------------------------------------------------------*
038100041018      * Gestione videata sia della sede che della filiale             *
038200041018      *---------------------------------------------------------------*
038300041018     c     SR_gesvidfil  BegSr
038400041018      *
038500041018     c                   do        *hival
038600041018      *
038700041018     c                   exfmt     dnd0d01
038800041019
038900041019     c                   setoff                                       2890
039000041018      *
039100041018      * per F12 o F03 gesco da ciclo
039200041018      *
039300041018     c                   If        *inkl or *inkc
039400041018     c                   leave
039500041018     c                   endif
039600041018      *
039700041018      * controllo i dati inseriti
039800041018      *
039900141204     c                   clear                   fidnd0ds
040000041018     c                   exsr      SR_ctrd01
040100041018      * se ci sono errori riemetto la videata
040200041018     c                   if        *in90
040300041018     c                   iter
040400041018     c                   endif
040500041018      *
040600041018      * F6 di conferma lancio la statistica
040700041018      *
040800041018     c                   If        *inKF
040900041018     c                   exsr      SR_conferma
041000041018     c                   leave
041100041018     c                   endif
041200041018      *
041300041018     c                   enddo
041400041018      *
041500041018     c                   endsr
041600041015      *---------------------------------------------------------------*
041700041015      * Controllo i campi della seconda videata                       *
041800041015      *---------------------------------------------------------------*
041900041015     c     SR_ctrd01     BegSr
042000041015      *
042100041015     c                   do
042200041019
042300041019     c                   setoff                                       404142
042400041019     c                   setoff                                       434445
042500041019     c                   setoff                                       464748
042600041019     c                   setoff                                       495051
042700041019     c                   setoff                                       525354
042800041019     c                   setoff                                       555657
042900041019     c                   setoff                                       585960
043000041019     c                   setoff                                       616263
043100041130     c                   setoff                                       646566
043200141204     c                   setoff                                       676869
043300141204     c                   setoff                                       7071
043400041018
043500041018      * P.O. Responsabile
043600041015     c                   clear                   v1dprs
043700041022     c                   If        v1cprs > 0
043800041022      * se distretto e/o area valorizzato e ordinamento 2 dare errore
043900041022     c                   if        v1cord = '2' and (v1cdis<>*blank
044000041022     c                             or v1care<>*blank)
044100041022     c                   seton                                        409028
044200041022     c                   movel     MSG(22)       v1cmsg
044300041022     c                   leave
044400041022     c                   endif
044500041022
044600041015     c                   eval      wfil=v1cprs
044700041015     c                   exsr      CTRPO
044800041018
044900041015     c   90              seton                                        40
045000041018     c   90              leave
045100041018
045200041015     c                   eval      v1dprs=orgdes
045300041018
045400041018      * Se sono in sede e selezionato distretto o area, controllo se
045500041018      * congruente
045600041018     c                   if        Dutlpo='S' and v1cord='2'
045700041018      * Distretto
045800041015     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
045900041015     c                   seton                                        409028
046000041018     c                   movel     MSG(7)        v1cmsg
046100041018     c                   leave
046200041015     c                   endif
046300041018
046400041015     c                   movel     orgcar        w003a             3
046500041018      * Areaa
046600041015     c                   if        w003a<>v1care  and v1care<>*blanks
046700041018     c                   seton                                        409028
046800041018     c                   movel     MSG(8)        v1cmsg
046900041018     c                   leave
047000041015     c                   endif
047100041018
047200041015     c                   endif
047300041018
047400041015     c                   endif
047500041015     c
047600041130
047700041130      * Solo aperti da altro P.O.
047800041130     c                   If        v1caap <> ' '  and v1cord <> '2'
047900041130     c                   seton                                        649028
048000041130     c                   movel     MSG(24)       v1cmsg
048100041130     c                   leave
048200041130     c                   endif
048300041015
048400041018      * P.O. Apertura Reclamo
048500041015     c                   clear                   v1dfil
048600041015     c                   if        v1cfil>0
048700041022      * se distretto e/o area valorizzato e ordinamento 1 dare errore
048800041022     c                   if        v1cord = '2' and (v1cdis<>*blank
048900041022     c                             or v1care<>*blank)
049000041022     c                   seton                                        419028
049100041022     c                   movel     MSG(22)       v1cmsg
049200041022     c                   leave
049300041022     c                   endif
049400041022
049500041015     c                   eval      wfil=v1cfil
049600041015     c                   exsr      CTRPO
049700041015     c   90              seton                                        41
049800041018     c   90              leave
049900041018
050000041015     c                   eval      v1dfil=orgdes
050100041018
050200041018      * Se sono in sede e selezionato distretto o area controllo se
050300041018      * congruente
050400041018     c                   if        Dutlpo='S' and v1cord='1'
050500041018      * Distretto
050600041015     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
050700041015     c                   seton                                        419028
050800041018     c                   movel     MSG(7)        v1cmsg
050900041018     c                   leave
051000041015     c                   endif
051100041018
051200041015     c                   movel     orgcar        w003a             3
051300041018      * Areaa
051400041015     c                   if        w003a<>v1care  and v1care<>*blanks
051500041015     c                   seton                                        419028
051600041018     c                   movel     MSG(8)        v1cmsg
051700041018     c                   leave
051800041015     c                   endif
051900041018
052000041015     c                   endif
052100041018
052200041015     c                   endif
052300041020      *
052400041020      * in caso di ordinamento 1 è oggbligatorio inserire il P.O. apertura
052500041020      *
052600041021     c                   if        dutlpo<>'S' and v1cord='1' and v1cfil=0
052700041021     c                   seton                                        412890
052800041020     c                   movel     MSG(18)       v1cmsg
052900041020     c                   leave
053000041020     c                   endif
053100041020      *
053200041020      * in caso di ordinamento 2 è oggbligatorio inserire il P.O. responsabile
053300041020      *
053400041021     c                   if        dutlpo<>'S' and v1cord='2' and v1cprs=0
053500041021     c                   seton                                        402890
053600041020     c                   movel     MSG(18)       v1cmsg
053700041020     c                   leave
053800041020     c                   endif
053900041020      *
054000041020      * verifico che almeno uno dei due P.O. sia stato inserito se sono in filiale
054100041020      *
054200041020     c                   if        v1cfil = 0 and v1cprs = 0 and dutlpo <> 'S'
054300041020     c                   seton                                        402890
054400041020     c                   movel     MSG(20)       v1cmsg
054500041020     c                   leave
054600041020     c                   endif
054700041020      * verifico che almeno uno dei due P.O. inserito faccia parte della mia area
054800041020      * se sono in filiale e indipendentemente dal tipo ordinamento
054900041021     c                   if        dutlpo <> 'S' and ((v1cfil>0 and v1cprs>0) or
055000041021     c                             (v1cfil>0 and v1cprs=0) or (V1cfil=0 and
055100041021     c                              v1cprs>0))
055200041021     c                   If        v1cfil>0 and v1cprs>0
055300041021     c     v1cfil        lookup    opg                                    30
055400041020     c                   if        not %found
055500041021     c     v1cprs        lookup    opg                                    30
055600041020     c                   if        not %found
055700041020     c                   seton                                        402890
055800041020     c                   movel     MSG(21)       v1cmsg
055900041020     c                   leave
056000041020     c                   endif
056100041020     c                   endif
056200041020     c                   endif
056300041021
056400041021     c                   If        v1cfil>0 and v1cprs=0
056500041021     c     v1cfil        lookup    opg                                    30
056600041021     c                   if        not %found
056700041021     c                   seton                                        412890
056800041021     c                   movel     MSG(21)       v1cmsg
056900041021     c                   leave
057000041021     c                   endif
057100041021     c                   endif
057200041021
057300041021     c                   If        v1cprs>0 and v1cfil=0
057400041021     c     v1cprs        lookup    opg                                    30
057500041021     c                   if        not %found
057600041021     c                   seton                                        402890
057700041021     c                   movel     MSG(21)       v1cmsg
057800041021     c                   leave
057900041021     c                   endif
058000041021     c                   endif
058100041021
058200041021     c                   endif
058300041018
058400041130
058500041130      * Solo estero
058600041130     c                   If        v1csre <> ' '  and v1cord <> '1'
058700041130     c                   seton                                        659028
058800041130     c                   movel     MSG(23)       v1cmsg
058900041130     c                   leave
059000041130     c                   endif
059100041130
059200041018      * Data Richiesta Assistenza
059300041018
059400041018     c                   if        v1cdcd = 0 and v1cdca > 0
059500041018     c                   seton                                        422890
059600041018     c                   movel     MSG(9)        v1cmsg
059700041018     c                   leave
059800041018     c                   endif
059900041018
060000041018     c                   clear                   wamgdcd
060100041018     c                   clear                   wamgdca
060200041018      * Da data
060300041018     c                   if        v1cdcd > 0
060400041018     c                   move      V1cdcd        g02dat
060500041018     c                   eval      g02err = *blanks
060600041018     c                   call      'XSRDA8'
060700041018     c                   parm                    wlbdat
060800041018      *
060900041018     c                   if        g02err = '1'
061000041018     c                   seton                                        284290
061100041018     c                   movel     msg(9)        v1cmsg
061200041018     c                   leave
061300041018     c                   else
061400041018     c                   eval      wamgdcd=g02inv
061500041018     c                   eval      v1cdcd =g02dat
061600041018     c                   endif
061700041018
061800041018     c                   endif
061900041018      * A data
062000041018     c                   if        v1cdca > 0
062100041018     c                   move      V1cdca        g02dat
062200041018     c                   eval      g02err = *blanks
062300041018     c                   call      'XSRDA8'
062400041018     c                   parm                    wlbdat
062500041018      *  Errore
062600041018     c                   if        g02err = '1'
062700041018     c                   seton                                        284390
062800041018     c                   movel     msg(9)        v1cmsg
062900041018     c                   leave
063000041018
063100041018     c                   else
063200041018
063300041018     c                   eval      wamgdca=g02inv
063400041018     c                   eval      v1cdca =g02dat
063500041018     c
063600041018     c                   if        wamgdca < wamgdcd
063700041018     c                   seton                                        284390
063800041018     c                   movel     msg(10)       v1cmsg
063900041018     c                   leave
064000041018     c                   endif
064100041018
064200041018     c                   endif
064300041018
064400041018     c                   endif
064500041018
064600041018      * Data chiusura Richesta Assistenza
064700041018
064800041018     c                   if        v1cdhd = 0 and v1cdha > 0
064900041018     c                   seton                                        442890
065000041018     c                   movel     msg(9)        v1cmsg
065100041018     c                   leave
065200041018     c                   endif
065300041018
065400041018     c                   clear                   wamgdhd
065500041018     c                   clear                   wamgdha
065600041018      * Da data
065700041018     c                   if        v1cdhd > 0
065800041018     c                   move      V1cdhd        g02dat
065900041018     c                   eval      g02err = *blanks
066000041018     c                   call      'XSRDA8'
066100041018     c                   parm                    wlbdat
066200041018      *
066300041018     c                   if        g02err = '1'
066400041018     c                   seton                                        284490
066500041018     C                   movel     msg(9)        v1cmsg
066600041018     c                   leave
066700041018
066800041018     c                   else
066900041018
067000041018     c                   eval      wamgdhd=g02inv
067100041018     c                   eval      v1cdhd =g02dat
067200041018     c                   endif
067300050520     c                   if        wamgdhd < wdapul and v1csra = ' '
067400050520     c                   seton                                        284490
067500050520     C                   movel     msg(25)       v1cmsg
067600050520     c                   leave
067700050520     c                   endif
067800041018
067900041018     c                   endif
068000041018      * a data
068100041018     c                   if        v1cdha > 0
068200041018     c                   move      V1cdha        g02dat
068300041018     c                   eval      g02err = *blanks
068400041018     c                   call      'XSRDA8'
068500041018     c                   parm                    wlbdat
068600041018      *  Errore
068700041018     c                   if        g02err = '1'
068800041018     c                   seton                                        284590
068900041018     c                   movel     msg(9)        v1cmsg
069000041018     c                   leave
069100041018
069200041018     c                   else
069300041018
069400041018     c                   eval      wamgdha=g02inv
069500041018     c                   eval      v1cdha =g02dat
069600041018     c
069700041018     c                   if        wamgdha < wamgdhd
069800041018     c                   seton                                        284590
069900041018     c                   movel     msg(10)       v1cmsg
070000041018     c                   leave
070100041018     c                   endif
070200041018
070300041018     c                   endif
070400041018
070500041018     c                   endif
070600041020      *
070700041020      * almeno un'intervallo di data deve essere impostato
070800041020      *
070900041020     c                   if        v1cdcd = 0 and v1cdca = 0 and
071000041020     c                             v1cdhd = 0 and v1cdha = 0
071100041020     c                   seton                                        289042
071200041020     c                   movel     msg(19)       v1cmsg
071300041020     c                   leave
071400041020     c                   endif
071500041018
071600041018      *
071700041018      * Tipo Comunicazione
071800041018      *          (1)
071900041018
072000041018     c                   If        v1ctc1 <> ' '
072100041018
072200041018     c                   eval      wrk_cor = v1ctc1
072300041018     c                   exsr      ctr_cor
072400041018     c                   eval      v1ctc1  = wrk_cor
072500041018
072600041018     c                   if        *in90
072700041018     c                   eval      *in46 = *on
072800041018     c                   leave
072900041018     c                   endif
073000041018
073100041018     c                   endif
073200041018      *
073300041018      * Tipo Comunicazione
073400041018      *          (2)
073500041018
073600041018     c                   If        v1ctc2 <> ' '
073700041018
073800041018     c                   eval      wrk_cor = v1ctc2
073900041018     c                   exsr      ctr_cor
074000041018     c                   eval      v1ctc2  = wrk_cor
074100041018
074200041018     c                   if        *in90
074300041018     c                   eval      *in47 = *on
074400041018     c                   leave
074500041018     c                   endif
074600041018
074700041018     c                   endif
074800041018      *
074900041018      * Tipo Comunicazione
075000041018      *          (3)
075100041018
075200041018     c                   If        v1ctc3 <> ' '
075300041018
075400041018     c                   eval      wrk_cor = v1ctc3
075500041018     c                   exsr      ctr_cor
075600041018     c                   eval      v1ctc3  = wrk_cor
075700041018
075800041018     c                   if        *in90
075900041018     c                   eval      *in48 = *on
076000041018     c                   leave
076100041018     c                   endif
076200041018
076300041018     c                   endif
076400041018      *
076500041018      * Tipo Comunicazione
076600041018      *          (4)
076700041018
076800041018     c                   If        v1ctc4 <> ' '
076900041018
077000041018     c                   eval      wrk_cor = v1ctc4
077100041018     c                   exsr      ctr_cor
077200041018     c                   eval      v1ctc4  = wrk_cor
077300041018
077400041018     c                   if        *in90
077500041018     c                   eval      *in49 = *on
077600041018     c                   leave
077700041018     c                   endif
077800041018
077900041018     c                   endif
078000041018
078100041018      *
078200041018      * Motivo Apertura
078300041018      *          (1)
078400041018
078500041018     c                   If        v1cma1 <> '  '
078600041018
078700041018     c                   eval      wrk_mar = v1cma1
078800041018     c                   exsr      ctr_mar
078900041019     c                   eval      v1cma1  = wrk_mar
079000041018
079100041018     c                   if        *in90
079200041018     c                   eval      *in50 = *on
079300041018     c                   leave
079400041018     c                   endif
079500041018
079600041018     c                   endif
079700041018      *
079800041018      * Motivo Apertura
079900041018      *          (2)
080000041018
080100041018     c                   If        v1cma2 <> '  '
080200041018
080300041018     c                   eval      wrk_mar = v1cma2
080400041018     c                   exsr      ctr_mar
080500041019     c                   eval      v1cma2  = wrk_mar
080600041018
080700041018     c                   if        *in90
080800041018     c                   eval      *in51 = *on
080900041018     c                   leave
081000041018     c                   endif
081100041018
081200041018     c                   endif
081300041018      *
081400041018      * Motivo Apertura
081500041018      *          (3)
081600041018
081700041018     c                   If        v1cma3 <> '  '
081800041018
081900041018     c                   eval      wrk_mar = v1cma3
082000041018     c                   exsr      ctr_mar
082100041019     c                   eval      v1cma3  = wrk_mar
082200041018
082300041018     c                   if        *in90
082400041018     c                   eval      *in52 = *on
082500041018     c                   leave
082600041018     c                   endif
082700041018
082800041018     c                   endif
082900041018      *
083000041018      * Motivo Apertura
083100041018      *          (4)
083200041018
083300041018     c                   If        v1cma4 <> '  '
083400041018
083500041018     c                   eval      wrk_mar = v1cma4
083600041018     c                   exsr      ctr_mar
083700041019     c                   eval      v1cma4  = wrk_mar
083800041018
083900041018     c                   if        *in90
084000041018     c                   eval      *in53 = *on
084100041018     c                   leave
084200041018     c                   endif
084300041018
084400041018     c                   endif
084500041018      *
084600041018      * Motivo Apertura
084700041018      *          (5)
084800041018
084900041018     c                   If        v1cma5 <> '  '
085000041018
085100041018     c                   eval      wrk_mar = v1cma5
085200041018     c                   exsr      ctr_mar
085300041019     c                   eval      v1cma5  = wrk_mar
085400041018
085500041018     c                   if        *in90
085600041018     c                   eval      *in54 = *on
085700041018     c                   leave
085800041018     c                   endif
085900041018
086000041018     c                   endif
086100041018      *
086200041018      * Motivo Apertura
086300041018      *          (6)
086400041018
086500041018     c                   If        v1cma6 <> '  '
086600041018
086700041018     c                   eval      wrk_mar = v1cma6
086800041018     c                   exsr      ctr_mar
086900041019     c                   eval      v1cma6  = wrk_mar
087000041018
087100041018     c                   if        *in90
087200041018     c                   eval      *in55 = *on
087300041018     c                   leave
087400041018     c                   endif
087500041018
087600041018     c                   endif
087700041018      *
087800041018      * Motivo Apertura
087900041018      *          (7)
088000041018
088100041018     c                   If        v1cma7 <> '  '
088200041018
088300041018     c                   eval      wrk_mar = v1cma7
088400041018     c                   exsr      ctr_mar
088500041019     c                   eval      v1cma7  = wrk_mar
088600041018
088700041018     c                   if        *in90
088800041018     c                   eval      *in56 = *on
088900041018     c                   leave
089000041018     c                   endif
089100041018
089200041018     c                   endif
089300041018      *
089400041018      * Motivo Apertura
089500041018      *          (8)
089600041018
089700041018     c                   If        v1cma8 <> '  '
089800041018
089900041018     c                   eval      wrk_mar = v1cma8
090000041018     c                   exsr      ctr_mar
090100041019     c                   eval      v1cma8  = wrk_mar
090200041018
090300041018     c                   if        *in90
090400041018     c                   eval      *in57 = *on
090500041018     c                   leave
090600041018     c                   endif
090700041018
090800041018     c                   endif
090900041018      *
091000041018      * Codice Chiusura
091100041018      *          (1)
091200041018
091300041018     c                   If        v1ccc1 <> '   '
091400041018
091500041018     c                   eval      wrk_chr = v1ccc1
091600041018     c                   exsr      ctr_chr
091700041018     c                   eval      v1ccc1  = wrk_chr
091800041018
091900041018     c                   if        *in90
092000041018     c                   eval      *in58 = *on
092100041018     c                   leave
092200041018     c                   endif
092300041018
092400041018     c                   endif
092500041018      *
092600041018      * Codice Chiusura
092700041018      *          (2)
092800041018
092900041018     c                   If        v1ccc2 <> '   '
093000041018
093100041018     c                   eval      wrk_chr = v1ccc2
093200041018     c                   exsr      ctr_chr
093300041018     c                   eval      v1ccc2  = wrk_chr
093400041018
093500041018     c                   if        *in90
093600041018     c                   eval      *in59 = *on
093700041018     c                   leave
093800041018     c                   endif
093900041018
094000041018     c                   endif
094100041018      *
094200041018      * Codice Chiusura
094300041018      *          (3)
094400041018
094500041018     c                   If        v1ccc3 <> '   '
094600041018
094700041018     c                   eval      wrk_chr = v1ccc3
094800041018     c                   exsr      ctr_chr
094900041018     c                   eval      v1ccc3  = wrk_chr
095000041018
095100041018     c                   if        *in90
095200041018     c                   eval      *in60 = *on
095300041018     c                   leave
095400041018     c                   endif
095500041018
095600041018     c                   endif
095700041018      *
095800041018      * Se richieste solo le aperte non posso chiedere causale chiusura
095900041018      *  o date chiusura
096000041018     c                   if        v1csra='S'
096100041018     c                   if        v1cdhd>0 or v1ccc1<>'   ' or v1ccc2<>'   '
096200041018     c                             or v1ccc3<>'   '
096300041018     c                   seton                                        286190
096400041018     c                   movel     msg(14)       v1cmsg
096500041018     c                   leave
096600041018     c                   endif
096700041018     c                   endif
096800041018      * Se richieste le aperte da xx giorni posso chiedere causale chiusura
096900041018      *  o date chiusura
097000041018     c                   if        v1cxgg> 0
097100041018     c                   if        v1cdhd>0 or v1ccc1<>'   ' or v1ccc2<>'   '
097200041018     c                             or v1ccc3<>'   '
097300041018     c                   seton                                        286290
097400041018     c                   movel     msg(15)       v1cmsg
097500041018     c                   leave
097600041018     c                   endif
097700041018     c                   endif
097800041130
097900041130      * Solo aperte
098000041130     c                   If        v1csra <> ' '  and v1cord <> '1'
098100041130     c                   seton                                        669028
098200041130     c                   movel     MSG(23)       v1cmsg
098300041130     c                   leave
098400041130     c                   endif
098500041130
098600041130      * Conteggiare le aperte da oltre
098700041130     c                   If        v1cxgg <> 0    and v1cord <> '1'
098800041130     c                   seton                                        679028
098900041130     c                   movel     MSG(23)       v1cmsg
099000041130     c                   leave
099100041130     c                   endif
099200141204      /free
099300141204       //?Controllo Utente
099400141204        exsr ctr_UTE;
099500141204      /end-free
099600041018      * Utente
099700141204      *    (1)
099800041018      *
099900041018     c                   leave
100000041018      *
100100041018     c                   enddo
100200041018
100300041018     c                   endsr
100400041018      *---------------------------------------------------------------*
100500041018      * CONTROLLO P.O.                                                *
100600041018      *---------------------------------------------------------------*
100700041018     C     CTRPO         BEGSR
100800041018      *
100900041018     c     wfil          chain     azorg01l
101000041018
101100041019     c                   if        not %found(azorg01l) or (orgfva <> ' ')
101200041026     c                             or (orgfag <> 'A' and orgfag <> 'F' and
101300041026     c                              orgfil <> 046)
101400041018     c                   eval      v1cmsg=msg(6)
101500041018     c                   seton                                        2890
101600041018     c                   endif
101700041018      *
101800041018     c                   ENDSR
101900041018      *---------------------------------------------------------------*
102000041018      * CONTROLLO tipo comunicazione                                  *
102100041018      *---------------------------------------------------------------*
102200041018     C     CTR_COR       BEGSR
102300041018
102400041018      * ricerca
102500041018
102600041018     c     '?'           scan      wrk_cor                                90
102700041018     c                   clear                   tibs02ds
102800041018
102900041018     c                   if        *in90
103000041018     c                   movel     'R'           t02mod
103100041018     c                   movel     knsif         t02sif
103200041018     c                   movel     'COR'         t02cod
103300041018      *
103400041018     c                   call      'TIBS02R'
103500041018     c                   parm                    kpjba
103600041018     c                   parm                    tibs02ds
103700041018      *
103800041018     c                   if        t02err = ' '
103900041018     c                   movel     t02ke1        wrk_cor
104000041018     c                   endif
104100041018      *
104200041018     c                   else
104300041018      *  controllo
104400041018     c                   clear                   tibs02ds
104500041018     c                   movel     'C'           t02mod
104600041018     c                   movel     knsif         t02sif
104700041018     c                   movel     'COR'         t02cod
104800041018     c                   movel     wrk_cor       t02ke1
104900041018      *
105000041018     c                   call      'TIBS02R'
105100041018     c                   parm                    kpjba
105200041018     c                   parm                    tibs02ds
105300041018      *
105400041018     c                   if        t02err <> ' '
105500041018     c                   seton                                        2890
105600041018     c                   movel     MSG(11)       v1cmsg
105700140925     c                   leavesr
105800041018     c                   endif
105900140925      /free
106000140925       //?Verifico se il tipo comunicazione è utilizzabile dall'utente
106100140925         dCOR = T02uni;
106200140925         IF  §CORtcut  = 'N';
106300140925           *in28 = *on;
106400140925           *in90 = *on;
106500140925           V1Cmsg = msg(11);
106600140925           V1Cmsg = %trim(V1Cmsg) + '. Non utilizzabile dall''utente';
106700140925         ENDIF;
106800140925      /end-free
106900140925
107000041018      *
107100041018     c                   endif
107200041018
107300041018     c                   endsr
107400041018      *---------------------------------------------------------------*
107500041018      * CONTROLLO motivo apertura                                     *
107600041018      *---------------------------------------------------------------*
107700041018     C     CTR_MAR       BEGSR
107800041018
107900041018      * ricerca
108000041018
108100041018     c     '?'           scan      wrk_mar                                90
108200131018      *
108300041018     c                   if        *in90
108400131018      *
108500131018     c                   clear                   tntbMARds1
108600140210     c*//                eval      iMARutut = 'I'
108700140210     c                   eval      iMARest  = 'I'
108800131018     c                   movel(p)  tntbMARds1    KPJBU
108900131018      *
109000131018     c                   call      'TNTBMARR1'
109100131018     c                   parm                    KPJBA
109200131018      *
109300131018     c                   movel     KPJBU         tntbMARds1
109400131018     c                   if        oMARfxx = *blank  and  oMARerr = *off
109500131018     c                   movel     oMARcod       wrk_mar
109600041018     c                   endif
109700041018      *
109800041018     c                   else
109900041018      *  controllo
110000041018     c                   clear                   tibs02ds
110100041018     c                   movel     'C'           t02mod
110200041018     c                   movel     knsif         t02sif
110300041018     c                   movel     'MAR'         t02cod
110400041018     c                   movel     wrk_mar       t02ke1
110500041018      *
110600041018     c                   call      'TIBS02R'
110700041018     c                   parm                    kpjba
110800041018     c                   parm                    tibs02ds
110900041018      *
111000140130     c                   clear                   dMAR
111100140130     c                   if        T02err = *blanks
111200140130     c                   eval      dMAR = T02uni
111300140130     c                   endif
111400140130      *
111500140210     c*//                if        t02err <> ' '  or  §MARutUt = 'N'
111600140210     c                   if        t02err <> ' '  or  §MARest  = 'S'
111700041018     c                   seton                                        2890
111800041018     c                   movel     MSG(12)       v1cmsg
111900041018     c                   endif
112000041018      *
112100041018     c                   endif
112200041018
112300041018     c                   endsr
112400041018      *---------------------------------------------------------------*
112500041018      * CONTROLLO codice chiusura                                     *
112600041018      *---------------------------------------------------------------*
112700041018     C     CTR_CHR       BEGSR
112800041018
112900041018      * ricerca
113000041018
113100041018     c     '?'           scan      wrk_chr                                90
113200041018     c                   clear                   tibs02ds
113300041018
113400041018     c                   if        *in90
113500041018     c                   movel     'R'           t02mod
113600041018     c                   movel     knsif         t02sif
113700041018     c                   movel     'CHR'         t02cod
113800041018      *
113900041018     c                   call      'TIBS02R'
114000041018     c                   parm                    kpjba
114100041018     c                   parm                    tibs02ds
114200041018      *
114300041018     c                   if        t02err = ' '
114400041018     c                   movel     t02ke1        wrk_chr
114500041018     c                   endif
114600041018      *
114700041018     c                   else
114800041018      *  controllo
114900041018     c                   clear                   tibs02ds
115000041018     c                   movel     'C'           t02mod
115100041018     c                   movel     knsif         t02sif
115200041018     c                   movel     'CHR'         t02cod
115300041018     c                   movel     wrk_chr       t02ke1
115400041018      *
115500041018     c                   call      'TIBS02R'
115600041018     c                   parm                    kpjba
115700041018     c                   parm                    tibs02ds
115800041018      *
115900041018     c                   if        t02err <> ' '
116000041018     c                   seton                                        2890
116100041018     c                   movel     MSG(13)       v1cmsg
116200041018     c                   endif
116300041018      *
116400041018     c                   endif
116500041018
116600041018     c                   endsr
116700041018      *---------------------------------------------------------------*
116800041018      * CONFERMA ELABORAZIONE E LANCIO                                *
116900041018      *---------------------------------------------------------------*
117000041018     C     SR_conferma   BEGSR
117100041018
117200041018      * imposto la DS per il lancio
117300141204     c******             clear                   fidnd0ds
117400041019     c                   eval      id0dst = v1cdis
117500041025     c                   movel     v1care        id0are
117600041019     c                   eval      id0crp = v1ccrp
117700121116     c                   eval      id0crptch = v1ccrptch
117800041019     c   01              eval      id0toi = v1ctoi
117900041019     c  N01              eval      id0toi = v1ctsn
118000041019
118100041019     c                   eval      id0ord = v1cord
118200041019     c                   eval      id0sap = v1csap
118300041019
118400041019     c                   eval      id0prs = v1cprs
118500041019     c                   eval      id0fil = v1cfil
118600041019     c                   eval      id0aap = v1caap
118700041019     c                   eval      id0sre = v1csre
118800041019
118900041019     c                   eval      id0amgdcd = wamgdcd
119000041019     c                   eval      id0amgdca = wamgdca
119100041019     c                   eval      id0amgdhd = wamgdhd
119200041019     c                   eval      id0amgdha = wamgdha
119300041019     c                   eval      id0dcd = v1cdcd
119400041019     c                   eval      id0dca = v1cdca
119500041019     c                   eval      id0dhd = v1cdhd
119600041019     c                   eval      id0dha = v1cdha
119700041019
119800041019     c                   eval      id0tc1 = v1ctc1
119900041019     c                   eval      id0tc2 = v1ctc2
120000041019     c                   eval      id0tc3 = v1ctc3
120100140925     c                   eval      id0tc4 = v1ctc4
120200041019
120300041019     c                   eval      id0ma1 = v1cma1
120400041019     c                   eval      id0ma2 = v1cma2
120500041019     c                   eval      id0ma3 = v1cma3
120600041019     c                   eval      id0ma4 = v1cma4
120700041019     c                   eval      id0ma5 = v1cma5
120800041019     c                   eval      id0ma6 = v1cma6
120900041019     c                   eval      id0ma7 = v1cma7
121000041019     c                   eval      id0ma8 = v1cma8
121100041019
121200041019     c                   eval      id0cc1 = v1ccc1
121300041019     c                   eval      id0cc2 = v1ccc2
121400041019     c                   eval      id0cc3 = v1ccc3
121500041019
121600041019     c                   eval      id0sra = v1csra
121700041019     c                   eval      id0xgg = v1cxgg
121800041019
121900041019     c
122000041019     c* SE ditretto e area vuoti e ho impostato fil per ord 1 li riempo
122100041022     c**                 if        id0ord='1' and id0fil>0 AND
122200041022     c**                           id0dst=*blanks and id0are=*blanks
122300041022     c**   id0fil        chain     azorg01l                           30
122400041022     c**                 movel     orgfl3        id0dst
122500041022     c**                 movel     orgcar        id0are
122600041022     c**                 endif
122700041022     c**                 if        id0ord='2' and id0prs>0 AND
122800041022     c**                           id0dst=*blanks and id0are=*blanks
122900041022     c**   id0prs        chain     azorg01l                           30
123000041022     c**                 movel     orgfl3        id0dst
123100041022     c**                 movel     orgcar        id0are
123200041022     c**                 endif
123300041019      *
123400041019     c                   clear                   Wpgm             10
123500041019      *
123600041019     c                   movel     fidnd0ds      kpjbu
123700041019     c
123800041026     c                   eval      kcoaz='DND1'
123900041203     c******             eval      Wpgm = 'FIDND1R   '
124000080404     c                   If        *in01
124100080404     c                   eval      Wpgm = 'BCH09     '
124200080404     c                   call      Wpgm
124300080404     c                   parm                    kpjba
124400080404     c                   endif
124500080404     c                   eval      Wpgm = 'BCH10     '
124600041203     c                   call      Wpgm
124700041019     c                   parm                    kpjba
124800121116
124900130530     c*                  IF        *in01 and ID0crp = 'S'
125000130530     c*                  eval      kcoaz='DND4'
125100130530     c*                  eval      wpgm = 'BCH10     '
125200130530     c*                  call      wpgm
125300130530     c*                  parm                    kpjba
125400130530     c*                  ENDIF
125500121116
125600041019     c                   endsr
125700041018     c
125800141204
125900141204      /free
126000141204       //--------------------------------------------------------------
126100141204       //?Controllo Utente
126200141204       //--------------------------------------------------------------
126300141204       BEGSR ctr_UTE;
126400141204
126500141204       //?(1)
126600141204         w_utegen = *off;
126700141204         clear w_ute;
126800141204         clear wlute;
126900141204         w_utemax = *all'9';
127000141204
127100141204         IF  V1Cugr1 <> *blanks;
127200141204           wrk_ute = V1Cugr1;
127300141204         //?Come prima cosa verifico se richiesta ricerca su tab. UGR
127400141204           IF  %scan('?':V1Cugr1) > 0;
127500141204             exsr ricerca_UGR;
127600141204             V1Cugr1 = wrk_ute;
127700141204             IF  *in90;
127800141204               *in63 = *on;
127900141204               leavesr;
128000141204             ENDIF;
128100141204           ENDIF;
128200141204
128300141204         //?Verifico se richiesto utente generico (*)
128400141204           wlute = %scan('*':V1Cugr1);
128500141204           IF  wlute > 0;
128600141204             w_ute = %subst(V1Cugr1:1:(wlute - 1));
128700141204             %subst(w_utemax:1:(wlute - 1)) = %subst(V1Cugr1:1:(wlute - 1));
128800141204             w_utegen = *on;
128900141204           ENDIF;
129000141204
129100141204         //?Verifico se OK l'utente richiesto (generico e non)
129200141204         //?accedo alla tabella con i primi 3 byte dell'utente
129300141204           wrk_ute = %subst(V1Cugr1:1:3);
129400141204           exsr controlla_UGR;
129500141204           IF  *in28;
129600141204             *in63 = *on;
129700141204             leavesr;
129800141204           ENDIF;
129900141204
130000141204         //?Controllo che non sia stato richiesto un utente doppio
130100141204           IF  V1Cugr1 = V1Cugr2 or V1Cugr1 = V1Cugr3 or
130200141204               V1Cugr1 = V1Cugr4 or V1Cugr1 = V1Cugr5;
130300141204             *in28 = *on;
130400141204             *in63 = *on;
130500141204             *in90 = *on;
130600141204             V1Cmsg = msg(16);
130700141204             V1Cmsg = %trim(V1Cmsg) + '. Già presente nell''elenco';
130800141204             leavesr;
130900141204           ENDIF;
131000141204
131100141204         //?Imposto già i dati nella ds di richiamo pgm batch
131200141204           IF  w_utegen;
131300141204             ID0ugrmin1 = w_ute;
131400141204             ID0ugrmax1 = w_utemax;
131500141204           ELSE;
131600141204             ID0ugrmin1 = V1Cugr1;
131700141204             clear ID0ugrmax1;
131800141204           ENDIF;
131900141204         ENDIF;
132000141204
132100141204       //?(2)
132200141204         w_utegen = *off;
132300141204         clear w_ute;
132400141204         clear wlute;
132500141204         w_utemax = *all'9';
132600141204
132700141204         IF  V1Cugr2 <> *blanks;
132800141204           wrk_ute = V1Cugr2;
132900141204         //?Come prima cosa verifico se richiesta ricerca su tab. UGR
133000141204           IF  %scan('?':V1Cugr2) > 0;
133100141204             exsr ricerca_UGR;
133200141204             V1Cugr2 = wrk_ute;
133300141204             IF  *in90;
133400141204               *in68 = *on;
133500141204               leavesr;
133600141204             ENDIF;
133700141204           ENDIF;
133800141204
133900141204         //?Verifico se richiesto utente generico (*)
134000141204           wlute = %scan('*':V1Cugr2);
134100141204           IF  wlute > 0;
134200141204             w_ute = %subst(V1Cugr2:1:(wlute - 1));
134300141204             %subst(w_utemax:1:(wlute - 1)) = %subst(V1Cugr2:1:(wlute - 1));
134400141204             w_utegen = *on;
134500141204           ENDIF;
134600141204
134700141204         //?Verifico se OK l'utente richiesto (generico e non)
134800141204         //?accedo alla tabella con i primi 3 byte dell'utente
134900141204           wrk_ute = %subst(V1Cugr2:1:3);
135000141204           exsr controlla_UGR;
135100141204           IF  *in28;
135200141204             *in68 = *on;
135300141204             leavesr;
135400141204           ENDIF;
135500141204
135600141204         //?Controllo che non sia stato richiesto un utente doppio
135700141204           IF  V1Cugr2 = V1Cugr3 or V1Cugr2 = V1Cugr4 or
135800141204               V1Cugr2 = V1Cugr5;
135900141204             *in28 = *on;
136000141204             *in68 = *on;
136100141204             *in90 = *on;
136200141204             V1Cmsg = msg(16);
136300141204             V1Cmsg = %trim(V1Cmsg) + '. Già presente nell''elenco';
136400141204             leavesr;
136500141204           ENDIF;
136600141204
136700141204         //?Imposto già i dati nella ds di richiamo pgm batch
136800141204           IF  w_utegen;
136900141204             ID0ugrmin2 = w_ute;
137000141204             ID0ugrmax2 = w_utemax;
137100141204           ELSE;
137200141204             ID0ugrmin2 = V1Cugr2;
137300141204             clear ID0ugrmax2;
137400141204           ENDIF;
137500141204         ENDIF;
137600141204
137700141204       //?(3)
137800141204         w_utegen = *off;
137900141204         clear w_ute;
138000141204         clear wlute;
138100141204         w_utemax = *all'9';
138200141204
138300141204         IF  V1Cugr3 <> *blanks;
138400141204           wrk_ute = V1Cugr3;
138500141204         //?Come prima cosa verifico se richiesta ricerca su tab. UGR
138600141204           IF  %scan('?':V1Cugr3) > 0;
138700141204             exsr ricerca_UGR;
138800141204             V1Cugr3 = wrk_ute;
138900141204             IF  *in90;
139000141204               *in69 = *on;
139100141204               leavesr;
139200141204             ENDIF;
139300141204           ENDIF;
139400141204
139500141204         //?Verifico se richiesto utente generico (*)
139600141204           wlute = %scan('*':V1Cugr3);
139700141204           IF  wlute > 0;
139800141204             w_ute = %subst(V1Cugr3:1:(wlute - 1));
139900141204             %subst(w_utemax:1:(wlute - 1)) = %subst(V1Cugr3:1:(wlute - 1));
140000141204             w_utegen = *on;
140100141204           ENDIF;
140200141204
140300141204         //?Verifico se OK l'utente richiesto (generico e non)
140400141204         //?accedo alla tabella con i primi 3 byte dell'utente
140500141204           wrk_ute = %subst(V1Cugr3:1:3);
140600141204           exsr controlla_UGR;
140700141204           IF  *in28;
140800141204             *in69 = *on;
140900141204             leavesr;
141000141204           ENDIF;
141100141204
141200141204         //?Controllo che non sia stato richiesto un utente doppio
141300141204         IF  V1Cugr3 = V1Cugr4 or V1Cugr3 = V1Cugr5;
141400141204           *in28 = *on;
141500141204           *in69 = *on;
141600141204           *in90 = *on;
141700141204           V1Cmsg = msg(16);
141800141204           V1Cmsg = %trim(V1Cmsg) + '. Già presente nell''elenco';
141900141204           leavesr;
142000141204         ENDIF;
142100141204
142200141204         //?Imposto già i dati nella ds di richiamo pgm batch
142300141204           IF  w_utegen;
142400141204             ID0ugrmin3 = w_ute;
142500141204             ID0ugrmax3 = w_utemax;
142600141204           ELSE;
142700141204             ID0ugrmin3 = V1Cugr3;
142800141204             clear ID0ugrmax3;
142900141204           ENDIF;
143000141204         ENDIF;
143100141204
143200141204       //?(4)
143300141204         w_utegen = *off;
143400141204         clear w_ute;
143500141204         clear wlute;
143600141204         w_utemax = *all'9';
143700141204
143800141204         IF  V1Cugr4 <> *blanks;
143900141204           wrk_ute = V1Cugr4;
144000141204         //?Come prima cosa verifico se richiesta ricerca su tab. UGR
144100141204           IF  %scan('?':V1Cugr4) > 0;
144200141204             exsr ricerca_UGR;
144300141204             V1Cugr4 = wrk_ute;
144400141204             IF  *in90;
144500141204               *in70 = *on;
144600141204               leavesr;
144700141204             ENDIF;
144800141204           ENDIF;
144900141204
145000141204         //?Verifico se richiesto utente generico (*)
145100141204           wlute = %scan('*':V1Cugr4);
145200141204           IF  wlute > 0;
145300141204             w_ute = %subst(V1Cugr4:1:(wlute - 1));
145400141204             %subst(w_utemax:1:(wlute - 1)) = %subst(V1Cugr4:1:(wlute - 1));
145500141204             w_utegen = *on;
145600141204           ENDIF;
145700141204
145800141204         //?Verifico se OK l'utente richiesto (generico e non)
145900141204         //?accedo alla tabella con i primi 3 byte dell'utente
146000141204           wrk_ute = %subst(V1Cugr4:1:3);
146100141204           exsr controlla_UGR;
146200141204           IF  *in28;
146300141204             *in70 = *on;
146400141204             leavesr;
146500141204           ENDIF;
146600141204
146700141204         //?Controllo che non sia stato richiesto un utente doppio
146800141204         IF  V1Cugr4 = V1Cugr5;
146900141204           *in28 = *on;
147000141204           *in70 = *on;
147100141204           *in90 = *on;
147200141204           V1Cmsg = msg(16);
147300141204           V1Cmsg = %trim(V1Cmsg) + '. Già presente nell''elenco';
147400141204           leavesr;
147500141204         ENDIF;
147600141204
147700141204         //?Imposto già i dati nella ds di richiamo pgm batch
147800141204           IF  w_utegen;
147900141204             ID0ugrmin4 = w_ute;
148000141204             ID0ugrmax4 = w_utemax;
148100141204           ELSE;
148200141204             ID0ugrmin4 = V1Cugr4;
148300141204             clear ID0ugrmax4;
148400141204           ENDIF;
148500141204         ENDIF;
148600141204
148700141204       //?(5)
148800141204         w_utegen = *off;
148900141204         clear w_ute;
149000141204         clear wlute;
149100141204         w_utemax = *all'9';
149200141204
149300141204         IF  V1Cugr5 <> *blanks;
149400141204           wrk_ute = V1Cugr5;
149500141204         //?Come prima cosa verifico se richiesta ricerca su tab. UGR
149600141204           IF  %scan('?':V1Cugr5) > 0;
149700141204             exsr ricerca_UGR;
149800141204             V1Cugr5 = wrk_ute;
149900141204             IF  *in90;
150000141204               *in71 = *on;
150100141204               leavesr;
150200141204             ENDIF;
150300141204           ENDIF;
150400141204
150500141204         //?Verifico se richiesto utente generico (*)
150600141204           wlute = %scan('*':V1Cugr5);
150700141204           IF  wlute > 0;
150800141204             w_ute = %subst(V1Cugr5:1:(wlute - 1));
150900141204             %subst(w_utemax:1:(wlute - 1)) = %subst(V1Cugr5:1:(wlute - 1));
151000141204             w_utegen = *on;
151100141204           ENDIF;
151200141204
151300141204         //?Verifico se OK l'utente richiesto (generico e non)
151400141204         //?accedo alla tabella con i primi 3 byte dell'utente
151500141204           wrk_ute = %subst(V1Cugr5:1:3);
151600141204           exsr controlla_UGR;
151700141204           IF  *in28;
151800141204             *in71 = *on;
151900141204             leavesr;
152000141204           ENDIF;
152100141204
152200141204         //?Imposto già i dati nella ds di richiamo pgm batch
152300141204           IF  w_utegen;
152400141204             ID0ugrmin5 = w_ute;
152500141204             ID0ugrmax5 = w_utemax;
152600141204           ELSE;
152700141204             ID0ugrmin5 = V1Cugr5;
152800141204             clear ID0ugrmax5;
152900141204           ENDIF;
153000141204         ENDIF;
153100141204
153200141204       ENDSR;
153300141204
153400141204       //--------------------------------------------------------------
153500141204       //?Ricerca utente tabella UGR
153600141204       //--------------------------------------------------------------
153700141204       BEGSR ricerca_UGR;
153800141204
153900141204         clear TIBS02DS;
154000141204         clear wrk_ute;
154100141204         T02mod = 'R';
154200141204         T02cod = 'UGR';
154300141204         T02sif = KNSIF;
154400141204         TNTBE_RicercaControllo  (kpjba : tibs02ds);
154500141204         IF  T02err = *blanks;
154600141204           wrk_ute = T02ke1;
154700141204           *in90 = *on;
154800141204         ENDIF;
154900141204
155000141204       ENDSR;
155100141204
155200141204       //--------------------------------------------------------------
155300141204       //?Controllo utente tabella UGR
155400141204       //--------------------------------------------------------------
155500141204       BEGSR controlla_UGR;
155600141204
155700141204         clear TIBS02DS;
155800141204         T02mod = 'C';
155900141204         T02cod = 'UGR';
156000141204         T02sif = KNSIF;
156100141204         T02ke1 = wrk_ute;
156200141204         TNTBE_RicercaControllo  (kpjba : tibs02ds);
156300141204         IF  T02err <> *blanks;
156400141204           *in28 = *on;
156500141204           *in90 = *on;
156600141204           V1Cmsg = msg(16);
156700141204         ENDIF;
156800141204
156900141204       ENDSR;
157000141204
157100141204      /end-free
157200141204
157300041018      *---------------------------------------------------------------*
157400041014      * Reperimento Dati del job (Utente/Operativi)                   *
157500041014      *---------------------------------------------------------------*
157600041014     c     DatiJob       BegSr
157700041014      *
157800041014     c     *dtaara       define    §azute        azuteds
157900041014     c     *dtaara       define    §datiute      ddatiute
158000041014      *
158100041014     c                   in(E)     *dtaara
158200041014     c                   IF        %ERROR or RSUT = *blanks
158300041014     c                   clear                   Tibs34Ds
158400041014     c                   call      'TIBS34R'
158500041014     c                   parm                    Tibs34Ds
158600041014     c                   in        *dtaara
158700041014     c                   ENDIF
158800041014      *
158900041015     c     ktab          klist
159000041015     c                   kfld                    codut
159100041015     c                   kfld                    cod
159200041015     c                   kfld                    key
159300041015      *
159400041015     c     korg          klist
159500041015     c                   kfld                    v1cdis
159600041015     c                   kfld                    ware
159700041015      *
159800041015     c     korg1         klist
159900041015     c                   kfld                    orgfl3
160000041015     c                   kfld                    orgcar
160100041015      *
160200041014     c                   EndSr
160300041014      *------------------------------------------------------------------------*
160400041014      * Routine Iniziale                                              *
160500041014      *---------------------------------------------------------------*
160600041014     c     *InzSR        BegSR
160700041014      *
160800041014     c     *Entry        plist
160900041014     c                   parm                    KPJBA
161000041014      * Reperisco data corrente
161100041014     c                   time                    w0140
161200041014     c                   move      w0140         wData
161300041014     c                   clear                   WLBdat
161400041014     c                   eval      G02dat = wData
161500041014     c                   call      'XSRDA8'
161600041014     c                   parm                    WLBdat
161700041014     c                   z-add     G02inv        DateU
161800050520     c                   z-add     g02tgi        sav_tgi
161900041014      *
162000041014      * Reperisco dati job
162100041014     c                   exsr      DatiJob
162200041014
162300041014      * Se sono in sede accendo 01
162400041014     c                   eval      *in01=(dutlpo ='S')
162500041014      * Se sono in 2°livell accendo 06
162600041014     c                   eval      *in06=(dutlpo ='2')
162700041020      * CARICO p.o. per eventuali controlli
162800041020     c                   if        dutlpo <>'S'
162900041020      * cerco i PO autorizzati
163000041020     c                   clear                   trul31ds
163100041021     c                   eval      I31cdi = dutdis
163200041021     c                   eval      I31car = dutare
163300041020     c                   eval      i31cpo = dutpou
163400041020     c                   eval      i31abi = 'RA'
163500041020
163600041020     c                   call      'TRUL31R'
163700041020     c                   parm                    kpjba
163800041021     c                   parm                    trul31ds
163900041020     c                   movea     opg           poa
164000041020     c                   Endif
164100041020      * se secondo livello guardo anche la L6
164200041020     c                   If        Dutlpo = '2'
164300041019     c                   clear                   trul06ds
164400041020     c                   move      '£6'          d06cod
164500041019     c                   movel     dutpou        d06key
164600041021     c                   movel(p)  trul06ds      KPJBU
164700041019
164800041019     c                   call      'TRUL06R'
164900041019     c                   parm                    kpjba
165000041019     c                   movel     kpjbu         trul06ds
165100041020     c                   movea     lin           l6
165200041019     c                   Endif
165300050520     c
165400050520      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
165500050520      *
165600050520     C                   MOVEL     '5A'          COD
165700050520     C                   MOVEL(p)  '2'           KEY
165800050520     C     KTAB          CHAIN     TABEL00F                           30
165900050520     C  N30              MOVEL     TBLUNI        DS5A2
166000050520     C   30              CLEAR                   DS5A2
166100050520     c* determino data pulizia file reclami
166200050520     C     sav_tgi       SUB       §5Atgd        GIOTGI
166300050520     C                   CALL      'XSRGI8'
166400050520     C                   PARM                    WGIDAT
166500050520     C                   Z-ADD     GIOinv        Wdapul            8 0          GG/MM/AAAA
166600041014
166700041014     c                   ENDSR
166800041015** MSG  Lungh. 78                                                            *
166900041019Cod.Distretto errato                                                           1
167000041019Cod.Distretto non presente nell'oranigramma                                    2
167100041019Cod.Area errato                                                                3
167200041019Cod.Area non congruente col distretto selezionato                              4
167300041019Cod.Area non presente nell'organigramma                                        5
167400041019Punto operativo inesistente                                                    6
167500041019Il P.O. non fa parte del distretto selezionato                                 7
167600041019Il P.O. non fa parte dell'area selezionata                                     8
167700041019Data errata                                                                    9
167800041019Data AL   MINORE  data DAL                                                    10
167900041019Tipo Comunicazione errata                                                     11
168000041018Motivo Apertura errato                                                        12
168100041018Codice Chiusura errato                                                        13
168200041019Se selezionate le "Solo aperte" è inutile selezionare data e codici Chiusura  14
168300041018Se richiesto conteggio delle aperte da oltre x gg NO data a codici Chiusura   15
168400041019Utente errato                                                                 16
168500041020Punto Operativo errato                                                        17
168600041020Immettere Punto Operativo valido                                              18
168700041020Immettere almeno un'intervallo di date                                        19
168800041020Immettere o il P.O. apertura o il P.O. responsabile                           20
168900041020Almeno uno dei due P.O. deve appartenere all'area                             21
169000041022Se inserito Distretto e/o Area non inserire il P.O.                           22
169100041130Selezionare solo se richiesto Ordinamento "1"=Num.Richiesta                   23
169200041130Selezionare solo se richiesto Ordinamento "2"=P.O. responsabile               24
169300050520Data non valida per trascorsi termini di pulizia                              25
