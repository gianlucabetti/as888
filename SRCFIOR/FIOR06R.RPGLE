000100080415     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200001024
000300001024      *****************************************************************
000400001024      *                                                               *
000500001024      * MANUTENZIONE FILE NOTE                                        *
000600001024      *                                                               *
000700001024      *****************************************************************
000800001024
000900001024      ****************************************************************
001000001024      *  RIEPILOGO INDICATORI
001100001024      ****************************************************************
001200001024      * 01    - flag modalità diverso da *blanks e da "V" (Manutenzione e conferma)
001300080410      *         e da 'P' (note da PDA)
001400001024      * 02    - interrogazione
001500001024      * 06    - Disabilitazione rollup
001600001109      * 08    - protezione note
001700001219      * 09    - Errore su tipo nota
001800040402      * 10    - Errore su nota
001900010723      * 12    - Sono in sede
002000080318      * 13    - protezione campi in testata
002100080318      * 14    - protezione campi in testata
002200080318      * 15    - protezione campi in testata
002300080319      * 20    - ho ricevuto + di 2 parametri
002400001219      * 28    - Errore a video
002500001024      * 30    - COMODO
002600001024      * 31    - COMODO
002700001109      * 60    - COMODO gestione Subfile
002800001109      * 61    - COMODO gestione Subfile
002900001109      * 63    - COMODO gestione Subfile
003000001024      ****************************************************************
003100001024
003200001024     FFIOR06D   CF   E             WORKSTN sfile(fior06s:nrr)
003300001024     F                                     INFDS(dn06DS)
003400001023     FFNORN01L  UF A E           K DISK
003500080318     FFNORM01L  IF   E           K DISK
003600080318     FFNORF01L  IF   E           K DISK
003700080320     FFIDST09L  IF   E           K DISK    usropn extfile(openthis)
003800080318     FFIAPD01L  IF   E           K DISK
003900001027     FTNTBE01L  IF   E           K DISK
004000001024
004100001024      *------------------------------------------------------------------------*
004200981026     D dtld          E DS
004300080318     D DCMR          E DS
004400080410     d dorf01        e ds
004500001027     D TIBS02DS      E DS
004600001027     D FIOR06DS      E DS
004700080318     D FIOR06VDS     E DS
004800080318     d fnlv55ds      e ds
004900951009     D KPJBA         E DS
005000150515
005100150515      * - Parametri x Controllo profilo utenti
005200150515     d TIBS34ds      e ds
005300150515      * - Ds di riferimento al file esterno AZUTE00F
005400150515     d AzuteDs       e ds                  extname(AZUTE00F)
005500150515      * - Ds per dati organigramma
005600150515     d dDatiUte      e ds
005700001027
005800001027     D Parm01          DS
005900001027     D  ppoe                          3  0
006000001027     D  pnor                          7  0
006100001027     D  pnsr                          2  0
006200001027     D  pnrv                          2  0
006300001027     D  psce                          1
006400001027     D  pfgs                          3  0
006500001027
006600001027     D ktbcod          S                   LIKE(TBEcod)
006700001027     D ktbke1          S                   LIKE(TBEke1)
006800001027     D ktbke2          S                   LIKE(TBEke2)
006900001024
007000001024     D dn06DS          DS
007100971023     D  NRG                  370    370
007200971023     D  PRIMA                378    379B 0
007300001024
007400971024     D CURSOR          DS
007500971024     D  RIRI                   1      2B 0
007600971024     D  R$$                    2      2
007700971024     D  COCO                   3      4B 0
007800971024     D  C$$                    4      4
007900001024
008000001024     D                 DS
008100001024     D  §06poe                 5      7  0
008200001024     D  §06nsr                 8      9  0
008300001024     D  §06nor                10     16  0
008400001024     D  §06nrv                17     18  0
008500001027     D  §06dai                19     26  0
008600001027     D  §06ori                27     32  0
008700001027     D  §06far                33     35  0
008800001027     D  i06key                 1     35
008900001024
009000980714     D s2ds            DS
009100001114     D  vshkey                 1     31
009200001114     D  vshpoe                 1      3  0
009300001114     D  vshnsr                 4      5  0
009400001114     D  vshnor                 6     12  0
009500001114     D  vshnrv                13     14  0
009600001114     D  vshdai                15     22  0
009700001114     D  vshori                23     28  0
009800001114     D  vshfar                29     31  0
009900001114     D  vsdai                 32     39  0
010000001114     D  vsori                 40     45  0
010100001114     D  vsfar                 46     48  0
010200001114     d  vsnot                 49     83
010300001114     D  vspue                 84     93
010400001114     D  vspon                 94     96  0
010500980422     D                 DS
010600980608     D  w035a                  1     35
010700951009     D                SDS
010800971016     D  VTCPGM                 1     10
010900980609      *
011000980609     D WLBDAT          DS                  INZ
011100980609     D  G02DAT                 1      8  0
011200980609     D  G02INV                 9     16  0
011300980609     D  G02ERR                17     17
011400980609     D  G02TGI                18     22  0
011500980720      *
011600980720     D s2sav           s                   LIKE(s2ds)
011700980720     D s2sav2          s                   LIKE(s2ds)
011800980720     D s2sav3          s                   LIKE(s2ds)
011900001024     D prog            s                   LIKE(ORNprg)
012000080318     D kfgs            s                   LIKE(ORmpor)
012100001129     D wflag           S              1
012200080320     D openthis        s             21
012300980609      *
012400080318     d catk            s              1  0 inz(4)
012500981216     D x               s              2  0
012600990610     D t               s              2  0
012700990610     D wtesto          s              3
012800040402     d savpos          s                   like(vccpos)
012900080318     D kpdtip          S                   LIKE(APDtip) inz('A')
013000080410     d wdata           s              8  0
013100080410     d wora            s              6  0
013200080410     d savorfesito     s                   like(§orfesito)
013300080411     d kdae            s                   like(orfdae)
013400080411     d kore            s                   like(orfore)
013500080411     d kfar            s                   like(orffar)
013600080507     d nrnotapda       s              3  0
013700080415
013800080415     d $nonotapda      s               n
013900981216      *
014000980610     D TIT1            C                   CONST('  IMMISSIONE  ')
014100980610     D TIT2            C                   CONST(' MANUTENZIONE ')
014200980610     D TIT3            C                   CONST('INTERROGAZIONE')
014300001024      ***************************************************************
014400001024
014500981216     C     *ENTRY        PLIST
014600981216     C                   PARM                    KPJBA
014700001027     C                   parm                    fior06ds
014800080318     C                   parm                    fior06vds
014900001027     C                   movel     KPJBU         parm01
015000001024
015100080318      * Se vengono passati anche i parametri della testata nota li visualizzo
015200080318     c                   If        %Parms > 2
015300080319     c                   eval      *in20 = *on
015400080318     c                   else
015500080319     c                   eval      *in20 = *off
015600080318     c                   endif
015700080318
015800001027     C                   eval      §06poe = i06poe
015900001027     C                   eval      §06nsr = i06nsr
016000001027     C                   eval      §06nor = i06nor
016100001027     C                   eval      §06nrv = i06nrv
016200001027     C                   eval      §06dai = i06dai
016300001027     C                   eval      §06ori = i06ori
016400001027     C                   eval      §06far = i06far
016500001024
016600001024     C                   if        i06tla = 'C'
016700001024     C                   goto      fine
016800001024     C                   endif
016900001024      * Nel caso di tipo elaborazione RECUPERO o SCRITTURA descrizione non va a cercare i
017000001024      * dati nel subfile
017100001024     C                   select
017200001027
017300001027     C                   when      i06flm = 'R'
017400001027     C     korn          chain     fnorn01l
017500001027     C                   if        %found(fnorn01l)
017600001110     C                   eval      o06err = *blanks
017700001027     C                   else
017800001027     C                   eval      o06err = 'E'
017900001024     C                   endif
018000001027
018100001027     C                   when      i06flm = 'W'
018200001027     C     korn          chain     fnorn01l
018300001027     C                   if        %found(fnorn01l)
018400001027     C                   eval      ORNpue = knmus
018500150515     C                   eval      ORNpon = DUTpou
018600040818     c                   Z-add     *Date         OrnDtr
018700001027     C                   update    fnorn000
018800001027     C                   else
018900001024     C                   clear                   fnorn000
019000001027     C                   eval      ORNpoe = i06poe
019100001027     C                   eval      ORNnsr = i06nsr
019200001027     C                   eval      ORNnor = i06nor
019300001027     C                   eval      ORNnrv = i06nrv
019400001027     C                   eval      ORNdai = i06dai
019500001027     C                   eval      ORNori = i06ori
019600001027     C                   eval      ORNfar = i06far
019700001027     C                   eval      ORNprg = 1
019800001027     C                   eval      ORNpue = knmus
019900150515     C                   eval      ORNpon = DUTpou
020000040818     c                   Z-add     *Date         OrnDtr
020100001024     C                   write     fnorn000
020200001024     C                   endif
020300001024     C
020400001027     C                   other
020500001024
020600001024      * Immissione / Manutenzione / Interrogazione
020700001024     C     i06flm        ifeq      'M'
020800001024     C                   movel     tit1          vtcmod
020900001024     C                   endif
021000001024
021100001024     C     i06flm        ifeq      'V'
021200040402     c     i06flm        oreq      'D'
021300080410     c     i06flm        oreq      'P'
021400001024     C                   movel     tit2          vtcmod
021500001024     C                   endif
021600001024
021700001024     C     i06flm        ifeq      'I'
021800001024     C                   movel     tit3          vtcmod
021900001024     C                   seton                                        08
022000001024     C                   endif
022100001024
022200001027     C                   eval      vtcpoe = i06poe
022300001027     C                   eval      vtnsr  = i06nsr
022400001027     C                   eval      vtnor  = i06nor
022500001027     C                   eval      vtnrv  = i06nrv
022600001027     C                   eval      vtfar  = i06far
022700001027      * Decodifica fase
022800001027     C                   clear                   TIBS02DS
022900001027     C                   movel     'C'           T02mod
023000001027     C                   movel     knsif         t02sif
023100001027     C                   movel     'FAR'         t02cod
023200001027     C                   movel(p)  vtfar         T02ke1
023300001027     C                   call      'TIBS02R'
023400001027     C                   parm                    KPJBA
023500001027     C                   parm                    TIBS02DS
023600001027     C                   movel     t02uni        vtdfar
023700080318
023800080318     c                   exsr      SR_DecTest
023900001024      * Pulizia campi di output
024000001024     C                   clear                   o06f03
024100001024     C                   clear                   o06f12
024200001024     C                   clear                   o06err
024300001024     C                   clear                   o06msg
024400010301     C                   clear                   o06dai
024500010301     C                   clear                   o06ori
024600010301     C                   clear                   o06prg
024700001024
024800971024     C                   Z-ADD     0             RIRI
024900971024     C                   Z-ADD     0             COCO
025000001024
025100001024     C     i06flm        comp      ' '                                0101
025200001024     C   01i06flm        comp      'V'                                0101
025300080410     C   01i06flm        comp      'P'                                0101
025400040402     C   01i06flm        comp      'D'                                0101
025500001024     C     i06flm        comp      'I'                                    02
025600001024
025700001024
025800001024     C                   if        i06flm = 'M'   and
025900001024     C                             wcont  = ' '
026000001024      * Riempimento subfile note
026100001024     C                   exsr      ries02
026200001024     C                   movel     '1'           wcont             1
026300001024     C                   move      i06key        keysav
026400001024     C                   z-add     1             wprimo            4 0
026500001024     C                   endif
026600001024
026700001024     C                   exsr      SUB_NOTE
026800001024
026900001027     C                   endsl
027000001024
027100001024     C     fine          tag
027200001024
027300001024      * Chiusura pgm
027400001024     C     i06tla        ifeq      ' '
027500001024     C                   return
027600001024     C                   else
027700001027     C                   clear                   tibs02ds
027800001024     C                   movel     'C'           t02tla
027900001024     C                   call      'TIBS02R'
028000001024     C                   parm                    kpjba
028100001027     C                   parm                    tibs02ds
028200001024     C                   eval      *inlr = *on
028300001024     C                   end
028400001024
028500001024      **********************************************************************
028600001024      * GESTIONE SUBILE NOTE
028700001024      **********************************************************************
028800980710     C     SUB_NOTE      BEGSR
028900001024
029000001024     C     i06tla        ifne      'C'
029100001024     C     i06flm        ifeq      *blanks
029200001024     C     i06flm        oreq      'V'
029300080410     C     i06flm        oreq      'P'
029400040402     C     i06flm        oreq      'D'
029500001024     C     i06flm        oreq      'M'
029600001024     C     i06flm        oreq      'I'
029700001024      * Caricamento
029800001024     C     i06flm        ifne      'M'
029900001024     C                   exsr      ries02
030000001024     C                   endif
030100001024      * Legge il subfile per proteggere eventualmente campi con key diversa
030200001024      * e per aggiornare la prima riga se key =
030300001024     C                   if        i06flm <> 'I' and *in60 = *on
030400001024     C                   exsr      aggs02
030500001024     C   06              seton                                        63
030600001024     C  n06              setoff                                       63
030700001024     C                   move      i06key        keysav
030800001024     C                   endif
030900040402
031000040402     c                   Eval      savpos = vccpos
031100001024      * Emissione
031200001024     C     emc02         tag
031300040402     C                   setoff                                       093010
031400001024     C                   z-add     vccpos        vccnrr
031500001024     C                   write     fior06t
031600001024
031700001024     C                   write     fior06z
031800001024     C  n60              write     fior06v
031900001024     C                   exfmt     fior06c
032000001219     C                   setoff                                       28
032100001024      * F12=Ritorno
032200001024     C     *inkl         ifeq      *on
032300001024     C                   movel     'S'           o06f12
032400001024     C                   goto      fine2
032500001024     C                   end
032600001024      * F03=Fine
032700001024     C     *inkc         ifeq      *on
032800001024     C                   movel     'S'           o06f03
032900001024     C                   goto      fine2
033000001024     C                   end
033100001024      * F14=Cancella riga
033200001024     C     *inkn         ifeq      *on
033300001024     C                   exsr      deler2
033400001024     C                   goto      emc02
033500001024     C                   end
033600001024      * Rollup
033700001024     C     *IN25         IFEQ      *ON
033800001024     C                   Z-ADD     PRIMA         vccpos
033900980714     C                   EXSR      ROLUP2
034000001024     C                   goto      emc02
034100001024     C                   END
034200001024     C     i06flm        ifeq      'M'
034300001024     C                   exsr      ctrrig2
034400001024     C                   end
034500040402
034600040402     C     i06flm        ifeq      'D'
034700040402     C                   z-add     savpos        wprimo
034800040402     C                   exsr      ctrrig2
034900040402     C                   end
035000040402      * Se ho errore ritorno
035100040402     C                   if        *in09 or *in10
035200040402     C                   goto      emc02
035300040402     C                   endif
035400001024
035500001024      * Se non è manutenzione/immissione e non è stato dato il F6 di conferma   OPPURE
035600001024      * siamo in interrogazione e non è stato dato F12 di ritorno
035700001024      *   RIMANE NELLA STESSA VIDEATA
035800001024     C                   if        (not *in01 and not *inkf) or
035900001024     C                             (*in02 and not *inkl)
036000121203      * controllo se vccpos = 0 in questo caso lo imposto a 1
036100121203      * per evitare errore di sessione/unità
036200121203     c                   IF        VCCpos = 0
036300121203     c                   eval      VCCpos = 1
036400121203     c                   ENDIF
036500001024     C                   goto      emc02
036600001024     C                   endif
036700001024     C                   endif
036800001024      *****************
036900001024      * CONFERMA DATI
037000001024      *****************
037100001024     C     i06flm        ifeq      'V'
037200080410     C     i06flm        oreq      'P'
037300001024     C                   z-add     1             wprimo
037400001024     C                   exsr      ctrrig2
037500001024     C                   end
037600001219
037700001219      * Se ho errore ritorno
037800040402     C                   if        *in09 = *on
037900001219     C                   goto      emc02
038000001219     C                   endif
038100001024
038200001024     C     i06flm        ifeq      *blanks
038300001024     C     i06flm        oreq      'C'
038400001024     C     i06flm        oreq      'V'
038500080410     C     i06flm        oreq      'P'
038600040402     C     i06flm        oreq      'D'
038700001024     C                   exsr      confe2
038800001024     C                   endif
038900001024
039000001024     C     fine2         tag
039100001024
039200001024     C                   endif
039300001024     C                   endsr
039400001024
039500001024      **********************************************************************
039600001024      * RIEMPIMENTO SUBFILE NOTE
039700001024      **********************************************************************
039800980714     C     RIES02        BEGSR
039900001024
040000001024      * Pulizia
040100981216     C                   SETOFF                                       616306
040200001024     C                   WRITE     fior06c
040300981216     C                   SETON                                        6061
040400001024     C                   MOVE      *ZEROS        NRR               5 0
040500001024      * Se passato il numero ORM carica dati da file descrizioni
040600001024     C     i06nor        ifne      *zeros
040700001027     C                   eval      vshpoe = i06poe
040800001027     C                   eval      vshnsr = i06nsr
040900001027     C                   eval      vshnor = i06nor
041000001027     C                   eval      vshnrv = i06nrv
041100001027     C                   eval      vshdai = i06dai
041200001027     C                   eval      vshori = i06ori
041300001027     C                   eval      vshfar = i06far
041400001024     C                   clear                   wwrt              1
041500001024     C     korn1         setll     fnorn01l
041600001027     C                   do        *hival
041700001027     C     korn1         reade     fnorn01l
041800001027     C                   if        %eof(fnorn01l)
041900001027     C                   leave
042000001027     C                   endif
042100001024     C                   if        ORNatb = *blanks
042200001024      * Scrive un record del subfile
042300001024     C                   movel(P)  ORNnot        vsnot
042400001129     C                   movel(P)  ORNsns        vssns
042500001024      * Data
042600001024     C                   Z-ADD     ORNdai        G02inv
042700980714     C                   movel     '3'           G02err
042800980714     C                   CALL      'XSRDA8'
042900980714     C                   PARM                    WLBDAT
043000001026     C                   z-add     g02dat        vsdai
043100001024      * Ora
043200001024     C                   Z-ADD     ORNori        vsori
043300001024      * Fase
043400001024     C                   z-add     ORNfar        vsfar
043500001024      * Utente che ha scritto la nota
043600001024     C                   movel     ORNpue        vspue
043700001024      * Filiale che ha scritto la nota
043800001024     C                   movel     ORNpon        vspon
043900980714      * PROTEZIONE NOTE
044000001114     C     i06poe        ifne      vshpoe
044100001024     C     i06nsr        orne      vshnsr
044200001024     C     i06nor        orne      vshnor
044300001024     C     i06nrv        orne      vshnrv
044400001024     C     i06flm        oreq      'M'
044500001024     C                   seton                                        08
044600001024     C                   movel     '1'           vshin8
044700001024     C                   else
044800001024     C                   setoff                                       08
044900001024     C                   movel     ' '           vshin8
045000001024     C                   endif
045100001024      * 02 on protegge il campo note / descrizione
045200001024     C                   if        *in02 = *on     or
045300001024      * Fase differente
045400001024     C                             ORNfar <> i06far
045500061006      * blocco nota
045600061006     c                             or ornftr = 'S'
045700001024     C                   seton                                        08
045800001024     C                   move      '1'           vshin8
045900001024     C                   endif
046000001109
046100001024     C                   add       1             nrr
046200001024     C                   write     fior06s
046300001024     C                   clear                   wwrt
046400001024     C                   clear                   vshin8
046500001109     C                   setoff                                       08
046600001024     C                   update    fnorn000
046700001024     C                   endif
046800001024     C                   enddo
046900080415     c                   eval      $nonotapda = *off
047000080507     c                   clear                   nrnotapda
047100080410      * se richiamato con 'P' devo impostare la nota inserita sul PDA
047200080410      * per facilitare la quadratura distinte
047300080415      * se ho trovato almeno una nota
047400080415     c                   if        i06flm = 'P' and nrr > 0
047500080410     c                   exsr      sr_notapda
047600080410     c                   endif
047700080507      * se richiamato con 'P' e non ho caricato nessuna nota da PDA
047800080507      * imposto il flag per caricare bene il subfile
047900080507     c                   if        i06flm = 'P' and nrnotapda = 0
048000080507     c                   eval      $nonotapda = *on
048100080507     c                   endif
048200080415      * se richiamato con 'P' e nessuna nota caricata imposto flag per
048300080415      * caricare bene il subfile vuoto
048400080415     c                   if        i06flm = 'P' and nrr = 0
048500080415     c                   eval      $nonotapda = *on
048600080415     c                   endif
048700080410
048800001024     C                   end
048900001024     C                   z-add     1             vccpos
049000001024      * Carica righe vuote
049100001024     C     i06flm        ifne      'I'
049200001024     C                   z-add     nrr           max2              5 0
049300001024     C                   exsr      rolup2
049400001024     C                   else
049500001024     C                   seton                                        0663
049600001024     C                   end
049700001024     C     nrr           ifeq      *zeros
049800001024     C                   setoff                                       60
049900001024     C                   end
050000001024
050100001024     C                   endsr
050200001024      **********************************************************************
050300001024      * ROLLUP NOTE
050400001024      **********************************************************************
050500980714     C     ROLUP2        BEGSR
050600001024
050700001024     C                   Z-ADD     MAX2          NRR
050800080318     C     MAX2          DIV       13            WDIV              5 0
050900980714     C                   MVR                     WRES              3 0
051000980714     C                   ADD       1             WRES
051100001024     C                   CLEAR                   vsnot
051200001129     C                   clear                   vssns
051300001024     C                   CLEAR                   vsdai
051400001024     C                   CLEAR                   vsori
051500001024     C                   CLEAR                   vsfar
051600001024     C                   CLEAR                   vspue
051700001024     C                   CLEAR                   vspon
051800080318     C     WRES          DO        13
051900001024     C                   ADD       1             NRR
052000001024      * Se modalità=solo immissione memorizza nel sfl anche la key parziale
052100001024      * ricevuta
052200001024     C                   CLEAR                   vsnot
052300001027     C                   eval      vshpoe = i06poe
052400001027     C                   eval      vshnsr = i06nsr
052500001027     C                   eval      vshnor = i06nor
052600001027     C                   eval      vshnrv = i06nrv
052700001027     C                   eval      vshdai = i06dai
052800001027     C                   eval      vshori = i06ori
052900001027     C                   eval      vshfar = i06far
053000001024
053100001024     C                   WRITE     fior06s
053200001026     C                   ENDdo
053300001024
053400001024     C     NRR           SUB       16            vccpos
053500001024     C                   Z-ADD     NRR           MAX2
053600001024
053700980714     C                   ENDSR
053800001024      **********************************************************************
053900001024      * ANNULLA UNA RIGA NEL SUBFILE
054000001024      **********************************************************************
054100980715     C     delEr2        BEGSR
054200001024
054300980714     C                   EXSR      RIGA
054400001024      * Solo se non e' campo protetto
054500001024     C     $rig          CHAIN     fior06s                            30
054600001024     C                   if        vshin8 = ' ' and *in30 = *off
054700980714     C     $RIG          IFNE      0
054800980714     C                   CLEAR                   s2ds
054900001027     C                   eval      vshpoe = i06poe
055000001027     C                   eval      vshnsr = i06nsr
055100001027     C                   eval      vshnor = i06nor
055200001027     C                   eval      vshnrv = i06nrv
055300001027     C                   eval      vshdai = i06dai
055400001027     C                   eval      vshori = i06ori
055500001027     C                   eval      vshfar = i06far
055600980720     C                   MOVE      s2ds          S2SAV2
055700001024     C     MAX2          CHAIN     fior06s                            30
055800980720     C                   MOVE      s2ds          S2SAV
055900981216     C                   Z-SUB     MAX2          $MAX2             5 0
056000980714     C                   Z-SUB     $RIG          $RIG              5 0
056100980714     C                   ADD       1             $RIG
056200981216     C                   ADD       1             $MAX2
056300981216     C     $MAX2         DO        $RIG          Y                 5 0
056400980714     C                   Z-SUB     Y             I                 5 0
056500980715     C                   MOVE      S2SAV2        s2ds
056600001024     C     s2sav2        ifne      *blanks
056700001114     C     i06poe        ifne      vshpoe
056800001024     C     i06nsr        orne      vshnsr
056900001024     C     i06nor        orne      vshnor
057000001024     C     i06nrv        orne      vshnrv
057100001027     C     i06dai        orne      vshdai
057200001027     C     i06ori        orne      vshori
057300001024     C     i06far        orne      vshfar
057400001024     C                   seton                                        08
057500001024     C                   movel     '1'           vshin8
057600001024     C                   else
057700001024     C                   setoff                                       08
057800001024     C                   movel     ' '           vshin8
057900001024     C                   end
058000001024
058100001024     C  N30              UPDATE    fior06s
058200001024     C                   endif
058300001024     C                   setoff                                       08
058400001024     C                   movel     ' '           vshin8
058500001024     C     I             CHAIN     fior06s                            30
058600980720     C                   MOVE      s2ds          S2SAV3
058700980715     C                   MOVE      S2SAV         S2SAV2
058800980715     C                   MOVE      S2SAV3        S2SAV
058900980714     C                   END
059000980714     C                   END
059100001024     C                   end
059200001024     C                   setoff                                       08
059300001024
059400980714     C                   ENDSR
059500001024      **********************************************************************
059600001024      * DETERMINA NRR DEL SUBFILE
059700001024      **********************************************************************
059800971024     C     RIGA          BEGSR
059900001024
060000971024     C                   Z-ADD     PRIMA         $RIG              5 0
060100971024     C                   MOVE      NRG           R$$
060200971024     C                   ADD       RIRI          $RIG
060300971024     C                   SUB       6             $RIG
060400971024     C     $RIG          IFLT      0
060500971024     C     RIRI          ORLT      6
060600971024     C                   Z-ADD     0             $RIG
060700971024     C                   END
060800001024
060900971024     C                   ENDSR
061000001024      **********************************************************************
061100001024      * AGGIORNAMENTO SFL NOTE IN MODALITA' SOLO IMMISSIONE
061200001024      **********************************************************************
061300001024     C     aggs02        begsr
061400001024
061500001024     C                   setoff                                       06
061600001024     C                   z-add     1             nrr
061700001024     C                   move      *zeros        vccpos
061800001024     C     nrr           chain     fior06s                            30
061900001024     C     *in30         doweq     *off
062000001024      * Se riga non protetta e' la prima che deve essere utlizzata
062100080411     C**!!!VsHIN8        IFEQ      ' '
062200080411     C**!!!vsnot         andeq     *blanks
062300080411     c                   if        vshin8 = *blanks and ((vsnot = *blanks and
062400080411     c                             i06flm <> 'P') or (vsnot <> *blanks and
062500080415     c                             i06flm = 'P') or $nonotapda)
062600001024     C     vccpos        ifeq      *zeros
062700001024     C                   z-add     nrr           vccpos
062800001024     C                   z-add     nrr           wprimo            4 0
062900001024      * Se non valorizzata la chiave ma è nella gestione note
063000001024      * valorizza i campi della data ora e fase nella prima riga
063100001024      * con data e ora del momento
063200001024     C                   if        vsnot = *blanks
063300001024     C                   Z-ADD     wdtgio        vsdai
063400001024      * Ora
063500010301     C                   z-add     i06ori        vsori
063600010301     C************       Z-ADD     wora          vsori
063700001024      * Fase
063800001024     C                   movel     i06far        vsfar
063900001024      * Profilo utente
064000001024     C                   movel     knmus         vspue
064100001024      * Filiale
064200150515     C                   movel     DUTpou        vspon
064300001129      * Flag note interne/stampa
064400001129     C                   movel     'I'           vssns
064500001024     C                   endif
064600001024      * Se ci sono già records nel sfl con quella chiave disabilita rollup
064700001024     C                   seton                                        06
064800001024     C                   end
064900001024     C                   end
065000001024     C     VsHIN8        IFne      ' '
065100001024     C                   seton                                        08
065200001024     C                   endif
065300001024     C                   update    fior06s
065400001024     C                   setoff                                       08
065500001024     C                   movel     ' '           vshin8
065600001024     C                   add       1             nrr
065700001024     C     nrr           chain     fior06s                            30
065800001024     C                   enddo
065900001024     C                   setoff                                       08
066000001024     C     vccpos        ifeq      *zeros
066100001024     C     max2          add       1             wprimo
066200980714     C                   EXSR      ROLUP2
066300001024     C                   end
066400001024
066500980714     C                   ENDSR
066600001024      **********************************************************************
066700001024      * CONFERMA DATI SU DATA BASE
066800001024      **********************************************************************
066900001024     C     confe2        begsr
067000001024
067100001024      * Se esistono dei records ancora da vedere con la chiave passata li cancella
067200001024      * (se modalità "C" non possono esserci records da cancellare perchè
067300001024      * in questo caso è in inserimento di nuovi records)
067400001024     C     i06flm        ifne      'C'
067500001024     C     korn1         setll     fnorn01l
067600001024     C     korn1         reade     fnorn01l                               30
067700001024     C     *in30         doweq     *off
067800001024      * Se la nota e' manutenzionabile la cancella
067900061006     C                   If        ORNfar = i06far and ornftr <> 'S'
068000001024     C                   delete    fnorn000
068100001024     C                   endif
068200001024     C     korn1         reade     fnorn01l                               30
068300001024     C                   enddo
068400001024     C                   endif
068500001024      * Scrive records a video
068600001024      * Cerce l'ultimo record valido.
068700981216     C     MAX2          DOWGT     *ZERO
068800001024     C     MAX2          CHAIN     fior06s                            30
068900001024     C     vsnot         COMP      *BLANK                             3030
069000981216     C  N30              SUB       1             MAX2
069100980714     C  N30              END
069200001024
069300001024     C     max2          ifgt      *zeros
069400001024
069500001024     C                   clear                   fnorn000
069600001027     C                   eval      ORNpoe = i06poe
069700001027     C                   eval      ORNnsr = i06nsr
069800001027     C                   eval      ORNnor = i06nor
069900001027     C                   eval      ORNnrv = i06nrv
070000001027     C                   eval      ORNdai = i06dai
070100001027     C                   eval      ORNori = i06ori
070200001027     C                   eval      ORNfar = i06far
070300001024
070400001024     C                   z-add     0             ORNprg
070500001024     C
070600001024     C                   DO        MAX2          NRR
070700001024      * Per ogni record di subfile scrive due records di fnorn00f
070800001024     C     NRR           CHAIN     fior06s                            30
070900001024      * Se record non protetto elabora
071000001024     C                   if        vshin8 = ' '
071100010301      * Se modalità "C", e cambia chiave azzera il progressivo
071200001024     C     i06flm        ifeq      'C'
071300001024     C     vshkey        andne     vshsav
071400001024     C                   move      *zeros        ORNprg
071500001024     C                   move      vshkey        vshsav
071600001024     C                   end
071700001024      * Se record vuoto lo salta
071800001024     C     vsnot         ifne      *blanks
071900001024     C                   add       1             ORNprg
072000001027     C                   eval      ORNnot = vsnot
072100001129     C                   eval      ORNsns = vssns
072200010220      * Se modalità "C" il P.O. emissione e la fase sono
072300001024      * memorizzati nel subfile
072400001024     C     i06flm        ifeq      'C'
072500001027     C                   eval      ORNpoe = vshpoe
072600001027     C                   eval      ORNfar = vshfar
072700010220     C                   if        vshfar = *zeros
072800010220     C                   eval      ORNfar = i06far
072900010220     C                   endif
073000001024     C                   end
073100001024      * Valorizza il campo data con la data passata in chiave
073200980714      * oppure con la data e l'ora del momnento oppure se record gia' esistente
073300980714      * con quella del subfile
073400001024     C                   if        vsdai   <> *zeros
073500001024     C                   Z-ADD     vsdai         G02dat
073600980714     C                   movel     ' '           G02err
073700980714     C                   CALL      'XSRDA8'
073800980714     C                   PARM                    WLBDAT
073900001024     C                   z-add     g02inv        ORNdai
074000001024     C                   z-add     vsori         ORNori
074100001024     C                   movel     vsfar         ORNfar
074200001024     C                   move      vspue         ORNpue
074300001024     C                   movel     vspon         ORNpon
074400010220     C                   if        vsfar = *zeros
074500010220     C                   eval      ORNfar = i06far
074600010220     C                   endif
074700001024     C                   else
074800001024     C                   if        i06dai  <> *zeros
074900001024     C                   z-add     i06dai        ORNdai
075000001024     C                   z-add     i06ori        ORNori
075100001024     C                   else
075200001024     C                   z-add     wora          ORNori
075300001024     C                   z-add     dateu         ORNdai
075400001024     C                   endif
075500001024      * Valorizza profilo + P.O.
075600001024     C                   move      knmus         ORNpue
075700150515     C                   z-add     DUTpou        ORNpon
075800001024     C                   endif
075900040818     c                   Z-add     *Date         OrnDtr
076000001024     C                   write     fnorn000
076100010301      *---
076200010301     C                   eval      o06dai = orndai
076300010301     C                   eval      o06ori = ornori
076400010301     C                   eval      o06prg = ornprg
076500010301      *---
076600001024     C                   endif
076700001024     C                   endif
076800001024     C                   enddo
076900001024
077000001024     C                   end
077100001024
077200001024     C                   endsr
077300001024      **********************************************************************
077400001024      * CONTROLLI IN MODALITA' DI SOLA IMMISSIONE O VARIAZIONE
077500001024      **********************************************************************
077600001024     C     ctrrig2       begsr
077700001024
077800001024     C     wprimo        chain     fior06s                            30
077900001024     C     vsnot         ifeq      *blanks
078000001024     C     wprimo        add       1             wnrr
078100001024     C     wnrr          chain     fior06s                            30
078200001024     C     *in30         doweq     *off
078300001024     C     i06flm        ifeq      'M'
078400001024     C     vshpoe        andeq     i06poe
078500001024     C     vshnsr        andeq     i06nsr
078600001024     C     vshnor        andeq     i06nor
078700001024     C     vshnrv        andeq     i06nrv
078800001024     C     i06flm        oreq      'V'
078900040402     C     i06flm        oreq      'D'
079000001024     C     vsnot         ifne      *blanks
079100001024     C                   seton                                        30
079200001024     C                   movel     vsnot         w035a
079300001129     C                   movel     vssns         wflag
079400001024     C                   clear                   vsnot
079500001129     C                   clear                   vssns
079600001024     C                   update    fior06s
079700001024     C     wprimo        chain     fior06s                            31
079800001024     C     w035a         ifne      *blanks
079900001024     C                   movel     w035a         vsnot
080000001129     C                   movel     wflag         vssns
080100001024     C                   endif
080200001024     C                   update    fior06s
080300001024     C                   endif
080400001024
080500001024     C                   add       1             wnrr
080600001024     C  n30wnrr          chain     fior06s                            30
080700001024     C                   else
080800001024     C                   seton                                        30
080900001024     C                   end
081000001024     C                   enddo
081100001024
081200001219     C                   else
081300001219
081400001219     C                   z-add     wprimo        wnrr
081500001219     C     *in30         doweq     *off
081600001219     C     vshpoe        ifeq      i06poe
081700001219     C     vshnsr        andeq     i06nsr
081800001219     C     vshnor        andeq     i06nor
081900001219     C     vshnrv        andeq     i06nrv
082000040402      * tipo richiamo = 'D'
082100040402      * immettere almeno 10 caratteri
082200040402     c                   If        I06Flm = 'D' and vsnot <> *Blanks
082300040402     c                             and wnrr = savpos
082400040402     c                   If        %subst(vsnot:11:24) = *Blanks
082500040402     c                   Eval      *In10 = *On
082600040402     c                   Eval      *In28 = *On
082700040402     c                   Eval      *In30 = *On
082800040402     c                   Eval      VccMsg = 'immettere una nota valida'
082900040402     c                   Update    Fior06s
083000040402     c                   EndIf
083100040402     c                   EndIf
083200001219      * Controllo se ok il flag tipo nota (interna o stampa)
083300001219     C                   if        vssns = *blanks  and vsnot <> *blanks
083400040402     c                             and not *in28
083500001219     C                   seton                                        280930
083600001219     C                   eval      vccmsg = 'Flag stp.nota non valido'
083700001219     C                   update    fior06s
083800001219     C                   endif
083900001219     C                   endif
084000001219     C                   add       1             wnrr
084100001219     C  n30wnrr          chain     fior06s                            30
084200001219     C                   enddo
084300001219
084400001219     C                   endif
084500001024
084600040402     C     endctrrig     endsr
084700080318
084800080318      **********************************************************************
084900080318      * ROUTINE di decodifica dati della testata
085000080318      **********************************************************************
085100080318     C     SR_DecTest    BEGSR
085200080319      *
085300080319      * se valorizzato indicatore 20 valorizzo i campi del video
085400080319     c                   IF        *in20
085500080319      * verifico se ho ricevuto solo la causale e basta
085600080319     c                   if        v06car <> *blank
085700080319     c                   movel(p)  v06car        vtccar
085800080319     c                   movel(p)  v06dcar       vtdcar
085900080319      * verifico se campo valorizzato
086000080319     c                   If        v06car <> *blanks
086100080319     c                   eval      *in14 = *on
086200080319     c                   else
086300080319     c                   eval      *in14 = *off
086400080319     c                   endif
086500080319      * verifico se si sono  altri dati valorizzati
086600080319     c                   eval      *in20 = (v06rsr <> *blanks)
086700080319     c                   endif
086800080319
086900080319     c                   endif
087000080319      * se recupero da DS
087100080319     c                   If        *in20
087200080319     c                   eval      *in13 = *on
087300080318     c                   movel(p)  v06rsr        vtcrsr
087400080318     c                   movel(p)  v06rso        vtcrso
087500080318
087600080318     c                   movel(p)  v06ndc        vtcndc
087700080318      * Inversione data distinta  ORM
087800080318     c                   clear                   wlbdat
087900080318     c                   z-add     v06ddc        G02inv
088000080318     c                   movel     '3'           G02err
088100080318     c                   call      'XSRDA8'
088200080318     c                   parm                    wlbdat
088300080318     c                   z-add     G02dat        vtcddc
088400080318      * verifico se campo valorizzato
088500080318     c                   If        vtcndc <> *zeros
088600080318     c                   eval      *in15 = *on
088700080318     c                   else
088800080318     c                   eval      *in15 = *off
088900080318     c                   endif
089000080318
089100080318
089200080318     c                   movel(p)  v06pdr        vtcpdr
089300080318     c                   movel(p)  v06dpdr       vtdpdr
089400080318
089500080318     c                   else
089600080318
089700080319      * Se i dati non sono stati passati dalla DS e quindi il 20 spento recupero dai file
089800080318
089900080318      * aggancio FNORM
090000080318     c     korn1         chain     fnorm01l
090100080318     c                   if        %found(fnorm01l)
090200080318     c                   movel     ormrsr        vtcrsr
090300080318     c                   movel     ormrso        vtcrso
090400080318     c                   eval      *in13 = *on
090500080318      * verifico la distinta
090600080318     c                   eval      *in15 = *off
090700080318     c                   if        ormndc > 0
090800080318      * aggancio la distinta
090900080319     c     kdst          chain     fidst09l
091000080318      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
091100080319     c                   if        not %found(fidst09l)
091200080318     c                   clear                   fnlv55ds
091300080318     c                   eval      d55tpt = '6'
091400080318     c                   move      ormpor        d55lin
091500080318     c                   eval      d55drf = ormddc
091600080318     c                   call      'FNLV55R'
091700080318     c                   parm                    fnlv55ds
091800080318     c                   if        d55err = *blanks and d55tfa <> d55lin
091900080318     c                   move      d55tfa        kfgs
092000080319     c     kfidst        chain     fidst09l
092100080318     c                   endif
092200080318     c                   endif
092300080319     c                   If        %found(Fidst09l)
092400080318     c                   eval      *in15 = *on
092500080318     c                   z-add     ormndc        vtcndc
092600080318      * Inversione data distinta  ORM
092700080318     c                   clear                   wlbdat
092800080318     c                   z-add     ormddc        G02inv
092900080318     c                   movel     '3'           G02err
093000080318     c                   call      'XSRDA8'
093100080318     c                   parm                    wlbdat
093200080318     c                   z-add     G02dat        vtcddc
093300080318      * recupero il padroncino
093400080318     c                   movel     dstpdr        vtcpdr
093500080318     c     kfapd         chain     fiapd01L
093600080318     c                   if        %found(fiapd01l)
093700080318     c                   movel     APDrsc        vtdpdr
093800080318     C                   endif
093900080318
094000080318     C                   endif
094100080318
094200080318     C                   endif
094300080318
094400080318     C                   endif
094500080318
094600080319      * verifico la causale mancato ritiro solo se non è stata valorizzata
094700080320     c                   if        %parms <= 2
094800080318     c                   eval      *in14 = *off
094900080318     c                   exsr      sr_carcau
095000080319     c                   endif
095100080318
095200080318     c                   endif
095300080318
095400080318     c                   Endsr
095500080318
095600080318      **********************************************************************
095700080318      * Cerca causale chiusura e decodifica
095800080318      **********************************************************************
095900080318     C     Sr_Carcau     begsr
096000080318
096100080318     C     kfnorf        chain     fnorf01L
096200080318     C                   if        %found(fnorf01l)
096300080318     C                   eval      vtccar= orfcar
096400080318     C                   endif
096500080318
096600080318     C                   clear                   dcmr
096700080318     C                   clear                   TIBS02DS
096800080318     C                   movel     'C'           T02mod
096900080318     C                   movel     knsif         t02sif
097000080318     C                   movel     'CMR'         t02cod
097100080318     C                   movel(p)  vtccar        T02ke1
097200080318     C                   call      'TIBS02R'
097300080318     C                   parm                    KPJBA
097400080318     C                   parm                    TIBS02DS
097500080318     C                   movel     t02uni        dcmr
097600080318     C                   movel     d§cmrdes      vtdcar
097700080318
097800080318     C                   eval      *in14 = (vtccar <> *blanks)
097900080318
098000080318     C                   endsr
098100080410
098200080410      *------------------------------------------------------------------------*
098300080410      *  Recupero l'ultima nota inserita da PDA
098400080410      *------------------------------------------------------------------------*
098500080410     c     sr_notapda    begsr
098600080410
098700080411      * cerco la fase precedente dell'orm
098800080411     c     kfnorf        setll     fnorf01l
098900080411     c     kfnorm        readpe    fnorf01l
099000080411     c                   if        %eof(fnorf01l)
099100080411     c                   leavesr
099200080411     c                   endif
099300080411      * imposto la chiave per leggere tutte le fasi arrivate da PDA
099400080411     c                   eval      kdae = orfdae
099500080411     c                   eval      kore = orfore
099600080411     c                   eval      kfar = orffar
099700080410      * cerco l'ultima fase dell'orm
099800080410     c                   clear                   dorf01
099900080410     c                   clear                   savorfesito
100000080411     c     kfnorf1       setll     fnorf01l
100100080410     c                   do        *hival
100200080411     c     kfnorf1       reade     fnorf01l
100300080410     c                   if        %eof(fnorf01l)
100400080410     c                   leave
100500080410     c                   endif
100600080410     c                   eval      dorf01 = orfflo
100700080410      * prendo l'ultima fase a parità di data e ora
100800080410      * lo faccio controllando l'esito del PDA memorizzato in orfflo
100900080410     c                   if        §orfesito <> *blanks
101000080410     c                   if        §orfesito < savorfesito and
101100080410     c                             savorfesito > *zeros
101200080410     c                   iter
101300080410     c                   endif
101400080410     c                   eval      savorfesito = §orfesito
101500080410     c                   endif
101600080410     c                   enddo
101700080410      * imposto i dati per agganciare la nota
101800080410     c                   if        savorfesito = *blanks
101900080410     c                   leavesr
102000080410     c                   endif
102100080410     c                   eval      wdata = %dec(%subst(§orfesito:1:8):8:0)
102200080410     c                   eval      wora  = %dec(%subst(§orfesito:9:6):6:0)
102300080410      * aggancio la nota
102400080410     c     korn2         chain     fnorn01l
102500080410     c                   if        not %found(fnorn01l)
102600080410     c                   leavesr
102700080410     c                   endif
102800080410      * scrivo un rcd nel subfile con la nota inserita da PDA
102900080410     c                   eval      vshpoe = ornpoe
103000080410     c                   eval      vshnsr = ornnsr
103100080410     c                   eval      vshnor = ornnor
103200080410     c                   eval      vshnrv = ornnrv
103300080410     c                   eval      vshdai = i06dai
103400080410     c                   eval      vshori = i06ori
103500080410     c                   eval      vshfar = i06far
103600080410      * nota scritta da PDA
103700080410     c                   eval      vsnot = ornnot
103800080410      * nota interna
103900080410     c                   eval      vssns = 'I'
104000080410      * data e ora
104100080410     c                   z-add     i06dai        g02inv
104200080410     c                   movel     '3'           g02err
104300080410     c                   call      'XSRDA8'
104400080410     c                   parm                    wlbdat
104500080410     c                   z-add     g02dat        vsdai
104600080410     c                   z-add     i06ori        vsori
104700080410     c                   z-add     i06far        vsfar
104800080410      * utente
104900080410     c                   eval      vspue = knmus
105000080410      * filiale
105100150515     c                   eval      vspon = DUTpou
105200080410     c                   add       1             nrr
105300080507     c                   add       1             nrnotapda
105400080410     c                   clear                   vshin8
105500080410     c                   setoff                                       08
105600080410     c                   write     fior06s
105700080410
105800080410     c                   endsr
105900080410
106000001024      **********************************************************************
106100001024      * ROUTINE INIZIALE
106200001024      **********************************************************************
106300951009     C     *INZSR        BEGSR
106400001024
106500971016     C                   TIME                    W0140            14 0
106600001024      * UDATE IN GGMMAAAA
106700971016     C                   MOVE      W0140         WDTGIO            8 0
106800001026     C                   MOVEL     W0140         WORA              6 0
106900001024
107000001024      * UDATE IN AAAAMMGG
107100980619     C                   Z-ADD     WDTGIO        G02DAT
107200980619     C                   MOVEL     *BLANK        G02ERR
107300980619     C                   CALL      'XSRDA8'
107400980619     C                   PARM                    WLBDAT
107500980619     C                   MOVEL     G02INV        DATEU             8 0
107600150515
107700150515     C                   Z-ADD     1             CODUT             1 0
107800150515     c     *dtaara       define    §azute        AZUTEds
107900150515     c     *dtaara       define    §datiute      dDATIUTE
108000150515     c                   in(E)     *dtaara
108100150515     c                   if        %ERROR or RSUT = *blanks
108200150515     c                   clear                   Tibs34Ds
108300150515     c                   call      'TIBS34R'
108400150515     c                   parm                    Tibs34Ds
108500150515     c                   in        *dtaara
108600150515     c                   endif
108700001024
108800010723      * Da sede non imposto il p.o. di gestione
108900010723     C                   if        simfel = 0
109000010723     C                   eval      *in12 = *on
109100010723     C                   else
109200010723     C                   eval      *in12 = *off
109300010723     C                   endif
109400080320      * valorizzo la libreria file fidst
109500080320
109600080320     c                   If        %subst(knsif:7:1) = 'P'
109700080320     c                   Eval      openthis = 'FILTRAPRD/FIDST09L'
109800080320     c                   Else
109900080320     c                   Eval      openthis = 'FILTRA201/FIDST09L'
110000080320     c                   EndIf
110100080320     c                   open      fidst09l
110200010723
110300001024     C     *like         define    i06key        keysav
110400001026     C     *like         define    vshkey        vshsav
110500001024     C     *like         define    nrr           wnrr
110600001024
110700001024     C     korn          klist
110800001024     C                   kfld                    i06poe
110900001024     C                   kfld                    i06nsr
111000001024     C                   kfld                    i06nor
111100001024     C                   kfld                    i06nrv
111200001026     C                   kfld                    i06dai
111300001026     C                   kfld                    i06ori
111400001024     C                   kfld                    i06far
111500001024     C                   kfld                    prog
111600001024     C                   z-add     1             prog
111700001024     C
111800001024
111900001024     C     korn1         klist
112000001024     C                   kfld                    i06poe
112100001024     C                   kfld                    i06nsr
112200001024     C                   kfld                    i06nor
112300001024     C                   kfld                    i06nrv
112400080410
112500080410     c     korn2         klist
112600080410     c                   kfld                    orfpoe
112700080410     c                   kfld                    orfnsr
112800080410     c                   kfld                    orfnor
112900080410     c                   kfld                    orfnrv
113000080410     c                   kfld                    wdata
113100080410     c                   kfld                    wora
113200080410     c                   kfld                    orffar
113300001027
113400001027     C     Ktbe          klist
113500001027     C                   kfld                    ktbcod
113600001027     C                   kfld                    ktbke1
113700001027     C                   kfld                    ktbke2
113800080318
113900080318     C     Kdst          klist
114000080318     C                   kfld                    ormpor
114100080318     C                   kfld                    catk
114200080318     C                   kfld                    ormndc
114300080318
114400080318     C     Kfidst        klist
114500080318     C                   kfld                    Kfgs
114600080318     C                   kfld                    catk
114700080318     C                   kfld                    ormndc
114800080318
114900080318     C     Kfapd         klist
115000080318     C                   kfld                    kpdtip
115100080318     C                   kfld                    dstpdr
115200001024
115300080318     C     Kfnorf        klist
115400080318     C                   kfld                    ormpoe
115500080318     C                   kfld                    ormnsr
115600080318     C                   kfld                    ormnor
115700080318     C                   kfld                    ormnrv
115800080318     C                   kfld                    ormdfo
115900080318     C                   kfld                    ormofo
116000080318     C                   kfld                    ormfao
116100080411
116200080411     c     kfnorf1       klist
116300080411     c                   kfld                    ormpoe
116400080411     c                   kfld                    ormnsr
116500080411     c                   kfld                    ormnor
116600080411     c                   kfld                    ormnrv
116700080411     c                   kfld                    kdae
116800080411     c                   kfld                    kore
116900080411     c                   kfld                    kfar
117000080411
117100080411     c     kfnorm        klist
117200080411     c                   kfld                    ormpoe
117300080411     c                   kfld                    ormnsr
117400080411     c                   kfld                    ormnor
117500080411     c                   kfld                    ormnrv
117600080318
117700951009     C                   ENDSR
