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
003900001027     FAZORG01L  IF   E           K DISK
004000001027     FTNTBE01L  IF   E           K DISK
004100001024
004200001024      *------------------------------------------------------------------------*
004300001027     D UT§DSE0F      E DS
004400980619     D cncr80        E DS
004500981026     D dtld          E DS
004600080318     D DCMR          E DS
004700080410     d dorf01        e ds
004800001027     D TIBS02DS      E DS
004900001027     D FIOR06DS      E DS
005000080318     D FIOR06VDS     E DS
005100080318     d fnlv55ds      e ds
005200951009     D KPJBA         E DS
005300001027
005400001027     D Parm01          DS
005500001027     D  ppoe                          3  0
005600001027     D  pnor                          7  0
005700001027     D  pnsr                          2  0
005800001027     D  pnrv                          2  0
005900001027     D  psce                          1
006000001027     D  pfgs                          3  0
006100001027
006200001027     D ktbcod          S                   LIKE(TBEcod)
006300001027     D ktbke1          S                   LIKE(TBEke1)
006400001027     D ktbke2          S                   LIKE(TBEke2)
006500001024
006600001024     D dn06DS          DS
006700971023     D  NRG                  370    370
006800971023     D  PRIMA                378    379B 0
006900001024
007000971024     D CURSOR          DS
007100971024     D  RIRI                   1      2B 0
007200971024     D  R$$                    2      2
007300971024     D  COCO                   3      4B 0
007400971024     D  C$$                    4      4
007500001024
007600001024     D                 DS
007700001024     D  §06poe                 5      7  0
007800001024     D  §06nsr                 8      9  0
007900001024     D  §06nor                10     16  0
008000001024     D  §06nrv                17     18  0
008100001027     D  §06dai                19     26  0
008200001027     D  §06ori                27     32  0
008300001027     D  §06far                33     35  0
008400001027     D  i06key                 1     35
008500001024
008600980714     D s2ds            DS
008700001114     D  vshkey                 1     31
008800001114     D  vshpoe                 1      3  0
008900001114     D  vshnsr                 4      5  0
009000001114     D  vshnor                 6     12  0
009100001114     D  vshnrv                13     14  0
009200001114     D  vshdai                15     22  0
009300001114     D  vshori                23     28  0
009400001114     D  vshfar                29     31  0
009500001114     D  vsdai                 32     39  0
009600001114     D  vsori                 40     45  0
009700001114     D  vsfar                 46     48  0
009800001114     d  vsnot                 49     83
009900001114     D  vspue                 84     93
010000001114     D  vspon                 94     96  0
010100980422     D                 DS
010200980608     D  w035a                  1     35
010300951009     D                SDS
010400971016     D  VTCPGM                 1     10
010500980609      *
010600980609     D WLBDAT          DS                  INZ
010700980609     D  G02DAT                 1      8  0
010800980609     D  G02INV                 9     16  0
010900980609     D  G02ERR                17     17
011000980609     D  G02TGI                18     22  0
011100980720      *
011200980720     D s2sav           s                   LIKE(s2ds)
011300980720     D s2sav2          s                   LIKE(s2ds)
011400980720     D s2sav3          s                   LIKE(s2ds)
011500001024     D prog            s                   LIKE(ORNprg)
011600080318     D kfgs            s                   LIKE(ORmpor)
011700001129     D wflag           S              1
011800080320     D openthis        s             21
011900980609      *
012000080318     d catk            s              1  0 inz(4)
012100981216     D x               s              2  0
012200990610     D t               s              2  0
012300990610     D wtesto          s              3
012400040402     d savpos          s                   like(vccpos)
012500080318     D kpdtip          S                   LIKE(APDtip) inz('A')
012600080410     d wdata           s              8  0
012700080410     d wora            s              6  0
012800080410     d savorfesito     s                   like(§orfesito)
012900080411     d kdae            s                   like(orfdae)
013000080411     d kore            s                   like(orfore)
013100080411     d kfar            s                   like(orffar)
013200080507     d nrnotapda       s              3  0
013300080415
013400080415     d $nonotapda      s               n
013500981216      *
013600980610     D TIT1            C                   CONST('  IMMISSIONE  ')
013700980610     D TIT2            C                   CONST(' MANUTENZIONE ')
013800980610     D TIT3            C                   CONST('INTERROGAZIONE')
013900001024      ***************************************************************
014000001024
014100981216     C     *ENTRY        PLIST
014200981216     C                   PARM                    KPJBA
014300001027     C                   parm                    fior06ds
014400080318     C                   parm                    fior06vds
014500001027     C                   movel     KPJBU         parm01
014600001024
014700080318      * Se vengono passati anche i parametri della testata nota li visualizzo
014800080318     c                   If        %Parms > 2
014900080319     c                   eval      *in20 = *on
015000080318     c                   else
015100080319     c                   eval      *in20 = *off
015200080318     c                   endif
015300080318
015400001027     C                   eval      §06poe = i06poe
015500001027     C                   eval      §06nsr = i06nsr
015600001027     C                   eval      §06nor = i06nor
015700001027     C                   eval      §06nrv = i06nrv
015800001027     C                   eval      §06dai = i06dai
015900001027     C                   eval      §06ori = i06ori
016000001027     C                   eval      §06far = i06far
016100001024
016200001024     C                   if        i06tla = 'C'
016300001024     C                   goto      fine
016400001024     C                   endif
016500001024      * Nel caso di tipo elaborazione RECUPERO o SCRITTURA descrizione non va a cercare i
016600001024      * dati nel subfile
016700001024     C                   select
016800001027
016900001027     C                   when      i06flm = 'R'
017000001027     C     korn          chain     fnorn01l
017100001027     C                   if        %found(fnorn01l)
017200001110     C                   eval      o06err = *blanks
017300001027     C                   else
017400001027     C                   eval      o06err = 'E'
017500001024     C                   endif
017600001027
017700001027     C                   when      i06flm = 'W'
017800001027     C     korn          chain     fnorn01l
017900001027     C                   if        %found(fnorn01l)
018000001027     C                   eval      ORNpue = knmus
018100001110     C                   eval      ORNpon = vtcfgs
018200040818     c                   Z-add     *Date         OrnDtr
018300001027     C                   update    fnorn000
018400001027     C                   else
018500001024     C                   clear                   fnorn000
018600001027     C                   eval      ORNpoe = i06poe
018700001027     C                   eval      ORNnsr = i06nsr
018800001027     C                   eval      ORNnor = i06nor
018900001027     C                   eval      ORNnrv = i06nrv
019000001027     C                   eval      ORNdai = i06dai
019100001027     C                   eval      ORNori = i06ori
019200001027     C                   eval      ORNfar = i06far
019300001027     C                   eval      ORNprg = 1
019400001027     C                   eval      ORNpue = knmus
019500001110     C                   eval      ORNpon = vtcfgs
019600040818     c                   Z-add     *Date         OrnDtr
019700001024     C                   write     fnorn000
019800001024     C                   endif
019900001024     C
020000001027     C                   other
020100001024
020200001024      * Immissione / Manutenzione / Interrogazione
020300001024     C     i06flm        ifeq      'M'
020400001024     C                   movel     tit1          vtcmod
020500001024     C                   endif
020600001024
020700001024     C     i06flm        ifeq      'V'
020800040402     c     i06flm        oreq      'D'
020900080410     c     i06flm        oreq      'P'
021000001024     C                   movel     tit2          vtcmod
021100001024     C                   endif
021200001024
021300001024     C     i06flm        ifeq      'I'
021400001024     C                   movel     tit3          vtcmod
021500001024     C                   seton                                        08
021600001024     C                   endif
021700001027      * P.O. Gestione
021800001027     C                   movel     pfgs          vtcfgs
021900001027     C     vtcfgs        chain     azorg01L
022000001027     C                   if        %found(azorg01l)
022100001027     C                   movel     orgDES        vtdfgs
022200001027     C                   else
022300001027     C                   clear                   vtdfgs
022400001027     C                   endif
022500001024
022600001027     C                   eval      vtcpoe = i06poe
022700001027     C                   eval      vtnsr  = i06nsr
022800001027     C                   eval      vtnor  = i06nor
022900001027     C                   eval      vtnrv  = i06nrv
023000001027     C                   eval      vtfar  = i06far
023100001027      * Decodifica fase
023200001027     C                   clear                   TIBS02DS
023300001027     C                   movel     'C'           T02mod
023400001027     C                   movel     knsif         t02sif
023500001027     C                   movel     'FAR'         t02cod
023600001027     C                   movel(p)  vtfar         T02ke1
023700001027     C                   call      'TIBS02R'
023800001027     C                   parm                    KPJBA
023900001027     C                   parm                    TIBS02DS
024000001027     C                   movel     t02uni        vtdfar
024100080318
024200080318     c                   exsr      SR_DecTest
024300001024      * Pulizia campi di output
024400001024     C                   clear                   o06f03
024500001024     C                   clear                   o06f12
024600001024     C                   clear                   o06err
024700001024     C                   clear                   o06msg
024800010301     C                   clear                   o06dai
024900010301     C                   clear                   o06ori
025000010301     C                   clear                   o06prg
025100001024
025200971024     C                   Z-ADD     0             RIRI
025300971024     C                   Z-ADD     0             COCO
025400001024
025500001024     C     i06flm        comp      ' '                                0101
025600001024     C   01i06flm        comp      'V'                                0101
025700080410     C   01i06flm        comp      'P'                                0101
025800040402     C   01i06flm        comp      'D'                                0101
025900001024     C     i06flm        comp      'I'                                    02
026000001024
026100001024
026200001024     C                   if        i06flm = 'M'   and
026300001024     C                             wcont  = ' '
026400001024      * Riempimento subfile note
026500001024     C                   exsr      ries02
026600001024     C                   movel     '1'           wcont             1
026700001024     C                   move      i06key        keysav
026800001024     C                   z-add     1             wprimo            4 0
026900001024     C                   endif
027000001024
027100001024     C                   exsr      SUB_NOTE
027200001024
027300001027     C                   endsl
027400001024
027500001024     C     fine          tag
027600001024
027700001024      * Chiusura pgm
027800001024     C     i06tla        ifeq      ' '
027900001024     C                   return
028000001024     C                   else
028100001027     C                   clear                   tibs02ds
028200001024     C                   movel     'C'           t02tla
028300001024     C                   call      'TIBS02R'
028400001024     C                   parm                    kpjba
028500001027     C                   parm                    tibs02ds
028600001024     C                   eval      *inlr = *on
028700001024     C                   end
028800001024
028900001024      **********************************************************************
029000001024      * GESTIONE SUBILE NOTE
029100001024      **********************************************************************
029200980710     C     SUB_NOTE      BEGSR
029300001024
029400001024     C     i06tla        ifne      'C'
029500001024     C     i06flm        ifeq      *blanks
029600001024     C     i06flm        oreq      'V'
029700080410     C     i06flm        oreq      'P'
029800040402     C     i06flm        oreq      'D'
029900001024     C     i06flm        oreq      'M'
030000001024     C     i06flm        oreq      'I'
030100001024      * Caricamento
030200001024     C     i06flm        ifne      'M'
030300001024     C                   exsr      ries02
030400001024     C                   endif
030500001024      * Legge il subfile per proteggere eventualmente campi con key diversa
030600001024      * e per aggiornare la prima riga se key =
030700001024     C                   if        i06flm <> 'I' and *in60 = *on
030800001024     C                   exsr      aggs02
030900001024     C   06              seton                                        63
031000001024     C  n06              setoff                                       63
031100001024     C                   move      i06key        keysav
031200001024     C                   endif
031300040402
031400040402     c                   Eval      savpos = vccpos
031500001024      * Emissione
031600001024     C     emc02         tag
031700040402     C                   setoff                                       093010
031800001024     C                   z-add     vccpos        vccnrr
031900001024     C                   write     fior06t
032000001024
032100001024     C                   write     fior06z
032200001024     C  n60              write     fior06v
032300001024     C                   exfmt     fior06c
032400001219     C                   setoff                                       28
032500001024      * F12=Ritorno
032600001024     C     *inkl         ifeq      *on
032700001024     C                   movel     'S'           o06f12
032800001024     C                   goto      fine2
032900001024     C                   end
033000001024      * F03=Fine
033100001024     C     *inkc         ifeq      *on
033200001024     C                   movel     'S'           o06f03
033300001024     C                   goto      fine2
033400001024     C                   end
033500001024      * F14=Cancella riga
033600001024     C     *inkn         ifeq      *on
033700001024     C                   exsr      deler2
033800001024     C                   goto      emc02
033900001024     C                   end
034000001024      * Rollup
034100001024     C     *IN25         IFEQ      *ON
034200001024     C                   Z-ADD     PRIMA         vccpos
034300980714     C                   EXSR      ROLUP2
034400001024     C                   goto      emc02
034500001024     C                   END
034600001024     C     i06flm        ifeq      'M'
034700001024     C                   exsr      ctrrig2
034800001024     C                   end
034900040402
035000040402     C     i06flm        ifeq      'D'
035100040402     C                   z-add     savpos        wprimo
035200040402     C                   exsr      ctrrig2
035300040402     C                   end
035400040402      * Se ho errore ritorno
035500040402     C                   if        *in09 or *in10
035600040402     C                   goto      emc02
035700040402     C                   endif
035800001024
035900001024      * Se non è manutenzione/immissione e non è stato dato il F6 di conferma   OPPURE
036000001024      * siamo in interrogazione e non è stato dato F12 di ritorno
036100001024      *   RIMANE NELLA STESSA VIDEATA
036200001024     C                   if        (not *in01 and not *inkf) or
036300001024     C                             (*in02 and not *inkl)
036400121203      * controllo se vccpos = 0 in questo caso lo imposto a 1
036500121203      * per evitare errore di sessione/unità
036600121203     c                   IF        VCCpos = 0
036700121203     c                   eval      VCCpos = 1
036800121203     c                   ENDIF
036900001024     C                   goto      emc02
037000001024     C                   endif
037100001024     C                   endif
037200001024      *****************
037300001024      * CONFERMA DATI
037400001024      *****************
037500001024     C     i06flm        ifeq      'V'
037600080410     C     i06flm        oreq      'P'
037700001024     C                   z-add     1             wprimo
037800001024     C                   exsr      ctrrig2
037900001024     C                   end
038000001219
038100001219      * Se ho errore ritorno
038200040402     C                   if        *in09 = *on
038300001219     C                   goto      emc02
038400001219     C                   endif
038500001024
038600001024     C     i06flm        ifeq      *blanks
038700001024     C     i06flm        oreq      'C'
038800001024     C     i06flm        oreq      'V'
038900080410     C     i06flm        oreq      'P'
039000040402     C     i06flm        oreq      'D'
039100001024     C                   exsr      confe2
039200001024     C                   endif
039300001024
039400001024     C     fine2         tag
039500001024
039600001024     C                   endif
039700001024     C                   endsr
039800001024
039900001024      **********************************************************************
040000001024      * RIEMPIMENTO SUBFILE NOTE
040100001024      **********************************************************************
040200980714     C     RIES02        BEGSR
040300001024
040400001024      * Pulizia
040500981216     C                   SETOFF                                       616306
040600001024     C                   WRITE     fior06c
040700981216     C                   SETON                                        6061
040800001024     C                   MOVE      *ZEROS        NRR               5 0
040900001024      * Se passato il numero ORM carica dati da file descrizioni
041000001024     C     i06nor        ifne      *zeros
041100001027     C                   eval      vshpoe = i06poe
041200001027     C                   eval      vshnsr = i06nsr
041300001027     C                   eval      vshnor = i06nor
041400001027     C                   eval      vshnrv = i06nrv
041500001027     C                   eval      vshdai = i06dai
041600001027     C                   eval      vshori = i06ori
041700001027     C                   eval      vshfar = i06far
041800001024     C                   clear                   wwrt              1
041900001024     C     korn1         setll     fnorn01l
042000001027     C                   do        *hival
042100001027     C     korn1         reade     fnorn01l
042200001027     C                   if        %eof(fnorn01l)
042300001027     C                   leave
042400001027     C                   endif
042500001024     C                   if        ORNatb = *blanks
042600001024      * Scrive un record del subfile
042700001024     C                   movel(P)  ORNnot        vsnot
042800001129     C                   movel(P)  ORNsns        vssns
042900001024      * Data
043000001024     C                   Z-ADD     ORNdai        G02inv
043100980714     C                   movel     '3'           G02err
043200980714     C                   CALL      'XSRDA8'
043300980714     C                   PARM                    WLBDAT
043400001026     C                   z-add     g02dat        vsdai
043500001024      * Ora
043600001024     C                   Z-ADD     ORNori        vsori
043700001024      * Fase
043800001024     C                   z-add     ORNfar        vsfar
043900001024      * Utente che ha scritto la nota
044000001024     C                   movel     ORNpue        vspue
044100001024      * Filiale che ha scritto la nota
044200001024     C                   movel     ORNpon        vspon
044300980714      * PROTEZIONE NOTE
044400001114     C     i06poe        ifne      vshpoe
044500001024     C     i06nsr        orne      vshnsr
044600001024     C     i06nor        orne      vshnor
044700001024     C     i06nrv        orne      vshnrv
044800001024     C     i06flm        oreq      'M'
044900001024     C                   seton                                        08
045000001024     C                   movel     '1'           vshin8
045100001024     C                   else
045200001024     C                   setoff                                       08
045300001024     C                   movel     ' '           vshin8
045400001024     C                   endif
045500001024      * 02 on protegge il campo note / descrizione
045600001024     C                   if        *in02 = *on     or
045700001024      * Fase differente
045800001024     C                             ORNfar <> i06far
045900061006      * blocco nota
046000061006     c                             or ornftr = 'S'
046100001024     C                   seton                                        08
046200001024     C                   move      '1'           vshin8
046300001024     C                   endif
046400001109
046500001024     C                   add       1             nrr
046600001024     C                   write     fior06s
046700001024     C                   clear                   wwrt
046800001024     C                   clear                   vshin8
046900001109     C                   setoff                                       08
047000001024     C                   update    fnorn000
047100001024     C                   endif
047200001024     C                   enddo
047300080415     c                   eval      $nonotapda = *off
047400080507     c                   clear                   nrnotapda
047500080410      * se richiamato con 'P' devo impostare la nota inserita sul PDA
047600080410      * per facilitare la quadratura distinte
047700080415      * se ho trovato almeno una nota
047800080415     c                   if        i06flm = 'P' and nrr > 0
047900080410     c                   exsr      sr_notapda
048000080410     c                   endif
048100080507      * se richiamato con 'P' e non ho caricato nessuna nota da PDA
048200080507      * imposto il flag per caricare bene il subfile
048300080507     c                   if        i06flm = 'P' and nrnotapda = 0
048400080507     c                   eval      $nonotapda = *on
048500080507     c                   endif
048600080415      * se richiamato con 'P' e nessuna nota caricata imposto flag per
048700080415      * caricare bene il subfile vuoto
048800080415     c                   if        i06flm = 'P' and nrr = 0
048900080415     c                   eval      $nonotapda = *on
049000080415     c                   endif
049100080410
049200001024     C                   end
049300001024     C                   z-add     1             vccpos
049400001024      * Carica righe vuote
049500001024     C     i06flm        ifne      'I'
049600001024     C                   z-add     nrr           max2              5 0
049700001024     C                   exsr      rolup2
049800001024     C                   else
049900001024     C                   seton                                        0663
050000001024     C                   end
050100001024     C     nrr           ifeq      *zeros
050200001024     C                   setoff                                       60
050300001024     C                   end
050400001024
050500001024     C                   endsr
050600001024      **********************************************************************
050700001024      * ROLLUP NOTE
050800001024      **********************************************************************
050900980714     C     ROLUP2        BEGSR
051000001024
051100001024     C                   Z-ADD     MAX2          NRR
051200080318     C     MAX2          DIV       13            WDIV              5 0
051300980714     C                   MVR                     WRES              3 0
051400980714     C                   ADD       1             WRES
051500001024     C                   CLEAR                   vsnot
051600001129     C                   clear                   vssns
051700001024     C                   CLEAR                   vsdai
051800001024     C                   CLEAR                   vsori
051900001024     C                   CLEAR                   vsfar
052000001024     C                   CLEAR                   vspue
052100001024     C                   CLEAR                   vspon
052200080318     C     WRES          DO        13
052300001024     C                   ADD       1             NRR
052400001024      * Se modalità=solo immissione memorizza nel sfl anche la key parziale
052500001024      * ricevuta
052600001024     C                   CLEAR                   vsnot
052700001027     C                   eval      vshpoe = i06poe
052800001027     C                   eval      vshnsr = i06nsr
052900001027     C                   eval      vshnor = i06nor
053000001027     C                   eval      vshnrv = i06nrv
053100001027     C                   eval      vshdai = i06dai
053200001027     C                   eval      vshori = i06ori
053300001027     C                   eval      vshfar = i06far
053400001024
053500001024     C                   WRITE     fior06s
053600001026     C                   ENDdo
053700001024
053800001024     C     NRR           SUB       16            vccpos
053900001024     C                   Z-ADD     NRR           MAX2
054000001024
054100980714     C                   ENDSR
054200001024      **********************************************************************
054300001024      * ANNULLA UNA RIGA NEL SUBFILE
054400001024      **********************************************************************
054500980715     C     delEr2        BEGSR
054600001024
054700980714     C                   EXSR      RIGA
054800001024      * Solo se non e' campo protetto
054900001024     C     $rig          CHAIN     fior06s                            30
055000001024     C                   if        vshin8 = ' ' and *in30 = *off
055100980714     C     $RIG          IFNE      0
055200980714     C                   CLEAR                   s2ds
055300001027     C                   eval      vshpoe = i06poe
055400001027     C                   eval      vshnsr = i06nsr
055500001027     C                   eval      vshnor = i06nor
055600001027     C                   eval      vshnrv = i06nrv
055700001027     C                   eval      vshdai = i06dai
055800001027     C                   eval      vshori = i06ori
055900001027     C                   eval      vshfar = i06far
056000980720     C                   MOVE      s2ds          S2SAV2
056100001024     C     MAX2          CHAIN     fior06s                            30
056200980720     C                   MOVE      s2ds          S2SAV
056300981216     C                   Z-SUB     MAX2          $MAX2             5 0
056400980714     C                   Z-SUB     $RIG          $RIG              5 0
056500980714     C                   ADD       1             $RIG
056600981216     C                   ADD       1             $MAX2
056700981216     C     $MAX2         DO        $RIG          Y                 5 0
056800980714     C                   Z-SUB     Y             I                 5 0
056900980715     C                   MOVE      S2SAV2        s2ds
057000001024     C     s2sav2        ifne      *blanks
057100001114     C     i06poe        ifne      vshpoe
057200001024     C     i06nsr        orne      vshnsr
057300001024     C     i06nor        orne      vshnor
057400001024     C     i06nrv        orne      vshnrv
057500001027     C     i06dai        orne      vshdai
057600001027     C     i06ori        orne      vshori
057700001024     C     i06far        orne      vshfar
057800001024     C                   seton                                        08
057900001024     C                   movel     '1'           vshin8
058000001024     C                   else
058100001024     C                   setoff                                       08
058200001024     C                   movel     ' '           vshin8
058300001024     C                   end
058400001024
058500001024     C  N30              UPDATE    fior06s
058600001024     C                   endif
058700001024     C                   setoff                                       08
058800001024     C                   movel     ' '           vshin8
058900001024     C     I             CHAIN     fior06s                            30
059000980720     C                   MOVE      s2ds          S2SAV3
059100980715     C                   MOVE      S2SAV         S2SAV2
059200980715     C                   MOVE      S2SAV3        S2SAV
059300980714     C                   END
059400980714     C                   END
059500001024     C                   end
059600001024     C                   setoff                                       08
059700001024
059800980714     C                   ENDSR
059900001024      **********************************************************************
060000001024      * DETERMINA NRR DEL SUBFILE
060100001024      **********************************************************************
060200971024     C     RIGA          BEGSR
060300001024
060400971024     C                   Z-ADD     PRIMA         $RIG              5 0
060500971024     C                   MOVE      NRG           R$$
060600971024     C                   ADD       RIRI          $RIG
060700971024     C                   SUB       6             $RIG
060800971024     C     $RIG          IFLT      0
060900971024     C     RIRI          ORLT      6
061000971024     C                   Z-ADD     0             $RIG
061100971024     C                   END
061200001024
061300971024     C                   ENDSR
061400001024      **********************************************************************
061500001024      * AGGIORNAMENTO SFL NOTE IN MODALITA' SOLO IMMISSIONE
061600001024      **********************************************************************
061700001024     C     aggs02        begsr
061800001024
061900001024     C                   setoff                                       06
062000001024     C                   z-add     1             nrr
062100001024     C                   move      *zeros        vccpos
062200001024     C     nrr           chain     fior06s                            30
062300001024     C     *in30         doweq     *off
062400001024      * Se riga non protetta e' la prima che deve essere utlizzata
062500080411     C**!!!VsHIN8        IFEQ      ' '
062600080411     C**!!!vsnot         andeq     *blanks
062700080411     c                   if        vshin8 = *blanks and ((vsnot = *blanks and
062800080411     c                             i06flm <> 'P') or (vsnot <> *blanks and
062900080415     c                             i06flm = 'P') or $nonotapda)
063000001024     C     vccpos        ifeq      *zeros
063100001024     C                   z-add     nrr           vccpos
063200001024     C                   z-add     nrr           wprimo            4 0
063300001024      * Se non valorizzata la chiave ma è nella gestione note
063400001024      * valorizza i campi della data ora e fase nella prima riga
063500001024      * con data e ora del momento
063600001024     C                   if        vsnot = *blanks
063700001024     C                   Z-ADD     wdtgio        vsdai
063800001024      * Ora
063900010301     C                   z-add     i06ori        vsori
064000010301     C************       Z-ADD     wora          vsori
064100001024      * Fase
064200001024     C                   movel     i06far        vsfar
064300001024      * Profilo utente
064400001024     C                   movel     knmus         vspue
064500001024      * Filiale
064600001110     C                   movel     vtcfgs        vspon
064700001129      * Flag note interne/stampa
064800001129     C                   movel     'I'           vssns
064900001024     C                   endif
065000001024      * Se ci sono già records nel sfl con quella chiave disabilita rollup
065100001024     C                   seton                                        06
065200001024     C                   end
065300001024     C                   end
065400001024     C     VsHIN8        IFne      ' '
065500001024     C                   seton                                        08
065600001024     C                   endif
065700001024     C                   update    fior06s
065800001024     C                   setoff                                       08
065900001024     C                   movel     ' '           vshin8
066000001024     C                   add       1             nrr
066100001024     C     nrr           chain     fior06s                            30
066200001024     C                   enddo
066300001024     C                   setoff                                       08
066400001024     C     vccpos        ifeq      *zeros
066500001024     C     max2          add       1             wprimo
066600980714     C                   EXSR      ROLUP2
066700001024     C                   end
066800001024
066900980714     C                   ENDSR
067000001024      **********************************************************************
067100001024      * CONFERMA DATI SU DATA BASE
067200001024      **********************************************************************
067300001024     C     confe2        begsr
067400001024
067500001024      * Se esistono dei records ancora da vedere con la chiave passata li cancella
067600001024      * (se modalità "C" non possono esserci records da cancellare perchè
067700001024      * in questo caso è in inserimento di nuovi records)
067800001024     C     i06flm        ifne      'C'
067900001024     C     korn1         setll     fnorn01l
068000001024     C     korn1         reade     fnorn01l                               30
068100001024     C     *in30         doweq     *off
068200001024      * Se la nota e' manutenzionabile la cancella
068300061006     C                   If        ORNfar = i06far and ornftr <> 'S'
068400001024     C                   delete    fnorn000
068500001024     C                   endif
068600001024     C     korn1         reade     fnorn01l                               30
068700001024     C                   enddo
068800001024     C                   endif
068900001024      * Scrive records a video
069000001024      * Cerce l'ultimo record valido.
069100981216     C     MAX2          DOWGT     *ZERO
069200001024     C     MAX2          CHAIN     fior06s                            30
069300001024     C     vsnot         COMP      *BLANK                             3030
069400981216     C  N30              SUB       1             MAX2
069500980714     C  N30              END
069600001024
069700001024     C     max2          ifgt      *zeros
069800001024
069900001024     C                   clear                   fnorn000
070000001027     C                   eval      ORNpoe = i06poe
070100001027     C                   eval      ORNnsr = i06nsr
070200001027     C                   eval      ORNnor = i06nor
070300001027     C                   eval      ORNnrv = i06nrv
070400001027     C                   eval      ORNdai = i06dai
070500001027     C                   eval      ORNori = i06ori
070600001027     C                   eval      ORNfar = i06far
070700001024
070800001024     C                   z-add     0             ORNprg
070900001024     C
071000001024     C                   DO        MAX2          NRR
071100001024      * Per ogni record di subfile scrive due records di fnorn00f
071200001024     C     NRR           CHAIN     fior06s                            30
071300001024      * Se record non protetto elabora
071400001024     C                   if        vshin8 = ' '
071500010301      * Se modalità "C", e cambia chiave azzera il progressivo
071600001024     C     i06flm        ifeq      'C'
071700001024     C     vshkey        andne     vshsav
071800001024     C                   move      *zeros        ORNprg
071900001024     C                   move      vshkey        vshsav
072000001024     C                   end
072100001024      * Se record vuoto lo salta
072200001024     C     vsnot         ifne      *blanks
072300001024     C                   add       1             ORNprg
072400001027     C                   eval      ORNnot = vsnot
072500001129     C                   eval      ORNsns = vssns
072600010220      * Se modalità "C" il P.O. emissione e la fase sono
072700001024      * memorizzati nel subfile
072800001024     C     i06flm        ifeq      'C'
072900001027     C                   eval      ORNpoe = vshpoe
073000001027     C                   eval      ORNfar = vshfar
073100010220     C                   if        vshfar = *zeros
073200010220     C                   eval      ORNfar = i06far
073300010220     C                   endif
073400001024     C                   end
073500001024      * Valorizza il campo data con la data passata in chiave
073600980714      * oppure con la data e l'ora del momnento oppure se record gia' esistente
073700980714      * con quella del subfile
073800001024     C                   if        vsdai   <> *zeros
073900001024     C                   Z-ADD     vsdai         G02dat
074000980714     C                   movel     ' '           G02err
074100980714     C                   CALL      'XSRDA8'
074200980714     C                   PARM                    WLBDAT
074300001024     C                   z-add     g02inv        ORNdai
074400001024     C                   z-add     vsori         ORNori
074500001024     C                   movel     vsfar         ORNfar
074600001024     C                   move      vspue         ORNpue
074700001024     C                   movel     vspon         ORNpon
074800010220     C                   if        vsfar = *zeros
074900010220     C                   eval      ORNfar = i06far
075000010220     C                   endif
075100001024     C                   else
075200001024     C                   if        i06dai  <> *zeros
075300001024     C                   z-add     i06dai        ORNdai
075400001024     C                   z-add     i06ori        ORNori
075500001024     C                   else
075600001024     C                   z-add     wora          ORNori
075700001024     C                   z-add     dateu         ORNdai
075800001024     C                   endif
075900001024      * Valorizza profilo + P.O.
076000001024     C                   move      knmus         ORNpue
076100001110     C                   z-add     vtcfgs        ORNpon
076200001024     C                   endif
076300040818     c                   Z-add     *Date         OrnDtr
076400001024     C                   write     fnorn000
076500010301      *---
076600010301     C                   eval      o06dai = orndai
076700010301     C                   eval      o06ori = ornori
076800010301     C                   eval      o06prg = ornprg
076900010301      *---
077000001024     C                   endif
077100001024     C                   endif
077200001024     C                   enddo
077300001024
077400001024     C                   end
077500001024
077600001024     C                   endsr
077700001024      **********************************************************************
077800001024      * CONTROLLI IN MODALITA' DI SOLA IMMISSIONE O VARIAZIONE
077900001024      **********************************************************************
078000001024     C     ctrrig2       begsr
078100001024
078200001024     C     wprimo        chain     fior06s                            30
078300001024     C     vsnot         ifeq      *blanks
078400001024     C     wprimo        add       1             wnrr
078500001024     C     wnrr          chain     fior06s                            30
078600001024     C     *in30         doweq     *off
078700001024     C     i06flm        ifeq      'M'
078800001024     C     vshpoe        andeq     i06poe
078900001024     C     vshnsr        andeq     i06nsr
079000001024     C     vshnor        andeq     i06nor
079100001024     C     vshnrv        andeq     i06nrv
079200001024     C     i06flm        oreq      'V'
079300040402     C     i06flm        oreq      'D'
079400001024     C     vsnot         ifne      *blanks
079500001024     C                   seton                                        30
079600001024     C                   movel     vsnot         w035a
079700001129     C                   movel     vssns         wflag
079800001024     C                   clear                   vsnot
079900001129     C                   clear                   vssns
080000001024     C                   update    fior06s
080100001024     C     wprimo        chain     fior06s                            31
080200001024     C     w035a         ifne      *blanks
080300001024     C                   movel     w035a         vsnot
080400001129     C                   movel     wflag         vssns
080500001024     C                   endif
080600001024     C                   update    fior06s
080700001024     C                   endif
080800001024
080900001024     C                   add       1             wnrr
081000001024     C  n30wnrr          chain     fior06s                            30
081100001024     C                   else
081200001024     C                   seton                                        30
081300001024     C                   end
081400001024     C                   enddo
081500001024
081600001219     C                   else
081700001219
081800001219     C                   z-add     wprimo        wnrr
081900001219     C     *in30         doweq     *off
082000001219     C     vshpoe        ifeq      i06poe
082100001219     C     vshnsr        andeq     i06nsr
082200001219     C     vshnor        andeq     i06nor
082300001219     C     vshnrv        andeq     i06nrv
082400040402      * tipo richiamo = 'D'
082500040402      * immettere almeno 10 caratteri
082600040402     c                   If        I06Flm = 'D' and vsnot <> *Blanks
082700040402     c                             and wnrr = savpos
082800040402     c                   If        %subst(vsnot:11:24) = *Blanks
082900040402     c                   Eval      *In10 = *On
083000040402     c                   Eval      *In28 = *On
083100040402     c                   Eval      *In30 = *On
083200040402     c                   Eval      VccMsg = 'immettere una nota valida'
083300040402     c                   Update    Fior06s
083400040402     c                   EndIf
083500040402     c                   EndIf
083600001219      * Controllo se ok il flag tipo nota (interna o stampa)
083700001219     C                   if        vssns = *blanks  and vsnot <> *blanks
083800040402     c                             and not *in28
083900001219     C                   seton                                        280930
084000001219     C                   eval      vccmsg = 'Flag stp.nota non valido'
084100001219     C                   update    fior06s
084200001219     C                   endif
084300001219     C                   endif
084400001219     C                   add       1             wnrr
084500001219     C  n30wnrr          chain     fior06s                            30
084600001219     C                   enddo
084700001219
084800001219     C                   endif
084900001024
085000040402     C     endctrrig     endsr
085100080318
085200080318      **********************************************************************
085300080318      * ROUTINE di decodifica dati della testata
085400080318      **********************************************************************
085500080318     C     SR_DecTest    BEGSR
085600080319      *
085700080319      * se valorizzato indicatore 20 valorizzo i campi del video
085800080319     c                   IF        *in20
085900080319      * verifico se ho ricevuto solo la causale e basta
086000080319     c                   if        v06car <> *blank
086100080319     c                   movel(p)  v06car        vtccar
086200080319     c                   movel(p)  v06dcar       vtdcar
086300080319      * verifico se campo valorizzato
086400080319     c                   If        v06car <> *blanks
086500080319     c                   eval      *in14 = *on
086600080319     c                   else
086700080319     c                   eval      *in14 = *off
086800080319     c                   endif
086900080319      * verifico se si sono  altri dati valorizzati
087000080319     c                   eval      *in20 = (v06rsr <> *blanks)
087100080319     c                   endif
087200080319
087300080319     c                   endif
087400080319      * se recupero da DS
087500080319     c                   If        *in20
087600080319     c                   eval      *in13 = *on
087700080318     c                   movel(p)  v06rsr        vtcrsr
087800080318     c                   movel(p)  v06rso        vtcrso
087900080318
088000080318     c                   movel(p)  v06ndc        vtcndc
088100080318      * Inversione data distinta  ORM
088200080318     c                   clear                   wlbdat
088300080318     c                   z-add     v06ddc        G02inv
088400080318     c                   movel     '3'           G02err
088500080318     c                   call      'XSRDA8'
088600080318     c                   parm                    wlbdat
088700080318     c                   z-add     G02dat        vtcddc
088800080318      * verifico se campo valorizzato
088900080318     c                   If        vtcndc <> *zeros
089000080318     c                   eval      *in15 = *on
089100080318     c                   else
089200080318     c                   eval      *in15 = *off
089300080318     c                   endif
089400080318
089500080318
089600080318     c                   movel(p)  v06pdr        vtcpdr
089700080318     c                   movel(p)  v06dpdr       vtdpdr
089800080318
089900080318     c                   else
090000080318
090100080319      * Se i dati non sono stati passati dalla DS e quindi il 20 spento recupero dai file
090200080318
090300080318      * aggancio FNORM
090400080318     c     korn1         chain     fnorm01l
090500080318     c                   if        %found(fnorm01l)
090600080318     c                   movel     ormrsr        vtcrsr
090700080318     c                   movel     ormrso        vtcrso
090800080318     c                   eval      *in13 = *on
090900080318      * verifico la distinta
091000080318     c                   eval      *in15 = *off
091100080318     c                   if        ormndc > 0
091200080318      * aggancio la distinta
091300080319     c     kdst          chain     fidst09l
091400080318      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
091500080319     c                   if        not %found(fidst09l)
091600080318     c                   clear                   fnlv55ds
091700080318     c                   eval      d55tpt = '6'
091800080318     c                   move      ormpor        d55lin
091900080318     c                   eval      d55drf = ormddc
092000080318     c                   call      'FNLV55R'
092100080318     c                   parm                    fnlv55ds
092200080318     c                   if        d55err = *blanks and d55tfa <> d55lin
092300080318     c                   move      d55tfa        kfgs
092400080319     c     kfidst        chain     fidst09l
092500080318     c                   endif
092600080318     c                   endif
092700080319     c                   If        %found(Fidst09l)
092800080318     c                   eval      *in15 = *on
092900080318     c                   z-add     ormndc        vtcndc
093000080318      * Inversione data distinta  ORM
093100080318     c                   clear                   wlbdat
093200080318     c                   z-add     ormddc        G02inv
093300080318     c                   movel     '3'           G02err
093400080318     c                   call      'XSRDA8'
093500080318     c                   parm                    wlbdat
093600080318     c                   z-add     G02dat        vtcddc
093700080318      * recupero il padroncino
093800080318     c                   movel     dstpdr        vtcpdr
093900080318     c     kfapd         chain     fiapd01L
094000080318     c                   if        %found(fiapd01l)
094100080318     c                   movel     APDrsc        vtdpdr
094200080318     C                   endif
094300080318
094400080318     C                   endif
094500080318
094600080318     C                   endif
094700080318
094800080318     C                   endif
094900080318
095000080319      * verifico la causale mancato ritiro solo se non è stata valorizzata
095100080320     c                   if        %parms <= 2
095200080318     c                   eval      *in14 = *off
095300080318     c                   exsr      sr_carcau
095400080319     c                   endif
095500080318
095600080318     c                   endif
095700080318
095800080318     c                   Endsr
095900080318
096000080318      **********************************************************************
096100080318      * Cerca causale chiusura e decodifica
096200080318      **********************************************************************
096300080318     C     Sr_Carcau     begsr
096400080318
096500080318     C     kfnorf        chain     fnorf01L
096600080318     C                   if        %found(fnorf01l)
096700080318     C                   eval      vtccar= orfcar
096800080318     C                   endif
096900080318
097000080318     C                   clear                   dcmr
097100080318     C                   clear                   TIBS02DS
097200080318     C                   movel     'C'           T02mod
097300080318     C                   movel     knsif         t02sif
097400080318     C                   movel     'CMR'         t02cod
097500080318     C                   movel(p)  vtccar        T02ke1
097600080318     C                   call      'TIBS02R'
097700080318     C                   parm                    KPJBA
097800080318     C                   parm                    TIBS02DS
097900080318     C                   movel     t02uni        dcmr
098000080318     C                   movel     d§cmrdes      vtdcar
098100080318
098200080318     C                   eval      *in14 = (vtccar <> *blanks)
098300080318
098400080318     C                   endsr
098500080410
098600080410      *------------------------------------------------------------------------*
098700080410      *  Recupero l'ultima nota inserita da PDA
098800080410      *------------------------------------------------------------------------*
098900080410     c     sr_notapda    begsr
099000080410
099100080411      * cerco la fase precedente dell'orm
099200080411     c     kfnorf        setll     fnorf01l
099300080411     c     kfnorm        readpe    fnorf01l
099400080411     c                   if        %eof(fnorf01l)
099500080411     c                   leavesr
099600080411     c                   endif
099700080411      * imposto la chiave per leggere tutte le fasi arrivate da PDA
099800080411     c                   eval      kdae = orfdae
099900080411     c                   eval      kore = orfore
100000080411     c                   eval      kfar = orffar
100100080410      * cerco l'ultima fase dell'orm
100200080410     c                   clear                   dorf01
100300080410     c                   clear                   savorfesito
100400080411     c     kfnorf1       setll     fnorf01l
100500080410     c                   do        *hival
100600080411     c     kfnorf1       reade     fnorf01l
100700080410     c                   if        %eof(fnorf01l)
100800080410     c                   leave
100900080410     c                   endif
101000080410     c                   eval      dorf01 = orfflo
101100080410      * prendo l'ultima fase a parità di data e ora
101200080410      * lo faccio controllando l'esito del PDA memorizzato in orfflo
101300080410     c                   if        §orfesito <> *blanks
101400080410     c                   if        §orfesito < savorfesito and
101500080410     c                             savorfesito > *zeros
101600080410     c                   iter
101700080410     c                   endif
101800080410     c                   eval      savorfesito = §orfesito
101900080410     c                   endif
102000080410     c                   enddo
102100080410      * imposto i dati per agganciare la nota
102200080410     c                   if        savorfesito = *blanks
102300080410     c                   leavesr
102400080410     c                   endif
102500080410     c                   eval      wdata = %dec(%subst(§orfesito:1:8):8:0)
102600080410     c                   eval      wora  = %dec(%subst(§orfesito:9:6):6:0)
102700080410      * aggancio la nota
102800080410     c     korn2         chain     fnorn01l
102900080410     c                   if        not %found(fnorn01l)
103000080410     c                   leavesr
103100080410     c                   endif
103200080410      * scrivo un rcd nel subfile con la nota inserita da PDA
103300080410     c                   eval      vshpoe = ornpoe
103400080410     c                   eval      vshnsr = ornnsr
103500080410     c                   eval      vshnor = ornnor
103600080410     c                   eval      vshnrv = ornnrv
103700080410     c                   eval      vshdai = i06dai
103800080410     c                   eval      vshori = i06ori
103900080410     c                   eval      vshfar = i06far
104000080410      * nota scritta da PDA
104100080410     c                   eval      vsnot = ornnot
104200080410      * nota interna
104300080410     c                   eval      vssns = 'I'
104400080410      * data e ora
104500080410     c                   z-add     i06dai        g02inv
104600080410     c                   movel     '3'           g02err
104700080410     c                   call      'XSRDA8'
104800080410     c                   parm                    wlbdat
104900080410     c                   z-add     g02dat        vsdai
105000080410     c                   z-add     i06ori        vsori
105100080410     c                   z-add     i06far        vsfar
105200080410      * utente
105300080410     c                   eval      vspue = knmus
105400080410      * filiale
105500080410     c                   eval      vspon = vtcfgs
105600080410     c                   add       1             nrr
105700080507     c                   add       1             nrnotapda
105800080410     c                   clear                   vshin8
105900080410     c                   setoff                                       08
106000080410     c                   write     fior06s
106100080410
106200080410     c                   endsr
106300080410
106400001024      **********************************************************************
106500001024      * ROUTINE INIZIALE
106600001024      **********************************************************************
106700951009     C     *INZSR        BEGSR
106800001024
106900971016     C                   TIME                    W0140            14 0
107000001024      * UDATE IN GGMMAAAA
107100971016     C                   MOVE      W0140         WDTGIO            8 0
107200001026     C                   MOVEL     W0140         WORA              6 0
107300001024
107400001024      * UDATE IN AAAAMMGG
107500980619     C                   Z-ADD     WDTGIO        G02DAT
107600980619     C                   MOVEL     *BLANK        G02ERR
107700980619     C                   CALL      'XSRDA8'
107800980619     C                   PARM                    WLBDAT
107900980619     C                   MOVEL     G02INV        DATEU             8 0
108000001024
108100971024     C                   Z-ADD     1             CODUT
108200971024     C                   CALL      'X§PARUT'
108300001027     C                   PARM                    UT§DSE0F
108400971024     C                   MOVEL     RAGUT         RSUT
108500980619     C                   MOVEL     Rec80         cncr80
108600001024     C
108700010723      * Da sede non imposto il p.o. di gestione
108800010723     C                   if        simfel = 0
108900010723     C                   eval      *in12 = *on
109000010723     C                   else
109100010723     C                   eval      *in12 = *off
109200010723     C                   endif
109300080320      * valorizzo la libreria file fidst
109400080320
109500080320     c                   If        %subst(knsif:7:1) = 'P'
109600080320     c                   Eval      openthis = 'FILTRAPRD/FIDST09L'
109700080320     c                   Else
109800080320     c                   Eval      openthis = 'FILTRA201/FIDST09L'
109900080320     c                   EndIf
110000080320     c                   open      fidst09l
110100010723
110200001024     C     *like         define    i06key        keysav
110300001026     C     *like         define    vshkey        vshsav
110400001024     C     *like         define    nrr           wnrr
110500001024
110600001024     C     korn          klist
110700001024     C                   kfld                    i06poe
110800001024     C                   kfld                    i06nsr
110900001024     C                   kfld                    i06nor
111000001024     C                   kfld                    i06nrv
111100001026     C                   kfld                    i06dai
111200001026     C                   kfld                    i06ori
111300001024     C                   kfld                    i06far
111400001024     C                   kfld                    prog
111500001024     C                   z-add     1             prog
111600001024     C
111700001024
111800001024     C     korn1         klist
111900001024     C                   kfld                    i06poe
112000001024     C                   kfld                    i06nsr
112100001024     C                   kfld                    i06nor
112200001024     C                   kfld                    i06nrv
112300080410
112400080410     c     korn2         klist
112500080410     c                   kfld                    orfpoe
112600080410     c                   kfld                    orfnsr
112700080410     c                   kfld                    orfnor
112800080410     c                   kfld                    orfnrv
112900080410     c                   kfld                    wdata
113000080410     c                   kfld                    wora
113100080410     c                   kfld                    orffar
113200001027
113300001027     C     Ktbe          klist
113400001027     C                   kfld                    ktbcod
113500001027     C                   kfld                    ktbke1
113600001027     C                   kfld                    ktbke2
113700080318
113800080318     C     Kdst          klist
113900080318     C                   kfld                    ormpor
114000080318     C                   kfld                    catk
114100080318     C                   kfld                    ormndc
114200080318
114300080318     C     Kfidst        klist
114400080318     C                   kfld                    Kfgs
114500080318     C                   kfld                    catk
114600080318     C                   kfld                    ormndc
114700080318
114800080318     C     Kfapd         klist
114900080318     C                   kfld                    kpdtip
115000080318     C                   kfld                    dstpdr
115100001024
115200080318     C     Kfnorf        klist
115300080318     C                   kfld                    ormpoe
115400080318     C                   kfld                    ormnsr
115500080318     C                   kfld                    ormnor
115600080318     C                   kfld                    ormnrv
115700080318     C                   kfld                    ormdfo
115800080318     C                   kfld                    ormofo
115900080318     C                   kfld                    ormfao
116000080411
116100080411     c     kfnorf1       klist
116200080411     c                   kfld                    ormpoe
116300080411     c                   kfld                    ormnsr
116400080411     c                   kfld                    ormnor
116500080411     c                   kfld                    ormnrv
116600080411     c                   kfld                    kdae
116700080411     c                   kfld                    kore
116800080411     c                   kfld                    kfar
116900080411
117000080411     c     kfnorm        klist
117100080411     c                   kfld                    ormpoe
117200080411     c                   kfld                    ormnsr
117300080411     c                   kfld                    ormnor
117400080411     c                   kfld                    ormnrv
117500080318
117600951009     C                   ENDSR
