000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500001006      *       GESTIONE ORDINI RITIRO FISSI                            *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001018
000900001018      * ATTENZIONE
001000010405      * CARICO SUBFILE SOLO FINO A 9999
001100001018
001200980521      ****************************************************************
001300980521      *  RIEPILOGO INDICATORI
001400981008      ****************************************************************
001500060104      * 01 - richiamato da anagrafica ritiri
001600060105      * 02 - richiamato da anagrafica ritiri x interrogazione
001700980521      * 05 - SPROTEGGO P.O. IN GESTIONE
001800980521      * 06 - NON ABILITO TASTO FUNZIONALE F18
001900010405      * 11 - LEGGO PER N.ritiro fisso
002000010405      * 12 - SONO IN SEDE
002100010405      * 13 - LETTURA PER P.O. se sono in sede
002200990312      * 20 - GESTIONE SUBFILE
002300990312      * 21 - GESTIONE SUBFILE
002400990312      * 22 - GESTIONE SUBFILE
002500990312      * 23 - GESTIONE SUBFILE - ROLLUP
002600990315      * 24 - LETTURA FILE PER CARICAMENTO SUBFILE
002700990323      * 25 - LETTURA FILE PER CARICAMENTO SUBFILE
002800990505      * 26 - DISABILITO - ROLLUP
002900980521      * 28 - ERRORE GENERICO DSPF
003000001009      * 30 - Comodo
003100010316      * 31 - Comodo
003200010316      * 32 - Comodo
003300980521      * 40 - ERRORE p.o. in gestione
003400010201      * 41 - ERRORE Numero ritiro fisso
003500001009      * 42 - ERRORE Codice ritiro
003600001009      * 43 - ERRORE Codice ordinante
003700130411      * 44 - ERRORE Giro ritiro o pos.cursore su Giro ritiro
003800001009      * 45 - ERRORE Nessuna selezione
003900001009      * 46 - ERRORE Ritiro M/P
004000001012      * 47 - ERRORE Ricerca ritiro
004100001012      * 48 - ERRORE Ricerca ordinante
004200010405      * 49 - ERRORE scelta
004300010405      * 50 - ERRORE p.o. emissione se sono in sede
004400010207      * 52 - ERRORE Procedura ORM non attiva
004500980624      * 90 - riemissione videata
004600001011      * 91 - riemissione videata
004700990317      ****************************************************************
004800980623
004900001009     FAZORG01L  IF   E           K DISK
005000001006     FFNORS01L  IF   E           K DISK
005100070905     ffnors11l  if   e           k disk
005200001006     FFNACR01L  IF   E           K DISK
005300001009     FFIOR02D   CF   E             WORKSTN  sfile(fiOR02S:nrr1)
005400980623
005500980521      *------------------------------------------------------------------------*
005600001006     D kcfgs           S                   LIKE(V1Cfgs)
005700001006     D knor            S                   LIKE(V1nor)
005800001009
005900001009     D knacr           S                   LIKE(ACRcro)
006000001020
006100001020     D §ccra           S             10  0
006200001020     D §ccor           S             10  0
006300001009
006400070905     d wccgi           s                   like(v1ccgi)
006500070905     d wdcgi           s                   like(v1dcgi)
006600001006     D W0140           S             14  0
006700001006     D Wdtgio          S              8  0
006800001006     D dateu           S              8  0
006900001009
007000001006     D DATA_oggi       S               D   DATFMT(*eur)
007100001006
007200001215      *  titolo videata (lunghezza massima 33)
007300001215     D TIT_A           C                   CONST('***  ANAGRAFICA RITIRI FISSI-
007400980624     D                                       ***')
007500001006
007600980622      *  Descrizione opzioni (lunghezza massima 78)
007700001006     D DOPZ1           C                   CONST('2=Manutenzione   5=Visualizza-
007800001006     D                                          ')
007900010405     D DOPZ1A          C                   CONST('5=Visualizza                 -
008000010405     D                                          ')
008100001006     D DOPZ2           C                   CONST(' ')
008200001006
008300980622      *  Opzioni valide
008400001006     D DOPZ            S             20    INZ('25')
008500010405     D DOPZA           S             20    INZ('5')
008600001006
008700980521      *   S C H I E R E
008800070905     D MSG             S             78    DIM(14)CTDATA PERRCD(1)              MSG VIDEO
008900001009     D VOC             S              3    DIM(7) CTDATA PERRCD(1)              VOCI A VIDEO
009000980622     D SEL             S              1    DIM(20)                              SELEZIONI
009100980622     D OPZ             S              1    DIM(20)                              OPZIONI
009200001025     D L1              S              3  0 DIM(30)                              P.O. Gestiti
009300001006
009400980521      *   D S   I N T E R N E / E S T E R N E
009500001009
009600001009     D WLBDAT          DS                  INZ
009700001009     D  G02DAT                 1      8  0
009800001009     D  G02INV                 9     16  0
009900001009     D  G02ERR                17     17
010000001009     D  G02TGI                18     22  0
010100001020
010200001020     D v1ccra          DS
010300001020     D  v1cra1                        3  0
010400001020     D  v1cra2                        4  0
010500001020     D  v1cra3                        3  0
010600001020
010700001020     D v1ccor          DS
010800001110     D  v1cor1                        3  0
010900001020     D  v1cor2                        4  0
011000001020     D  v1cor3                        3  0
011100001009
011200001009     D Parm01          DS
011300001009     D  ppoe                   1      3  0
011400001009     D  pnor                   4     10  0
011500001009     D  psce                  11     11
011600060104     d  pfior01               12     12
011700060111     d  pcro                  13     22  0
011800001009
011900070905     d Azuteds       e ds                  Extname(Azute00f)
012000070905     d dDatiute      e ds
012100070905     d fidg09ds      e ds
012200060104     d fior02ds      e ds
012300070808     d FIOR37ds      e ds                  inz
012400070808     d  I37opz       e                     inz('1')
012500070808     d  I37cgi       e                     inz(*all'9')
012600070905     d Tibs34ds      e ds
012700070810      *
012800010207     D OG148         E DS
012900001025
013000010730     D DORS01        E DS
013100010730
013200001025     D DSUL06        E DS                  EXTNAME(TRUL06DS)
013300070905     D  LIN                    1     90  0 DIM(30)
013400001006
013500980521     D KPJBA         E DS
013600001025
013700980521     D                SDS
013800980521     D  VTCPGM                 1     10
013900981009
014000001006      *------------------------------------------------------------------------*
014100001009
014200060104      * Se richiamato vado subito al subfile
014300060104     c   01              Goto      sfl
014400001006
014500001006      * Emissione 1° Videata
014600980623
014700001006     C                   do        *hival
014800001009      * Pulizia 1° Videata
014900010313     C                   if        (*in28 = *off and *in90 = *off and
015000010313     C                             *in05 = *off) or (*in52 = *on)
015100001009     C                   exsr      Sr_pulvid1
015200001009     C                   endif
015300010405
015400010405     C                   eval      *in11 = *off
015500010405     C                   eval      *in13 = *off
015600980623
015700001009     C                   exfmt     fior021
015800980623
015900010405     C                   setoff                                       289091
016000010405
016100001006      * F3=Fine
016200001006     C   kc              leave
016300010405
016400010405      * F18=Cambio P.O. in gestione
016500010405     C   ks              eval      *in05 = *on
016600010405     C   ks              iter
016700010405
016800010405      * 05 on --> controllo P.O. in gestione
016900010405     C                   if        *in05 = *on
017000010405     C                   exsr      Sr_Contrpo
017100010405     C  n28              eval      *in05 = *off
017200010405     C   28              iter
017300010405     C                   endif
017400010405
017500010405      * Procedura ORM non attiva emetto l'errore
017600051027     C                   if        §ogorm = *Blanks
017700010405     C                             and *inks = *off and *in05 = *off
017800010405     C                             and *in12 = *off
017900010405     C                   movel     msg(8)        v1cmsg
018000010405     C                   seton                                        52  28
018100010405     C                   iter
018200010405     C                   endif
018300001006
018400001006      * F10=Immissione
018500010405     C                   if        *inkj = *on and *in05 = *off
018600010409     C                   movel     v1cfgs        v2poe
018700001009     C                   movel     '1'           v2scel
018800001009     C                   movel     *zeros        v2nor
018900001009     C                   exsr      Sr_Modifica
019000001009     C                   endif
019100980623
019200001006      * Controlli 1° videata
019300001006     C                   exsr      Sr_Contr01
019400980623
019500001025     C                   if        *in28 = *on   or
019600001025     C                             *in90 = *on   or
019700001025     C                             *in91 = *on
019800001025     C                   iter
019900001025     C                   endif
020000060104
020100060104     c     sfl           tag
020200980623
020300001006      * Imposto opzioni
020400001006     C                   exsr      Sr_Caropz
020500001020
020600001020     C                   movel     v1ccra        §ccra
020700001020     C                   movel     v1ccor        §ccor
020800001009      * Gestione subfile
020900001009     C                   exsr      Sr_Gessfl
021000060105     C                   if        *inkc = *on or *in01
021100001011     C                   leave
021200001011     C                   endif
021300981008
021400001006     C                   enddo
021500981204
021600001006     C                   eval      *inlr = *on
021700001009      *****************************************************************
021800001009      * PULIZIA 1° VIDEATA
021900001009      *****************************************************************
022000001009     C     Sr_pulvid1    BEGSR
022100001009
022200001009     C                   clear                   v1nor
022300001009     C                   clear                   v1ccra
022400001009     C                   clear                   v1dcra
022500001009     C                   clear                   v1ccor
022600001009     C                   clear                   v1dcor
022700070905     c                   clear                   v1ccgi
022800070905     c                   clear                   v1dcgi
022900010405     C                   clear                   v1cpoe
023000001009
023100001009     C                   endsr
023200001006      *****************************************************************
023300001006      * IMPOSTO OPZIONI
023400001006      *****************************************************************
023500001006     C     Sr_Caropz     BEGSR
023600001009
023700001006     C                   clear                   opz
023800001009
023900010405     C                   movel     dopz2         v2dse2
024000060104
024100060104      * non richiamato e non sono in sede
024200060104      * oppure richiamato e gestione
024300060104     c                   If        (not *In01 and Not *In12) or
024400060105     c                             (*In01 and Not *In02)
024500060104     C                   movel     dopz1         v2dse1
024600060104     C                   movea     dopz          OPZ
024700060104     c                   endif
024800060104      * non richiamato e sono in sede
024900060104      * oppure richiamato e interrogazione
025000060104     c                   If        (not *In01 and *In12) or
025100060105     c                             (*In01 and *In02)
025200060104     C                   movel     dopz1a        v2dse1
025300060104     C                   movea     dopza         OPZ
025400060104     c                   endif
025500001009
025600001006     C                   endsr
025700001009      **********************************************************************
025800010405      * CONTROLLO IL P.O. IN GESTIONE
025900001009      **********************************************************************
026000001009     C     Sr_Contrpo    begsr
026100001009
026200001009     C                   setoff                                       40
026300001025     C                   clear                   v1dfgs
026400001009
026500001025     C     v1cfgs        lookup    L1                                     30
026600001025     C     *in30         ifeq      *off
026700001025     C                   seton                                        40  28
026800001025     C                   movel     msg(7)        v1cmsg
026900010405     C                   goto      endsrcpo
027000010405     C                   endif
027100001025      * Decodifica
027200001025     C     v1cfgs        chain     azorg01L
027300001025     C                   if        %found(azorg01l)
027400001025     C                   movel     orgDES        v1dfgs
027500010207     C                   movel     orgde8        og148
027600051027     C                   if        §ogorm = *Blanks
027700010207     C                   movel     msg(8)        v1cmsg
027800010207     C                   seton                                        52  28
027900010207     C                   endif
028000001025     C                   else
028100001025     C                   seton                                        40  28
028200001025     C                   movel     msg(7)        v1cmsg
028300001025     C                   endif
028400001009
028500010405     C     endsrcpo      endsr
028600980521      **********************************************************************
028700980521      * CONTROLLI VIDEO 1
028800980521      **********************************************************************
028900001006     C     Sr_Contr01    begsr
029000001006
029100001009      * Reset indicatori
029200001009
029300001009     C                   setoff                                       414243
029400001009     C                   setoff                                       444546
029500001009     C                   setoff                                       50
029600010405
029700010405     C                   if        *in12 = *on and v1cpoe <> *zeros
029800010405     C                   exsr      Sr_contrpoe                                  controllo p.o.
029900010405     C   28              goto      endsrcontr
030000010405     C                   eval      v1cfgs = v1cpoe
030100010405     C                   eval      *in13 = *on
030200010405     C                   endif
030300001006
030400001011     C                   if        v1nor <> *zeros
030500010405      * non sono in sede
030600010405     C                   if        *in12 = *off
030700001006     C                   exsr      Sr_contrnor                                  controllo num.ORM
030800001011     C   28              goto      endsrcontr
030900010405     C                   eval      *in11 = *on
031000010405      * sono in sede
031100010405     C                   else
031200010405     C                   if        v1cpoe <> *zeros
031300010405     C                   exsr      Sr_contrnor                                  controllo num.ORM
031400010405     C   28              goto      endsrcontr
031500010405     C                   eval      *in11 = *on
031600010405     C                   else
031700010405     C                   movel     msg(12)       v1cmsg
031800010405     C                   seton                                        50  28
031900010405     C                   goto      endsrcontr
032000010405     C                   endif
032100010405     C                   endif
032200010405     C                   endif
032300001006
032400001011     C                   if        v1dcra <> *blanks
032500001110     C                             and v1ccra = *zeros
032600001012     C                   exsr      Sr_riccra                                    richiamo la ricerca
032700001011     C   28              goto      endsrcontr
032800001011     C                   endif
032900001006
033000001020     C                   if        v1ccra <> *zeros
033100001006     C                   exsr      Sr_contrcra                                  controllo cod.ritiro
033200001011     C   28              goto      endsrcontr
033300001011     C                   endif
033400001006
033500001011     C                   if        v1dcor <> *blanks
033600001110     C                             and v1ccor = *zeros
033700001012     C                   exsr      Sr_riccor                                    richiamo la ricerca
033800001011     C   28              goto      endsrcontr
033900001011     C                   endif
034000001006
034100001020     C                   if        v1ccor <> *zeros
034200001006     C                   exsr      Sr_contrcor                                  controllo cod.ordin.
034300001011     C   28              goto      endsrcontr
034400001011     C                   endif
034500070905
034600070905     c                   if        v1ccgi <> *blanks
034700070905     c                   eval      wccgi = v1ccgi
034800070905     c                   exsr      sr_contrcgi                                  contr. zona ritiro
034900070905     c   28              leavesr
035000070905     c                   eval      v1ccgi = wccgi
035100070905     c                   eval      v1dcgi = wdcgi
035200070905     c                   endif
035300010122
035400010122     C                   if        v1crmp <> *blanks
035500010122     C                   if        v1crmp <> 'M' and v1crmp <> 'P'
035600010122     C                   movel     msg(6)        v1cmsg                         errore
035700010122     C                   seton                                        46  28
035800010122     C                   endif
035900010122     C                   endif
036000001006
036100001011     C     endsrcontr    endsr
036200010405      **********************************************************************
036300010405      * CONTROLLO IL P.O. EMISSIONE
036400010405      **********************************************************************
036500010405     C     Sr_Contrpoe   begsr
036600010405
036700010405     C                   setoff                                       50
036800010405
036900010405     C     v1cpoe        chain     azorg01L
037000010405     C                   if        %found(azorg01l)
037100010405     C                   movel     orgde8        og148
037200051027     C                   if        §ogorm = *Blanks
037300010405     C                   movel     msg(8)        v1cmsg
037400010405     C                   seton                                        52  28
037500010405     C                   endif
037600010405     C                   else
037700010405     C                   seton                                        50  28
037800010405     C                   movel     msg(13)       v1cmsg
037900010405     C                   endif
038000010405
038100010405     C                   endsr
038200980527      **********************************************************************
038300010201      * CONTROLLI VIDEO 1     * controllo numero ritiro fisso *
038400980527      **********************************************************************
038500001006     C     Sr_contrnor   BEGSR
038600001009
038700001009     C                   movel     v1cfgs        kcfgs
038800001009     C                   movel     v1nor         knor
038900001006     C     Kfnor         chain     fnors01l
039000010405      * Non trovato
039100010405     C                   if        not%found(fnors01l)
039200001009     C                   movel     msg(1)        v1cmsg
039300001009     C                   seton                                        41  28
039400001006     C                   endif
039500001009
039600001006     C                   endsr
039700001009      **********************************************************************
039800001012      * CONTROLLI VIDEO 1     * ricerca ritiro *
039900001009      **********************************************************************
040000001012     C     Sr_ricCra     BEGSR
040100001009
040200001018     C                   eval      *in90 = *on
040300070808     c                   reset                   FIOR37ds
040400070808     c                   movel(p)  V1Dcra        I37rag
040500070808     c                   movel     V1Cfgs        I37fgs
040600070808     c                   movel(p)  FIOR37ds      KPJBU
040700070808     c                   call      'FIOR37R'
040800070808     c                   parm                    KPJBA
040900070808     c                   movel     KPJBU         FIOR37ds
041000070808     c                   select
041100070808     c                   when      O37err <> *blanks
041200070808     c                   movel     O37msg        V1Cmsg
041300070808     c                   seton                                        47  28
041400070808     c                   when      O37cro <> *zeros
041500070808     c                   movel     O37cro        V1Ccra
041600070808     c                   endsl
041700001009
041800001009     C                   endsr
041900001012      **********************************************************************
042000001012      * CONTROLLI VIDEO 1     * ricerca ordinante *
042100001012      **********************************************************************
042200001012     C     Sr_ricCor     BEGSR
042300001012
042400001018     C                   eval      *in90 = *on
042500070808     c                   reset                   FIOR37ds
042600070808     c                   movel(p)  V1Dcor        I37rag
042700070808     c                   movel     V1Cfgs        I37fgs
042800070808     c                   movel(p)  FIOR37ds      KPJBU
042900070808     c                   call      'FIOR37R'
043000070808     c                   parm                    KPJBA
043100070808     c                   movel     KPJBU         FIOR37ds
043200070808     c                   select
043300070808     c                   when      O37err <> *blanks
043400070808     c                   movel     O37msg        V1Cmsg
043500070808     c                   seton                                        48  28
043600070808     c                   when      O37cro <> *zeros
043700070808     c                   movel     O37cro        V1Ccor
043800070808     c                   endsl
043900001012
044000001012     C                   endsr
044100001009      **********************************************************************
044200001009      * CONTROLLI VIDEO 1     * codice ritiro *
044300001009      **********************************************************************
044400001009     C     Sr_contrcra   BEGSR
044500001009
044600001009     C                   movel     v1ccra        knacr
044700001009     C     knacr         chain     fnacr01l
044800001009     C                   if        %found(fnacr01l)
044900071030     C                   if        ACRatb <> *blanks
045000001009     C                   movel     msg(2)        v1cmsg
045100001009     C                   seton                                        42  28
045200001009     C                   endif
045300001009     C                   else
045400001009     C                   movel     msg(2)        v1cmsg
045500001009     C                   seton                                        42  28
045600001009     C                   endif
045700001009
045800001009     C                   endsr
045900001009      **********************************************************************
046000001009      * CONTROLLI VIDEO 1     * codice ordinante *
046100001009      **********************************************************************
046200001009     C     Sr_contrcor   BEGSR
046300001009
046400001009     C                   movel     v1ccor        knacr
046500001009     C     knacr         chain     fnacr01l
046600001009     C                   if        %found(fnacr01l)
046700071030     C                   if        ACRatb <> *blanks
046800001009     C                   movel     msg(3)        v1cmsg
046900001009     C                   seton                                        43  28
047000001009     C                   endif
047100001009     C                   else
047200001009     C                   movel     msg(3)        v1cmsg
047300001009     C                   seton                                        43  28
047400001009     C                   endif
047500001009
047600001009     C                   endsr
047700070905
047800070905      *------------------------------------------------------------------------*
047900070905      * CODICE GIRO
048000070905      *------------------------------------------------------------------------*
048100070905     c     sr_contrcgi   begsr
048200070905
048300070905     c                   clear                   v1dcgi
048400070905
048500070905      * ricerca giro con "?"
048600070905     c                   if        %scan('?':wccgi) > *zeros
048700070905     c                   clear                   fidg09ds
048800070905     c                   eval      d09iop0 = 'P01'
048900070905     c                   eval      d09ifgs = v1cfgs
049000070905     c                   eval      d09idat = *date
049100070905     c                   eval      d09itug = 'R'
049200070905     c                   eval      kpjbu = fidg09ds
049300070905     c                   call      'FIDG09R'
049400070905     c                   parm                    kpjba
049500070905     c                   eval      fidg09ds = kpjbu
049600070905      *     imposto il giro scelto
049700070905     c                   eval      wccgi = d09ocgi
049800070905     c                   endif
049900070905      *     controllo
050000070905     c                   if        wccgi <> *blank
050100070905     c                   exsr      ctrgiro
050200070905      *     giro errato
050300070905     c                   if        d09oerr = '1'
050400070905     c                   eval      *in28 = *on
050500070905     c                   eval      *in44 = *on
050600070905     c                   eval      v1cmsg = msg(14)
050700070905     c                   leavesr
050800070905     c                   endif
050900070905      *     imposto la descrizione del giro
051000070905     c                   if        d09ocgi <> ' '
051100070905     c                   eval      wdcgi = d09odes
051200070905     c                   endif
051300070905     c                   endif
051400070905
051500070905     c                   endsr
051600070905
051700070905      *------------------------------------------------------------------------*
051800070905      *   Controllo e decodifica giro
051900070905      *------------------------------------------------------------------------*
052000070905     c     ctrgiro       begsr
052100070905
052200070905     c                   clear                   fidg09ds
052300070905     c                   eval      d09iop0 = '001'
052400070905     c                   eval      d09ifgs = v1cfgs
052500070905     c                   eval      d09idat = *date
052600070905     c                   eval      d09icgi = wccgi
052700070905     c                   eval      d09itug = 'R'
052800070905     c                   eval      kpjbu = fidg09ds
052900070905     c                   call      'FIDG09R'
053000070905     c                   parm                    kpjba
053100070905     c                   eval      fidg09ds = kpjbu
053200070905
053300070905     c                   endsr
053400070905
053500001009      **********************************************************************
053600001009      * GESTIONE SUBFILE
053700001009      **********************************************************************
053800001009     C     Sr_Gessfl     begsr
053900001009
054000010405     C                   z-add     1             recsf
054100001009     C                   do        *hival
054200010405     C                   setoff                                       49
054300001009      * Pulisco subfile
054400001009     C                   z-add     *zeros        nrr1              4 0
054500001009     C                   eval      *in21 = *off
054600001009     C                   write     fior02c
054700001009     C                   eval      *in20 = *on
054800001009     C                   eval      *in21 = *on
054900010405
055000010405      * Non è stato messo un numero ritiro
055100010405     C                   if        *in11 = *off
055200001009
055300001009      * Carico il subfile in base alle scelte fatte
055400010405      * solo 9999 record xchè il file video è definito così
055500010405
055600010405     C  n12v1cfgs        setll     fnors01l
055700010405     C  n13
055800010405     Can 12*loval        setll     fnors01l
055900010405     C   13
056000010405     Can 12v1cpoe        setll     fnors01l
056100001009     C                   do        *hival
056200010405     C  n12v1cfgs        reade     fnors01l
056300010405     C  n13
056400010405     Can 12              read      fnors01l
056500010405     C   13
056600010405     Can 12v1cpoe        reade     fnors01l
056700001009     C                   if        %eof
056800001009     C                   leave
056900001009     C                   endif
057000001009
057100010405     C                   if        nrr1 >= 9998
057200010405     C                   seton                                        28
057300010405     C                   movel     msg(10)       v2cmsg
057400001009     C                   leave
057500001009     C                   endif
057600001009
057700001020     C                   if        (v1ccra <> *zeros  and ORScra <> §ccra ) or
057800130411     C                             (v1ccor <> *zeros  and ORScor <> §ccor )
057900001009     C                   iter
058000001009     C                   endif
058100010123
058200010123     C                   if        v1crmp <> *blanks and v1crmp <> ORSrmp
058300010123     C                   iter
058400010123     C                   endif
058500071030
058600071030      * controllo se c'è il giro sul ritiro fisso
058700071030     C                   movel     orspoe        kcfgs
058800071030     C                   movel     orsnor        knor
058900071030     C     Kfnor         chain     fnors11l
059000071030     c                   if        %found(fnors11l) and ors1cgi <> *blanks
059100071030      * se richiesta selezione per giro escludo se diverso
059200071030     c                   if        v1ccgi <> *blanks and v1ccgi <> ors1cgi
059300071030     c                   iter
059400071030     c                   endif
059500071030     c                   eval      v2cgi = 'S'
059600071030     c                   else
059700071030      * se richiesta selezione per giro escludo se diverso
059800071030     c                   if        v1ccgi <> *blanks
059900071030     c                   iter
060000071030     c                   endif
060100071030     c                   clear                   v2cgi
060200071030     c                   endif
060300001009
060400010405     C  n12              movel     v1cfgs        v2cfgs
060500010405     C  n12              movel     v1dfgs        v2dfgs
060600001009     C                   exsr      Sr_Carsfl
060700001009     C                   enddo
060800010405     C                   endif
060900010405
061000010405      * E' stato messo un numero ritiro
061100010405     C                   if        *in11 = *on
061200010405     C     Kfnor         chain     fnors01l
061300010405     C                   if        %found(fnors01l)
061400010405     C  n12              movel     v1cfgs        v2cfgs
061500010405     C  n12              movel     v1dfgs        v2dfgs
061600071030      * controllo se c'è il giro sul ritiro fisso
061700071030     C     Kfnor         chain     fnors11l
061800071030     c                   if        %found(fnors11l) and ors1cgi <> *blanks
061900071030     c                   eval      v2cgi = 'S'
062000071030     c                   else
062100071030     c                   clear                   v2cgi
062200071030     c                   endif
062300071030
062400010405     C                   exsr      Sr_Carsfl
062500010405     C                   endif
062600010405     C                   endif
062700010405
062800010405     C     emettosf      tag
062900010405
063000010405      * Controllo se recsf è maggiore di nrr
063100010405     C                   if        recsf > nrr1
063200010405     C                   z-add     1             recsf
063300010405     C                   endif
063400001009
063500001009      * Se non ho caricato emetto una videata di segnalazione.
063600001009     C                   if        nrr1 = *zeros
063700001009     C                   exfmt     fior023
063800010405     C                   eval      *in91 = *on
063900001009     C                   endif
064000001009
064100001009      * Se ho caricato emetto il subfile
064200001009     C                   if        nrr1 > *zeros
064300001009     C                   write     fior022
064400001009     C                   exfmt     fior02c
064500001009     C                   endif
064600001009
064700001011      * F3=Fine
064800001011     C   kc              leave
064900001011
065000001009      * F12=Ritorno
065100001009     C   kl              leave
065200001009
065300001009      * F10=Immissione
065400001009     C                   if        *inkj = *on
065500010409     C                   movel     v1cfgs        v2poe
065600001009     C                   movel     '1'           v2scel
065700001009     C                   movel     *zeros        v2nor
065800001009     C                   exsr      Sr_Modifica
065900001009     C                   leave
066000001009     C                   endif
066100001009
066200001009      *    Leggo Scelte Effettuate dall'utente
066300001107
066400001009     C                   if        nrr1 >0
066500010405
066600010405     C                   eval      nrr1 = *zeros
066700001009     C                   do        *hival
066800010405     C                   eval      nrr1 = nrr1 +1
066900010405     C     nrr1          chain     fior02s                            30
067000010405     C                   if        *in30 = *on
067100010405     C                   leave
067200010405     C                   endif
067300010405
067400010405     C                   setoff                                       28  49
067500010405
067600010405      * Controllo se scelta valida
067700010405     C     v2scel        lookup    opz                                    30
067800010405     C                   if        *in30 = *off
067900010405     C                   seton                                        28  49
068000010405     C                   movel     msg(11)       v2cmsg
068100010405     C                   z-add     nrr1          recsf
068200010405     C                   update    fior02s
068300010405     C                   goto      emettosf
068400010405     C                   endif
068500010405
068600001107     C                   if        v2scel <> *blanks
068700010405     C                   z-add     nrr1          recsf
068800001009     C                   exsr      Sr_Modifica
068900001009     C                   endif
069000010405
069100010405     C                   update    fior02s
069200001107
069300001009     C                   enddo
069400001009     C                   endif
069500001009
069600001009     C                   enddo
069700001009
069800001009     C                   endsr
069900001009      **********************************************************************
070000001009      * CARICO SUBFILE
070100001009      **********************************************************************
070200001009     C     Sr_Carsfl     begsr
070300001009
070400001009     C                   clear                   v2scel
070500010405     C                   eval      v2poe  = ORSpoe
070600001009     C                   eval      v2nor  = ORSnor
070700001009     C                   eval      v2ccra = ORScra
070800001009     C                   eval      v2crlu = ORSrlu
070900001009     C                   eval      v2crma = ORSrma
071000001009     C                   eval      v2crme = ORSrme
071100001009     C                   eval      v2crgi = ORSrgi
071200001009     C                   eval      v2crve = ORSrve
071300010730     C                   eval      dors01 = ORSflo
071400010730     C                   if        §orsdif <> *zeros and §orsdff <> *zeros
071500010730     C                   eval      v2fer = 'S'
071600010730     C                   else
071700010730     C                   eval      v2fer = *blanks
071800010730     C                   endif
071900031030     c                   if        orsatb = 'A'
072000070905     c                   eval      V2ann = 'A'
072100031030     c                   else
072200031030     c                   clear                   v2ann
072300031030     c                   endif
072400001009
072500001009      * Decodifico Ritiro
072600001009     C     v2ccra        chain     fnacr01l
072700001009     C                   if        %found(fnacr01l)
072800001009     C                   eval      v2dcra = ACRrsr
072900010419     C                   else
073000010419     C                   clear                   v2dcra
073100001009     C                   endif
073200001009
073300001009      * Decodifico voce e imposto qtà relativa
073400001009     C                   select
073500001009     C                   when      ORSncl <> *zeros
073600001009     C                   movel     voc(1)        v2voc
073700001009     C                   z-add     ORSncl        v2qta
073800001009
073900001009     C                   when      ORSpkg <> *zeros
074000001009     C                   movel     voc(2)        v2voc
074100001009     C                   z-add     ORSpkg        v2qta
074200001009
074300001009     C                   when      ORSvlm <> *zeros
074400001009     C                   movel     voc(3)        v2voc
074500001009     C                   z-add     ORSvlm        v2qta
074600001009
074700001009     C                   when      ORSbnc <> *zeros
074800001009     C                   movel     voc(4)        v2voc
074900001009     C                   z-add     ORSbnc        v2qta
075000001009
075100001009     C                   when      ORSblc <> *zeros
075200001009     C                   movel     voc(5)        v2voc
075300001009     C                   z-add     ORSblc        v2qta
075400001009
075500001009     C                   when      ORSatt <> *zeros
075600001009     C                   movel     voc(6)        v2voc
075700001009     C                   z-add     ORSatt        v2qta
075800001009
075900001009     C                   when      ORSmtc <> *zeros
076000001009     C                   movel     voc(7)        v2voc
076100001009     C                   z-add     ORSmtc        v2qta
076200001009
076300001011     C                   other
076400001011     C                   z-add     *zeros        v2qta
076500001011     C                   movel     *blanks       v2voc
076600001011
076700001009     C                   endsl
076800001009
076900001009     C                   add       1             nrr1
077000001009     C                   write     fior02s
077100001009
077200001009     C                   endsr
077300001009      **********************************************************************
077400001009      * RICHIAMO IL PROGRAMMA DI MODIFICA
077500001009      **********************************************************************
077600001009     C     Sr_Modifica   begsr
077700001009
077800001009     C                   clear                   parm01
077900010405     C                   movel     v2poe         ppoe
078000001009     C                   movel     v2nor         pnor
078100001009     C                   movel     v2scel        psce
078200060104     c   01              eval      pfior01 = 'S'
078300060111     c   01              eval      pcro = i02cro
078400060111     c  n01              Clear                   pcro
078500001009     C                   movel     parm01        kpjbu
078600001009     C                   call      'FIOR03R'
078700001009     C                   parm                    kpjba
078800001009     C                   movel     kpjbu         parm01
078900001009
079000001009     C                   endsr
079100980923      *****************************************************************
079200980923      * ROUTINE INIZIALE
079300980923      *****************************************************************
079400980923     C     *INZSR        BEGSR
079500980923      *
079600980521     C     *ENTRY        PLIST
079700980521     C                   PARM                    KPJBA
079800060104     c                   Parm                    fior02ds
079900060104
080000060104      * Richiamato per interrogazione/manutenzione da anagrafica ritiri
080100060104     c                   If        %Parms > 1
080200060104     c                   Eval      *In01 = *On
080300060104     c                   Eval      v1cpoe = i02poa
080400060104     c                   Move      i02cro        v1ccra
080500060104     c                   Clear                   v1ccor
080600060104     c                   Clear                   v1nor
080700060105     c                   Eval      *In02 = (i02ric = '1')
080800060104     c                   Else
080900060104     c                   Eval      *In01 = *Off
081000060105     c                   Eval      *In02 = *Off
081100060104     c                   EndIf
081200001011      *
081300010405     C                   movel     TIT_A         VTCtit
081400070905
081500070905     c     *dtaara       define    §azute        azuteds
081600070905     c     *dtaara       define    §datiute      ddatiute
081700070905     c                   in(e)     *dtaara
081800070905     c                   if        %error or rsut = *blanks
081900070905     c                   clear                   tibs34ds
082000070905     c                   call      'TIBS34R'
082100070905     c                   parm                    tibs34ds
082200070905     c                   in        *dtaara
082300070905     c                   endif
082400010405
082500010405      * Da sede non imposto il p.o. di gestione
082600010405     C                   if        simfel = 0
082700010405     C                   eval      *in12 = *on
082800010405     C                   else
082900001006      * IMPOSTO IL P.O. DI GESTIONE
083000071109     c                   if             %Parms > 1
083100071109     c                             and  I02poa > *zeros
083200071109     c                   move      I02poa        V1Cfgs
083300071109     c                   else
083400070905     C                   movel     dutpou        V1CFGS
083500071109     c                   endif
083600070905      * abilito f18
083700070905     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
083800010405     C                   endif
083900001025
084000001025      * Carico schiera P.O. gestiti
084100001025     C                   CLEAR                   DSUL06
084200001025     C                   MOVE      '£1'          D06COD
084300001025     C                   MOVEL     SIMFEL        D06KEY
084400001025     C                   MOVEL     DSUL06        KPJBU
084500001025     C                   CALL      'TRUL06R'
084600001025     C                   PARM                    KPJBA
084700001025     C                   MOVEL     KPJBU         DSUL06
084800001025     C                   MOVEA     LIN           L1
084900001009
085000130411     c                   clear                   Og148
085100001006     C     v1cfgs        chain     azorg01L
085200001006     C                   if        %found(azorg01l)
085300001006     C                   movel     orgDES        v1dfgs
085400010207     C                   movel     orgde8        og148
085500051027     C                   if        §ogorm = *Blanks
085600010207     C                   movel     msg(8)        v1cmsg
085700010207     C                   seton                                        52  28
085800010207     C                   endif
085900001006     C                   else
086000001006     C                   clear                   v1dfgs
086100001013     C                   endif
086200980923
086300980923      * reperisco data e ora
086400980923     C                   TIME                    W0140
086500980923      * UDATE IN GGMMAAAA
086600980923     C                   MOVE      W0140         WDTGIO
086700980923      * UDATE IN AAAAMMGG
086800980923     C                   Z-ADD     WDTGIO        G02DAT
086900980923     C                   MOVEL     *BLANK        G02ERR
087000980923     C                   CALL      'XSRDA8'
087100980923     C                   PARM                    WLBDAT
087200980923     C                   MOVEL     G02INV        DATEU
087300980923     C     *iso          MOVEL     DATEU         DATA_oggi
087400980923
087500001009     C     Kfnor         klist
087600001009     C                   kfld                    kcfgs
087700001009     C                   kfld                    knor
087800001006
087900010405     C                   endsr
088000980521      *---------------------------------------------------------------------------------------------
088100980521** MSG  Lungh. 78                                                            *
088200010201Codice ritiro fisso errato                                                          1
088300010420Cliente mittente non valido                                                         2
088400010201Cliente ordinante non valido                                                        3
088500130411-----Libero                                                                         4
088600001009Immettere una selezione valida                                                      5
088700001009Scelta errata                                                                       6
088800070418Filiale in gestione errata                                                          7
088900010207Procedura ORM non attiva                                                            8
089000130411-----Libero                                                                         9
089100010405Ampiezza massima subfile superata                                                  10
089200010405Scelta non valida                                                                  11
089300070418Immettere la filiale di emissione                                                  12
089400070418Filiale emissione errata                                                           13
089500070905Codice Giro non corretto                                                           14
089600001009** VOC  Lungh. 3                                                             *
089700001009COL                                                                                 1
089800001009PKG                                                                                 2
089900001009VLM                                                                                 3
090000001009BNC                                                                                 4
090100001009BLC                                                                                 5
090200001009AUT                                                                                 6
090300001009MOT                                                                                 7
