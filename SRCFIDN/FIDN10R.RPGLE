000100951009     H*------------------------------------------------------------------------*
000200971023     H* MANUTENZIONE FILE DESCRIZIONI                                          *
000300951009     H*------------------------------------------------------------------------*
000400971023     H DECEDIT('0,') DATEDIT(*ymd.)
000500951009     F*------------------------------------------------------------------------*
000600980714     FFIDN10D   CF   E             WORKSTN sfile(fi10s01:nrr1)
000700980710     F                                     sfile(fi10s02:nrr2)
000800971023     F                                     INFDS(dn10DS)
000900971024     FFNDCS01L  UF A E           K DISK
001000980610     ffndfa01l  if   e           k disk
001100971023     F*
001200981027     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              dec VIDEO
001300980716     D TES             S             10    DIM(2) CTDATA PERRCD(1)              Tes VIDEO
001400981026     D ERR             S             50    DIM(2) CTDATA PERRCD(1)              Err Video
001500981215     D*
001600951009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700980619     D cncr80        E DS
001800981026     D dtld          E DS
001900981026     D DSbs02        E DS                  EXTNAME(tibs02DS)
002000971022     D DSDN10        E DS                  EXTNAME(FIDN10DS)
002100980609     D  i10key                 1     34
002200980716     D  Anno                   4      7
002300980716     D  Po                     8     10
002400980716     D  ca                    11     17
002500980716     D  evento                 8     14
002600951009     D KPJBA         E DS
002700971023     D dn10DS          DS
002800971023     D  NRG                  370    370
002900971023     D  PRIMA                378    379B 0
003000971024     D CURSOR          DS
003100971024     D  RIRI                   1      2B 0
003200971024     D  R$$                    2      2
003300971024     D  COCO                   3      4B 0
003400971024     D  C$$                    4      4
003500980611     D s1ds            DS
003600980715     D  v1hkey                 1     10
003700980714     D  v1htpd                 1      1
003800980714     D  v1hstd                 2      2
003900980714     D  v1hnks                 3      9
004000980714     D  v1htrc                10     10
004100980714     d  v1cnot                11     45
004200980714     D s2ds            DS
004300980715     D  v2hkey                 1     10
004400980714     D  v2htpd                 1      1
004500980714     D  v2hstd                 2      2
004600980714     D  v2hnks                 3      9
004700980714     D  v2htrc                10     10
004800980715     D  v2odim                11     18  0
004900980715     D  v2ohim                19     22  0
005000980715     D  v2ocfa                23     25  0
005100980715     d  v2cnot                26     60
005200980715     D  v2opru                61     70
005300980715     D  v2opos                71     73  0
005400980720     D  v2ostn                74     79
005500980422     D                 DS
005600980608     D  w035a                  1     35
005700951009     D                SDS
005800971016     D  VTCPGM                 1     10
005900980609      *
006000980609     D WLBDAT          DS                  INZ
006100980609     D  G02DAT                 1      8  0
006200980609     D  G02INV                 9     16  0
006300980609     D  G02ERR                17     17
006400980609     D  G02TGI                18     22  0
006500990610     d* schiera campo note testo
006600990610     d DS_DESCR        DS
006700990610     d WNOTET                       550
006800990610     d tno                           35    DIM(15)  overlay(Wnotet:1)           Note testo
006900990610     d righe                        105    DIM(5)   overlay(Wnotet:1)           Righe note testo
007000990610     d V3Cpri                       100             overlay(WNOTET:1)           1° 100 caratteri
007100990610     d v3csec                       100             overlay(WNOTET:106)         2° 100 caratteri
007200990610     d v3cter                       100             overlay(WNOTET:211)         3° 100 caratteri
007300990610     d v3cqua                       100             overlay(WNOTET:316)         4° 100 caratteri
007400990610     d v3cqui                       100             overlay(WNOTET:421)         5° 100 caratteri
007500980716      *
007600980720     D s1sav           s                   LIKE(s1ds)
007700980720     D s1sav2          s                   LIKE(s1ds)
007800980720     D s1sav3          s                   LIKE(s1ds)
007900980720      *
008000980720     D s2sav           s                   LIKE(s2ds)
008100980720     D s2sav2          s                   LIKE(s2ds)
008200980720     D s2sav3          s                   LIKE(s2ds)
008300980716     D prog            s                   LIKE(DCSpno)
008400980609      *
008500981216     D x               s              2  0
008600990610     D t               s              2  0
008700990610     D wtesto          s              3
008800981216      *
008900980610     D TIT1            C                   CONST('  IMMISSIONE  ')
009000980610     D TIT2            C                   CONST(' MANUTENZIONE ')
009100980610     D TIT3            C                   CONST('INTERROGAZIONE')
009200951009     C****************************************************************
009300951009     C*  RIEPILOGO INDICATORI
009400951009     C***************************************************************
009500981216     c* 01    - flag modalità diverso da *blanks e da "V" (Manutenzione e conferma)
009600971024     c* 02    - interrogazione
009700971029     c* 05    - protezione record subfile
009800971029     c* 06    - Disabilitazione rollup
009900990610     c* 11    - Abilita / disabilita più righe note stampa
010000980610     c* 15/16 - acceso in caso di gestione NOTE
010100981216     c* 20    - COMODO gestione Subfile 1
010200981216     c* 21    - COMODO gestione Subfile 1
010300981216     c* 23    - COMODO gestione Subfile 1
010400981216     c* 60    - COMODO gestione Subfile 2
010500981216     c* 61    - COMODO gestione Subfile 2
010600981216     c* 63    - COMODO gestione Subfile 2
010700971024     C* 30    - COMODO
010800971022     C* 31    - COMODO
010900971022     C* 32    - COMODO
011000971023     C* 40    - ERRORE
011100980716     c* 50    - Gestione Dati relativi ad una C.A.
011200980716     c* 51    - Gestione Dati relativi ad un evento
011300981126     c* 52    - Gestione Interlocutore
011400951009     C***************************************************************
011500971016     C*
011600981216     C*
011700981216     C     *ENTRY        PLIST
011800981216     C                   PARM                    KPJBA
011900981216     C                   MOVEL     KPJBU         DSDN10
012000981216     c*
012100981216     c                   if        i10tla = 'C'
012200981216     c                   goto      fine
012300981216     c                   endif
012400051025
012500980716     c* Nel caso di tipo elaborazione RECUPERO o SCRITTURA descrizione non vado a cercare i
012600980716     c* dati nel subfile
012700980716     c                   select
012800980716     c                   WHEN      i10flm = 'R'
012900980716     c     kdcs          chain     fndcs01l                           30
013000981029    1c                   if        *in30 = *on
013100980716     c                   movel     'E'           o10err
013200980716     c                   else
013300980716     c                   movel     dcsnot        o10not
013400981029    1c                   endif
013500980803     c                   movel     dsdn10        kpjbu
013600980716     c                   WHEN      i10flm = 'W'
013700980716     c     kdcs          chain     fndcs01l                           30
013800981029    1c                   if        *in30 = *on
013900980716     c                   clear                   fndcs000
014000980716     c                   movel     i10tpd        dcstpd
014100980716     c                   movel     i10nkt        dcsnkt
014200980716     c                   movel     i10std        dcsstd
014300980716     C                   movel     i10nks        dcsnks
014400980716     c                   movel     i10trc        dcstrc
014500980716     c                   z-add     1             dcspno
014600980716     c                   movel     i10not        dcsnot
014700980730     c                   move      knmus         dcspru
014800980730     c                   movel     simfel        dcspos
014900980716     c                   write     fndcs000
015000980716     c                   else
015100980730     c                   move      knmus         dcspru
015200980730     c                   movel     simfel        dcspos
015300980716     c                   movel     i10not        dcsnot
015400980716     c                   update    fndcs000
015500981029    1c                   endif
015600980716     c
015700981026     c
015800980716     c                   OTHER
015900981026     c*
016000980610     C* immissione / manutenzione / interrogazione
016100981029    1c     i10flm        ifeq      'M'
016200980610     c                   movel     tit1          vtcmod
016300981029    1c                   endif
016400980610     c*
016500981029    1c     i10flm        ifeq      'V'
016600980610     c                   movel     tit2          vtcmod
016700981029    1c                   endif
016800980610     c*
016900981029    1c     i10flm        ifeq      'I'
017000980610     c                   movel     tit3          vtcmod
017100980825     c                   seton                                        08
017200981029    1c                   endif
017300980716      * decodifico la testata con il numero C.A. oppure il numero EVENTO
017400980716     c                   z-add     i10dta        vtcdta
017500981029    1C     I10tpd        ifeq      'C'
017600980716      *C.A.
017700980716     c                   seton                                        50
017800980716     c                   movel     ca            vtcca
017900980716     c                   movel     po            vtcpo
018000980716     c                   movel     tes(1)        vtccos
018100980716     c                   else
018200980716      *EVENTO
018300980716     c                   seton                                        51
018400980716     c                   movel     evento        vtceve
018500980716     c                   movel     tes(2)        vtccos
018600981029    1c                   endif
018700980610     c* se sono in manutenzione fase decodifico la fase
018800981029    1c     i10trc        ifeq      'N'
018900980710     c* e sto gestendo una C.A.
019000980716     c     i10tpd        andeq     'C'
019100980610     c                   movel     i10nks        wcfa              3 0
019200980610     c     wcfa          chain     fndfa01l                           30
019300980720     c  n30              movel     dfades        v2tcom
019400980720     c  n30              z-add     wcfa          v2tcfa
019500980720     c   30              movel     *blanks       v2tcom
019600981029    1c                   endif
019700981026     c* Descrizione Evento
019800981029    1c                   if        i10tpd = 'E'
019900980616     c                   movel     msg(5)        v1tcom
020000981029    1c                   endif
020100981026     c* Descrizione Aggiuntiva Anomalia
020200981029    1c                   if        i10std = 'A'
020300980610     c                   movel     msg(1)        v1tcom
020400981029    1c                   endif
020500981026     c* Descrizione Motivo del Danno
020600981029    1c                   if        i10std = 'M'
020700980610     c                   movel     msg(2)        v1tcom
020800981029    1c                   endif
020900981026     c* Descrizione dei Pezzi danneggiati
021000981029    1c                   if        i10std = 'P'
021100980610     c                   movel     msg(3)        v1tcom
021200981029    1c                   endif
021300981026     c* Descrizione dei KG / Pezzi presenti
021400981029    1c                   if        i10std = 'K'
021500980610     c                   movel     msg(4)        v1tcom
021600981029    1c                   endif
021700981126     c* Interlocutore
021800981029    1c                   if        i10std = 'N'
021900980715     c                   movel     msg(6)        v1tcom
022000980716     c                   seton                                        52
022100981029    1c                   endif
022200981027     c* quando si tratta di note a livello di testo emetto una finestra che mi chiede il codice
022300990107     c* testo al quale mi riferisco ..... se non mi è stato già passato dal PGM chiamante
022400981027     c
022500990107    1c                   if        i10std = 'T'
022600990610     c                   setoff                                       11
022700990107     c                   if        i10nks = *blanks
022800990610     c                   exsr      SUB_WINT
022900981029     c* se è stato premuto F12 vado a fine pgm
023000981029    2c                   if        *inKL
023100981029     c                   eval      o10f12 = 'S'
023200981030     c                   goto      fine
023300981029    2c                   endif
023400990107     c* else decodifico il codice testo
023500990107     c                   else
023600990107     c*
023700990107     c                   clear                   dsbs02
023800990107     c                   clear                   dtld
023900990107     C                   MOVEL     'C'           t02mod
024000990107     C                   MOVEL     knsif         t02sif
024100990107     C                   MOVEL     'TLD'         t02cod
024200990610     C                   MOVEL(P)  i10nks        t02ke1
024300990107      *
024400990107     C                   CALL      'TIBS02R'
024500990107     C                   PARM                    KPJBA
024600990107     C                   PARM                    dsbs02
024700990107      *
024800990107    3C                   if        t02err = *BLANKS
024900990107     C                   MOVEL     T02UNI        DTLD
025000990107     C                   MOVEL     §tlddesc      v1tcom
025100990610     c* controllo se si possono inserire più di 2 righe
025200990610     c                   if        §tldnote = 'P'
025300990610     c                   seton                                        11
025400990610     c                   endif
025500990107      *
025600990107     c                   else
025700990107     C                   clear                   v1tcom
025800990107      *
025900990107    3C                   endif
026000990107     c*
026100990107     c                   endif
026200981030    1c                   endif
026300971024     c*
026400971024     c* pulizia campi di output
026500971024     c                   clear                   o10not
026600971024     c                   clear                   o10f03
026700971024     c                   clear                   o10f12
026800971024     c                   clear                   o10err
026900971024     c                   clear                   o10msg
027000971024     c*
027100971024     C                   Z-ADD     0             RIRI
027200971024     C                   Z-ADD     0             COCO
027300971024     c*
027400971024     c     i10flm        comp      ' '                                0101
027500980526     c   01i10flm        comp      'V'                                0101
027600971024     c     i10flm        comp      'I'                                    02
027700971024     c*
027800981215     c* controllo se richiesta la gestione delle NOTE TESTO / o / DESCRIZIONI / o / NOTE
027900981215      * NOTE TESTO
028000981216    1c                   if        i10std = 'T'
028100981215      * manutenzione note testo
028200981216    2c                   if        i10flm = 'M'
028300981215      * riempimento videata note testo
028400981215     c                   exsr      ries03
028500981216    2c                   endif
028600981215      * routine gestione note testo
028700981215     c                   exsr      SUB_TESTO
028800981215      *
028900981216   x1c                   ELSE
029000981215      *
029100981216    3c                   if        i10flm = 'M'     and
029200981216     c                             i10trc = 'D'     and
029300981216     c                             wcontd = ' '
029400980713     c* riempimento subfile descrizioni
029500980713     c                   exsr      ries01
029600981216     c                   movel     '1'           wcontd            1
029700981216     c                   clear                   wnot
029800981216     c                   z-add     1             wprimo            4 0
029900981216     c                   move      i10key        keysav
030000981216     c                   endif
030100981216     c*
030200981216    3c                   if        i10trc = 'N'     and
030300990111     c                             wcont  = ' '     and
030400990111     c                             i10flm = 'M'
030500980713     c* riempimento subfile note
030600980713     c                   exsr      ries02
030700981216     c                   movel     '1'           wcont             1
030800980713     c                   move      i10key        keysav
030900981216     c                   clear                   wnot
031000981216     c                   z-add     1             wprimo            4 0
031100981216    3c                   endif
031200981216      *
031300981029    2c     i10trc        ifeq      'N'
031400980710     c                   exsr      SUB_NOTE
031500981029    2c                   end
031600980710     c*
031700981029    2c     i10trc        ifeq      'D'
031800980710     c                   exsr      SUB_DESC
031900981029    2c                   end
032000981026     c*
032100981216    1c                   endif
032200981216     c*
032300980716     c                   ENDSL
032400980716     c*
032500981030     c     fine          tag
032600971024    1c     i10tla        ifeq      ' '
032700971024     c                   return
032800971024   x1c                   else
032900981029     c                   clear                   dsbs02
033000981029     c                   movel     'C'           t02tla
033100981029     c                   call      'TIBS02R'
033200981029     c                   parm                    kpjba
033300981029     c                   parm                    dsbs02
033400971024     c                   seton                                        lr
033500971024    1c                   end
033600971024     c
033700980710     C****************   GESTIONE SUBILE NOTE *************************
033800980710     C     SUB_NOTE      BEGSR
033900980714      *
034000980714    1c     i10tla        ifne      'C'
034100980714    2c     i10flm        ifeq      *blanks
034200980714     c     i10flm        oreq      'V'
034300980714     c     i10flm        oreq      'M'
034400980714     c     i10flm        oreq      'I'
034500980714     C* Caricamento
034600980714    3c     i10flm        ifne      'M'
034700980714     c                   exsr      ries02
034800980824    3c                   endif
034900980804     c*                  else
035000980714     c* leggo il subfile per proteggere eventualmente campi con key diversa
035100980714     c* e per aggiornare la prima riga se key =
035200981216     c                   if        i10flm <> 'I' and *in60 = *on
035300980824     c                   exsr      aggs02
035400981216     c   06              seton                                        63
035500981216     c  n06              setoff                                       63
035600980714     c                   move      i10key        keysav
035700980824     c                   endif
035800980824     c*                  end
035900980714     c* Emissione
036000980714     c     emc02         tag
036100980714     c                   z-add     v2cpos        v2cnrr
036200051028     c                   if        v2cnrr = 0
036300051028     c                   z-add     1             v2cnrr
036400051028     c                   endif
036500051028
036600980714     c                   write     fi10t02
036700980714     c*
036800980720     c                   write     fi10z02
036900981216     c  n60              write     fi10d02
037000980714     c                   exfmt     fi10c02
037100980714     c* f12=Ritorno
037200980714    3c     *inkl         ifeq      *on
037300980714     c                   movel     'S'           o10f12
037400980714     c                   goto      fine2
037500980714    3c                   end
037600980826     c* f03=Fine
037700980826    3c     *inkc         ifeq      *on
037800980826     c                   movel     'S'           o10f03
037900980826     c                   goto      fine2
038000980826    3c                   end
038100980714     c* f14=Cancella riga
038200980714    3c     *inkn         ifeq      *on
038300980714     c                   exsr      deler2
038400980714     c                   goto      emc02
038500980714    3c                   end
038600980714     c* rollup
038700980714    3C     *IN25         IFEQ      *ON
038800980714     C                   Z-ADD     PRIMA         v2cpos
038900980714     C                   EXSR      ROLUP2
039000980714     c                   goto      emc02
039100980714    3C                   END
039200980714    3c     i10flm        ifeq      'M'
039300980714     c                   exsr      ctrrig2
039400980714    3c                   end
039500980714     c
039600981216     c* se non è manutenzione/immissione e non è stato dato il F6 di conferma   OPPURE
039700981216     c* siamo in interrogazione enon è stato dato F12 di ritorno
039800981216     c*   RIMANGO NELLA STESSA VIDEATA
039900981216     c                   if        (not *in01 and not *inkf) or
040000981216     c                             (*in02 and not *inkl)
040100981216     c                   goto      emc02
040200981216    2c                   endif
040300980714    2c                   endif
040400980714     C*****************
040500980714     C* CONFERMA DATI
040600980714     C*****************
040700980714    2c     i10flm        ifeq      'V'
040800980714     c                   z-add     1             wprimo
040900980714     c                   exsr      ctrrig2
041000980714    2c                   end
041100980714     c*
041200980714    2c     i10flm        ifeq      *blanks
041300980714     c     i10flm        oreq      'C'
041400980714     c     i10flm        oreq      'V'
041500980714     c                   exsr      confe2
041600980714    2c                   endif
041700980714     c*
041800980714     c     fine2         tag
041900980714     c*
042000980714     c                   movel     dsdn10        kpjbu
042100980714    1c                   endif
042200980714     c                   endsr
042300980710     C****************   GESTIONE SUBILE DESCRIZIONE  *****************
042400980710     C     SUB_DESC      BEGSR
042500980714      *
042600980710    1c     i10tla        ifne      'C'
042700980710     c     i10flm        ifeq      *blanks
042800980710     c     i10flm        oreq      'V'
042900980710     c     i10flm        oreq      'M'
043000980710     c     i10flm        oreq      'I'
043100980710     C* Caricamento
043200980710     c     i10flm        ifne      'M'
043300980710     c                   exsr      ries01
043400980710     c                   else
043500980710     c* leggo il subfile per proteggere eventualmente campi con key diversa
043600980710     c* e per aggiornare la prima riga se key =
043700980710     c                   exsr      aggs01
043800980710     c   06              seton                                        23
043900980710     c  n06              setoff                                       23
044000980710     c                   move      i10key        keysav
044100980710     c                   end
044200980710     c* Emissione
044300980710     c     emc01         tag
044400980710     c                   z-add     v1cpos        v1cnrr
044500051028     c                   if        v1cnrr = 0
044600051028     c                   z-add     1             v1cnrr
044700051028     c                   endif
044800981027     c*
044900981027     c                   write     fi10t01
045000980710     c*
045100980710     c                   write     fi10z01
045200980710     c  n20              write     fi10d01
045300980710     c                   exfmt     fi10c01
045400980710     c* f12=Ritorno
045500980710     c     *inkl         ifeq      *on
045600980710     c                   movel     'S'           o10f12
045700980714     c                   goto      fine1
045800980710     c                   end
045900980710     c* f07=Inserimento  riga
046000980710     c     *inkg         ifeq      *on
046100980714     c                   exsr      inser1
046200980710     c                   goto      emc01
046300980710     c                   end
046400980710     c* f14=Cancella riga
046500980710     c     *inkn         ifeq      *on
046600980714     c                   exsr      deler1
046700980710     c                   goto      emc01
046800980710     c                   end
046900980710     c* rollup
047000980714     C     *IN25         IFEQ      *ON
047100980714     C                   Z-ADD     PRIMA         v1cpos
047200980714     C                   EXSR      ROLUP1
047300980714     c                   goto      emc01
047400980714     C                   END
047500980710     c     i10flm        ifeq      'M'
047600980714     c                   exsr      ctrrig1
047700980710     c                   end
047800981216     c* se non è manutenzione/immissione e non è stato dato il F6 di conferma   OPPURE
047900981216     c* siamo in interrogazione enon è stato dato F12 di ritorno
048000981216     c*   RIMANGO NELLA STESSA VIDEATA
048100981216     c                   if        (not *in01 and not *inkf) or
048200981216     c                             (*in02 and not *inkl)
048300981216     c                   goto      emc01
048400981216     c                   endif
048500980710     c                   endif
048600980710     C*****************
048700980710     C* CONFERMA DATI
048800980710     C*****************
048900980710     c     i10flm        ifeq      'V'
049000980710     c                   z-add     1             wprimo
049100980714     c                   exsr      ctrrig1
049200980710     c                   end
049300980710     c*
049400980710     c     i10flm        ifeq      *blanks
049500980710     c     i10flm        oreq      'C'
049600980710     c     i10flm        oreq      'V'
049700980714     c                   exsr      confe1
049800980710     c                   endif
049900980710     c*
050000980714     c     fine1         tag
050100980710     c*
050200980710     c     *in20         ifeq      *on
050300980710     c     i10flm        ifne      'M'
050400980710     c     1             chain     fi10s01                            30
050500980710     c                   else
050600980710     c     wprimo        chain     fi10s01                            30
050700980710     c                   end
050800980710     c  n30              movel     v1cnot        o10not
050900980710     c                   end
051000980710     c                   movel     dsdn10        kpjbu
051100980710    1c                   endif
051200980710     c                   endsr
051300981215     C****************   GESTIONE SUBILE NOTE TESTO *************************
051400981215     C     SUB_TESTO     BEGSR
051500981215      * se non si tratta di conferma
051600981215    1c                   if        i10tla <> 'C'
051700981215      * e non si tratta di manutenzione di note già esistenti
051800981215    2c                   if        i10flm   <> ' '  or
051900981215     c                             i10flm    = 'V'  or
052000981215     c                             i10flm    = 'M'  or
052100981215     c                             i10flm    = 'I'
052200981215     C* Caricamento
052300981215    3c                   if        i10flm <> 'M'
052400981215     c                   exsr      ries03
052500981215    3c                   endif
052600981215     c* Emissione
052700981215     c     emc03         tag
052800981215     c                   write     fi10t03
052900981215     c*
053000981215     c                   write     fi10z03
053100981215     c                   exfmt     fi10d03
053200981215     c* f12=Ritorno
053300981215    3c     *inkl         ifeq      *on
053400981215     c                   movel     'S'           o10f12
053500981215     c                   goto      fine3
053600981215    3c                   end
053700981215     c* f03=Fine
053800981215    3c     *inkc         ifeq      *on
053900981215     c                   movel     'S'           o10f03
054000981215     c                   goto      fine3
054100981215    3c                   end
054200981215     c
054300981216     c* se non è manutenzione/immissione e non è stato dato il F6 di conferma   OPPURE
054400981216     c* siamo in interrogazione enon è stato dato F12 di ritorno
054500981216     c*   RIMANGO NELLA STESSA VIDEATA
054600981216     c                   if        (not *in01 and not *inkf) or
054700981216     c                             (*in02 and not *inkl)
054800981216     c                   goto      emc03
054900981216    2c                   endif
055000981215    2c                   endif
055100981215     C*****************
055200981215     C* CONFERMA DATI
055300981215     C*****************
055400981215     c*
055500981215    2c     i10flm        ifeq      *blanks
055600981215     c     i10flm        oreq      'C'
055700981215     c     i10flm        oreq      'V'
055800981215     c                   exsr      confe3
055900981215    2c                   endif
056000981215     c*
056100981215     c     fine3         tag
056200981215     c*
056300981215     c                   movel     dsdn10        kpjbu
056400981215    1c                   endif
056500981215     c                   endsr
056600980713     C**********  RIEMPIMENTO SUBFILE DESCRIZIONE  *************************
056700980713     C     RIES01        BEGSR
056800971023     c* pulizia
056900971030     C                   SETOFF                                       212306
057000971023     C                   WRITE     FI10C01
057100971023     C                   SETON                                        2021
057200971023     C                   MOVE      *ZEROS        NRR1              5 0
057300971024     c* se passata la key carico dati da file descrizioni
057400971028    1c     i10nkt        ifne      *blanks
057500971024     c                   clear                   wwrt              1
057600980715     c                   movel     i10tpd        v1htpd
057700980715     c                   movel     i10std        v1hstd
057800980715     c                   movel     i10nks        v1hnks
057900980715     c                   movel     i10trc        v1htrc
058000980617     c     kdcsca        setll     fndcs01l
058100980617     c     kdcsca        reade     fndcs01l                               31
058200971024    2c     *in31         doweq     *off
058300980617    3c                   if        dcsatb = *blanks and dcsnks = i10nks
058400980617     c                             and dcstrc = i10trc
058500980608     c* scrivo un record del subfile
058600980608     c                   movel(P)  dcsnot        v1cnot
058700980609     c*
058800971024     c                   add       1             nrr1
058900980610     c* imposto la prima riga con i10not se non vengono gestite le note
059000980713    4c     i10flm        ifeq      'V'
059100980526     c     nrr1          andeq     1
059200981013     c     i10not        andne     *blanks
059300980526     c                   movel(P)  i10not        v1cnot
059400980611    4c                   end
059500971024     c                   write     fi10s01
059600971024     c                   clear                   wwrt
059700980619    3c                   endif
059800980617     c     kdcsca        reade     fndcs01l                               31
059900971024    2C                   enddo
060000971024    1c                   end
060100971029     c                   z-add     1             v1cpos
060200971024     c* carico righe vuote
060300980713    1c     i10flm        ifne      'I'
060400981216     c                   z-add     nrr1          max1              5 0
060500980714     c                   exsr      rolup1
060600980522     c                   else
060700980522     c                   seton                                        0623
060800980713    1c                   end
060900980713    1c     nrr1          ifeq      *zeros
061000980522     c                   setoff                                       20
061100980713    1c                   end
061200971023     c*
061300971023     c                   endsr
061400980714     C**********  RIEMPIMENTO SUBFILE NOTE *********************************
061500980714     C     RIES02        BEGSR
061600980714     c* pulizia
061700981216     C                   SETOFF                                       616306
061800980714     C                   WRITE     FI10C02
061900981216     C                   SETON                                        6061
062000980714     C                   MOVE      *ZEROS        NRR2              5 0
062100980714     c* se passata la key carico dati da file descrizioni
062200980714    1c     i10nkt        ifne      *blanks
062300980715     c                   movel     i10tpd        v2htpd
062400980715     c                   movel     i10std        v2hstd
062500980715     c                   movel     i10trc        v2htrc
062600980714     c                   clear                   wwrt              1
062700980714     c     kdcsca        setll     fndcs01l
062800980714     c     kdcsca        reade     fndcs01l                               31
062900980714    2c     *in31         doweq     *off
063000980716    3c                   if        dcsatb = *blanks
063100980714     c                             and dcstrc = i10trc
063200980803     c*                            and dcsnks = i10nks
063300980803     c                   movel     dcsnks        v2hnks
063400980714     c* scrivo un record del subfile
063500980714     c                   movel(P)  dcsnot        v2cnot
063600980714      * data
063700980714     C                   Z-ADD     dcsdim        G02inv
063800980714     C                   movel     '3'           G02err
063900980714     C                   CALL      'XSRDA8'
064000980714     C                   PARM                    WLBDAT
064100980714     c                   z-add     g02dat        v2odim
064200980714      * ora
064300980714     C                   Z-ADD     dcshim        v2ohim
064400980714      * fase
064500980714     c                   movel     dcsnks        v2ocfa
064600980714      * utente che ha scritto la nota
064700980714     c                   movel     dcspru        v2opru
064800980714      * filiale che ha scritto la nota
064900980714     c                   movel     dcspos        v2opos
065000980714      * flag stato riga nota vista o non vista
065100980714    4c                   if        dcsstn = ' '
065200980720     c                   eval      v2ostn='NUOVA '
065300980714     c                   else
065400980720     c                   eval      v2ostn='APERTA'
065500980714    4c                   endif
065600980714      * PROTEZIONE NOTE
065700980804     c     i10tpd        ifne      v2htpd
065800980804     c     i10std        orne      v2hstd
065900980804     c     i10nks        orne      v2hnks
066000980804     c     i10trc        orne      v2htrc
066100980824     c     i10flm        oreq      'M'
066200980804     c                   seton                                        08
066300980804     c                   movel     '1'           v2hin8
066400980804     c                   else
066500980804     c                   setoff                                       08
066600980804     c                   movel     ' '           v2hin8
066700980804    4c                   endif
066800980714      * 02 on protegge il campo note / descrizione
066900051024     c******             if        *in02 = *on     or
067000051024    4c******                       dcsft1 <> ' '   or
067100980714      * se già trasmesse
067200051024     c******                       dcsnks <> i10nks
067300980826      * fase differente
067400051024      *****             PROTEGGO SEMPRE
067500980804     c                   seton                                        08
067600980804     c                   move      '1'           v2hin8
067700051024    4c****               endif
067800980714      * ALTA INTENSITA'
067900980714      * Record nuovo mai trasmesso
068000051024    4c*****              if        dcsft1 = ' ' and
068100051024     c*****                        dcsstn = ' '
068200051024    4c                   if        dcsstn = ' '
068300980714      * NUOVA
068400980714     c                   seton                                        10
068500980714    4c                   endif
068600980714      * Record nuovo ricevuto
068700051024    4c*****              if        dcsft1 = 'R' and
068800051024     c*****
068900980714      * NUOVA
069000051024     c******             seton                                        10
069100980714     c*
069200051024    4c******             endif
069300980714     c                   add       1             nrr2
069400980714     c                   write     fi10s02
069500980714     c                   clear                   wwrt
069600980804     c                   clear                   v2hin8
069700980826     c                   setoff                                       0810
069800980714     c* aggiorno il flag di visto
069900980714     c                   movel     'V'           dcsstn
070000980714     c                   update    fndcs000
070100980714    3c                   endif
070200980714     c     kdcsca        reade     fndcs01l                               31
070300980714    2C                   enddo
070400980714    1c                   end
070500980714     c                   z-add     1             v2cpos
070600980714     c* carico righe vuote
070700980714     c     i10flm        ifne      'I'
070800981216     c                   z-add     nrr2          max2              5 0
070900980714     c                   exsr      rolup2
071000980714     c                   else
071100981216     c                   seton                                        0663
071200980714     c                   end
071300980714     c     nrr2          ifeq      *zeros
071400981216     c                   setoff                                       60
071500980714     c                   end
071600980714     c*
071700980714     c                   endsr
071800981215     C**********  RIEMPIMENTO SUBFILE NOTE TESTO  *************************
071900981215     C     RIES03        BEGSR
072000981215     c* pulizia
072100981215     C                   clear                   v3cpri
072200981215     C                   clear                   v3csec
072300990610     C                   clear                   v3cter
072400990610     C                   clear                   v3cqua
072500990610     C                   clear                   v3cqui
072600981215     c* pulizia numeratori e schiera di caricamento note
072700981215     c                   clear                   X
072800981215     c                   clear                   TNO
072900981215     c* se passata la key carico dati da file descrizioni
073000981215    1c     i10nkt        ifne      *blanks
073100981215     c                   movel     i10tpd        v1htpd
073200981215     c                   movel     i10std        v1hstd
073300981215     c                   movel     i10nks        v1hnks
073400981215     c                   movel     i10trc        v1htrc
073500981215     c     kdcsca        setll     fndcs01l
073600981215     c     kdcsca        reade     fndcs01l                               31
073700981215    2c     *in31         doweq     *off
073800981215    3c                   if        dcsatb = *blanks and dcsnks = i10nks
073900981215     c                             and dcstrc = i10trc
074000981215     c* aggiorno la schiera delle note
074100981215     c                   add       1             x
074200981215     c*
074300990610     c                   if        x <= 15
074400981215     c                   movel(P)  dcsnot        tno(x)
074500981215    1c                   endif
074600981215     c*
074700981215    1c                   endif
074800981215     c*
074900981215     c     kdcsca        reade     fndcs01l                               31
075000981215    2C                   enddo
075100981215     c*
075200981215    1c                   endif
075300981215     c*
075400981215     c                   endsr
075500980714     C****************  gestione rollup Descrizioni  ******************
075600980714     C     ROLUP1        BEGSR
075700971024     C*
075800981216     C                   Z-ADD     MAX1          NRR1
075900981216     C     MAX1          DIV       17            WDIV              5 0
076000971024     C                   MVR                     WRES              3 0
076100971024     C                   ADD       1             WRES
076200981126     c* in caso di gestione interlocutore prevedo il caricamento di una sola riga
076300981126     c*  e inibisco i cmd di inserimento ed annullamento riga
076400980716     c     i10std        ifeq      'N'
076500980716     c     nrr1          ifeq      1
076600980716     c                   z-add     17            wres
076700980716     c                   else
076800980716     c                   z-add     16            wres
076900980716     c                   endif
077000980716     c                   endif
0771009710241    C     WRES          DO        17
077200980730     C                   CLEAR                   v1cnot
077300971024     C                   ADD       1             NRR1
077400971028     C* se modalità=solo immissione memorizzo nel sfl anche la key parziale
077500971028     c* ricevuta
077600971028     c     i10flm        ifeq      'M'
077700971030     c     wnot          ifeq      *blanks
077800980518     c     i10not        andne     *blanks
077900971030     c                   movel     i10not        v1cnot
078000971030     c                   movel     '1'           wnot              1
078100971030     c                   end
078200971028     c                   movel     i10tpd        v1htpd
078300971028     c                   movel     i10std        v1hstd
078400971029     c                   movel     i10nks        v1hnks
078500971028     c                   movel     i10trc        v1htrc
078600980610     c
078700971028     c                   end
078800980526     c* imposto la prima riga con i10not
078900980526     c     i10flm        ifeq      'V'
079000980526     c     nrr1          andeq     1
079100980617     c                   movel     i10tpd        v1htpd
079200980617     c                   movel     i10std        v1hstd
079300980617     c                   movel     i10nks        v1hnks
079400980617     c                   movel     i10trc        v1htrc
079500981013     c     i10not        ifne      *blanks
079600980526     c                   movel(P)  i10not        v1cnot
079700980526     c                   end
079800981013     c                   end
079900971024     C                   WRITE     fi10s01
0800009710241-   C                   ENDdo
080100971024     C*
080200971029     C     NRR1          SUB       16            v1cpos
080300981216     C                   Z-ADD     NRR1          MAX1
080400971024     C*
080500971024     C                   ENDSR
080600980714     C****************  gestione rollup Note **************************
080700980714     C     ROLUP2        BEGSR
080800980714     C*
080900051028     c                   clear                   FI10S02
081000051028      *
081100981216     C                   Z-ADD     MAX2          NRR2
081200981216     C     MAX2          DIV       17            WDIV              5 0
081300980714     C                   MVR                     WRES              3 0
081400980714     C                   ADD       1             WRES
081500051028      *
0816009807141    C     WRES          DO        17
081700980714     C                   ADD       1             NRR2
081800051028     C* memorizzo nel sfl anche la key parziale ricevuta
081900980714     c                   movel     i10tpd        v2htpd
082000980714     c                   movel     i10std        v2hstd
082100980714     c                   movel     i10nks        v2hnks
082200980714     c                   movel     i10trc        v2htrc
082300980714     c
082400980714     C                   WRITE     fi10s02
0825009807141-   C                   ENDdo
082600980714     C*
082700980714     C     NRR2          SUB       16            v2cpos
082800981216     C                   Z-ADD     NRR2          MAX2
082900980714     C*
083000980714     C                   ENDSR
083100971024      *********** INSER INSERISCE UNA RIGA NEL SUBFILE  *******************
083200980714     C     INSER1        BEGSR
083300971024     C                   EXSR      RIGA
083400971024     C     $RIG          IFNE      0
083500971029     C                   CLEAR                   s1ds
083600971029     c                   movel     i10tpd        v1htpd
083700971029     c                   movel     i10std        v1hstd
083800971029     c                   movel     i10nks        v1hnks
083900971029     c                   movel     i10trc        v1htrc
084000980720     C                   MOVE      s1ds          S1SAV2
084100971024     C     $RIG          CHAIN     fi10s01                            30
084200980720     C                   MOVE      s1ds          S1SAV
084300981216     C     MAX1          ADD       2             $MAX1             5 0
084400971024     C                   ADD       1             $RIG
084500981216     C     $RIG          DO        $MAX1         I                 5 0
084600971029     C                   MOVE      S1SAV2        s1ds
084700971029     c     i10tpd        ifne      v1htpd
084800971029     c     i10std        orne      v1hstd
084900971029     c     i10nks        orne      v1hnks
085000971029     c     i10trc        orne      v1htrc
085100971029     c                   seton                                        05
085200971029     c                   else
085300971029     c                   setoff                                       05
085400971029     c                   end
085500971024     C  N30              UPDATE    fi10s01
085600981216     C   30MAX1          ADD       1             NRR1
085700971024     C   30              WRITE     fi10s01
085800971024     C     I             CHAIN     fi10s01                            30
085900980720     C                   MOVE      s1ds          S1SAV3
086000971024     C                   MOVE      S1SAV         S1SAV2
086100971024     C                   MOVE      S1SAV3        S1SAV
086200971024     C                   END
086300981216     C                   ADD       1             MAX1
086400971024     C                   END
086500971029     c                   setoff                                       05
086600971024     C                   ENDSR
086700971024      **********  ANNULLA   UNA RIGA NEL SUBFILE **************************
086800980714     C     DELEr1        BEGSR
086900971024     C                   EXSR      RIGA
087000971024     C     $RIG          IFNE      0
087100971029     C                   CLEAR                   s1ds
087200971029     c                   movel     i10tpd        v1htpd
087300971029     c                   movel     i10std        v1hstd
087400971029     c                   movel     i10nks        v1hnks
087500971029     c                   movel     i10trc        v1htrc
087600980714     C                   MOVE      s1ds          S1SAV2           45
087700981216     C     MAX1          CHAIN     fi10s01                            30
087800980714     C                   MOVE      s1ds          S1SAV            45
087900981216     C                   Z-SUB     MAX1          $MAX1             5 0
088000971024     C                   Z-SUB     $RIG          $RIG              5 0
088100971024     C                   ADD       1             $RIG
088200981216     C                   ADD       1             $MAX1
088300981216     C     $MAX1         DO        $RIG          Y                 5 0
088400971024     C                   Z-SUB     Y             I                 5 0
088500971029     C                   MOVE      S1SAV2        s1ds
088600980714     c     s1sav2        ifne      *blanks
088700971029     c     i10tpd        ifne      v1htpd
088800971029     c     i10std        orne      v1hstd
088900971029     c     i10nks        orne      v1hnks
089000971029     c     i10trc        orne      v1htrc
089100971029     c                   seton                                        05
089200980715     c                   movel     *blanks       v1cnot
089300971029     c                   else
089400971029     c                   setoff                                       05
089500971029     c                   end
089600971024     C  N30              UPDATE    fi10s01
089700980715     c                   setoff                                       05
089800980714     c                   endif
089900971024     C     I             CHAIN     fi10s01                            30
090000980714     C                   MOVE      s1ds          S1SAV3           45
090100971024     C                   MOVE      S1SAV         S1SAV2
090200971024     C                   MOVE      S1SAV3        S1SAV
090300971024     C                   END
090400971024     C                   END
090500971029     c                   setoff                                       05
090600971024     C                   ENDSR
090700980714      **********  ANNULLA   UNA RIGA NEL SUBFILE **************************
090800980715     C     delEr2        BEGSR
090900980714     C                   EXSR      RIGA
091000980826      * solo se non e' campo protetto
091100980826     C     $rig          CHAIN     fi10s02                            30
091200980826     c                   if        v2hin8 = ' ' and *in30 = *off
091300980714     C     $RIG          IFNE      0
091400980714     C                   CLEAR                   s2ds
091500980714     c                   movel     i10tpd        v2htpd
091600980714     c                   movel     i10std        v2hstd
091700980714     c                   movel     i10nks        v2hnks
091800980714     c                   movel     i10trc        v2htrc
091900980720     C                   MOVE      s2ds          S2SAV2
092000981216     C     MAX2          CHAIN     fi10s02                            30
092100980720     C                   MOVE      s2ds          S2SAV
092200981216     C                   Z-SUB     MAX2          $MAX2             5 0
092300980714     C                   Z-SUB     $RIG          $RIG              5 0
092400980714     C                   ADD       1             $RIG
092500981216     C                   ADD       1             $MAX2
092600981216     C     $MAX2         DO        $RIG          Y                 5 0
092700980714     C                   Z-SUB     Y             I                 5 0
092800980715     C                   MOVE      S2SAV2        s2ds
092900980715     c     s2sav2        ifne      *blanks
093000980714     c     i10tpd        ifne      v2htpd
093100980714     c     i10std        orne      v2hstd
093200980714     c     i10nks        orne      v2hnks
093300980714     c     i10trc        orne      v2htrc
093400980825     c*                  movel     *blanks       v2cnot
093500980804     c                   seton                                        08
093600980804     c                   movel     '1'           v2hin8
093700980714     c                   else
093800980804     c                   setoff                                       08
093900980804     c                   movel     ' '           v2hin8
094000980714     c                   end
094100980715     c
094200980714     C  N30              UPDATE    fi10s02
094300980715     c                   endif
094400980804     c                   setoff                                       08
094500980804     c                   movel     ' '           v2hin8
094600980714     C     I             CHAIN     fi10s02                            30
094700980720     C                   MOVE      s2ds          S2SAV3
094800980715     C                   MOVE      S2SAV         S2SAV2
094900980715     C                   MOVE      S2SAV3        S2SAV
095000980714     C                   END
095100980714     C                   END
095200980826     c                   end
095300980804     c                   setoff                                       08
095400980714     C                   ENDSR
095500971024     c*************   detemina nrr del subfile *****************************
095600971024     C     RIGA          BEGSR
095700971024     C                   Z-ADD     PRIMA         $RIG              5 0
095800971024     C                   MOVE      NRG           R$$
095900971024     C                   ADD       RIRI          $RIG
096000971024     C                   SUB       6             $RIG
096100971024     C     $RIG          IFLT      0
096200971024     C     RIRI          ORLT      6
096300971024     C                   Z-ADD     0             $RIG
096400971024     C                   END
096500971024     C                   ENDSR
096600980714     C***  Aggiornamento sfl descrizioni in modalita' solo immissione ***********
096700971028     c     aggs01        begsr
096800971029     c                   setoff                                       06
096900971028     c                   z-add     1             nrr1
097000971029     c                   move      *zeros        v1cpos
097100971028     c     nrr1          chain     fi10s01                            30
097200971028     c     *in30         doweq     *off
097300971029     c     i10tpd        ifne      v1htpd
097400971029     c     i10std        orne      v1hstd
097500971029     c     i10nks        orne      v1hnks
097600971029     c     i10trc        orne      v1htrc
097700971028     c                   seton                                        05
097800971028     c                   else
097900971028     c                   setoff                                       05
098000980609     c*
098100971029     c     v1cpos        ifeq      *zeros
098200971029     c                   z-add     nrr1          v1cpos
098300971030     c                   z-add     nrr1          wprimo            4 0
098400981013     c     i10not        ifne      *blanks
098500980714     c                   movel     i10not        v1cnot
098600981013     c                   endif
098700971030     c* se ci sono già records nel sfl con quella chiave disabilito rollup
098800971030     c                   seton                                        06
098900971028     c                   end
099000971028     c                   end
099100971029     c                   update    fi10s01
099200971029     c                   add       1             nrr1
099300971028     c     nrr1          chain     fi10s01                            30
099400971028     c                   enddo
099500971029     c                   setoff                                       05
099600971029     c     v1cpos        ifeq      *zeros
099700971030     c                   clear                   wnot
099800981216     c     max1          add       1             wprimo
099900980714     C                   EXSR      ROLUP1
100000971028     c                   end
100100971028     C                   ENDSR
100200980714     C***  Aggiornamento sfl note in modalita' solo immissione ***********
100300980714     c     aggs02        begsr
100400980714     c                   setoff                                       06
100500980714     c                   z-add     1             nrr2
100600980714     c                   move      *zeros        v2cpos
100700980714     c     nrr2          chain     fi10s02                            30
100800980714     c     *in30         doweq     *off
100900980714     c*
101000980825      * se riga non protetta e' la prima che deve essere utlizzata
101100980825     C     V2HIN8        IFEQ      ' '
101200980825     c     v2cnot        andeq     *blanks
101300980714     c     v2cpos        ifeq      *zeros
101400980714     c                   z-add     nrr2          v2cpos
101500980714     c                   z-add     nrr2          wprimo            4 0
101600980714     c* se non valorizzata la chiave ma sono nella gestione note
101700980714     c* valorizzo i campi della data ora e fase nella prima riga
101800980714     c* con data e ora del momento
101900980803     c                   if        v2cnot = *blanks
102000980716     C                   Z-ADD     wdtgio        v2odim
102100980714      * ora
102200980714     C                   Z-ADD     wora          v2ohim
102300980714      * fase
102400980714     c                   movel     i10nks        v2ocfa
102500980714      * profilo utente
102600980714     c                   movel     knmus         v2opru
102700980714      * filiale
102800980714     c                   movel     simfel        v2opos
102900980714      * stato nota
103000980714     c                   movel     *blanks       v2ostn
103100980803     c                   endif
103200980714     c* se ci sono già records nel sfl con quella chiave disabilito rollup
103300980714     c                   seton                                        06
103400980714     c                   end
103500980714     c                   end
103600051028     ***C     V2HIN8        IFne      ' '
103700051028     ***c                   seton                                        08
103800051028     ***c                   endif
103900051028     c     V2Hin8        comp      *blanks                            0808
104000980714     c                   update    fi10s02
104100980804     c                   setoff                                       08
104200051028     ***c                   movel     ' '           v2hin8
104300980714     c                   add       1             nrr2
104400980714     c     nrr2          chain     fi10s02                            30
104500980714     c                   enddo
104600980824     c                   setoff                                       08
104700980714     c     v2cpos        ifeq      *zeros
104800980714     c                   clear                   wnot
104900981216     c     max2          add       1             wprimo
105000980714     C                   EXSR      ROLUP2
105100980714     c                   end
105200980714     C                   ENDSR
105300971024     C****************  conferma dati su data base  *******************
105400980714     c     confe1        begsr
105500971029     c* Se esistono dei records con la chiave passata li cancello
105600971029     c* (se modalità "C" non possono esserci records da cancellare perchè
105700971029     c* in questo caso sono in inserimento di nuovi records)
105800971029     c     i10flm        ifne      'C'
105900980714     c     kdcsca        setll     fndcs01l
106000980714     c     kdcsca        reade     fndcs01l                               30
106100980714     c     *in30         doweq     *off
106200980714     c                   if        i10nks = dcsnks and
106300980714     c                             i10trc = dcstrc
106400980714     c                   delete    fndcs000
106500980714     c                   endif
106600980714     c     kdcsca        reade     fndcs01l                               30
106700971024     c                   enddo
106800971029     c                   endif
106900971024     c* scrivo records a video
107000971024     C* Cerco l'ultimo record valido.
107100981216     C     MAX1          DOWGT     *ZERO
107200981216     C     MAX1          CHAIN     fi10s01                            30
107300971024     C     v1cnot        COMP      *BLANK                             3030
107400981216     C  N30              SUB       1             MAX1
107500971024     C  N30              END
107600971024     C*
107700981216     c     max1          ifgt      *zeros
107800971024     c*
107900971024     c                   clear                   fndcs000
108000971024     c                   movel     i10tpd        dcstpd
108100971024     c                   movel     i10nkt        dcsnkt
108200971024     c                   movel     i10std        dcsstd
108300971024     C                   movel     i10nks        dcsnks
108400971024     c                   movel     i10trc        dcstrc
108500971024     c                   z-add     0             dcspno
108600971024     c
108700981216     C                   DO        MAX1          NRR1
108800971024     c* per ogni record di subfile scrivo due records di fndcs00f
108900971024     C     NRR1          CHAIN     fi10s01                            30
109000971029     c* se modalità "C", a cambio chiave azzero il progressivo
109100971029     c     i10flm        ifeq      'C'
109200971029     c     v1hkey        andne     v1hsav
109300971029     c                   move      *zeros        dcspno
109400971029     c                   move      v1hkey        v1hsav
109500971029     c                   end
109600971029     c* se record vuoto lo salto
109700971029     c     i10flm        ifne      'C'
109800971029     c     v1cnot        orne      *blanks
109900971024     c                   add       1             dcspno
110000971024     c                   movel     v1cnot        dcsnot
110100971029     c* se modalità "C" il tipo/key sottotipo/sottotipo e tipo record sono
110200971029     c* memorizzati nel subfile
110300971028     c     i10flm        ifeq      'C'
110400971028     c                   movel     v1htpd        dcstpd
110500971029     c                   movel     v1hstd        dcsstd
110600971029     C                   movel     v1hnks        dcsnks
110700971029     c                   movel     v1htrc        dcstrc
110800971028     c                   end
110900980730     c                   move      knmus         dcspru
111000980730     c                   movel     simfel        dcspos
111100971024     c                   write     fndcs000
111200971029     c                   endif
111300971024     c                   enddo
111400971024     c
111500971024     c                   end
111600971024     c                   endsr
111700980714     C****************  conferma dati su data base  *******************
111800980714     c     confe2        begsr
111900980720     c* Se esistono dei records ancora da vedere con la chiave passata li cancello
112000980714     c* (se modalità "C" non possono esserci records da cancellare perchè
112100980714     c* in questo caso sono in inserimento di nuovi records)
112200051024     c**** i10flm        ifne      'C'
112300051024     c**** kdcsca        setll     fndcs01l
112400051024     c**** kdcsca        reade     fndcs01l                               30
112500051024     c**** *in30         doweq     *off
112600980720     c* se la nota e' manutenzionabile la cancello
112700051024     c****               if        dcsft1 = *blank and
112800051024     c****                         dcsdt1 = *zeros and
112900051024     c****                         dcsnks = i10nks
113000051024     c****               delete    fndcs000
113100051024     c****               endif
113200051024     c**** kdcsca        reade     fndcs01l                               30
113300051024     c****               enddo
113400051024     c****               endif
113500980714     c* scrivo records a video
113600980714     C* Cerco l'ultimo record valido.
113700981216     C     MAX2          DOWGT     *ZERO
113800981216     C     MAX2          CHAIN     fi10s02                            30
113900980714     C     v2cnot        COMP      *BLANK                             3030
114000981216     C  N30              SUB       1             MAX2
114100980714     C  N30              END
114200980714     C*
114300981216     c     max2          ifgt      *zeros
114400980714     c*
114500980714     c                   clear                   fndcs000
114600980714     c                   movel     i10tpd        dcstpd
114700980714     c                   movel     i10nkt        dcsnkt
114800980714     c                   movel     i10std        dcsstd
114900980714     C                   movel     i10nks        dcsnks
115000980714     c                   movel     i10trc        dcstrc
115100980714      *
115200980714     c                   z-add     0             dcspno
115300980714     c
115400981216     C                   DO        MAX2          NRR2
115500980714     c* per ogni record di subfile scrivo due records di fndcs00f
115600980714     C     NRR2          CHAIN     fi10s02                            30
115700980804     c* se record non protetto elaboro
115800051025     c***                if        v2hin8 = ' '
115900980714     c* se modalità "C", a cambio chiave azzero il progressivo
116000051026     c                   if        (i10flm = 'C'  or  i10flm = 'V') and
116100051026     c                             v2hkey <> v2hsav
116200980714     c                   move      *zeros        dcspno
116300980804     c                   move      v2hkey        v2hsav
116400051025     c                   end
116500051025      **********************************************************************************
116600980714     c* se record vuoto lo salto
116700980714     c     v2cnot        ifne      *blanks
116800980714     c                   add       1             dcspno
116900980714     c                   movel     v2cnot        dcsnot
117000980714     c* se modalità "C" il tipo/key sottotipo/sottotipo e tipo record sono
117100980714     c* memorizzati nel subfile
117200980714     c     i10flm        ifeq      'C'
117300980714     c                   movel     v2htpd        dcstpd
117400980714     c                   movel     v2hstd        dcsstd
117500980714     C                   movel     v2hnks        dcsnks
117600980714     c                   movel     v2htrc        dcstrc
117700980714     c                   end
117800980714      * in caso di note valorizzo il campo data con la data passata in chiave
117900980714      * oppure con la data e l'ora del momnento oppure se record gia' esistente
118000980714      * con quella del subfile
118100980714     c                   if        v2odim  <> *zeros
118200980714     C                   Z-ADD     v2odim        G02dat
118300980714     C                   movel     ' '           G02err
118400980714     C                   CALL      'XSRDA8'
118500980714     C                   PARM                    WLBDAT
118600980714     c                   z-add     g02inv        dcsdim
118700980714     c                   z-add     v2ohim        dcshim
118800980825     c                   if        v2ostn  = *blanks
118900980825     c                   move      ' '           dcsstn
119000980825     c                   else
119100980825     c                   move      'V'           dcsstn
119200980825     c                   endif
119300980714     C                   movel     v2ocfa        dcsnks
119400980714     c                   move      v2opru        dcspru
119500980714     c                   movel     v2opos        dcspos
119600980714     c                   else
119700980714     c                   if        i10dim  <> *zeros
119800980714     C                   z-add     i10dim        dcsdim
119900980714     c                   z-add     i10him        dcshim
120000980714     c                   else
120100980714     c                   z-add     wora          dcshim
120200980714     c                   z-add     dateu         dcsdim
120300980714     c                   endif
120400980714      * valorizzo profilo + P.O.
120500980714     c                   move      knmus         dcspru
120600980714     c                   movel     simfel        dcspos
120700980714     c                   endif
120800051024     c                   if        dcspos = 0
120900051024     c                   eval      dcspos = 046
121000051024     c                   endif
121100051025     c* se record non protetto elaboro
121200051025     c                   if        v2hin8 = ' '
121300980714     c                   write     fndcs000
121400980714     c                   endif
121500980804     c                   endif
121600980714     c                   enddo
121700980714     c
121800980714     c                   end
121900980714     c                   endsr
122000981215     C****************  conferma dati su data base  *******************
122100981215     c     confe3        begsr
122200981216     c* cancello i records già presenti nell'archivio con la stessa chiave CA
122300981215     c     kdcsca        setll     fndcs01l
122400981215     c     kdcsca        reade     fndcs01l                               30
122500981215     c     *in30         doweq     *off
122600981216     c* cancello le note relative al documento
122700981215     c                   if        dcsnks = i10nks
122800981215     c                   delete    fndcs000
122900981215     c                   endif
123000981215     c     kdcsca        reade     fndcs01l                               30
123100981215     c                   enddo
123200981215     c*
123300981215     c                   z-add     0             x
123400981215     c*
123500981215     c
123600981215     c                   clear                   fndcs000
123700981215     c                   movel     i10tpd        dcstpd
123800981215     c                   movel     i10nkt        dcsnkt
123900981215     c                   movel     i10std        dcsstd
124000981215     C                   movel     i10nks        dcsnks
124100981215     c                   movel     i10trc        dcstrc
124200981215      *
124300981215     c                   z-add     0             dcspno
124400981215     c
124500990610     c                   dow       x <  15
124600981215     c* per ogni riga scrivo 3 record di  fndcs00f
124700990108     c                   add       1             x
124800990610     c* controllo se riga vuota la salto
124900990610     c*
125000990610     c                   eval      t = ((x / 3) + 0.99)
125100990610     c*
125200990610     c                   if        righe(t) <> *blanks
125300981215     c                   add       1             dcspno
125400981216     c                   movel     tno(x)        dcsnot
125500981215      *
125600981215      * valorizzo profilo + P.O.
125700981215     c                   move      knmus         dcspru
125800981215     c                   movel     simfel        dcspos
125900981215     c                   write     fndcs000
126000981215     c                   endif
126100981215     c                   enddo
126200981216     c*
126300981215     c                   endsr
126400980526     C********  controlli in modalità di sola immissione o variazione ******
126500980714     c     ctrrig1       begsr
126600980422     c     wprimo        chain     fi10s01                            30
126700980608    1c     v1cnot        ifeq      *blanks
126800980422     c     wprimo        add       1             wnrr
126900980422     c     wnrr          chain     fi10s01                            30
127000980608    2c     *in30         doweq     *off
127100980608    3c     i10flm        ifeq      'M'
127200980526     c     v1htpd        andeq     i10tpd
127300980422     c     v1hstd        andeq     i10std
127400980422     c     v1hnks        andeq     i10nks
127500980422     c     v1htrc        andeq     i10trc
127600980526     c     i10flm        oreq      'V'
127700980608    4c     v1cnot        ifne      *blanks
127800980422     c                   seton                                        30
127900980608     c                   movel     v1cnot        w035a
128000980422     c                   clear                   v1cnot
128100980422     c                   update    fi10s01
128200980422     c     wprimo        chain     fi10s01                            31
128300980608    5c     w035a         ifne      *blanks
128400980608     c                   movel     w035a         v1cnot
128500980608    5c                   endif
128600980422     c                   update    fi10s01
128700980608    4c                   endif
128800980422     c
128900980422     c                   add       1             wnrr
129000980422     c  n30wnrr          chain     fi10s01                            30
129100980608   x3c                   else
129200980526     c                   seton                                        30
129300980608    3c                   end
129400980608    2c                   enddo
129500980422     c
129600980608    1c                   endif
129700980422     c
129800980422     c
129900980422     c                   endsr
130000980714     C********  controlli in modalità di sola immissione o variazione ******
130100980714     c     ctrrig2       begsr
130200980714     c     wprimo        chain     fi10s02                            30
130300980714    1c     v2cnot        ifeq      *blanks
130400980714     c     wprimo        add       1             wnrr
130500980714     c     wnrr          chain     fi10s02                            30
130600980714    2c     *in30         doweq     *off
130700980714    3c     i10flm        ifeq      'M'
130800980714     c     v2htpd        andeq     i10tpd
130900980714     c     v2hstd        andeq     i10std
131000980714     c     v2hnks        andeq     i10nks
131100980714     c     v2htrc        andeq     i10trc
131200980714     c     i10flm        oreq      'V'
131300980714    4c     v2cnot        ifne      *blanks
131400980714     c                   seton                                        30
131500980714     c                   movel     v2cnot        w035a
131600980714     c                   clear                   v2cnot
131700980714     c                   update    fi10s02
131800980714     c     wprimo        chain     fi10s02                            31
131900980714    5c     w035a         ifne      *blanks
132000980714     c                   movel     w035a         v2cnot
132100980714    5c                   endif
132200980714     c                   update    fi10s02
132300980714    4c                   endif
132400980714     c
132500980714     c                   add       1             wnrr
132600980714     c  n30wnrr          chain     fi10s02                            30
132700980714   x3c                   else
132800980714     c                   seton                                        30
132900980714    3c                   end
133000980714    2c                   enddo
133100980714     c
133200980714    1c                   endif
133300980714     c
133400980714     c
133500980714     c                   endsr
133600981026     C*****************************************************************
133700981026     c* SUB_WINT       Richiesta codice testo lettera
133800981026     C*****************************************************************
133900981026     C     SUB_WINT      BEGSR
134000981026      *
134100981027     C                   clear                   v1tcom
134200981026     c                   clear                   w1ctld
134300981026     c                   clear                   w1dtld
134400981027     c* Emetto Testata Note aggiuntive Testo
134500981027     c                   write     fi10t03
134600981027     c* Gestione finestra richiesta codice testo
134700981026    1C                   do        *hival
134800981026     c*
134900981026     C                   exfmt     fi10w01
135000981027     c*
135100981026     C                   setoff                                       2829
135200981026     C                   clear                   w1cmsg
135300981027     c* KL = Videata Precedente
135400981026    2C                   if        not *inkl
135500981027     c* RICERCA codice Testo
135600981026     C     '?'           SCAN      w1ctld                                 31
135700981027     c*
135800981026    3C                   IF        *in31
135900981026     c                   clear                   dsbs02
136000981026     c                   movel     'R'           t02mod
136100981030     c                   movel     knsif         t02sif
136200981026     C                   MOVEL     'TLD'         t02cod
136300981026     C                   CALL      'TIBS02R'                            99
136400981026     c                   parm                    kpjba
136500981026     c                   parm                    dsbs02
136600981027     c*
136700981026    4c                   if        t02err = *blanks
136800981026     C                   MOVEL     t02ke1        W1Ctld
136900981027     c                   clear                   w1dtld
137000981027     c* recupero decodifica codice
137100981026    5C                   IF        w1ctld <> *BLANKS
137200981026     c                   movel     t02uni        dtld
137300981026     C                   MOVEL     §tlddesc      W1Dtld
137400981026     c* controllo se si possono aggiungere note al testo lettera selezionato
137500990610    6C                   if        §tldnote  = 'N'
137600981026     C                   MOVEL     ERR(2)        W1CMSG
137700981026     C                   SETON                                        4028
137800981026     C                   iter
137900981026    6C                   END
138000990610     c* controllo se si possono aggiungere + di 2 righe note al testo lettera selezionato
138100990610    6C                   if        §tldnote  = 'P'
138200990610     C                   SETON                                        11
138300990610    6C                   END
138400981030     C                   MOVEL     §tlddesc      v1tcom
138500981030     c                   movel(p)  w1ctld        i10nks
138600981026    5C                   END
138700981027   x4c                   else
138800981027     c* se c'è errore emetto il msg di errore
138900981027     C                   MOVEL     t02msg        W1CMSG
139000981027     C                   SETON                                        4028
139100981027     C                   iter
139200981026    4c                   end
139300981026     C                   SETON                                        29
139400981026     c                   leave
139500981026    3C                   END
139600981026     C*   Controlli di validità'
139700981026    3C                   IF        w1ctld = *BLANKS
139800981026     C                   MOVEL     ERR(1)        W1CMSG
139900981026     C                   SETON                                        4028
140000981026     C                   iter
140100981026    3C                   END
140200981026     C* Chaino tabella per reperimento altri dati
140300981026     c                   clear                   dsbs02
140400981026     c                   clear                   dtld
140500981026     C                   MOVEL     'C'           t02mod
140600981026     C                   MOVEL     knsif         t02sif
140700981026     C                   MOVEL     'TLD'         t02cod
140800981026     C                   MOVEL(P)  W1ctld        t02ke1
140900981026      *
141000981026     C                   CALL      'TIBS02R'
141100981026     C                   PARM                    KPJBA
141200981026     C                   PARM                    dsbs02
141300981026      *
141400981026    3C                   if        t02err = *BLANKS
141500981026     C                   MOVEL     T02UNI        DTLD
141600981026     C                   MOVEL     §tlddesc      W1Dtld
141700981026     c* controllo se si possono aggiungere note al testo lettera selezionato
141800990610    4C                   if        §tldnote = 'N'
141900981026     C                   MOVEL     ERR(2)        W1CMSG
142000981026     C                   SETON                                        4028
142100981026     C                   iter
142200981026    4C                   END
142300990610     c* controllo se si possono aggiungere + di 2 righe note al testo lettera selezionato
142400990610    6c                   if        §tldnote = 'P'
142500990610     C                   SETON                                        11
142600990610    6C                   END
142700981027      *
142800981027     C                   MOVEL     §tlddesc      v1tcom
142900981027     c                   movel(p)  w1ctld        i10nks
143000981027     c                   leave
143100981027      *
143200981027     c                   else
143300981027     C                   MOVEL     ERR(1)        W1CMSG
143400981027     C                   SETON                                        4028
143500981027     C                   iter
143600981026      *
143700981026    3C                   endif
143800981026     c                   else
143900981026     c                   leave
144000981026    2C                   endif
144100981026      *
144200981026    1C                   enddo
144300981027      *
144400981026     C                   ENDSR
144500971024     C****************  ROUTINE INIZIALE  *****************************
144600951009     C     *INZSR        BEGSR
144700951013     C*
144800051025     C                   TIME                    W0140            14 0
144900051025     C* UDATE IN GGMMAAAA
145000051025     C                   MOVE      W0140         WDTGIO            8 0
145100051025     C                   MOVEL     W0140         WORA              4 0
145200051025     C*
145300051025     C* UDATE IN AAAAMMGG
145400051025     C                   Z-ADD     WDTGIO        G02DAT
145500051025     C                   MOVEL     *BLANK        G02ERR
145600051025     C                   CALL      'XSRDA8'
145700051025     C                   PARM                    WLBDAT
145800051025     C                   MOVEL     G02INV        DATEU             8 0
145900971024     C*
146000971024     C                   Z-ADD     1             CODUT
146100971024     C                   CALL      'X§PARUT'
146200971024     C                   PARM                    UT§DSE
146300971024     C                   MOVEL     RAGUT         RSUT
146400980619     C                   MOVEL     Rec80         cncr80
146500971028     c
146600971028     c     *like         define    i10key        keysav
146700971029     c     *like         define    v1hkey        v1hsav
146800980714     c     *like         define    v2hkey        v2hsav
146900980422     c     *like         define    nrr1          wnrr
147000971024     c*
147100971024     c     kdcs          klist
147200971024     c                   kfld                    i10tpd
147300971024     c                   kfld                    i10nkt
147400971024     c                   kfld                    i10std
147500980608     c                   kfld                    i10dim
147600980608     c                   kfld                    i10him
147700971024     c                   kfld                    i10nks
147800971024     c                   kfld                    i10trc
147900980716     c                   kfld                    prog
148000980716     c                   z-add     1             prog
148100971028     c
148200980617     c*
148300980617     c     kdcsca        klist
148400980617     c                   kfld                    i10tpd
148500980617     c                   kfld                    i10nkt
148600980617     c                   kfld                    i10std
148700971016     C*
148800951009     C                   ENDSR
148900980610**
149000980610Descrizione aggiuntiva anomalia
149100980610Motivo del danno
149200980610Descrizione dei Pezzi danneggiati
149300980610Descrizione dei KG / Pezzi presenti
149400980616Descrizione evento
149500981126Interlocutore
149600980716**
149700980716C.A......:
149800980716EVENTO...:
149900981026** ERR
150000981026Codice Testo Errato                                                             1
150100981026Per questo testo non si devono aggiungere  note                                 2
