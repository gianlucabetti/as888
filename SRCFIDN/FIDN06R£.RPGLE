000100980506      *------------------------------------------------------------------------*
000200980602      * GESTIONE APERTURE EVENTO                                               *
000300990119      * in gaitrafil non lancio la trasmissione
000400980506      *------------------------------------------------------------------------*
000500980506     H DECEDIT('0,') DATEDIT(*DMY.)
000600980506      *--------------------------------------------
000700980615     Ffidn06D   CF   E             WORKSTN sfile(fi06s01:nrr)
000800980615     Ffndet01l  uf a e           k disk
000900980615     Ffnded01l  uf a e           k disk
001000980616     Ffndcs01l  uf a e           k disk
001100990223     Ftntbe01l  uf a e           k disk
001200981005     Ffndct03l  if   e           k disk
001300051018     fFNDPT01L  if   e           k disk
001400100427     fTITAS30C  if   e           k disk    extfile(wLibFile)
001500100427     f                                     usropn
001600051018     fTABEL00F  if   e           k disk
001700980603     Fazorg01l  if   e           k disk
001800051018      *--------------------------------------------
001900051018     d C_DataLimite    c                   const(20050418)
002000980506      *--------------------------------------------
002100051018     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             MSG VIDEO
002200981013     D L1              S              3  0 DIM(30)                              P.O.GESTITI
002300980507      *--------------------------------------------
002400980615     D                SDS
002500980615     D  VTCPGM                 1     10
002600980615      *
002700980507     D KPJBA         E DS
002800980507     D UT§DSE0F      E DS
002900981020     D CNCR80        E DS
003000980615      * Ds passaggio parametri a pgm manutenzione descrizioni
003100980615     D DSDN10        E DS                  EXTNAME(FIDN10DS)
003200980507      *--------------------------------------------
003300980721     D Fidn07ds      E DS
003400981005     D Fidn06ds      E DS
003500981020     D DSbs02        E DS                  EXTNAME(tibs02ds)
003600981013     D dslv55        E DS                  EXTNAME(fnlv55ds)
003700981013     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003800981013     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
003900981020     D DTAD          E DS
004000050701      * Tabella CCH = Causali Chiusura C.A.
004100050701     D DCCH          e ds                  inz
004200990223     D DPAD          E DS
004300050530     d dSTD4         e ds                  inz
004400050530     d  §STD4flg     e                     inz('L')
004500051018      *
004600051018      * Tabella "TB"
004700051018     d dsTB          e ds                  inz
004800980528      *
004900980528     D WLBDAT          DS                  INZ
005000980528     D  G02DAT                 1      8  0
005100980528     D  G02INV                 9     16  0
005200980528     D  G02ERR                17     17
005300980528     D  G02TGI                18     22  0
005400980615     D DSNUMEV         DS                  INZ
005500980615     D  DSANEV                 1      4
005600980615     D  DSNUEV                 5     11
005700980615     D DSevento        DS                  INZ
005800980615     D  v1caa4
005900051020     D  w1cnev                        7  0
006000980611     D COMLIN          S                   LIKE(detfce)
006100980611     D COMLNA          S                   LIKE(detfce)
006200051018     d W1Cdse          s                   like(DETdtr)  inz
006300980615     D ktpd            s                   LIKE(DCStpd)
006400980615     D knkt            s                   LIKE(DCSnkt)
006500980615     D kstd            s                   LIKE(DCSstd)
006600980615     D knks            s                   LIKE(DCSnks)
006700980615     D ktrc            s                   LIKE(DCStrc)
006800980615     D kdim            s                   LIKE(DCSdim)
006900980615     D khim            s                   LIKE(DCShim)
007000050701     D CCH             S                   like(DCTcch)  dim(99)  inz
007100010718     D cisono          s              2
007200980611     D wdatac          S              8  0
007300980611     D w001a           s              1
007400981005     D w0080           S              8  0
007500981005     D selez           s              1  0
007600981013     D x               s              3  0
007700050701     D xx              s              2  0
007800990212     d confe           s              1
007900990223     DW_numeve         s              7  0
008000051020     d INVdev          s              8  0 inz
008100051020     d ANNOev          s              4  0 inz
008200990223     d                 DS                  INZ
008300990223     d  Kke1                               LIKE(TBEKE1)
008400990223     d  Wdit                   1      3
008500990223     d  Wpra                   4      7s 0
008600050705     D Kcod            S                   LIKE(TBEcod)
008700051020      *
008800051020     d NRR             s              4  0 inz
008900051018      *
009000051018     d $CAok_EvFo      s              1    inz(*off)
009100100427
009200100427       // -?Nome esteso Libreria/File per TITAS30C?
009300100427     d wLibFile        s             21a   inz
009400980528      *
009500980603      ***********************************************************
009600980603     C*  RIEPILOGO INDICATORI
009700980603      ***********************************************************
009800980603      * 01    - APERTURA EVENTO  chain fndet
009900980603      * 10    - Protezione campi video in manutenzione
010000980603      * 11    - Attivazione comandi a video F12- F6
010100980603      * 12    - Attivazione comandi a video F18
010200010625      * 13    - Protezione campo anomalia con calcolo responsabilità
010300981005      * 15    - Protezione campi video per interrogazione (chiamata da pgm ricerca eventi)
010400981110      * 16    - Protezione Azienda Pratica assicurativa dopo invio denuncia a Consolindustria
010500980603      * 28    - ERRORE GENERICO EMISSIONE CAMPO MESSAGGI
010600980603      * 30    - di comodo
010700981020      * 31    - di comodo
010800981110      * 40/48 - Errori a video
010900990223      * 99    - Errore allocazione
011000980603      *----------------------------------------------------------
011100980506      *
011200981005     c                   if        not *in15
011300981005     c*
011400981005    1C                   DOW                     *inkc = *off
011500980602      * EMISSIONE TESTATA  * ZOCCOLO
011600980603      *indicatori attivazione f3 e disattivo gli altri
011700980603     c                   setoff                                       1112
011800980612      * indicatore 01 di immissione nuovo evento
011900980612     c                   setoff                                       01
012000980603      *indicatori errori
012100980603      *
012200980611     C                   WRITE     FI06Z01
012300980611     C                   exfmt     FI06T01
012400981030     C                   setoff                                       404130
012500980603     C                   setoff                                       28
012600980603     C                   CLEAR                   V1CMSG
012700980602      * F3=FINE spento
012800980603    2c                   IF        *INKC =*off
012900980603     c* controllo il formato
013000980603     c                   EXSR      SUB_CTR1
013100981005     c* gestione dettaglio eventi
013200981020     c  n28              exsr      sub_GESSBF
013300980528    2c                   endif                                                  *NO CMD 3 Fine
013400980506      *
013500980603     c     end99         tag
013600980528    1c                   enddo                                                  *fine ciclo KC *on
013700981005     c*
013800981005     c                   else
013900981005     c*
014000981005     c* se acceso l'indicatore 15 devo caricare i dati in interrogazione e controllra se sono state
014100981005     c* aperte delle C.A. per ogni P.O. segnalato
014200981005     c* carico i dati della videata
014300981005     c                   exsr      SUB_CARFV
014400981005     c*
014500981005     c                   endif
014600980528      *
014700980720     c                   clear                   dsdn10
014800980615     c                   movel     'C'           i10tla
014900980615     c                   movel     dsdn10        kpjbu
015000990119     c                   call      'FIDN10R'
015100990119     c                   parm                    kpjba
015200980615     c*
015300981020     c     fine          tag
015400990212     c*
015500990212     c* se confe = a 'Y' lancio la trasmissione se sono in sede
015600050218     c**!!!              if        simfel = 0 and confe = 'Y'
015700990212      * trasmetto immediatamente il fle in filiale
015800050218     c**!!!              feod      fndet01l
015900050218     c**!!!              feod      fnded01l
016000050218     c**!!!              feod      fndcs01l
016100050218     c**!!!              CALL      'FIDN82R'
016200050218     c**!!!              PARM                    KPJBA
016300050218     c**!!!              endif
016400990212     c*
016500981029     c                   clear                   dsbs02
016600981029     c                   movel     'C'           t02tla
016700981029     c                   call      'TIBS02R'
016800981029     c                   parm                    kpjba
016900981029     c                   parm                    dsbs02
017000980506     C                   EVAL      *INLR = *ON
017100980506      *****************************************************************
017200980506      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
017300980506      *****************************************************************
017400980603     C     SUB_CTR1      BEGSR
017500981110      *
017600981110     c* pulisco il video
017700981110     c                   clear                   v1cdte
017800981110     c                   clear                   v1cdes
017900981110     c                   clear                   v1hdes
018000981110     c                   clear                   v1cseg
018100981110     c                   clear                   v1hseg
018200981110     c                   clear                   v1cfce
018300981110     c                   clear                   v1dfce
018400981110     c                   clear                   v1dtad
018500981110     c                   clear                   v1ctad
018600981110     c                   clear                   v1cdit
018700010718      * indicatore di protezione tipo anomalia
018800081021     c                   setoff                                       1316
018900010718     c* ANNO EVENTO
019000990107    1c                   if        v1caa4 = 0
019100990107     c                   seton                                        4028
019200990107     c                   movel     msg(1)        v1cmsg
019300990107     c                   goto      end01
019400990107     c*
019500990107     c                   else
019600990107      * se anno di 2 cifre
019700990107     C                   if        v1caa4 < 100  and v1caa4 > 60
019800990107     C                   ADD       1900          v1caa4
019900990107     C                   endif
020000990107     C                   if        v1caa4 < 100  and v1caa4 <= 60
020100990107     C                   ADD       2000          v1caa4
020200990107     C                   endif
020300990107      *
020400990107    1c                   endif
020500980603     c* NUMERO EVENTO
020600980615      * F10=Inserimento nuovo evento
020700980615    1c                   if        *inkj  = *on
020800980612     c* accendo l'indicatore di immissione nuovo evento
020900980612     c                   seton                                        01
021000980603     c* cerco un nuovo numero
021100981013      *
021200990223     C                   exsr      RIC_NUM
021300990223     c   28              goto      end01
021400990223     c
021500981013
021600980603     C*
021700990223     C                   Z-ADD     W_numeve      w1cnev
021800990223     C                   move      W_numeve      v1cnev
021900980615      * inizializzo il subfile
022000980615     c                   setoff                                       2120
022100051020     c                   z-add     *zeros        nrr
022200980615     c                   seton                                        22
022300980615     c                   write     fi06c01
022400980615     c                   setoff                                       22
022500981030     c*
022600980603     c*
022700980721   x1c                   else                                                   NO CMD10
022800980603     c* controllo numero evento
022900980721      *
023000980826    2C                   IF        v1cnev <> *blanks  and  v1cnev <> '0000000'
023100980721      *
023200980721      * Ricerca
023300980722     C     '?'           scan      v1cnev                                 30
023400980721    3C                   IF        *IN30 = *ON
023500980721     c                   clear                   fidn07ds
023600981005     c                   movel     'R'           i07mod
023700980721     C                   call      'FIDN07R'
023800980721     c                   PARM                    KPJBA
023900980721     c                   PARM                    FIDN07DS
024000980721    4c                   if        O07err = 'E'
024100980722     c                   movel     *blanks       V1Cnev
024200980722     c                   seton                                        4128
024300980722     c                   movel     o07msg        v1cmsg
024400980722     c                   goto      end01
024500980721   x4c                   else
024600980722     c                   movel     o07nev        v1cnev
024700990127     c                   movel     o07aae        v1caa4
024800980721   x4c                   endif
024900980721    3c                   endif
025000980723     C                   TESTN                   V1CNEV               31
025100980908      * testo ultimo carattere
025200980908     C   31              move      V1CNEV        W001A
025300980908     c   31              eval      *in31 = (W001A >= '0')
025400980908      * Se codice non numerico errore
025500980908    3C                   if        *IN31 = *off
025600980723     C                   movel     MSG(4)        V1CMSG
025700980723     C                   seton                                        4128
025800980723     c                   goto      end01
025900980723     C                   else
026000980723      *
026100980723      * Controllo esistenza evento in testata eventi
026200980722     c                   move      v1cnev        w1cnev
026300980723    3c                   endif
026400980721    2c                   endif
026500980723     c*
026600980721     c* controllo numero evento
026700980722    2c                   if        w1cnev = 0
026800980721     c                   seton                                        4128
026900980721     c                   movel     msg(4)        v1cmsg
027000980721     c                   goto      end01
027100980721   x2c                   else
027200980721     c* aggancio il file eventi per manutenzione
027300980603     c     kdet          chain     fndet01l                           01
027400980603    3c                   if        *in01 = *on
027500980603     c                   seton                                        4128
027600980603     c                   movel     msg(5)        v1cmsg
027700980603     c                   goto      end01
027800980603    3c                   endif
027900980603     c                   exsr      sub_carv2
028000980603    2c                   endif
028100980721    1c                   endif                                                  CMD 10
028200980507      *
028300980603     C     end01         ENDSR
028400990223      *
028500990223      *****************************************************************
028600990223      *   ROUTINE PER la riocerca del nuovo numero evento
028700990223      *****************************************************************
028800990223     C     RIC_NUM       BEGSR
028900990223     c*
029000990223     C                   movel     'EVE'         Wdit
029100990223     c                   z-add     V1Caa4        Wpra
029200990223
029300990223     c     Ktbe          chain     TNTBE000                           3099
029400990223
029500990223     c* se 99 acceso record allocato
029600990223     c                   if        *in99
029700990223     c                   seton                                        28
029800990223     c                   movel     msg(13)       v1cmsg
029900990223     c                   goto      ericnum
030000990223     c                   endif
030100990223     c*
030200990223     c                   IF        *IN30 = *ON
030300990223     c                   clear                   TNTBE000
030400990223     C                   MOVEL     Kcod          TBEcod
030500990223     C                   MOVEL     Kke1          TBEke1
030600990223     C                   z-add     0             §padnume
030700990223     C                   MOVEL(P)  DPAD          TBEuni
030800990223     c                   WRITE     TNTBE000
030900990223     c                   feod      TNTBE01l
031000990223     c*
031100990223     c     Ktbe          chain     TNTBE000                           3099
031200990223     c* se 99 acceso record allocato
031300990223     c                   if        *in99
031400990223     c                   seton                                        28
031500990223     c                   movel     msg(13)       v1cmsg
031600990223     c                   goto      ericnum
031700990223     c                   endif
031800990223     c                   endif
031900990223
032000990223     c                   clear                   dpad
032100990223      *
032200990223     c                   movel     TBEuni        DPAD
032300990223     c     §padnume      add       1             W_numeve
032400990223
032500990223     c*
032600990223     c     ericnum       ENDSR
032700981005      *****************************************************************
032800981005      *   ROUTINE PER la gestione del dettaglio
032900981005      *****************************************************************
033000981005     C     SUB_GESSBF    BEGSR
033100981005     c*
033200981005      * EMISSIONE VIDEATA testata evento
033300981005      * posizionamento del cursore
033400981005     c                   seton                                        43
033500981005     c                   seton                                        1112
033600981005     C                   move      v1cnev        vccnev
033700981005    1C                   dow                     *inkl = *off
033800981005     C                   write     fi06z01
033900981005      *indicatori disattivazione f3 e attivo gli altri f12/f6
034000981005     c                   seton                                        2021
034100981005     c                   z-add     1             nrr
034200981005     C                   EXFMT     FI06c01
034300981005     C                   CLEAR                   V1CMSG
034400981110     C                   setoff                                       28
034500981110     c                   setoff                                       404142
034600981005     c                   setoff                                       434445
034700981110     c                   setoff                                       464748
034800051018     c                   setoff                                       49
034900981005      * F12=PRECEDENTE
035000981005    2c                   IF        *INKl =*off
035100981005      * CONTROLLI VIDEATA
035200981005     C                   EXSR      SUB_CTR2
035300981005      *
035400981005    2c                   endif
035500981005      * se non ci sono errori registro l'evento
035600990201      * e ha dato il CMD 06  e sono in sede
035700981005    2C                   if        *in28 = *off and
035800990201     c                             *inkf = *on
035900981005      * Aggiorno campi file da video e scrivo
036000981005     c                   exsr      SUB_WRITE
036100050518      * Gestione finestra  richiesta stampa Apertura evento ad AIG
036200050518     c                   exsr      Ges_wind
036300981005      * vado in prima videata
036400981005     c                   eval      *inkl = *on
036500051020     c                   clear                   w1cnev
036600981005     c                   clear                   v1cnev
036700981005      *
036800981005    2c                   endif                                                  *SI CMD 6 Conferma
036900981005    1c                   enddo                                                  *NO CMD12
037000050916      *
037100050916      * Libero FNDET per FIDN86R
037200050916     c                   UNLOCK(e) FNDET01L
037300050916      *
037400051018     ***C                   write     fi06p01
037500010718     c                   setoff                                       10
037600981005     c                   clear                   w1cnev
037700981005     c                   clear                   v1cnev
037800981005     c*
037900981005     c                   endsr
038000980603      *****************************************************************
038100980603      *   ROUTINE PER CARICARE SECONDA VIDEATA
038200980603      *****************************************************************
038300980615     c     SUB_CARV2     BEGSR
038400051121      *
038500051121      * reperisco l'anomalia e la sua descrizione
038600051121     c                   clear                   dTAD
038700051121     c                   clear                   dsbs02
038800051121     c                   movel     'C'           t02mod
038900051121     c                   movel     knsif         t02sif
039000051121     c                   movel(P)  DETtad        t02ke1
039100051121     c                   movel     'TAD'         t02cod
039200051121      *
039300051121     c                   call      'TIBS02R'
039400051121     c                   parm                    KPJBA
039500051121     c                   parm                    dsbs02
039600051121      *
039700051121    4c                   if        t02err =  *blanks
039800051121     c                   movel     t02ke1        v1ctad
039900051121     c                   movel     t02uni        dtad
040000051121     c                   movel     §TADdesc      v1dtad
040100051121     c                   endif
040200010718      *
040300010718      * verifico sempre se ci sono delle C.A. aperte su questo evento
040400010718      *
040500010718     c                   clear                   cisono
040600050701     c                   clear                   vcctmr
040700010718      *
040800010718     c     kdet          setll     fndct03l
040900010718      * se trovato almeno un record
041000051018if  1c                   if        %equal(fndct03l)
041100051018      *
041200051018do  2c                   do        *hival
041300010718     c     kdet          reade     fndct03l
041400010718      * fine file
041500010718     c                   if        %eof(fndct03l)
041600010718     c                   leave
041700010718     c                   endif
041800010718      * se annullata leggo le successive
041900051018if  3c                   if        dctatb = 'A'
042000010718     c                   iter
042100051018x   3c                   else
042200010718      * se non anullata valorizzo il flag di esistenza C.A.  e vado a fine lettura
042300010718     c                   eval      cisono ='SI'
042400050701      * se la CA  risulta chiusa con causali di pagamento oppure è aperta calcolo
042500050701      * il totale massimo risarcibile
042600050701     c                   If        dctcch <> '  '
042700050701      * verifico se presente nelle causali di chiusura tecniche
042800050701     c     dctcch        lookup    cch(1)                                 50
042900050701     c                   If        *in50
043000050701     c                   iter
043100050701     c                   endif
043200050701     c                   endif
043300051018      *
043400051018      * VERIFICA IMPORTO MASSIMO RISARCIBILE PER EVENTO FORTUITO
043500051018     c                   eval      $CAok_EvFo =  *off
043600051018     c                   if        §TADevfo   =  'S'
043700051018     c                   exsr      Ctr_IMRxEvFor
043800051018     c                   endif
043900050701      * calcolo totale
044000051018     c                   if            §TADevfo   = *blanks
044100051018     c                             or  §TADevfo   = 'S'
044200051018     c                             and $CAok_EvFo = *on
044300051018     c                   eval      vcctmr = vcctmr +  dctipv
044400051018     c                   endif
044500051018      *
044600050701     c                   iter
044700051018e   3c                   endif
044800010718      *
044900051018e   2c                   enddo
045000980615      *
045100051018e    c                   endif
045200010718      *
045300980615      * data apertura evento
045400980615     c                   eval      wdatac=(detaae * 10000) + detmge
045500980615     C                   CLEAR                   WLBDAT
045600980615     C                   Z-ADD     wdatac        G02inv
045700980615     C                   movel     '3'           G02err
045800980615     C                   CALL      'XSRDA8'
045900980615     C                   PARM                    WLBDAT
046000980615     c                   z-add     g02dat        v1cdte
046100980615      * P.O. che ha comunicato l'evento decodifica
046200980603     c                   move      detfce        v1cfce
046300980603     c     detfce        chain     azorg01l                           30
046400980615    1C                   if        *in30 = *off
046500980603     C                   movel     orgdes        v1dfce
046600980615   x1C                   else
046700980603     C                   clear                   v1dfce
046800980615    1C                   endif
046900120515      * data segnalazione evento a Chartis
047000051018     c                   clear                   V1Cdse
047100051018     c                   clear                   W1Cdse
047200051018     c                   if        DETdtr > C_DataLimite
047300051018     c                   clear                   WLBdat
047400051018     c                   eval      G02inv = DETdtr
047500051018     c                   eval      G02err = '3'
047600051018     c                   call      'XSRDA8'
047700051018     c                   parm                    WLBdat
047800051018     c                   eval      V1Cdse = G02dat
047900051018     c                   eval      W1Cdse = DETdtr
048000051018     c                   endif
048100051121     *** *reperisco l'anomalia e la sua descrizione
048200051121     *** > SPOSTATO ALL'INIZIO DELLE SUBR. <
048300051121     ***c                   clear                   dsbs02
048400051121     ***c                   movel     'C'           t02mod
048500051121     ***c                   movel     knsif         t02sif
048600051121     ***C                   MOVEL(P)  dettad        t02ke1
048700051121     ***C                   movel     'TAD'         t02cod
048800051121     *** *
048900051121     ***C                   CALL      'TIBS02R'
049000051121     ***c                   parm                    KPJBA
049100051121     ***c                   parm                    dsbs02
049200051121     *** *
049300051121     ***c                   if        t02err =  *blanks
049400051121     ***C                   MOVEL     t02ke1        v1ctad
049500051121     ***c                   movel     t02uni        dtad
049600051121     ***C                   movel     §TADdesc      v1dtad
049700051121     ***c                   endif
049800010625     c*
049900010718      * Proteggiamo l'anomalia se esiste il calcolo responsabilità e se ci sono delle CA aperte
050000010718      * su questo evento
050100050719     c****               if        §tadcare = 'S' and cisono = 'SI'
050200050719      * proteggo sempre se ci sono delle CA legate all'evento
050300050719     c                   if        cisono = 'SI'
050400010625     c                   seton                                        13
050500010718     c                   endif
050600010625      *
050700981110      * reperisco ditta intestataria Pratica Assicurativa
050800981110     c                   move      detdit        v1cdit
050900981020      * reperisco dati vettore e polizza
051000981020     c                   move      dettrg        v1ctrg
051100981020     c                   move      detvet        v1cvet
051200981020     c                   move      detpca        v1cpca
051300981020     c*
051400980615      *reperisco da file descrizioni la descrizione dell'evento
051500980615     c                   movel     'E'           ktpd
051600980617     c                   movel     'D'           ktrc
051700980615     C                   MOVE      DETAAE        dsanev
051800980615     C                   MOVE      DETnev        dsnuev
051900980615     c                   movel     dsnumEV       knkt
052000980615     c                   exsr      chadcs
052100980615     c  n30              movel     dcsnot        v1cdes
052200980615      * dati denuncia
052300980611     c                   z-add     detaad        v1oaad
052400980908     c                   z-add     detpod        v1opod
052500981020     c                   z-add     detnde        v1onde
052600981110      * dati pratica assicuarativa solo se numero pratica inserito
052700981110     c                   if        detprn > 0
052800981020     c                   move      detdit        v1odit
052900981020     c                   z-add     detpra        v1opra
053000981020     c                   z-add     detprn        v1oprn
053100981110     c                   seton                                        16
053200981110     c                   else
053300981110     c                   clear                   v1odit
053400981110     c                   clear                   v1opra
053500981110     c                   clear                   v1oprn
053600981110     c                   setoff                                       16
053700981110     c                   endif
053800980603     c*
053900980611     c                   z-add     detdsb        w0080
054000980603     C                   Z-ADD     w0080         G02inv
054100980603     C                   movel     '3'           G02err
054200980603     C                   CALL      'XSRDA8'
054300980603     C                   PARM                    WLBDAT
054400980603     c                   z-add     g02dat        v1odsb
054500051018     ***c*
054600051018     *** » (data stampa denuncia / invio a Consuldanni
054700051018     ***    non più visualizzata dal "lontano" 07/09/2005)
054800051018     ***c                   z-add     detdso        w0080
054900051018     ***C                   Z-ADD     w0080         G02inv
055000051018     ***C                   movel     '3'           G02err
055100051018     ***C                   CALL      'XSRDA8'
055200051018     ***C                   PARM                    WLBDAT
055300051018     ***c                   z-add     g02dat        v1odso
055400980603     c*
055500981110     c* accendo l'indicatore 10 per proteggere i campi da non manutenzionare
055600980603     c                   seton                                        10
055700980615     c* carico i dati del sub-file dei punti operativi interessati
055800980615     c                   exsr      SUB_WRSBF
055900980603     c* emetto la testata / il dettaglio / lo zoccolo
056000980603     c                   write     fi06z01
056100980603     c*
056200980603     c                   endsr
056300051018      *****************************************************************
056400051018      * CALCOLO IMPORTO MASSIMO RISARCIBILE X C.A. DI EVENTO FORTUITO *
056500051018      *****************************************************************
056600051018     c     Ctr_IMRxEvFor BEGSR
056700051018      *
056800051018      *  Verifica se Mandato Assicurativo
056900051018     c     K03DPT01      chain     FNDPT000
057000051018      *
057100051018if  1c                   IF            %found(FNDPT01L)
057200051018     c                             and DPTatb =  *blanks
057300051018      *
057400051018     c                   eval      $CAok_EvFo =  *on
057500051018      *
057600051018x   1c                   else
057700051018      *
057800051018      *  Verifica se Importo da Assicurare in bolla
057900051018     c                   eval      *in30      =  *off
058000051018     c     K04TAS30      chain     TITAS30C                           30
058100051021      *    (?impossibile?che NON ci sia... )
058200051018      *  - se tipo bolla non è valido cerco altro rcd
058300051018     c                   eval      TBLkut = 1
058400051018     c                   eval      TBLcod = 'TB'
058500051018     c                   movel(p)  TAStbl        TBLkey
058600051018     c     K03TABEL      chain(n)  TABEL
058700051018if  2c                   if        %found(TABEL00F)
058800051018     c                   movel     TBLuni        dsTB
058900051018if  3c                   if        §TBrbl = 'R'
059000051018     c     K04TAS30      reade     TITAS30C                               30
059100051018e   3c                   endif
059200051018e   2c                   endif
059300051018      *
059400051018if  2c                   if        NOT *in30
059500051018     c                             and TASias > *zeros
059600051018     c                             and TASfcm = *blanks
059700051018     c                   eval      $CAok_EvFo =  *on
059800051018e   2c                   endif
059900051018      *
060000051018e   1c                   ENDIF
060100051018      *
060200051018     c                   ENDSR
060300980603      *****************************************************************
060400980603      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
060500980603      *****************************************************************
060600980603     C     SUB_CTR2      BEGSR
060700980603     c* DATA EVENTO
060800980603     C                   CLEAR                   WLBDAT
060900980615     C                   Z-ADD     v1cdte        G02DAT
061000980603     C                   CALL      'XSRDA8'
061100980603     C                   PARM                    WLBDAT
061200980603    1C     G02ERR        IFEQ      '1'
061300980603     C                   MOVEL     MSG(6)        V1CMSG
061400980603     C                   SETON                                        4328
061500980615     c                   goto      endc2
061600980603   X1C                   ELSE
061700051020     C                   MOVE      G02INV        INVdev
061800980615     C                   MOVE      G02dat        v1cdte
061900980603    1C                   END
062000120515      * data segnalazione evento a Chartis
062100051018     c                   clear                   W1Cdse
062200051018if  1c                   if        V1Cdse > *zeros
062300051018     c                   clear                   WLBdat
062400051018     c                   eval      G02dat = V1Cdse
062500051018     c                   call      'XSRDA8'
062600051018     c                   parm                    WLBdat
062700051018if  2c                   if        G02err = *on
062800051018     c                   movel     Msg(2)        V1Cmsg
062900051018     c                   seton                                        49  28
063000051018     c                   leavesr
063100051018e   2c                   endif
063200051018     c                   eval      V1Cdse = G02dat
063300051018     c                   eval      W1Cdse = G02inv
063400051018sel 2c                   select
063500051018w   2c                   when      W1Cdse < INVdev
063600051018     c                   movel     Msg(3)        V1Cmsg
063700051018     c                   seton                                        49  28
063800051018     c                   leavesr
063900051018w   2c                   when      W1Cdse < C_DataLimite
064000051018     c                   eval      V1Cmsg = %trim(Msg(2))
064100051018     c                                    + ' (antecedente il giorno +
064200051018     c                                      18/04/2005)'
064300051018     c                   seton                                        49  28
064400051018     c                   leavesr
064500051018w   2c                   when      W1Cdse > DateU
064600051018     c                   eval      V1Cmsg = %trim(Msg(2))
064700051018     c                                    + ' (futura)'
064800051018     c                   seton                                        49  28
064900051018     c                   leavesr
065000051018e   2c                   endsl
065100051018e   1c                   endif
065200981105     c*  anno creazione numero evento = all'anno data evento
065300051020     c                   movel     invdev        annoev
065400980615     c     annoev        ifne      v1caa4
065500980615     C                   eval      *in28 = *on
065600980615     c                   eval      *in43 = *on
065700980615     c                   movel     msg(9)        v1cmsg
065800980615     c                   goto      endc2
065900981020    2C                   Endif
066000980603     c* punto operativo che ha comunicato l'evento
066100980615    1c                   if        v1cfce <> *blanks
066200980615     c                             or  v1cfce <> *zeros
066300980615     C     '?'           SCAN      v1cfce                                 30
066400980615      * Ricerca
066500980615    2C                   IF        *IN30 = *ON
066600980615     C                   MOVE      *BLANKS       §COD1
066700980615     C                   MOVE      *BLANKS       v1cfce
066800980615     C                   CALL      'TNSD19R'
066900980615     C                   PARM                    §COD1             3
067000980615     C                   PARM                    §TIP              1
067100980615     C                   PARM                    §DES             25
067200980615     C                   MOVEL     §COD1         v1cfce
067300980615     C                   MOVEL     §DES          V1dfce
067400980615    2C                   ENDIF
067500980615     C*
067600980615     C                   testn                   V1cfce               30
067700980615      * testo ultimo carattere
067800980615     C   30              move      V1cfce        W001A
067900980615     C   30              eval      *in30 = (W001A >= '0')
068000980615      * Se codice non numerico errore
068100980615    2C                   IF        *IN30 = *OFF
068200980615     C                   eval      *in28 = *on
068300980615     c                   eval      *in44 = *on
068400980615     c                   movel     msg(7)        v1cmsg
068500980615     c                   goto      endc2
068600980615   x2C                   ELSE
068700980615      * Controllo
068800980615     C*
068900980615    3C                   IF        v1cfce <> *blanks
069000980615     C                   movel     v1cfce        comlna
069100980615     C     comlna        chain     azorg01l                           30
069200980615    3C                   endif
069300980615     C*
069400980615     C* CONTROLLO SE ANNULLATO
069500980615     C* CONSIDERO SOLO I TIPI "F" E "A"
069600980615    3C  n30              if        orgfva <> *blank or
069700980615     C                             orgfag <> 'F'  and
069800980615     C                             orgfag <> 'A'
069900980615     C                   SETON                                        30
070000980615    3C                   ENDif
070100980615     C* DESCRIZIONE
070200980615     C  n30              MOVEL     ORGDES        V1dfce
070300980615    3C                   IF        *IN30 = *on
070400980615     C                   eval      *in28 = *on
070500980615     c                   eval      *in44 = *on
070600980615     c                   movel     msg(7)        v1cmsg
070700980615     c                   goto      endc2
070800980615    3C                   Endif
070900980615    2C                   Endif
071000980615   x1c                   else
071100980615     C                   eval      *in28 = *on
071200980615     c                   eval      *in44 = *on
071300980615     c                   movel     msg(7)        v1cmsg
071400980615     c                   goto      endc2
071500980603    1c                   endif
071600981020      * Controllo il tipo anomalia
071700981020    1c                   if        *in28 = *off
071800981020     c                   clear                   v1dtad
071900981030    2c                   if        v1ctad = '  '
072000981020     c                   seton                                        4728
072100981020     c                   movel     msg(10)       v1cmsg
072200981020     c                   goto      endc2
072300981020    2c                   endif
072400981020      * controllo Tipo Anomalia
072500981020    2c                   if        v1ctad <> '  '
072600981020      *
072700981020     C     '?'           SCAN      v1Ctad                                 31
072800981020      *
072900981020    3C                   IF        *IN31 = *ON
073000981020     c                   clear                   dsbs02
073100981020     c                   movel     'R'           t02mod
073200981020     c                   movel     knsif         t02sif
073300981020     C                   movel     'TAD'         t02cod
073400981020      *
073500981020     C                   CALL      'TIBS02R'
073600981020     c                   parm                    KPJBA
073700981020     c                   parm                    dsbs02
073800981020      *
073900981020    4c                   if        t02err =  *blanks
074000981020     C                   MOVEL     t02ke1        v1ctad
074100981020     c                   movel     t02uni        dtad
074200981020     C                   movel     §TADdesc      v1dtad
074300990212     c                   seton                                        47
074400990212     c                   goto      endc2
074500981020   x4c                   else
074600981110     c                   seton                                        4728
074700981110     c                   movel     msg(10)       v1cmsg
074800981020     c                   goto      endc2
074900981020    4c                   endif
075000981020      *
075100981020   x3C                   Else
075200981020      *   Controlli di validità'
075300981020      *
075400981020     c                   clear                   dsbs02
075500981020     C                   MOVEL     'C'           t02mod
075600981020     C                   MOVEL     knsif         t02sif
075700981020     C                   MOVEL     'TAD'         t02cod
075800981020     C                   MOVEL(P)  v1ctad        t02ke1
075900981020      *
076000981020     C                   CALL      'TIBS02R'
076100981020     C                   PARM                    KPJBA
076200981020     C                   PARM                    dsbs02
076300981020      *
076400981020    4C                   if        t02err = *BLANKS
076500981020     C                   MOVEL     T02UNI        DTAD
076600981020     C                   movel     §TADdesc      v1dtad
076700981020   x4c                   else
076800981020     c                   seton                                        4728
076900981020     c                   movel     msg(10)       v1cmsg
077000981020     c                   goto      endc2
077100981020    4C                   endif
077200981020    3C                   endif
077300981020      * Se Anomalia per non Eventi non è valida
077400981020    3C                   IF        §TADragr <> 'E'
077500981020     C                   seton                                        4728
077600981020     c                   movel     MSG(11)       v1cmsg
077700981020     c                   goto      endc2
077800981020    3C                   ENDIF
077900010718      * solo in caso di modifica del tipo anomalia verifico se tipo anomalia con calcolo della
078000010718      * responsabilità
078100010718     c                   if        *in10                                        sono in MANUTENZIONE
078200020424     c                             and V1Ctad <> DETtad
078300050718      * tolto il controllo del calcolo responsabilità e verifico solo se esistono
078400050718      * delle CA aperte sull'ebento per bloccare il cambio tipo anomalia
078500050718     C****               if        §tadcare <> *blanks and cisono = 'SI'
078600050718     C                   if        cisono = 'SI'
078700010625     C                   seton                                        4728
078800010625     C                   movel     msg(15)       v1cmsg
078900010625     C                   goto      endc2
079000010625     C                   endif
079100010718      *
079200010718     c                   endif
079300010718      *
079400981020    2C                   endif
079500981020    1C                   endif
079600980615     c
079700981020      * controllo la descrizione
079800981020    1c     v1cdes        ifeq      *blanks
079900981020     c     v1cseg        andeq     ' '
080000981020     C                   eval      *in28 = *on
080100981020     c                   eval      *in45 = *on
080200981020     c                   movel     msg(8)        v1cmsg
080300981020     c                   goto      endc2
080400981020    1c                   endif
080500981020     c     v1cseg        ifeq      '+'
080600981020     C* Richiamo pgm gestione descrizioni
080700981020     C                   CLEAR                   DSDN10
080800981020     c                   move      v1cdte        i10dta
080900981020     c   01              movel     'M'           i10flm
081000981020     c  n01              movel     'V'           i10flm
081100981020     c                   movel     'E'           i10tpd
081200981020     c                   movel     'D'           i10trc
081300981020     c                   movel     dsevento      i10nkt
081400981020     c                   movel     v1cdes        i10not
081500981020     c                   move      v1cdte        i10dta
081600981020     c                   movel(P)  dsdn10        kpjbu
081700981020     C                   CALL      'FIDN10R'                            99
081800981020     C                   PARM                    kpjba
081900981020     c                   movel     kpjbu         dsdn10
082000981020    2C     O10ERR        IFEQ      *BLANKS
082100981020     C                   MOVEL     O10NOT        V1cdes
082200981020     C                   MOVEL     O10NOT        V1hdes
082300981020     C                   CLEAR                   V1Cseg
082400981020     c     v1hdes        ifne      *blanks
082500981020     c                   move      '+'           v1hseg
082600981020     c                   end
082700981020    2C                   END
082800981020    1C                   END
082900981020     c                   movel     v1cdes        v1hdes
083000981110     c* controllo se inserita l'azienda Pratica Assicurativa
083100981110     c                   if        v1cdit = *blanks
083200981110     c                   seton                                        4828
083300981110     c                   movel     msg(12)       v1cmsg
083400981110     c                   goto      endc2
083500981110    4C                   endif
083600981020     c
083700001227
083800001227      * controllo se data evento maggiore del  2000 non può essere legata alla
083900001227      * ditta SDI
084000001227     c                   if        v1cdit = 'SDI' and annoev > 2000
084100001227     c                   seton                                        4828
084200001227     c                   movel     msg(14)       v1cmsg
084300001227     c                   goto      endc2
084400001227    4C                   endif
084500001227     c
084600980615      * se non ci sono errori controllo il subfile dei p.o.
084700980615    1c                   if        *in28 = *off
084800980615     c                   exsr      sub_poin
084900980615    1c                   endif
085000980603      *
085100980615     c     endc2         endsr
085200980507      *****************************************************************
085300980603      *   ROUTINE GESTIONE SUBFILE PUNTI OPERATIVI INTERESSATI
085400980507      *****************************************************************
085500980603     C     SUB_POIN      BEGSR
085600980604     c* gestione subfile
085700980603     c                   z-add     1             nrr
085800980615     c                   setoff                                       31
085900980603     c* controllo se sono stati immessi dei dati
086000980615    1c                   dow       *in31 = *off
086100980616     c                   setoff                                       2846
086200980615     c     nrr           chain     fi06s01                            31
086300980615    2c                   if        *in31 = *off
086400980616    3c                   if        v1hfie <> *blanks and v1hfie <> *zeros
086500980616     c                   seton                                        10
086600980616     c                   else
086700980616     c                   setoff                                       10
086800980616    3c                   endif
086900980603     c* se 10 spento controllo la filiale  se valorizzata
087000980615    3c                   if        *in10 = *off
087100980615     c                             and v1sfie <> *blanks
087200980615     c                             and v1sfie <> *zeros
087300980615     C     '?'           SCAN      v1sfie                                 30
087400980603      * Ricerca
087500980615    4C                   IF        *IN30 = *ON
087600980603     C                   MOVE      *BLANKS       §COD1
087700980615     C                   MOVE      *BLANKS       v1sfie
087800980603     C                   CALL      'TNSD19R'
087900980603     C                   PARM                    §COD1             3
088000980603     C                   PARM                    §TIP              1
088100980603     C                   PARM                    §DES             25
088200980615     C                   MOVEL     §COD1         v1sfie
088300980615     C                   MOVEL     §DES          V1sdfe
088400980615     c                   seton                                        46
088500980615    4C                   ENDIF
088600980603     C*
088700980615     C                   testn                   V1sfie               30
088800980603      * testo ultimo carattere
088900980615     C   30              move      V1sfie        W001A
089000980603     C   30              eval      *in30 = (W001A >= '0')
089100980603      * Se codice non numerico errore
089200980615    4C                   IF        *IN30 = *OFF
089300980603     C                   eval      *in28 = *on
089400980615     c                   eval      *in46 = *on
089500980603     c                   movel     msg(7)        v1cmsg
089600980615   x4C                   ELSE
089700980603      * Controllo
089800980603     C*
089900980615    5C                   IF        v1sfie <> *blanks
090000980615     C                   movel     v1sfie        comlna
090100980604     C     comlna        chain     azorg01l                           30
090200980615    5C                   endif
090300980603     C*
090400980603     C* CONTROLLO SE ANNULLATO
090500980603     C* CONSIDERO SOLO I TIPI "F" E "A"
090600980615    5C  n30              if        orgfva <> *blank or
090700980604     C                             orgfag <> 'F'  and
090800980604     C                             orgfag <> 'A'
090900980604     C                   SETON                                        30
091000980615    5C                   ENDif
091100980603     C* DESCRIZIONE
091200980615     C  n30              MOVEL     ORGDES        V1sdfe
091300980615    5C                   IF        *IN30 = *on
091400980604     C                   eval      *in28 = *on
091500980615     c                   eval      *in46 = *on
091600980615     c                   movel     msg(7)        v1cmsg
091700980615    5C                   Endif
091800980615    4C                   Endif
091900980615    3c                   endif
092000980615     c                   update    fi06s01
092100980611     c                   add       1             nrr
092200980615    2c                   endif
092300980615     c   28              seton                                        31
092400980615    1c                   enddo
092500980604     c*
092600980507     C                   ENDSR
092700981005      *****************************************************************
092800981105      *   ROUTINE PER GESTIRE LA VIDEATA DI INTERROGAZIONE
092900981005      *****************************************************************
093000981005     c     SUB_CARFV     BEGSR
093100981005      *
093200981005      * Aggancio il file FNDET01l
093300981005     c     kdet          chain(n)  fndet01l                           01
093400981005     c*
093500981005     c* caricamento dati
093600981005     c                   exsr      SUB_CARV2
093700981005     c*
093800981005     c* emetto la videata
093900981005    1C                   dow                     *inkl = *off
094000981005     C                   write     fi06z01
094100981005      *indicatori disattivazione f3 e attivo gli altri f12
094200981005     c                   seton                                        2021
094300981005     c                   z-add     1             nrr
094400981005     C                   EXFMT     FI06c01
094500981116     c                   if        v1cseg = '+' and *inkl = *off
094600981116     C* Richiamo pgm gestione descrizioni
094700981116     C                   CLEAR                   DSDN10
094800981116     c                   move      v1cdte        i10dta
094900981116     c                   movel     'I'           i10flm
095000981116     c                   movel     'E'           i10tpd
095100981116     c                   movel     'D'           i10trc
095200981116     c                   movel     dsevento      i10nkt
095300981116     c                   movel     v1cdes        i10not
095400981116     c                   move      v1cdte        i10dta
095500981116     c                   movel(P)  dsdn10        kpjbu
095600981116     C                   CALL      'FIDN10R'                            99
095700981116     C                   PARM                    kpjba
095800981116     c                   clear                   v1cseg
095900981116    1C                   END
096000981116     c*
096100981005     c                   enddo
096200981005     c*
096300981005     c                   endsr
096400980604      *****************************************************************
096500980604      *   ROUTINE SCRITTURA SUBFILE
096600980604      *****************************************************************
096700980604     C     SUB_WRSBF     BEGSR
096800980604      * inizializzo il subfile
096900980604     c                   setoff                                       2120
097000051020     c                   z-add     0             nrr
097100980604     c                   seton                                        22
097200980615     c                   write     fi06c01
097300980604     c                   setoff                                       22
097400980604      * se sono in variazione carico il subfile con i p.o. precedentemente inseriti
097500980604      * con i campi del codice protetto in quanto non si possono modificare ma solo
097600980604      * annullare e riscrivere
097700980611    1c                   if        *in01 = *off
097800980604     c                   seton                                        10
097900980604     c     kdet          setll     fnded01l
098000980604     c     kdet          reade     fnded01l                               31
098100980611    2c                   dow       *in31 = *off
098200980604     c                   add       1             nrr
098300980615     c     nrr           chain     fi06s01                            30
098400980615     c                   move      dedatb        v1sann
098500980615     c                   move      dedfie        v1sfie
098600980616     c                   move      dedfie        v1hfie
098700980604     c     dedfie        chain     azorg01l                           31
098800980615     C  n31              movel     orgdes        v1sdfe
098900981005     c* in caso di interrogazione evento devo verificare se per ogni punto operativo
099000981005     c* sono state aperte delle CA
099100981005     c                   exsr      RIC_CA
099200981005     c*
099300980615     c  n30              update    fi06s01
099400980615     c   30              write     fi06s01
099500980604     c     kdet          reade     fnded01l                               31
099600980611    2c                   enddo
099700980611    1c                   endif
099800981006     c* in caso di interrogazione aggiorno i record rimasti del subfile per proteggere il campo
099900981006     c* del P.O. interessato
100000981006     c                   dow       not *in30  and *in15
100100981006     c                   add       1             nrr
100200981006     c     nrr           chain     fi06s01                            30
100300981006     c  n30              update    fi06s01
100400981006     c                   enddo
100500980604     c*
100600980604     c                   endsr
100700980615      *****************************************************************
100800980615      * ROUTINE LETTURA RECORD FILE DESCRIZIONI
100900980615      *****************************************************************
101000980615     C     CHADCS        BEGSR
101100980615     c     kdcs          setll     fndcs01l
101200980616     c     kdcs          reade(n)  fndcs01l                               30
101300980615     c     *in30         doweq     *off
101400980615     c     dcsatb        andne     *blanks
101500980616     c     kdcs          reade(n)  fndcs01l                               30
101600980615     c                   enddo
101700980615     c                   endsr
101800980615      *****************************************************************
101900980615      *   ROUTINE SCRITTURA DEL FILE
102000980615      *****************************************************************
102100980615     C     SUB_WRITE     BEGSR
102200990223      *
102300990223      * 01 valorizzo il numero evento in tabella
102400980615      *
102500990223     c                   if        *in01
102600990223     C                   z-add     W_numeve      §padnume
102700990223     C                   MOVEL(P)  DPAD          TBEuni
102800990223     c                   update    TNTBE000
102900990223     c                   endif
103000980615      * valorizzo i campi del file di testata evento
103100980615     c   01              clear                   fndet000
103200980615      *
103300980615     c                   z-add     v1caa4        detaae
103400980722     c                   z-add     w1cnev        detnev
103500980615     c                   z-add     invdev        detmge
103600980615     c                   move      v1cfce        detfce
103700981020     c                   move      v1ctad        dettad
103800981020     c                   move      v1ctrg        dettrg
103900981020     c                   move      v1cvet        detvet
104000981020     c                   move      v1cpca        detpca
104100981110     c                   move      v1cdit        detdit
104200050718      * non si deve più pulire il flag di btrasmissione
104300050718     c*********          clear                   detftr
104400050718      * se evento fortuito e flag uguale a blank valorizzo il flag
104500051018     c*****              if        detftr = ' ' and §tadevfo = 'S'
104600051018     c*****              eval      detftr = 'F'
104700051018     c*****              endif
104800051018      * e neanche più impostarlo ad "F"...
104900120515      * data segnalazione evento a Chartis
105000051018     c                   eval      DETdtr = W1Cdse
105100050718      *
105200980615     c   01              write     fndet000
105300980615     c  n01              update    fndet000
105400980616      * scrittura delle note solo in fase di immissione
105500990127     c                   if        *in01 = *on
105600980615     C                   CLEAR                   DSDN10
105700980716     c                   move      v1cdte        i10dta
105800980615     C                   MOVEL     'C'           I10FLM
105900990105     C                   MOVEL     'L'           I10TLA
106000980723     c                   movel     dsevento      i10nkt
106100990119     c                   movel     'E'           i10tpd
106200980617     c                   movel     'D'           i10trc
106300980615     c                   movel     v1cdes        i10not
106400980615     c                   movel(P)  dsdn10        kpjbu
106500980615     C                   CALL      'FIDN10R'                            99
106600980615     C                   PARM                    kpjba
106700990201     c                   endif
106800990201     c*                  else
106900980616     c     v1cdes        ifne      *blanks
107000980616     c                   movel     'E'           ktpd
107100980617     c                   movel     'D'           ktrc
107200980616     c                   movel(P)  dsevento      knkt
107300990119     c                   z-add     1             dcspno
107400990119     c     kdcspr        chain     fndcs01l                           30
107500980616     c     *in30         ifeq      *on
107600980616     c                   clear                   fndcs000
107700980616     c                   movel     ktpd          dcstpd
107800980617     c                   movel     knkt          dcsnkt
107900980617     c                   movel     ktrc          dcstrc
108000980616     c                   z-add     1             dcspno
108100980616     c                   end
108200980616     c                   movel     v1cdes        dcsnot
108300980616     c  n30              clear                   dcsft1
108400980616     c  n30              clear                   dcsdt1
108500980616     c  n30              update    fndcs000
108600980616     c   30              write     fndcs000
108700980616     c                   endif
108800990201     c*                  endif
108900980615      * valorizzo il dettaglio evento
109000980615     c                   z-add     1             nrr
109100980615     c                   setoff                                       31
109200980615    1c                   dow       *in31 = *off
109300980615     c     nrr           chain     fi06s01                            31
109400980615    2c                   if        *in31 = *off
109500980615    3c                   if        v1sfie <> *zeros and
109600980615     c                             v1sfie <> *blanks
109700980615     c                   move      v1sfie        linea
109800980615     c     kded          chain     fnded01l                           30
109900980615    4c                   if        *in30 = *off
110000980615    5c     v1sann        ifne      ' '
110100980826     c                   eval      dedatb = 'A'
110200980826     c                   update    fnded000
110300980615    5c                   endif
110400980615   x4c                   else
110500980615     c                   clear                   fnded000
110600980615     c                   z-add     v1caa4        dedaae
110700980722     c                   z-add     w1cnev        dednev
110800980615     c                   move      v1sfie        dedfie
110900980615     c                   write     fnded000
111000980615    4c                   endif
111100980615    3c                   endif
111200980615     c                   add       1             nrr
111300980615    2c                   endif
111400980615    1c                   enddo
111500990212     c*
111600990212     c* valorizzo a 'Y' il campo confe per pilotare le trasmissione a fine lavoro
111700990212     c                   eval      confe = 'Y'
111800980615     c*
111900980615     c                   endsr
112000050518      *****************************************************************
112100050518      *   Gestione della fnestra
112200050518      *****************************************************************
112300050518     C     GES_Wind      BEGSR
112400050530
112500050530     c                   movel     KNSIF         T02sif
112600050530     c                   movel     'STD'         T02cod
112700050530     c                   movel(p)  '4'           T02ke1
112800050530     c                   call      'TIBS02R'
112900050530     c                   parm                    KPJBA
113000050530     c                   parm                    DSBS02
113100050530    4c                   if        T02err = *blanks
113200050530     c                   movel     T02uni        dSTD4
113300050530     c                   endif
113400050530
113500050530     c                   eval      *in18 = (§std4flg = 'L')
113600050518
113700050518      * imposto il campo
113800050518     c                   eval      w01stp = 'NO'
113900050518      *
114000050518     c                   exfmt     fi06w01
114100050518      *
114200050518     c                   If        w01stp = 'SI'
114300050519      * stampo la lettera oppure la invio via mail
114400050519     c                   eval      I06aae = V1Caa4
114500050519     c                   eval      I06nev = W1Cnev
114600050519     c                   movel(p)  FIDN06ds      KPJBU
114700050519     c                   call      'FIDN06R1'
114800050519     c                   parm                    KPJBA
114900050518     c                   endif
115000050518
115100050518     c                   endsr
115200981005      *****************************************************************
115300981005      *   RICERCA ESISTENZA CA PER P.O.
115400981005      *****************************************************************
115500981005     C     RIC_CA        BEGSR
115600981005      *
115700981005     c     kdct          chain     fndct03l                           32
115800981005     c  n32              eval      V1SFCA = 'SI'
115900981005     c   32              eval      V1SFCA = '  '
116000981013     c*
116100981013     c* controllo se sono un terminal di partenza e se lo sono recupero la sua £1 e controllando
116200981013     c* se ci sono CA nei 2° livelli
116300981005     c*
116400981013     c                   if        *in32
116500981013     c                   clear                   dslv55
116600981013     c                   movel     'P'           d55tpt
116700981013     c                   movel     dedfie        d55lin
116800981013     c                   z-add     wdatac        d55drf
116900981013     c                   call      'FNLV55R'
117000981013     c                   parm                    dslv55
117100981013      * se non c'è errore e la filiale  è = al  terminal di partenza
117200981013    4c                   if        d55err = ' '    and
117300981013     c                             d55tfp = dedfie
117400981013     c* carico la £1 del terminal
117500981013      ***
117600981013      * CARICO TABELLA £1
117700981013      ***
117800981013     C                   CLEAR                   L1
117900981013     C                   CLEAR                   DSUL06
118000981013     C                   MOVE      '£1'          D06COD
118100981013     C                   MOVEL     dedfie        D06key
118200981013     C                   MOVEL     DSUL06        KPJBU
118300981013     C                   CALL      'TRUL06R'
118400981013     C                   PARM                    KPJBA
118500981013     C                   MOVEL     KPJBU         DSUL06
118600981013     C                   MOVEA     LIN           L1
118700981013     c*
118800981013     c                   z-add     1             x
118900981013     c*
119000981013     c                   dow       l1(x) > *zeros
119100981013     c                   eval      dedfie = l1(x)
119200981013     c     kdct          chain     fndct03l                           32
119300981013     c  n32              eval      V1SFCA = 'SI'
119400981013     c   32              eval      V1SFCA = '  '
119500981013     c  n32              leave
119600981013     c                   add       1             x
119700981013     c                   enddo
119800981013      *
119900981013     c                   endif
120000981013      *
120100981013     c                   endif
120200981005     c                   endsr
120300981005      *
120400980506      *****************************************************************
120500980506      *   ROUTINE INIZIALE
120600980506      *****************************************************************
120700980506     C     *INZSR        BEGSR
120800980506      *
120900980506     C     *ENTRY        PLIST
121000980506     C                   PARM                    KPJBA
121100981005     c* controllo se richiamato dal programma ricerca eventi
121200981005     c                   movel     kpjbu         fidn06ds
121300100427      /free
121400100427         // -?Impostazione nome libreria per il file TITAS30C?
121500100427         //  ?e sua apertura?
121600100427         if  %subst( KNSIF : 7 : 1 ) = 'P';
121700100427           wLibFile = 'GAITRAGRPS/TITAS30C';
121800100427         else;
121900100427           wLibFile = 'GAITRAGRU/TITAS30C';
122000100427         endif;
122100100427         open  TITAS30C;
122200100427      /end-free
122300981005     c* se il campo selezione è = a 5 sono in interrogazione
122400981005     c                   if        i06mod = 'I'
122500981005     c                   eval      *in15 = *on
122600981005     c* valorizzo i campi chiave
122700981005     c                   movel     i06aae        v1caa4
122800981005     c                   movel     i06nev        w1cnev
122900981005     c                   movel     i06nev        vccnev
123000981005     c                   endif
123100980506      *
123200980506     C                   Z-ADD     1             CODUT
123300980506     C                   CALL      'X§PARUT'
123400980507     C                   PARM                    UT§DSE0F
123500980506     C                   MOVEL     RAGUT         RSUT
123600981020     C                   MOVEL     REC80         CNCR80
123700981020
123800980506      *
123900980603     C                   TIME                    W0140            14 0
124000980603     C* UDATE IN GGMMAAAA
124100980603     C                   MOVE      W0140         WDTGIO            8 0
124200980603     C* UDATE IN AAAAMMGG
124300980603     C                   Z-ADD     WDTGIO        G02DAT
124400980603     C                   MOVEL     *BLANK        G02ERR
124500980603     C                   CALL      'XSRDA8'
124600980603     C                   PARM                    WLBDAT
124700980603     C                   MOVEL     G02INV        DATEU             8 0
124800990108     c*
124900990108     c                   if        v1caa4 =  0
125000980603     c                   move      wdtgio        v1caa4
125100990108     c                   endif
125200050701      *
125300050701      * Memorizzazione tab. CCH in schiera per le sole causali di chiusure tecniche
125400050701     C                   clear                   XX
125500050701     C                   clear                   CCH
125600050705     c                   eval      Kcod    = 'CCH'
125700050701     C     Kcod          setll     TNTBE01L
125800050701do  1C                   do        *hival
125900130801     C     Kcod          reade(n)  TNTBE000
126000050701      * - e.o.f.
126100050701if  2C                   if        %eof(TNTBE01L)
126200050701     C                   leave
126300050701e   2C                   endif
126400050701      * - rec. annullato
126500050701if  2C                   if            TBEatb <> *blank
126600050701     C                   iter
126700050701e   2C                   endif
126800050701      * - rec. da non memorizzare
126900050701     C                   movel     TBEuni        DCCH
127000050701if  2C                   if        §CCHchte <> 'S'
127100050701     C                   iter
127200050701e   2C                   endif
127300050701      *
127400050701     C                   add       1             XX
127500050701     C                   movel     TBEke1        CCH(xx)
127600050701e   1C                   enddo
127700050705     c                   eval      Kcod    = 'PAD'
127800990223     c*
127900990223     c* chiave file numeratore eventi
128000990223     c     KTBE          KLIST
128100990223     c                   KFLD                    Kcod
128200990223     c                   KFLD                    Kke1
128300051018      * chiave file tabelle
128400051018     c     K03TABEL      klist
128500051018     c                   kfld                    TBLkut
128600051018     c                   kfld                    TBLcod
128700051018     c                   kfld                    TBLkey
128800990108     c*
128900980603     c* chiave file testata eventi
129000980603     C     Kdet          KLIST
129100980603     C                   KFLD                    v1caa4
129200980722     C                   KFLD                    w1cnev
129300980615     c* chiave file dettaglio  eventi
129400980615     C     Kded          KLIST
129500980615     C                   KFLD                    v1caa4
129600980722     C                   KFLD                    w1cnev
129700980615     c                   kfld                    linea             3 0
129800981005     c* chiave file Testata ca
129900981005     C     Kdct          KLIST
130000981005     C                   KFLD                    dedaae
130100981005     C                   KFLD                    dednev
130200981005     c                   kfld                    dedfie
130300051018      * chiave file FNDPT01L
130400051018     c     K03DPT01      klist
130500051018     c                   kfld                    DCTaac
130600051018     c                   kfld                    DCTfil
130700051018     c                   kfld                    DCTnca
130800051018      * chiave file TITAS30C
130900051018     c     K04TAS30      klist
131000051018     c                   kfld                    DCTaas
131100051018     c                   kfld                    DCTlnp
131200051018     c                   kfld                    DCTnrs
131300051018     c                   kfld                    DCTnsp
131400980615     c* chiave file descrizione
131500980615     C     KDCS          KLIST
131600980615     C                   KFLD                    KTPD
131700980615     C                   KFLD                    KNKT
131800980617     C                   KFLD                    KSTD
131900980617     C                   KFLD                    KDIM
132000980617     C                   KFLD                    KHIM
132100980617     C                   KFLD                    KNKS
132200980617     C                   KFLD                    KTRC
132300990119     c* chiave file descrizione con progressivo
132400990119     C     KDCSPR        KLIST
132500990119     C                   KFLD                    KTPD
132600990119     C                   KFLD                    KNKT
132700990119     C                   KFLD                    KSTD
132800990119     C                   KFLD                    KDIM
132900990119     C                   KFLD                    KHIM
133000990119     C                   KFLD                    KNKS
133100990119     C                   KFLD                    KTRC
133200990119     C                   KFLD                    dcspno
133300980603     C*
133400980506     C                   ENDSR
133500980506      *****************************************************************
133600980508** MSG  (Lungh. 78)                                                          *
133700980602Immettere Anno Corretto                                                            1
133800120515Data segnalazione evento a Chartis errata                                          2
133900120515Data segnalazione evento a Chartis   antecedente alla   Data apertura evento       3
134000980603Inserire numero evento                                                             4
134100980603Numero Evento inesistente                                                          5
134200980603Data apertura evento errata                                                        6
134300980603Punto Operativo errato                                                             7
134400980615Inserire la descrizione dell'evento                                                8
134500980615ERRORE Data Apertura ed Anno evento sono diversi                                   9
134600981020Inserire Tipo anomalia Evento                                                     10
134700981020Anomalia non valida per un EVENTO                                                 11
134800981110Inserire Azienda intestataria Pratica Assicurativa di questo Evento               12
134900990223Altri utenti stanno inserendo nuovi eventi ... Riprovare più tardi                13
135000001227Non si possono attribuire eventi con anno superiore al 2000 alla SDI              14
135100010718ERRORE : esistono C.A. legate all'evento per cambiare Anomalia chiamare il CED    15
