000100080912     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080912      *
000300081031     Ftntl84d   CF   E             WORKSTN
000400080912     f                                     sfile(video2:snrr1)
000500081028     Faiatm01l  uF a E           k disk
000600081028     Faiatm02l  iF   E           k disk    rename(aiatm000:aiatm2)
000700100105     Faiats01l  iF   E           k disk
000800081016     Fanfrn01l  iF   E           k disk
000900081016     Fansog01l  iF   E           k disk
001000081112     Fansif01l  iF   E           k disk
001100080912      *
001200080912     D Psds           SDS
001300080912     D  PgmName          *PROC
001400080912     D Kpjba         E DS
001500081028     d param           ds
001600081028     d  parsoc                        3s 0
001700081028     d  parksc                        7s 0
001800130423     d* Parmod = 1 --> modalità scelta conducente
001900081028     d  parmod                        1
002000130423     d  pardrf                        8
002100130423     d  par_oTRM                     10
002200080912      *
002300080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002400080912     D CNCR80        E DS
002500080912     d Tibs36Ds      e ds
002600080912     d data            ds            10
002700080912     d aa                      3      4
002800080912     d mm                      6      7
002900080912     d gg                      9     10
003000080912      *
003100080912     D fatto           S              1
003200080912     D modalita        S              1
003300080912     D x               S              4  0
003400080912     D WrkEofS01       S              1
003500080912     D WrkCarS01       S              1
003600080912     D $VIDEO          S             10
003700080912     D snrr1           S              5i 0
003800080912     D Savkpjbu        S                   like(kpjbu)
003900080912     D SavOpzione      S                   like(v1csce)
004000080912     d dataiso         s               d   datfmt(*iso)
004100080912     d dataeur         s               d   datfmt(*eur)
004200080912     D WLBDA8          DS
004300080912     D  G02DAT                 1      8  0
004400080912     D  G02INV                 9     16  0
004500080912     D  G02ERR                17     17
004600080912     D  G02TGI                18     22  0
004700130424      *---------------- ----------------------------------------------------
004800130424      * 21 on - modalità interrogazione da menù
004900130424      * 25 on - modalità interrogazione: richiamo per scelta conducente
005000130424      * 26 on - non visulizzo fornitore se non passato
005100130424      *---------------- ----------------------------------------------------
005200081028      * se il programma parte da menu o se richiamato da altro
005300081028     c                   if        richiamato = *blank
005400100105     c                             and atsdtfic = 0
005500130424     c                   seton                                        2126
005600080912      *---------------- Gestione formato video video01----------------------
005700081028      *
005800080912     C                   EXSR      Inzvideo1
005900080912     C                   DOU       $Video <> 'VIDEO1'
006000080912      *----------- Visualizzo dati di output in caso di errori.-------------
006100080912     C                   IF        *IN99
006200080912     C                   EVAL      *IN99 = *OFF
006300080912     C                   WRITE     video1
006400080912     C                   EVAL      *IN99 = *ON
006500080912     C                   ENDIF
006600080912      *
006700080912     c     emetti        tag
006800080912     C                   EXFMT     video1
006900080912      *
007000080912     C                   SELECT
007100080912     C                   WHEN      *INKC
007200080912     C                             OR
007300080912     C                             *INKL
007400080912     C                   EVAL      $Video = *BLANKS
007500080912
007600080912     C                   WHEN      *INKE
007700080912     C                   EXSR      Inzvideo1
007800080912     C                   WHEN      *INKB
007900080912     C                   EXSR      Chkvideo1
008000080912     C                   OTHER
008100080912     C                   EXSR      Chkvideo1
008200080912     c   84              goto      emetti
008300080912     C  N99              EXSR      Wrkvideo2c
008400080912     C                   ENDSL
008500080912     C                   ENDDO
008600081028      *richiamato da altri
008700081028     c                   else
008800100105     c                   if        atsdtfic > 0
008900100105     c                   seton                                        21
009000100105     c                   endif
009100081028     C                   EXSR      Wrkvideo2c
009200081028     c                   endif
009300080912      *
009400080912     C                   EXSR      Uscita
009500080912     c**********************************************************************
009600080912     c* uscita
009700080912     c**********************************************************************
009800080912     C     Uscita        BEGSR
009900130423      *
010000130423      * Se richiamo per la scelta-> passo param in kpjbu
010100130423     c                   if        *in25
010200130423     c                   movel     param         kpjbu
010300130423     c                   endif
010400080912      *
010500080912     C                   EVAL      *INLR = *ON
010600080912     C                   RETURN
010700080912      *
010800080912     C                   ENDSR
010900080912     c**********************************************************************
011000080912     c* gestione sfl
011100080912     c**********************************************************************
011200080912     C     Wrkvideo2c    BEGSR
011300080912      *
011400080912     C                   EVAL      WrkCarS01 = *ON
011500080912     C                   EVAL      $Video = 'VIDEO2C'
011600130522      *
011700130522     c                   if        *in25
011800130522     C                   EXSR      Carvideo2
011900130522     c                   endif
012000130522     c
012100080912      *
012200080912     C                   DOU       $Video <> 'VIDEO2C'
012300130522     c* in caso di scelta, una volta carico non ricarico
012400130522     c                   if        not *in25
012500080912     C                   EXSR      Carvideo2
012600130522     c                   endif
012700130522     c                   if        nrr1=0
012800130522     c                   z-add     1             nrr1
012900130522     c                   endif
013000130522     c
013100080912     c*Descrizione della ricerca
013200080912      *
013300080912     C                   WRITE     video2z
013400080912     C                   EXFMT     video2c
013500080912     C                   SELECT
013600080912     c* fine
013700080912     C                   WHEN      *INKC=*ON
013800080912     C                   EVAL      $VIDEO=*BLANKS
013900080912     c* guida
014000080912     C                   WHEN      *INKL=*ON
014100080912     C                   EVAL      $VIDEO='VIDEO1'
014200080912     c* ripristino
014300080912     C                   WHEN      *INKE=*ON
014400080912     C                   EVAL      WrkCarS01 = *ON
014500080912
014600080912     C                   OTHER
014700080912
014800080912     C                   EXSR      GestioneSFL
014900080912     C                   ENDSL
015000080912
015100080912     C                   ENDDO
015200080912     C                   ENDSR
015300080912     c**********************************************************************
015400080912     c* Controlli video R01.
015500080912     c**********************************************************************
015600080912     C     Chkvideo1     BEGSR
015700080912      *
015800080912     C                   EVAL      *IN99 = *OFF
015900080912     C                   EVAL      *IN84 = *OFF
016000080912      * profilo di filiale
016100080912     c                   if        o36pou <> 046
016200080912     c                   endif
016300080912      *
016400080912     C                   ENDSR
016500080912     c**********************************************************************
016600080912     c* inizializza r01
016700080912     c**********************************************************************
016800080912     C     Inzvideo1     BEGSR
016900080912
017000080912     c                   if        o36pou <> 046
017100081028     c                   move      *blank        vtarga
017200081112     c                   move      'N'           vdis
017300081112     c                   else
017400081112     c                   move      'S'           vdis
017500080912     c                   endif
017600080912     C                   EXSR      Chkvideo1
017700080912
017800080912     C                   ENDSR
017900080912     c**********************************************************************
018000080912     C     opzione       BEGSR
018100080912     c**********************************************************************
018200080912     c                   move      v1csce        modalita
018300081016     c                   movea     '00000000'    *in(50)
018400130606     c                   setoff                                       9637
018500080912     c                   select
018600130423     c                   when      v1csce = '1' and richiamato <> *blank
018700130423     c                             and  *in25
018800130423     c                   eval      par_oTRM=vi2targa
018900130423     c                   eval      $Video=*blanks
019000130423     c
019100081103     c                   when      v1csce = '2' and richiamato <> *blank
019200130423     c                             and not *in25
019300080912     c                   exsr      modifica
019400081103     c                   when      v1csce = '3' and richiamato <> *blank
019500130423     c                             and not *in25
019600080912     c                   exsr      copia
019700080912     c                   when      v1csce = '5'
019800080912     c                   seton                                        21
019900080912     c                   exsr      modifica
020000130423     c                   if        richiamato <> *blank and not *in25
020100080912     c                   setoff                                       21
020200081103     c                   endif
020300130423
020400081103     c                   when      v1csce = '4' and richiamato <> *blank
020500130423     c                             and  not *in25
020600080912     c                   seton                                        21
020700080912     c                   exsr      modifica
020800080912     c                   setoff                                       2128
020900080912     c                   clear                   $msg
021000080912     c                   endsl
021100080912     C                   ENDSR
021200080912     c**********************************************************************
021300080912     C     modifica      BEGSR
021400080912     c**********************************************************************
021500081217     c  n21katm          chain     aiatm01l                           98
021600081217     c   21katm          chain(n)  aiatm01l                           98
021700080912     c                   if        not *in98
021800081028     c                   move      ATmKSC        vATmKSC
021900081028     c                   move      ATmSOC        vATmSOC
022000081112     c                   move      atmsoc        socalfa           3
022100081112     c                   clear                   vatmsocd
022200081112     c     socalfa       chain     ansif01l
022300081112     c                   if        %found(ansif01l)
022400081112     c                   movel     sifrgs        vatmsocd
022500081112     c                   endif
022600081028     c                   move      ATmtarga      vATmtarga
022700081028     c                   move      ATMTARGA      vATMTARGA
022800081028     c                   move      ATMCTER       vATMCTER
022900081028     c                   move      ATMISCRA      vATMISCRA
023000081028     c                   move      ATMTIPO       vATMTIPO
023100081028     c                   move      ATMANNO       vATMANNO
023200081028     c                   move      ATmassic      vATmassic
023300081028     c                   move      ATMANTIF      vATMANTIF
023400081028     c                   move      ATMFOTO       vATMFOTO
023500080912      *data inizio contratto
023600081028     c                   if        atmdtinr > 0
023700081028     c                   move      atmdtinr      dataiso
023800080912     c                   move      dataiso       dataeur
023900081028     c                   move      dataeur       vatmdtinr
024000080912     c                   else
024100081028     c                   clear                   vatmdtinr
024200080912     c                   endif
024300080912      *data fine contratto
024400081028     c                   if        atmdtfir > 0
024500081028     c                   move      atmdtfir      dataiso
024600080912     c                   move      dataiso       dataeur
024700081028     c                   move      dataeur       vatmdtfir
024800081016     c                   else
024900081028     c                   clear                   vatmdtfir
025000081016     c                   endif
025100081028      *data controllo
025200081028     c                   if        atmdtcon > 0
025300081028     c                   move      atmdtcon      dataiso
025400081028     c                   move      dataiso       dataeur
025500081028     c                   move      dataeur       vatmdtcon
025600081028     c                   else
025700081028     c                   clear                   vatmdtcon
025800081028     c                   endif
025900081028      *data scadenza RCA
026000081028     c                   if        atmdtrca > 0
026100081028     c                   move      atmdtrca      dataiso
026200081028     c                   move      dataiso       dataeur
026300081028     c                   move      dataeur       vatmdtrca
026400081028     c                   else
026500081028     c                   clear                   vatmdtrca
026600081028     c                   endif
026700081103     c   21              exsr      controv3
026800080912     c                   exsr      Gesvideo3
026900080912     c                   endif
027000080912     C                   ENDSR
027100080912     c**********************************************************************
027200080912     C     copia         BEGSR
027300080912     c**********************************************************************
027301130606     c                   seton                                        37
027303130606     c     katm          chain(n)  aiatm01l                           98
027304130606     c                   if        not *in98
027305130606     c                   clear                   vATmKSC
027306130611     c                   clear                   vATmrags
027307130606     c                   move      ATmSOC        vATmSOC
027308130606     c                   move      atmsoc        socalfa           3
027309130606     c                   clear                   vatmsocd
027310130606     c     socalfa       chain     ansif01l
027311130606     c                   if        %found(ansif01l)
027312130606     c                   movel     sifrgs        vatmsocd
027313130606     c                   endif
027314130606     c                   move      ATmtarga      vATmtarga
027315130606     c                   move      ATMTARGA      vATMTARGA
027316130606     c                   move      ATMCTER       vATMCTER
027317130606     c                   move      ATMISCRA      vATMISCRA
027318130606     c                   move      ATMTIPO       vATMTIPO
027319130606     c                   move      ATMANNO       vATMANNO
027320130606     c                   move      ATmassic      vATmassic
027321130606     c                   move      ATMANTIF      vATMANTIF
027322130606     c                   move      ATMFOTO       vATMFOTO
027323130606      *data inizio contratto
027324130606     c                   if        atmdtinr > 0
027325130606     c                   move      atmdtinr      dataiso
027326130606     c                   move      dataiso       dataeur
027327130606     c                   move      dataeur       vatmdtinr
027328130606     c                   else
027329130606     c                   clear                   vatmdtinr
027330130606     c                   endif
027331130606      *data fine contratto
027332130606     c                   if        atmdtfir > 0
027333130606     c                   move      atmdtfir      dataiso
027334130606     c                   move      dataiso       dataeur
027335130606     c                   move      dataeur       vatmdtfir
027336130606     c                   else
027337130606     c                   clear                   vatmdtfir
027338130606     c                   endif
027339130606      *data controllo
027340130606     c                   if        atmdtcon > 0
027341130606     c                   move      atmdtcon      dataiso
027342130606     c                   move      dataiso       dataeur
027343130606     c                   move      dataeur       vatmdtcon
027344130606     c                   else
027345130606     c                   clear                   vatmdtcon
027346130606     c                   endif
027347130606      *data scadenza RCA
027348130606     c                   if        atmdtrca > 0
027349130606     c                   move      atmdtrca      dataiso
027350130606     c                   move      dataiso       dataeur
027351130606     c                   move      dataeur       vatmdtrca
027352130606     c                   else
027353130606     c                   clear                   vatmdtrca
027354130606     c                   endif
027355130606     c   21              exsr      controv3
027356130606     c                   exsr      Gesvideo3
027357130606     c                   endif
027400080912     C                   ENDSR
027500080912     c**********************************************************************
027600080912     C     gesvideo3     BEGSR
027700080912     c**********************************************************************
027800080912     c                   clear                   fatto
027900080912     c                   do        *hival
028000080912     c                   exfmt     video3
028100080912     c   kl              leave
028200081103     c  n21              exsr      controv3
028300080912     c   96              iter
028400080912     c   kf              exsr      aggiorna
028500080912     c                   if        fatto <> *blank
028600080912     c                   leave
028700080912     c                   endif
028800080912
028900080912     c                   enddo
029000080912     C                   ENDSR
029100080912     c**********************************************************************
029200080912     C     Controv3      BEGSR
029300080912     c**********************************************************************
029400080912      *
029500080912      * *in96 errore generico videata dettaglio
029600080912     c                   setoff                                       9628
029700080912     c                   clear                   $msg
029800080912     c                   if        modalita ='4'
029900080912     c                   goto      endctr
030000080912     c                   endif
030100081028      *fornitore
030200130606     c                   setoff                                       595458
030201130606     c                   if        vatmtarga =  *blank
030202130606     c                   seton                                        5896
030203130606     c                   endif
030204130606     c                   if        modalita = '3'
030205130606     c                   if        vi2ksc  =  vatmksc
030206130606     c                   seton                                        5596
030207130606     c                   endif
030208130606     c     katmr3        chain     aiats01l
030209130606     c                   if        not%found(aiats01l)
030210130606     c                   seton                                        5996
030211130606     c                   else
030212130606     c     katmr         chain     aiats01l
030213130606     c                   endif
030214130606     c     katm3         chain     aiatm01l
030215130606     c                   if        %found(aiatm01l)
030216130606     c                   seton                                        5496
030217130606     c                   endif
030218130606     c                   endif
030300081028     c                   movel     vatmsoc       societa
030400081016     c                   movel     *zeros        codice
030500081028     c                   move      vatmksc       codice
030600081016     c     kfrn          chain     anfrn01l
030700081016     c                   if        not%found(anfrn01l)
030800081016     c                   seton                                        5996
030900081016     c                   else
031000081016     c     frnsogg       chain     ansog01l
031100081028     c                   movel     sogdes        vatmrags
031200081016     c                   endif
031300081016     c
031400081028      *targa
031500081028     c                   setoff                                       58
031600081028     c                   if        vatmtarga =  *blank
031700081028     c                   seton                                        5896
031800081028     c                   endif
031900090108      *tipo
032000090108     c                   setoff                                       55
032100090108     c                   if        vatmtipo =  *blank
032200090108     c                   seton                                        5596
032300090108     c                   endif
032400081016      *data inizio rapporto
032500090109     c                   setoff                                       56
032600081028     c                   if        vatmdtinr =  *zeros
032700090109     c                   seton                                        5696
032800080912     c                   endif
032900081016      *controlli data  inizio rapporto
033000080912     c                   setoff                                       50
033100081028     c                   if        vatmdtinr > 0
033200081028     C                   MOVE      Vatmdtinr     G02DAT
033300081016     C                   MOVEL     *BLaNK        G02ERR
033400080912     C                   CALL      'XSRDA8'
033500080912     C                   PARM                    WLBDA8
033600080912     C     G02ERR        IFEQ      '1'
033700080912     C                   SETON                                        5096
033800080912     C                   END
033900081028     c                   move      g02dat        vatmdtinr
034000081028     c                   move      g02inv        vatmdtinrg        8 0
034100080912     c                   endif
034200081016      *controlli data  fine rapporto
034300081016     c                   setoff                                       51
034400081028     c                   if        vatmdtfir > 0
034500081028     C                   MOVE      Vatmdtfir     G02DAT
034600080912     C                   MOVEL     *BLANK        G02ERR
034700080912     C                   CALL      'XSRDA8'
034800080912     C                   PARM                    WLBDA8
034900080912     C     G02ERR        IFEQ      '1'
035000081016     C                   SETON                                        5196
035100080912     C                   END
035200081028     c                   move      g02dat        vatmdtfir
035300081028     c                   move      g02inv        vatmdtfirg        8 0
035400080912     c                   endif
035500090108      *controllo campi s/n
035600090108     c                   setoff                                       31
035700090108     c                   if        vatmcter = *blank
035800090108     c                   seton                                        3196
035900090108     c                   endif
036000090108     c                   setoff                                       32
036100090108     c                   if        vatmiscra = *blank
036200090108     c                   seton                                        3296
036300090108     c                   endif
036400090108     c                   setoff                                       33
036500090108     c                   if        vatmassic = *blank
036600090108     c                   seton                                        3396
036700090108     c                   endif
036800090108     c                   setoff                                       34
036900090108     c                   if        vatmfoto = *blank
037000090108     c                   seton                                        3496
037100090108     c                   endif
037200081028      *data controllo
037300081028     c                   setoff                                       53
037400081028     c                   if        vatmdtcon > 0
037500081028     C                   MOVE      Vatmdtcon     G02DAT
037600081016     C                   MOVEL     *BLANK        G02ERR
037700081016     C                   CALL      'XSRDA8'
037800081016     C                   PARM                    WLBDA8
037900081016     C     G02ERR        IFEQ      '1'
038000081028     C                   SETON                                        5396
038100081016     C                   END
038200081028     c                   move      g02dat        vatmdtcon
038300081028     c                   move      g02inv        vatmdtcong        8 0
038400081016     c                   endif
038500081028      *data rca
038600081028     c                   setoff                                       57
038700081028     c                   if        vatmdtrca > 0
038800081028     C                   MOVE      Vatmdtrca     G02DAT
038900081028     C                   MOVEL     *BLANK        G02ERR
039000081028     C                   CALL      'XSRDA8'
039100081028     C                   PARM                    WLBDA8
039200081028     C     G02ERR        IFEQ      '1'
039300081028     C                   SETON                                        5796
039400081028     C                   END
039500081028     c                   move      g02dat        vatmdtrca
039600081028     c                   move      g02inv        vatmdtrcag        8 0
039700081028     c                   endif
039800081028      *controlli inizio validità
039900080912     C     endctr        ENDSR
040000080912     c**********************************************************************
040100080912     C     aggiorna      BEGSR
040200080912     c**********************************************************************
040300081028     c                   move      vatmtipo      atmtipo
040400081215     c                   move      vATMANTIF     ATMANTIF
040500081028     c                   move      vATMCTER      ATMCTER
040600081028     c                   move      vATMISCRA     ATMISCRA
040700081028     c                   move      vATMANNO      ATMANNO
040800081028     c                   move      vATMAssic     ATMassic
040900081028     c                   move      vATMfoto      ATMfoto
041000081028     c                   if        vatmdtinr > 0
041100081028     c                   move      vatmdtinr     dataeur
041200080912     c                   move      dataeur       dataiso
041300081028     c                   move      dataiso       atmdtinr
041400080912     c                   endif
041500081028     c                   if        vatmdtfir > 0
041600081028     c                   move      vatmdtfir     dataeur
041700081016     c                   move      dataeur       dataiso
041800081028     c                   move      dataiso       atmdtfir
041900090109     c                   else
042000090109     c                   z-add     0             atmdtfir
042100081016     c                   endif
042200081028     c                   if        vatmdtcon > 0
042300081028     c                   move      vatmdtcon     dataeur
042400081028     c                   move      dataeur       dataiso
042500081028     c                   move      dataiso       atmdtcon
042600090109     c                   else
042700090109     c                   z-add     0             atmdtcon
042800081028     c                   endif
042900081028     c                   if        vatmdtrca > 0
043000081028     c                   move      vatmdtrca     dataeur
043100081028     c                   move      dataeur       dataiso
043200081028     c                   move      dataiso       atmdtrca
043300090109     c                   else
043400090109     c                   z-add     0             atmdtrca
043500081028     c                   endif
043600080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
043700080912     c                   select
043800080912     c                   when      modalita = '4'
043900081028     c                   delete    aiatm000
044000080912     c                   when      modalita = *blank or modalita = '3'
044100081028     c                   move      vATmKSC       ATmKSC
044200081028     c                   move      vATmSOC       ATmSOC
044300081028     c                   move      vATmtarga     ATmtarga
044400081028     c                   write     aiatm000
044500080912     c                   other
044600081028     c                   update    aiatm000
044700080912     c                   endsl
044800080912     c                   move      'X'           fatto
044900080912     C                   ENDSR
045000080912     c**********************************************************************
045100080912     c* carica sfl
045200080912     c**********************************************************************
045300080912     C     Carvideo2     BEGSR
045400080912     C                   IF        WrkCarS01 = *ON
045500080912     C                   EVAL      snrr1   = 0
045600080912     C                   EVAL      *IN90 = *ON
045700080912     C                   EVAL      *IN91 = *OFF
045800080912     C                   WRITE     video2c
045900080912     C                   EVAL      *IN90 = *OFF
046000081029      * imposto lettura se programma richiamato oppure se parte da menu
046100081029     c                   if        richiamato = *blank
046200100105     c                   if        atsdtfic > 0
046300100105     c     katmr         setll     aiatm01l
046400100105     c                   else
046500081028     c     vtarga        setll     aiatm02l
046600100105     c                   endif
046700081029     c                   else
046800081029     c     katmr         setll     aiatm01l
046900081029     c                   endif
047000080912     c                   do        *hival
047100081029     c                   if        richiamato = *blank
047200100105     c                   if        atsdtfic > 0
047300100105     c     katmr         reade(n)  aiatm01l                               97
047400100105     c                   else
047500081029     c                   read      aiatm02l                               97
047600100105     c                   endif
047700081029     c                   else
047800081029     c     katmr         reade(n)  aiatm01l                               97
047900081029     c                   endif
048000081029     c   97              leave
048100081112      * escludo disaccreditati
048200081112     c                   if        vdis = 'N' and atmdtfir > 0
048300081112     c                   iter
048400081112     c                   endif
048500080912     c                   move      *blanks       v1csce
048600081028     c                   move      atmSOC        VI2SOC
048700081028     c                   move      atmKSC        VI2KSC
048800081028     c                   move      atmtarga      VI2targa
048900081028     c                   move      atmtipo       VI2tipo
049000081016      *data inizio contratto
049100081028     c                   if        atmdtinr> 0
049200081028     c                   move      atmdtinr      dataiso
049300080912     c                   move      dataiso       dataeur
049400081016     c                   move      dataeur       vi2dtinr
049500080912     c                   else
049600081016     c                   clear                   vi2dtinr
049700080912     c                   endif
049800081028      *data fine contratto
049900081028     c                   if        atmdtfir> 0
050000081028     c                   move      atmdtfir      dataiso
050100081028     c                   move      dataiso       dataeur
050200081028     c                   move      dataeur       vi2dtfir
050300081028     c                   else
050400081028     c                   clear                   vi2dtfir
050500081028     c                   endif
050600081028      *data controllo
050700081028     c                   if        atmdtcon> 0
050800081028     c                   move      atmdtcon      dataiso
050900081028     c                   move      dataiso       dataeur
051000081028     c                   move      dataeur       vi2dtcon
051100081028     c                   else
051200081028     c                   clear                   vi2dtcon
051300081028     c                   endif
051400080912      *esco per raggiunta massima capacità sfl
051500080912     c                   if        snrr1 > 9990
051600080912     c                   leave
051700080912     c                   endif
051800080912
051900080912     C                   EVAL      snrr1 = snrr1 + 1
052000080912     C                   WRITE     video2
052100080912     C                   ENDDO
052200080912
052300080912     C                   IF        snrr1  > 0
052400080912     C                   EVAL      *IN91 = *ON
052500080912     C                   MOVE      1             OK                1 0
052600080912     C                   ELSE
052700080912     C                   MOVE      0             OK                1 0
052800080912     C                   ENDIF
052900080912     C                   EVAL      nrr1  = 1
053000080912     C                   ENDIF
053100081215     C                   EVAL      WrkCarS01 = *On
053200080912     C                   EVAL      snrr1  = 1
053300080912     C                   EVAL      csrrrn = 1
053400130522     c                   clear                   vi2posiz
053500080912     C                   ENDSR
053600080912     c**********************************************************************
053700080912     c* gestione sfl
053800080912     c**********************************************************************
053900080912     C     GestioneSFL   BEGSR
054000080912      *
054100080912     C                   EVAL      WrkEofS01 = *OFF
054200080912      * F10 Immissione.
054300081103     C                   IF        *INKJ and richiamato <> *blank
054400080912     c                   clear                   modalita
054500080912     c                   clear                   video3
054600081028     c                   clear                   aiatm000
054700081217     c                   seton                                        24
054800081028     c                   z-add     vi2soc        vatmsoc
054900081028     c                   z-add     vi2ksc        vatmksc
055000080912     c                   move      knmus         vknmus
055100080912     c                   move      knsif         vknsif
055200080912     c                   movel     ragut         rsut
055300080912     c                   exsr      Gesvideo3
055400081217     c                   setoff                                       24
055500080912     c                   eval      wrkcars01 = *on
055600080912     c                   if        csrrrn = 0
055700080912     c                   goto      noriga
055800080912     c                   endif
055900080912     c                   endif
056000080912      * Elaborazione opzioni.
056100080912     c*
056200130522     c                   clear                   nrr1
056300080912     c                   do        *hival        X                 4 0
056400080912     c     X             CHAIN     video2                             50
056500080912     C   50              LEAVE
056600080912     C                   IF        v1csce <> *blanks
056700080912     c                   exsr      opzione
056800080912
056900080912     c                   move      *blank        v1csce
057000080912     c                   update    video2
057100130423     c
057200130423     c* se ho effettuato una scelta esco dal pgm
057300130423     c                   if        *in25 and par_oTRM<>*blanks
057400130423     c                   leavesr
057500130423     c                   endif
057600130522     c* se ho scritto posizionamento --> eseguo
057700130522     c                   else
057800130522     c
057900130522     c                   if        vi2posiz<>*blanks and vi2posiz<=vi2targa
058000130522     c                             and nrr1=0
058100130522     c                   eval      nrr1=x
058200130522     c                   endif
058300130522     c
058400130522     c                   if        vi2posiz=*blanks
058500130522     c                   eval      nrr1=csrrrn
058600130522     c                   endif
058700080912
058800080912     C                   ENDIF
058900080912     C                   enddo
059000080912      *
059100080912     c                   if        modalita = '1' or modalita = '3'
059200080912     c                             or modalita = '4'
059300080912     c                   eval      wrkcars01 = *on
059400080912     c                   endif
059500080912      *
059600080912     C     noriga        ENDSR
059700080912     c**********************************************************************
059800080912     C     *INZSR        BEGSR
059900080912     c**********************************************************************
060000080912      *
060100080912     C     *ENTRY        PLIST
060200080912     C                   PARM                    Kpjba
060300081028      *richiamato da altri
060400081028     c                   clear                   param
060500130423     c                   clear                   amg_drf
060600081028     c                   if        kpjbu <> *blank
060700100105      *
060800081028     c                   movel     kpjbu         param
060900081028     C                   z-add     parsoc        vi2soc
061000081028     C                   z-add     parksc        vi2ksc
061100081028     c                   movel     'X'           richiamato        1
061200100105     c     katmr         chain     aiats01l
061300100105     c                   if        %found(aiats01l)
061400100105     c                   if        atsdtfic > 0
061500100105     c                   move      *blank        richiamato
061600100105     c                   endif
061700100105     c                   endif
061800130424     C
061900130424     c* Se non passato fornitore o non più accreditato -> non lo visualizzo
062000130424     c                   if        richiamato=*blanks or not %found(aiats01l)
062100130424     c                   seton                                        26
062200130424     c                   endif
062300130424      *
062400130423      *
062500130423      * Se richiamato in modalità scelta --> emetto "1"
062600130423     c                   if        parmod='1'
062700130423     c                   seton                                        2521
062800130423     c                   clear                   par_oTRM
062900130423     c                   eval      vdis='N'
063000130423     c                   endif
063100130423     c
063200130423     C* Se data riferimento piena --> la uso per caricare
063300130423     c                   if        pardrf>*zeros
063400130423     c                   movel     pardrf        AMG_drf           8 0
063500130423     c                   endif
063600100105      *
063700081028     c                   endif
063800080912     C                   Z-ADD     1             CODUT
063900080912     C                   CALL      'X§PARUT'
064000080912     C                   PARM                    UTEDSE
064100080912     C                   MOVEL     REC80         CNCR80
064200080912     C                   MOVEL     Ragut         rsut
064300080912     C                   MOVEL     knsif         vknsif
064400080912     C                   MOVEL     knmus         vknmus
064500080912      *reperimento data
064600080912     C                   TIME                    W0120            14 0
064700080912     C                   MOVE      W0120         WDAT              8 0
064800080912     C*
064900080912     C                   Z-ADD     WDAT          G02DAT
065000080912     C                   MOVEL     *BLANK        G02ERR
065100080912     C                   CALL      'XSRDA8'
065200080912     C                   PARM                    WLBDA8
065300080912     C* UDATE A 8 IN AAAA/MM/GG
065400080912     C                   Z-ADD     G02INV        WUDA              8 0
065500080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
065600080912     C                   CLEAR                   Tibs36Ds
065700080912     C                   EVAL      I36ute = knmus
065800080912     C                   CALL      'TIBS36R'
065900080912     C                   PARM                    tibs36ds
066000081112      * profilo di filiale
066100081112     c                   if        o36pou <> 046
066200081112     c                   seton                                        30
066300081112     c                   endif
066400081029      *decodifica fornitore
066500081029     c                   movel     vi2soc        societa
066600081029     c                   movel     *zeros        codice
066700081029     c                   move      vi2ksc        codice
066800081029     c     kfrn          chain     anfrn01l
066900081029     c                   if        not%found(anfrn01l)
067000081029     c                   seton                                        5996
067100081029     c                   else
067200081029     c     frnsogg       chain     ansog01l
067300081029     c                   movel     sogdes        vi2rasd
067400081029     c                   endif
067500080912
067600081016     C     Kfrn          KLIST
067700081016     C                   KFLD                    societa           3
067800081016     C                   KFLD                    codice            8
067900080912      *
068000081028     C     Katm          KLIST
068100081016     C                   KFLD                    vi2soc
068200081016     C                   KFLD                    vi2ksc
068300081028     C                   KFLD                    vi2targa
068400081029     C     Katmr         KLIST
068500081029     C                   KFLD                    vi2soc
068600081029     C                   KFLD                    vi2ksc
068601130606     C     Katmr3        KLIST
068602130606     C                   KFLD                    vi2soc
068603130606     C                   KFLD                    vatmksc
068604130606     C     Katm3         KLIST
068605130606     C                   KFLD                    vi2soc
068606130606     C                   KFLD                    vatmksc
068607130606     C                   KFLD                    vatmtarga
068700080912      *---------------------------------------------------------------------
068800080912     C                   ENDSR
