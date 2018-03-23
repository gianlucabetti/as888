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
027400130606     c                   seton                                        37
027500130606     c     katm          chain(n)  aiatm01l                           98
027600130606     c                   if        not *in98
027700130606     c                   clear                   vATmKSC
027800130611     c                   clear                   vATmrags
027900130606     c                   move      ATmSOC        vATmSOC
028000130606     c                   move      atmsoc        socalfa           3
028100130606     c                   clear                   vatmsocd
028200130606     c     socalfa       chain     ansif01l
028300130606     c                   if        %found(ansif01l)
028400130606     c                   movel     sifrgs        vatmsocd
028500130606     c                   endif
028600130606     c                   move      ATmtarga      vATmtarga
028700130606     c                   move      ATMTARGA      vATMTARGA
028800130606     c                   move      ATMCTER       vATMCTER
028900130606     c                   move      ATMISCRA      vATMISCRA
029000130606     c                   move      ATMTIPO       vATMTIPO
029100130606     c                   move      ATMANNO       vATMANNO
029200130606     c                   move      ATmassic      vATmassic
029300130606     c                   move      ATMANTIF      vATMANTIF
029400130606     c                   move      ATMFOTO       vATMFOTO
029500130606      *data inizio contratto
029600130606     c                   if        atmdtinr > 0
029700130606     c                   move      atmdtinr      dataiso
029800130606     c                   move      dataiso       dataeur
029900130606     c                   move      dataeur       vatmdtinr
030000130606     c                   else
030100130606     c                   clear                   vatmdtinr
030200130606     c                   endif
030300130606      *data fine contratto
030400130606     c                   if        atmdtfir > 0
030500130606     c                   move      atmdtfir      dataiso
030600130606     c                   move      dataiso       dataeur
030700130606     c                   move      dataeur       vatmdtfir
030800130606     c                   else
030900130606     c                   clear                   vatmdtfir
031000130606     c                   endif
031100130606      *data controllo
031200130606     c                   if        atmdtcon > 0
031300130606     c                   move      atmdtcon      dataiso
031400130606     c                   move      dataiso       dataeur
031500130606     c                   move      dataeur       vatmdtcon
031600130606     c                   else
031700130606     c                   clear                   vatmdtcon
031800130606     c                   endif
031900130606      *data scadenza RCA
032000130606     c                   if        atmdtrca > 0
032100130606     c                   move      atmdtrca      dataiso
032200130606     c                   move      dataiso       dataeur
032300130606     c                   move      dataeur       vatmdtrca
032400130606     c                   else
032500130606     c                   clear                   vatmdtrca
032600130606     c                   endif
032700130606     c   21              exsr      controv3
032800130606     c                   exsr      Gesvideo3
032900130606     c                   endif
033000080912     C                   ENDSR
033100080912     c**********************************************************************
033200080912     C     gesvideo3     BEGSR
033300080912     c**********************************************************************
033400080912     c                   clear                   fatto
033500080912     c                   do        *hival
033600080912     c                   exfmt     video3
033700080912     c   kl              leave
033800081103     c  n21              exsr      controv3
033900080912     c   96              iter
034000080912     c   kf              exsr      aggiorna
034100080912     c                   if        fatto <> *blank
034200080912     c                   leave
034300080912     c                   endif
034400080912
034500080912     c                   enddo
034600080912     C                   ENDSR
034700080912     c**********************************************************************
034800080912     C     Controv3      BEGSR
034900080912     c**********************************************************************
035000080912      *
035100080912      * *in96 errore generico videata dettaglio
035200141001     c                   setoff                                       962848
035300080912     c                   clear                   $msg
035400080912     c                   if        modalita ='4'
035500080912     c                   goto      endctr
035600080912     c                   endif
035700141001     c                   if        modalita = *blank
035800141001     c     katm3         chain     aiatm01l
035900141001     c                   if        %found(aiatm01l)
036000141001     c                   seton                                        9648
036100141001     c                   leavesr
036200141001     c                   endif
036300141001     c                   endif
036400081028      *fornitore
036500130606     c                   setoff                                       595458
036600130606     c                   if        vatmtarga =  *blank
036700130606     c                   seton                                        5896
036800130606     c                   endif
036900130606     c                   if        modalita = '3'
037000130606     c                   if        vi2ksc  =  vatmksc
037100130606     c                   seton                                        5596
037200130606     c                   endif
037300130606     c     katmr3        chain     aiats01l
037400130606     c                   if        not%found(aiats01l)
037500130606     c                   seton                                        5996
037600130606     c                   else
037700130606     c     katmr         chain     aiats01l
037800130606     c                   endif
037900130606     c     katm3         chain     aiatm01l
038000130606     c                   if        %found(aiatm01l)
038100130606     c                   seton                                        5496
038200130606     c                   endif
038300130606     c                   endif
038400081028     c                   movel     vatmsoc       societa
038500081016     c                   movel     *zeros        codice
038600081028     c                   move      vatmksc       codice
038700081016     c     kfrn          chain     anfrn01l
038800081016     c                   if        not%found(anfrn01l)
038900081016     c                   seton                                        5996
039000081016     c                   else
039100081016     c     frnsogg       chain     ansog01l
039200081028     c                   movel     sogdes        vatmrags
039300081016     c                   endif
039400081016     c
039500081028      *targa
039600081028     c                   setoff                                       58
039700081028     c                   if        vatmtarga =  *blank
039800081028     c                   seton                                        5896
039900081028     c                   endif
040000090108      *tipo
040100090108     c                   setoff                                       55
040200090108     c                   if        vatmtipo =  *blank
040300090108     c                   seton                                        5596
040400090108     c                   endif
040500081016      *data inizio rapporto
040600090109     c                   setoff                                       56
040700081028     c                   if        vatmdtinr =  *zeros
040800090109     c                   seton                                        5696
040900080912     c                   endif
041000081016      *controlli data  inizio rapporto
041100080912     c                   setoff                                       50
041200081028     c                   if        vatmdtinr > 0
041300081028     C                   MOVE      Vatmdtinr     G02DAT
041400081016     C                   MOVEL     *BLaNK        G02ERR
041500080912     C                   CALL      'XSRDA8'
041600080912     C                   PARM                    WLBDA8
041700080912     C     G02ERR        IFEQ      '1'
041800080912     C                   SETON                                        5096
041900080912     C                   END
042000081028     c                   move      g02dat        vatmdtinr
042100081028     c                   move      g02inv        vatmdtinrg        8 0
042200080912     c                   endif
042300081016      *controlli data  fine rapporto
042400081016     c                   setoff                                       51
042500081028     c                   if        vatmdtfir > 0
042600081028     C                   MOVE      Vatmdtfir     G02DAT
042700080912     C                   MOVEL     *BLANK        G02ERR
042800080912     C                   CALL      'XSRDA8'
042900080912     C                   PARM                    WLBDA8
043000080912     C     G02ERR        IFEQ      '1'
043100081016     C                   SETON                                        5196
043200080912     C                   END
043300081028     c                   move      g02dat        vatmdtfir
043400081028     c                   move      g02inv        vatmdtfirg        8 0
043500080912     c                   endif
043600090108      *controllo campi s/n
043700090108     c                   setoff                                       31
043800090108     c                   if        vatmcter = *blank
043900090108     c                   seton                                        3196
044000090108     c                   endif
044100090108     c                   setoff                                       32
044200090108     c                   if        vatmiscra = *blank
044300090108     c                   seton                                        3296
044400090108     c                   endif
044500090108     c                   setoff                                       33
044600090108     c                   if        vatmassic = *blank
044700090108     c                   seton                                        3396
044800090108     c                   endif
044900090108     c                   setoff                                       34
045000090108     c                   if        vatmfoto = *blank
045100090108     c                   seton                                        3496
045200090108     c                   endif
045300081028      *data controllo
045400081028     c                   setoff                                       53
045500081028     c                   if        vatmdtcon > 0
045600081028     C                   MOVE      Vatmdtcon     G02DAT
045700081016     C                   MOVEL     *BLANK        G02ERR
045800081016     C                   CALL      'XSRDA8'
045900081016     C                   PARM                    WLBDA8
046000081016     C     G02ERR        IFEQ      '1'
046100081028     C                   SETON                                        5396
046200081016     C                   END
046300081028     c                   move      g02dat        vatmdtcon
046400081028     c                   move      g02inv        vatmdtcong        8 0
046500081016     c                   endif
046600081028      *data rca
046700081028     c                   setoff                                       57
046800081028     c                   if        vatmdtrca > 0
046900081028     C                   MOVE      Vatmdtrca     G02DAT
047000081028     C                   MOVEL     *BLANK        G02ERR
047100081028     C                   CALL      'XSRDA8'
047200081028     C                   PARM                    WLBDA8
047300081028     C     G02ERR        IFEQ      '1'
047400081028     C                   SETON                                        5796
047500081028     C                   END
047600081028     c                   move      g02dat        vatmdtrca
047700081028     c                   move      g02inv        vatmdtrcag        8 0
047800081028     c                   endif
047900081028      *controlli inizio validità
048000080912     C     endctr        ENDSR
048100080912     c**********************************************************************
048200080912     C     aggiorna      BEGSR
048300080912     c**********************************************************************
048400081028     c                   move      vatmtipo      atmtipo
048500081215     c                   move      vATMANTIF     ATMANTIF
048600081028     c                   move      vATMCTER      ATMCTER
048700081028     c                   move      vATMISCRA     ATMISCRA
048800081028     c                   move      vATMANNO      ATMANNO
048900081028     c                   move      vATMAssic     ATMassic
049000081028     c                   move      vATMfoto      ATMfoto
049100081028     c                   if        vatmdtinr > 0
049200081028     c                   move      vatmdtinr     dataeur
049300080912     c                   move      dataeur       dataiso
049400081028     c                   move      dataiso       atmdtinr
049500080912     c                   endif
049600081028     c                   if        vatmdtfir > 0
049700081028     c                   move      vatmdtfir     dataeur
049800081016     c                   move      dataeur       dataiso
049900081028     c                   move      dataiso       atmdtfir
050000090109     c                   else
050100090109     c                   z-add     0             atmdtfir
050200081016     c                   endif
050300081028     c                   if        vatmdtcon > 0
050400081028     c                   move      vatmdtcon     dataeur
050500081028     c                   move      dataeur       dataiso
050600081028     c                   move      dataiso       atmdtcon
050700090109     c                   else
050800090109     c                   z-add     0             atmdtcon
050900081028     c                   endif
051000081028     c                   if        vatmdtrca > 0
051100081028     c                   move      vatmdtrca     dataeur
051200081028     c                   move      dataeur       dataiso
051300081028     c                   move      dataiso       atmdtrca
051400090109     c                   else
051500090109     c                   z-add     0             atmdtrca
051600081028     c                   endif
051700080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
051800080912     c                   select
051900080912     c                   when      modalita = '4'
052000081028     c                   delete    aiatm000
052100080912     c                   when      modalita = *blank or modalita = '3'
052200081028     c                   move      vATmKSC       ATmKSC
052300081028     c                   move      vATmSOC       ATmSOC
052400081028     c                   move      vATmtarga     ATmtarga
052500081028     c                   write     aiatm000
052600080912     c                   other
052700081028     c                   update    aiatm000
052800080912     c                   endsl
052900080912     c                   move      'X'           fatto
053000080912     C                   ENDSR
053100080912     c**********************************************************************
053200080912     c* carica sfl
053300080912     c**********************************************************************
053400080912     C     Carvideo2     BEGSR
053500080912     C                   IF        WrkCarS01 = *ON
053600080912     C                   EVAL      snrr1   = 0
053700080912     C                   EVAL      *IN90 = *ON
053800080912     C                   EVAL      *IN91 = *OFF
053900080912     C                   WRITE     video2c
054000080912     C                   EVAL      *IN90 = *OFF
054100081029      * imposto lettura se programma richiamato oppure se parte da menu
054200081029     c                   if        richiamato = *blank
054300100105     c                   if        atsdtfic > 0
054400100105     c     katmr         setll     aiatm01l
054500100105     c                   else
054600081028     c     vtarga        setll     aiatm02l
054700100105     c                   endif
054800081029     c                   else
054900081029     c     katmr         setll     aiatm01l
055000081029     c                   endif
055100080912     c                   do        *hival
055200081029     c                   if        richiamato = *blank
055300100105     c                   if        atsdtfic > 0
055400100105     c     katmr         reade(n)  aiatm01l                               97
055500100105     c                   else
055600081029     c                   read      aiatm02l                               97
055700100105     c                   endif
055800081029     c                   else
055900081029     c     katmr         reade(n)  aiatm01l                               97
056000081029     c                   endif
056100081029     c   97              leave
056200081112      * escludo disaccreditati
056300081112     c                   if        vdis = 'N' and atmdtfir > 0
056400081112     c                   iter
056500081112     c                   endif
056600080912     c                   move      *blanks       v1csce
056700081028     c                   move      atmSOC        VI2SOC
056800081028     c                   move      atmKSC        VI2KSC
056900081028     c                   move      atmtarga      VI2targa
057000081028     c                   move      atmtipo       VI2tipo
057100081016      *data inizio contratto
057200081028     c                   if        atmdtinr> 0
057300081028     c                   move      atmdtinr      dataiso
057400080912     c                   move      dataiso       dataeur
057500081016     c                   move      dataeur       vi2dtinr
057600080912     c                   else
057700081016     c                   clear                   vi2dtinr
057800080912     c                   endif
057900081028      *data fine contratto
058000081028     c                   if        atmdtfir> 0
058100081028     c                   move      atmdtfir      dataiso
058200081028     c                   move      dataiso       dataeur
058300081028     c                   move      dataeur       vi2dtfir
058400081028     c                   else
058500081028     c                   clear                   vi2dtfir
058600081028     c                   endif
058700081028      *data controllo
058800081028     c                   if        atmdtcon> 0
058900081028     c                   move      atmdtcon      dataiso
059000081028     c                   move      dataiso       dataeur
059100081028     c                   move      dataeur       vi2dtcon
059200081028     c                   else
059300081028     c                   clear                   vi2dtcon
059400081028     c                   endif
059500080912      *esco per raggiunta massima capacità sfl
059600080912     c                   if        snrr1 > 9990
059700080912     c                   leave
059800080912     c                   endif
059900080912
060000080912     C                   EVAL      snrr1 = snrr1 + 1
060100080912     C                   WRITE     video2
060200080912     C                   ENDDO
060300080912
060400080912     C                   IF        snrr1  > 0
060500080912     C                   EVAL      *IN91 = *ON
060600080912     C                   MOVE      1             OK                1 0
060700080912     C                   ELSE
060800080912     C                   MOVE      0             OK                1 0
060900080912     C                   ENDIF
061000080912     C                   EVAL      nrr1  = 1
061100080912     C                   ENDIF
061200081215     C                   EVAL      WrkCarS01 = *On
061300080912     C                   EVAL      snrr1  = 1
061400080912     C                   EVAL      csrrrn = 1
061500130522     c                   clear                   vi2posiz
061600080912     C                   ENDSR
061700080912     c**********************************************************************
061800080912     c* gestione sfl
061900080912     c**********************************************************************
062000080912     C     GestioneSFL   BEGSR
062100080912      *
062200080912     C                   EVAL      WrkEofS01 = *OFF
062300080912      * F10 Immissione.
062400081103     C                   IF        *INKJ and richiamato <> *blank
062500080912     c                   clear                   modalita
062600080912     c                   clear                   video3
062700081028     c                   clear                   aiatm000
062800081217     c                   seton                                        24
062900081028     c                   z-add     vi2soc        vatmsoc
063000081028     c                   z-add     vi2ksc        vatmksc
063100080912     c                   move      knmus         vknmus
063200080912     c                   move      knsif         vknsif
063300080912     c                   movel     ragut         rsut
063400080912     c                   exsr      Gesvideo3
063500081217     c                   setoff                                       24
063600080912     c                   eval      wrkcars01 = *on
063700080912     c                   if        csrrrn = 0
063800080912     c                   goto      noriga
063900080912     c                   endif
064000080912     c                   endif
064100080912      * Elaborazione opzioni.
064200080912     c*
064300130522     c                   clear                   nrr1
064400080912     c                   do        *hival        X                 4 0
064500080912     c     X             CHAIN     video2                             50
064600080912     C   50              LEAVE
064700080912     C                   IF        v1csce <> *blanks
064800080912     c                   exsr      opzione
064900080912
065000080912     c                   move      *blank        v1csce
065100080912     c                   update    video2
065200130423     c
065300130423     c* se ho effettuato una scelta esco dal pgm
065400130423     c                   if        *in25 and par_oTRM<>*blanks
065500130423     c                   leavesr
065600130423     c                   endif
065700130522     c* se ho scritto posizionamento --> eseguo
065800130522     c                   else
065900130522     c
066000130522     c                   if        vi2posiz<>*blanks and vi2posiz<=vi2targa
066100130522     c                             and nrr1=0
066200130522     c                   eval      nrr1=x
066300130522     c                   endif
066400130522     c
066500130522     c                   if        vi2posiz=*blanks
066600130522     c                   eval      nrr1=csrrrn
066700130522     c                   endif
066800080912
066900080912     C                   ENDIF
067000080912     C                   enddo
067100080912      *
067200080912     c                   if        modalita = '1' or modalita = '3'
067300080912     c                             or modalita = '4'
067400080912     c                   eval      wrkcars01 = *on
067500080912     c                   endif
067600080912      *
067700080912     C     noriga        ENDSR
067800080912     c**********************************************************************
067900080912     C     *INZSR        BEGSR
068000080912     c**********************************************************************
068100080912      *
068200080912     C     *ENTRY        PLIST
068300080912     C                   PARM                    Kpjba
068400081028      *richiamato da altri
068500081028     c                   clear                   param
068600130423     c                   clear                   amg_drf
068700081028     c                   if        kpjbu <> *blank
068800100105      *
068900081028     c                   movel     kpjbu         param
069000081028     C                   z-add     parsoc        vi2soc
069100081028     C                   z-add     parksc        vi2ksc
069200081028     c                   movel     'X'           richiamato        1
069300100105     c     katmr         chain     aiats01l
069400100105     c                   if        %found(aiats01l)
069500100105     c                   if        atsdtfic > 0
069600100105     c                   move      *blank        richiamato
069700100105     c                   endif
069800100105     c                   endif
069900130424     C
070000130424     c* Se non passato fornitore o non più accreditato -> non lo visualizzo
070100130424     c                   if        richiamato=*blanks or not %found(aiats01l)
070200130424     c                   seton                                        26
070300130424     c                   endif
070400130424      *
070500130423      *
070600130423      * Se richiamato in modalità scelta --> emetto "1"
070700130423     c                   if        parmod='1'
070800130423     c                   seton                                        2521
070900130423     c                   clear                   par_oTRM
071000130423     c                   eval      vdis='N'
071100130423     c                   endif
071200130423     c
071300130423     C* Se data riferimento piena --> la uso per caricare
071400130423     c                   if        pardrf>*zeros
071500130423     c                   movel     pardrf        AMG_drf           8 0
071600130423     c                   endif
071700100105      *
071800081028     c                   endif
071900080912     C                   Z-ADD     1             CODUT
072000080912     C                   CALL      'X§PARUT'
072100080912     C                   PARM                    UTEDSE
072200080912     C                   MOVEL     REC80         CNCR80
072300080912     C                   MOVEL     Ragut         rsut
072400080912     C                   MOVEL     knsif         vknsif
072500080912     C                   MOVEL     knmus         vknmus
072600080912      *reperimento data
072700080912     C                   TIME                    W0120            14 0
072800080912     C                   MOVE      W0120         WDAT              8 0
072900080912     C*
073000080912     C                   Z-ADD     WDAT          G02DAT
073100080912     C                   MOVEL     *BLANK        G02ERR
073200080912     C                   CALL      'XSRDA8'
073300080912     C                   PARM                    WLBDA8
073400080912     C* UDATE A 8 IN AAAA/MM/GG
073500080912     C                   Z-ADD     G02INV        WUDA              8 0
073600080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
073700080912     C                   CLEAR                   Tibs36Ds
073800080912     C                   EVAL      I36ute = knmus
073900080912     C                   CALL      'TIBS36R'
074000080912     C                   PARM                    tibs36ds
074100081112      * profilo di filiale
074200081112     c                   if        o36pou <> 046
074300081112     c                   seton                                        30
074400081112     c                   endif
074500081029      *decodifica fornitore
074600081029     c                   movel     vi2soc        societa
074700081029     c                   movel     *zeros        codice
074800081029     c                   move      vi2ksc        codice
074900081029     c     kfrn          chain     anfrn01l
075000081029     c                   if        not%found(anfrn01l)
075100081029     c                   seton                                        5996
075200081029     c                   else
075300081029     c     frnsogg       chain     ansog01l
075400081029     c                   movel     sogdes        vi2rasd
075500081029     c                   endif
075600080912
075700081016     C     Kfrn          KLIST
075800081016     C                   KFLD                    societa           3
075900081016     C                   KFLD                    codice            8
076000080912      *
076100081028     C     Katm          KLIST
076200081016     C                   KFLD                    vi2soc
076300081016     C                   KFLD                    vi2ksc
076400081028     C                   KFLD                    vi2targa
076500081029     C     Katmr         KLIST
076600081029     C                   KFLD                    vi2soc
076700081029     C                   KFLD                    vi2ksc
076800130606     C     Katmr3        KLIST
076900130606     C                   KFLD                    vi2soc
077000130606     C                   KFLD                    vatmksc
077100130606     C     Katm3         KLIST
077200130606     C                   KFLD                    vi2soc
077300130606     C                   KFLD                    vatmksc
077400130606     C                   KFLD                    vatmtarga
077500080912      *---------------------------------------------------------------------
077600080912     C                   ENDSR
