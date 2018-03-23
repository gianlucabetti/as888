000100150305     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) dftactgrp(*no) actgrp(*caller)
000200150305     h BNDDIR('UBBNDDIR')
000300080912      *
000400081031     Ftntl83d   CF   E             WORKSTN
000500130515     f                                     sfile(video2:snrr1)
000600081016     Faiata01l  uF a E           k disk
000700081016     Faiata02l  iF   E           k disk    rename(aiata000:aiata2)
000800100104     Faiats01l  iF   E           k disk
000900081016     Fanfrn01l  iF   E           k disk
001000081016     Fansog01l  iF   E           k disk
001100081112     Fansif01l  iF   E           k disk
001200080912      *
001300080912     D Psds           SDS
001400080912     D  PgmName          *PROC
001500080912     D Kpjba         E DS
001600081028     d param           ds
001700081028     d  parsoc                        3s 0
001800081028     d  parksc                        7s 0
001900130422     d* Parmod = 1 --> modalità scelta conducente
002000081028     d  parmod                        1
002100130423     d  pardrf                        8
002200130423     d  par_oAUT                     35
002300130424     d  par_oCEL                     12
002400080912      *
002500081216     D tibs02ds      E DS
002600080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002700080912     D CNCR80        E DS
002800080912     d Tibs36Ds      e ds
002900081216     d dftr          e ds
003000080912     d data            ds            10
003100080912     d aa                      3      4
003200080912     d mm                      6      7
003300080912     d gg                      9     10
003400080912      *
003500080912     D fatto           S              1
003600080912     D modalita        S              1
003700080912     D x               S              4  0
003800080912     D WrkEofS01       S              1
003900080912     D WrkCarS01       S              1
004000080912     D $VIDEO          S             10
004100080912     D snrr1           S              5i 0
004200080912     D Savkpjbu        S                   like(kpjbu)
004300080912     D SavOpzione      S                   like(v1csce)
004400080912     d dataiso         s               d   datfmt(*iso)
004500080912     d dataeur         s               d   datfmt(*eur)
004600150305     D EsitoCel        s              4s 0
004700150305     d V1err           s              1
004800150305     d V1InCellOp      s              1    inz('0')
004900080912     D WLBDA8          DS
005000080912     D  G02DAT                 1      8  0
005100080912     D  G02INV                 9     16  0
005200080912     D  G02ERR                17     17
005300080912     D  G02TGI                18     22  0
005400150305      *__________________________________________________________________________________
005500150305       // - Parametri x UBCHKCEL
005600150305      /copy gaitrasrc/srcProtoPi,UBCHKCEL
005700150305       // - Prototipo srvpgm
005800150305      /copy gaitrasrc/srcProtoPr,UBCHKCEL
005900080912      *---------------- ----------------------------------------------------
006000130424      * 21 on - modalità interrogazione da menù
006100130424      * 25 on - modalità interrogazione: richiamo per scelta conducente
006200130424      * 26 on - non visulizzo fornitore se non passato
006300130424      *---------------- ----------------------------------------------------
006400081028      * se il programma parte da menu o se richiamato da altro
006500081103      * se parte da menu la modalità è solo interrogazione
006600081028     c                   if        richiamato = *blank
006700100104     c                             and atsdtfic = 0
006800130424     c                   seton                                        2126
006900080912      *---------------- Gestione formato video video01----------------------
007000080912     C                   EXSR      Inzvideo1
007100080912     C                   DOU       $Video <> 'VIDEO1'
007200080912      *----------- Visualizzo dati di output in caso di errori.-------------
007300080912     C                   IF        *IN99
007400080912     C                   EVAL      *IN99 = *OFF
007500080912     C                   WRITE     video1
007600080912     C                   EVAL      *IN99 = *ON
007700080912     C                   ENDIF
007800080912      *
007900080912     c     emetti        tag
008000080912     C                   EXFMT     video1
008100080912      *
008200080912     C                   SELECT
008300080912     C                   WHEN      *INKC
008400080912     C                             OR
008500080912     C                             *INKL
008600080912     C                   EVAL      $Video = *BLANKS
008700080912
008800080912     C                   WHEN      *INKE
008900080912     C                   EXSR      Inzvideo1
009000080912     C                   WHEN      *INKB
009100080912     C                   EXSR      Chkvideo1
009200080912     C                   OTHER
009300080912     C                   EXSR      Chkvideo1
009400080912     c   84              goto      emetti
009500080912     C  N99              EXSR      Wrkvideo2c
009600080912     C                   ENDSL
009700080912      *
009800080912     C                   ENDDO
009900081028      *richiamato da altri
010000081028     c                   else
010100100104     c                   if        atsdtfic > 0
010200100104     c                   seton                                        21
010300100104     c                   endif
010400081028     C                   EXSR      Wrkvideo2c
010500081028     c                   endif
010600080912      *
010700080912     C                   EXSR      Uscita
010800080912     c**********************************************************************
010900080912     c* uscita
011000080912     c**********************************************************************
011100080912     C     Uscita        BEGSR
011200080912      *
011300130422      * Se richiamo per la scelta-> passo param in kpjbu
011400130422     c                   if        *in25
011500130422     c                   movel     param         kpjbu
011600130422     c                   endif
011700130422     c
011800080912     C                   EVAL      *INLR = *ON
011900080912     C                   RETURN
012000080912      *
012100080912     C                   ENDSR
012200080912     c**********************************************************************
012300080912     c* gestione sfl
012400080912     c**********************************************************************
012500080912     C     Wrkvideo2c    BEGSR
012600080912      *
012700080912     C                   EVAL      WrkCarS01 = *ON
012800080912     C                   EVAL      $Video = 'VIDEO2C'
012900080912      *
013000130515     c                   if        *in25
013100130515     C                   EXSR      Carvideo2
013200130515     c                   endif
013300130515     c
013400080912     C                   DOU       $Video <> 'VIDEO2C'
013500130515     c* in caso di scelta, una volta carico non ricarico
013600130515     c                   if        not *in25
013700080912     C                   EXSR      Carvideo2
013800130515     c                   endif
013900130515     c                   if        nrr1=0
014000130515     c                   z-add     1             nrr1
014100130515     c                   endif
014200130515     c
014300080912     c*Descrizione della ricerca
014400080912      *
014500080912     C                   WRITE     video2z
014600080912     C                   EXFMT     video2c
014700080912     C                   SELECT
014800080912     c* fine
014900080912     C                   WHEN      *INKC=*ON
015000080912     C                   EVAL      $VIDEO=*BLANKS
015100080912     c* guida
015200080912     C                   WHEN      *INKL=*ON
015300080912     C                   EVAL      $VIDEO='VIDEO1'
015400080912     c* ripristino
015500080912     C                   WHEN      *INKE=*ON
015600080912     C                   EVAL      WrkCarS01 = *ON
015700080912
015800080912     C                   OTHER
015900080912
016000080912     C                   EXSR      GestioneSFL
016100080912     C                   ENDSL
016200080912
016300080912     C                   ENDDO
016400080912     C                   ENDSR
016500080912     c**********************************************************************
016600080912     c* Controlli video R01.
016700080912     c**********************************************************************
016800080912     C     Chkvideo1     BEGSR
016900080912      *
017000080912     C                   EVAL      *IN99 = *OFF
017100080912     C                   EVAL      *IN84 = *OFF
017200080912      *
017300080912     C                   ENDSR
017400080912     c**********************************************************************
017500080912     c* inizializza r01
017600080912     c**********************************************************************
017700080912     C     Inzvideo1     BEGSR
017800080912
017900080912     c                   if        o36pou <> 046
018000081016     c                   move      *blank        vnome
018100081112     c                   move      'N'           vdis
018200081112     c                   else
018300081112     c                   move      'S'           vdis
018400080912     c                   endif
018500080912     C                   EXSR      Chkvideo1
018600080912
018700080912     C                   ENDSR
018800080912     c**********************************************************************
018900080912     C     opzione       BEGSR
019000080912     c**********************************************************************
019100080912     c                   move      v1csce        modalita
019200081016     c                   movea     '00000000'    *in(50)
019300130605     c                   setoff                                       9637
019400080912     c                   select
019500130422     c                   when      v1csce = '1' and richiamato <> *blank
019600130423     c                             and  *in25
019700130424     c                   eval      par_oAut=vi2aut
019800130424     c                   eval      par_oCel=vi2tel
019900130423     c                   eval      $Video=*blanks
020000130422     c
020100130422     c                   when      v1csce = '2' and richiamato <> *blank
020200130423     c                             and not *in25
020300080912     c                   exsr      modifica
020400130423     c
020500081103     c                   when      v1csce = '3' and richiamato <> *blank
020600130423     c                             and not *in25
020700080912     c                   exsr      copia
020800130423     c
020900080912     c                   when      v1csce = '5'
021000080912     c                   seton                                        21
021100080912     c                   exsr      modifica
021200130423     c                   if        richiamato <> *blank  and not *in25
021300080912     c                   setoff                                       21
021400081103     c                   endif
021500130423     c
021600081103     c                   when      v1csce = '4' and richiamato <> *blank
021700130423     c                             and  not *in25
021800080912     c                   seton                                        21
021900080912     c                   exsr      modifica
022000080912     c                   setoff                                       2128
022100080912     c                   clear                   $msg
022200080912     c                   endsl
022300080912     C                   ENDSR
022400080912     c**********************************************************************
022500080912     C     modifica      BEGSR
022600080912     c**********************************************************************
022700081217     c  n21kata          chain     aiata01l                           98
022800081217     c   21kata          chain(n)  aiata01l                           98
022900080912     c                   if        not *in98
023000081016     c                   move      ATaKSC        vATaKSC
023100081016     c                   move      ATaSOC        vATaSOC
023200081112     c                   move      atasoc        socalfa           3
023300081112     c                   clear                   vatasocd
023400081112     c     socalfa       chain     ansif01l
023500081112     c                   if        %found(ansif01l)
023600081112     c                   movel     sifrgs        vatasocd
023700081112     c                   endif
023800081016     c                   move      ATaaut        vATaaut
023900081016     c                   move      ATATEL        vATATEL
024000081016     c                   move      ATAPAT        vATAPAT
024100081016     c                   move      ATACQUAL      vATACQUAL
024200081016     c                   move      ATAFOTO       vATAFOTO
024300081216     c                   move      ATAfinr       vATAfinr
024400081016     c                   move      ATACPENA      vATACPENA
024500081016     c                   move      ATACPEND      vATACPEND
024600080912      *data inizio contratto
024700081016     c                   if        atadtinr > 0
024800081016     c                   move      atadtinr      dataiso
024900080912     c                   move      dataiso       dataeur
025000081016     c                   move      dataeur       vatadtinr
025100080912     c                   else
025200081016     c                   clear                   vatadtinr
025300080912     c                   endif
025400080912      *data fine contratto
025500081016     c                   if        atadtfir > 0
025600081016     c                   move      atadtfir      dataiso
025700080912     c                   move      dataiso       dataeur
025800081016     c                   move      dataeur       vatadtfir
025900081016     c                   else
026000081016     c                   clear                   vatadtfir
026100081016     c                   endif
026200081216      * decodifica motivo fine rapporto
026300081216     c                   clear                   vatafinrd
026400081216     c                   if        vatafinr <> *blank
026500081216     c                   clear                   tibs02ds
026600081216     c                   movel     'C'           t02mod
026700081216     c                   movel     knsif         t02sif
026800081216     c                   movel     'FTR'         t02cod
026900081216     c                   movel     vatafinr      t02ke1
027000081216     c                   call      'TIBS02R'
027100081216     c                   parm                    KPJBA
027200081216     c                   parm                    TIBS02DS
027300081216     c                   if        t02err =  *blanks
027400081216     c                   movel     t02uni        dftr
027500081216     c                   movel     §ftrdes       vatafinrd
027600081216     c                   endif
027700081216     c                   endif
027800081103     c   21              exsr      controv3
027900080912     c                   exsr      Gesvideo3
028000080912     c                   endif
028100080912     C                   ENDSR
028200080912     c**********************************************************************
028300080912     C     copia         BEGSR
028400080912     c**********************************************************************
028500130605     c                   seton                                        37
028600130605     c     kata          chain(n)  aiata01l                           98
028700130605     c                   if        not *in98
028800130605     c                   clear                   vATaKSC
028900130611     c                   clear                   vATarags
029000130605     c                   move      ATaSOC        vATaSOC
029100130605     c                   move      atasoc        socalfa           3
029200130605     c                   clear                   vatasocd
029300130605     c     socalfa       chain     ansif01l
029400130605     c                   if        %found(ansif01l)
029500130605     c                   movel     sifrgs        vatasocd
029600130605     c                   endif
029700130605     c                   move      ATaaut        vATaaut
029800130605     c                   move      ATATEL        vATATEL
029900130605     c                   move      ATAPAT        vATAPAT
030000130605     c                   move      ATACQUAL      vATACQUAL
030100130605     c                   move      ATAFOTO       vATAFOTO
030200130605     c                   move      ATAfinr       vATAfinr
030300130605     c                   move      ATACPENA      vATACPENA
030400130605     c                   move      ATACPEND      vATACPEND
030500130605      *data inizio contratto
030600130605     c                   if        atadtinr > 0
030700130605     c                   move      atadtinr      dataiso
030800130605     c                   move      dataiso       dataeur
030900130605     c                   move      dataeur       vatadtinr
031000130605     c                   else
031100130605     c                   clear                   vatadtinr
031200130605     c                   endif
031300130605      *data fine contratto
031400130605     c                   if        atadtfir > 0
031500130605     c                   move      atadtfir      dataiso
031600130605     c                   move      dataiso       dataeur
031700130605     c                   move      dataeur       vatadtfir
031800130605     c                   else
031900130605     c                   clear                   vatadtfir
032000130605     c                   endif
032100130605      * decodifica motivo fine rapporto
032200130605     c                   clear                   vatafinrd
032300130605     c                   if        vatafinr <> *blank
032400130605     c                   clear                   tibs02ds
032500130605     c                   movel     'C'           t02mod
032600130605     c                   movel     knsif         t02sif
032700130605     c                   movel     'FTR'         t02cod
032800130605     c                   movel     vatafinr      t02ke1
032900130605     c                   call      'TIBS02R'
033000130605     c                   parm                    KPJBA
033100130605     c                   parm                    TIBS02DS
033200130605     c                   if        t02err =  *blanks
033300130605     c                   movel     t02uni        dftr
033400130605     c                   movel     §ftrdes       vatafinrd
033500130605     c                   endif
033600130605     c                   endif
033700130605     c   21              exsr      controv3
033800130605     c                   exsr      Gesvideo3
033900130605     c                   endif
034000130605     C                   ENDSR
034100080912     c**********************************************************************
034200080912     C     gesvideo3     BEGSR
034300080912     c**********************************************************************
034400080912     c                   clear                   fatto
034500080912     c                   do        *hival
034600080912     c                   exfmt     video3
034700080912     c   kl              leave
034800081103     c  n21              exsr      controv3
034900080912     c   96              iter
035000080912     c   kf              exsr      aggiorna
035100080912     c                   if        fatto <> *blank
035200080912     c                   leave
035300080912     c                   endif
035400080912
035500080912     c                   enddo
035600080912     C                   ENDSR
035700080912     c**********************************************************************
035800080912     C     Controv3      BEGSR
035900080912     c**********************************************************************
036000080912      *
036100080912      * *in96 errore generico videata dettaglio
036200080912     c                   setoff                                       9628
036300080912     c                   clear                   $msg
036400080912     c                   if        modalita ='4'
036500080912     c                   goto      endctr
036600080912     c                   endif
036700141001     c                   if        modalita = *blank
036800141001     c     kata3         chain     aiata01l
036900141001     c                   if        %found(aiata01l)
037000141001     c                   seton                                        9654
037100141001     c                   leavesr
037200141001     c                   endif
037300141001     c                   endif
037400081016      *codice fornitore
037500130605     c                   setoff                                       585559
037600130605     c                   setoff                                       54
037700081016     c                   if        vataaut =  *blank
037800081016     c                   seton                                        5896
037900080912     c                   endif
038000130605     c                   if        modalita = '3'
038100130605     c                   if        vi2ksc  =  vataksc
038200130605     c                   seton                                        5596
038300130605     c                   endif
038400130605     c     katar3        chain     aiats01l
038500130605     c                   if        not%found(aiats01l)
038600130605     c                   seton                                        5996
038700130605     c                   else
038800130605     c     katar         chain     aiats01l
038900130605     c                   endif
039000130605     c     kata3         chain     aiata01l
039100130605     c                   if        %found(aiata01l)
039200130605     c                   seton                                        5496
039300130605     c                   endif
039400130605     c                   endif
039500081016     c                   movel     vatasoc       societa
039600081016     c                   movel     *zeros        codice
039700081016     c                   move      vataksc       codice
039800081016     c     kfrn          chain     anfrn01l
039900081016     c                   if        not%found(anfrn01l)
040000081016     c                   seton                                        5996
040100081016     c                   else
040200081016     c     frnsogg       chain     ansog01l
040300081016     c                   movel     sogdes        vatarags
040400081016     c                   endif
040500081016     c
040600090108      *codice fornitore
040700150305     c                   setoff                                       5752
040800090108     c                   if        vatatel =  *blank
040900150306      *obbligatorio al momento asteriscato
041000150306     c*m                 seton                                        5796
041100150305     c                   else
041200150305     c                   exsr      chktel
041300150305     c                   if        v1err <> 'N'
041400150305     C                   SETON                                        5296
041500150305     c                   endif
041600090108     c                   endif
041700081016      *data inizio rapporto
041800080912     c                   setoff                                       53
041900081016     c                   if        vatadtinr =  *zeros
042000080912     c                   seton                                        5396
042100080912     c                   endif
042200081016      *controlli data  inizio rapporto
042300080912     c                   setoff                                       50
042400081016     c                   if        vatadtinr > 0
042500081016     C                   MOVE      Vatadtinr     G02DAT
042600081016     C                   MOVEL     *BLaNK        G02ERR
042700080912     C                   CALL      'XSRDA8'
042800080912     C                   PARM                    WLBDA8
042900080912     C     G02ERR        IFEQ      '1'
043000080912     C                   SETON                                        5096
043100080912     C                   END
043200081016     c                   move      g02dat        vatadtinr
043300081016     c                   move      g02inv        vatadtinrg        8 0
043400080912     c                   endif
043500081016      *controlli data  fine rapporto
043600081016     c                   setoff                                       51
043700081016     c                   if        vatadtfir > 0
043800081016     C                   MOVE      Vatadtfir     G02DAT
043900080912     C                   MOVEL     *BLANK        G02ERR
044000080912     C                   CALL      'XSRDA8'
044100080912     C                   PARM                    WLBDA8
044200080912     C     G02ERR        IFEQ      '1'
044300081016     C                   SETON                                        5196
044400080912     C                   END
044500081016     c                   move      g02dat        vatadtfir
044600081016     c                   move      g02inv        vatadtfirg        8 0
044700080912     c                   endif
044800080912      *controllo data decorrenza maggiore data scadenza
044900081210     c*                  setoff                                       56
045000081210     c*                  if        vatadtinr > vatadtfir
045100081210     c*                  seton                                        5696
045200081210     c*                  endif
045300081016      *controlli inizio validità
045400081216     c                   clear                   vatafinrd
045500081216     c                   if        vatafinr = '?'
045600081216     c                   clear                   tibs02ds
045700081216     c                   movel     'R'           t02mod
045800081216     c                   movel     knsif         t02sif
045900081216     c                   movel     'FTR'         t02cod
046000081216     c                   call      'TIBS02R'
046100081216     c                   parm                    KPJBA
046200081216     c                   parm                    TIBS02DS
046300081216     c                   if        t02err =  *blanks
046400081216     c                   movel     t02uni        dftr
046500081216     c                   movel     §ftrDES       vatafinrd
046600081216     c                   movel     t02ke1        vatafinr
046700081216     c                   goto      endctr
046800081216     c                   endif
046900081216     c                   else
047000081216     c                   if        vatafinr <> *blank
047100081216     c                   clear                   tibs02ds
047200081216     c                   movel     'C'           t02mod
047300081216     c                   movel     knsif         t02sif
047400081216     c                   movel     'FTR'         t02cod
047500081216     c                   movel     vatafinr      t02ke1
047600081216     c                   call      'TIBS02R'
047700081216     c                   parm                    KPJBA
047800081216     c                   parm                    TIBS02DS
047900081216     c                   if        t02err =  *blanks
048000081216     c                   movel     t02uni        dftr
048100081216     c                   movel     §ftrDES       vatafinrd
048200081216     c                   endif
048300081216     c                   endif
048400081216     c                   endif
048500090108      *controllo campi s/n
048600090108     c                   setoff                                       35
048700090108     c                   if        vatapat = *blank
048800090108     c                   seton                                        3596
048900090108     c                   endif
049000090108     c                   setoff                                       31
049100090108     c                   if        vatacqual = *blank
049200090108     c                   seton                                        3196
049300090108     c                   endif
049400090108     c                   setoff                                       32
049500090108     c                   if        vatafoto = *blank
049600090108     c                   seton                                        3296
049700090108     c                   endif
049800090108     c                   setoff                                       33
049900090108     c                   if        vatacpena = *blank
050000090108     c                   seton                                        3396
050100090108     c                   endif
050200090108     c                   setoff                                       34
050300090108     c                   if        vatacpend = *blank
050400090108     c                   seton                                        3496
050500090108     c                   endif
050600080912     C     endctr        ENDSR
050700080912     c**********************************************************************
050800080912     C     aggiorna      BEGSR
050900080912     c**********************************************************************
051000081215     c                   move      vATATEL       ATATEL
051100081215     c                   move      vATAPAT       ATAPAT
051200081215     c                   move      vATACQUAL     ATACQUAL
051300081215     c                   move      vATAFOTO      ATAFOTO
051400081216     c                   move      vATAfinr      ATAFinr
051500081215     c                   move      vATACPENA     ATACPENA
051600081215     c                   move      vATACPEND     ATACPEND
051700081016     c                   if        vatadtinr > 0
051800081016     c                   move      vatadtinr     dataeur
051900080912     c                   move      dataeur       dataiso
052000081016     c                   move      dataiso       atadtinr
052100080912     c                   endif
052200081016     c                   if        vatadtfir > 0
052300081016     c                   move      vatadtfir     dataeur
052400081016     c                   move      dataeur       dataiso
052500081016     c                   move      dataiso       atadtfir
052600090109     c                   else
052700090109     c                   z-add     0             atadtfir
052800081016     c                   endif
052900080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
053000080912     c                   select
053100080912     c                   when      modalita = '4'
053200081016     c                   delete    aiata000
053300080912     c                   when      modalita = *blank or modalita = '3'
053400081028     c                   move      vATaKSC       ATaKSC
053500081028     c                   move      vATaSOC       ATaSOC
053600081028     c                   move      vATaaut       ATaaut
053700081016     c                   write     aiata000
053800080912     c                   other
053900081016     c                   update    aiata000
054000080912     c                   endsl
054100080912     c                   move      'X'           fatto
054200080912     C                   ENDSR
054300080912     c**********************************************************************
054400080912     c* carica sfl
054500080912     c**********************************************************************
054600080912     C     Carvideo2     BEGSR
054700080912     C                   IF        WrkCarS01 = *ON
054800080912     C                   EVAL      snrr1   = 0
054900080912     C                   EVAL      *IN90 = *ON
055000080912     C                   EVAL      *IN91 = *OFF
055100080912     C                   WRITE     video2c
055200080912     C                   EVAL      *IN90 = *OFF
055300081029     c                   if        richiamato = *blank
055400100104     c                   if        atsdtfic > 0
055500100104     c     katar         setll     aiata01l
055600100104     c                   else
055700081029     c     vnome         setll     aiata02l
055800100104     c                   endif
055900081029     c                   else
056000081029     c     katar         setll     aiata01l
056100081029     c                   endif
056200081029     c                   do        *hival
056300081029     c                   if        richiamato = *blank
056400100104     c                   if        atsdtfic > 0
056500100104     c     katar         reade(n)  aiata01l                               97
056600100104     c                   else
056700081029     c                   read      aiata02l                               97
056800100104     c                   endif
056900081029     c                   else
057000081029     c     katar         reade(n)  aiata01l                               97
057100081029     c                   endif
057200081029     c   97              leave
057300081112      * escludo disaccreditati
057400081112     c                   if        vdis = 'N' and atadtfir > 0
057500081112     c                   iter
057600081112     c                   endif
057700081112
057800080912     c                   move      *blanks       v1csce
057900081016     c                   move      ataSOC        VI2SOC
058000081016     c                   move      ataKSC        VI2KSC
058100081016     c                   move      ataaut        VI2aut
058200081016     c                   move      atatel        VI2tel
058300081016      *data inizio contratto
058400081016     c                   if        atadtinr> 0
058500081016     c                   move      atadtinr      dataiso
058600080912     c                   move      dataiso       dataeur
058700081016     c                   move      dataeur       vi2dtinr
058800080912     c                   else
058900081016     c                   clear                   vi2dtinr
059000080912     c                   endif
059100081029      *data inizio contratto
059200081029     c                   if        atadtfir> 0
059300081029     c                   move      atadtfir      dataiso
059400081029     c                   move      dataiso       dataeur
059500081029     c                   move      dataeur       vi2dtfir
059600081029     c                   else
059700081029     c                   clear                   vi2dtfir
059800081029     c                   endif
059900080912      *esco per raggiunta massima capacità sfl
060000080912     c                   if        snrr1 > 9990
060100080912     c                   leave
060200080912     c                   endif
060300080912
060400080912     C                   EVAL      snrr1 = snrr1 + 1
060500080912     C                   WRITE     video2
060600080912     C                   ENDDO
060700080912
060800080912     C                   IF        snrr1  > 0
060900080912     C                   EVAL      *IN91 = *ON
061000080912     C                   MOVE      1             OK                1 0
061100080912     C                   ELSE
061200080912     C                   MOVE      0             OK                1 0
061300080912     C                   ENDIF
061400080912     C                   EVAL      nrr1  = 1
061500080912     C                   ENDIF
061600081215     C                   EVAL      WrkCarS01 = *On
061700080912     C                   EVAL      snrr1  = 1
061800080912     C                   EVAL      csrrrn = 1
061900130515     c                   clear                   vi2posiz
062000080912     C                   ENDSR
062100080912     c**********************************************************************
062200080912     c* gestione sfl
062300080912     c**********************************************************************
062400080912     C     GestioneSFL   BEGSR
062500080912      *
062600080912     C                   EVAL      WrkEofS01 = *OFF
062700080912      * F10 Immissione.
062800081103     C                   IF        *INKJ and richiamato <> *blank
062900080912     c                   clear                   modalita
063000080912     c                   clear                   video3
063100081016     c                   clear                   aiata000
063200081216     c                   seton                                        24
063300081016     c                   z-add     vi2soc        vatasoc
063400081016     c                   z-add     vi2ksc        vataksc
063500080912     c                   move      knmus         vknmus
063600080912     c                   move      knsif         vknsif
063700080912     c                   movel     ragut         rsut
063800080912     c                   exsr      Gesvideo3
063900081216     c                   setoff                                       24
064000080912     c                   eval      wrkcars01 = *on
064100080912     c                   if        csrrrn = 0
064200080912     c                   goto      noriga
064300080912     c                   endif
064400080912     c                   endif
064500130515     c
064600080912      * Elaborazione opzioni.
064700080912     c*
064800130515     c                   clear                   nrr1
064900080912     c                   do        *hival        X                 4 0
065000080912     c     X             CHAIN     video2                             50
065100080912     C   50              LEAVE
065200080912     C                   IF        v1csce <> *blanks
065300080912     c                   exsr      opzione
065400080912
065500080912     c                   move      *blank        v1csce
065600080912     c                   update    video2
065700130422     c
065800130422     c* se ho effettuato una scelta esco dal pgm
065900130422     c                   if        *in25 and par_oaut<>*blanks
066000130422     c                   leavesr
066100130422     c                   endif
066200130515     c* se ho scritto posizionamento --> eseguo
066300130515     c                   else
066400130515     c
066500130515     c                   if        vi2posiz<>*blanks and vi2posiz<=vi2aut
066600130515     c                             and nrr1=0
066700130515     c                   eval      nrr1=x
066800130515     c                   endif
066900130515     c
067000130515     c                   if        vi2posiz=*blanks
067100130515     c                   eval      nrr1=csrrrn
067200130515     c                   endif
067300080912
067400080912     C                   ENDIF
067500080912     C                   enddo
067600080912      *
067700080912     c                   if        modalita = '1' or modalita = '3'
067800090108     c                             or modalita = '2'
067900080912     c                   eval      wrkcars01 = *on
068000080912     c                   endif
068100080912      *
068200080912     C     noriga        ENDSR
068300080912     c**********************************************************************
068400080912     C     *INZSR        BEGSR
068500080912     c**********************************************************************
068600080912      *
068700080912     C     *ENTRY        PLIST
068800080912     C                   PARM                    Kpjba
068900081028      *richiamato da altri
069000081028     c                   clear                   param
069100130423     c                   clear                   amg_drf
069200130423     c
069300081028     c                   if        kpjbu <> *blank
069400100104      *
069500081028     c                   movel     kpjbu         param
069600081028     C                   z-add     parsoc        vi2soc
069700081028     C                   z-add     parksc        vi2ksc
069800081028     c                   movel     'X'           richiamato        1
069900100104     c     katar         chain     aiats01l
070000100104     c                   if        %found(aiats01l)
070100100104     c                   if        atsdtfic > 0
070200100104     c                   move      *blank        richiamato
070300100104     c                   endif
070400100104     c                   endif
070500130424     c
070600130424     c* Se non passato fornitore o non più accreditato -> non lo visualizzo
070700130424     c                   if        richiamato=*blanks or not %found(aiats01l)
070800130424     c                   seton                                        26
070900130424     c                   endif
071000100104      *
071100130422      * Se richiamato in modalità scelta --> emetto "1"
071200130422     c                   if        parmod='1'
071300130423     c                   seton                                        2521
071400130422     c                   clear                   par_oaut
071500130424     c                   clear                   par_ocel
071600130423     c                   eval      vdis='N'
071700081028     c                   endif
071800130423     c
071900130423     C* Se data riferimento piena --> la uso per caricare
072000130423     c                   if        pardrf>*zeros
072100130423     c                   movel     pardrf        AMG_drf           8 0
072200130423     c                   endif
072300130423     c
072400130422     c                   endif
072500130422     c
072600080912     C                   Z-ADD     1             CODUT
072700080912     C                   CALL      'X§PARUT'
072800080912     C                   PARM                    UTEDSE
072900080912     C                   MOVEL     REC80         CNCR80
073000080912     C                   MOVEL     Ragut         rsut
073100080912     C                   MOVEL     knsif         vknsif
073200080912     C                   MOVEL     knmus         vknmus
073300080912      *reperimento data
073400080912     C                   TIME                    W0120            14 0
073500080912     C                   MOVE      W0120         WDAT              8 0
073600080912     C*
073700080912     C                   Z-ADD     WDAT          G02DAT
073800080912     C                   MOVEL     *BLANK        G02ERR
073900080912     C                   CALL      'XSRDA8'
074000080912     C                   PARM                    WLBDA8
074100080912     C* UDATE A 8 IN AAAA/MM/GG
074200080912     C                   Z-ADD     G02INV        WUDA              8 0
074300080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
074400080912     C                   CLEAR                   Tibs36Ds
074500080912     C                   EVAL      I36ute = knmus
074600080912     C                   CALL      'TIBS36R'
074700080912     C                   PARM                    tibs36ds
074800081112      * profilo di filiale
074900081112     c                   if        o36pou <> 046
075000081112     c                   seton                                        30
075100081112     c                   endif
075200081029      *decodifica fornitore
075300081029     c                   movel     vi2soc        societa
075400081029     c                   movel     *zeros        codice
075500081029     c                   move      vi2ksc        codice
075600081029     c     kfrn          chain     anfrn01l
075700081029     c                   if        not%found(anfrn01l)
075800081029     c                   seton                                        5996
075900081029     c                   else
076000081029     c     frnsogg       chain     ansog01l
076100081029     c                   movel     sogdes        vi2rasd
076200081029     c                   endif
076300080912
076400081016     C     Kfrn          KLIST
076500081016     C                   KFLD                    societa           3
076600081016     C                   KFLD                    codice            8
076700080912      *
076800081016     C     Kata          KLIST
076900081016     C                   KFLD                    vi2soc
077000081016     C                   KFLD                    vi2ksc
077100081016     C                   KFLD                    vi2aut
077200081029     C     Katar         KLIST
077300081029     C                   KFLD                    vi2soc
077400081029     C                   KFLD                    vi2ksc
077500130605     C     Katar3        KLIST
077600130605     C                   KFLD                    vi2soc
077700130605     C                   KFLD                    vataksc
077800130605     C     Kata3         KLIST
077900130605     C                   KFLD                    vi2soc
078000130605     C                   KFLD                    vataksc
078100130605     C                   KFLD                    vataaut
078200080912      *---------------------------------------------------------------------
078300080912     C                   ENDSR
078400150305      /FREE
078500150305       //--------------------------------------------------------------
078600150305       //?Richiamo Service Program UBCHKCEL                             ?
078700150305       //--------------------------------------------------------------
078800150305       BEGSR ChkTeL;
078900150305
079000150305         pInCell  = Vatatel;
079100150305         pOutCell = *blank;
079200150305         pOutErr = *blank;
079300150305
079400150305         if V1InCellOp = *blank;
079500150305           // è il NOPASS
079600150305           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
079700150305                                      pOutErr );
079800150305         else;
079900150305           pInCellOpz = V1InCellOp;
080000150305           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
080100150305                                      pOutErr: pInCellOpz );
080200150305         endif;
080300150305
080400150305         Vatatel = pOutCell;
080500150305         V1Err     = pOutErr ;
080600150305
080700150305       ENDSR;
080800150305      /END-FREE
