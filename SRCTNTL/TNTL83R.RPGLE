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
015700160830     c* Forza uscita autisti non trovati
015800160831     C                   WHEN      ok = 0 and not*inkj
015900160830     C                   exsr      uscita
016000080912
016100080912     C                   OTHER
016200080912
016300080912     C                   EXSR      GestioneSFL
016400080912     C                   ENDSL
016500080912
016600080912     C                   ENDDO
016700080912     C                   ENDSR
016800080912     c**********************************************************************
016900080912     c* Controlli video R01.
017000080912     c**********************************************************************
017100080912     C     Chkvideo1     BEGSR
017200080912      *
017300080912     C                   EVAL      *IN99 = *OFF
017400080912     C                   EVAL      *IN84 = *OFF
017500080912      *
017600080912     C                   ENDSR
017700080912     c**********************************************************************
017800080912     c* inizializza r01
017900080912     c**********************************************************************
018000080912     C     Inzvideo1     BEGSR
018100080912
018200080912     c                   if        o36pou <> 046
018300081016     c                   move      *blank        vnome
018400081112     c                   move      'N'           vdis
018500081112     c                   else
018600081112     c                   move      'S'           vdis
018700080912     c                   endif
018800080912     C                   EXSR      Chkvideo1
018900080912
019000080912     C                   ENDSR
019100080912     c**********************************************************************
019200080912     C     opzione       BEGSR
019300080912     c**********************************************************************
019400080912     c                   move      v1csce        modalita
019500081016     c                   movea     '00000000'    *in(50)
019600130605     c                   setoff                                       9637
019700080912     c                   select
019800130422     c                   when      v1csce = '1' and richiamato <> *blank
019900130423     c                             and  *in25
020000130424     c                   eval      par_oAut=vi2aut
020100130424     c                   eval      par_oCel=vi2tel
020200130423     c                   eval      $Video=*blanks
020300130422     c
020400130422     c                   when      v1csce = '2' and richiamato <> *blank
020500130423     c                             and not *in25
020600080912     c                   exsr      modifica
020700130423     c
020800081103     c                   when      v1csce = '3' and richiamato <> *blank
020900130423     c                             and not *in25
021000080912     c                   exsr      copia
021100130423     c
021200080912     c                   when      v1csce = '5'
021300080912     c                   seton                                        21
021400080912     c                   exsr      modifica
021500130423     c                   if        richiamato <> *blank  and not *in25
021600080912     c                   setoff                                       21
021700081103     c                   endif
021800130423     c
021900081103     c                   when      v1csce = '4' and richiamato <> *blank
022000130423     c                             and  not *in25
022100080912     c                   seton                                        21
022200080912     c                   exsr      modifica
022300080912     c                   setoff                                       2128
022400080912     c                   clear                   $msg
022500080912     c                   endsl
022600080912     C                   ENDSR
022700080912     c**********************************************************************
022800080912     C     modifica      BEGSR
022900080912     c**********************************************************************
023000081217     c  n21kata          chain     aiata01l                           98
023100081217     c   21kata          chain(n)  aiata01l                           98
023200080912     c                   if        not *in98
023300081016     c                   move      ATaKSC        vATaKSC
023400081016     c                   move      ATaSOC        vATaSOC
023500081112     c                   move      atasoc        socalfa           3
023600081112     c                   clear                   vatasocd
023700081112     c     socalfa       chain     ansif01l
023800081112     c                   if        %found(ansif01l)
023900081112     c                   movel     sifrgs        vatasocd
024000081112     c                   endif
024100081016     c                   move      ATaaut        vATaaut
024200081016     c                   move      ATATEL        vATATEL
024300081016     c                   move      ATAPAT        vATAPAT
024400081016     c                   move      ATACQUAL      vATACQUAL
024500081016     c                   move      ATAFOTO       vATAFOTO
024600081216     c                   move      ATAfinr       vATAfinr
024700081016     c                   move      ATACPENA      vATACPENA
024800081016     c                   move      ATACPEND      vATACPEND
024900080912      *data inizio contratto
025000081016     c                   if        atadtinr > 0
025100081016     c                   move      atadtinr      dataiso
025200080912     c                   move      dataiso       dataeur
025300081016     c                   move      dataeur       vatadtinr
025400080912     c                   else
025500081016     c                   clear                   vatadtinr
025600080912     c                   endif
025700080912      *data fine contratto
025800081016     c                   if        atadtfir > 0
025900081016     c                   move      atadtfir      dataiso
026000080912     c                   move      dataiso       dataeur
026100081016     c                   move      dataeur       vatadtfir
026200081016     c                   else
026300081016     c                   clear                   vatadtfir
026400081016     c                   endif
026500081216      * decodifica motivo fine rapporto
026600081216     c                   clear                   vatafinrd
026700081216     c                   if        vatafinr <> *blank
026800081216     c                   clear                   tibs02ds
026900081216     c                   movel     'C'           t02mod
027000081216     c                   movel     knsif         t02sif
027100081216     c                   movel     'FTR'         t02cod
027200081216     c                   movel     vatafinr      t02ke1
027300081216     c                   call      'TIBS02R'
027400081216     c                   parm                    KPJBA
027500081216     c                   parm                    TIBS02DS
027600081216     c                   if        t02err =  *blanks
027700081216     c                   movel     t02uni        dftr
027800081216     c                   movel     §ftrdes       vatafinrd
027900081216     c                   endif
028000081216     c                   endif
028100081103     c   21              exsr      controv3
028200080912     c                   exsr      Gesvideo3
028300080912     c                   endif
028400080912     C                   ENDSR
028500080912     c**********************************************************************
028600080912     C     copia         BEGSR
028700080912     c**********************************************************************
028800130605     c                   seton                                        37
028900130605     c     kata          chain(n)  aiata01l                           98
029000130605     c                   if        not *in98
029100130605     c                   clear                   vATaKSC
029200130611     c                   clear                   vATarags
029300130605     c                   move      ATaSOC        vATaSOC
029400130605     c                   move      atasoc        socalfa           3
029500130605     c                   clear                   vatasocd
029600130605     c     socalfa       chain     ansif01l
029700130605     c                   if        %found(ansif01l)
029800130605     c                   movel     sifrgs        vatasocd
029900130605     c                   endif
030000130605     c                   move      ATaaut        vATaaut
030100130605     c                   move      ATATEL        vATATEL
030200130605     c                   move      ATAPAT        vATAPAT
030300130605     c                   move      ATACQUAL      vATACQUAL
030400130605     c                   move      ATAFOTO       vATAFOTO
030500130605     c                   move      ATAfinr       vATAfinr
030600130605     c                   move      ATACPENA      vATACPENA
030700130605     c                   move      ATACPEND      vATACPEND
030800130605      *data inizio contratto
030900130605     c                   if        atadtinr > 0
031000130605     c                   move      atadtinr      dataiso
031100130605     c                   move      dataiso       dataeur
031200130605     c                   move      dataeur       vatadtinr
031300130605     c                   else
031400130605     c                   clear                   vatadtinr
031500130605     c                   endif
031600130605      *data fine contratto
031700130605     c                   if        atadtfir > 0
031800130605     c                   move      atadtfir      dataiso
031900130605     c                   move      dataiso       dataeur
032000130605     c                   move      dataeur       vatadtfir
032100130605     c                   else
032200130605     c                   clear                   vatadtfir
032300130605     c                   endif
032400130605      * decodifica motivo fine rapporto
032500130605     c                   clear                   vatafinrd
032600130605     c                   if        vatafinr <> *blank
032700130605     c                   clear                   tibs02ds
032800130605     c                   movel     'C'           t02mod
032900130605     c                   movel     knsif         t02sif
033000130605     c                   movel     'FTR'         t02cod
033100130605     c                   movel     vatafinr      t02ke1
033200130605     c                   call      'TIBS02R'
033300130605     c                   parm                    KPJBA
033400130605     c                   parm                    TIBS02DS
033500130605     c                   if        t02err =  *blanks
033600130605     c                   movel     t02uni        dftr
033700130605     c                   movel     §ftrdes       vatafinrd
033800130605     c                   endif
033900130605     c                   endif
034000130605     c   21              exsr      controv3
034100130605     c                   exsr      Gesvideo3
034200130605     c                   endif
034300130605     C                   ENDSR
034400080912     c**********************************************************************
034500080912     C     gesvideo3     BEGSR
034600080912     c**********************************************************************
034700080912     c                   clear                   fatto
034800080912     c                   do        *hival
034900080912     c                   exfmt     video3
035000080912     c   kl              leave
035100081103     c  n21              exsr      controv3
035200080912     c   96              iter
035300080912     c   kf              exsr      aggiorna
035400080912     c                   if        fatto <> *blank
035500080912     c                   leave
035600080912     c                   endif
035700080912
035800080912     c                   enddo
035900080912     C                   ENDSR
036000080912     c**********************************************************************
036100080912     C     Controv3      BEGSR
036200080912     c**********************************************************************
036300080912      *
036400080912      * *in96 errore generico videata dettaglio
036500080912     c                   setoff                                       9628
036600080912     c                   clear                   $msg
036700080912     c                   if        modalita ='4'
036800080912     c                   goto      endctr
036900080912     c                   endif
037000141001     c                   if        modalita = *blank
037100141001     c     kata3         chain     aiata01l
037200141001     c                   if        %found(aiata01l)
037300141001     c                   seton                                        9654
037400141001     c                   leavesr
037500141001     c                   endif
037600141001     c                   endif
037700081016      *codice fornitore
037800130605     c                   setoff                                       585559
037900130605     c                   setoff                                       54
038000081016     c                   if        vataaut =  *blank
038100081016     c                   seton                                        5896
038200080912     c                   endif
038300130605     c                   if        modalita = '3'
038400130605     c                   if        vi2ksc  =  vataksc
038500130605     c                   seton                                        5596
038600130605     c                   endif
038700130605     c     katar3        chain     aiats01l
038800130605     c                   if        not%found(aiats01l)
038900130605     c                   seton                                        5996
039000130605     c                   else
039100130605     c     katar         chain     aiats01l
039200130605     c                   endif
039300130605     c     kata3         chain     aiata01l
039400130605     c                   if        %found(aiata01l)
039500130605     c                   seton                                        5496
039600130605     c                   endif
039700130605     c                   endif
039800081016     c                   movel     vatasoc       societa
039900081016     c                   movel     *zeros        codice
040000081016     c                   move      vataksc       codice
040100081016     c     kfrn          chain     anfrn01l
040200081016     c                   if        not%found(anfrn01l)
040300081016     c                   seton                                        5996
040400081016     c                   else
040500081016     c     frnsogg       chain     ansog01l
040600081016     c                   movel     sogdes        vatarags
040700081016     c                   endif
040800081016     c
040900090108      *codice fornitore
041000150305     c                   setoff                                       5752
041100090108     c                   if        vatatel =  *blank
041200150306      *obbligatorio al momento asteriscato
041300150306     c*m                 seton                                        5796
041400150305     c                   else
041500150305     c                   exsr      chktel
041600150305     c                   if        v1err <> 'N'
041700150305     C                   SETON                                        5296
041800150305     c                   endif
041900090108     c                   endif
042000081016      *data inizio rapporto
042100080912     c                   setoff                                       53
042200081016     c                   if        vatadtinr =  *zeros
042300080912     c                   seton                                        5396
042400080912     c                   endif
042500081016      *controlli data  inizio rapporto
042600080912     c                   setoff                                       50
042700081016     c                   if        vatadtinr > 0
042800081016     C                   MOVE      Vatadtinr     G02DAT
042900081016     C                   MOVEL     *BLaNK        G02ERR
043000080912     C                   CALL      'XSRDA8'
043100080912     C                   PARM                    WLBDA8
043200080912     C     G02ERR        IFEQ      '1'
043300080912     C                   SETON                                        5096
043400080912     C                   END
043500081016     c                   move      g02dat        vatadtinr
043600081016     c                   move      g02inv        vatadtinrg        8 0
043700080912     c                   endif
043800081016      *controlli data  fine rapporto
043900081016     c                   setoff                                       51
044000081016     c                   if        vatadtfir > 0
044100081016     C                   MOVE      Vatadtfir     G02DAT
044200080912     C                   MOVEL     *BLANK        G02ERR
044300080912     C                   CALL      'XSRDA8'
044400080912     C                   PARM                    WLBDA8
044500080912     C     G02ERR        IFEQ      '1'
044600081016     C                   SETON                                        5196
044700080912     C                   END
044800081016     c                   move      g02dat        vatadtfir
044900081016     c                   move      g02inv        vatadtfirg        8 0
045000080912     c                   endif
045100080912      *controllo data decorrenza maggiore data scadenza
045200081210     c*                  setoff                                       56
045300081210     c*                  if        vatadtinr > vatadtfir
045400081210     c*                  seton                                        5696
045500081210     c*                  endif
045600081016      *controlli inizio validità
045700081216     c                   clear                   vatafinrd
045800081216     c                   if        vatafinr = '?'
045900081216     c                   clear                   tibs02ds
046000081216     c                   movel     'R'           t02mod
046100081216     c                   movel     knsif         t02sif
046200081216     c                   movel     'FTR'         t02cod
046300081216     c                   call      'TIBS02R'
046400081216     c                   parm                    KPJBA
046500081216     c                   parm                    TIBS02DS
046600081216     c                   if        t02err =  *blanks
046700081216     c                   movel     t02uni        dftr
046800081216     c                   movel     §ftrDES       vatafinrd
046900081216     c                   movel     t02ke1        vatafinr
047000081216     c                   goto      endctr
047100081216     c                   endif
047200081216     c                   else
047300081216     c                   if        vatafinr <> *blank
047400081216     c                   clear                   tibs02ds
047500081216     c                   movel     'C'           t02mod
047600081216     c                   movel     knsif         t02sif
047700081216     c                   movel     'FTR'         t02cod
047800081216     c                   movel     vatafinr      t02ke1
047900081216     c                   call      'TIBS02R'
048000081216     c                   parm                    KPJBA
048100081216     c                   parm                    TIBS02DS
048200081216     c                   if        t02err =  *blanks
048300081216     c                   movel     t02uni        dftr
048400081216     c                   movel     §ftrDES       vatafinrd
048500081216     c                   endif
048600081216     c                   endif
048700081216     c                   endif
048800090108      *controllo campi s/n
048900090108     c                   setoff                                       35
049000090108     c                   if        vatapat = *blank
049100090108     c                   seton                                        3596
049200090108     c                   endif
049300090108     c                   setoff                                       31
049400090108     c                   if        vatacqual = *blank
049500090108     c                   seton                                        3196
049600090108     c                   endif
049700090108     c                   setoff                                       32
049800090108     c                   if        vatafoto = *blank
049900090108     c                   seton                                        3296
050000090108     c                   endif
050100090108     c                   setoff                                       33
050200090108     c                   if        vatacpena = *blank
050300090108     c                   seton                                        3396
050400090108     c                   endif
050500090108     c                   setoff                                       34
050600090108     c                   if        vatacpend = *blank
050700090108     c                   seton                                        3496
050800090108     c                   endif
050900080912     C     endctr        ENDSR
051000080912     c**********************************************************************
051100080912     C     aggiorna      BEGSR
051200080912     c**********************************************************************
051300081215     c                   move      vATATEL       ATATEL
051400081215     c                   move      vATAPAT       ATAPAT
051500081215     c                   move      vATACQUAL     ATACQUAL
051600081215     c                   move      vATAFOTO      ATAFOTO
051700081216     c                   move      vATAfinr      ATAFinr
051800081215     c                   move      vATACPENA     ATACPENA
051900081215     c                   move      vATACPEND     ATACPEND
052000081016     c                   if        vatadtinr > 0
052100081016     c                   move      vatadtinr     dataeur
052200080912     c                   move      dataeur       dataiso
052300081016     c                   move      dataiso       atadtinr
052400080912     c                   endif
052500081016     c                   if        vatadtfir > 0
052600081016     c                   move      vatadtfir     dataeur
052700081016     c                   move      dataeur       dataiso
052800081016     c                   move      dataiso       atadtfir
052900090109     c                   else
053000090109     c                   z-add     0             atadtfir
053100081016     c                   endif
053200080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
053300080912     c                   select
053400080912     c                   when      modalita = '4'
053500081016     c                   delete    aiata000
053600080912     c                   when      modalita = *blank or modalita = '3'
053700081028     c                   move      vATaKSC       ATaKSC
053800081028     c                   move      vATaSOC       ATaSOC
053900081028     c                   move      vATaaut       ATaaut
054000081016     c                   write     aiata000
054100080912     c                   other
054200081016     c                   update    aiata000
054300080912     c                   endsl
054400080912     c                   move      'X'           fatto
054500080912     C                   ENDSR
054600080912     c**********************************************************************
054700080912     c* carica sfl
054800080912     c**********************************************************************
054900080912     C     Carvideo2     BEGSR
055000080912     C                   IF        WrkCarS01 = *ON
055100080912     C                   EVAL      snrr1   = 0
055200080912     C                   EVAL      *IN90 = *ON
055300080912     C                   EVAL      *IN91 = *OFF
055400080912     C                   WRITE     video2c
055500080912     C                   EVAL      *IN90 = *OFF
055600081029     c                   if        richiamato = *blank
055700100104     c                   if        atsdtfic > 0
055800100104     c     katar         setll     aiata01l
055900100104     c                   else
056000081029     c     vnome         setll     aiata02l
056100100104     c                   endif
056200081029     c                   else
056300081029     c     katar         setll     aiata01l
056400081029     c                   endif
056500081029     c                   do        *hival
056600081029     c                   if        richiamato = *blank
056700100104     c                   if        atsdtfic > 0
056800100104     c     katar         reade(n)  aiata01l                               97
056900100104     c                   else
057000081029     c                   read      aiata02l                               97
057100100104     c                   endif
057200081029     c                   else
057300081029     c     katar         reade(n)  aiata01l                               97
057400081029     c                   endif
057500081029     c   97              leave
057600081112      * escludo disaccreditati
057700081112     c                   if        vdis = 'N' and atadtfir > 0
057800081112     c                   iter
057900081112     c                   endif
058000081112
058100080912     c                   move      *blanks       v1csce
058200081016     c                   move      ataSOC        VI2SOC
058300081016     c                   move      ataKSC        VI2KSC
058400081016     c                   move      ataaut        VI2aut
058500081016     c                   move      atatel        VI2tel
058600081016      *data inizio contratto
058700081016     c                   if        atadtinr> 0
058800081016     c                   move      atadtinr      dataiso
058900080912     c                   move      dataiso       dataeur
059000081016     c                   move      dataeur       vi2dtinr
059100080912     c                   else
059200081016     c                   clear                   vi2dtinr
059300080912     c                   endif
059400081029      *data inizio contratto
059500081029     c                   if        atadtfir> 0
059600081029     c                   move      atadtfir      dataiso
059700081029     c                   move      dataiso       dataeur
059800081029     c                   move      dataeur       vi2dtfir
059900081029     c                   else
060000081029     c                   clear                   vi2dtfir
060100081029     c                   endif
060200080912      *esco per raggiunta massima capacità sfl
060300080912     c                   if        snrr1 > 9990
060400080912     c                   leave
060500080912     c                   endif
060600080912
060700080912     C                   EVAL      snrr1 = snrr1 + 1
060800080912     C                   WRITE     video2
060900080912     C                   ENDDO
061000080912
061100080912     C                   IF        snrr1  > 0
061200080912     C                   EVAL      *IN91 = *ON
061300080912     C                   MOVE      1             OK                1 0
061400080912     C                   ELSE
061500080912     C                   MOVE      0             OK                1 0
061600080912     C                   ENDIF
061700080912     C                   EVAL      nrr1  = 1
061800080912     C                   ENDIF
061900081215     C                   EVAL      WrkCarS01 = *On
062000080912     C                   EVAL      snrr1  = 1
062100080912     C                   EVAL      csrrrn = 1
062200130515     c                   clear                   vi2posiz
062300080912     C                   ENDSR
062400080912     c**********************************************************************
062500080912     c* gestione sfl
062600080912     c**********************************************************************
062700080912     C     GestioneSFL   BEGSR
062800080912      *
062900080912     C                   EVAL      WrkEofS01 = *OFF
063000080912      * F10 Immissione.
063100081103     C                   IF        *INKJ and richiamato <> *blank
063200080912     c                   clear                   modalita
063300080912     c                   clear                   video3
063400081016     c                   clear                   aiata000
063500081216     c                   seton                                        24
063600081016     c                   z-add     vi2soc        vatasoc
063700081016     c                   z-add     vi2ksc        vataksc
063800080912     c                   move      knmus         vknmus
063900080912     c                   move      knsif         vknsif
064000080912     c                   movel     ragut         rsut
064100080912     c                   exsr      Gesvideo3
064200081216     c                   setoff                                       24
064300080912     c                   eval      wrkcars01 = *on
064400080912     c                   if        csrrrn = 0
064500080912     c                   goto      noriga
064600080912     c                   endif
064700080912     c                   endif
064800130515     c
064900080912      * Elaborazione opzioni.
065000080912     c*
065100130515     c                   clear                   nrr1
065200080912     c                   do        *hival        X                 4 0
065300080912     c     X             CHAIN     video2                             50
065400080912     C   50              LEAVE
065500080912     C                   IF        v1csce <> *blanks
065600080912     c                   exsr      opzione
065700080912
065800080912     c                   move      *blank        v1csce
065900080912     c                   update    video2
066000130422     c
066100130422     c* se ho effettuato una scelta esco dal pgm
066200130422     c                   if        *in25 and par_oaut<>*blanks
066300130422     c                   leavesr
066400130422     c                   endif
066500130515     c* se ho scritto posizionamento --> eseguo
066600130515     c                   else
066700130515     c
066800130515     c                   if        vi2posiz<>*blanks and vi2posiz<=vi2aut
066900130515     c                             and nrr1=0
067000130515     c                   eval      nrr1=x
067100130515     c                   endif
067200130515     c
067300130515     c                   if        vi2posiz=*blanks
067400130515     c                   eval      nrr1=csrrrn
067500130515     c                   endif
067600080912
067700080912     C                   ENDIF
067800080912     C                   enddo
067900080912      *
068000080912     c                   if        modalita = '1' or modalita = '3'
068100090108     c                             or modalita = '2'
068200080912     c                   eval      wrkcars01 = *on
068300080912     c                   endif
068400080912      *
068500080912     C     noriga        ENDSR
068600080912     c**********************************************************************
068700080912     C     *INZSR        BEGSR
068800080912     c**********************************************************************
068900080912      *
069000080912     C     *ENTRY        PLIST
069100080912     C                   PARM                    Kpjba
069200081028      *richiamato da altri
069300081028     c                   clear                   param
069400130423     c                   clear                   amg_drf
069500130423     c
069600081028     c                   if        kpjbu <> *blank
069700100104      *
069800081028     c                   movel     kpjbu         param
069900081028     C                   z-add     parsoc        vi2soc
070000081028     C                   z-add     parksc        vi2ksc
070100081028     c                   movel     'X'           richiamato        1
070200100104     c     katar         chain     aiats01l
070300100104     c                   if        %found(aiats01l)
070400100104     c                   if        atsdtfic > 0
070500100104     c                   move      *blank        richiamato
070600100104     c                   endif
070700100104     c                   endif
070800130424     c
070900130424     c* Se non passato fornitore o non più accreditato -> non lo visualizzo
071000130424     c                   if        richiamato=*blanks or not %found(aiats01l)
071100130424     c                   seton                                        26
071200130424     c                   endif
071300100104      *
071400130422      * Se richiamato in modalità scelta --> emetto "1"
071500130422     c                   if        parmod='1'
071600130423     c                   seton                                        2521
071700130422     c                   clear                   par_oaut
071800130424     c                   clear                   par_ocel
071900130423     c                   eval      vdis='N'
072000081028     c                   endif
072100130423     c
072200130423     C* Se data riferimento piena --> la uso per caricare
072300130423     c                   if        pardrf>*zeros
072400130423     c                   movel     pardrf        AMG_drf           8 0
072500130423     c                   endif
072600130423     c
072700130422     c                   endif
072800130422     c
072900080912     C                   Z-ADD     1             CODUT
073000080912     C                   CALL      'X§PARUT'
073100080912     C                   PARM                    UTEDSE
073200080912     C                   MOVEL     REC80         CNCR80
073300080912     C                   MOVEL     Ragut         rsut
073400080912     C                   MOVEL     knsif         vknsif
073500080912     C                   MOVEL     knmus         vknmus
073600080912      *reperimento data
073700080912     C                   TIME                    W0120            14 0
073800080912     C                   MOVE      W0120         WDAT              8 0
073900080912     C*
074000080912     C                   Z-ADD     WDAT          G02DAT
074100080912     C                   MOVEL     *BLANK        G02ERR
074200080912     C                   CALL      'XSRDA8'
074300080912     C                   PARM                    WLBDA8
074400080912     C* UDATE A 8 IN AAAA/MM/GG
074500080912     C                   Z-ADD     G02INV        WUDA              8 0
074600080912      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
074700080912     C                   CLEAR                   Tibs36Ds
074800080912     C                   EVAL      I36ute = knmus
074900080912     C                   CALL      'TIBS36R'
075000080912     C                   PARM                    tibs36ds
075100081112      * profilo di filiale
075200081112     c                   if        o36pou <> 046
075300081112     c                   seton                                        30
075400081112     c                   endif
075500081029      *decodifica fornitore
075600081029     c                   movel     vi2soc        societa
075700081029     c                   movel     *zeros        codice
075800081029     c                   move      vi2ksc        codice
075900081029     c     kfrn          chain     anfrn01l
076000081029     c                   if        not%found(anfrn01l)
076100081029     c                   seton                                        5996
076200081029     c                   else
076300081029     c     frnsogg       chain     ansog01l
076400081029     c                   movel     sogdes        vi2rasd
076500081029     c                   endif
076600080912
076700081016     C     Kfrn          KLIST
076800081016     C                   KFLD                    societa           3
076900081016     C                   KFLD                    codice            8
077000080912      *
077100081016     C     Kata          KLIST
077200081016     C                   KFLD                    vi2soc
077300081016     C                   KFLD                    vi2ksc
077400081016     C                   KFLD                    vi2aut
077500081029     C     Katar         KLIST
077600081029     C                   KFLD                    vi2soc
077700081029     C                   KFLD                    vi2ksc
077800130605     C     Katar3        KLIST
077900130605     C                   KFLD                    vi2soc
078000130605     C                   KFLD                    vataksc
078100130605     C     Kata3         KLIST
078200130605     C                   KFLD                    vi2soc
078300130605     C                   KFLD                    vataksc
078400130605     C                   KFLD                    vataaut
078500080912      *---------------------------------------------------------------------
078600080912     C                   ENDSR
078700150305      /FREE
078800150305       //--------------------------------------------------------------
078900150305       //?Richiamo Service Program UBCHKCEL                             ?
079000150305       //--------------------------------------------------------------
079100150305       BEGSR ChkTeL;
079200150305
079300150305         pInCell  = Vatatel;
079400150305         pOutCell = *blank;
079500150305         pOutErr = *blank;
079600150305
079700150305         if V1InCellOp = *blank;
079800150305           // è il NOPASS
079900150305           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
080000150305                                      pOutErr );
080100150305         else;
080200150305           pInCellOpz = V1InCellOp;
080300150305           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
080400150305                                      pOutErr: pInCellOpz );
080500150305         endif;
080600150305
080700150305         Vatatel = pOutCell;
080800150305         V1Err     = pOutErr ;
080900150305
081000150305       ENDSR;
081100150305      /END-FREE
