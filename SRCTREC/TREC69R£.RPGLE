000100080912     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200080912      *
000300081031     Ftntl83d   CF   E             WORKSTN
000400130515     f                                     sfile(video2:snrr1)
000500081016     Faiata01l  uF a E           k disk
000600081016     Faiata02l  iF   E           k disk    rename(aiata000:aiata2)
000700100104     Faiats01l  iF   E           k disk
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
001800130422     d* Parmod = 1 --> modalità scelta conducente
001900081028     d  parmod                        1
002000130423     d  pardrf                        8
002100130423     d  par_oAUT                     35
002200130424     d  par_oCEL                     12
002300080912      *
002400081216     D tibs02ds      E DS
002500080912     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002600080912     D CNCR80        E DS
002700080912     d Tibs36Ds      e ds
002800081216     d dftr          e ds
002900080912     d data            ds            10
003000080912     d aa                      3      4
003100080912     d mm                      6      7
003200080912     d gg                      9     10
003300080912      *
003400080912     D fatto           S              1
003500080912     D modalita        S              1
003600080912     D x               S              4  0
003700080912     D WrkEofS01       S              1
003800080912     D WrkCarS01       S              1
003900080912     D $VIDEO          S             10
004000080912     D snrr1           S              5i 0
004100080912     D Savkpjbu        S                   like(kpjbu)
004200080912     D SavOpzione      S                   like(v1csce)
004300080912     d dataiso         s               d   datfmt(*iso)
004400080912     d dataeur         s               d   datfmt(*eur)
004500080912     D WLBDA8          DS
004600080912     D  G02DAT                 1      8  0
004700080912     D  G02INV                 9     16  0
004800080912     D  G02ERR                17     17
004900080912     D  G02TGI                18     22  0
005000080912      *---------------- ----------------------------------------------------
005100130424      * 21 on - modalità interrogazione da menù
005200130424      * 25 on - modalità interrogazione: richiamo per scelta conducente
005300130424      * 26 on - non visulizzo fornitore se non passato
005400130424      *---------------- ----------------------------------------------------
005500081028      * se il programma parte da menu o se richiamato da altro
005600081103      * se parte da menu la modalità è solo interrogazione
005700081028     c                   if        richiamato = *blank
005800100104     c                             and atsdtfic = 0
005900130424     c                   seton                                        2126
006000080912      *---------------- Gestione formato video video01----------------------
006100080912     C                   EXSR      Inzvideo1
006200080912     C                   DOU       $Video <> 'VIDEO1'
006300080912      *----------- Visualizzo dati di output in caso di errori.-------------
006400080912     C                   IF        *IN99
006500080912     C                   EVAL      *IN99 = *OFF
006600080912     C                   WRITE     video1
006700080912     C                   EVAL      *IN99 = *ON
006800080912     C                   ENDIF
006900080912      *
007000080912     c     emetti        tag
007100080912     C                   EXFMT     video1
007200080912      *
007300080912     C                   SELECT
007400080912     C                   WHEN      *INKC
007500080912     C                             OR
007600080912     C                             *INKL
007700080912     C                   EVAL      $Video = *BLANKS
007800080912
007900080912     C                   WHEN      *INKE
008000080912     C                   EXSR      Inzvideo1
008100080912     C                   WHEN      *INKB
008200080912     C                   EXSR      Chkvideo1
008300080912     C                   OTHER
008400080912     C                   EXSR      Chkvideo1
008500080912     c   84              goto      emetti
008600080912     C  N99              EXSR      Wrkvideo2c
008700080912     C                   ENDSL
008800080912      *
008900080912     C                   ENDDO
009000081028      *richiamato da altri
009100081028     c                   else
009200100104     c                   if        atsdtfic > 0
009300100104     c                   seton                                        21
009400100104     c                   endif
009500081028     C                   EXSR      Wrkvideo2c
009600081028     c                   endif
009700080912      *
009800080912     C                   EXSR      Uscita
009900080912     c**********************************************************************
010000080912     c* uscita
010100080912     c**********************************************************************
010200080912     C     Uscita        BEGSR
010300080912      *
010400130422      * Se richiamo per la scelta-> passo param in kpjbu
010500130422     c                   if        *in25
010600130422     c                   movel     param         kpjbu
010700130422     c                   endif
010800130422     c
010900080912     C                   EVAL      *INLR = *ON
011000080912     C                   RETURN
011100080912      *
011200080912     C                   ENDSR
011300080912     c**********************************************************************
011400080912     c* gestione sfl
011500080912     c**********************************************************************
011600080912     C     Wrkvideo2c    BEGSR
011700080912      *
011800080912     C                   EVAL      WrkCarS01 = *ON
011900080912     C                   EVAL      $Video = 'VIDEO2C'
012000080912      *
012100130515     c                   if        *in25
012200130515     C                   EXSR      Carvideo2
012300130515     c                   endif
012400130515     c
012500080912     C                   DOU       $Video <> 'VIDEO2C'
012600130515     c* in caso di scelta, una volta carico non ricarico
012700130515     c                   if        not *in25
012800080912     C                   EXSR      Carvideo2
012900130515     c                   endif
013000130515     c                   if        nrr1=0
013100130515     c                   z-add     1             nrr1
013200130515     c                   endif
013300130515     c
013400080912     c*Descrizione della ricerca
013500080912      *
013600080912     C                   WRITE     video2z
013700080912     C                   EXFMT     video2c
013800080912     C                   SELECT
013900080912     c* fine
014000080912     C                   WHEN      *INKC=*ON
014100080912     C                   EVAL      $VIDEO=*BLANKS
014200080912     c* guida
014300080912     C                   WHEN      *INKL=*ON
014400080912     C                   EVAL      $VIDEO='VIDEO1'
014500080912     c* ripristino
014600080912     C                   WHEN      *INKE=*ON
014700080912     C                   EVAL      WrkCarS01 = *ON
014800080912
014900080912     C                   OTHER
015000080912
015100080912     C                   EXSR      GestioneSFL
015200080912     C                   ENDSL
015300080912
015400080912     C                   ENDDO
015500080912     C                   ENDSR
015600080912     c**********************************************************************
015700080912     c* Controlli video R01.
015800080912     c**********************************************************************
015900080912     C     Chkvideo1     BEGSR
016000080912      *
016100080912     C                   EVAL      *IN99 = *OFF
016200080912     C                   EVAL      *IN84 = *OFF
016300080912      *
016400080912     C                   ENDSR
016500080912     c**********************************************************************
016600080912     c* inizializza r01
016700080912     c**********************************************************************
016800080912     C     Inzvideo1     BEGSR
016900080912
017000080912     c                   if        o36pou <> 046
017100081016     c                   move      *blank        vnome
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
018400130605     c                   setoff                                       9637
018500080912     c                   select
018600130422     c                   when      v1csce = '1' and richiamato <> *blank
018700130423     c                             and  *in25
018800130424     c                   eval      par_oAut=vi2aut
018900130424     c                   eval      par_oCel=vi2tel
019000130423     c                   eval      $Video=*blanks
019100130422     c
019200130422     c                   when      v1csce = '2' and richiamato <> *blank
019300130423     c                             and not *in25
019400080912     c                   exsr      modifica
019500130423     c
019600081103     c                   when      v1csce = '3' and richiamato <> *blank
019700130423     c                             and not *in25
019800080912     c                   exsr      copia
019900130423     c
020000080912     c                   when      v1csce = '5'
020100080912     c                   seton                                        21
020200080912     c                   exsr      modifica
020300130423     c                   if        richiamato <> *blank  and not *in25
020400080912     c                   setoff                                       21
020500081103     c                   endif
020600130423     c
020700081103     c                   when      v1csce = '4' and richiamato <> *blank
020800130423     c                             and  not *in25
020900080912     c                   seton                                        21
021000080912     c                   exsr      modifica
021100080912     c                   setoff                                       2128
021200080912     c                   clear                   $msg
021300080912     c                   endsl
021400080912     C                   ENDSR
021500080912     c**********************************************************************
021600080912     C     modifica      BEGSR
021700080912     c**********************************************************************
021800081217     c  n21kata          chain     aiata01l                           98
021900081217     c   21kata          chain(n)  aiata01l                           98
022000080912     c                   if        not *in98
022100081016     c                   move      ATaKSC        vATaKSC
022200081016     c                   move      ATaSOC        vATaSOC
022300081112     c                   move      atasoc        socalfa           3
022400081112     c                   clear                   vatasocd
022500081112     c     socalfa       chain     ansif01l
022600081112     c                   if        %found(ansif01l)
022700081112     c                   movel     sifrgs        vatasocd
022800081112     c                   endif
022900081016     c                   move      ATaaut        vATaaut
023000081016     c                   move      ATATEL        vATATEL
023100081016     c                   move      ATAPAT        vATAPAT
023200081016     c                   move      ATACQUAL      vATACQUAL
023300081016     c                   move      ATAFOTO       vATAFOTO
023400081216     c                   move      ATAfinr       vATAfinr
023500081016     c                   move      ATACPENA      vATACPENA
023600081016     c                   move      ATACPEND      vATACPEND
023700080912      *data inizio contratto
023800081016     c                   if        atadtinr > 0
023900081016     c                   move      atadtinr      dataiso
024000080912     c                   move      dataiso       dataeur
024100081016     c                   move      dataeur       vatadtinr
024200080912     c                   else
024300081016     c                   clear                   vatadtinr
024400080912     c                   endif
024500080912      *data fine contratto
024600081016     c                   if        atadtfir > 0
024700081016     c                   move      atadtfir      dataiso
024800080912     c                   move      dataiso       dataeur
024900081016     c                   move      dataeur       vatadtfir
025000081016     c                   else
025100081016     c                   clear                   vatadtfir
025200081016     c                   endif
025300081216      * decodifica motivo fine rapporto
025400081216     c                   clear                   vatafinrd
025500081216     c                   if        vatafinr <> *blank
025600081216     c                   clear                   tibs02ds
025700081216     c                   movel     'C'           t02mod
025800081216     c                   movel     knsif         t02sif
025900081216     c                   movel     'FTR'         t02cod
026000081216     c                   movel     vatafinr      t02ke1
026100081216     c                   call      'TIBS02R'
026200081216     c                   parm                    KPJBA
026300081216     c                   parm                    TIBS02DS
026400081216     c                   if        t02err =  *blanks
026500081216     c                   movel     t02uni        dftr
026600081216     c                   movel     §ftrdes       vatafinrd
026700081216     c                   endif
026800081216     c                   endif
026900081103     c   21              exsr      controv3
027000080912     c                   exsr      Gesvideo3
027100080912     c                   endif
027200080912     C                   ENDSR
027300080912     c**********************************************************************
027400080912     C     copia         BEGSR
027500080912     c**********************************************************************
027501130605     c                   seton                                        37
027502130605     c     kata          chain(n)  aiata01l                           98
027503130605     c                   if        not *in98
027504130605     c                   clear                   vATaKSC
027505130611     c                   clear                   vATarags
027506130605     c                   move      ATaSOC        vATaSOC
027507130605     c                   move      atasoc        socalfa           3
027508130605     c                   clear                   vatasocd
027509130605     c     socalfa       chain     ansif01l
027510130605     c                   if        %found(ansif01l)
027511130605     c                   movel     sifrgs        vatasocd
027512130605     c                   endif
027513130605     c                   move      ATaaut        vATaaut
027514130605     c                   move      ATATEL        vATATEL
027515130605     c                   move      ATAPAT        vATAPAT
027516130605     c                   move      ATACQUAL      vATACQUAL
027517130605     c                   move      ATAFOTO       vATAFOTO
027518130605     c                   move      ATAfinr       vATAfinr
027519130605     c                   move      ATACPENA      vATACPENA
027520130605     c                   move      ATACPEND      vATACPEND
027521130605      *data inizio contratto
027522130605     c                   if        atadtinr > 0
027523130605     c                   move      atadtinr      dataiso
027524130605     c                   move      dataiso       dataeur
027525130605     c                   move      dataeur       vatadtinr
027526130605     c                   else
027527130605     c                   clear                   vatadtinr
027528130605     c                   endif
027529130605      *data fine contratto
027530130605     c                   if        atadtfir > 0
027531130605     c                   move      atadtfir      dataiso
027532130605     c                   move      dataiso       dataeur
027533130605     c                   move      dataeur       vatadtfir
027534130605     c                   else
027535130605     c                   clear                   vatadtfir
027536130605     c                   endif
027537130605      * decodifica motivo fine rapporto
027538130605     c                   clear                   vatafinrd
027539130605     c                   if        vatafinr <> *blank
027540130605     c                   clear                   tibs02ds
027541130605     c                   movel     'C'           t02mod
027542130605     c                   movel     knsif         t02sif
027543130605     c                   movel     'FTR'         t02cod
027544130605     c                   movel     vatafinr      t02ke1
027545130605     c                   call      'TIBS02R'
027546130605     c                   parm                    KPJBA
027547130605     c                   parm                    TIBS02DS
027548130605     c                   if        t02err =  *blanks
027549130605     c                   movel     t02uni        dftr
027550130605     c                   movel     §ftrdes       vatafinrd
027551130605     c                   endif
027552130605     c                   endif
027553130605     c   21              exsr      controv3
027554130605     c                   exsr      Gesvideo3
027555130605     c                   endif
027556130605     C                   ENDSR
027700080912     c**********************************************************************
027800080912     C     gesvideo3     BEGSR
027900080912     c**********************************************************************
028000080912     c                   clear                   fatto
028100080912     c                   do        *hival
028200080912     c                   exfmt     video3
028300080912     c   kl              leave
028400081103     c  n21              exsr      controv3
028500080912     c   96              iter
028600080912     c   kf              exsr      aggiorna
028700080912     c                   if        fatto <> *blank
028800080912     c                   leave
028900080912     c                   endif
029000080912
029100080912     c                   enddo
029200080912     C                   ENDSR
029300080912     c**********************************************************************
029400080912     C     Controv3      BEGSR
029500080912     c**********************************************************************
029600080912      *
029700080912      * *in96 errore generico videata dettaglio
029800080912     c                   setoff                                       9628
029900080912     c                   clear                   $msg
030000080912     c                   if        modalita ='4'
030100080912     c                   goto      endctr
030200080912     c                   endif
030300081016      *codice fornitore
030400130605     c                   setoff                                       585559
030401130605     c                   setoff                                       54
030500081016     c                   if        vataaut =  *blank
030600081016     c                   seton                                        5896
030700080912     c                   endif
030701130605     c                   if        modalita = '3'
030702130605     c                   if        vi2ksc  =  vataksc
030703130605     c                   seton                                        5596
030704130605     c                   endif
030705130605     c     katar3        chain     aiats01l
030706130605     c                   if        not%found(aiats01l)
030707130605     c                   seton                                        5996
030708130605     c                   else
030709130605     c     katar         chain     aiats01l
030710130605     c                   endif
030711130605     c     kata3         chain     aiata01l
030712130605     c                   if        %found(aiata01l)
030713130605     c                   seton                                        5496
030714130605     c                   endif
030715130605     c                   endif
030900081016     c                   movel     vatasoc       societa
031000081016     c                   movel     *zeros        codice
031100081016     c                   move      vataksc       codice
031200081016     c     kfrn          chain     anfrn01l
031300081016     c                   if        not%found(anfrn01l)
031400081016     c                   seton                                        5996
031500081016     c                   else
031600081016     c     frnsogg       chain     ansog01l
031700081016     c                   movel     sogdes        vatarags
031800081016     c                   endif
031900081016     c
032000090108      *codice fornitore
032100090108     c                   setoff                                       57
032200090108     c                   if        vatatel =  *blank
032300090108     c                   seton                                        5796
032400090108     c                   endif
032500081016      *data inizio rapporto
032600080912     c                   setoff                                       53
032700081016     c                   if        vatadtinr =  *zeros
032800080912     c                   seton                                        5396
032900080912     c                   endif
033000081016      *controlli data  inizio rapporto
033100080912     c                   setoff                                       50
033200081016     c                   if        vatadtinr > 0
033300081016     C                   MOVE      Vatadtinr     G02DAT
033400081016     C                   MOVEL     *BLaNK        G02ERR
033500080912     C                   CALL      'XSRDA8'
033600080912     C                   PARM                    WLBDA8
033700080912     C     G02ERR        IFEQ      '1'
033800080912     C                   SETON                                        5096
033900080912     C                   END
034000081016     c                   move      g02dat        vatadtinr
034100081016     c                   move      g02inv        vatadtinrg        8 0
034200080912     c                   endif
034300081016      *controlli data  fine rapporto
034400081016     c                   setoff                                       51
034500081016     c                   if        vatadtfir > 0
034600081016     C                   MOVE      Vatadtfir     G02DAT
034700080912     C                   MOVEL     *BLANK        G02ERR
034800080912     C                   CALL      'XSRDA8'
034900080912     C                   PARM                    WLBDA8
035000080912     C     G02ERR        IFEQ      '1'
035100081016     C                   SETON                                        5196
035200080912     C                   END
035300081016     c                   move      g02dat        vatadtfir
035400081016     c                   move      g02inv        vatadtfirg        8 0
035500080912     c                   endif
035600080912      *controllo data decorrenza maggiore data scadenza
035700081210     c*                  setoff                                       56
035800081210     c*                  if        vatadtinr > vatadtfir
035900081210     c*                  seton                                        5696
036000081210     c*                  endif
036100081016      *controlli inizio validità
036200081216     c                   clear                   vatafinrd
036300081216     c                   if        vatafinr = '?'
036400081216     c                   clear                   tibs02ds
036500081216     c                   movel     'R'           t02mod
036600081216     c                   movel     knsif         t02sif
036700081216     c                   movel     'FTR'         t02cod
036800081216     c                   call      'TIBS02R'
036900081216     c                   parm                    KPJBA
037000081216     c                   parm                    TIBS02DS
037100081216     c                   if        t02err =  *blanks
037200081216     c                   movel     t02uni        dftr
037300081216     c                   movel     §ftrDES       vatafinrd
037400081216     c                   movel     t02ke1        vatafinr
037500081216     c                   goto      endctr
037600081216     c                   endif
037700081216     c                   else
037800081216     c                   if        vatafinr <> *blank
037900081216     c                   clear                   tibs02ds
038000081216     c                   movel     'C'           t02mod
038100081216     c                   movel     knsif         t02sif
038200081216     c                   movel     'FTR'         t02cod
038300081216     c                   movel     vatafinr      t02ke1
038400081216     c                   call      'TIBS02R'
038500081216     c                   parm                    KPJBA
038600081216     c                   parm                    TIBS02DS
038700081216     c                   if        t02err =  *blanks
038800081216     c                   movel     t02uni        dftr
038900081216     c                   movel     §ftrDES       vatafinrd
039000081216     c                   endif
039100081216     c                   endif
039200081216     c                   endif
039300090108      *controllo campi s/n
039400090108     c                   setoff                                       35
039500090108     c                   if        vatapat = *blank
039600090108     c                   seton                                        3596
039700090108     c                   endif
039800090108     c                   setoff                                       31
039900090108     c                   if        vatacqual = *blank
040000090108     c                   seton                                        3196
040100090108     c                   endif
040200090108     c                   setoff                                       32
040300090108     c                   if        vatafoto = *blank
040400090108     c                   seton                                        3296
040500090108     c                   endif
040600090108     c                   setoff                                       33
040700090108     c                   if        vatacpena = *blank
040800090108     c                   seton                                        3396
040900090108     c                   endif
041000090108     c                   setoff                                       34
041100090108     c                   if        vatacpend = *blank
041200090108     c                   seton                                        3496
041300090108     c                   endif
041400080912     C     endctr        ENDSR
041500080912     c**********************************************************************
041600080912     C     aggiorna      BEGSR
041700080912     c**********************************************************************
041800081215     c                   move      vATATEL       ATATEL
041900081215     c                   move      vATAPAT       ATAPAT
042000081215     c                   move      vATACQUAL     ATACQUAL
042100081215     c                   move      vATAFOTO      ATAFOTO
042200081216     c                   move      vATAfinr      ATAFinr
042300081215     c                   move      vATACPENA     ATACPENA
042400081215     c                   move      vATACPEND     ATACPEND
042500081016     c                   if        vatadtinr > 0
042600081016     c                   move      vatadtinr     dataeur
042700080912     c                   move      dataeur       dataiso
042800081016     c                   move      dataiso       atadtinr
042900080912     c                   endif
043000081016     c                   if        vatadtfir > 0
043100081016     c                   move      vatadtfir     dataeur
043200081016     c                   move      dataeur       dataiso
043300081016     c                   move      dataiso       atadtfir
043400090109     c                   else
043500090109     c                   z-add     0             atadtfir
043600081016     c                   endif
043700080912      * inserimento e copia scrive altrimenti aggiorna il record esistente
043800080912     c                   select
043900080912     c                   when      modalita = '4'
044000081016     c                   delete    aiata000
044100080912     c                   when      modalita = *blank or modalita = '3'
044200081028     c                   move      vATaKSC       ATaKSC
044300081028     c                   move      vATaSOC       ATaSOC
044400081028     c                   move      vATaaut       ATaaut
044500081016     c                   write     aiata000
044600080912     c                   other
044700081016     c                   update    aiata000
044800080912     c                   endsl
044900080912     c                   move      'X'           fatto
045000080912     C                   ENDSR
045100080912     c**********************************************************************
045200080912     c* carica sfl
045300080912     c**********************************************************************
045400080912     C     Carvideo2     BEGSR
045500080912     C                   IF        WrkCarS01 = *ON
045600080912     C                   EVAL      snrr1   = 0
045700080912     C                   EVAL      *IN90 = *ON
045800080912     C                   EVAL      *IN91 = *OFF
045900080912     C                   WRITE     video2c
046000080912     C                   EVAL      *IN90 = *OFF
046100081029     c                   if        richiamato = *blank
046200100104     c                   if        atsdtfic > 0
046300100104     c     katar         setll     aiata01l
046400100104     c                   else
046500081029     c     vnome         setll     aiata02l
046600100104     c                   endif
046700081029     c                   else
046800081029     c     katar         setll     aiata01l
046900081029     c                   endif
047000081029     c                   do        *hival
047100081029     c                   if        richiamato = *blank
047200100104     c                   if        atsdtfic > 0
047300100104     c     katar         reade(n)  aiata01l                               97
047400100104     c                   else
047500081029     c                   read      aiata02l                               97
047600100104     c                   endif
047700081029     c                   else
047800081029     c     katar         reade(n)  aiata01l                               97
047900081029     c                   endif
048000081029     c   97              leave
048100081112      * escludo disaccreditati
048200081112     c                   if        vdis = 'N' and atadtfir > 0
048300081112     c                   iter
048400081112     c                   endif
048500081112
048600080912     c                   move      *blanks       v1csce
048700081016     c                   move      ataSOC        VI2SOC
048800081016     c                   move      ataKSC        VI2KSC
048900081016     c                   move      ataaut        VI2aut
049000081016     c                   move      atatel        VI2tel
049100081016      *data inizio contratto
049200081016     c                   if        atadtinr> 0
049300081016     c                   move      atadtinr      dataiso
049400080912     c                   move      dataiso       dataeur
049500081016     c                   move      dataeur       vi2dtinr
049600080912     c                   else
049700081016     c                   clear                   vi2dtinr
049800080912     c                   endif
049900081029      *data inizio contratto
050000081029     c                   if        atadtfir> 0
050100081029     c                   move      atadtfir      dataiso
050200081029     c                   move      dataiso       dataeur
050300081029     c                   move      dataeur       vi2dtfir
050400081029     c                   else
050500081029     c                   clear                   vi2dtfir
050600081029     c                   endif
050700080912      *esco per raggiunta massima capacità sfl
050800080912     c                   if        snrr1 > 9990
050900080912     c                   leave
051000080912     c                   endif
051100080912
051200080912     C                   EVAL      snrr1 = snrr1 + 1
051300080912     C                   WRITE     video2
051400080912     C                   ENDDO
051500080912
051600080912     C                   IF        snrr1  > 0
051700080912     C                   EVAL      *IN91 = *ON
051800080912     C                   MOVE      1             OK                1 0
051900080912     C                   ELSE
052000080912     C                   MOVE      0             OK                1 0
052100080912     C                   ENDIF
052200080912     C                   EVAL      nrr1  = 1
052300080912     C                   ENDIF
052400081215     C                   EVAL      WrkCarS01 = *On
052500080912     C                   EVAL      snrr1  = 1
052600080912     C                   EVAL      csrrrn = 1
052700130515     c                   clear                   vi2posiz
052800080912     C                   ENDSR
052900080912     c**********************************************************************
053000080912     c* gestione sfl
053100080912     c**********************************************************************
053200080912     C     GestioneSFL   BEGSR
053300080912      *
053400080912     C                   EVAL      WrkEofS01 = *OFF
053500080912      * F10 Immissione.
053600081103     C                   IF        *INKJ and richiamato <> *blank
053700080912     c                   clear                   modalita
053800080912     c                   clear                   video3
053900081016     c                   clear                   aiata000
054000081216     c                   seton                                        24
054100081016     c                   z-add     vi2soc        vatasoc
054200081016     c                   z-add     vi2ksc        vataksc
054300080912     c                   move      knmus         vknmus
054400080912     c                   move      knsif         vknsif
054500080912     c                   movel     ragut         rsut
054600080912     c                   exsr      Gesvideo3
054700081216     c                   setoff                                       24
054800080912     c                   eval      wrkcars01 = *on
054900080912     c                   if        csrrrn = 0
055000080912     c                   goto      noriga
055100080912     c                   endif
055200080912     c                   endif
055300130515     c
055400080912      * Elaborazione opzioni.
055500080912     c*
055600130515     c                   clear                   nrr1
055700080912     c                   do        *hival        X                 4 0
055800080912     c     X             CHAIN     video2                             50
055900080912     C   50              LEAVE
056000080912     C                   IF        v1csce <> *blanks
056100080912     c                   exsr      opzione
056200080912
056300080912     c                   move      *blank        v1csce
056400080912     c                   update    video2
056500130422     c
056600130422     c* se ho effettuato una scelta esco dal pgm
056700130422     c                   if        *in25 and par_oaut<>*blanks
056800130422     c                   leavesr
056900130422     c                   endif
057000130515     c* se ho scritto posizionamento --> eseguo
057100130515     c                   else
057200130515     c
057300130515     c                   if        vi2posiz<>*blanks and vi2posiz<=vi2aut
057400130515     c                             and nrr1=0
057500130515     c                   eval      nrr1=x
057600130515     c                   endif
057700130515     c
057800130515     c                   if        vi2posiz=*blanks
057900130515     c                   eval      nrr1=csrrrn
058000130515     c                   endif
058100080912
058200080912     C                   ENDIF
058300080912     C                   enddo
058400080912      *
058500080912     c                   if        modalita = '1' or modalita = '3'
058600090108     c                             or modalita = '2'
058700080912     c                   eval      wrkcars01 = *on
058800080912     c                   endif
058900080912      *
059000080912     C     noriga        ENDSR
059100080912     c**********************************************************************
059200080912     C     *INZSR        BEGSR
059300080912     c**********************************************************************
059400080912      *
059500080912     C     *ENTRY        PLIST
059600080912     C                   PARM                    Kpjba
059700081028      *richiamato da altri
059800081028     c                   clear                   param
059900130423     c                   clear                   amg_drf
060000130423     c
060100081028     c                   if        kpjbu <> *blank
060200100104      *
060300081028     c                   movel     kpjbu         param
060400081028     C                   z-add     parsoc        vi2soc
060500081028     C                   z-add     parksc        vi2ksc
060600081028     c                   movel     'X'           richiamato        1
060700100104     c     katar         chain     aiats01l
060800100104     c                   if        %found(aiats01l)
060900100104     c                   if        atsdtfic > 0
061000100104     c                   move      *blank        richiamato
061100100104     c                   endif
061200100104     c                   endif
061300130424     c
061400130424     c* Se non passato fornitore o non più accreditato -> non lo visualizzo
061500130424     c                   if        richiamato=*blanks or not %found(aiats01l)
061600130424     c                   seton                                        26
061700130424     c                   endif
061800100104      *
061900130422      * Se richiamato in modalità scelta --> emetto "1"
062000130422     c                   if        parmod='1'
062100130423     c                   seton                                        2521
062200130422     c                   clear                   par_oaut
062300130424     c                   clear                   par_ocel
062400130423     c                   eval      vdis='N'
062500081028     c                   endif
062600130423     c
062700130423     C* Se data riferimento piena --> la uso per caricare
062800130423     c                   if        pardrf>*zeros
062900130423     c                   movel     pardrf        AMG_drf           8 0
063000130423     c                   endif
063100130423     c
063200130422     c                   endif
063300130422     c
063400080912     C                   Z-ADD     1             CODUT
063500080912     C                   CALL      'X§PARUT'
063600080912     C                   PARM                    UTEDSE
063700080912     C                   MOVEL     REC80         CNCR80
063800080912     C                   MOVEL     Ragut         rsut
063900080912     C                   MOVEL     knsif         vknsif
064000080912     C                   MOVEL     knmus         vknmus
064100080912      *reperimento data
064200080912     C                   TIME                    W0120            14 0
064300080912     C                   MOVE      W0120         WDAT              8 0
064400080912     C*
064500080912     C                   Z-ADD     WDAT          G02DAT
064600080912     C                   MOVEL     *BLANK        G02ERR
064700080912     C                   CALL      'XSRDA8'
064800080912     C                   PARM                    WLBDA8
064900080912     C* UDATE A 8 IN AAAA/MM/GG
065000080912     C                   Z-ADD     G02INV        WUDA              8 0
065100080912      *profilo di filiale attivo indicatore di protezione campi non manutenziona
065200080912     C                   CLEAR                   Tibs36Ds
065300080912     C                   EVAL      I36ute = knmus
065400080912     C                   CALL      'TIBS36R'
065500080912     C                   PARM                    tibs36ds
065600081112      * profilo di filiale
065700081112     c                   if        o36pou <> 046
065800081112     c                   seton                                        30
065900081112     c                   endif
066000081029      *decodifica fornitore
066100081029     c                   movel     vi2soc        societa
066200081029     c                   movel     *zeros        codice
066300081029     c                   move      vi2ksc        codice
066400081029     c     kfrn          chain     anfrn01l
066500081029     c                   if        not%found(anfrn01l)
066600081029     c                   seton                                        5996
066700081029     c                   else
066800081029     c     frnsogg       chain     ansog01l
066900081029     c                   movel     sogdes        vi2rasd
067000081029     c                   endif
067100080912
067200081016     C     Kfrn          KLIST
067300081016     C                   KFLD                    societa           3
067400081016     C                   KFLD                    codice            8
067500080912      *
067600081016     C     Kata          KLIST
067700081016     C                   KFLD                    vi2soc
067800081016     C                   KFLD                    vi2ksc
067900081016     C                   KFLD                    vi2aut
068000081029     C     Katar         KLIST
068100081029     C                   KFLD                    vi2soc
068200081029     C                   KFLD                    vi2ksc
068201130605     C     Katar3        KLIST
068202130605     C                   KFLD                    vi2soc
068203130605     C                   KFLD                    vataksc
068204130605     C     Kata3         KLIST
068205130605     C                   KFLD                    vi2soc
068206130605     C                   KFLD                    vataksc
068207130605     C                   KFLD                    vataaut
068300080912      *---------------------------------------------------------------------
068400080912     C                   ENDSR
