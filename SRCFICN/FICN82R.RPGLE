000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300070625     Fficn82d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030709     Ffiatt01l  uf a E           k disk
000501130524     Ffiatt11l  if   E           k disk
000600030709     Fazorg01l  if   E           k disk
000700030714     Ffiapd01l  if   E           k disk
000800070626     Ftntbe01l  if   E           k disk
000900011228      *
001000020103     D Psds           SDS
001100020103     D  PgmName          *PROC
001200011228     D Kpjba         E DS
001300011228      *
001400030702     D fnlv55ds      E DS
001401100617     d dvpocont      e ds
001402100617     d tibs02ds      e ds
001500030619     D trul33ds      E DS
001600030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001700030604     D CNCR80        E DS
001800070626     D dbld          E DS
001900030617     d Tibs36Ds      e ds
002000011227     d data            ds            10
002100011228     d aa                      3      4
002200011228     d mm                      6      7
002300011228     d gg                      9     10
002400011228      *
002500030714     D errter          S              1
002600030702     D fatto           S              1
002700030610     D modalita        S              1
002800030711     D prime_time      S              1    inz('X')
002900030604     D x               S              4  0
003000011227     D WrkEofS01       S              1
003100011227     D WrkCarS01       S              1
003200011228     D $VIDEO          S             10
003300030604     D snrr1           S              5i 0
003400030604     D SavOpzione      S                   like(v1csce)
003500030709     D Savattter       S                   like(vattter)
003600030709     D Savattterd      S                   like(vattterd)
003700030606     d dataiso         s               d   datfmt(*iso)
003800030606     d dataeur         s               d   datfmt(*eur)
003900030613     D WLBDA8          DS
004000030613     D  G02DAT                 1      8  0
004100030613     D  G02INV                 9     16  0
004200030613     D  G02ERR                17     17
004300030613     D  G02TGI                18     22  0
004400030701      *---------------- ----------------------------------------------------
004500030604      *---------------- Gestione formato video video01----------------------
004600030604     C                   EXSR      Inzvideo1
004700030604     C                   DOU       $Video <> 'VIDEO1'
004800000608      *----------- Visualizzo dati di output in caso di errori.-------------
004900000608     C                   IF        *IN99
005000000608     C                   EVAL      *IN99 = *OFF
005100030604     C                   WRITE     video1
005200000608     C                   EVAL      *IN99 = *ON
005300000608     C                   ENDIF
005400000608      *
005500030702     c     emetti        tag
005600030604     C                   EXFMT     video1
005700000608      *
005800011227     C                   SELECT
005900030612     C                   WHEN      *INKC
006000011227     C                             OR
006100011227     C                             *INKL
006200011227     C                   EVAL      $Video = *BLANKS
006300011227
006400011227     C                   WHEN      *INKE
006500030604     C                   EXSR      Inzvideo1
006600011227     C                   WHEN      *INKB
006700030604     C                   EXSR      Chkvideo1
006800011227     C                   OTHER
006900030604     C                   EXSR      Chkvideo1
007000030716     c   44              goto      emetti
007100030604     C  N99              EXSR      Wrkvideo2c
007200031020     C     OK            CABEQ     0             EMETTI
007300011227     C                   ENDSL
007400011227      *
007500011227     C                   ENDDO
007600011227      *
007700011227     C                   EXSR      Uscita
007800011227     c**********************************************************************
007900011227     c* uscita
008000011227     c**********************************************************************
008100011227     C     Uscita        BEGSR
008200011227      *
008300011227     C                   EVAL      *INLR = *ON
008400011227     C                   RETURN
008500011227      *
008600011227     C                   ENDSR
008700011227     c**********************************************************************
008800011227     c* gestione sfl
008900011227     c**********************************************************************
009000030604     C     Wrkvideo2c    BEGSR
009100000627      *
009200011227     C                   EVAL      WrkCarS01 = *ON
009300030604     C                   EVAL      $Video = 'VIDEO2C'
009400011227      *
009500030604     C                   DOU       $Video <> 'VIDEO2C'
009600030604     C                   EXSR      Carvideo2
009700011228     c*Descrizione della ricerca
009800000627      *
009900031020     C     OK            CABEQ     0             TORNA
010000030604     C                   WRITE     video2z
010100030604     C                   EXFMT     video2c
010200011227     C                   SELECT
010300011228     c* fine
010400011227     C                   WHEN      *INKC=*ON
010500011227     C                   EVAL      $VIDEO=*BLANKS
010600011228     c* guida
010700011227     C                   WHEN      *INKL=*ON
010800030604     C                   EVAL      $VIDEO='VIDEO1'
010900030715
011000030715     C                   WHEN      *INKa=*ON
011100030715     C                   exsr      spalmaopz
011200011227
011300011227     C                   OTHER
011400011227
011500011227     C                   EXSR      GestioneSFL
011600011227     C                   ENDSL
011700011228
011800011227     C                   ENDDO
011900031020     C     torna         ENDSR
012000011227     c**********************************************************************
012100011227     c* Controlli video R01.
012200011227     c**********************************************************************
012300030604     C     Chkvideo1     BEGSR
012400000609      *
012500000608     C                   EVAL      *IN99 = *OFF
012600030702     C                   EVAL      *IN44 = *OFF
012700060915      * profilo di filiale controllo che il terminal sia congruente
012800060915     c                   if        o36pou <> 046
012900060915     c                   if        vattter <> terma and vattter <> termp
013000060915     c                   seton                                        44
013100060915     c                   endif
013200060915     c                   else
013300030702     c
013400030702     c                   clear                   fnlv55ds
013500030702     c                   move      'L'           d55tla
013600030709     c                   move      vattter       d55lin
013601100617     c                   if        wuda >= §vpoter
013602100617     c                   move      'E'           d55tpt
013603100617     c                   else
013604100617     c                   move      ' '           d55tpt
013605100617     c                   endif
013800030702     c                   z-add     wuda          d55drf
013900030702     c                   call      'FNLV55R'
014000030702     c                   parm                    fnlv55ds
014100030709     c                   if        vattter <> d55tfp and vattter <> d55tfa
014200030709     c                             or vattter = *zeros
014300030702     c                   seton                                        44
014400030702     c                   endif
014500060915     c                   endif
014600030709     c     vattter       chain     azorg01l
014700030709     c                   movel     orgdes        vattterd
014800070626     C                   SETOFF                                       43
014900030709     c                   if        vdata1 > 0
015000030715     c                   move      vdata1        g02dat
015100030715     c                   move      *blank        g02err
015200030715     C                   CALL      'XSRDA8'
015300030715     C                   PARM                    WLBDA8
015400030715     C     G02ERR        IFEQ      '1'
015500030715     C                   SETON                                        43
015600030715     C                   END
015700030715     c                   move      g02dat        vdata1
015800030715     c                   move      g02inv        wuda
015900070626     c                   move      g02inv        vdata1g
016000070626     c                   move      g02inv        vdata1gi
016100070626     c                   move      01            vdata1gi
016200030709     c                   endif
016300000608      *
016400000608     C                   ENDSR
016500011227     c**********************************************************************
016600011227     c* inizializza r01
016700011227     c**********************************************************************
016800030604     C     Inzvideo1     BEGSR
016900000608
017000030702     c                   if        o36pou <> 046
017100060915     c                   move      termp         vattter
017200030702     c                   endif
017300030604     C                   EXSR      Chkvideo1
017400000609
017500000608     C                   ENDSR
017600030604     c**********************************************************************
017700030605     C     opzione       BEGSR
017800030605     c**********************************************************************
017900030610     c                   move      v1csce        modalita
018000030605     c                   select
018100030710     c                   when      v1csce = '1'
018200030717     c                   exsr      conferma
018300030717     c                   clear                   $msg
018400030717     c                   setoff                                       28
018500030605     c                   when      v1csce = '2'
018600030605     c                   exsr      modifica
018700030717     c   21              clear                   $msg
018800030717     c                   setoff                                       2128
018900030605     c                   when      v1csce = '5'
019000030606     c                   seton                                        21
019100030606     c                   exsr      modifica
019200030606     c                   setoff                                       21
019300030605     c                   endsl
019400030604     C                   ENDSR
019500030717     c**********************************************************************
019600030717     C     conferma      BEGSR
019700030717     c**********************************************************************
019800030717     c                   setoff                                       28
019900030717     c     katt1         chain     fiatt01l                           98
020000030717     c                   if        not *in98
020100030717     c                   if        attdco > 0
020200030717     c                   seton                                        28
020300030717     c                   eval      $msg = 'Viaggio già confermato non è +
020400030717     c                             possibile modificare i dati'
020500030717     c                   endif
020600030717     c                   if        attdco = 0 and attorar = 0 or attimpp = 0
020700030717     c                   seton                                        28
020800030717     c                   eval      $msg = 'Viaggio non confermabile mancano i +
020900030717     c                             dati di arrivo o l''importo '
021000030717     c                   endif
021100030717     c                   move      attVAD        vattVAD
021200030717     c                   move      attTER        vattTER
021300030717     c                   move      attFGP        vattFGP
021400030717     c                   move      attFGA        vattFGA
021500030717     c                   move      attDDc        vattDDc
021600030717     c                   move      attORPr       vattORPr
021700030717     c                   move      attORAr       vattORAr
021800030717     c                   move      attPRV        vattPRV
021900030728     c                   move      attPRg        vattPRg
022000030728     c                   move      attnot        vattnot
022001130524     c                   movel(p)  attpmb        vattpmb
022002130524     c     katt1         chain     fiatt11l
022003130524     c                   if        %found(fiatt11l)
022004130524     c                   move      at1pmb        vattpmb
022005130524     c                   end
022100030717     c                   z-add     attIMP        vattIMP
022200030717     c                   z-add     attIMPp       vattIMPp
022300030717     c                   move      attPDR        vattPDR
022400030717     c     kapd          chain     fiapd01l                           95
022500090507     c  n95              movel     apdrsf        vattpdrd
022600030717     c   95              clear                   vattpdrd
022700030717      * decodifica p.o.
022800030717     c     attfgp        chain     azorg01l                           95
022900030717     c  n95              movel     orgdes        vattfgpd
023000030717     c   95              clear                   vattfgpd
023100030717     c     attfga        chain     azorg01l                           95
023200030717     c  n95              movel     orgdes        vattfgad
023300030717     c   95              clear                   vattfgad
023400030717      *data decorrenza
023500030717     c                   if        attddc > 0
023600030717     c                   move      attddc        dataiso
023700030717     c                   move      dataiso       dataeur
023800030717     c                   move      dataeur       vattddc
023900030717     c                   else
024000030717     c                   clear                   vattddc
024100030717     c                   endif
024200030717     c                   exsr      Gesvideo3
024300030717     c                   endif
024400030717     C                   ENDSR
024500030605     c**********************************************************************
024600030605     C     modifica      BEGSR
024700030605     c**********************************************************************
024800030710     c     katt1         chain     fiatt01l                           98
024900030610     c                   if        not *in98
025000030717     c                   if        attdco > 0 and modalita ='2'
025100030717     c                   seton                                        2128
025200030717     c                   eval      $msg = 'Viaggio già confermato non è +
025300030717     c                             possibile modificare i dati'
025400030717     c                   endif
025500030709     c                   move      attVAD        vattVAD
025600030709     c                   move      attTER        vattTER
025700030709     c                   move      attFGP        vattFGP
025800030709     c                   move      attFGA        vattFGA
025900030709     c                   move      attDDc        vattDDc
026000030716     c                   move      attORPr       vattORPr
026100030716     c                   move      attORAr       vattORAr
026200030709     c                   move      attPRV        vattPRV
026300030728     c                   move      attPRg        vattPRg
026400030709     c                   z-add     attIMP        vattIMP
026500030709     c                   z-add     attIMPp       vattIMPp
026600030709     c                   move      attPDR        vattPDR
026700030714     c     kapd          chain     fiapd01l                           95
026800090507     c  n95              movel     apdrsf        vattpdrd
026900030714     c   95              clear                   vattpdrd
027000030709     c                   move      attNOT        vattNOT
027100130524     c                   movel(p)  attpmb        vattpmb
027101130524     c     katt1         chain     fiatt11l
027102130524     c                   if        %found(fiatt11l)
027103130524     c                   move      at1pmb        vattpmb
027104130524     c                   end
027200030701      * decodifica p.o.
027300030709     c     attfgp        chain     azorg01l                           95
027400030709     c  n95              movel     orgdes        vattfgpd
027500030709     c   95              clear                   vattfgpd
027600030709     c     attfga        chain     azorg01l                           95
027700030709     c  n95              movel     orgdes        vattfgad
027800030709     c   95              clear                   vattfgad
027900030701      *data decorrenza
028000030709     c                   if        attddc > 0
028100030709     c                   move      attddc        dataiso
028200030701     c                   move      dataiso       dataeur
028300030709     c                   move      dataeur       vattddc
028400030701     c                   else
028500030709     c                   clear                   vattddc
028600030701     c                   endif
028700030605     c                   exsr      Gesvideo3
028800030605     c                   endif
028900030605     C                   ENDSR
029000030605     c**********************************************************************
029100030605     C     gesvideo3     BEGSR
029200030605     c**********************************************************************
029300030702     c                   clear                   fatto
029400030605     c                   do        *hival
029500030605     c                   exfmt     video3
029600030605     c   kl              leave
029700030624     c                   exsr      controv3
029800030717     c   28
029900030717     cor 96              iter
030000030605     c   kf              exsr      aggiorna
030100030702     c                   if        fatto <> *blank
030200030702     c                   leave
030300030702     c                   endif
030400030605
030500030605     c                   enddo
030600030605     C                   ENDSR
030700030605     c**********************************************************************
030800030624     C     Controv3      BEGSR
030900030605     c**********************************************************************
031000030613      *
031100030702      * *in96 errore generico videata dettaglio
031200030703     c                   setoff                                       96
031300030703      * decodifica p.o.
031400030709     c     vattfgp       chain     azorg01l                           95
031500030709     c  n95              movel     orgdes        vattfgpd
031600030709     c   95              clear                   vattfgpd
031700030709     c     vattfga       chain     azorg01l                           95
031800030709     c  n95              movel     orgdes        vattfgad
031900030709     c   95              clear                   vattfgad
032000030702      *Importo obbligatorio
032100030702     c                   setoff                                       49
032200030709     c                   if        vattimpp =  *zeros
032300030702     c                   seton                                        4996
032400030702     c                   endif
032500030716     c                   endsr
032600030605     c**********************************************************************
032700030605     C     aggiorna      BEGSR
032800030605     c**********************************************************************
032900030717     c                   if        modalita = '1'
033000030828     c                   move      'C'           attflg
033100030717     c                   z-add     wudate        attdco
033200030717     c                   endif
033300030717     c                   z-add     vattIMPp      attIMPp
033400030717     c                   z-add     vattIMPp      vattIMPp2
033500030717     c                   movel     vattnot       attnot
033600030709     c                   update    fiatt000
033700030702     c                   move      'X'           fatto
033800030605     C                   ENDSR
033900030715     c**********************************************************************
034000030715     C     spalmaopz     BEGSR
034100030715     c**********************************************************************
034200030715     c                   do        *hival        recor             4 0
034300030715     c     recor         chain     video2                             93
034400030715     c   93              leave
034500030715     c                   move      '1'           v1csce
034600030715     c                   update    video2
034700030715     c                   enddo
034800030715     C                   ENDSR
034900011228     c**********************************************************************
035000011228     c* carica sfl
035100011228     c**********************************************************************
035200030604     C     Carvideo2     BEGSR
035300011228     C                   IF        WrkCarS01 = *ON
035400030604     C                   EVAL      snrr1   = 0
035500011228     C                   EVAL      *IN90 = *ON
035600020219     C                   EVAL      *IN91 = *OFF
035700030604     C                   WRITE     video2c
035800011228     C                   EVAL      *IN90 = *OFF
035900030711     c     kattd         setll     fiatt01l
036000030701     c                   do        *hival
036100070827     c     katt          reade(n)  fiatt01l                               97
036200030701     c   97              leave
036300070625      * scarto i viaggi confermati
036400070625     c                   if        attdco > 0
036500070625     c                   iter
036600070625     c                   endif
036700070625      * scarto i viaggi con data maggiore di quella richiesta a video
036800070626     c                   if        attddc > vdata1g or attddc < vdata1gi
036900070625     c                   iter
037000070625     c                   endif
037100070625      *
037200030701     c                   move      *blanks       v1csce
037300070625     c                   move      attddc        vattddcg
037400030709     c                   move      attVAD        vattVAD
037500030709     c                   move      attTER        vattTER
037600031110     c                   z-add     attprg        vattprg
037700030709     c     attter        chain     azorg01l                           95
037800030709     c  n95              movel     orgdes        vattterd
037900030709     c   95              clear                   vattterd
038000030709     c                   move      attFGP        vattFGP
038100030709     c     attfgp        chain     azorg01l                           95
038200030709     c  n95              movel     orgdes        vattfgpd2
038300030709     c   95              clear                   vattfgpd2
038400030709     c                   move      attFGA        vattFGA
038500030709     c     attfga        chain     azorg01l                           95
038600030709     c  n95              movel     orgdes        vattfgad2
038700030709     c   95              clear                   vattfgad2
038800030717     c                   move      attORAr       vattORAr
038900030709     c                   move      attPRV        vattPRV
039000030710     c                   move      attPRg        vattPRg
039100030709     c                   move      attPDR        vattPDR
039200030716     c     kapd          chain     fiapd01l                           95
039300090507     c  n95              movel     apdrsf        vattpdrd2
039400030716     c   95              clear                   vattpdrd2
039500030709     c                   z-add     attimpp       vattimpp2
039600030709      *data prestazione
039700030709     c                   if        attddc > 0
039800030709     c                   move      attddc        dataiso
039900030701     c                   move      dataiso       dataeur
040000030709     c                   move      dataeur       vattddc
040100030701     c                   else
040200030709     c                   clear                   vattddc
040300030701     c                   endif
040400031110      *data conferma
040500031110     c                   if        attdco > 0
040600031110     c                   move      attdco        dataiso
040700031110     c                   move      dataiso       dataeur
040800031110     c                   move      dataeur       vattdco
040900031110     c                   else
041000031110     c                   clear                   vattdco
041100031110     c                   endif
041200030701      *esco per raggiunta massima capacità sfl
041300030617     c                   if        snrr1 > 9990
041400030617     c                   leave
041500030617     c                   endif
041600030617
041700030604     C                   EVAL      snrr1 = snrr1 + 1
041800030604     C                   WRITE     video2
041900011228     C                   ENDDO
042000011228
042100030604     C                   IF        snrr1  > 0
042200020219     C                   EVAL      *IN91 = *ON
042300020219     C                   MOVE      1             OK                1 0
042400020219     C                   ELSE
042500020219     C                   MOVE      0             OK                1 0
042600030701     C                   ENDIF
042700030612     C                   EVAL      nrr1  = 1
042800011228     C                   ENDIF
042900020219     C                   EVAL      WrkCarS01 = *OFF
043000030604     C                   EVAL      snrr1  = 1
043100030604     C                   EVAL      csrrrn = 1
043200011228     C                   ENDSR
043300011227     c**********************************************************************
043400011227     c* gestione sfl
043500011227     c**********************************************************************
043600000613     C     GestioneSFL   BEGSR
043700000627      *
043800000627     C                   EVAL      WrkEofS01 = *OFF
043900030605      * Elaborazione opzioni.
044000011228     c*
044100030605     c                   do        *hival        X                 4 0
044200030605     c     X             CHAIN     video2                             50
044300030605     C   50              LEAVE
044400030604     C                   IF        v1csce <> *blanks
044500030605     c                   exsr      opzione
044600030611
044700030611     c                   move      *blank        v1csce
044800030605     c                   update    video2
044900030611
045000011228     C                   ENDIF
045100030605     C                   enddo
045200030710     c                   if        modalita = '1'
045300030710     c                   eval      wrkcars01 = *on
045400030710     c                   endif
045500000627      *
045600030703     C                   ENDSR
045700011227     c**********************************************************************
045800000607     C     *INZSR        BEGSR
045900011227     c**********************************************************************
046000000607      *
046100000607     C     *ENTRY        PLIST
046200000607     C                   PARM                    Kpjba
046300030604     C                   Z-ADD     1             CODUT
046400030604     C                   CALL      'X§PARUT'
046500030604     C                   PARM                    UTEDSE
046600030604     C                   MOVEL     REC80         CNCR80
046700030703     C                   MOVEL     Ragut         rsut
046800030703     C                   MOVEL     knsif         vknsif
046900030703     C                   MOVEL     knmus         vknmus
047000030702      *reperimento data
047100030702     C                   TIME                    W0120            14 0
047200030702     C                   MOVE      W0120         WDAT              8 0
047300030702     C*
047400030702     C                   Z-ADD     WDAT          G02DAT
047500030702     C                   MOVEL     *BLANK        G02ERR
047600030702     C                   CALL      'XSRDA8'
047700030702     C                   PARM                    WLBDA8
047800030702     C* UDATE A 8 IN AAAA/MM/GG
047900030702     C                   Z-ADD     G02INV        WUDA              8 0
048000030717     C                   Z-ADD     G02INV        WUDAte            8 0
048100030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
048200070827      * data modificabile solo da EDP
048300070827     c                   setoff                                       23
048400070827     c                   if        %subst(knmus:1:3) <> 'EDP'
048500070827     c                   seton                                        23
048600070827     c                   endif
048700070827      *
048800030617     C                   CLEAR                   Tibs36Ds
048900030617     C                   EVAL      I36ute = knmus
049000030617     C                   CALL      'TIBS36R'
049100030617     C                   PARM                    tibs36ds
049101100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
049102100617     c                   clear                   tibs02ds
049103100617     c                   clear                   dvpocont
049104100617     c                   eval      t02mod = 'C'
049105100617     c                   eval      t02sif = knsif
049106100617     c                   eval      t02cod = 'VPO'
049107100617     c                   eval      t02ke1 = 'CONT'
049108100617     c                   call      'TIBS02R'
049109100617     c                   parm                    kpjba
049110100617     c                   parm                    tibs02ds
049111100617    2c                   if        t02err = *blanks
049112100617     c                   eval      dvpocont  = t02uni
049113100617    2c                   endif
049200060915     c                   if        o36pou <> 046
049300060915     c                   clear                   fnlv55ds
049400060915     c                   move      'L'           d55tla
049500060915     c                   move      o36pou        d55lin
049501100617     c                   if        wuda >= §vpoter
049502100617     c                   move      'E'           d55tpt
049503100617     c                   else
049504100617     c                   move      ' '           d55tpt
049505100617     c                   endif
049700060915     c                   z-add     wuda          d55drf
049800060915     c                   call      'FNLV55R'
049900060915     c                   parm                    fnlv55ds
050000060915     c                   z-add     d55tfp        termp             3 0
050100060915     c                   z-add     d55tfa        terma             3 0
050200060915      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
050300060915     c                   if        terma = termp
050400060915     c                   seton                                        20
050500060915     c                   endif
050600060915     c                   endif
050700000607      *
050800030710     C     Katt1         KLIST
050900030709     C                   KFLD                    vattter
051000070625     C                   KFLD                    vattddcG
051100030709     C                   KFLD                    vattvad
051200030709     C                   KFLD                    vattfgp
051300030709     C                   KFLD                    vattfga
051400030710     C                   KFLD                    vattprg
051500030709     C     Katt          KLIST
051600030709     C                   KFLD                    vattter
051700030711     C     Kattd         KLIST
051800030711     C                   KFLD                    vattter
051900070626     c                   kfld                    vdata1gi
052000070625      *
052100030714     C     Kapd          KLIST
052200030714     C                   KFLD                    tipoa             1
052300030714     C                   KFLD                    vattpdr
052400070626     C                   move      'D'           tipoa
052500070626     C     Ktbe          KLIST
052600070626     C                   KFLD                    tbecod
052700070626     C                   KFLD                    tbeke1
052800070626      * reperimento periodo da tabella bld tramite data protocollo
052900070626     c                   clear                   dbld
053000070626     c                   movel(p)  '1'           tbeke1
053100070626     c                   movel     'BLD'         tbecod
053200070626     c     ktbe          chain     tntbe01l
053300070626     c                   if        %found(tntbe01l)
053400070626     c                   movel     tbeuni        dbld
053500070626     c* data protocollo
053600070626     c                   move      §blddtp       dataiso
053700070626     c                   move      dataiso       dataeur
053800070626     c                   move      dataeur       vdata1
053900070626     c                   move      §blddtp       vdata1g           8 0
054000070626     c                   move      §blddtp       vdata1gi
054100070626     c                   move      01            vdata1gi          8 0
054200070626     c                   else
054300070626     c                   clear                   dbld
054400070626     c                   move      0             vdata1g
054500070626     c                   move      0             vdata1gi
054600070626     c                   end
054700030714
054800011227     C                   ENDSR
054900030714      *---------------------------------------------------------------------
