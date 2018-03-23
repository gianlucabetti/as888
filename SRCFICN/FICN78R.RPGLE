000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300030715     Fficn78d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030715     Ffiatt04l  uf   E           k disk
000600130522     Ffiatt14l  if   E           k disk
000700030709     Fazorg01l  if   E           k disk
000800030714     Ffiapd01l  if   E           k disk
000900011228      *
001000020103     D Psds           SDS
001100020103     D  PgmName          *PROC
001200011228     D Kpjba         E DS
001300011228      *
001400131007     D uteautds      E DS
001500131007     d tibs42ds      e ds
001600100617     d dvpocont      e ds
001700100617     d tibs02ds      e ds
001800030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001900030604     D CNCR80        E DS
002000030617     d Tibs36Ds      e ds
002100011227     d data            ds            10
002200011228     d aa                      3      4
002300011228     d mm                      6      7
002400011228     d gg                      9     10
002500011228      *
002600030702     D fatto           S              1
002700030610     D modalita        S              1
002800030604     D x               S              4  0
002900011227     D WrkEofS01       S              1
003000011227     D WrkCarS01       S              1
003100011228     D $VIDEO          S             10
003200030604     D snrr1           S              5i 0
003300030604     D SavOpzione      S                   like(v1csce)
003400030606     d dataiso         s               d   datfmt(*iso)
003500030606     d dataeur         s               d   datfmt(*eur)
003600030613     D WLBDA8          DS
003700030613     D  G02DAT                 1      8  0
003800030613     D  G02INV                 9     16  0
003900030613     D  G02ERR                17     17
004000030613     D  G02TGI                18     22  0
004100030701      *---------------- ----------------------------------------------------
004200030604      *---------------- Gestione formato video video01----------------------
004300030604     C                   EXSR      Inzvideo1
004400030604     C                   DOU       $Video <> 'VIDEO1'
004500000608      *----------- Visualizzo dati di output in caso di errori.-------------
004600000608     C                   IF        *IN99
004700000608     C                   EVAL      *IN99 = *OFF
004800030604     C                   WRITE     video1
004900000608     C                   EVAL      *IN99 = *ON
005000000608     C                   ENDIF
005100000608      *
005200030702     c     emetti        tag
005300030604     C                   EXFMT     video1
005400000608      *
005500011227     C                   SELECT
005600030612     C                   WHEN      *INKC
005700011227     C                             OR
005800011227     C                             *INKL
005900011227     C                   EVAL      $Video = *BLANKS
006000011227
006100011227     C                   WHEN      *INKE
006200030604     C                   EXSR      Inzvideo1
006300011227     C                   WHEN      *INKB
006400030604     C                   EXSR      Chkvideo1
006500011227     C                   OTHER
006600030604     C                   EXSR      Chkvideo1
006700030715     c   43
006800030715     cor 44              goto      emetti
006900030604     C  N99              EXSR      Wrkvideo2c
007000031020     C     OK            CABEQ     0             EMETTI
007100011227     C                   ENDSL
007200011227      *
007300011227     C                   ENDDO
007400011227      *
007500011227     C                   EXSR      Uscita
007600011227     c**********************************************************************
007700011227     c* uscita
007800011227     c**********************************************************************
007900011227     C     Uscita        BEGSR
008000011227      *
008100011227     C                   EVAL      *INLR = *ON
008200011227     C                   RETURN
008300011227      *
008400011227     C                   ENDSR
008500011227     c**********************************************************************
008600011227     c* gestione sfl
008700011227     c**********************************************************************
008800030604     C     Wrkvideo2c    BEGSR
008900000627      *
009000011227     C                   EVAL      WrkCarS01 = *ON
009100030604     C                   EVAL      $Video = 'VIDEO2C'
009200011227      *
009300030604     C                   DOU       $Video <> 'VIDEO2C'
009400030604     C                   EXSR      Carvideo2
009500011228     c*Descrizione della ricerca
009600000627      *
009700031020     C     OK            CABEQ     0             TORNA
009800030604     C                   WRITE     video2z
009900030604     C                   EXFMT     video2c
010000011227     C                   SELECT
010100011228     c* fine
010200011227     C                   WHEN      *INKC=*ON
010300011227     C                   EVAL      $VIDEO=*BLANKS
010400011228     c* guida
010500011227     C                   WHEN      *INKL=*ON
010600030604     C                   EVAL      $VIDEO='VIDEO1'
010700030714
010800011227     C                   OTHER
010900011227
011000011227     C                   EXSR      GestioneSFL
011100011227     C                   ENDSL
011200011228
011300011227     C                   ENDDO
011400031020     C     torna         ENDSR
011500011227     c**********************************************************************
011600011227     c* Controlli video R01.
011700011227     c**********************************************************************
011800030604     C     Chkvideo1     BEGSR
011900000609      *
012000000608     C                   EVAL      *IN99 = *OFF
012100030715     C                   EVAL      *IN43 = *OFF
012200030908     C                   EVAL      *IN44 = *OFF
012201130812      * controllo autorizzazione del profilo
012202130812     c                   if        wuda    >= §vpoaut
012203131007     c                   clear                   tibs42ds
012204130812     c                   if        vattfga = 001 or vattfga = 101
012205130812     c                             or vattfga = 102
012206131007     c                   move      001           i42pge
012207130812     c                   else
012208131007     c                   movel     vattfga       i42pge
012209130812     c                   endif
012211131007     c                   call      'TIBS42R'
012212131007     c                   parm                    tibs42ds
012213131007     c                   movel     o42uni        uteautds
012214130812     c                   if        §AFFARRM <> 'S'
012215130812     c                   seton                                        44
012216130812     c                   leavesr
012217130812     c                   endif
012218130812     c                   endif
012300090630     c                   if        %subst(knmus:1:3) <> 'EDP'
012400030908     c                   if        o36pou = 001
012500030908     c                   select
012600030908     c                   when      vattfga = 001 or vattfga = 101
012700030908     c                             or vattfga = 102
012800030908     c                   other
012900030908     c                   seton                                        44
013000030908     c                   endsl
013100030908     c                   endif
013200090630     c                   endif
013300030715     c     vattfga       chain     azorg01l
013400030715     c                   movel     orgdes        vattfgad
013500030709     c                   if        vdata1 > 0
013600030715     c                   move      vdata1        g02dat
013700030715     c                   move      *blank        g02err
013800030715     C                   CALL      'XSRDA8'
013900030715     C                   PARM                    WLBDA8
014000030715     C     G02ERR        IFEQ      '1'
014100030715     C                   SETON                                        43
014200030715     C                   END
014300030715     c                   move      g02dat        vdata1
014400030715     c                   move      g02inv        wuda
014500030715     c                   move      g02inv        vdata1g
014600030709     c                   endif
014700000608      *
014800000608     C                   ENDSR
014900011227     c**********************************************************************
015000011227     c* inizializza r01
015100011227     c**********************************************************************
015200030604     C     Inzvideo1     BEGSR
015300000608
015400030715     c                   move      o36pou        vattfga
015500030709     c                   move      wdat          vdata1
015600030709     c                   move      wuda          vdata1g
015700030604     C                   EXSR      Chkvideo1
015800000609
015900000608     C                   ENDSR
016000030604     c**********************************************************************
016100030605     C     opzione       BEGSR
016200030605     c**********************************************************************
016300030610     c                   move      v1csce        modalita
016400030605     c                   select
016500030605     c                   when      v1csce = '2'
016600030605     c                   exsr      modifica
016700030717     c   21              clear                   $msg
016800030717     c                   setoff                                       2128
016900030605     c                   when      v1csce = '5'
017000030606     c                   seton                                        21
017100030606     c                   exsr      modifica
017200030606     c                   setoff                                       21
017300030605     c                   endsl
017400030604     C                   ENDSR
017500030605     c**********************************************************************
017600030605     C     modifica      BEGSR
017700030605     c**********************************************************************
017800030715     c     katt1         chain     fiatt04l                           98
017900030610     c                   if        not *in98
018000030717     c                   if        attdst = 0 and modalita = '2'
018100030717     c                   seton                                        2821
018200030717     c                   eval      $msg = 'Non è possibile inserire l''ora arri-
018300030717     c                             vo reale il foglio non è stato stampato in -
018400030717     c                             partenza'
018500030717     c                   endif
018600030709     c                   move      attVAD        vattVAD
018700030709     c                   move      attTER        vattTER
018800030709     c                   move      attFGP        vattFGP
018900030709     c                   move      attFGA        vattFGA
019000030709     c                   move      attDDc        vattDDc
019100030709     c                   move      attORP        vattORP
019200030714     c                   move      attORpr       vattORpr
019300030715     c                   move      attORa        vattORa
019400030901     c                   move      attORar       vattORar
019500030709     c                   move      attPRV        vattPRV
019600030728     c                   move      attPRg        vattPRg
019700030709     c                   z-add     attIMP        vattIMP
019800030709     c                   z-add     attIMPp       vattIMPp
019900030709     c                   move      attPDR        vattPDR
020000130522     c                   movel(p)  attpmb        vattpmb
020100130522     c     katt1         chain     fiatt14l
020200130522     c                   if        %found(fiatt14l)
020300130522     c                   move      at1pmb        vattpmb
020400130522     c                   end
020500030714     c     kapd          chain     fiapd01l                           95
020600090507     c  n95              movel     apdrsf        vattpdrd
020700030714     c   95              clear                   vattpdrd
020800030709     c                   move      attNOT        vattNOT
020900030714      * decodifica p.o.
021000030714     c     attter        chain     azorg01l                           95
021100030714     c  n95              movel     orgdes        vattterd
021200030714     c   95              clear                   vattterd
021300030701      * decodifica p.o.
021400030709     c     attfgp        chain     azorg01l                           95
021500030709     c  n95              movel     orgdes        vattfgpd
021600030709     c   95              clear                   vattfgpd
021700030709     c     attfga        chain     azorg01l                           95
021800030709     c  n95              movel     orgdes        vattfgad
021900030709     c   95              clear                   vattfgad
022000030701      *data decorrenza
022100030709     c                   if        attddc > 0
022200030709     c                   move      attddc        dataiso
022300030701     c                   move      dataiso       dataeur
022400030709     c                   move      dataeur       vattddc
022500030701     c                   else
022600030709     c                   clear                   vattddc
022700030701     c                   endif
022800030605     c                   exsr      Gesvideo3
022900030605     c                   endif
023000030605     C                   ENDSR
023100030605     c**********************************************************************
023200030605     C     gesvideo3     BEGSR
023300030605     c**********************************************************************
023400030702     c                   clear                   fatto
023500030605     c                   do        *hival
023600030605     c                   exfmt     video3
023700030605     c   kl              leave
023800030624     c                   exsr      controv3
023900030717     c   28
024000030717     cor 96              iter
024100030605     c   kf              exsr      aggiorna
024200030702     c                   if        fatto <> *blank
024300030702     c                   leave
024400030702     c                   endif
024500030605
024600030605     c                   enddo
024700030605     C                   ENDSR
024800030605     c**********************************************************************
024900030624     C     Controv3      BEGSR
025000030605     c**********************************************************************
025100030613      *
025200030702      * *in96 errore generico videata dettaglio
025300030703     c                   setoff                                       96
025400030703      * decodifica p.o.
025500030709     c     vattfgp       chain     azorg01l                           95
025600030709     c  n95              movel     orgdes        vattfgpd
025700030709     c   95              clear                   vattfgpd
025800030709     c     vattfga       chain     azorg01l                           95
025900030709     c  n95              movel     orgdes        vattfgad
026000030709     c   95              clear                   vattfgad
026100030701      *controlli data  decorrenza
026200030613     c                   setoff                                       50
026300030709     c                   if        vattddc > 0
026400030709     C                   MOVE      Vattddc       G02DAT
026500030613     C                   MOVEL     *BLANK        G02ERR
026600030613     C                   CALL      'XSRDA8'
026700030613     C                   PARM                    WLBDA8
026800030613     C     G02ERR        IFEQ      '1'
026900030701     C                   SETON                                        5096
027000030613     C                   END
027100030709     c                   move      g02dat        vattddc
027200030709     c                   move      g02inv        vattddcg          8 0
027300030709     c                   endif
027400030714     c     kapd          chain     fiapd01l                           95
027500090507     c  n95              movel     apdrsf        vattpdrd
027600030714     c   95              clear                   vattpdrd
027700030702      *controllo ore
027800030715     c                   setoff                                       5253
027900030715     c                   if        vattorar = *zeros
028000030702     c                   seton                                        5296
028100030702     c                   endif
028200030715     c                   if        vattorar <= vattorpr
028300030715     c                   seton                                        5396
028400030715     c                   endif
028500030624     C                   ENDSR
028600030605     c**********************************************************************
028700030605     C     aggiorna      BEGSR
028800030605     c**********************************************************************
028900030715     c                   move      vattORar      attORar
029000030709     c                   move      vattNOT       attNOT
029100030709     c                   update    fiatt000
029200030702     c                   move      'X'           fatto
029300030605     C                   ENDSR
029400011228     c**********************************************************************
029500011228     c* carica sfl
029600011228     c**********************************************************************
029700030604     C     Carvideo2     BEGSR
029800011228     C                   IF        WrkCarS01 = *ON
029900030604     C                   EVAL      snrr1   = 0
030000011228     C                   EVAL      *IN90 = *ON
030100020219     C                   EVAL      *IN91 = *OFF
030200030604     C                   WRITE     video2c
030300011228     C                   EVAL      *IN90 = *OFF
030400030715     c     kattd         setll     fiatt04l
030500030701     c                   do        *hival
030600030715     c     kattd         reade     fiatt04l                               97
030700030701     c   97              leave
030800030701     c                   move      *blanks       v1csce
030900030709     c                   move      attVAD        vattVAD
031000030709     c                   move      attTER        vattTER
031100030709     c     attter        chain     azorg01l                           95
031200030709     c  n95              movel     orgdes        vattterd
031300030709     c   95              clear                   vattterd
031400030709     c                   move      attFGP        vattFGP
031500030709     c     attfgp        chain     azorg01l                           95
031600030715     c  n95              movel     orgdes        vattfgpd2
031700030715     c   95              clear                   vattfgpd2
031800030709     c                   move      attFGA        vattFGA
031900030709     c     attfga        chain     azorg01l                           95
032000030715     c  n95              movel     orgdes        vattfgad
032100030715     c   95              clear                   vattfgad
032200030715     c                   move      attorar       vattorar
032300030709     c                   move      attPRV        vattPRV
032400030710     c                   move      attPRg        vattPRg
032500030709     c                   move      attPDR        vattPDR
032600030714     c                   movel     attpmb        vattpmb2
032700030725     c                   if        attdst > 0
032800030725     c                   move      attdst        dataiso
032900030725     c                   move      dataiso       dataeur
033000030725     c                   move      dataeur       vattdst
033100030725     c                   else
033200030725     c                   clear                   vattdst
033300030725     c                   endif
033400030725
033500030714     c     kapd          chain     fiapd01l                           95
033600090507     c  n95              movel     apdrsf        vattpdrd2
033700030714     c   95              clear                   vattpdrd2
033800030701      *esco per raggiunta massima capacità sfl
033900030617     c                   if        snrr1 > 9990
034000030617     c                   leave
034100030617     c                   endif
034200030617
034300030604     C                   EVAL      snrr1 = snrr1 + 1
034400030604     C                   WRITE     video2
034500011228     C                   ENDDO
034600011228
034700030604     C                   IF        snrr1  > 0
034800020219     C                   EVAL      *IN91 = *ON
034900020219     C                   MOVE      1             OK                1 0
035000020219     C                   ELSE
035100020219     C                   MOVE      0             OK                1 0
035200030701     C                   ENDIF
035300030612     C                   EVAL      nrr1  = 1
035400011228     C                   ENDIF
035500020219     C                   EVAL      WrkCarS01 = *OFF
035600030604     C                   EVAL      snrr1  = 1
035700030604     C                   EVAL      csrrrn = 1
035800011228     C                   ENDSR
035900011227     c**********************************************************************
036000011227     c* gestione sfl
036100011227     c**********************************************************************
036200000613     C     GestioneSFL   BEGSR
036300000627      *
036400000627     C                   EVAL      WrkEofS01 = *OFF
036500030605      * Elaborazione opzioni.
036600011228     c*
036700030605     c                   do        *hival        X                 4 0
036800030605     c     X             CHAIN     video2                             50
036900030605     C   50              LEAVE
037000030604     C                   IF        v1csce <> *blanks
037100030605     c                   exsr      opzione
037200030611
037300030611     c                   move      *blank        v1csce
037400030605     c                   update    video2
037500030611
037600011228     C                   ENDIF
037700030605     C                   enddo
037800000627      *
037900030703     C                   ENDSR
038000011227     c**********************************************************************
038100000607     C     *INZSR        BEGSR
038200011227     c**********************************************************************
038300000607      *
038400000607     C     *ENTRY        PLIST
038500000607     C                   PARM                    Kpjba
038600030604     C                   Z-ADD     1             CODUT
038700030604     C                   CALL      'X§PARUT'
038800030604     C                   PARM                    UTEDSE
038900030604     C                   MOVEL     REC80         CNCR80
039000030703     C                   MOVEL     Ragut         rsut
039100030703     C                   MOVEL     knsif         vknsif
039200030703     C                   MOVEL     knmus         vknmus
039300030702      *reperimento data
039400030702     C                   TIME                    W0120            14 0
039500030702     C                   MOVE      W0120         WDAT              8 0
039600030702     C*
039700030702     C                   Z-ADD     WDAT          G02DAT
039800030702     C                   MOVEL     *BLANK        G02ERR
039900030702     C                   CALL      'XSRDA8'
040000030702     C                   PARM                    WLBDA8
040100030702     C* UDATE A 8 IN AAAA/MM/GG
040200030702     C                   Z-ADD     G02INV        WUDA              8 0
040300030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
040400030617     C                   CLEAR                   Tibs36Ds
040500030617     C                   EVAL      I36ute = knmus
040600030617     C                   CALL      'TIBS36R'
040700030617     C                   PARM                    tibs36ds
040800100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
040900100617     c                   clear                   tibs02ds
041000100617     c                   clear                   dvpocont
041100100617     c                   eval      t02mod = 'C'
041200100617     c                   eval      t02sif = knsif
041300100617     c                   eval      t02cod = 'VPO'
041400100617     c                   eval      t02ke1 = 'CONT'
041500100617     c                   call      'TIBS02R'
041600100617     c                   parm                    kpjba
041700100617     c                   parm                    tibs02ds
041800100617    2c                   if        t02err = *blanks
041900100617     c                   eval      dvpocont  = t02uni
042000100617    2c                   endif
043300100318
043400030714     c                   movel     i36ute        edp3              3
043500030908     c                   if        edp3 <> 'EDP' and o36pou <> 001
043600030714     c                   seton                                        20
043700030714     c                   endif
043800000607      *
043900030711     C     Kattd         KLIST
044000030715     C                   KFLD                    vattfga
044100030714     C                   KFLD                    vdata1g           8 0
044200030714     C     Katt1         KLIST
044300030715     C                   KFLD                    vattfga
044400030714     C                   KFLD                    vdata1g
044500030714     C                   KFLD                    vattprg
044600030714     C     Kapd          KLIST
044700030714     C                   KFLD                    tipoa             1
044800030714     C                   KFLD                    vattpdr
044900030714     C                   move      'D'           tipoa
045000030714
045100011227     C                   ENDSR
045200030714      *---------------------------------------------------------------------
