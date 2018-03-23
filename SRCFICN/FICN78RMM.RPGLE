000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300030715     Fficn78d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030715     Ffiatt04l  uf   E           k disk
000600030709     Fazorg01l  if   E           k disk
000700030714     Ffiapd01l  if   E           k disk
000800011228      *
000900020103     D Psds           SDS
001000020103     D  PgmName          *PROC
001100011228     D Kpjba         E DS
001200011228      *
001201100318     D daut          E DS
001202100318     d ficn70ds      e ds
001203100617     d dvpocont      e ds
001204100617     d tibs02ds      e ds
001300030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001400030604     D CNCR80        E DS
001500030617     d Tibs36Ds      e ds
001600011227     d data            ds            10
001700011228     d aa                      3      4
001800011228     d mm                      6      7
001900011228     d gg                      9     10
002000011228      *
002100030702     D fatto           S              1
002200030610     D modalita        S              1
002300030604     D x               S              4  0
002400011227     D WrkEofS01       S              1
002500011227     D WrkCarS01       S              1
002600011228     D $VIDEO          S             10
002700030604     D snrr1           S              5i 0
002800030604     D SavOpzione      S                   like(v1csce)
002900030606     d dataiso         s               d   datfmt(*iso)
003000030606     d dataeur         s               d   datfmt(*eur)
003100030613     D WLBDA8          DS
003200030613     D  G02DAT                 1      8  0
003300030613     D  G02INV                 9     16  0
003400030613     D  G02ERR                17     17
003500030613     D  G02TGI                18     22  0
003600030701      *---------------- ----------------------------------------------------
003700030604      *---------------- Gestione formato video video01----------------------
003800030604     C                   EXSR      Inzvideo1
003900030604     C                   DOU       $Video <> 'VIDEO1'
004000000608      *----------- Visualizzo dati di output in caso di errori.-------------
004100000608     C                   IF        *IN99
004200000608     C                   EVAL      *IN99 = *OFF
004300030604     C                   WRITE     video1
004400000608     C                   EVAL      *IN99 = *ON
004500000608     C                   ENDIF
004600000608      *
004700030702     c     emetti        tag
004800030604     C                   EXFMT     video1
004900000608      *
005000011227     C                   SELECT
005100030612     C                   WHEN      *INKC
005200011227     C                             OR
005300011227     C                             *INKL
005400011227     C                   EVAL      $Video = *BLANKS
005500011227
005600011227     C                   WHEN      *INKE
005700030604     C                   EXSR      Inzvideo1
005800011227     C                   WHEN      *INKB
005900030604     C                   EXSR      Chkvideo1
006000011227     C                   OTHER
006100030604     C                   EXSR      Chkvideo1
006200030715     c   43
006300030715     cor 44              goto      emetti
006400030604     C  N99              EXSR      Wrkvideo2c
006500031020     C     OK            CABEQ     0             EMETTI
006600011227     C                   ENDSL
006700011227      *
006800011227     C                   ENDDO
006900011227      *
007000011227     C                   EXSR      Uscita
007100011227     c**********************************************************************
007200011227     c* uscita
007300011227     c**********************************************************************
007400011227     C     Uscita        BEGSR
007500011227      *
007600011227     C                   EVAL      *INLR = *ON
007700011227     C                   RETURN
007800011227      *
007900011227     C                   ENDSR
008000011227     c**********************************************************************
008100011227     c* gestione sfl
008200011227     c**********************************************************************
008300030604     C     Wrkvideo2c    BEGSR
008400000627      *
008500011227     C                   EVAL      WrkCarS01 = *ON
008600030604     C                   EVAL      $Video = 'VIDEO2C'
008700011227      *
008800030604     C                   DOU       $Video <> 'VIDEO2C'
008900030604     C                   EXSR      Carvideo2
009000011228     c*Descrizione della ricerca
009100000627      *
009200031020     C     OK            CABEQ     0             TORNA
009300030604     C                   WRITE     video2z
009400030604     C                   EXFMT     video2c
009500011227     C                   SELECT
009600011228     c* fine
009700011227     C                   WHEN      *INKC=*ON
009800011227     C                   EVAL      $VIDEO=*BLANKS
009900011228     c* guida
010000011227     C                   WHEN      *INKL=*ON
010100030604     C                   EVAL      $VIDEO='VIDEO1'
010200030714
010300011227     C                   OTHER
010400011227
010500011227     C                   EXSR      GestioneSFL
010600011227     C                   ENDSL
010700011228
010800011227     C                   ENDDO
010900031020     C     torna         ENDSR
011000011227     c**********************************************************************
011100011227     c* Controlli video R01.
011200011227     c**********************************************************************
011300030604     C     Chkvideo1     BEGSR
011400000609      *
011500000608     C                   EVAL      *IN99 = *OFF
011600030715     C                   EVAL      *IN43 = *OFF
011700030908     C                   EVAL      *IN44 = *OFF
011800090630     c                   if        %subst(knmus:1:3) <> 'EDP'
011900030908     c                   if        o36pou = 001
012000030908     c                   select
012100030908     c                   when      vattfga = 001 or vattfga = 101
012200030908     c                             or vattfga = 102
012300030908     c                   other
012301110117     c                   seton                                        44
012500030908     c                   endsl
012501110117     c                   else
012502110117     c                   if        i36ute = 'UTE01801' and
012503110117     c                              vattfga <> 165 and vattfga <> 018
012504110117     c                   seton                                        44
012505110117     c                   endif
012600030908     c                   endif
012700090630     c                   endif
012800030715     c     vattfga       chain     azorg01l
012900030715     c                   movel     orgdes        vattfgad
013000030709     c                   if        vdata1 > 0
013100030715     c                   move      vdata1        g02dat
013200030715     c                   move      *blank        g02err
013300030715     C                   CALL      'XSRDA8'
013400030715     C                   PARM                    WLBDA8
013500030715     C     G02ERR        IFEQ      '1'
013600030715     C                   SETON                                        43
013700030715     C                   END
013800030715     c                   move      g02dat        vdata1
013900030715     c                   move      g02inv        wuda
014000030715     c                   move      g02inv        vdata1g
014100030709     c                   endif
014200000608      *
014300000608     C                   ENDSR
014400011227     c**********************************************************************
014500011227     c* inizializza r01
014600011227     c**********************************************************************
014700030604     C     Inzvideo1     BEGSR
014800000608
014900030715     c                   move      o36pou        vattfga
015000030709     c                   move      wdat          vdata1
015100030709     c                   move      wuda          vdata1g
015200030604     C                   EXSR      Chkvideo1
015300000609
015400000608     C                   ENDSR
015500030604     c**********************************************************************
015600030605     C     opzione       BEGSR
015700030605     c**********************************************************************
015800030610     c                   move      v1csce        modalita
015900030605     c                   select
016000030605     c                   when      v1csce = '2'
016100030605     c                   exsr      modifica
016200030717     c   21              clear                   $msg
016300030717     c                   setoff                                       2128
016400030605     c                   when      v1csce = '5'
016500030606     c                   seton                                        21
016600030606     c                   exsr      modifica
016700030606     c                   setoff                                       21
016800030605     c                   endsl
016900030604     C                   ENDSR
017000030605     c**********************************************************************
017100030605     C     modifica      BEGSR
017200030605     c**********************************************************************
017300030715     c     katt1         chain     fiatt04l                           98
017400030610     c                   if        not *in98
017500030717     c                   if        attdst = 0 and modalita = '2'
017600030717     c                   seton                                        2821
017700030717     c                   eval      $msg = 'Non è possibile inserire l''ora arri-
017800030717     c                             vo reale il foglio non è stato stampato in -
017900030717     c                             partenza'
018000030717     c                   endif
018100030709     c                   move      attVAD        vattVAD
018200030709     c                   move      attTER        vattTER
018300030709     c                   move      attFGP        vattFGP
018400030709     c                   move      attFGA        vattFGA
018500030709     c                   move      attDDc        vattDDc
018600030709     c                   move      attORP        vattORP
018700030714     c                   move      attORpr       vattORpr
018800030715     c                   move      attORa        vattORa
018900030901     c                   move      attORar       vattORar
019000030709     c                   move      attPRV        vattPRV
019100030728     c                   move      attPRg        vattPRg
019200030709     c                   z-add     attIMP        vattIMP
019300030709     c                   z-add     attIMPp       vattIMPp
019400030714     c                   movel     attpmb        vattpmb
019500030709     c                   move      attPDR        vattPDR
019600030714     c     kapd          chain     fiapd01l                           95
019700090507     c  n95              movel     apdrsf        vattpdrd
019800030714     c   95              clear                   vattpdrd
019900030709     c                   move      attNOT        vattNOT
020000030714      * decodifica p.o.
020100030714     c     attter        chain     azorg01l                           95
020200030714     c  n95              movel     orgdes        vattterd
020300030714     c   95              clear                   vattterd
020400030701      * decodifica p.o.
020500030709     c     attfgp        chain     azorg01l                           95
020600030709     c  n95              movel     orgdes        vattfgpd
020700030709     c   95              clear                   vattfgpd
020800030709     c     attfga        chain     azorg01l                           95
020900030709     c  n95              movel     orgdes        vattfgad
021000030709     c   95              clear                   vattfgad
021100030701      *data decorrenza
021200030709     c                   if        attddc > 0
021300030709     c                   move      attddc        dataiso
021400030701     c                   move      dataiso       dataeur
021500030709     c                   move      dataeur       vattddc
021600030701     c                   else
021700030709     c                   clear                   vattddc
021800030701     c                   endif
021900030605     c                   exsr      Gesvideo3
022000030605     c                   endif
022100030605     C                   ENDSR
022200030605     c**********************************************************************
022300030605     C     gesvideo3     BEGSR
022400030605     c**********************************************************************
022500030702     c                   clear                   fatto
022600030605     c                   do        *hival
022700030605     c                   exfmt     video3
022800030605     c   kl              leave
022900030624     c                   exsr      controv3
023000030717     c   28
023100030717     cor 96              iter
023200030605     c   kf              exsr      aggiorna
023300030702     c                   if        fatto <> *blank
023400030702     c                   leave
023500030702     c                   endif
023600030605
023700030605     c                   enddo
023800030605     C                   ENDSR
023900030605     c**********************************************************************
024000030624     C     Controv3      BEGSR
024100030605     c**********************************************************************
024200030613      *
024300030702      * *in96 errore generico videata dettaglio
024400030703     c                   setoff                                       96
024500030703      * decodifica p.o.
024600030709     c     vattfgp       chain     azorg01l                           95
024700030709     c  n95              movel     orgdes        vattfgpd
024800030709     c   95              clear                   vattfgpd
024900030709     c     vattfga       chain     azorg01l                           95
025000030709     c  n95              movel     orgdes        vattfgad
025100030709     c   95              clear                   vattfgad
025200030701      *controlli data  decorrenza
025300030613     c                   setoff                                       50
025400030709     c                   if        vattddc > 0
025500030709     C                   MOVE      Vattddc       G02DAT
025600030613     C                   MOVEL     *BLANK        G02ERR
025700030613     C                   CALL      'XSRDA8'
025800030613     C                   PARM                    WLBDA8
025900030613     C     G02ERR        IFEQ      '1'
026000030701     C                   SETON                                        5096
026100030613     C                   END
026200030709     c                   move      g02dat        vattddc
026300030709     c                   move      g02inv        vattddcg          8 0
026400030709     c                   endif
026500030714     c     kapd          chain     fiapd01l                           95
026600090507     c  n95              movel     apdrsf        vattpdrd
026700030714     c   95              clear                   vattpdrd
026800030702      *controllo ore
026900030715     c                   setoff                                       5253
027000030715     c                   if        vattorar = *zeros
027100030702     c                   seton                                        5296
027200030702     c                   endif
027300030715     c                   if        vattorar <= vattorpr
027400030715     c                   seton                                        5396
027500030715     c                   endif
027600030624     C                   ENDSR
027700030605     c**********************************************************************
027800030605     C     aggiorna      BEGSR
027900030605     c**********************************************************************
028000030715     c                   move      vattORar      attORar
028100030709     c                   move      vattNOT       attNOT
028200030709     c                   update    fiatt000
028300030702     c                   move      'X'           fatto
028400030605     C                   ENDSR
028500011228     c**********************************************************************
028600011228     c* carica sfl
028700011228     c**********************************************************************
028800030604     C     Carvideo2     BEGSR
028900011228     C                   IF        WrkCarS01 = *ON
029000030604     C                   EVAL      snrr1   = 0
029100011228     C                   EVAL      *IN90 = *ON
029200020219     C                   EVAL      *IN91 = *OFF
029300030604     C                   WRITE     video2c
029400011228     C                   EVAL      *IN90 = *OFF
029500030715     c     kattd         setll     fiatt04l
029600030701     c                   do        *hival
029700030715     c     kattd         reade     fiatt04l                               97
029800030701     c   97              leave
029900030701     c                   move      *blanks       v1csce
030000030709     c                   move      attVAD        vattVAD
030100030709     c                   move      attTER        vattTER
030200030709     c     attter        chain     azorg01l                           95
030300030709     c  n95              movel     orgdes        vattterd
030400030709     c   95              clear                   vattterd
030500030709     c                   move      attFGP        vattFGP
030600030709     c     attfgp        chain     azorg01l                           95
030700030715     c  n95              movel     orgdes        vattfgpd2
030800030715     c   95              clear                   vattfgpd2
030900030709     c                   move      attFGA        vattFGA
031000030709     c     attfga        chain     azorg01l                           95
031100030715     c  n95              movel     orgdes        vattfgad
031200030715     c   95              clear                   vattfgad
031300030715     c                   move      attorar       vattorar
031400030709     c                   move      attPRV        vattPRV
031500030710     c                   move      attPRg        vattPRg
031600030709     c                   move      attPDR        vattPDR
031700030714     c                   movel     attpmb        vattpmb2
031800030725     c                   if        attdst > 0
031900030725     c                   move      attdst        dataiso
032000030725     c                   move      dataiso       dataeur
032100030725     c                   move      dataeur       vattdst
032200030725     c                   else
032300030725     c                   clear                   vattdst
032400030725     c                   endif
032500030725
032600030714     c     kapd          chain     fiapd01l                           95
032700090507     c  n95              movel     apdrsf        vattpdrd2
032800030714     c   95              clear                   vattpdrd2
032900030701      *esco per raggiunta massima capacità sfl
033000030617     c                   if        snrr1 > 9990
033100030617     c                   leave
033200030617     c                   endif
033300030617
033400030604     C                   EVAL      snrr1 = snrr1 + 1
033500030604     C                   WRITE     video2
033600011228     C                   ENDDO
033700011228
033800030604     C                   IF        snrr1  > 0
033900020219     C                   EVAL      *IN91 = *ON
034000020219     C                   MOVE      1             OK                1 0
034100020219     C                   ELSE
034200020219     C                   MOVE      0             OK                1 0
034300030701     C                   ENDIF
034400030612     C                   EVAL      nrr1  = 1
034500011228     C                   ENDIF
034600020219     C                   EVAL      WrkCarS01 = *OFF
034700030604     C                   EVAL      snrr1  = 1
034800030604     C                   EVAL      csrrrn = 1
034900011228     C                   ENDSR
035000011227     c**********************************************************************
035100011227     c* gestione sfl
035200011227     c**********************************************************************
035300000613     C     GestioneSFL   BEGSR
035400000627      *
035500000627     C                   EVAL      WrkEofS01 = *OFF
035600030605      * Elaborazione opzioni.
035700011228     c*
035800030605     c                   do        *hival        X                 4 0
035900030605     c     X             CHAIN     video2                             50
036000030605     C   50              LEAVE
036100030604     C                   IF        v1csce <> *blanks
036200030605     c                   exsr      opzione
036300030611
036400030611     c                   move      *blank        v1csce
036500030605     c                   update    video2
036600030611
036700011228     C                   ENDIF
036800030605     C                   enddo
036900000627      *
037000030703     C                   ENDSR
037100011227     c**********************************************************************
037200000607     C     *INZSR        BEGSR
037300011227     c**********************************************************************
037400000607      *
037500000607     C     *ENTRY        PLIST
037600000607     C                   PARM                    Kpjba
037700030604     C                   Z-ADD     1             CODUT
037800030604     C                   CALL      'X§PARUT'
037900030604     C                   PARM                    UTEDSE
038000030604     C                   MOVEL     REC80         CNCR80
038100030703     C                   MOVEL     Ragut         rsut
038200030703     C                   MOVEL     knsif         vknsif
038300030703     C                   MOVEL     knmus         vknmus
038400030702      *reperimento data
038500030702     C                   TIME                    W0120            14 0
038600030702     C                   MOVE      W0120         WDAT              8 0
038700030702     C*
038800030702     C                   Z-ADD     WDAT          G02DAT
038900030702     C                   MOVEL     *BLANK        G02ERR
039000030702     C                   CALL      'XSRDA8'
039100030702     C                   PARM                    WLBDA8
039200030702     C* UDATE A 8 IN AAAA/MM/GG
039300030702     C                   Z-ADD     G02INV        WUDA              8 0
039400030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
039500030617     C                   CLEAR                   Tibs36Ds
039600030617     C                   EVAL      I36ute = knmus
039700030617     C                   CALL      'TIBS36R'
039800030617     C                   PARM                    tibs36ds
039801100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
039802100617     c                   clear                   tibs02ds
039803100617     c                   clear                   dvpocont
039804100617     c                   eval      t02mod = 'C'
039805100617     c                   eval      t02sif = knsif
039806100617     c                   eval      t02cod = 'VPO'
039807100617     c                   eval      t02ke1 = 'CONT'
039808100617     c                   call      'TIBS02R'
039809100617     c                   parm                    kpjba
039810100617     c                   parm                    tibs02ds
039811100617    2c                   if        t02err = *blanks
039812100617     c                   eval      dvpocont  = t02uni
039813100617    2c                   endif
039814100318      * controllo autorizzazione del profilo
039815100617     c                   if        wuda    >= §vpoaut
039816100318     c                   movel     o36pou        i70pog
039817100318     c                   movel     o36pou        i70pge
039818100318     c                   movel     knmus         i70prf
039819100318     c                   call      'FICN70R'
039820100318     c                   parm                    ficn70ds
039821100318     c                   movel     o70uni        daut
039822100318     c                   if        §AFFARRM <> 'S'
039823100318     c                   exsr      uscita
039824100617     c                   endif
039825100318     c                   endif
039826100318
039900030714     c                   movel     i36ute        edp3              3
040000030908     c                   if        edp3 <> 'EDP' and o36pou <> 001
040001110117     c                             and i36ute <> 'UTE01801'
040100030714     c                   seton                                        20
040200030714     c                   endif
040300000607      *
040400030711     C     Kattd         KLIST
040500030715     C                   KFLD                    vattfga
040600030714     C                   KFLD                    vdata1g           8 0
040700030714     C     Katt1         KLIST
040800030715     C                   KFLD                    vattfga
040900030714     C                   KFLD                    vdata1g
041000030714     C                   KFLD                    vattprg
041100030714     C     Kapd          KLIST
041200030714     C                   KFLD                    tipoa             1
041300030714     C                   KFLD                    vattpdr
041400030714     C                   move      'D'           tipoa
041500030714
041600011227     C                   ENDSR
041700030714      *---------------------------------------------------------------------
