000100030117      *****************************************************************
000200030825      *      stampa Saldi di periodo
000300030117      *****************************************************************
000400030826     Ffifss02L  IF   E           K DISK                                         x Data Prestazione
000500030826     FfiApd01L  IF   E           K DISK
000600030825     FFICN24P   O    E             PRINTER oflind(*in01)
000700030117      *****************************************************************
000800030131      *
000900030129      *****************************************************************
001000030117     d KPJBA         E DS
001100030117     D CNCR80        E DS
001200030117     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300030825      *
001400030825     D ficn23ds      e DS
001500030121      *
001600030117     D WLBDAT          DS
001700030117     D  G02DAT                 1      8  0
001800030117     D  G02INV                 9     16  0
001900030117     D  G02ERR                17     17
002000030117     D  G02TGI                18     22  0
002100030121      *-
002200030212     D  dataiso        S               d   inz
002300030212     D  dataeur        S               d   inz datfmt(*eur)
002400030827      *
002500030827     d Generico155     s             15  5
002600030826      *
002700030826     d t_Cos_P_Gen     s             15  5
002800030826     d t_Com_P_Gen     s             15  5
002900030826     d t_Cos_A_Gen     s             15  5
003000030826     d t_Com_A_Gen     s             15  5
003100030826      *
003200030826     d t_Cos_P_Pdr     s             15  5
003300030826     d t_Cos_A_Pdr     s             15  5
003400030826     d t_Com_P_Pdr     s             15  5
003500030826     d t_Com_A_Pdr     s             15  5
003600030826      *
003700030826     d t_Cos_P_Gio     s             15  5
003800030826     d t_Com_P_Gio     s             15  5
003900030826     d t_Cos_A_Gio     s             15  5
004000030826     d t_Com_A_Gio     s             15  5
004100030827      *
004200030827     d t_Cos_T_Gen     s             15  5
004300030827     d t_Com_T_Gen     s             15  5
004400030827      *
004500030827     d t_Cos_T_Pdr     s             15  5
004600030827     d t_Com_T_Pdr     s             15  5
004700030827      *
004800030827     d t_Cos_T_Gio     s             15  5
004900030827     d t_Com_T_Gio     s             15  5
005000030826      *
005100030117      *****************************************************************
005200030117     C     *ENTRY        PLIST
005300030117     C                   PARM                    KPJBA
005400030825     C                   movel     KPJBU         ficn23ds
005500030121      *
005600030121      * impostazioni iniziali
005700030120     c                   exsr      Decod_Ini
005800030120      *
005900030826     c                   write     TESPAG
006000030826     c                   write     TESDET
006100030117      *--------------------------------------------------------------*
006200030117      *             M A I N      L I N E
006300030117      *--------------------------------------------------------------*
006400030825      *  azzera i totali generali
006500030825     c                   Z-add     0             t_Cos_P_Gen
006600030825     c                   Z-add     0             t_Com_P_Gen
006700030825     c                   Z-add     0             t_Cos_A_Gen
006800030825     c                   Z-add     0             t_Com_A_Gen
006900030827      *
007000030827     c                   Z-add     0             t_Cos_T_Gen
007100030827     c                   Z-add     0             t_Com_T_Gen
007200030825      *
007300030826     C     KFss          setll     FiFSS02l
007400030826     C     f23PO         READE     FiFSS02l
007500030825      *
007600030826     C                   Dow       not %Eof(FiFSS02l) and
007700030826     c                             FssPDR <= F23CA
007800030826      *   salva l'autotrasportatore
007900030826     c                   z-add     fssPDR        savPDR
008000030826     c                   clear                   almeno_uno        1
008100030826     c*
008200030826     C                   clear                   Dpdr
008300030826     C                   move      'A'           D1ctip            1
008400030826     C                   move      fssPdr        kpdr              7 0
008500030826     C                   move      fssPdr        cpdr
008600030826     C     Kapd          chain     fiapd01l
008700030826     c                   if        %Found(fiapd01l)
008800030826     C                   MOVEL     apdrsF        Dpdr
008900030826     C                   End
009000030825      *
009100030826     c                   Z-add     0             t_Cos_P_Pdr
009200030826     c                   Z-add     0             t_Com_P_Pdr
009300030826     c                   Z-add     0             t_Cos_A_Pdr
009400030826     c                   Z-add     0             t_Com_A_Pdr
009500030827      *
009600030827     c                   Z-add     0             t_Cos_T_Pdr
009700030827     c                   Z-add     0             t_Com_T_Pdr
009800030826      *
009900030826      *  Per Autotrasportatore
010000030826     C                   Dow       not %Eof(FiFSS02l) and
010100030826     c                             fssPDR  = savPDR   and
010200030826     c                             FssPDR  <= F23CA
010300030826      *
010400030826      * Entro il Range di date esegue il dettaglio
010500030826     c                   if        FssDDC >= F23DTI and
010600030826     c                             FssDDC <= F23DTF
010700030826      * stampa dettaglio data
010800030826     c                   Exsr      Stampa_Data
010900030826     c                   End
011000030825      *
011100030826     C     f23PO         READE     FiFSS02l
011200030825     c                   EndDO
011300030826      *
011400030826     c                   Exsr      TOT_Autotr
011500030825      *
011600030826     c                   EndDO
011700030826      *
011800030825     c                   Exsr      TOT_Generali
011900030416     c*
012000030415     C                   SETON                                        LR
012100030825      *--------------------------------------------------------------*
012200030825      *  somma i dati per data
012300030825      *--------------------------------------------------------------*
012400030826     c     Stampa_data   BEGSR
012500030826      *
012600030826     c     almeno_uno    ifeq      *blank
012700030826     c                   if        *in01
012800030826     c                   write     TESPAG
012900030826     c                   write     TESDET
013000030826     c                   setoff                                       01
013100030826     c                   end
013200030826     c                   write     TESAUT
013300030826     c                   move      'S'           almeno_uno
013400030826     c                   end
013500030826      *
013600030826     c                   move      fssddc        dataiso
013700030826     c                   move      dataiso       dataeur
013800030826     c                   move      dataeur       dgio
013900030825      *
014000030826     c                   clear                   t_Cos_P_Gio
014100030826     c                   clear                   t_Cos_A_Gio
014200030826     c                   clear                   t_Com_P_Gio
014300030826     c                   clear                   t_Com_A_Gio
014400030827     c                   clear                   t_Cos_T_Gio
014500030827     c                   clear                   t_Com_T_Gio
014600030826      *
014700030826     c                   clear                   CsPGio
014800030826     c                   clear                   CmPGio
014900030826     c                   clear                   CsAGio
015000030826     c                   clear                   CmAGio
015100030826     c                   clear                   ScPGio
015200030826     c                   clear                   ScAGio
015300030826     c                   clear                   DePGio
015400030826     c                   clear                   DeAGio
015500030827      *
015600030827     c                   clear                   CsTGio
015700030827     c                   clear                   CmTGio
015800030827     c                   clear                   ScTGio
015900030827     c                   clear                   DeTGio
016000030826      * Partenze
016100030826     c                   add       fss001        t_Cos_P_Gio
016200030826     c                   add       fss035        t_Cos_P_Gio
016300030826     c                   add       fss997        t_Cos_P_Gio
016400030826      *
016500030826     c                   add       fss001b       t_Com_P_Gio
016600030826     c                   add       fss035b       t_Com_P_Gio
016700030826      * Arrivi
016800030826     c                   add       fss005        t_Cos_A_Gio
016900030826     c                   add       fss016        t_Cos_A_Gio
017000030826     c                   add       fss019        t_Cos_A_Gio
017100030826     c                   add       fss034        t_Cos_A_Gio
017200030826     c                   add       fss998        t_Cos_A_Gio
017300030826     c                   add       fssXX1        t_Cos_A_Gio
017400030826     c                   add       fssXX2        t_Cos_A_Gio
017500030826      *
017600030826     c                   add       fss005b       t_Com_A_Gio
017700030826     c                   add       fss016b       t_Com_A_Gio
017800030826     c                   add       fss019b       t_Com_A_Gio
017900030826     c                   add       fss034b       t_Com_A_Gio
018000030826     c                   add       fssXX1b       t_Com_A_Gio
018100030826     c                   add       fssXX2b       t_Com_A_Gio
018200030827      *
018300030827     c                   add       t_Cos_P_Gio   t_Cos_P_Pdr
018400030827     c                   add       t_Com_P_Gio   t_Com_P_Pdr
018500030827     c                   add       t_Cos_A_Gio   t_Cos_A_Pdr
018600030827     c                   add       t_Com_A_Gio   t_Com_A_Pdr
018700030827     c                   add       t_Cos_P_Gio   t_Cos_T_Pdr
018800030827     c                   add       t_Cos_A_Gio   t_Cos_T_Pdr
018900030827     c                   add       t_Com_P_Gio   t_Com_T_Pdr
019000030827     c                   add       t_Com_A_Gio   t_Com_T_Pdr
019100030826      *
019200030826      *  esegue il calcolo del dettaglio e stampa
019300030826     c                   if        *in75
019400030826      *
019500030826     c                   z-add     t_Cos_P_Gio   CsPGio
019600030826     c                   z-add     t_Com_P_Gio   CmPGio
019700030826     c                   z-add     t_Cos_A_Gio   CsAGio
019800030826     c                   z-add     t_Com_A_Gio   CmAGio
019900030826      *
020000030827     c     t_Cos_A_Gio   add       t_Cos_P_Gio   t_Cos_T_Gio
020100030827     c     t_Cos_A_Gio   add       t_Cos_P_Gio   CsTGio
020200030827     c     t_Com_A_Gio   add       t_Com_P_Gio   t_Com_T_Gio
020300030827     c     t_Com_A_Gio   add       t_Com_P_Gio   CmTGio
020400030827      *
020500030826      * Scostamento
020600030826     c     t_Com_P_Gio   sub       t_Cos_P_Gio   ScPGio
020700030826     c     t_Com_A_Gio   sub       t_Cos_A_Gio   ScAGio
020800030827     c     t_Com_T_Gio   sub       t_Cos_T_Gio   ScTGio
020900030826      *
021000030826      *  Delta
021100030826     c                   if        t_Com_P_Gio <> 0
021200030827     c     ScPGio        mult      100           Generico155
021300030827     c     Generico155   div(h)    t_Com_P_Gio   Generico155
021400030827     c                   if        Generico155 > 999,99 or
021500030827     c                             Generico155 < -999,99
021600030827     c                   eval      Generico155 = 0
021700030827     c                   end
021800030827     c                   z-add     Generico155   DePGio
021900030826     c                   end
022000030826      *
022100030826     c                   if        t_Com_A_Gio <> 0
022200030827     c     ScAGio        mult      100           Generico155
022300030827     c     Generico155   div(h)    t_Com_A_Gio   Generico155
022400030827     c                   if        Generico155 > 999,99 or
022500030827     c                             Generico155 < -999,99
022600030827     c                   eval      Generico155 = 0
022700030827     c                   end
022800030827     c                   z-add     Generico155   DeAGio
022900030826     c                   end
023000030827      *
023100030827     c                   if        t_Com_T_Gio <> 0
023200030827     c     ScTGio        mult      100           Generico155
023300030827     c     Generico155   div(h)    t_Com_T_Gio   Generico155
023400030827     c                   if        Generico155 > 999,99 or
023500030827     c                             Generico155 < -999,99
023600030827     c                   eval      Generico155 = 0
023700030827     c                   end
023800030827     c                   z-add     Generico155   DeTGio
023900030827     c                   end
024000030827      *
024100030826     c                   if        *in01
024200030826     c                   write     TESPAG
024300030826     c                   write     TESDET
024400030826     c                   write     TESAUT
024500030826     c                   setoff                                       01
024600030826     c                   end
024700030827     c                   write     RIGDET
024800030826      *
024900030826     c                   End
025000030826      *
025100030826     c                   Endsr
025200030826      *--------------------------------------------------------------*
025300030826      *  somma i dati dell'Autotrasportatore
025400030826      *--------------------------------------------------------------*
025500030826     c     TOT_Autotr    BEGSR
025600030826      *
025700030826     c                   clear                   ScPPdr
025800030826     c                   clear                   ScAPdr
025900030826     c                   clear                   DePPdr
026000030826     c                   clear                   DeAPdr
026100030827     c                   clear                   ScTPdr
026200030827     c                   clear                   DeTPdr
026300030826      *
026400030826     c                   add       t_Cos_P_Pdr   t_Cos_P_Gen
026500030826     c                   add       t_Com_P_Pdr   t_Com_P_Gen
026600030826     c                   add       t_Cos_A_Pdr   t_Cos_A_Gen
026700030826     c                   add       t_Com_A_Pdr   t_Com_A_Gen
026800030827     c                   add       t_Com_T_Pdr   t_Com_T_Gen
026900030827     c                   add       t_Cos_T_Pdr   t_Cos_T_Gen
027000030826      *
027100030826     c                   z-add     t_Cos_P_Pdr   CsPPdr
027200030826     c                   z-add     t_Com_P_Pdr   CmPPdr
027300030826     c                   z-add     t_Cos_A_Pdr   CsAPdr
027400030826     c                   z-add     t_Com_A_Pdr   CmAPdr
027500030827     c                   z-add     t_Cos_T_Pdr   CsTPdr
027600030827     c                   z-add     t_Com_T_Pdr   CmTPdr
027700030826      *
027800030826      * Scostamento
027900030826     c     t_Com_P_Pdr   sub       t_Cos_P_Pdr   ScPPdr
028000030826     c     t_Com_A_Pdr   sub       t_Cos_A_Pdr   ScAPdr
028100030827     c     t_Com_T_Pdr   sub       t_Cos_T_Pdr   ScTPdr
028200030826      *
028300030826      *  Delta
028400030826     c                   if        t_Com_P_Pdr <> 0
028500030827     c     ScPPdr        mult      100           Generico155
028600030827     c     Generico155   div(h)    t_Com_P_Pdr   Generico155
028700030827     c                   if        Generico155 > 999,99 or
028800030827     c                             Generico155 < -999,99
028900030827     c                   eval      Generico155 = 0
029000030827     c                   end
029100030827     c                   z-add     Generico155   DePPdr
029200030826     c                   end
029300030826      *
029400030826     c                   if        t_Com_A_Pdr <> 0
029500030827     c     ScAPdr        mult      100           Generico155
029600030827     c     Generico155   div(h)    t_Com_A_Pdr   Generico155
029700030827     c                   if        Generico155 > 999,99 or
029800030827     c                             Generico155 < -999,99
029900030827     c                   eval      Generico155 = 0
030000030827     c                   end
030100030827     c                   z-add     Generico155   DeAPdr
030200030826     c                   end
030300030827      *
030400030827     c                   if        t_Com_T_Pdr <> 0
030500030827     c     ScTPdr        mult      100           Generico155
030600030827     c     Generico155   div(h)    t_Com_T_Pdr   Generico155
030700030827     c                   if        Generico155 > 999,99 or
030800030827     c                             Generico155 < -999,99
030900030827     c                   eval      Generico155 = 0
031000030827     c                   end
031100030827     c                   z-add     Generico155   DeTPdr
031200030827     c                   end
031300030826      *
031400030826     c     almeno_uno    ifeq      'S'
031500030826     c                   if        *in01
031600030826     c                   write     TESPAG
031700030826     c                   write     TESDET
031800030826     c                   setoff                                       01
031900030826     c                   end
032000030826     c                   write     TOTPDR
032100030826     c                   end
032200030826      *
032300030825     c                   Endsr
032400030826      *--------------------------------------------------------------*
032500030826      *  Totali Generali
032600030826      *--------------------------------------------------------------*
032700030826     c     TOT_Generali  BEGSR
032800030826      *
032900030826     c                   clear                   ScPGen
033000030826     c                   clear                   ScAGen
033100030826     c                   clear                   DePGen
033200030826     c                   clear                   DeAGen
033300030827     c                   clear                   ScTGen
033400030827     c                   clear                   DeTGen
033500030826      *
033600030826     c                   z-add     t_Cos_P_Gen   CsPGen
033700030826     c                   z-add     t_Com_P_Gen   CmPGen
033800030826     c                   z-add     t_Cos_A_Gen   CsAGen
033900030826     c                   z-add     t_Com_A_Gen   CmAGen
034000030827     c                   z-add     t_Cos_T_Gen   CsTGen
034100030827     c                   z-add     t_Com_T_Gen   CmTGen
034200030826      *
034300030826      * Scostamento
034400030826     c     t_Com_P_Gen   sub       t_Cos_P_Gen   ScPGen
034500030826     c     t_Com_A_Gen   sub       t_Cos_A_Gen   ScAGen
034600030827     c     t_Com_T_Gen   sub       t_Cos_T_Gen   ScTGen
034700030826      *
034800030826      *  Delta
034900030826     c                   if        t_Com_P_Gen <> 0
035000030827     c     ScPGen        mult      100           Generico155
035100030827     c     Generico155   div(h)    t_Com_P_Gen   Generico155
035200030827     c                   if        Generico155 > 999,99 or
035300030827     c                             Generico155 < -999,99
035400030827     c                   eval      Generico155 = 0
035500030827     c                   end
035600030827     c                   z-add     Generico155   DePGen
035700030826     c                   end
035800030826      *
035900030826     c                   if        t_Com_A_Gen <> 0
036000030827     c     ScAGen        mult      100           Generico155
036100030827     c     Generico155   div(h)    t_Com_A_Gen   Generico155
036200030827     c                   if        Generico155 > 999,99 or
036300030827     c                             Generico155 < -999,99
036400030827     c                   eval      Generico155 = 0
036500030827     c                   end
036600030827     c                   z-add     Generico155   DeAGen
036700030826     c                   end
036800030827      *
036900030827     c                   if        t_Com_T_Gen <> 0
037000030827     c     ScTGen        mult      100           Generico155
037100030827     c     Generico155   div(h)    t_Com_T_Gen   Generico155
037200030827     c                   if        Generico155 > 999,99 or
037300030827     c                             Generico155 < -999,99
037400030827     c                   eval      Generico155 = 0
037500030827     c                   end
037600030827     c                   z-add     Generico155   DeTGen
037700030827     c                   end
037800030826      *
037900030827     c                   if        F23GEN ='S'
038000030826     c                   if        *in01
038100030826     c                   write     TESPAG
038200030826     c                   write     TESDET
038300030826     c                   setoff                                       01
038400030826     c                   end
038500030826     c                   write     TOTGEN
038600030827     c                   end
038700030826      *
038800030826     c                   Endsr
038900030131      *--------------------------------------------------------------*
039000030131      *  Decodifiche Iniziali
039100030131      *--------------------------------------------------------------*
039200030131     c     Decod_Ini     BEGSR
039300030131      *
039400030205     C                   TIME                    W0140            14 0
039500030205     C                   MOVE      W0140         data_08           8
039600030205     C                   MOVEl     W0140         ora_06            6
039700030826     C                   MOVE      data_08       data10           10
039800030826     C                   MOVEL     ora_06        ora04             4
039900030204      * Parametri di stampa
040000030825     c                   move      f23po         po
040100030826     c                   move      f23cda        daPdr
040200030826     c                   move      f23ca         aPdr
040300030825     c                   move      f23dti        dataiso
040400030212     c                   move      dataiso       dataeur
040500030212     c                   move      dataeur       dadata
040600030825     c                   move      f23dtf        dataiso
040700030212     c                   move      dataiso       dataeur
040800030212     c                   move      dataeur       adata
040900030825     c                   eval      dettag = f23det
041000030825      *
041100030825      *  Decodifica l'autotrasportatore
041200030825     C                   clear                   desAU1
041300030825     C                   move      'A'           D1ctip            1
041400030826     C                   move      daPdr         kpdr              7 0
041500030825     C     Kapd          chain     fiapd01l
041600030825     c                   if        %Found(fiapd01l)
041700030825     C                   MOVEL     apdrsF        desAU1
041800030825     C                   End
041900030825     c*
042000030825     C                   clear                   desAU2
042100030825     C                   move      'A'           D1ctip            1
042200030826     C                   move      aPdr          kpdr              7 0
042300030825     C     Kapd          chain     fiapd01l
042400030825     c                   if        %Found(fiapd01l)
042500030825     C                   MOVEL     apdrsF        desAU2
042600030825     C                   End
042700030204      *
042800030826     C* f23det   => 'N' Deve stampare solo i Totali
042900030826     C* f23det   => 'S' Deve stampare tutto                   75=ON
043000030826     c                   if        f23det   = 'N'
043100030120     c                   setoff                                       75
043200030120     c                   else
043300030120     c                   seton                                        75
043400030120     c                   end
043500030120     C*
043600030120     C                   Z-ADD     1             CODUT
043700030120     C                   CALL      'X§PARUT'
043800030120     C                   PARM                    UT§DSE
043900030120     C                   MOVEL     RAGUT         RSUT             20
044000030120     C                   MOVEL     REC80         CNCR80
044100030826      *-------------------------            Pagina Iniziale con Parametri
044200030826     c*
044300030826>    c                   write     TESPAG
044400030826     c                   write     PRIPAG
044500030826     c*
044600030826      *-------------------------
044700030120      *
044800030120     c                   EndSR
044900030122      *--------------------------------------------------------------*
045000030122      *  Definizioni
045100030122      *--------------------------------------------------------------*
045200030122     c     DEFCAM        BEGSR
045300030825     C*
045400030826     c     *like         define    fssPDR        savPDR
045500030131     C*
045600030131     C     Kapd          KLIST
045700030131     C                   KFLD                    D1ctip
045800030131     C                   KFLD                    kpdr              7 0
045900030825     C*
046000030825     C     KFss          KLIST
046100030825     C                   KFLD                    F23PO
046200030826     C                   KFLD                    F23cda
046300030826     C                   KFLD                    F23DTI
046400030131      *
046500030122     c                   ENDSR
046600030131      *--------------------------------------------------------------*
