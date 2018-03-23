000100030117      *****************************************************************
000200030825      *      stampa Saldi di periodo
000300030117      *****************************************************************
000400091112     Ffiftt02L  IF   E           K DISK                                         x Data Prestazione
000500091113     Ffiapd01L  IF   E           K DISK
000600091112     Ffifss02L  IF   E           K DISK                                         x Data Prestazione
000700120403     Ffiqdt05L  IF   E           K DISK                                         x Data Prestazione
000800120403     Fwficnd10f O    E             disk    usropn
000900030117      *****************************************************************
001000030131      *
001100030129      *****************************************************************
001200030117     d KPJBA         E DS
001300030825      *
001400091112     D ficnd7ds      e DS
001500091112     D ddatiute      e ds
001600091112     D azuteds       e ds                  extname(AZUTE00F)
001700091112     D tibs34ds      E DS                  inz
001800091112     D dapdflr       E DS                  inz
001900030121      *
002000030117     D WLBDAT          DS
002100030117     D  G02DAT                 1      8  0
002200030117     D  G02INV                 9     16  0
002300030117     D  G02ERR                17     17
002400030117     D  G02TGI                18     22  0
002500030121      *-
002600030212     D  dataiso        S               d   inz
002700030212     D  dataeur        S               d   inz datfmt(*eur)
002800030826      *
002900091113     d t_Cos_P_Pdr     s             19  5
003000120403     d t_Com_P_Pdr     s             19  5
003100091113     d t_Cos_A_Pdr     s             19  5
003200091113     d t_Com_A_Pdr     s             19  5
003300091113     d t_nsp_C_Pdr     s              7  0
003400091118     d t_cla_C_Pdr     s              7  0
003500091113     d t_sna_C_Pdr     s              7  0
003600091113     d t_kfa_C_Pdr     s              8  1
003700091113     d t_tvl_C_Pdr     s              9  3
003800120403     d t_nsp_R_Pdr     s              7  0
003900120403     d t_cla_R_Pdr     s              7  0
004000120403     d t_sna_R_Pdr     s              7  0
004100120403     d t_kfa_R_Pdr     s              8  1
004200120403     d t_tvl_R_Pdr     s              9  3
004300120403     d t_BLAF_C_Pdr    s              9  0
004400120403     d t_BLNC_C_Pdr    s              9  0
004500120403     d t_RMAF_R_Pdr    s              9  0
004600120403     d t_RMFA_R_Pdr    s              9  0
004700030826      *
004800030826     d t_Cos_P_Gio     s             15  5
004900030826     d t_Com_P_Gio     s             15  5
005000030826     d t_Cos_A_Gio     s             15  5
005100030826     d t_Com_A_Gio     s             15  5
005200091113     d t_nsp_C_gio     s              7  0
005300091118     d t_cla_C_gio     s              7  0
005400091113     d t_sna_C_gio     s              7  0
005500091113     d t_kfa_C_gio     s              8  1
005600091113     d t_tvl_C_gio     s              9  3
005700091113     d t_nsp_R_gio     s              7  0
005800091118     d t_cla_R_gio     s              7  0
005900091113     d t_sna_R_gio     s              7  0
006000091113     d t_kfa_R_gio     s              8  1
006100091113     d t_tvl_R_gio     s              9  3
006200120403     d t_BLAF_C_gio    s              7  0
006300120403     d t_BLNC_C_gio    s              7  0
006400120403     d t_RMAF_R_gio    s              7  0
006500120403     d t_RMFA_R_gio    s              7  0
006600030117      *****************************************************************
006700030117     C     *ENTRY        PLIST
006800030117     C                   PARM                    KPJBA
006900091112     C                   movel     KPJBU         ficnd7ds
007000120403      *
007100120403     c                   open      wficnd10f
007200091113     c* pulisce il wf
007300091113     c                   if        parclr = 'S'
007400091113     C/EXEC SQL
007500120403     C+ DELETE FROM WFicnd10F
007600091113     C/END-EXEC
007700110822      *****
007800110822     C/EXEC SQL
007900120403     C+ DELETE FROM WFicnD10CM
008000110822     C/END-EXEC
008100091113     c                   end
008200091113     c*
008300091112     c     *dtaara       define    §azute        azuteds
008400091112     c     *dtaara       define    §datiute      ddatiute
008500091112     C                   in(E)     *dtaara
008600091112     C                   IF        %Error  or  RSUT = *blanks
008700091112     C                   call      'TIBS34R'
008800091112     C                   parm                    Tibs34Ds
008900091112     C                   in        *dtaara
009000091112     c                   ENDIF
009100120403      *
009200091112     c     *like         define    fssPDR        savPDR
009300091113     c     *like         define    §apdmcp       savMCP
009400120403     c     *like         define    §apdKM        savKM
009500120403     c     *like         define    apdrsc        savRSC
009600120403     c     *like         define    apdpkl        savPKL
009700120403     c     *like         define    apdvlu        savVLU
009800091112     C*
009900091112     C     KFss          KLIST
010000091112     C                   KFLD                    F23PO
010100091112     C                   KFLD                    parp1
010200091112     C                   KFLD                    pardd1
010300091112     c                   movel     parp1         f23po             3 0
010400091113     C     Kapd          KLIST
010500091113     C                   KFLD                    APDTIP
010600091113     C                   KFLD                    FSSPDR
010700091113     c                   movel     'A'           APDTIP
010800091113     C     KFTT          KLIST
010900091113     C                   KFLD                    fsspdr
011000091113     C                   KFLD                    fttfvl
011100091113     C                   KFLD                    fssddc
011200091113     C                   KFLD                    ftttsr
011300091113     c                   movel     'C'           fttfvl
011400030120      *
011500120403     C     KQDT          KLIST
011600120403     C                   KFLD                    FSSfgs
011700120403     C                   KFLD                    fsspdr
011800120403     C                   KFLD                    fssddc
011900030117      *--------------------------------------------------------------*
012000030117      *             M A I N      L I N E
012100030117      *--------------------------------------------------------------*
012200030825      *
012300030826     C     KFss          setll     FiFSS02l
012400120403      *
012500091112     c                   do        *hival
012600091113     C                   READ      FiFSS02l
012700120403      *
012800120403      *  x EoF o raggiunto il Limite massimo esce dal ciclo
012900120403     C                   if        %Eof(FiFSS02l)   or
013000120403     c                             FssPDR > parp2
013100091116     c                   leave
013200091116     c                   end
013300120403      *
013400120403      *   Ne rileggo un altro poichè
013500091112     c* escludo quelli esclusi da autofat???
013600091112     c                   if        fsspdd = 'S'
013700091112     c                   iter
013800091112     c                   end
013900120403      *
014000120403      *  a rottura di aut  scrive ....
014100091113     c                   if        fssPDR <> savPDR
014200120403      *-
014300120403      *    dal secondo autista letto
014400091113     c                   if        partsp   = '2' and savpdr <> 0
014500091113     c                   Exsr      srscrivipdr
014600091113     c                   end
014700120403      *-
014800091113      *   salva l'autotrasportatore
014900091113     c                   z-add     fssPDR        savPDR
015000091113     c     kapd          chain     fiapd01l
015100120403      *
015200091113     C                   IF        %FOUND(FIAPD01L)
015300091113     C                   EVAL      DAPDFLR = APDFLR
015400091113     C                   ELSE
015500091113     C                   CLEAR                   DAPDFLR
015600091113     C                   END
015700120403      *
015800120403     C                   EVAL      savRSC = APDrsc
015900120403     C                   EVAL      savPKL = APDpkl
016000120403     C                   EVAL      savVLU = APDvlu
016100091113     c                   z-add     §APDMCP       savMCP
016200120523     c                   movel     §apdpda       savPDA            1
016300120523      * Se l'autista ha il PDA
016400120523     c                   if          savPDA = *blank
016500120523     c                   movel     'N'           savPDA
016600120523     c                   end
016700120403      *
016800120403      * se c'è un valore in KM lo espongo altrimenti è "000"
016900120403     c                   move      *zeros        savKM
017000120403     c                   if        §APDKM <> *blank
017100120403     c                   move      §APDKM        savKM
017200120403     c                   end
017300120403      *
017400091113     c                   if        partsp   = '2'
017500091113     c                   Z-add     0             t_Cos_P_Pdr
017600091113     c                   Z-add     0             t_Com_P_Pdr
017700091113     c                   Z-add     0             t_Cos_A_Pdr
017800091113     c                   Z-add     0             t_Com_A_Pdr
017900091113     c                   Z-add     0             t_nsp_C_pdr
018000091118     c                   Z-add     0             t_cla_C_pdr
018100091113     c                   Z-add     0             t_sna_C_pdr
018200091113     c                   Z-add     0             t_kfa_C_pdr
018300091113     c                   Z-add     0             t_tvl_C_pdr
018400091113     c                   Z-add     0             t_nsp_R_pdr
018500091118     c                   Z-add     0             t_cla_R_pdr
018600091113     c                   Z-add     0             t_sna_R_pdr
018700091113     c                   Z-add     0             t_kfa_R_pdr
018800091113     c                   Z-add     0             t_tvl_R_pdr
018900120403     C                   Z-add     0             t_BLAF_C_Pdr
019000120403     C                   Z-add     0             t_BLNC_C_Pdr
019100120403     c                   Z-add     0             t_RMAF_R_Pdr
019200120403     C                   Z-add     0             t_RMFA_R_Pdr
019300091113     c                   End
019400091113     c                   End
019500120403      *
019600030826      * Entro il Range di date esegue il dettaglio
019700091112     c                   if        FssDDC >= pardd1 and
019800091112     c                             FssDDC <= pardd2
019900091112      * elabora il dettaglio
020000091113     c                   Exsr      Srdata
020100030826     c                   End
020200030825      *
020300030826     c                   EndDO
020400110823      *
020500110823     c*  Altrimenti non rilasciava gli ultimi records scritti
020600110823     c*   che servivano al programma seguente
020700120403     c                   close     wficnd10f
020800030416     c*
020900110822     c* Calcolo costo medio Giornaliero su Richiesta Parametri
021000110822     c                   if        parCMG ='S'
021100110822     c                   Exsr      Calc_CosMedio
021200110822     c                   End
021300110822     c*
021400030415     C                   SETON                                        LR
021500091113      *--------------------------------------------------------------*
021600091113      *  somma i dati per data
021700091113      *--------------------------------------------------------------*
021800091113     c     srdata        BEGSR
021900030825      *
022000030826     c                   clear                   t_Cos_P_Gio
022100030826     c                   clear                   t_Cos_A_Gio
022200030826     c                   clear                   t_Com_P_Gio
022300030826     c                   clear                   t_Com_A_Gio
022400091112     c                   clear                   t_nsp_C_gio
022500091118     c                   clear                   t_cla_C_gio
022600091112     c                   clear                   t_sna_C_gio
022700091112     c                   clear                   t_kfa_C_gio
022800091112     c                   clear                   t_tvl_C_gio
022900091112     c                   clear                   t_nsp_R_gio
023000091118     c                   clear                   t_cla_R_gio
023100091112     c                   clear                   t_sna_R_gio
023200091112     c                   clear                   t_kfa_R_gio
023300091112     c                   clear                   t_tvl_R_gio
023400120403     C                   clear                   t_BLAF_C_gio
023500120403     c                   clear                   t_BLNC_C_gio
023600120403     c                   clear                   t_RMAF_R_gio
023700120403     c                   clear                   t_RMFA_R_gio
023800120403     C*
023900120403     c* calcola i dati della giornata dell'autista
024000120403     C     KQDT          setll     fiqdt05l
024100120403     C     KQDT          reade     fiqdt05l
024200120403     c                   DOw       not %EoF(fiqdt05l)
024300120403     C                   add       qdtnBLAF      t_BLAF_C_gio
024400120403     c                   add       qdtnBLNC      t_BLNC_C_gio
024500120403     c                   add       qdtoRMAF      t_RMAF_R_gio
024600120403     c                   add       qdtoRMFA      t_RMFA_R_gio
024700120403     C     KQDT          reade     fiqdt05l
024800120403     c                   endDO
024900030826      * Partenze
025000030826     c                   add       fss001        t_Cos_P_Gio
025100030826     c                   add       fss035        t_Cos_P_Gio
025200030826     c                   add       fss997        t_Cos_P_Gio
025300030826      *
025400030826     c                   add       fss001b       t_Com_P_Gio
025500030826     c                   add       fss035b       t_Com_P_Gio
025600030826      * Arrivi
025700030826     c                   add       fss005        t_Cos_A_Gio
025800030826     c                   add       fss016        t_Cos_A_Gio
025900030826     c                   add       fss019        t_Cos_A_Gio
026000030826     c                   add       fss034        t_Cos_A_Gio
026100030826     c                   add       fss998        t_Cos_A_Gio
026200030826     c                   add       fssXX1        t_Cos_A_Gio
026300030826     c                   add       fssXX2        t_Cos_A_Gio
026400030826      *
026500030826     c                   add       fss005b       t_Com_A_Gio
026600030826     c                   add       fss016b       t_Com_A_Gio
026700030826     c                   add       fss019b       t_Com_A_Gio
026800030826     c                   add       fss034b       t_Com_A_Gio
026900030826     c                   add       fssXX1b       t_Com_A_Gio
027000030826     c                   add       fssXX2b       t_Com_A_Gio
027100091113      * SE TOTALE AUT SOMMO
027200091113     c                   if        partsp   = '2'
027300030827     c                   add       t_Cos_P_Gio   t_Cos_P_Pdr
027400030827     c                   add       t_Com_P_Gio   t_Com_P_Pdr
027500030827     c                   add       t_Cos_A_Gio   t_Cos_A_Pdr
027600030827     c                   add       t_Com_A_Gio   t_Com_A_Pdr
027700120403     C                   add       t_BLAF_C_gio  t_BLAF_C_Pdr
027800120403     c                   add       t_BLNC_C_gio  t_BLNC_C_Pdr
027900120403     c                   add       t_RMAF_R_gio  t_RMAF_R_Pdr
028000120403     c                   add       t_RMFA_R_gio  t_RMFA_R_Pdr
028100091113     C                   END
028200091112     c* consegne
028300091112     c                   eval      ftttsr = 'C'
028400091113     c     kftt          setll     fiftt02l
028500091113     c                   do        *hival
028600091113     c     kftt          reade     fiftt02l
028700091113     c                   if        %eof(fiftt02l)
028800091113     c                   leave
028900091113     c                   end
029000091113     c                   add       fttset        t_nsp_C_gio
029100091116     C*???
029200091116     c                   sub       fttsnE        t_nsp_C_gio
029300091118     c                   add       fttcla        t_cla_C_gio
029400091113     c                   add       fttsna        t_sna_C_gio
029500091113     c                   add       fttkfa        t_kfa_C_gio
029600091113     c                   add       ftttvl        t_tvl_C_gio
029700091113     c                   enddo
029800120403      *
029900091113      * SE TOTALE AUT SOMMO
030000091113     c                   if        partsp   = '2'
030100091113     c                   add       t_nsp_C_gio   t_nsp_C_pdr
030200091118     c                   add       t_cla_C_gio   t_cla_C_pdr
030300091113     c                   add       t_sna_C_gio   t_sna_C_pdr
030400091113     c                   add       t_kfa_C_gio   t_kfa_C_pdr
030500091113     c                   add       t_tvl_C_gio   t_tvl_C_pdr
030600091113     c                   end
030700091112     c* ritiri
030800091112     c                   eval      ftttsr = 'R'
030900091113     c     kftt          setll     fiftt02l
031000091113     c                   do        *hival
031100091113     c     kftt          reade     fiftt02l
031200091113     c                   if        %eof(fiftt02l)
031300091113     c                   leave
031400091113     c                   end
031500120403      *
031600091113     c                   add       fttset        t_nsp_R_gio
031700091116     c* ???
031800091116     c                   sub       fttsne        t_nsp_R_gio
031900091118     c                   add       fttcla        t_cla_R_gio
032000091118     c                   add       fttsna        t_sna_R_gio
032100091113     c                   add       fttkfa        t_kfa_R_gio
032200091113     c                   add       ftttvl        t_tvl_R_gio
032300091113     c                   enddo
032400120403      *
032500091113      * SE TOTALE AUT SOMMO
032600091113     c                   if        partsp   = '2'
032700091113     c                   add       t_nsp_R_gio   t_nsp_R_pdr
032800091118     c                   add       t_cla_R_gio   t_cla_R_pdr
032900091113     c                   add       t_sna_R_gio   t_sna_R_pdr
033000091113     c                   add       t_kfa_R_gio   t_kfa_R_pdr
033100091113     c                   add       t_tvl_R_gio   t_tvl_R_pdr
033200091113     c                   end
033300120403      *
033400091112      *  se dettaglio gg
033500091112     c                   if        partsp   = '1'
033600120403     c                   move      fssddc        FN10ddc
033700120403      *-
033800091112     c                   exsr      srscrivigg
033900030826     c                   End
034000030826      *
034100030826     c                   Endsr
034200120403      *--------------------------------------------------------------*
034300120403      *  scrivo record aut a totale
034400120403      *--------------------------------------------------------------*
034500120403     c     Srscrivipdr   BEGSR
034600120403      *-
034700120403     c                   if        T_Cos_A_Pdr <> 0 or
034800120403     c                             t_Com_A_Pdr <> 0 or
034900120403     c                             t_nsp_C_Pdr <> 0 or
035000120403     c                             t_cla_C_Pdr <> 0 or
035100120403     c                             t_sna_C_Pdr <> 0 or
035200120403     c                             t_kfa_C_Pdr <> 0 or
035300120403     c                             t_tvl_C_Pdr <> 0 or
035400120403     c                             t_BLAF_C_Pdr<> 0 or
035500120403     c                             t_BLNC_C_Pdr<> 0
035600120403     c                   eval      FN10PDR    =  savpdr
035700120403     c                   eval      FN10DDC    =  0
035800120403     c                   eval      FN10TSR    =  'C'
035900120403     c                   eval      FN10MCP    =  savmcp
036000120403     c                   move      savkm         FN10KM
036100120523     c                   move      savPDA        FN10PDA
036200120403     c                   eval      FN10rsc    =  savrsc
036300120403     c                   eval      FN10VLU    =  savVLU
036400120403     c                   eval      FN10PKL    =  savPKL
036500120403     c                   eval      FN10COSTI  =  T_Cos_A_Pdr
036600120403     c                   eval      FN10COMP   =  t_Com_A_Pdr
036700120403     c                   eval      FN10NSP    =  t_nsp_C_Pdr
036800120403     c                   eval      FN10cla    =  t_cla_C_Pdr
036900120403     c                   eval      FN10SNA    =  t_sna_C_Pdr
037000120403     c                   eval      FN10KFA    =  t_kfa_C_Pdr
037100120403     c                   eval      FN10TVL    =  t_tvl_C_Pdr
037200120403     c                   eval      FN10BLAF   =  t_BLAF_C_Pdr
037300120403     c                   eval      FN10BLNC   =  t_BLNC_C_Pdr
037400120403     c                   eval      FN10RMAF   =  0
037500120403     c                   eval      FN10RMFA   =  0
037600120403     c                   write     wficnd10
037700120403     c                   end
037800120403     c*
037900120403     c                   if        T_Cos_P_Pdr <> 0 or
038000120403     c                             t_Com_P_Pdr <> 0 or
038100120403     c                             t_nsp_R_Pdr <> 0 or
038200120403     c                             t_cla_R_Pdr <> 0 or
038300120403     c                             t_sna_R_Pdr <> 0 or
038400120403     c                             t_kfa_R_Pdr <> 0 or
038500120403     c                             t_tvl_R_Pdr <> 0 or
038600120403     c                             t_RMAF_R_Pdr<> 0 or
038700120403     c                             t_RMFA_R_Pdr<> 0
038800120403     c                   eval      FN10PDR    =  savpdr
038900120403     c                   eval      FN10DDC    =  0
039000120403     c                   eval      FN10TSR    =  'R'
039100120403     c                   eval      FN10MCP    =  savmcp
039200120403     c                   move      savkm         FN10KM
039300120523     c                   move      savPDA        FN10PDA
039400120403     c                   eval      FN10rsc    =  savrsc
039500120403     c                   eval      FN10VLU    =  savVLU
039600120403     c                   eval      FN10PKL    =  savPKL
039700120403     c                   eval      FN10COSTI  =  T_Cos_P_Pdr
039800120403     c                   eval      FN10COMP   =  t_Com_P_Pdr
039900120403     c                   eval      FN10NSP    =  t_nsp_R_Pdr
040000120403     c                   eval      FN10cla    =  t_cla_R_Pdr
040100120403     c                   eval      FN10SNA    =  t_sna_R_Pdr
040200120403     c                   eval      FN10KFA    =  t_kfa_R_Pdr
040300120403     c                   eval      FN10TVL    =  t_tvl_R_Pdr
040400120403     c                   eval      FN10BLAF   =  0
040500120403     c                   eval      FN10BLNC   =  0
040600120403     c                   eval      FN10RMAF   =  t_RMAF_R_Pdr
040700120403     c                   eval      FN10RMFA   =  t_RMFA_R_Pdr
040800120403     c                   write     wficnd10
040900120403     c                   end
041000120403     c*-
041100120403     c                   ENDSR
041200120403      *--------------------------------------------------------------*
041300120403      *  scrivo record aut x giornata
041400120403      *--------------------------------------------------------------*
041500120403     c     Srscrivigg    BEGSR
041600120403      *-
041700120403     c                   eval      FN10PDR    =  savpdr
041800120403     c                   eval      FN10TSR    =  'C'
041900120403     c                   eval      FN10MCP    =  savmcp
042000120403     c                   move      savkm         FN10KM
042100120523     c                   move      savPDA        FN10PDA
042200120403     c                   eval      FN10rsc    =  savrsc
042300120403     c                   eval      FN10VLU    =  savVLU
042400120403     c                   eval      FN10PKL    =  savPKL
042500120403     c                   eval      FN10COSTI  =  T_Cos_A_Gio
042600120403     c                   eval      FN10COMP   =  t_Com_A_Gio
042700120403     c                   eval      FN10NSP    =  t_nsp_C_Gio
042800120403     c                   eval      FN10cla    =  t_cla_C_Gio
042900120403     c                   eval      FN10SNA    =  t_sna_C_Gio
043000120403     c                   eval      FN10KFA    =  t_kfa_C_gio
043100120403     c                   eval      FN10TVL    =  t_tvl_C_gio
043200120403     c                   eval      FN10BLAF   =  t_BLAF_C_gio
043300120403     c                   eval      FN10BLNC   =  t_BLNC_C_gio
043400120403     c                   eval      FN10RMAF   =  0
043500120403     c                   eval      FN10RMFA   =  0
043600120403     c                   write     wficnd10
043700120403      *-
043800120403     c                   eval      FN10PDR    =  savpdr
043900120403     c                   eval      FN10TSR    =  'R'
044000120403     c                   eval      FN10MCP    =  savmcp
044100120403     c                   move      savkm         FN10KM
044200120523     c                   move      savPDA        FN10PDA
044300120403     c                   eval      FN10rsc    =  savrsc
044400120403     c                   eval      FN10VLU    =  savVLU
044500120403     c                   eval      FN10PKL    =  savPKL
044600120403     c                   eval      FN10COSTI  =  T_Cos_P_Gio
044700120403     c                   eval      FN10COMP   =  t_Com_P_Gio
044800120403     c                   eval      FN10NSP    =  t_nsp_R_Gio
044900120403     c                   eval      FN10cla    =  t_cla_R_Gio
045000120403     c                   eval      FN10SNA    =  t_sna_R_Gio
045100120403     c                   eval      FN10KFA    =  t_kfa_R_GIO
045200120403     c                   eval      FN10TVL    =  t_tvl_R_GIO
045300120403     c                   eval      FN10BLAF   =  0
045400120403     c                   eval      FN10BLNC   =  0
045500120403     c                   eval      FN10RMAF   =  t_RMAF_R_gio
045600120403     c                   eval      FN10RMFA   =  t_RMFA_R_gio
045700120403     c                   write     wficnd10
045800120403      *-
045900120403     c                   ENDSR
046000110822      * ---------------------------------------------------------------
046100110822     c     Calc_CosMedio Begsr
046200110822      *
046300110822      *   Richiama Routine di Calcolo costo Medio Giornaliero
046400120403      *    che crea il WFICN10CM
046500120404     C                   call      'FICND10R1'
046600120404     C                   parm                    Kpjba
046700110822      *
046800110822     c                   Endsr
046900110822      * ---------------------------------------------------------------
