000100030117      *****************************************************************
000200030825      *      stampa Saldi di periodo
000300030117      *****************************************************************
000400091112     Ffiftt02L  IF   E           K DISK                                         x Data Prestazione
000500091113     Ffiapd01L  IF   E           K DISK
000600091112     Ffifss02L  IF   E           K DISK                                         x Data Prestazione
000601120403     Ffiqdt05L  IF   E           K DISK                                         x Data Prestazione
000700120403     Fwficnd10f O    E             disk    usropn
000800030117      *****************************************************************
000900030131      *
001000030129      *****************************************************************
001100030117     d KPJBA         E DS
001200030825      *
001300091112     D ficnd7ds      e DS
001400091112     D ddatiute      e ds
001500091112     D azuteds       e ds                  extname(AZUTE00F)
001600091112     D tibs34ds      E DS                  inz
001700091112     D dapdflr       E DS                  inz
001800030121      *
001900030117     D WLBDAT          DS
002000030117     D  G02DAT                 1      8  0
002100030117     D  G02INV                 9     16  0
002200030117     D  G02ERR                17     17
002300030117     D  G02TGI                18     22  0
002400030121      *-
002500030212     D  dataiso        S               d   inz
002600030212     D  dataeur        S               d   inz datfmt(*eur)
002700030826      *
002800091113     d t_Cos_P_Pdr     s             19  5
002801120403     d t_Com_P_Pdr     s             19  5
002900091113     d t_Cos_A_Pdr     s             19  5
003100091113     d t_Com_A_Pdr     s             19  5
003200091113     d t_nsp_C_Pdr     s              7  0
003300091118     d t_cla_C_Pdr     s              7  0
003400091113     d t_sna_C_Pdr     s              7  0
003500091113     d t_kfa_C_Pdr     s              8  1
003600091113     d t_tvl_C_Pdr     s              9  3
003700120403     d t_nsp_R_Pdr     s              7  0
003800120403     d t_cla_R_Pdr     s              7  0
003900120403     d t_sna_R_Pdr     s              7  0
004000120403     d t_kfa_R_Pdr     s              8  1
004100120403     d t_tvl_R_Pdr     s              9  3
004101120403     d t_BLAF_C_Pdr    s              9  0
004102120403     d t_BLNC_C_Pdr    s              9  0
004103120403     d t_RMAF_R_Pdr    s              9  0
004104120403     d t_RMFA_R_Pdr    s              9  0
004200030826      *
004300030826     d t_Cos_P_Gio     s             15  5
004400030826     d t_Com_P_Gio     s             15  5
004500030826     d t_Cos_A_Gio     s             15  5
004600030826     d t_Com_A_Gio     s             15  5
004700091113     d t_nsp_C_gio     s              7  0
004800091118     d t_cla_C_gio     s              7  0
004900091113     d t_sna_C_gio     s              7  0
005000091113     d t_kfa_C_gio     s              8  1
005100091113     d t_tvl_C_gio     s              9  3
005200091113     d t_nsp_R_gio     s              7  0
005300091118     d t_cla_R_gio     s              7  0
005400091113     d t_sna_R_gio     s              7  0
005500091113     d t_kfa_R_gio     s              8  1
005600091113     d t_tvl_R_gio     s              9  3
005601120403     d t_BLAF_C_gio    s              7  0
005602120403     d t_BLNC_C_gio    s              7  0
005603120403     d t_RMAF_R_gio    s              7  0
005604120403     d t_RMFA_R_gio    s              7  0
005700030117      *****************************************************************
005800030117     C     *ENTRY        PLIST
005900030117     C                   PARM                    KPJBA
006000091112     C                   movel     KPJBU         ficnd7ds
006001120403      *
006100120403     c                   open      wficnd10f
006200091113     c* pulisce il wf
006300091113     c                   if        parclr = 'S'
006400091113     C/EXEC SQL
006500120403     C+ DELETE FROM WFicnd10F
006600091113     C/END-EXEC
006700110822      *****
006800110822     C/EXEC SQL
006900120403     C+ DELETE FROM WFicnD10CM
007000110822     C/END-EXEC
007100091113     c                   end
007200091113     c*
007300091112     c     *dtaara       define    §azute        azuteds
007400091112     c     *dtaara       define    §datiute      ddatiute
007500091112     C                   in(E)     *dtaara
007600091112     C                   IF        %Error  or  RSUT = *blanks
007700091112     C                   call      'TIBS34R'
007800091112     C                   parm                    Tibs34Ds
007900091112     C                   in        *dtaara
008000091112     c                   ENDIF
008001120403      *
008100091112     c     *like         define    fssPDR        savPDR
008200091113     c     *like         define    §apdmcp       savMCP
008201120403     c     *like         define    §apdKM        savKM
008202120403     c     *like         define    apdrsc        savRSC
008203120403     c     *like         define    apdpkl        savPKL
008204120403     c     *like         define    apdvlu        savVLU
008300091112     C*
008400091112     C     KFss          KLIST
008500091112     C                   KFLD                    F23PO
008600091112     C                   KFLD                    parp1
008700091112     C                   KFLD                    pardd1
008800091112     c                   movel     parp1         f23po             3 0
008900091113     C     Kapd          KLIST
009000091113     C                   KFLD                    APDTIP
009100091113     C                   KFLD                    FSSPDR
009200091113     c                   movel     'A'           APDTIP
009300091113     C     KFTT          KLIST
009400091113     C                   KFLD                    fsspdr
009500091113     C                   KFLD                    fttfvl
009600091113     C                   KFLD                    fssddc
009700091113     C                   KFLD                    ftttsr
009800091113     c                   movel     'C'           fttfvl
009900030120      *
009901120403     C     KQDT          KLIST
009902120403     C                   KFLD                    FSSfgs
009903120403     C                   KFLD                    fsspdr
009904120403     C                   KFLD                    fssddc
010000030117      *--------------------------------------------------------------*
010100030117      *             M A I N      L I N E
010200030117      *--------------------------------------------------------------*
010300030825      *
010400030826     C     KFss          setll     FiFSS02l
010401120403      *
010500091112     c                   do        *hival
010600091113     C                   READ      FiFSS02l
010601120403      *
010602120403      *  x EoF o raggiunto il Limite massimo esce dal ciclo
010700120403     C                   if        %Eof(FiFSS02l)   or
011000120403     c                             FssPDR > parp2
011100091116     c                   leave
011200091116     c                   end
011201120403      *
011202120403      *   Ne rileggo un altro poichè
011300091112     c* escludo quelli esclusi da autofat???
011400091112     c                   if        fsspdd = 'S'
011500091112     c                   iter
011600091112     c                   end
011601120403      *
011700120403      *  a rottura di aut  scrive ....
011800091113     c                   if        fssPDR <> savPDR
011801120403      *-
011802120403      *    dal secondo autista letto
011900091113     c                   if        partsp   = '2' and savpdr <> 0
012000091113     c                   Exsr      srscrivipdr
012100091113     c                   end
012101120403      *-
012200091113      *   salva l'autotrasportatore
012300091113     c                   z-add     fssPDR        savPDR
012400091113     c     kapd          chain     fiapd01l
012401120403      *
012500091113     C                   IF        %FOUND(FIAPD01L)
012600091113     C                   EVAL      DAPDFLR = APDFLR
012700091113     C                   ELSE
012800091113     C                   CLEAR                   DAPDFLR
012900091113     C                   END
012901120403      *
012902120403     C                   EVAL      savRSC = APDrsc
012903120403     C                   EVAL      savPKL = APDpkl
012904120403     C                   EVAL      savVLU = APDvlu
013000091113     c                   z-add     §APDMCP       savMCP
013001120403      *
013002120403      * se c'è un valore in KM lo espongo altrimenti è "000"
013003120403     c                   move      *zeros        savKM
013004120403     c                   if        §APDKM <> *blank
013005120403     c                   move      §APDKM        savKM
013006120403     c                   end
013024120403      *
013100091113     c                   if        partsp   = '2'
013200091113     c                   Z-add     0             t_Cos_P_Pdr
013300091113     c                   Z-add     0             t_Com_P_Pdr
013400091113     c                   Z-add     0             t_Cos_A_Pdr
013500091113     c                   Z-add     0             t_Com_A_Pdr
013600091113     c                   Z-add     0             t_nsp_C_pdr
013700091118     c                   Z-add     0             t_cla_C_pdr
013800091113     c                   Z-add     0             t_sna_C_pdr
013900091113     c                   Z-add     0             t_kfa_C_pdr
014000091113     c                   Z-add     0             t_tvl_C_pdr
014100091113     c                   Z-add     0             t_nsp_R_pdr
014200091118     c                   Z-add     0             t_cla_R_pdr
014300091113     c                   Z-add     0             t_sna_R_pdr
014400091113     c                   Z-add     0             t_kfa_R_pdr
014500091113     c                   Z-add     0             t_tvl_R_pdr
014501120403     C                   Z-add     0             t_BLAF_C_Pdr
014502120403     C                   Z-add     0             t_BLNC_C_Pdr
014503120403     c                   Z-add     0             t_RMAF_R_Pdr
014504120403     C                   Z-add     0             t_RMFA_R_Pdr
014600091113     c                   End
014700091113     c                   End
014701120403      *
014800030826      * Entro il Range di date esegue il dettaglio
014900091112     c                   if        FssDDC >= pardd1 and
015000091112     c                             FssDDC <= pardd2
015100091112      * elabora il dettaglio
015200091113     c                   Exsr      Srdata
015300030826     c                   End
015400030825      *
015500030826     c                   EndDO
015600110823      *
015700110823     c*  Altrimenti non rilasciava gli ultimi records scritti
015800110823     c*   che servivano al programma seguente
015900120403     c                   close     wficnd10f
016000030416     c*
016100110822     c* Calcolo costo medio Giornaliero su Richiesta Parametri
016200110822     c                   if        parCMG ='S'
016300110822     c                   Exsr      Calc_CosMedio
016400110822     c                   End
016500110822     c*
016600030415     C                   SETON                                        LR
024000091113      *--------------------------------------------------------------*
024100091113      *  somma i dati per data
024200091113      *--------------------------------------------------------------*
024300091113     c     srdata        BEGSR
024400030825      *
024500030826     c                   clear                   t_Cos_P_Gio
024600030826     c                   clear                   t_Cos_A_Gio
024700030826     c                   clear                   t_Com_P_Gio
024800030826     c                   clear                   t_Com_A_Gio
024900091112     c                   clear                   t_nsp_C_gio
025000091118     c                   clear                   t_cla_C_gio
025100091112     c                   clear                   t_sna_C_gio
025200091112     c                   clear                   t_kfa_C_gio
025300091112     c                   clear                   t_tvl_C_gio
025400091112     c                   clear                   t_nsp_R_gio
025500091118     c                   clear                   t_cla_R_gio
025600091112     c                   clear                   t_sna_R_gio
025700091112     c                   clear                   t_kfa_R_gio
025800091112     c                   clear                   t_tvl_R_gio
025801120403     C                   clear                   t_BLAF_C_gio
025802120403     c                   clear                   t_BLNC_C_gio
025803120403     c                   clear                   t_RMAF_R_gio
025804120403     c                   clear                   t_RMFA_R_gio
025805120403     C*
025806120403     c* calcola i dati della giornata dell'autista
025807120403     C     KQDT          setll     fiqdt05l
025808120403     C     KQDT          reade     fiqdt05l
025809120403     c                   DOw       not %EoF(fiqdt05l)
025811120403     C                   add       qdtnBLAF      t_BLAF_C_gio
025812120403     c                   add       qdtnBLNC      t_BLNC_C_gio
025813120403     c                   add       qdtoRMAF      t_RMAF_R_gio
025814120403     c                   add       qdtoRMFA      t_RMFA_R_gio
025816120403     C     KQDT          reade     fiqdt05l
025817120403     c                   endDO
025900030826      * Partenze
026000030826     c                   add       fss001        t_Cos_P_Gio
026100030826     c                   add       fss035        t_Cos_P_Gio
026200030826     c                   add       fss997        t_Cos_P_Gio
026300030826      *
026400030826     c                   add       fss001b       t_Com_P_Gio
026500030826     c                   add       fss035b       t_Com_P_Gio
026600030826      * Arrivi
026700030826     c                   add       fss005        t_Cos_A_Gio
026800030826     c                   add       fss016        t_Cos_A_Gio
026900030826     c                   add       fss019        t_Cos_A_Gio
027000030826     c                   add       fss034        t_Cos_A_Gio
027100030826     c                   add       fss998        t_Cos_A_Gio
027200030826     c                   add       fssXX1        t_Cos_A_Gio
027300030826     c                   add       fssXX2        t_Cos_A_Gio
027400030826      *
027500030826     c                   add       fss005b       t_Com_A_Gio
027600030826     c                   add       fss016b       t_Com_A_Gio
027700030826     c                   add       fss019b       t_Com_A_Gio
027800030826     c                   add       fss034b       t_Com_A_Gio
027900030826     c                   add       fssXX1b       t_Com_A_Gio
028000030826     c                   add       fssXX2b       t_Com_A_Gio
028100091113      * SE TOTALE AUT SOMMO
028200091113     c                   if        partsp   = '2'
028300030827     c                   add       t_Cos_P_Gio   t_Cos_P_Pdr
028400030827     c                   add       t_Com_P_Gio   t_Com_P_Pdr
028500030827     c                   add       t_Cos_A_Gio   t_Cos_A_Pdr
028600030827     c                   add       t_Com_A_Gio   t_Com_A_Pdr
028601120403     C                   add       t_BLAF_C_gio  t_BLAF_C_Pdr
028602120403     c                   add       t_BLNC_C_gio  t_BLNC_C_Pdr
028603120403     c                   add       t_RMAF_R_gio  t_RMAF_R_Pdr
028604120403     c                   add       t_RMFA_R_gio  t_RMFA_R_Pdr
028700091113     C                   END
028800091112     c* consegne
028900091112     c                   eval      ftttsr = 'C'
029000091113     c     kftt          setll     fiftt02l
029100091113     c                   do        *hival
029200091113     c     kftt          reade     fiftt02l
029300091113     c                   if        %eof(fiftt02l)
029400091113     c                   leave
029500091113     c                   end
029600091113     c                   add       fttset        t_nsp_C_gio
029700091116     C*???
029800091116     c                   sub       fttsnE        t_nsp_C_gio
029900091118     c                   add       fttcla        t_cla_C_gio
030000091113     c                   add       fttsna        t_sna_C_gio
030100091113     c                   add       fttkfa        t_kfa_C_gio
030200091113     c                   add       ftttvl        t_tvl_C_gio
030300091113     c                   enddo
030301120403      *
030400091113      * SE TOTALE AUT SOMMO
030500091113     c                   if        partsp   = '2'
030600091113     c                   add       t_nsp_C_gio   t_nsp_C_pdr
030700091118     c                   add       t_cla_C_gio   t_cla_C_pdr
030800091113     c                   add       t_sna_C_gio   t_sna_C_pdr
030900091113     c                   add       t_kfa_C_gio   t_kfa_C_pdr
031000091113     c                   add       t_tvl_C_gio   t_tvl_C_pdr
031100091113     c                   end
031200091112     c* ritiri
031300091112     c                   eval      ftttsr = 'R'
031400091113     c     kftt          setll     fiftt02l
031500091113     c                   do        *hival
031600091113     c     kftt          reade     fiftt02l
031700091113     c                   if        %eof(fiftt02l)
031800091113     c                   leave
031900091113     c                   end
031901120403      *
032000091113     c                   add       fttset        t_nsp_R_gio
032100091116     c* ???
032200091116     c                   sub       fttsne        t_nsp_R_gio
032300091118     c                   add       fttcla        t_cla_R_gio
032400091118     c                   add       fttsna        t_sna_R_gio
032500091113     c                   add       fttkfa        t_kfa_R_gio
032600091113     c                   add       ftttvl        t_tvl_R_gio
032700091113     c                   enddo
032701120403      *
032800091113      * SE TOTALE AUT SOMMO
032900091113     c                   if        partsp   = '2'
033000091113     c                   add       t_nsp_R_gio   t_nsp_R_pdr
033100091118     c                   add       t_cla_R_gio   t_cla_R_pdr
033200091113     c                   add       t_sna_R_gio   t_sna_R_pdr
033300091113     c                   add       t_kfa_R_gio   t_kfa_R_pdr
033400091113     c                   add       t_tvl_R_gio   t_tvl_R_pdr
033500091113     c                   end
033501120403      *
033600091112      *  se dettaglio gg
033700091112     c                   if        partsp   = '1'
033800120403     c                   move      fssddc        FN10ddc
033801120403      *-
033900091112     c                   exsr      srscrivigg
034000030826     c                   End
034100030826      *
034200030826     c                   Endsr
034201120403      *--------------------------------------------------------------*
034202120403      *  scrivo record aut a totale
034203120403      *--------------------------------------------------------------*
034204120403     c     Srscrivipdr   BEGSR
034205120403      *-
034206120403     c                   if        T_Cos_A_Pdr <> 0 or
034207120403     c                             t_Com_A_Pdr <> 0 or
034208120403     c                             t_nsp_C_Pdr <> 0 or
034209120403     c                             t_cla_C_Pdr <> 0 or
034210120403     c                             t_sna_C_Pdr <> 0 or
034211120403     c                             t_kfa_C_Pdr <> 0 or
034212120403     c                             t_tvl_C_Pdr <> 0 or
034213120403     c                             t_BLAF_C_Pdr<> 0 or
034214120403     c                             t_BLNC_C_Pdr<> 0
034215120403     c                   eval      FN10PDR    =  savpdr
034216120403     c                   eval      FN10DDC    =  0
034217120403     c                   eval      FN10TSR    =  'C'
034218120403     c                   eval      FN10MCP    =  savmcp
034219120403     c                   move      savkm         FN10KM
034220120403     c                   eval      FN10rsc    =  savrsc
034223120403     c                   eval      FN10VLU    =  savVLU
034224120403     c                   eval      FN10PKL    =  savPKL
034225120403     c                   eval      FN10COSTI  =  T_Cos_A_Pdr
034226120403     c                   eval      FN10COMP   =  t_Com_A_Pdr
034227120403     c                   eval      FN10NSP    =  t_nsp_C_Pdr
034228120403     c                   eval      FN10cla    =  t_cla_C_Pdr
034229120403     c                   eval      FN10SNA    =  t_sna_C_Pdr
034230120403     c                   eval      FN10KFA    =  t_kfa_C_Pdr
034231120403     c                   eval      FN10TVL    =  t_tvl_C_Pdr
034232120403     c                   eval      FN10BLAF   =  t_BLAF_C_Pdr
034233120403     c                   eval      FN10BLNC   =  t_BLNC_C_Pdr
034234120403     c                   eval      FN10RMAF   =  0
034235120403     c                   eval      FN10RMFA   =  0
034236120403     c                   write     wficnd10
034237120403     c                   end
034238120403     c*
034239120403     c                   if        T_Cos_P_Pdr <> 0 or
034240120403     c                             t_Com_P_Pdr <> 0 or
034241120403     c                             t_nsp_R_Pdr <> 0 or
034242120403     c                             t_cla_R_Pdr <> 0 or
034243120403     c                             t_sna_R_Pdr <> 0 or
034244120403     c                             t_kfa_R_Pdr <> 0 or
034245120403     c                             t_tvl_R_Pdr <> 0 or
034246120403     c                             t_RMAF_R_Pdr<> 0 or
034247120403     c                             t_RMFA_R_Pdr<> 0
034248120403     c                   eval      FN10PDR    =  savpdr
034249120403     c                   eval      FN10DDC    =  0
034250120403     c                   eval      FN10TSR    =  'R'
034251120403     c                   eval      FN10MCP    =  savmcp
034252120403     c                   move      savkm         FN10KM
034253120403     c                   eval      FN10rsc    =  savrsc
034254120403     c                   eval      FN10VLU    =  savVLU
034255120403     c                   eval      FN10PKL    =  savPKL
034256120403     c                   eval      FN10COSTI  =  T_Cos_P_Pdr
034257120403     c                   eval      FN10COMP   =  t_Com_P_Pdr
034258120403     c                   eval      FN10NSP    =  t_nsp_R_Pdr
034259120403     c                   eval      FN10cla    =  t_cla_R_Pdr
034260120403     c                   eval      FN10SNA    =  t_sna_R_Pdr
034261120403     c                   eval      FN10KFA    =  t_kfa_R_Pdr
034262120403     c                   eval      FN10TVL    =  t_tvl_R_Pdr
034263120403     c                   eval      FN10BLAF   =  0
034264120403     c                   eval      FN10BLNC   =  0
034265120403     c                   eval      FN10RMAF   =  t_RMAF_R_Pdr
034266120403     c                   eval      FN10RMFA   =  t_RMFA_R_Pdr
034267120403     c                   write     wficnd10
034268120403     c                   end
034269120403     c*-
034270120403     c                   ENDSR
034271120403      *--------------------------------------------------------------*
034272120403      *  scrivo record aut x giornata
034273120403      *--------------------------------------------------------------*
034274120403     c     Srscrivigg    BEGSR
034275120403      *-
034276120403     c                   eval      FN10PDR    =  savpdr
034277120403     c                   eval      FN10TSR    =  'C'
034278120403     c                   eval      FN10MCP    =  savmcp
034279120403     c                   move      savkm         FN10KM
034280120403     c                   eval      FN10rsc    =  savrsc
034281120403     c                   eval      FN10VLU    =  savVLU
034282120403     c                   eval      FN10PKL    =  savPKL
034283120403     c                   eval      FN10COSTI  =  T_Cos_A_Gio
034284120403     c                   eval      FN10COMP   =  t_Com_A_Gio
034285120403     c                   eval      FN10NSP    =  t_nsp_C_Gio
034286120403     c                   eval      FN10cla    =  t_cla_C_Gio
034287120403     c                   eval      FN10SNA    =  t_sna_C_Gio
034288120403     c                   eval      FN10KFA    =  t_kfa_C_gio
034289120403     c                   eval      FN10TVL    =  t_tvl_C_gio
034290120403     c                   eval      FN10BLAF   =  t_BLAF_C_gio
034291120403     c                   eval      FN10BLNC   =  t_BLNC_C_gio
034292120403     c                   eval      FN10RMAF   =  0
034293120403     c                   eval      FN10RMFA   =  0
034294120403     c                   write     wficnd10
034295120403      *-
034296120403     c                   eval      FN10PDR    =  savpdr
034297120403     c                   eval      FN10TSR    =  'R'
034298120403     c                   eval      FN10MCP    =  savmcp
034299120403     c                   move      savkm         FN10KM
034300120403     c                   eval      FN10rsc    =  savrsc
034301120403     c                   eval      FN10VLU    =  savVLU
034302120403     c                   eval      FN10PKL    =  savPKL
034303120403     c                   eval      FN10COSTI  =  T_Cos_P_Gio
034304120403     c                   eval      FN10COMP   =  t_Com_P_Gio
034305120403     c                   eval      FN10NSP    =  t_nsp_R_Gio
034306120403     c                   eval      FN10cla    =  t_cla_R_Gio
034307120403     c                   eval      FN10SNA    =  t_sna_R_Gio
034308120403     c                   eval      FN10KFA    =  t_kfa_R_GIO
034309120403     c                   eval      FN10TVL    =  t_tvl_R_GIO
034310120403     c                   eval      FN10BLAF   =  0
034311120403     c                   eval      FN10BLNC   =  0
034312120403     c                   eval      FN10RMAF   =  t_RMAF_R_gio
034313120403     c                   eval      FN10RMFA   =  t_RMFA_R_gio
034314120403     c                   write     wficnd10
034315120403      *-
034316120403     c                   ENDSR
034317110822      * ---------------------------------------------------------------
034400110822     c     Calc_CosMedio Begsr
034500110822      *
034600110822      *   Richiama Routine di Calcolo costo Medio Giornaliero
034700120403      *    che crea il WFICN10CM
034800120404     C                   call      'FICND10R1'
034900120404     C                   parm                    Kpjba
035000110822      *
035100110822     c                   Endsr
035200110822      * ---------------------------------------------------------------
