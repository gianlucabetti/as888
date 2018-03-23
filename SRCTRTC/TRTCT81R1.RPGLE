000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200110316     H* TRTCT81R1 ---------------------------------------------------*
000300900418     H*                                                              *
000400110316     H* Scrittura del IFTSTA da rendere al Cliente/Partner           *
000500110316     H*                                                              *
000600050927      **?__________________________________________________________________ */
000700110405     F*---------
000800110405     FEDIFTSTA  iF A E             DISK    usropn
000900110405     F*---------
001000080424     ftivgd00f  if a E             DISK    commit
001100110601     fEdiStd80F if   E           k DISK    usropn
001200110601      *
001300110617     Ftivir02L  IF   E           K DISK
001400110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001500110617     F                                     PREFIX(f_) usropn
001600110617      **?__________________________________________________________________ */
001700110617     D TipoF           C                   CONST('EA')
001800110617     D dwlprg          s             10    INZ(*all'0')
001900110617     D wrkprg          s              8  0 INZ(*zeros)
002000060814     d trul47ds      e ds
002100060814     D W0140           S             14  0 inz
002200110316     D Segmento        s           2048
002300110601     D Segmento1950    s           1950
002400110316     D Conta_righe     s              5i 0
002500110621     d  esito          s              1
002600110617      *---------------------------------------------------------------------*
002700110617     D Var_ISV         s                   like(Virfi1)
002800110316     D*---------------------------------------------------------------------*
002900110316     D* DS
003000110316     D*---------------------------------------------------------------------*
003100110316     D* .. per scompozione dati da spedire a seconda del tipo record
003200110316     D ST00ds        E DS                  EXTNAME(EDST00DS)
003300110316     D ST05ds        E DS                  EXTNAME(EDST05DS)
003400110316     D ST10ds        E DS                  EXTNAME(EDST10DS)
003500110316     D  T10                   48    103
003600110316     D                                     DIM(2)
003700110316     D ST15ds        E DS                  EXTNAME(EDST15DS)
003800110316     D  T15                   13    362
003900110316     D                                     DIM(5)
004000110316     D SD00ds        E DS                  EXTNAME(EDSD00DS)
004100110316     D SD05ds        E DS                  EXTNAME(EDSD05DS)
004200110316     D SD10ds        E DS                  EXTNAME(EDSD10DS)
004300110316     D SD15ds        E DS                  EXTNAME(EDSD15DS)
004400110316     D  D15                   13    362
004500110316     D                                     DIM(5)
004600110316     D SD20ds        E DS                  EXTNAME(EDSD20DS)
004700110316     D  D20blk                10     22
004800110316     D  D20                   33    732
004900110316     D                                     DIM(20)
005000110316     I*---------------------------------------------------------------------*
005100110316     D UNA_sgm         C                   CONST('UNA:+.? ''')
005200110316     D UNB_sgm         C                   CONST('UNB+UNOA:2+')
005300110316     D UNH_sgm         C                   CONST('UNH+1+IFTSTA:D:94A:UN''')
005400110316     D BGM_sgm         C                   CONST('BGM+')
005500110316     D NAD_sgm         C                   CONST('NAD+')
005600110316     D LOC_sgm         C                   CONST('LOC+')
005700110316     D CNI_sgm         C                   CONST('CNI+')
005800110316     D STS_sgm         C                   CONST('STS+')
005900110316     D RFF_sgm         C                   CONST('RFF+')
006000110316     D DTM_sgm         C                   CONST('DTM+')
006100110316     D FTX_sgm         C                   CONST('FTX+')
006200110316     D GID_sgm         C                   CONST('GID+')
006300110316     D PCI_sgm         C                   CONST('PCI+')
006400110316     D UNT_sgm         C                   CONST('UNT+')
006500110411     D UNZ_sgm         C                   CONST('UNZ+1+')
006600110405     I*---------------------------------------------------------------------*
006700110405     I* FILE
006800110405     I*---------------------------------------------------------------------*
006900110405     IIFTSTA00
007000110405     I              SSIFTSTA                    staDAT
007100050927      **?__________________________________________________________________ */
007200050704      *    main
007300110316      **?__________________________________________________________________ */
007400110316     c     *entry        plist                                                   esempio
007500110316     C                   PARM                    CLIENTE           8            "03400002"
007600110316     C                   PARM                    Suffisso          2            "AZ"
007700110316     C                   PARM                    Tipo_DS           4            "ST00"
007800110316     C                   PARM                    DsGenerica     1024             EDST00DS
007900110601     C                   PARM                    Numero_Righe      5 0          nnnnn
008000110601     c                   PARM                    Compatta_80       1            S/*blk
008100110617     C                   PARM                    Progressivo      10
008200110621     c                   PARM                    esito             1
008300110617     c*
008400110617     c     kvir02        klist
008500110617     C                   kfld                    virKSC
008600110617     C                   kfld                    virTIP
008700110322     c*
008800110322     C                   z-add     Numero_Righe  Conta_righe
008900110316     c*
009000110316     C                   TIME                    ORADAT           14 0
009100110316     C                   MOVEL     ORADAT        ORATR             6 0
009200110316     c                   move      *date         udtymd            8 0
009300110316     C                   TIME                    W0140
009400110316     C                   MOVE      W0140         W0080             8 0
009500050927      **?__________________________________________________________________ */
009600110316      * Decodifica la DS in base ai Parametri
009700110316      **?__________________________________________________________________ */
009800110316      **
009900110316     c                   select
010000110316
010100110316     c                   when      Tipo_DS = 'ST00'
010200110405     c                   eval        ST00ds = DsGenerica
010300110316     c                   exsr      segm_UNA
010400110316     c                   exsr      segm_UNB
010500110316     c                   exsr      segm_UNH
010600110316     c                   exsr      segm_BGM
010700110316     c                   exsr      segm_DTM
010800110316
010900110316     c                   when      Tipo_DS = 'ST05'
011000110316     c                   eval        ST05ds = DsGenerica
011100110316     c                   exsr      segm_NAD
011200110316
011300110316     c                   when      Tipo_DS = 'ST10'
011400110316     c                   eval        ST10ds = DsGenerica
011500110316     c                   exsr      segm_RFF
011600110316     c                   exsr      segm_LOC
011700110316
011800110316     c                   when      Tipo_DS = 'ST15'
011900110316     c                   eval        ST15ds = DsGenerica
012000110316     c                   exsr      segm_FTX
012100110316
012200110405      * da qui inizia il dettaglio
012300110405     c                   when      Tipo_DS = 'SD00'
012400110405      *  solo se non aveva scritto nulla prima    Conta_righe = zero
012500110405     C                   if        Conta_righe = 0
012600110405     c                   exsr      Copia_TESTATA
012700110405     c                   end
012800110405      *
012900110316     c                   eval        SD00ds = DsGenerica
013000110316     c                   exsr      segm_CNI
013100110316
013200110316     c                   when      Tipo_DS = 'SD05'
013300110316     c                   eval        SD05ds = DsGenerica
013400110316     c                   exsr      segm_STS
013500110316     c                   exsr      segm_RFF
013600110316     c                   exsr      segm_DTM
013700110316
013800110316     c                   when      Tipo_DS = 'SD10'
013900110316     c                   eval        SD10ds = DsGenerica
014000110316     c                   exsr      segm_NAD
014100110316
014200110316     c                   when      Tipo_DS = 'SD15'
014300110316     c                   eval        SD15ds = DsGenerica
014400110316     c                   exsr      segm_FTX
014500110316
014600110316     c                   when      Tipo_DS = 'SD20'
014700110316     c                   eval        SD20ds = DsGenerica
014800110316     c                   exsr      segm_GID
014900110316     c                   exsr      segm_PCI
015000110316
015100110316     c                   when      Tipo_DS = 'FINE'
015200110412     c                   eval        TAPRG = DsGenerica
015300110316     c                   exsr      segm_UNT
015400110316     c                   exsr      segm_UNZ
015500110405      * conferma la scrittura del TIVGD
015600110405     c                   commit
015700110405
015800110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
015900110405      *  si deve impostare in questo punto del programma.
016000110405     c                   when      Tipo_DS = 'RLBK'
016100110405     c                   exsr      ROLL_BCK
016200110316
016300110316     C                   ENDSL
016400050707      *
016500110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
016600110322     C                   z-add     Conta_righe   Numero_Righe
016700110322      *
016800050705     c                   seton                                        lr
016900110316      **?------------------------------------------------------------------ */
017000110316      **    Scrittura  del segmento UNA                Edi Standard
017100110316      **?------------------------------------------------------------------ */
017200110316     C     Segm_UNA      Begsr
017300110405
017400110405      * salva la testat in QTEMP
017500110406     c                   open      EDIFTSTA                             99
017600110405
017700110316     c                   clear                   segmento
017800110318     c                   eval         segmento = UNA_sgm
017900110316
018000110405     C                   EXSR      TIVGD_SAV
018100110316      *
018200110316     C                   Endsr
018300110316      **?------------------------------------------------------------------ */
018400110316      **    Scrittura  del segmento UNB                Edi Standard
018500110316      **?------------------------------------------------------------------ */
018600110316     C     Segm_UNB      Begsr
018700110316      *
018800110316      * Campi preparatori
018900110316     c                   move      TA0017        TA6017            6
019000110316      *
019100110316     c                   clear                   segmento
019200110318     c                   eval         segmento = UNB_sgm
019300110316     c                   eval         segmento = %Trim(segmento)
019400110316     c                              + %Trim(TA0004)
019500110316     c                              + ':'
019600110316     c                              + %Trim(TA0007)
019700110316     c                              + '+'
019800110316     c                              + %Trim(TA0010)
019900110322      *
020000110322      *  Aggiunge il qualificatore se c'è
020100110322     c                   if             TAA007 <> *blank
020200110322     c                   eval         segmento = %Trim(segmento)
020300110316     c                              + ':'
020400110316     c                              + %Trim(TAA007)
020500110322     c                   end
020600110322      *
020700110322     c                   eval         segmento = %Trim(segmento)
020800110316     c                              + '+'
020900110316     c                              + %Trim(TA6017)
021000110316     c                              + ':'
021100110316     c                              + %Trim(TA0019)
021200110316     c                              + '+'
021300110318     c                              + %Trim(TAPRG)
021400110316     c                              + ''''
021500110316
021600110405     C                   EXSR      TIVGD_SAV
021700110316      *
021800110316     C                   Endsr
021900110316      **?------------------------------------------------------------------ */
022000110316      **    Scrittura  del segmento UNH                Edi Standard
022100110316      **?------------------------------------------------------------------ */
022200110316     C     Segm_UNH      Begsr
022300110316      *
022400110316     c                   clear                   segmento
022500110318     c                   eval         segmento = UNH_sgm
022600110316     c                   eval         segmento = %Trim(segmento)
022700110316
022800110405     C                   EXSR      TIVGD_SAV
022900110316      *
023000110316     C                   Endsr
023100110316      **?------------------------------------------------------------------ */
023200110316      **    Scrittura  del segmento BGM                Edi Standard
023300110316      **?------------------------------------------------------------------ */
023400110316     C     Segm_BGM      Begsr
023500110316      *
023600110322     c                   move      W0140         dati_Oramin      14
023700110316     c                   clear                   segmento
023800110318     c                   eval         segmento = BGM_sgm
023900110316     c                   eval         segmento = %Trim(segmento)
024000110316     c                              + %Trim(TA1001)
024100110316     c                              + '+'
024200110322     c                              + %Trim(dati_Oramin)
024300110316     c                              + '+'
024400110318     c                              + %Trim(TA1225)
024500110316     c                              + ''''
024600110316
024700110405     C                   EXSR      TIVGD_SAV
024800110316      *
024900110316     C                   Endsr
025000110316      **?------------------------------------------------------------------ */
025100110316      **    Scrittura  del segmento DTM                Edi Standard
025200110316      **?------------------------------------------------------------------ */
025300110316     C     Segm_DTM      Begsr
025400110316      *
025500110316     c                   clear                   segmento
025600110316     c                   eval         segmento = DTM_sgm
025700110316      *
025800110405      *  in testata
025900110316     c                   if        Tipo_DS = 'ST00'
026000110316     c                   eval         segmento = %Trim(segmento)
026100110316     c                              + %Trim(TA2005)
026200110316     c                              + ':'
026300110316     c                              + %Trim(TA2380)
026400110316     c                              + ':'
026500110316     c                              + %Trim(TA2379)
026600110405     c                   eval         segmento = %Trim(segmento)
026700110405     c                              + ''''
026800110405      *
026900110405     C                   EXSR      TIVGD_SAV
027000110316      *
027100110405      *  in dettaglio
027200110316     c                   elseIf    Tipo_DS = 'SD05'
027300110316     c                   eval         segmento = %Trim(segmento)
027400110316     c                              + %Trim(DB2005)
027500110316     c                              + ':'
027600110316     c                              + %Trim(DB2380)
027700110316     c                              + ':'
027800110316     c                              + %Trim(DB2379)
027900110405     c                   eval         segmento = %Trim(segmento)
028000110405     c                              + ''''
028100110405
028200110405     C                   EXSR      TIVGD
028300110405      *
028400110316     c                   end
028500110316      *
028600110316     C                   Endsr
028700110316      **?------------------------------------------------------------------ */
028800110316      **    Scrittura  del segmento NAD                Edi Standard
028900110316      **?------------------------------------------------------------------ */
029000110316     C     Segm_NAD      Begsr
029100110316      *
029200110316     c                   clear                   segmento
029300110316     c                   eval         segmento = NAD_sgm
029400110316      *
029500110405      *  in testata
029600110316     c                   if        Tipo_DS = 'ST05'
029700110601      *
029800110601     c                   eval      campo_alfa = TE3036
029900110601     c                   exsr      Char_Speciali
030000110601     c                   eval      TE3036 = campo_alfa
030100110601      *
030200110316     c                   eval         segmento = %Trim(segmento)
030300110316     c                              + %Trim(TE3035)
030400110316     c                              + '+'
030500110316     c                              + %Trim(TE3036)
030600110405     c                   eval         segmento = %Trim(segmento)
030700110405     c                              + ''''
030800110406     c                   open      EDIFTSTA                             99
030900110405      *
031000110405     C                   EXSR      TIVGD_SAV
031100110316      *
031200110405      *  in dettaglio
031300110316     c                   elseif    Tipo_DS = 'SD10'
031400110316     c                   eval         segmento = %Trim(segmento)
031500110316      *
031600110316     c                   end
031700110316      *
031800110316     c                   eval         segmento = %Trim(segmento)
031900110316     c                              + %Trim(DC3035)
032000110322      *
032100110322     c                   if             DC3036 <> *blank
032200110523      *
032300110523     c                   eval      campo_alfa = DC3036
032400110523     c                   exsr      Char_Speciali
032500110523     c                   eval      DC3036 = campo_alfa
032600110523      *
032700110322     c                   eval         segmento = %Trim(segmento)
032800110504     c                              + '+++'
032900110316     c                              + %Trim(DC3036)
033000110405     c                   eval         segmento = %Trim(segmento)
033100110405     c                              + ''''
033200110405
033300110405     C                   EXSR      TIVGD
033400110322     c                   end
033500110316      *
033600110316     C                   Endsr
033700110316      **?------------------------------------------------------------------ */
033800110405      **    Prima del primo CNI valido scrive la TESTATA parcheggiata su EDIFTSTA
033900110316      **?------------------------------------------------------------------ */
034000110405     C     Copia_Testata Begsr
034100110405      *
034200110405      * riporta sul TIVGD la testata parcheggiata su EDIFTSTA
034300110405      * SOLO SE
034400110405      *   il CONTA RIGHE era (0) ossia è il primo dettaglio in ASSOLUTO
034500110405      *   del messaggio
034600110405      *  quindi
034700110405      * Apre
034800110405      *   il TIVGD
034900110405     C                   EXSR      vgd_OPEN
035000110601      *
035100110601      *  Se deve compattare pulisce il file di lavoro in QTEMP EDISTD80F
035200110601     c                   if        compatta_80  ='S'
035300110601     C                   EXSR      CLEAR_STD80F
035400110601     c                   end
035500110405      *
035600110405      *  per partire dall'inizio
035700110405      * dal primo record scritto  CHIUDE E RIAPRE in Qtemp
035800110406     c                   close     EDIFTSTA                             99
035900110406     c                   open      EDIFTSTA                             99
036000110405      *  poi copia
036100110405     c                   read      EDIFTSTA
036200110405     c                   dow       not %EoF(EDIFTSTA)
036300110405     c                   clear                   segmento
036400110405     c                   eval         segmento = staDAT
036500110405     C                   EXSR      TIVGD
036600110405     **
036700110405     ** Solo al Segmento UNH
036800110405      *       resetta il conteggio delle righe contando 1
036900110405     c                   if        %subst(staDAT:1:3)='UNH'
037000110405     c                   eval       Conta_righe = 1
037100110405     c                   end
037200110405     **
037300110405     c                   read      EDIFTSTA
037400110405     c                   enddo
037500110405      *
037600110405     C                   Endsr
037700110405      **?------------------------------------------------------------------ */
037800110405      **    Scrittura  del segmento CNI                Edi Standard
037900110405      **?------------------------------------------------------------------ */
038000110405     C     Segm_CNI      Begsr
038100110405      *
038200110316     c                   clear                   segmento
038300110316     c                   eval         segmento = CNI_sgm
038400110316     c                   eval         segmento = %Trim(segmento)
038500110316     c                              + %Trim(%EDITC(DA1490:'Z'))
038600110316     c                              + '+'
038700110316     c                              + %Trim(DA1004)
038800110316     c                              + '+9999'
038900110316     c                              + ''''
039000110316
039100110316     C                   EXSR      TIVGD
039200110316      *
039300110316     C                   Endsr
039400110316      **?------------------------------------------------------------------ */
039500110316      **    Scrittura  del segmento STS                Edi Standard
039600110316      **?------------------------------------------------------------------ */
039700110316     C     Segm_STS      Begsr
039800110316      *
039900110316     c                   clear                   segmento
040000110316     c                   eval         segmento = STS_sgm
040100110316     c                   eval         segmento = %Trim(segmento)
040200110316     c                              + '+'
040300110316     c                              + %Trim(DB9011)
040400110316      **   se c'è il sotto_stato
040500110316     c                   If            DB9013 <> *blank
040600110316     c                   eval         segmento = %Trim(segmento)
040700110316     c                              + '+'
040800110316     c                              + %Trim(DB9013)
040900110316     c                   end
041000110316      **
041100110316     c                   eval         segmento = %Trim(segmento)
041200110316     c                              + ''''
041300110316
041400110316     C                   EXSR      TIVGD
041500110316      *
041600110316     C                   Endsr
041700110316      **?------------------------------------------------------------------ */
041800110316      **    Scrittura  del segmento RFF                Edi Standard
041900110316      **?------------------------------------------------------------------ */
042000110316     C     Segm_RFF      Begsr
042100110316      *
042200110316     c                   clear                   segmento
042300111223      *
042400111223     c                   eval      campo_alfa = DB1154
042500111223     c                   exsr      Char_Speciali
042600111223     c                   eval      DB1154 = campo_alfa
042700111223      *
042800110316     c                   eval         segmento = RFF_sgm
042900110316     c                   eval         segmento = %Trim(segmento)
043000110316     c                              + %Trim(DB1153)
043100110316     c                              + ':'
043200110316     c                              + %Trim(DB1154)
043300110316     c                              + ''''
043400110405
043500110405     c                   if        Tipo_DS = 'ST10'
043600110406     c                   open      EDIFTSTA                             99
043700110406      *
043800110405     C                   EXSR      TIVGD_SAV
043900110405     c                   else
044000110316     C                   EXSR      TIVGD
044100140416      *
044200140416      * Solo in Dettaglio:
044300140416      *  ulteriore Riferimento NUMERICO se abilitato tab.PS
044400140416     c                   if        DB1154A <> *blank
044500140416     C                   EXSR      Segm_RFF_A
044600140416     C                   End
044700140416      *
044800110405     C                   End
044900110316      *
045000110316     C                   Endsr
045100140416      **?------------------------------------------------------------------ */
045200140416      **    Scrittura  del segmento RFF  Aggiuntivo    Edi Standard
045300140416      **?------------------------------------------------------------------ */
045400140416     C     Segm_RFF_A    Begsr
045500140416      *
045600140416     c                   clear                   segmento
045700140416      *
045800140416     c                   eval      campo_alfa = DB1154A
045900140416     c                   exsr      Char_Speciali
046000140416     c                   eval      DB1154A = campo_alfa
046100140416      *
046200140416     c                   eval         segmento = RFF_sgm
046300140416     c                   eval         segmento = %Trim(segmento)
046400140416     c                              + %Trim(DB1153A)
046500140416     c                              + ':'
046600140416     c                              + %Trim(DB1154A)
046700140416     c                              + ''''
046800140416
046900140416     C                   EXSR      TIVGD
047000140416      *
047100140416     C                   Endsr
047200110316      **?------------------------------------------------------------------ */
047300110316      **    Scrittura  del segmento GID                Edi Standard
047400110316      **?------------------------------------------------------------------ */
047500110316     C     Segm_GID      Begsr
047600110316      *
047700110316     c                   if           D20blk <> *blank                          ????????????
047800110316     c                   clear                   segmento
047900110316     c                   eval         segmento = GID_sgm
048000110316     c                   eval         segmento = %Trim(segmento)
048100110316     c                              + %Trim(%EDITC(DE1496:'Z'))
048200110316     c                              + '+'
048300110316     c                              + %Trim(%EDITC(DE7224:'Z'))
048400110316     c                              + ''''
048500110316
048600110316     C                   EXSR      TIVGD
048700110316     C                   End
048800110316      *
048900110316     C                   Endsr
049000110316      **?------------------------------------------------------------------ */
049100110316      **    Scrittura  del segmento PCI                Edi Standard
049200110316      **?------------------------------------------------------------------ */
049300110316     C     Segm_PCI      Begsr
049400110411      *
049500110411      *  Se c'è almeno un Barcode
049600110411     C                   IF        DE7102 <> *BLANK
049700111223      *
049800111223     c                   eval      campo_alfa = DE7102
049900111223     c                   exsr      Char_Speciali
050000111223     c                   eval      DE7102 = campo_alfa
050100110316      *
050200110316     c                   clear                   segmento
050300110316     c                   eval         segmento = PCI_sgm
050400110316      *
050500110316     c                   eval         segmento = %Trim(segmento)
050600110316     c                              + %Trim(DE4233)
050700110316     c                              + '+'
050800110316     c                              + %Trim(DE7102)
050900110316      *
051000110316      *
051100110316     c                   if             DEA102 <> *blank
051200111223      *
051300111223     c                   eval      campo_alfa = DEA102
051400111223     c                   exsr      Char_Speciali
051500111223     c                   eval      DEA102 = campo_alfa
051600111223      *
051700110316     c                   eval         segmento = %Trim(segmento)
051800110316     c                              + ':'
051900110316     c                              + %Trim(DEA102)
052000110316     c                   end
052100110316      *
052200110316      *
052300110316     c                   if             DEB102 <> *blank
052400111223      *
052500111223     c                   eval      campo_alfa = DEB102
052600111223     c                   exsr      Char_Speciali
052700111223     c                   eval      DEB102 = campo_alfa
052800111223      *
052900110316     c                   eval         segmento = %Trim(segmento)
053000110316     c                              + ':'
053100110316     c                              + %Trim(DEB102)
053200110316     c                   end
053300110316      *
053400110316     c                   if             DEC102 <> *blank
053500111223      *
053600111223     c                   eval      campo_alfa = DEC102
053700111223     c                   exsr      Char_Speciali
053800111223     c                   eval      DEC102 = campo_alfa
053900111223      *
054000110316     c                   eval         segmento = %Trim(segmento)
054100110316     c                              + ':'
054200110316     c                              + %Trim(DEC102)
054300110316     c                   end
054400110316      *
054500110316      *
054600110316     c                   if             DED102 <> *blank
054700111223      *
054800111223     c                   eval      campo_alfa = DED102
054900111223     c                   exsr      Char_Speciali
055000111223     c                   eval      DED102 = campo_alfa
055100111223      *
055200110316     c                   eval         segmento = %Trim(segmento)
055300110316     c                              + ':'
055400110316     c                              + %Trim(DED102)
055500110316     c                   end
055600110316      *
055700110316      *
055800110316     c                   if             DEE102 <> *blank
055900111223      *
056000111223     c                   eval      campo_alfa = DEE102
056100111223     c                   exsr      Char_Speciali
056200111223     c                   eval      DEE102 = campo_alfa
056300111223      *
056400110316     c                   eval         segmento = %Trim(segmento)
056500110316     c                              + ':'
056600110316     c                              + %Trim(DEE102)
056700110316     c                   end
056800110316      *
056900110316     c                   if             DEF102 <> *blank
057000111223      *
057100111223     c                   eval      campo_alfa = DEF102
057200111223     c                   exsr      Char_Speciali
057300111223     c                   eval      DEF102 = campo_alfa
057400111223      *
057500110316     c                   eval         segmento = %Trim(segmento)
057600110316     c                              + ':'
057700110316     c                              + %Trim(DEF102)
057800110316     c                   end
057900110316      *
058000110316      *
058100110316     c                   if             DEG102 <> *blank
058200111223      *
058300111223     c                   eval      campo_alfa = DEG102
058400111223     c                   exsr      Char_Speciali
058500111223     c                   eval      DEG102 = campo_alfa
058600111223      *
058700110316     c                   eval         segmento = %Trim(segmento)
058800110316     c                              + ':'
058900110316     c                              + %Trim(DEG102)
059000110316     c                   end
059100110316      *
059200110316      *
059300110316     c                   if             DEH102 <> *blank
059400111223      *
059500111223     c                   eval      campo_alfa = DEH102
059600111223     c                   exsr      Char_Speciali
059700111223     c                   eval      DEH102 = campo_alfa
059800111223      *
059900110316     c                   eval         segmento = %Trim(segmento)
060000110316     c                              + ':'
060100110316     c                              + %Trim(DEH102)
060200110316     c                   end
060300110316      *
060400110316      *
060500110316     c                   if             DEI102 <> *blank
060600111223      *
060700111223     c                   eval      campo_alfa = DEI102
060800111223     c                   exsr      Char_Speciali
060900111223     c                   eval      DEI102 = campo_alfa
061000111223      *
061100110316     c                   eval         segmento = %Trim(segmento)
061200110316     c                              + ':'
061300110316     c                              + %Trim(DEI102)
061400110316     c                   end
061500110316      *
061600110316      *
061700110316     c                   eval         segmento = %Trim(segmento)
061800110316     c                              + ''''
061900110316
062000110316     C                   EXSR      TIVGD
062100110316      *
062200110316      *
062300110316      *  secondi 10 elementi
062400110316     c                   clear                   segmento
062500110316     c                   if             DEL102 <> *blank
062600111223      *
062700111223     c                   eval      campo_alfa = DEL102
062800111223     c                   exsr      Char_Speciali
062900111223     c                   eval      DEL102 = campo_alfa
063000111223      *
063100110316     c                   eval         segmento = %Trim(segmento)
063200110316     c                              + %Trim(DE4233)
063300110316     c                              + '+'
063400110316     c                              + %Trim(DEL102)
063500110316     c                   end
063600110316      *
063700110316      *
063800110316     c                   if             DEM102 <> *blank
063900111223      *
064000111223     c                   eval      campo_alfa = DEM102
064100111223     c                   exsr      Char_Speciali
064200111223     c                   eval      DEM102 = campo_alfa
064300111223      *
064400110316     c                   eval         segmento = %Trim(segmento)
064500110316     c                              + ':'
064600110316     c                              + %Trim(DEM102)
064700110316     c                   end
064800110316      *
064900110316      *
065000110316     c                   if             DEN102 <> *blank
065100111223      *
065200111223     c                   eval      campo_alfa = DEN102
065300111223     c                   exsr      Char_Speciali
065400111223     c                   eval      DEN102 = campo_alfa
065500111223      *
065600110316     c                   eval         segmento = %Trim(segmento)
065700110316     c                              + ':'
065800110316     c                              + %Trim(DEN102)
065900110316     c                   end
066000110316      *
066100110316      *
066200110316     c                   if             DEO102 <> *blank
066300111223      *
066400111223     c                   eval      campo_alfa = DEO102
066500111223     c                   exsr      Char_Speciali
066600111223     c                   eval      DEO102 = campo_alfa
066700111223      *
066800110316     c                   eval         segmento = %Trim(segmento)
066900110316     c                              + ':'
067000110316     c                              + %Trim(DEO102)
067100110316     c                   end
067200110316      *
067300110316      *
067400110316     c                   if             DEP102 <> *blank
067500111223      *
067600111223     c                   eval      campo_alfa = DEP102
067700111223     c                   exsr      Char_Speciali
067800111223     c                   eval      DEP102 = campo_alfa
067900111223      *
068000110316     c                   eval         segmento = %Trim(segmento)
068100110316     c                              + ':'
068200110316     c                              + %Trim(DEP102)
068300110316     c                   end
068400110316      *
068500110316      *
068600110316     c                   if             DEQ102 <> *blank
068700111223      *
068800111223     c                   eval      campo_alfa = DEQ102
068900111223     c                   exsr      Char_Speciali
069000111223     c                   eval      DEQ102 = campo_alfa
069100111223      *
069200110316     c                   eval         segmento = %Trim(segmento)
069300110316     c                              + ':'
069400110316     c                              + %Trim(DEQ102)
069500110316     c                   end
069600110316      *
069700110316      *
069800110316     c                   if             DER102 <> *blank
069900111223      *
070000111223     c                   eval      campo_alfa = DER102
070100111223     c                   exsr      Char_Speciali
070200111223     c                   eval      DER102 = campo_alfa
070300111223      *
070400110316     c                   eval         segmento = %Trim(segmento)
070500110316     c                              + ':'
070600110316     c                              + %Trim(DER102)
070700110316     c                   end
070800110316      *
070900110316      *
071000110316     c                   if             DES102 <> *blank
071100111223      *
071200111223     c                   eval      campo_alfa = DES102
071300111223     c                   exsr      Char_Speciali
071400111223     c                   eval      DES102 = campo_alfa
071500111223      *
071600110316     c                   eval         segmento = %Trim(segmento)
071700110316     c                              + ':'
071800110316     c                              + %Trim(DES102)
071900110316     c                   end
072000110316      *
072100110316     c                   if             DET102 <> *blank
072200111223      *
072300111223     c                   eval      campo_alfa = DET102
072400111223     c                   exsr      Char_Speciali
072500111223     c                   eval      DET102 = campo_alfa
072600111223      *
072700110316     c                   eval         segmento = %Trim(segmento)
072800110316     c                              + ':'
072900110316     c                              + %Trim(DET102)
073000110316     c                   end
073100110316      *
073200110316      *
073300110316     c                   if             DEU102 <> *blank
073400111223      *
073500111223     c                   eval      campo_alfa = DEU102
073600111223     c                   exsr      Char_Speciali
073700111223     c                   eval      DEU102 = campo_alfa
073800111223      *
073900110316     c                   eval         segmento = %Trim(segmento)
074000110316     c                              + ':'
074100110316     c                              + %Trim(DEU102)
074200110316     c                   end
074300110316      *
074400110316      *
074500110316     c                   if           segmento <> *blank
074600110316     c                   eval         segmento = %Trim(segmento)
074700110316     c                              + ''''
074800110316
074900110316     C                   EXSR      TIVGD
075000110316     c                   end
075100110316      *
075200110411     c                   end
075300110411      *
075400110316     C                   Endsr
075500110316      **?------------------------------------------------------------------ */
075600110316      **    Scrittura  del segmento FTX                Edi Standard
075700110316      **?------------------------------------------------------------------ */
075800110316     C     Segm_FTX      Begsr
075900110316      *
076000110316     c                   clear                   segmento
076100110316     c                   eval         segmento = FTX_sgm
076200110316      *
076300110316     c                   if        Tipo_DS = 'ST15'
076400110523      *
076500110523     c                   eval      campo_alfa = TC4440
076600110523     c                   exsr      Char_Speciali
076700110523     c                   eval      TC4440 = campo_alfa
076800110523      *
076900110316     c                   eval         segmento = %Trim(segmento)
077000110316     c                              + %Trim(TC4451)
077100110316     c                              + '+++'
077200110316     c                              + %Trim(TC4440)
077300110405     c                   eval         segmento = %Trim(segmento)
077400110405     c                              + ''''
077500110406     c                   open      EDIFTSTA                             99
077600110405      *
077700110405     C                   EXSR      TIVGD_SAV
077800110316      *
077900110316     c                   elseIf    Tipo_DS = 'SD15'
078000110523      *
078100110523     c                   eval      campo_alfa = DD4440
078200110523     c                   exsr      Char_Speciali
078300110523     c                   eval      DD4440 = campo_alfa
078400110523      *
078500110316     c                   eval         segmento = %Trim(segmento)
078600110316     c                              + %Trim(DD4451)
078700110316     c                              + '+++'
078800110316     c                              + %Trim(DD4440)
078900110405     c                   eval         segmento = %Trim(segmento)
079000110405     c                              + ''''
079100110405
079200110405     C                   EXSR      TIVGD
079300110405      *
079400110316     c                   end
079500110316      *
079600110316     C                   Endsr
079700110316      **?------------------------------------------------------------------ */
079800110316      **    Scrittura  del segmento LOC                Edi Standard
079900110316      **?------------------------------------------------------------------ */
080000110316     C     Segm_LOC      Begsr
080100110316      *
080200110412      *   solo se presenti dei dati
080300110412     c                   if        TB3227 <> ' ' or
080400110412     c                             TB3225 <> ' ' or
080500110412     c                             TBA227 <> ' ' or
080600110412     c                             TBA225 <> ' '
080700111223      *
080800111223     c                   eval      campo_alfa = TBA225
080900111223     c                   exsr      Char_Speciali
081000111223     c                   eval      TBA225 = campo_alfa
081100111223      *
081200111223     c                   eval      campo_alfa = TB3225
081300111223     c                   exsr      Char_Speciali
081400111223     c                   eval      TB3225 = campo_alfa
081500110412      *
081600110316     c                   clear                   segmento
081700110316     c                   eval         segmento = LOC_sgm
081800110316     c                   eval         segmento = %Trim(segmento)
081900110316     c                              + ''''
082000110316
082100110405     c                   if        Tipo_DS = 'ST10'
082200110406     c                   open      EDIFTSTA                             99
082300110405     C                   EXSR      TIVGD_SAV
082400110405     c                   else
082500110316     C                   EXSR      TIVGD
082600110405     C                   End
082700110316      *
082800110412     C                   End
082900110412      *
083000110316     C                   Endsr
083100110316      **?------------------------------------------------------------------ */
083200110316      **    Scrittura  del segmento UNT                Edi Standard
083300110316      **?------------------------------------------------------------------ */
083400110316     C     Segm_UNT      Begsr
083500110316      *
083600110316     c                   clear                   segmento
083700110316     c                   eval         segmento = UNT_sgm
083800110316     c                   eval         segmento = %Trim(segmento)
083900110412     c                              + %Trim(%EDITC(Conta_righe + 1:'Z'))
084000110316     c                              + '+1'
084100110316     c                              + ''''
084200110316
084300110316     C                   EXSR      TIVGD
084400110316      *
084500110316     C                   Endsr
084600110316      **?------------------------------------------------------------------ */
084700110316      **    Scrittura  del segmento UNZ                Edi Standard
084800110316      **?------------------------------------------------------------------ */
084900110316     C     Segm_UNZ      Begsr
085000110316      *
085100110316     c                   clear                   segmento
085200110316     c                   eval         segmento = UNZ_sgm
085300110316     c                   eval         segmento = %Trim(segmento)
085400110412     c                              + %Trim(TAPRG)
085500110316     c                              + ''''
085600110316
085700110316     C                   EXSR      TIVGD
085800110601      *
085900110601      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
086000110601     c                   if        compatta_80  = 'S'
086100110601     c                   exsr      Scarica_su_VGD
086200110601     c                   end
086300110405      * quindi chiude
086400110405      *   il TIVGD
086500110405     C                   EXSR      vgd_CLOSE
086600110316      *
086700110316     C                   Endsr
086800110316      **?------------------------------------------------------------------ */
086900110405      **    FINTO ROLLBACK
087000110316      **?------------------------------------------------------------------ */
087100110405     C     ROLL_BCK      Begsr
087200110316      *
087300110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
087400110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
087500110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
087600110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
087700110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
087800110405      *
087900110405      *
088000110405     C                   Endsr
088100110405      **?------------------------------------------------------------------ */
088200110405      **    Scrittura  del segmento                    Edi Standard
088300110405      **?------------------------------------------------------------------ */
088400110405     C     tivgd         Begsr
088500110405      *
088600110316     c                   eval       Conta_righe = 1 + Conta_righe
088700110601      *
088800110601      *  se deve scrivere il file di scarico segmento x ogni linea
088900110601     c                   if        compatta_80  <>'S'
089000110405     c                   exsr      VGD_WRITE
089100110316      *
089200110601     c                   else
089300110601      *
089400110601      *  se invece deve scrivere a 80 colonne
089500110601     c                   exsr      WRITE_80COL
089600110601      *
089700110601     c                   end
089800110601      *
089900110316     c                   endsr
090000110405      **?------------------------------------------------------------------ */
090100110405      **    Scrittura  righe di testata in QTEMP nel EDIFTSTA
090200110405      **?------------------------------------------------------------------ */
090300110405     C     tivgd_SAV     Begsr
090400110405      *
090500110405     c                   eval       STADAT = Segmento
090600110405     c                   write     IFTSTA00
090700110405      *
090800110405     c                   endsr
090900110405      **?__________________________________________________________________ */
091000110405      *    Apertura del TIVGD
091100110316      **?__________________________________________________________________ */
091200110405     C     VGD_OPEN      Begsr
091300110617      *
091400110617      *  recupera il progressivo
091500110617     c                   exsr      Piglia_progr
091600110405      *
091700110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
091800110405     C                   clear                   trul47ds
091900110405     C                   eval      d47opz  = 'I'
092000110617     C                   eval      d47tip  = TipoF
092100110405     C                   eval      d47lck  = 'N'
092200110405     C                   eval      d47chkj = 'N'
092300110405     C                   eval      d47pgm  = 'TRTCT81R1'
092400110405     C                   call      'TRUL47R'
092500110405     C                   parm                    trul47ds
092600110405      *
092700110405     c                   endsr
092800110405      **?__________________________________________________________________ */
092900110405      *   Scrittura del TIVGD
093000110405      **?__________________________________________________________________ */
093100110405     C     VGD_WRITE     Begsr
093200110405      *
093300110405     c                   clear                   tivgd000
093400110405     c                   eval      vgddta = %TrimR(Segmento)
093500110617     c                   eval      vgdtip = TipoF
093600110405     c                   eval      vgdksu = CLIENTE
093700110617     C                   eval      vgdprg = Progressivo
093800110405     c                   eval      vgdtsc = 'WW'
093900110405     c                   eval      vgdpgm = 'TRTCT81R1'
094000110405     c                   eval      vgddat = udtymd
094100110405      *
094200110405     C                   WRITE     tivgd000
094300110405      *
094400110405     c                   endsr
094500110405      **?__________________________________________________________________ */
094600110405      *    Chiusura del TIVGD
094700110405      **?__________________________________________________________________ */
094800110405     C     VGD_CLOSE     Begsr
094900110405      *
095000110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
095100110405     C                   clear                   trul47ds
095200110405     C                   eval      d47opz  = 'F'
095300110617     C                   eval      d47tip  = TipoF
095400110405     C                   call      'TRUL47R'
095500110405     C                   parm                    trul47ds
095600110405      *
095700110405     c                   endsr
095800110523      **?------------------------------------------------------------------ */
095900110523      **    Caratteri particolari        (? avanti a caratteri speciali)
096000110523      **?------------------------------------------------------------------ */
096100110523     C     Char_Speciali Begsr
096200110523      *
096300110523      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
096400110523      *   dichiarati nei segmenti    (+:'?)
096500110523      *   [più, due punti, apostrofo, punto interrogativo]
096600110523      *  Sono i caratteri che servono anche alla struttura EDI e quindi
096700110523      *   considerati caratteri Funzionali che, qualora debbano essere
096800110523      *   utilizzati come semplici caratteri di testo, dovranno essere
096900110523      *   preceduti da un (?).
097000110523      *
097100110523     c                   movel(p)  campo_alfa    campo_test
097200110523     c                   clear                   campo_ritorno
097300110523      *
097400110523     C                   call      'TRTCT00R3'
097500110523     c                   parm                    campo_test      512
097600110523     c                   parm                    campo_ritorno   512
097700110621     c                   parm                    esito
097800110523      *
097900110523     c                   movel(p)  campo_ritorno campo_alfa      512
098000110523      *
098100110523     C                   Endsr
098200110601      **?__________________________________________________________________ */
098300110601      *    Pulisce in QTEMP il file contenitore di 80 colonne
098400110601      **?__________________________________________________________________ */
098500110601     C     CLEAR_STD80F  Begsr
098600110601      *
098700110601      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
098800110601      *   a 80 colonne.
098900110601     c                   call      'TRTCT00R4C'
099000110601      *
099100110601     c                   endsr
099200110601      **?__________________________________________________________________ */
099300110601      *    Carica in  QTEMP il file contenitore di 80 colonne
099400110601      **?__________________________________________________________________ */
099500110601     C     WRITE_80COL   Begsr
099600110601      *
099700110601      * sposta la stringa del segmento appena composta
099800110601     c                   movel     segmento      segmento1950
099900110601      *
100000110601      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
100100110601     c                   call      'TRTCT00R4'
100200110601     c                   parm                    segmento1950
100300110601      *
100400110601     c                   endsr
100500110601      **?__________________________________________________________________ */
100600110601      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
100700110601      **?__________________________________________________________________ */
100800110601     C     Scarica_su_VGDBegsr
100900110601      *
101000110601     c                   open      EDISTD80F
101100110601     c                   read      EDISTD80F
101200110601      *
101300110601     c                   dow       not %EoF(EDISTD80F)
101400110601      *
101500110601     c                   eval      segmento = RIGA80STD
101600110601     c                   exsr      VGD_WRITE
101700110601      *
101800110601     c                   read      EDISTD80F
101900110601     c                   end
102000110601      *
102100110601     c                   endsr
102200110601      **?__________________________________________________________________ */
102300110617      *   Prende il Progressivo
102400110617      **?__________________________________________________________________ */
102500110617     C     Piglia_progr  Begsr
102600110617      *
102700110617     C                   MOVEL     CLIENTE       virKSC
102800110617     C                   MOVEL     TipoF         virTIP
102900110617     c     kvir02        chain     tivir02l
103000110617     c                   move      'OF'          Var_ISV
103100110617     c                   if        %Found(tivir02l)
103200110617     c                   move      VirFI1        Var_ISV
103300110617     C                   End
103400110617
103500110617      *       prende numeratore strategi
103600110617     C                   exsr      calprog
103700110617      *
103800110617     c                   endsr
103900110617     C*------------------------------------------------------*
104000110617     C*  prepara il file
104100110617     C*------------------------------------------------------*
104200110617     C     calprog       begsr
104300110617     C*
104400110617     C                   open      tis7prgf
104500110617     C                   read(e)   tis7prgf
104600110617     C*
104700110617     C                   if        not %error
104800110617     C                   eval      dwlprg = f_tis7prgf
104900110617     C*
105000110617     C                   move(p)   dwlprg        wrkprg
105100110617     C                   add       1             wrkprg
105200110617     C                   move(p)   wrkprg        dwlprg
105300110617     C                   movel     Var_ISV       dwlprg
105400110617     C*
105500110617     C                   eval       f_tis7prgf = dwlprg
105600110617     C                   eval      Progressivo = dwlprg
105700110617     C*
105800110617     C                   update    tis7prg0
105900110617     C                   endif
106000110617     C*
106100110617     C                   close     tis7prgf
106200110617     C*
106300110617     C                   endsr
106400110617      **?__________________________________________________________________ */
106500110621      * ?------------------------------------------------------------------ */
106600110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
106700110621      * ?------------------------------------------------------------------ */
106800110621     C     *pssr         BEGSR
106900110621     C
107000110621     C                   eval      esito = 'E'
107100110621     C                   ENDSR     '*CANCL'
107200110621     C
107300110621      *  ------------------------------------------------------------------ */
