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
044100110405     C                   End
044200110316      *
044300110316     C                   Endsr
044400110316      **?------------------------------------------------------------------ */
044500110316      **    Scrittura  del segmento GID                Edi Standard
044600110316      **?------------------------------------------------------------------ */
044700110316     C     Segm_GID      Begsr
044800110316      *
044900110316     c                   if           D20blk <> *blank                          ????????????
045000110316     c                   clear                   segmento
045100110316     c                   eval         segmento = GID_sgm
045200110316     c                   eval         segmento = %Trim(segmento)
045300110316     c                              + %Trim(%EDITC(DE1496:'Z'))
045400110316     c                              + '+'
045500110316     c                              + %Trim(%EDITC(DE7224:'Z'))
045600110316     c                              + ''''
045700110316
045800110316     C                   EXSR      TIVGD
045900110316     C                   End
046000110316      *
046100110316     C                   Endsr
046200110316      **?------------------------------------------------------------------ */
046300110316      **    Scrittura  del segmento PCI                Edi Standard
046400110316      **?------------------------------------------------------------------ */
046500110316     C     Segm_PCI      Begsr
046600110411      *
046700110411      *  Se c'è almeno un Barcode
046800110411     C                   IF        DE7102 <> *BLANK
046900111223      *
047000111223     c                   eval      campo_alfa = DE7102
047100111223     c                   exsr      Char_Speciali
047200111223     c                   eval      DE7102 = campo_alfa
047300110316      *
047400110316     c                   clear                   segmento
047500110316     c                   eval         segmento = PCI_sgm
047600110316      *
047700110316     c                   eval         segmento = %Trim(segmento)
047800110316     c                              + %Trim(DE4233)
047900110316     c                              + '+'
048000110316     c                              + %Trim(DE7102)
048100110316      *
048200110316      *
048300110316     c                   if             DEA102 <> *blank
048400111223      *
048500111223     c                   eval      campo_alfa = DEA102
048600111223     c                   exsr      Char_Speciali
048700111223     c                   eval      DEA102 = campo_alfa
048800111223      *
048900110316     c                   eval         segmento = %Trim(segmento)
049000110316     c                              + ':'
049100110316     c                              + %Trim(DEA102)
049200110316     c                   end
049300110316      *
049400110316      *
049500110316     c                   if             DEB102 <> *blank
049600111223      *
049700111223     c                   eval      campo_alfa = DEB102
049800111223     c                   exsr      Char_Speciali
049900111223     c                   eval      DEB102 = campo_alfa
050000111223      *
050100110316     c                   eval         segmento = %Trim(segmento)
050200110316     c                              + ':'
050300110316     c                              + %Trim(DEB102)
050400110316     c                   end
050500110316      *
050600110316     c                   if             DEC102 <> *blank
050700111223      *
050800111223     c                   eval      campo_alfa = DEC102
050900111223     c                   exsr      Char_Speciali
051000111223     c                   eval      DEC102 = campo_alfa
051100111223      *
051200110316     c                   eval         segmento = %Trim(segmento)
051300110316     c                              + ':'
051400110316     c                              + %Trim(DEC102)
051500110316     c                   end
051600110316      *
051700110316      *
051800110316     c                   if             DED102 <> *blank
051900111223      *
052000111223     c                   eval      campo_alfa = DED102
052100111223     c                   exsr      Char_Speciali
052200111223     c                   eval      DED102 = campo_alfa
052300111223      *
052400110316     c                   eval         segmento = %Trim(segmento)
052500110316     c                              + ':'
052600110316     c                              + %Trim(DED102)
052700110316     c                   end
052800110316      *
052900110316      *
053000110316     c                   if             DEE102 <> *blank
053100111223      *
053200111223     c                   eval      campo_alfa = DEE102
053300111223     c                   exsr      Char_Speciali
053400111223     c                   eval      DEE102 = campo_alfa
053500111223      *
053600110316     c                   eval         segmento = %Trim(segmento)
053700110316     c                              + ':'
053800110316     c                              + %Trim(DEE102)
053900110316     c                   end
054000110316      *
054100110316     c                   if             DEF102 <> *blank
054200111223      *
054300111223     c                   eval      campo_alfa = DEF102
054400111223     c                   exsr      Char_Speciali
054500111223     c                   eval      DEF102 = campo_alfa
054600111223      *
054700110316     c                   eval         segmento = %Trim(segmento)
054800110316     c                              + ':'
054900110316     c                              + %Trim(DEF102)
055000110316     c                   end
055100110316      *
055200110316      *
055300110316     c                   if             DEG102 <> *blank
055400111223      *
055500111223     c                   eval      campo_alfa = DEG102
055600111223     c                   exsr      Char_Speciali
055700111223     c                   eval      DEG102 = campo_alfa
055800111223      *
055900110316     c                   eval         segmento = %Trim(segmento)
056000110316     c                              + ':'
056100110316     c                              + %Trim(DEG102)
056200110316     c                   end
056300110316      *
056400110316      *
056500110316     c                   if             DEH102 <> *blank
056600111223      *
056700111223     c                   eval      campo_alfa = DEH102
056800111223     c                   exsr      Char_Speciali
056900111223     c                   eval      DEH102 = campo_alfa
057000111223      *
057100110316     c                   eval         segmento = %Trim(segmento)
057200110316     c                              + ':'
057300110316     c                              + %Trim(DEH102)
057400110316     c                   end
057500110316      *
057600110316      *
057700110316     c                   if             DEI102 <> *blank
057800111223      *
057900111223     c                   eval      campo_alfa = DEI102
058000111223     c                   exsr      Char_Speciali
058100111223     c                   eval      DEI102 = campo_alfa
058200111223      *
058300110316     c                   eval         segmento = %Trim(segmento)
058400110316     c                              + ':'
058500110316     c                              + %Trim(DEI102)
058600110316     c                   end
058700110316      *
058800110316      *
058900110316     c                   eval         segmento = %Trim(segmento)
059000110316     c                              + ''''
059100110316
059200110316     C                   EXSR      TIVGD
059300110316      *
059400110316      *
059500110316      *  secondi 10 elementi
059600110316     c                   clear                   segmento
059700110316     c                   if             DEL102 <> *blank
059800111223      *
059900111223     c                   eval      campo_alfa = DEL102
060000111223     c                   exsr      Char_Speciali
060100111223     c                   eval      DEL102 = campo_alfa
060200111223      *
060300110316     c                   eval         segmento = %Trim(segmento)
060400110316     c                              + %Trim(DE4233)
060500110316     c                              + '+'
060600110316     c                              + %Trim(DEL102)
060700110316     c                   end
060800110316      *
060900110316      *
061000110316     c                   if             DEM102 <> *blank
061100111223      *
061200111223     c                   eval      campo_alfa = DEM102
061300111223     c                   exsr      Char_Speciali
061400111223     c                   eval      DEM102 = campo_alfa
061500111223      *
061600110316     c                   eval         segmento = %Trim(segmento)
061700110316     c                              + ':'
061800110316     c                              + %Trim(DEM102)
061900110316     c                   end
062000110316      *
062100110316      *
062200110316     c                   if             DEN102 <> *blank
062300111223      *
062400111223     c                   eval      campo_alfa = DEN102
062500111223     c                   exsr      Char_Speciali
062600111223     c                   eval      DEN102 = campo_alfa
062700111223      *
062800110316     c                   eval         segmento = %Trim(segmento)
062900110316     c                              + ':'
063000110316     c                              + %Trim(DEN102)
063100110316     c                   end
063200110316      *
063300110316      *
063400110316     c                   if             DEO102 <> *blank
063500111223      *
063600111223     c                   eval      campo_alfa = DEO102
063700111223     c                   exsr      Char_Speciali
063800111223     c                   eval      DEO102 = campo_alfa
063900111223      *
064000110316     c                   eval         segmento = %Trim(segmento)
064100110316     c                              + ':'
064200110316     c                              + %Trim(DEO102)
064300110316     c                   end
064400110316      *
064500110316      *
064600110316     c                   if             DEP102 <> *blank
064700111223      *
064800111223     c                   eval      campo_alfa = DEP102
064900111223     c                   exsr      Char_Speciali
065000111223     c                   eval      DEP102 = campo_alfa
065100111223      *
065200110316     c                   eval         segmento = %Trim(segmento)
065300110316     c                              + ':'
065400110316     c                              + %Trim(DEP102)
065500110316     c                   end
065600110316      *
065700110316      *
065800110316     c                   if             DEQ102 <> *blank
065900111223      *
066000111223     c                   eval      campo_alfa = DEQ102
066100111223     c                   exsr      Char_Speciali
066200111223     c                   eval      DEQ102 = campo_alfa
066300111223      *
066400110316     c                   eval         segmento = %Trim(segmento)
066500110316     c                              + ':'
066600110316     c                              + %Trim(DEQ102)
066700110316     c                   end
066800110316      *
066900110316      *
067000110316     c                   if             DER102 <> *blank
067100111223      *
067200111223     c                   eval      campo_alfa = DER102
067300111223     c                   exsr      Char_Speciali
067400111223     c                   eval      DER102 = campo_alfa
067500111223      *
067600110316     c                   eval         segmento = %Trim(segmento)
067700110316     c                              + ':'
067800110316     c                              + %Trim(DER102)
067900110316     c                   end
068000110316      *
068100110316      *
068200110316     c                   if             DES102 <> *blank
068300111223      *
068400111223     c                   eval      campo_alfa = DES102
068500111223     c                   exsr      Char_Speciali
068600111223     c                   eval      DES102 = campo_alfa
068700111223      *
068800110316     c                   eval         segmento = %Trim(segmento)
068900110316     c                              + ':'
069000110316     c                              + %Trim(DES102)
069100110316     c                   end
069200110316      *
069300110316     c                   if             DET102 <> *blank
069400111223      *
069500111223     c                   eval      campo_alfa = DET102
069600111223     c                   exsr      Char_Speciali
069700111223     c                   eval      DET102 = campo_alfa
069800111223      *
069900110316     c                   eval         segmento = %Trim(segmento)
070000110316     c                              + ':'
070100110316     c                              + %Trim(DET102)
070200110316     c                   end
070300110316      *
070400110316      *
070500110316     c                   if             DEU102 <> *blank
070600111223      *
070700111223     c                   eval      campo_alfa = DEU102
070800111223     c                   exsr      Char_Speciali
070900111223     c                   eval      DEU102 = campo_alfa
071000111223      *
071100110316     c                   eval         segmento = %Trim(segmento)
071200110316     c                              + ':'
071300110316     c                              + %Trim(DEU102)
071400110316     c                   end
071500110316      *
071600110316      *
071700110316     c                   if           segmento <> *blank
071800110316     c                   eval         segmento = %Trim(segmento)
071900110316     c                              + ''''
072000110316
072100110316     C                   EXSR      TIVGD
072200110316     c                   end
072300110316      *
072400110411     c                   end
072500110411      *
072600110316     C                   Endsr
072700110316      **?------------------------------------------------------------------ */
072800110316      **    Scrittura  del segmento FTX                Edi Standard
072900110316      **?------------------------------------------------------------------ */
073000110316     C     Segm_FTX      Begsr
073100110316      *
073200110316     c                   clear                   segmento
073300110316     c                   eval         segmento = FTX_sgm
073400110316      *
073500110316     c                   if        Tipo_DS = 'ST15'
073600110523      *
073700110523     c                   eval      campo_alfa = TC4440
073800110523     c                   exsr      Char_Speciali
073900110523     c                   eval      TC4440 = campo_alfa
074000110523      *
074100110316     c                   eval         segmento = %Trim(segmento)
074200110316     c                              + %Trim(TC4451)
074300110316     c                              + '+++'
074400110316     c                              + %Trim(TC4440)
074500110405     c                   eval         segmento = %Trim(segmento)
074600110405     c                              + ''''
074700110406     c                   open      EDIFTSTA                             99
074800110405      *
074900110405     C                   EXSR      TIVGD_SAV
075000110316      *
075100110316     c                   elseIf    Tipo_DS = 'SD15'
075200110523      *
075300110523     c                   eval      campo_alfa = DD4440
075400110523     c                   exsr      Char_Speciali
075500110523     c                   eval      DD4440 = campo_alfa
075600110523      *
075700110316     c                   eval         segmento = %Trim(segmento)
075800110316     c                              + %Trim(DD4451)
075900110316     c                              + '+++'
076000110316     c                              + %Trim(DD4440)
076100110405     c                   eval         segmento = %Trim(segmento)
076200110405     c                              + ''''
076300110405
076400110405     C                   EXSR      TIVGD
076500110405      *
076600110316     c                   end
076700110316      *
076800110316     C                   Endsr
076900110316      **?------------------------------------------------------------------ */
077000110316      **    Scrittura  del segmento LOC                Edi Standard
077100110316      **?------------------------------------------------------------------ */
077200110316     C     Segm_LOC      Begsr
077300110316      *
077400110412      *   solo se presenti dei dati
077500110412     c                   if        TB3227 <> ' ' or
077600110412     c                             TB3225 <> ' ' or
077700110412     c                             TBA227 <> ' ' or
077800110412     c                             TBA225 <> ' '
077900111223      *
078000111223     c                   eval      campo_alfa = TBA225
078100111223     c                   exsr      Char_Speciali
078200111223     c                   eval      TBA225 = campo_alfa
078300111223      *
078400111223     c                   eval      campo_alfa = TB3225
078500111223     c                   exsr      Char_Speciali
078600111223     c                   eval      TB3225 = campo_alfa
078700110412      *
078800110316     c                   clear                   segmento
078900110316     c                   eval         segmento = LOC_sgm
079000110316     c                   eval         segmento = %Trim(segmento)
079100110316     c                              + ''''
079200110316
079300110405     c                   if        Tipo_DS = 'ST10'
079400110406     c                   open      EDIFTSTA                             99
079500110405     C                   EXSR      TIVGD_SAV
079600110405     c                   else
079700110316     C                   EXSR      TIVGD
079800110405     C                   End
079900110316      *
080000110412     C                   End
080100110412      *
080200110316     C                   Endsr
080300110316      **?------------------------------------------------------------------ */
080400110316      **    Scrittura  del segmento UNT                Edi Standard
080500110316      **?------------------------------------------------------------------ */
080600110316     C     Segm_UNT      Begsr
080700110316      *
080800110316     c                   clear                   segmento
080900110316     c                   eval         segmento = UNT_sgm
081000110316     c                   eval         segmento = %Trim(segmento)
081100110412     c                              + %Trim(%EDITC(Conta_righe + 1:'Z'))
081200110316     c                              + '+1'
081300110316     c                              + ''''
081400110316
081500110316     C                   EXSR      TIVGD
081600110316      *
081700110316     C                   Endsr
081800110316      **?------------------------------------------------------------------ */
081900110316      **    Scrittura  del segmento UNZ                Edi Standard
082000110316      **?------------------------------------------------------------------ */
082100110316     C     Segm_UNZ      Begsr
082200110316      *
082300110316     c                   clear                   segmento
082400110316     c                   eval         segmento = UNZ_sgm
082500110316     c                   eval         segmento = %Trim(segmento)
082600110412     c                              + %Trim(TAPRG)
082700110316     c                              + ''''
082800110316
082900110316     C                   EXSR      TIVGD
083000110601      *
083100110601      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
083200110601     c                   if        compatta_80  = 'S'
083300110601     c                   exsr      Scarica_su_VGD
083400110601     c                   end
083500110405      * quindi chiude
083600110405      *   il TIVGD
083700110405     C                   EXSR      vgd_CLOSE
083800110316      *
083900110316     C                   Endsr
084000110316      **?------------------------------------------------------------------ */
084100110405      **    FINTO ROLLBACK
084200110316      **?------------------------------------------------------------------ */
084300110405     C     ROLL_BCK      Begsr
084400110316      *
084500110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
084600110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
084700110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
084800110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
084900110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
085000110405      *
085100110405      *
085200110405     C                   Endsr
085300110405      **?------------------------------------------------------------------ */
085400110405      **    Scrittura  del segmento                    Edi Standard
085500110405      **?------------------------------------------------------------------ */
085600110405     C     tivgd         Begsr
085700110405      *
085800110316     c                   eval       Conta_righe = 1 + Conta_righe
085900110601      *
086000110601      *  se deve scrivere il file di scarico segmento x ogni linea
086100110601     c                   if        compatta_80  <>'S'
086200110405     c                   exsr      VGD_WRITE
086300110316      *
086400110601     c                   else
086500110601      *
086600110601      *  se invece deve scrivere a 80 colonne
086700110601     c                   exsr      WRITE_80COL
086800110601      *
086900110601     c                   end
087000110601      *
087100110316     c                   endsr
087200110405      **?------------------------------------------------------------------ */
087300110405      **    Scrittura  righe di testata in QTEMP nel EDIFTSTA
087400110405      **?------------------------------------------------------------------ */
087500110405     C     tivgd_SAV     Begsr
087600110405      *
087700110405     c                   eval       STADAT = Segmento
087800110405     c                   write     IFTSTA00
087900110405      *
088000110405     c                   endsr
088100110405      **?__________________________________________________________________ */
088200110405      *    Apertura del TIVGD
088300110316      **?__________________________________________________________________ */
088400110405     C     VGD_OPEN      Begsr
088500110617      *
088600110617      *  recupera il progressivo
088700110617     c                   exsr      Piglia_progr
088800110405      *
088900110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
089000110405     C                   clear                   trul47ds
089100110405     C                   eval      d47opz  = 'I'
089200110617     C                   eval      d47tip  = TipoF
089300110405     C                   eval      d47lck  = 'N'
089400110405     C                   eval      d47chkj = 'N'
089500110405     C                   eval      d47pgm  = 'TRTCT81R1'
089600110405     C                   call      'TRUL47R'
089700110405     C                   parm                    trul47ds
089800110405      *
089900110405     c                   endsr
090000110405      **?__________________________________________________________________ */
090100110405      *   Scrittura del TIVGD
090200110405      **?__________________________________________________________________ */
090300110405     C     VGD_WRITE     Begsr
090400110405      *
090500110405     c                   clear                   tivgd000
090600110405     c                   eval      vgddta = %TrimR(Segmento)
090700110617     c                   eval      vgdtip = TipoF
090800110405     c                   eval      vgdksu = CLIENTE
090900110617     C                   eval      vgdprg = Progressivo
091000110405     c                   eval      vgdtsc = 'WW'
091100110405     c                   eval      vgdpgm = 'TRTCT81R1'
091200110405     c                   eval      vgddat = udtymd
091300110405      *
091400110405     C                   WRITE     tivgd000
091500110405      *
091600110405     c                   endsr
091700110405      **?__________________________________________________________________ */
091800110405      *    Chiusura del TIVGD
091900110405      **?__________________________________________________________________ */
092000110405     C     VGD_CLOSE     Begsr
092100110405      *
092200110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
092300110405     C                   clear                   trul47ds
092400110405     C                   eval      d47opz  = 'F'
092500110617     C                   eval      d47tip  = TipoF
092600110405     C                   call      'TRUL47R'
092700110405     C                   parm                    trul47ds
092800110405      *
092900110405     c                   endsr
093000110523      **?------------------------------------------------------------------ */
093100110523      **    Caratteri particolari        (? avanti a caratteri speciali)
093200110523      **?------------------------------------------------------------------ */
093300110523     C     Char_Speciali Begsr
093400110523      *
093500110523      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
093600110523      *   dichiarati nei segmenti    (+:'?)
093700110523      *   [più, due punti, apostrofo, punto interrogativo]
093800110523      *  Sono i caratteri che servono anche alla struttura EDI e quindi
093900110523      *   considerati caratteri Funzionali che, qualora debbano essere
094000110523      *   utilizzati come semplici caratteri di testo, dovranno essere
094100110523      *   preceduti da un (?).
094200110523      *
094300110523     c                   movel(p)  campo_alfa    campo_test
094400110523     c                   clear                   campo_ritorno
094500110523      *
094600110523     C                   call      'TRTCT00R3'
094700110523     c                   parm                    campo_test      512
094800110523     c                   parm                    campo_ritorno   512
094900110621     c                   parm                    esito
095000110523      *
095100110523     c                   movel(p)  campo_ritorno campo_alfa      512
095200110523      *
095300110523     C                   Endsr
095400110601      **?__________________________________________________________________ */
095500110601      *    Pulisce in QTEMP il file contenitore di 80 colonne
095600110601      **?__________________________________________________________________ */
095700110601     C     CLEAR_STD80F  Begsr
095800110601      *
095900110601      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
096000110601      *   a 80 colonne.
096100110601     c                   call      'TRTCT00R4C'
096200110601      *
096300110601     c                   endsr
096400110601      **?__________________________________________________________________ */
096500110601      *    Carica in  QTEMP il file contenitore di 80 colonne
096600110601      **?__________________________________________________________________ */
096700110601     C     WRITE_80COL   Begsr
096800110601      *
096900110601      * sposta la stringa del segmento appena composta
097000110601     c                   movel     segmento      segmento1950
097100110601      *
097200110601      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
097300110601     c                   call      'TRTCT00R4'
097400110601     c                   parm                    segmento1950
097500110601      *
097600110601     c                   endsr
097700110601      **?__________________________________________________________________ */
097800110601      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
097900110601      **?__________________________________________________________________ */
098000110601     C     Scarica_su_VGDBegsr
098100110601      *
098200110601     c                   open      EDISTD80F
098300110601     c                   read      EDISTD80F
098400110601      *
098500110601     c                   dow       not %EoF(EDISTD80F)
098600110601      *
098700110601     c                   eval      segmento = RIGA80STD
098800110601     c                   exsr      VGD_WRITE
098900110601      *
099000110601     c                   read      EDISTD80F
099100110601     c                   end
099200110601      *
099300110601     c                   endsr
099400110601      **?__________________________________________________________________ */
099500110617      *   Prende il Progressivo
099600110617      **?__________________________________________________________________ */
099700110617     C     Piglia_progr  Begsr
099800110617      *
099900110617     C                   MOVEL     CLIENTE       virKSC
100000110617     C                   MOVEL     TipoF         virTIP
100100110617     c     kvir02        chain     tivir02l
100200110617     c                   move      'OF'          Var_ISV
100300110617     c                   if        %Found(tivir02l)
100400110617     c                   move      VirFI1        Var_ISV
100500110617     C                   End
100600110617
100700110617      *       prende numeratore strategi
100800110617     C                   exsr      calprog
100900110617      *
101000110617     c                   endsr
101100110617     C*------------------------------------------------------*
101200110617     C*  prepara il file
101300110617     C*------------------------------------------------------*
101400110617     C     calprog       begsr
101500110617     C*
101600110617     C                   open      tis7prgf
101700110617     C                   read(e)   tis7prgf
101800110617     C*
101900110617     C                   if        not %error
102000110617     C                   eval      dwlprg = f_tis7prgf
102100110617     C*
102200110617     C                   move(p)   dwlprg        wrkprg
102300110617     C                   add       1             wrkprg
102400110617     C                   move(p)   wrkprg        dwlprg
102500110617     C                   movel     Var_ISV       dwlprg
102600110617     C*
102700110617     C                   eval       f_tis7prgf = dwlprg
102800110617     C                   eval      Progressivo = dwlprg
102900110617     C*
103000110617     C                   update    tis7prg0
103100110617     C                   endif
103200110617     C*
103300110617     C                   close     tis7prgf
103400110617     C*
103500110617     C                   endsr
103600110617      **?__________________________________________________________________ */
103700110621      * ?------------------------------------------------------------------ */
103800110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
103900110621      * ?------------------------------------------------------------------ */
104000110621     C     *pssr         BEGSR
104100110621     C
104200110621     C                   eval      esito = 'E'
104300110621     C                   ENDSR     '*CANCL'
104400110621     C
104500110621      *  ------------------------------------------------------------------ */
