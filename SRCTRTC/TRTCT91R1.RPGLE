000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300110519     H* TRTCT81R1        ---------------------------------------------------*
000400110519      **?__________________________________________________________________ */
000500110519     H*                                                                     *
000600110519     H* Scrittura del IFCSUM da Manifest  x Partner  (EXPORT)               *
000700110519     H*  versione IFCSUM S93A UN/NIG001                                     *
000800050927      **?__________________________________________________________________ */
000900110519      *
001000080424     ftivgd00f  if a E             DISK    commit
001100110525     fEdiStd80F if   E           k DISK    usropn
001200110519      *
001300110617     Ftivir02L  IF   E           K DISK
001400110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001500110617     F                                     PREFIX(f_) usropn
001600110519      **?__________________________________________________________________ */
001700110617     D TipoF           C                   CONST('EU')
001800110617     D dwlprg          s             10    INZ(*all'0')
001900110617     D wrkprg          s              8  0 INZ(*zeros)
002000060814     d trul47ds      e ds
002100060814     D W0140           S             14  0 inz
002200110316     D Segmento        s           2048
002300110525     D Segmento1950    s           1950
002400110316     D Conta_righe     s              5i 0
002500110621     d  esito          s              1
002600111018     d lunghezza_campo...
002700111018     d                 s              3i 0
002800110617      *---------------------------------------------------------------------*
002900110617     D Var_ISV         s                   like(Virfi1)
003000110316     D*---------------------------------------------------------------------*
003100110316     D* DS
003200110316     D* .. per scompozione dati da spedire a seconda del tipo record
003300110316     I*---------------------------------------------------------------------*
003400110518     D* .. per scompozione dati ricevuti a seconda del tipo record
003500110519     D   TT00ds      E DS                  EXTNAME(EDTT00DS)
003600110519     D   TT05ds      E DS                  EXTNAME(EDTT05DS)
003700110519     D  T05sk                 13    362
003800110518     D  T05                   13    362
003900110518     D                                     DIM(5)
004000110519     D   TT10ds      E DS                  EXTNAME(EDTT10DS)
004100110518     D  T10                   10     53
004200110518     D                                     DIM(2)
004300110519     D   TT15ds      E DS                  EXTNAME(EDTT15DS)
004400110519     D   TT20ds      E DS                  EXTNAME(EDTT20DS)
004500110518     D  T20                  106    249
004600110518     D                                     DIM(3)
004700110518     D  T20D                 250    303
004800110518     D                                     DIM(3)
004900110519     D   TT25ds      E DS                  EXTNAME(EDTT25DS)
005000110518     D  T25                   68    123
005100110518     D                                     DIM(2)
005200110519     D   TT30ds      E DS                  EXTNAME(EDTT30DS)
005300110518     D*  Ds per reperimento dati peso/Volume intera spedizione
005400110518      * .. per scompozione dati ricevuti a seconda del tipo record
005500110519     D   TD00ds      E DS                  EXTNAME(EDTD00DS)
005600110518     D  D00                   77    142    DIM(3)
005700110519     D   TD05ds      E DS                  EXTNAME(EDTD05DS)
005800110519     D  D05sk                 13    362
005900110518     D  D05                   13    362    DIM(5)
006000121018     D  D05Ask               366    715
006100110518     D  D05A                 366    715    DIM(5)
006200110519     D   TD10ds      E DS                  EXTNAME(EDTD10DS)
006300110518     D  D10                   73    186    DIM(3)
006400110519     D   TD15ds      E DS                  EXTNAME(EDTD15DS)
006500110518     D  D15                  445    500    DIM(2)
006600110519     D   TD20ds      E DS                  EXTNAME(EDTD20DS)
006700110519     D   TD25ds      E DS                  EXTNAME(EDTD25DS)
006800110519     D   TD30ds      E DS                  EXTNAME(EDTD30DS)
006900110518     D  D30                   10    709    DIM(20)
007000110519     D   TD35ds      E DS                  EXTNAME(EDTD35DS)
007100110519     D   TD40ds      E DS                  EXTNAME(EDTD40DS)
007200110518      *-------
007300110520     D UNA_sgm         C                   CONST('UNA:+.? ''')
007400110518     D UNB_sgm         C                   CONST('UNB+UNOA:2+ITBAR005:ZZ+')
007500110518     D UNH_sgm         C                   CONST('UNH+1+IFCSUM:S:93A:UN:NIG001-
007600110519     D                                     ')
007700110519     D BGM_sgm         C                   CONST('BGM+')
007800110518     D CNI_sgm         C                   CONST('CNI+')
007900110518     D CNT_sgm         C                   CONST('CNT+')
008000110518     D COM_sgm         C                   CONST('COM+')
008100110519     D CTA_sgm         C                   CONST('CTA+')
008200110518     D DGS_sgm         C                   CONST('DGS+')
008300110518     D DTM_sgm         C                   CONST('DTM+')
008400110518     D EQD_sgm         C                   CONST('EQD+')
008500110518     D EQN_sgm         C                   CONST('EQN+')
008600110518     D FTX_sgm         C                   CONST('FTX+')
008700110519     D GID_sgm         C                   CONST('GID+')
008800110316     D LOC_sgm         C                   CONST('LOC+')
008900110518     D MEA_sgm         C                   CONST('MEA+')
009000110518     D MOA_sgm         C                   CONST('MOA+')
009100110518     D NAD_sgm         C                   CONST('NAD+')
009200110518     D PIA_sgm         C                   CONST('PIA+')
009300110519     D PCI_sgm         C                   CONST('PCI+')
009400110518     D RFF_sgm         C                   CONST('RFF+')
009500110518     D SEL_sgm         C                   CONST('SEL+')
009600110518     D TDT_sgm         C                   CONST('TDT+')
009700110518     D TOD_sgm         C                   CONST('TOD+')
009800110518     D TSR_sgm         C                   CONST('TSR+')
009900110518     D UNT_sgm         C                   CONST('UNT+')
010000110411     D UNZ_sgm         C                   CONST('UNZ+1+')
010100111018     D Punto_interrogativo...
010200111018     D                 C                   CONST('?')
010300111018     d un_blank        C                   CONST(' ')
010400050927      **?__________________________________________________________________ */
010500050704      *    main
010600110316      **?__________________________________________________________________ */
010700110316     c     *entry        plist                                                   esempio
010800110525     C                   PARM                    compatta_80       1            "S/ "
010900110316     C                   PARM                    CLIENTE           8            "03400002"
011000110316     C                   PARM                    Suffisso          2            "AZ"
011100110518     C                   PARM                    Tipo_DS           4            "TT00"
011200110518     C                   PARM                    DsGenerica     1950             EDTT00DS
011300110322     C                   PARM                    Numero_Righe      5 0
011400110617     C                   PARM                    Progressivo      10
011500110621     c                   PARM                    esito             1
011600110322     c*
011700110617     c     kvir02        klist
011800110617     C                   kfld                    virKSC
011900110617     C                   kfld                    virTIP
012000110617      *
012100110322     C                   z-add     Numero_Righe  Conta_righe
012200110316     c*
012300110316     C                   TIME                    ORADAT           14 0
012400110316     C                   MOVEL     ORADAT        ORATR             6 0
012500110316     c                   move      *date         udtymd            8 0
012600110316     C                   TIME                    W0140
012700110316     C                   MOVE      W0140         W0080             8 0
012800050927      **?__________________________________________________________________ */
012900110316      * Decodifica la DS in base ai Parametri
013000110518      **?__________________________________________________________________ */
013100110519     c                   clear                   segmento
013200110316      **
013300110316     c                   select
013400110316
013500110518     c                   when      Tipo_DS = 'TT00'
013600110518     c                   eval        TT00ds = DsGenerica
013700110316     c                   exsr      segm_UNA
013800110316     c                   exsr      segm_UNB
013900110316     c                   exsr      segm_UNH
014000110316     c                   exsr      segm_BGM
014100110316
014200110518     c                   when      Tipo_DS = 'TT05'
014300110518     c                   eval        TT05ds = DsGenerica
014400110519     c                   exsr      segm_FTX
014500110316
014600110518     c                   when      Tipo_DS = 'TT10'
014700110518     c                   eval        TT10ds = DsGenerica
014800110518     c                   exsr      segm_CNT
014900110316
015000110518     c                   when      Tipo_DS = 'TT15'
015100110518     c                   eval        TT15ds = DsGenerica
015200110518     c                   exsr      segm_RFF
015300110518     c                   exsr      segm_DTM
015400110316
015500110518     c                   when      Tipo_DS = 'TT20'
015600110518     c                   eval        TT20ds = DsGenerica
015700110518     c                   exsr      segm_TDT
015800110518     c                   exsr      segm_LOC
015900110518     c                   exsr      segm_DTM
016000110519
016100110519     c                   when      Tipo_DS = 'TT25'
016200110519     c                   eval        TT25ds = DsGenerica
016300110519     c                   exsr      segm_NAD
016400110519     c                   if          TF3412 <> ' ' or TF3148 <> ' '
016500110519     c                   exsr      segm_CTA
016600110519     c                   exsr      segm_COM
016700110519     c                   end
016800110519
016900110519     c                   when      Tipo_DS = 'TT30'
017000110519     c                   eval        TT30ds = DsGenerica
017100110519     c                   exsr      segm_EQD
017200110519     c                   exsr      segm_EQN
017300110519     c                   exsr      segm_SEL
017400110519      *
017500110519      *------------
017600110405      * da qui inizia il dettaglio
017700110518     c                   when      Tipo_DS = 'TD00'
017800110518     c                   eval        TD00ds = DsGenerica
017900110316     c                   exsr      segm_CNI
018000110519     c                   exsr      segm_DTM
018100110519     c                   exsr      segm_TSR
018200110519     c                   exsr      segm_MOA
018300110316
018400110518     c                   when      Tipo_DS = 'TD05'
018500110518     c                   eval        TD05ds = DsGenerica
018600110519     c                   exsr      segm_FTX
018700110316
018800110518     c                   when      Tipo_DS = 'TD10'
018900110518     c                   eval        TD10ds = DsGenerica
019000110519     c                   exsr      segm_TOD
019100110519     c                   exsr      segm_LOC
019200110519     c                   exsr      segm_RFF
019300110519     c                   exsr      segm_TDT
019400110316
019500110518     c                   when      Tipo_DS = 'TD15'
019600110518     c                   eval        TD15ds = DsGenerica
019700110519     c                   exsr      segm_NAD
019800110519     c                   if          DD3412 <> ' ' or DD3148 <> ' '
019900110519     c                   exsr      segm_CTA
020000110519     c                   exsr      segm_COM
020100110519     c                   end
020200110316
020300110518     c                   when      Tipo_DS = 'TD20'
020400110518     c                   eval        TD20ds = DsGenerica
020500110316     c                   exsr      segm_GID
020600110519     c                   exsr      segm_PIA
020700110519     c                   exsr      segm_FTX
020800110519
020900110519     c                   when      Tipo_DS = 'TD25'
021000110519     c                   eval        TD25ds = DsGenerica
021100110519     c                   exsr      segm_MEA
021200110519
021300110519     c                   when      Tipo_DS = 'TD30'
021400110519     c                   eval        TD30ds = DsGenerica
021500110519     c                   exsr      segm_PCI
021600110519
021700110519     c                   when      Tipo_DS = 'TD35'
021800110519     c                   eval        TD35ds = DsGenerica
021900110519
022000110519     c                   when      Tipo_DS = 'TD40'
022100110519     c                   eval        TD40ds = DsGenerica
022200110316
022300110316     c                   when      Tipo_DS = 'FINE'
022400110412     c                   eval        TAPRG = DsGenerica
022500110316     c                   exsr      segm_UNT
022600110316     c                   exsr      segm_UNZ
022700110405      * conferma la scrittura del TIVGD
022800110405     c                   commit
022900110405
023000110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
023100110405      *  si deve impostare in questo punto del programma.
023200110405     c                   when      Tipo_DS = 'RLBK'
023300110405     c                   exsr      ROLL_BCK
023400110316
023500110316     C                   ENDSL
023600050707      *
023700110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
023800110322     C                   z-add     Conta_righe   Numero_Righe
023900110322      *
024000050705     c                   seton                                        lr
024100110316      **?------------------------------------------------------------------ */
024200110316      **    Scrittura  del segmento UNA                Edi Standard
024300110316      **?------------------------------------------------------------------ */
024400110316     C     Segm_UNA      Begsr
024500110519      * apre
024600110519      *   il TIVGD
024700110519     C                   EXSR      vgd_OPEN
024800110525      *
024900110525      *  Se deve compattare pulisce il file di lavoro in QTEMP EDISTD80F
025000110525     c                   if        compatta_80  ='S'
025100110525     C                   EXSR      CLEAR_STD80F
025200110525     c                   end
025300110525      *
025400110318     c                   eval         segmento = UNA_sgm
025500110518     C                   EXSR      TIVGD
025600110316      *
025700110316     C                   Endsr
025800110316      **?------------------------------------------------------------------ */
025900110316      **    Scrittura  del segmento UNB                Edi Standard
026000110316      **?------------------------------------------------------------------ */
026100110316     C     Segm_UNB      Begsr
026200110316      *
026300110318     c                   eval         segmento = UNB_sgm
026400110316     c                              + %Trim(TA0010)
026500110519      *
026600110322      *  Aggiunge il qualificatore se c'è
026700110322     c                   if             TAA007 <> *blank
026800110322     c                   eval         segmento = %Trim(segmento)
026900110316     c                              + ':'
027000110316     c                              + %Trim(TAA007)
027100110322     c                   end
027200110519      *
027300110322     c                   eval         segmento = %Trim(segmento)
027400110316     c                              + '+'
027500110519     c                              + %Trim(%subst(%editc(TA0017:'X'):3:6))
027600110316     c                              + ':'
027700110519     c                              + %Editc(TA0019:'X')
027800110316     c                              + '+'
027900110518     c                              + %Trim(TAPRG)
028000110316
028100110518     C                   EXSR      TIVGD
028200110316      *
028300110316     C                   Endsr
028400110316      **?------------------------------------------------------------------ */
028500110316      **    Scrittura  del segmento UNH                Edi Standard
028600110316      **?------------------------------------------------------------------ */
028700110316     C     Segm_UNH      Begsr
028800110316      *
028900110318     c                   eval         segmento = UNH_sgm
029000110316
029100110518     C                   EXSR      TIVGD
029200110316      *
029300110316     C                   Endsr
029400110316      **?------------------------------------------------------------------ */
029500110316      **    Scrittura  del segmento BGM                Edi Standard
029600110316      **?------------------------------------------------------------------ */
029700110316     C     Segm_BGM      Begsr
029800110316      *
029900110318     c                   eval         segmento = BGM_sgm
030000110519     c                              + %Trim(TA1001)
030100110519     c                              + '+'
030200110518     c                              + %Trim(TA1004)
030300110316     c                              + '+'
030400110318     c                              + %Trim(TA1225)
030500110316
030600110518     C                   EXSR      TIVGD
030700110316      *
030800110316     C                   Endsr
030900110519      **?------------------------------------------------------------------ */
031000110519      **    Scrittura  del segmento FTX                Edi Standard
031100110519      **?------------------------------------------------------------------ */
031200110519     C     Segm_FTX      Begsr
031300110519      *
031400110519      *  testata
031500110519     c                   if        Tipo_DS = 'TT05'
031600110519     c                   eval         segmento = FTX_sgm
031700110519      *
031800110519     c                   if          T05sk <> *blank
031900110524      *
032000110524     c                   eval      campo_alfa = TB4440
032100111018     c                   eval      lunghezza_campo=%len(TB4440)
032200110524     c                   exsr      Char_Speciali
032300110524     c                   eval      TB4440 = campo_alfa
032400110519      *
032500110519     c                   eval         segmento = %Trim(segmento)
032600110519     c                              + %Trim(TB4451)
032700110519     c                              + '+++'
032800110519     c                              + %Trim(TB4440)
032900110519      *
033000110519     c                   if          T05(2) <> *blank
033100110524      *
033200110524     c                   eval      campo_alfa = TBA440
033300111018     c                   eval      lunghezza_campo=%len(TBA440)
033400110524     c                   exsr      Char_Speciali
033500110524     c                   eval      TBA440 = campo_alfa
033600110524      *
033700110519     c                   eval         segmento = %Trim(segmento)
033800110519     c                              + ':'
033900110519     c                              + %Trim(TBA440)
034000110519     c                   end
034100110519      *
034200110519     c                   if          T05(3) <> *blank
034300110524      *
034400110524     c                   eval      campo_alfa = TBB440
034500111018     c                   eval      lunghezza_campo=%len(TBB440)
034600110524     c                   exsr      Char_Speciali
034700110524     c                   eval      TBB440 = campo_alfa
034800110524      *
034900110519     c                   eval         segmento = %Trim(segmento)
035000110519     c                              + ':'
035100110519     c                              + %Trim(TBB440)
035200110519     c                   end
035300110519      *
035400110519     c                   if          T05(4) <> *blank
035500110524      *
035600110524     c                   eval      campo_alfa = TBC440
035700111018     c                   eval      lunghezza_campo=%len(TBC440)
035800110524     c                   exsr      Char_Speciali
035900110524     c                   eval      TBC440 = campo_alfa
036000110524      *
036100110519     c                   eval         segmento = %Trim(segmento)
036200110519     c                              + ':'
036300110519     c                              + %Trim(TBC440)
036400110519     c                   end
036500110519      *
036600110519     c                   if          T05(5) <> *blank
036700110524      *
036800110524     c                   eval      campo_alfa = TBD440
036900111018     c                   eval      lunghezza_campo=%len(TBD440)
037000110524     c                   exsr      Char_Speciali
037100110524     c                   eval      TBD440 = campo_alfa
037200110524      *
037300110519     c                   eval         segmento = %Trim(segmento)
037400110519     c                              + ':'
037500110519     c                              + %Trim(TBD440)
037600110519     c                   end
037700110519      *
037800110519     C                   EXSR      TIVGD
037900110519     c                   end
038000110519      *
038100110519      * dettaglio
038200110519     c                   elseIf    Tipo_DS = 'TD05'
038300110519      *
038400121018      *  Invia le note di di consegna oppure il tipo contrassegno.
038500121018      *   prima i 5 del secondo ciclo perchè potrebbe esserci il CONTRASSEGNO
038600121018     c                   if          D05Ask<> *blank
038700121018      *
038800121018     c                   eval      campo_alfa = DBF440
038900121018     c                   eval      lunghezza_campo=%len(DBF440)
039000121018     c                   exsr      Char_Speciali
039100121018     c                   eval      DBF440 = campo_alfa
039200121018      *
039300121018     c                   eval         segmento = FTX_sgm
039400121018     c                              + %Trim(DBE451)
039500121018     c                              + '+++'
039600121018     c                              + %Trim(DBF440)
039700121018      *
039800121018     c                   if          D05A(2) <> *blank
039900121018      *
040000121018     c                   eval      campo_alfa = DBG440
040100121018     c                   eval      lunghezza_campo=%len(DBG440)
040200121018     c                   exsr      Char_Speciali
040300121018     c                   eval      DBG440 = campo_alfa
040400121018      *
040500121018     c                   eval         segmento = %Trim(segmento)
040600121018     c                              + ':'
040700121018     c                              + %Trim(DBG440)
040800121018     c                   end
040900121018      *
041000121018     c                   if          D05A(3) <> *blank
041100121018      *
041200121018     c                   eval      campo_alfa = DBH440
041300121018     c                   eval      lunghezza_campo=%len(DBH440)
041400121018     c                   exsr      Char_Speciali
041500121018     c                   eval      DBH440 = campo_alfa
041600121018      *
041700121018     c                   eval         segmento = %Trim(segmento)
041800121018     c                              + ':'
041900121018     c                              + %Trim(DBH440)
042000121018     c                   end
042100121018      *
042200121018     c                   if          D05A(4) <> *blank
042300121018      *
042400121018     c                   eval      campo_alfa = DBI440
042500121018     c                   eval      lunghezza_campo=%len(DBI440)
042600121018     c                   exsr      Char_Speciali
042700121018     c                   eval      DBI440 = campo_alfa
042800121018      *
042900121018     c                   eval         segmento = %Trim(segmento)
043000121018     c                              + ':'
043100121018     c                              + %Trim(DBI440)
043200121018     c                   end
043300121018      *
043400121018     c                   if          D05A(5) <> *blank
043500121018      *
043600121018     c                   eval      campo_alfa = DBL440
043700121018     c                   eval      lunghezza_campo=%len(DBL440)
043800121018     c                   exsr      Char_Speciali
043900121018     c                   eval      DBL440 = campo_alfa
044000121018      *
044100121018     c                   eval         segmento = %Trim(segmento)
044200121018     c                              + ':'
044300121018     c                              + %Trim(DBL440)
044400121018     c                   end
044500121018      *
044600121018     C                   EXSR      TIVGD
044700121018     c                   end
044800121018      *
044900121018      * -----  NOTE DA INVIARE
045000110519     c                   if          D05sk <> *blank
045100110524      *
045200110524     c                   eval      campo_alfa = DB4440
045300111018     c                   eval      lunghezza_campo=%len(DB4440)
045400110524     c                   exsr      Char_Speciali
045500110524     c                   eval      DB4440 = campo_alfa
045600110519      *
045700110519     c                   eval         segmento = FTX_sgm
045800110519     c                              + %Trim(DB4451)
045900110519     c                              + '+++'
046000110519     c                              + %Trim(DB4440)
046100110519      *
046200110519     c                   if          D05(2) <> *blank
046300110524      *
046400110524     c                   eval      campo_alfa = DBA440
046500111018     c                   eval      lunghezza_campo=%len(DBA440)
046600110524     c                   exsr      Char_Speciali
046700110524     c                   eval      DBA440 = campo_alfa
046800110524      *
046900110519     c                   eval         segmento = %Trim(segmento)
047000110519     c                              + ':'
047100110519     c                              + %Trim(DBA440)
047200110519     c                   end
047300110519      *
047400110519     c                   if          D05(3) <> *blank
047500110524      *
047600110524     c                   eval      campo_alfa = DBB440
047700111018     c                   eval      lunghezza_campo=%len(DBB440)
047800110524     c                   exsr      Char_Speciali
047900110524     c                   eval      DBB440 = campo_alfa
048000110524      *
048100110519     c                   eval         segmento = %Trim(segmento)
048200110519     c                              + ':'
048300110519     c                              + %Trim(DBB440)
048400110519     c                   end
048500110519      *
048600110519     c                   if          D05(4) <> *blank
048700110524      *
048800110524     c                   eval      campo_alfa = DBC440
048900111018     c                   eval      lunghezza_campo=%len(DBC440)
049000110524     c                   exsr      Char_Speciali
049100110524     c                   eval      DBC440 = campo_alfa
049200110524      *
049300110519     c                   eval         segmento = %Trim(segmento)
049400110519     c                              + ':'
049500110519     c                              + %Trim(DBC440)
049600110519     c                   end
049700110519      *
049800110519     c                   if          D05(5) <> *blank
049900110524      *
050000110524     c                   eval      campo_alfa = DBD440
050100111018     c                   eval      lunghezza_campo=%len(DBD440)
050200110524     c                   exsr      Char_Speciali
050300110524     c                   eval      DBD440 = campo_alfa
050400110524      *
050500110519     c                   eval         segmento = %Trim(segmento)
050600110519     c                              + ':'
050700110519     c                              + %Trim(DBD440)
050800110519     c                   end
050900110519      *
051000110519     C                   EXSR      TIVGD
051100110519     c                   end
051200110519      * dettaglio
051300110519     c                   elseIf    Tipo_DS = 'TD20'
051400110519      *
051500110519     c                   if          DE4451 <> *blank
051600110524      *
051700110524     c                   eval      campo_alfa = DE4440
051800111018     c                   eval      lunghezza_campo=%len(DE4440)
051900110524     c                   exsr      Char_Speciali
052000110524     c                   eval      DE4440 = campo_alfa
052100110524      *
052200110519     c                   eval         segmento = FTX_sgm
052300110519     c                              + %Trim(DE4451)
052400110519     c                              + '+++'
052500110519     c                              + %Trim(DE4440)
052600110519      *
052700110519     C                   EXSR      TIVGD
052800110519     c                   end
052900110519      *
053000110519     c                   end
053100110519      *
053200110519     C                   Endsr
053300110518      **?------------------------------------------------------------------ */
053400110518      **    Scrittura  del segmento CNT                Edi Standard
053500110518      **?------------------------------------------------------------------ */
053600110518     C     Segm_CNT      Begsr
053700110518      *
053800110518     c                   eval         segmento = CNT_sgm
053900110518     c                              + %Trim(TC6069)
054000110520     c                              + ':'
054100110518     c                              + %Trim(%editc(TC6066:'Z'))
054200110519
054300110518     C                   EXSR      TIVGD
054400110518      *
054500110518      *  se c'è anche l'altro contatore:
054600110518     c                   if        TCA069 <> *blank
054700110518      *
054800110518     c                   eval         segmento = CNT_sgm
054900110518     c                              + %Trim(TCA069)
055000110520     c                              + ':'
055100110518     c                              + %Trim(%editc(TCA066:'Z'))
055200110518
055300110518     C                   EXSR      TIVGD
055400110518     C                   End
055500110518      *
055600110518     C                   Endsr
055700110518      **?------------------------------------------------------------------ */
055800110518      **    Scrittura  del segmento RFF                Edi Standard
055900110518      **?------------------------------------------------------------------ */
056000110518     C     Segm_RFF      Begsr
056100110518      *
056200110518      *   testata
056300110518     c                   if        Tipo_DS = 'TT15'
056400110518      *
056500110519     c                   if        TD1153 <> *blank
056600110519     c                   eval         segmento = RFF_sgm
056700110518     c                              + %Trim(TD1153)
056800110518     c                              + ':'
056900110518     c                              + %Trim(TD1154)
057000110519      *
057100110519     C                   EXSR      TIVGD
057200110518     c                   end
057300110519      *
057400110518      *   dettaglio
057500110519     c                   elseIf    Tipo_DS = 'TD10'
057600110519      *
057700110519     c                   if        DC1153 <> *blank
057800110518     c                   eval         segmento = RFF_sgm
057900110519     c                              + %Trim(DC1153)
058000110518     c                              + ':'
058100110519     c                              + %Trim(DC1154)
058200110519      *
058300110519     C                   EXSR      TIVGD
058400110518     c                   end
058500110519      *
058600110519     c                   if        DCA153 <> *blank
058700110519     c                   eval         segmento = RFF_sgm
058800110519     c                              + %Trim(DCA153)
058900110519     c                              + ':'
059000110519     c                              + %Trim(DCA154)
059100110519      *
059200110519     C                   EXSR      TIVGD
059300110519     c                   end
059400110519      *
059500110519     c                   if        DCB153 <> *blank
059600110519     c                   eval         segmento = RFF_sgm
059700110519     c                              + %Trim(DCB153)
059800110519     c                              + ':'
059900110519     c                              + %Trim(DCB154)
060000110519      *
060100110519     C                   EXSR      TIVGD
060200110519     c                   end
060300110519      *
060400110518     c                   end
060500110518      *
060600110518     C                   Endsr
060700110518      **?------------------------------------------------------------------ */
060800110519      **    Scrittura  del segmento TDT                Edi Standard
060900110316      **?------------------------------------------------------------------ */
061000110519     C     Segm_TDT      Begsr
061100110316      *
061200110520     c                   if        Tipo_DS = 'TT20'
061300110520      ***
061400110520     c                   eval         segmento = TDT_sgm
061500110520     c                              + %Trim(TE8051)
061600110520     c                              + '++'
061700110520     c                              + %Trim(TE8067)
061800110520      ***
061900110520     C                   EXSR      TIVGD
062000110520     C                   End
062100110519      *
062200110519     C                   Endsr
062300110519      **?------------------------------------------------------------------ */
062400110519      **    Scrittura  del segmento DTM                Edi Standard
062500110519      **?------------------------------------------------------------------ */
062600110519     C     Segm_DTM      Begsr
062700110519      *
062800110405      *  in testata
062900110518     c                   if        Tipo_DS = 'TT15'
063000110519      ***
063100110518     c                   if         TD2005 <> *blank
063200110518     c                   eval         segmento = DTM_sgm
063300110518     c                              + %Trim(TD2005)
063400110316     c                              + ':'
063500110518     c                              + %Trim(TD2380)
063600110316     c                              + ':'
063700110518     c                              + %Trim(TD2379)
063800110519      *
063900110518     C                   EXSR      TIVGD
064000110518     c                   end
064100110519      *
064200110518      ** altre date testata
064300110518     c                   elseIf    Tipo_DS = 'TT20'
064400110519      *
064500110518     c                   If          TE2005 <> *blank
064600110518     c                   eval         segmento = DTM_sgm
064700110518     c                              + %Trim(TE2005)
064800110518     c                              + ':'
064900110518     c                              + %Trim(TE2380)
065000110518     c                              + ':'
065100110518     c                              + %Trim(TE2379)
065200110519     c*
065300110518     C                   EXSR      TIVGD
065400110518     c                   end
065500110518      *-
065600110518     c                   If          TEA005 <> *blank
065700110518     c                   eval         segmento = DTM_sgm
065800110518     c                              + %Trim(TEA005)
065900110518     c                              + ':'
066000110518     c                              + %Trim(TEA380)
066100110518     c                              + ':'
066200110518     c                              + %Trim(TEA379)
066300110519      *
066400110518     C                   EXSR      TIVGD
066500110518     c                   end
066600110518      *-
066700110518     c                   If          TEB005 <> *blank
066800110518     c                   eval         segmento = DTM_sgm
066900110518     c                              + %Trim(TEB005)
067000110518     c                              + ':'
067100110518     c                              + %Trim(TEB380)
067200110518     c                              + ':'
067300110518     c                              + %Trim(TEB379)
067400110519      *
067500110518     C                   EXSR      TIVGD
067600110518     c                   end
067700110405      *  in dettaglio
067800110519     c                   elseIf    Tipo_DS = 'TD00'
067900110519      ***
068000110520     c                   If          DA2005 <> *blank
068100110518     c                   eval         segmento = DTM_sgm
068200110519     c                              + %Trim(DA2005)
068300110316     c                              + ':'
068400110519     c                              + %Trim(DA2380)
068500110316     c                              + ':'
068600110519     c                              + %Trim(DA2379)
068700110519      *
068800110519     C                   EXSR      TIVGD
068900110520     c                   end
069000110520      *
069100110518     c                   end
069200110316      *
069300110316     C                   Endsr
069400110316      **?------------------------------------------------------------------ */
069500110519      **    Scrittura  del segmento TSR                Edi Standard
069600110316      **?------------------------------------------------------------------ */
069700110519     C     Segm_TSR      Begsr
069800110316      *
069900110519     c                   if        DA4219 <> *blank
070000110519     c                   eval         segmento = TSR_sgm
070100110519     c                              + '++'
070200110519     c                              + %Trim(DA4219)
070300110519     c                   if        DA7085 <> *blank
070400110519     c                   eval         segmento = %trim(segmento)
070500110519     c                              + '+'
070600110519     c                              + %Trim(DA7085)
070700110519     c                   end
070800110520      *
070900110520     C                   EXSR      TIVGD
071000110519     c                   end
071100110519      *
071200110519     C                   Endsr
071300110519      **?------------------------------------------------------------------ */
071400110519      **    Scrittura  del segmento MOA                Edi Standard
071500110519      **?------------------------------------------------------------------ */
071600110519     C     Segm_MOA      Begsr
071700110519      *
071800110519     c                   if        DA5025 <> *blank
071900110519     c                   clear                   campo16          16
072000111128     c                   eval       campo16 = %editc(DA5004:'Z')
072100111128     c                   IF           %subst(CAMPO16:14:1) = ' '
072200111128     c                   eval         %subst(CAMPO16:14:1) = '0'
072300111128     c                   end
072400111128     c                   IF           %subst(CAMPO16:13:1) = ' '
072500111128     c                   eval         %subst(CAMPO16:13:1) = '0'
072600111128     c                   end
072700110519     c                   eval         segmento = MOA_sgm
072800110519     c                              + %Trim(DA5025)
072900110519     c                              + ':'
073000110519     c                              + %Trim(%subst(CAMPO16:1:13))
073100110519     c                              + '.'
073200110519     c                              + %subst(CAMPO16:14:2)
073300110519     c                              + ':'
073400110519     c                              + %Trim(DA6345)
073500110519      *
073600110519     C                   EXSR      TIVGD
073700110519     C                   End
073800110519      *
073900110519     c                   if        DAA025 <> *blank
074000110519     c                   clear                   campo16
074100111128     c                   eval       campo16 = %editc(DAA004:'Z')
074200111128     c                   IF           %subst(CAMPO16:14:1) = ' '
074300111128     c                   eval         %subst(CAMPO16:14:1) = '0'
074400111128     c                   end
074500111128     c                   IF           %subst(CAMPO16:13:1) = ' '
074600111128     c                   eval         %subst(CAMPO16:13:1) = '0'
074700111128     c                   end
074800110519     c                   eval         segmento = MOA_sgm
074900110519     c                              + %Trim(DAA025)
075000110519     c                              + ':'
075100110519     c                              + %Trim(%subst(CAMPO16:1:13))
075200110519     c                              + '.'
075300110519     c                              + %subst(CAMPO16:14:2)
075400110519     c                              + ':'
075500110519     c                              + %Trim(DAA345)
075600110519      *
075700110519     C                   EXSR      TIVGD
075800110519     C                   End
075900110519      *
076000110519     c                   if        DAB025 <> *blank
076100110519     c                   clear                   campo16
076200111128     c                   eval       campo16 = %editc(DAB004:'Z')
076300111128     c                   IF           %subst(CAMPO16:14:1) = ' '
076400111128     c                   eval         %subst(CAMPO16:14:1) = '0'
076500111128     c                   end
076600111128     c                   IF           %subst(CAMPO16:13:1) = ' '
076700111128     c                   eval         %subst(CAMPO16:13:1) = '0'
076800111128     c                   end
076900110519     c                   eval         segmento = MOA_sgm
077000110519     c                              + %Trim(DAB025)
077100110519     c                              + ':'
077200110519     c                              + %Trim(%subst(CAMPO16:1:13))
077300110519     c                              + '.'
077400110519     c                              + %subst(CAMPO16:14:2)
077500110519     c                              + ':'
077600110519     c                              + %Trim(DAB345)
077700110519      *
077800110519     C                   EXSR      TIVGD
077900110519     C                   End
078000110519      *
078100110519     C                   Endsr
078200110519      **?------------------------------------------------------------------ */
078300110519      **    Scrittura  del segmento TOD                Edi Standard
078400110519      **?------------------------------------------------------------------ */
078500110519     C     Segm_TOD      Begsr
078600110519      *
078700110519     c                   eval         segmento = TOD_sgm
078800110519     c                              + %Trim(DC4055)
078900110519     c                              + '+'
079000110519     c                              + %Trim(DC4215)
079100110519     c                              + '+'
079200110519     c                              + %Trim(DC4053)
079300110519     c                              + ':'
079400110519     c                              + %Trim(DC1131)
079500110519      *
079600110519     C                   EXSR      TIVGD
079700110519      *
079800110519     C                   Endsr
079900110519      **?------------------------------------------------------------------ */
080000110519      **    Scrittura  del segmento LOC                Edi Standard
080100110519      **?------------------------------------------------------------------ */
080200110519     C     Segm_LOC      Begsr
080300110519      *
080400110519     c                   if        Tipo_DS = 'TT20'
080500110519      *
080600110519      *   solo se presenti dei dati
080700110519     c                   if        TE3227 <> ' '
080800110524      *
080900110524     c                   eval      campo_alfa = TE3225
081000111018     c                   eval      lunghezza_campo=%len(TE3225)
081100110524     c                   exsr      Char_Speciali
081200110524     c                   eval      TE3225 = campo_alfa
081300110524      *
081400110519     c                   eval         segmento = LOC_sgm
081500110519     c                              + %Trim(TE3227)
081600110519     c                              + '+'
081700110519     c                              + %Trim(TE3225)
081800110519      *
081900110519     C                   EXSR      TIVGD
082000110519     C                   End
082100110519      *
082200110519     C                   End
082300110519      *
082400110519     C                   Endsr
082500110519      **?------------------------------------------------------------------ */
082600110518      **    Scrittura  del segmento NAD                Edi Standard
082700110518      **?------------------------------------------------------------------ */
082800110518     C     Segm_NAD      Begsr
082900110518      *
083000110316     c                   eval         segmento = NAD_sgm
083100110316      *
083200110405      *  in testata
083300110519     c                   if        Tipo_DS = 'TT25'
083400110524      *
083500110524     c                   eval      campo_alfa = TF3039
083600111018     c                   eval      lunghezza_campo=%len(TF3039)
083700110524     c                   exsr      Char_Speciali
083800110524     c                   eval      TF3039 = campo_alfa
083900110524      *
084000110316     c                   eval         segmento = %Trim(segmento)
084100110519     c                              + %Trim(TF3035)
084200110519     c                              + '+'
084300110519     c                              + %Trim(TF3039)
084400110519      *
084500110519     C                   EXSR      TIVGD
084600110316      *
084700110405      *  in dettaglio
084800110519     c                   elseif    Tipo_DS = 'TD15'
084900110524      *
085000110524     c                   eval      campo_alfa = DD3039
085100111018     c                   eval      lunghezza_campo=%len(DD3039)
085200110524     c                   exsr      Char_Speciali
085300110524     c                   eval      DD3039 = campo_alfa
085400110524      *
085500110524     c                   eval      campo_alfa = DD3124
085600111018     c                   eval      lunghezza_campo=%len(DD3124)
085700110524     c                   exsr      Char_Speciali
085800110524     c                   eval      DD3124 = campo_alfa
085900110524      *
086000110524     c                   eval      campo_alfa = DD3036
086100111018     c                   eval      lunghezza_campo=%len(DD3036)
086200110524     c                   exsr      Char_Speciali
086300110524     c                   eval      DD3036 = campo_alfa
086400110524      *
086500110316     c                   eval         segmento = %Trim(segmento)
086600110519     c                              + %Trim(DD3035)
086700110519     c                              + '+'
086800110519     c                              + %Trim(DD3039)
086900110519     c                              + '+'
087000110519     c                              + %Trim(DD3124)
087100110519     c                              + '+'
087200110519     c                              + %Trim(DD3036)
087300110316      *
087400110519     c                   if             DDA036 <> *blank
087500110524      *
087600110524     c                   eval      campo_alfa = DDA036
087700111018     c                   eval      lunghezza_campo=%len(DDA036)
087800110524     c                   exsr      Char_Speciali
087900110524     c                   eval      DDA036 = campo_alfa
088000110524      *
088100110322     c                   eval         segmento = %Trim(segmento)
088200110519     c                              + ':'
088300110519     c                              + %Trim(DDA036)
088400110519     c                   end
088500110519      *
088600110524     c                   eval      campo_alfa = DD3042
088700111018     c                   eval      lunghezza_campo=%len(DD3042)
088800110524     c                   exsr      Char_Speciali
088900110524     c                   eval      DD3042 = campo_alfa
089000110524      *
089100110519     c                   eval         segmento = %Trim(segmento)
089200110519     c                              + '+'
089300110519     c                              + %Trim(DD3042)
089400110519      *
089500110519     c                   if             DDA042 <> *blank
089600110524      *
089700110524     c                   eval      campo_alfa = DDA042
089800111018     c                   eval      lunghezza_campo=%len(DDA042)
089900110524     c                   exsr      Char_Speciali
090000110524     c                   eval      DDA042 = campo_alfa
090100110524      *
090200110519     c                   eval         segmento = %Trim(segmento)
090300110519     c                              + ':'
090400110519     c                              + %Trim(DDA042)
090500110519     c                   end
090600110524      *
090700110524     c                   eval      campo_alfa = DD3164
090800111018     c                   eval      lunghezza_campo=%len(DD3164)
090900110524     c                   exsr      Char_Speciali
091000110524     c                   eval      DD3164 = campo_alfa
091100110524      *
091200110524     c                   eval      campo_alfa = DD3229
091300111018     c                   eval      lunghezza_campo=%len(DD3229)
091400110524     c                   exsr      Char_Speciali
091500110524     c                   eval      DD3229 = campo_alfa
091600110524      *
091700110524     c                   eval      campo_alfa = DD3251
091800111018     c                   eval      lunghezza_campo=%len(DD3251)
091900110524     c                   exsr      Char_Speciali
092000110524     c                   eval      DD3251 = campo_alfa
092100110524      *
092200110519     c                   eval         segmento = %Trim(segmento)
092300110519     c                              + '+'
092400110519     c                              + %Trim(DD3164)
092500110519     c                              + '+'
092600110519     c                              + %Trim(DD3229)
092700110519     c                              + '+'
092800110519     c                              + %Trim(DD3251)
092900110519     c                              + '+'
093000110519     c                              + %Trim(DD3207)
093100110519
093200110405     C                   EXSR      TIVGD
093300110322     c                   end
093400110316      *
093500110316     C                   Endsr
093600110405      **?------------------------------------------------------------------ */
093700110519      **    Scrittura  del segmento CTA                Edi Standard
093800110519      **?------------------------------------------------------------------ */
093900110519     C     Segm_CTA      Begsr
094000110519      *
094100110519     c                   eval         segmento = CTA_sgm
094200110519      *  in testata
094300110519     c                   if        Tipo_DS = 'TT25'
094400110519      *
094500110524     c                   eval      campo_alfa = TF3412
094600111018     c                   eval      lunghezza_campo=%len(TF3412)
094700110524     c                   exsr      Char_Speciali
094800110524     c                   eval      TF3412 = campo_alfa
094900110524      *
095000110519     c                   if        TF3139 <> ' '
095100110519     c                   eval         segmento = %Trim(segmento)
095200110519     c                              + %Trim(TF3139)
095300110519     c                              + '+:'
095400110519     c                              + %Trim(TF3412)
095500110519      *
095600110519     C                   EXSR      TIVGD
095700110519     c                   end
095800110519      *
095900110519      *  in dettaglio
096000110519     c                   elseif    Tipo_DS = 'TD15'
096100110524      *
096200110524     c                   eval      campo_alfa = DD3412
096300111018     c                   eval      lunghezza_campo=%len(DD3412)
096400110524     c                   exsr      Char_Speciali
096500110524     c                   eval      DD3412 = campo_alfa
096600110524      *
096700110519     c                   eval         segmento = %Trim(segmento)
096800110519     c                              + %Trim(DD3139)
096900110519     c                              + '+:'
097000110519     c                              + %Trim(DD3412)
097100110519
097200110519     C                   EXSR      TIVGD
097300110519     c                   end
097400110519      *
097500110519     C                   Endsr
097600110519      **?------------------------------------------------------------------ */
097700110519      **    Scrittura  del segmento COM                Edi Standard
097800110519      **?------------------------------------------------------------------ */
097900110519     C     Segm_COM      Begsr
098000110519      *
098100110519     c                   eval         segmento = COM_sgm
098200110519      *  in testata
098300110519     c                   if        Tipo_DS = 'TT25'
098400110519      *
098500110519     c                   if        TF3148 <> ' '
098600110524      *
098700110524     c                   eval      campo_alfa = TF3148
098800111018     c                   eval      lunghezza_campo=%len(TF3148)
098900110524     c                   exsr      Char_Speciali
099000110524     c                   eval      TF3148 = campo_alfa
099100110524      *
099200110519     c                   eval         segmento = %Trim(segmento)
099300110519     c                              + %Trim(TF3148)
099400110519     c                              + ':'
099500110519     c                              + %Trim(TF3155)
099600110519      *
099700110519     C                   EXSR      TIVGD
099800110519     c                   end
099900110519      *
100000110519     c                   if        TFA148 <> ' '
100100110524      *
100200110524     c                   eval      campo_alfa = TFA148
100300111018     c                   eval      lunghezza_campo=%len(TFA148)
100400110524     c                   exsr      Char_Speciali
100500110524     c                   eval      TFA148 = campo_alfa
100600110524      *
100700110519     c                   eval         segmento = %Trim(segmento)
100800110519     c                              + %Trim(TFA148)
100900110519     c                              + ':'
101000110519     c                              + %Trim(TFA155)
101100110519      *
101200110519     C                   EXSR      TIVGD
101300110519     c                   end
101400110519      *
101500110519      *  in dettaglio
101600110519     c                   elseif    Tipo_DS = 'TD15'
101700110524      *
101800110524     c                   eval      campo_alfa = DD3148
101900111018     c                   eval      lunghezza_campo=%len(DD3148)
102000110524     c                   exsr      Char_Speciali
102100110524     c                   eval      DD3148 = campo_alfa
102200110524      *
102300110519     c                   eval         segmento = %Trim(segmento)
102400110519     c                              + %Trim(DD3148)
102500110519      *
102600110519     c                   if        DD3155 <> ' '
102700110519     c                   eval         segmento = %Trim(segmento)
102800110519     c                              + ':'
102900110519     c                              + %Trim(DD3155)
103000110519     c                   end
103100110519
103200110519     C                   EXSR      TIVGD
103300110519      *---
103400110519     c                   if        DDA148 <> ' '
103500110524      *
103600110524     c                   eval      campo_alfa = DDA148
103700111018     c                   eval      lunghezza_campo=%len(DDA148)
103800110524     c                   exsr      Char_Speciali
103900110524     c                   eval      DDA148 = campo_alfa
104000110524      *
104100110519     c                   eval         segmento = COM_sgm
104200110519     c                              + %Trim(DDA148)
104300110519      *
104400110519     c                   if        DDA155 <> ' '
104500110519     c                   eval         segmento = %Trim(segmento)
104600110519     c                              + ':'
104700110519     c                              + %Trim(DDA155)
104800110519     c                   end
104900110519
105000110519     C                   EXSR      TIVGD
105100110519     c                   end
105200110519
105300110519     c                   end
105400110519      *
105500110519     C                   Endsr
105600110519      **?------------------------------------------------------------------ */
105700110519      **    Scrittura  del segmento EQD                Edi Standard
105800110405      **?------------------------------------------------------------------ */
105900110519     C     Segm_EQD      Begsr
106000110405      *
106100110519      *
106200110519     C                   Endsr
106300110519      **?------------------------------------------------------------------ */
106400110519      **    Scrittura  del segmento EQN                Edi Standard
106500110519      **?------------------------------------------------------------------ */
106600110519     C     Segm_EQN      Begsr
106700110519      *
106800110519      *
106900110519     C                   Endsr
107000110519      **?------------------------------------------------------------------ */
107100110519      **    Scrittura  del segmento SEL                Edi Standard
107200110519      **?------------------------------------------------------------------ */
107300110519     C     Segm_SEL      Begsr
107400110519      *
107500110519      *
107600110519     C                   Endsr
107700110519      **?------------------------------------------------------------------ */
107800110519      **    Scrittura  del segmento CNI                Edi Standard
107900110519      **?------------------------------------------------------------------ */
108000110519     C     Segm_CNI      Begsr
108100110519      *
108200110316     c                   eval         segmento = CNI_sgm
108300110316     c                              + %Trim(%EDITC(DA1490:'Z'))
108400110316     c                              + '+'
108500110316     c                              + %Trim(DA1004)
108600110519     c                              + '+'
108700110519     c                              + %Trim(%EDITC(DA1312:'Z'))
108800110316
108900110316     C                   EXSR      TIVGD
109000110316      *
109100110316     C                   Endsr
109200110316      **?------------------------------------------------------------------ */
109300110519      **    Scrittura  del segmento MEA                Edi Standard
109400110316      **?------------------------------------------------------------------ */
109500110519     C     Segm_MEA      Begsr
109600110520      *
109700110519     c                   clear                   campo16          16
109800110519     c                   eval       campo16 = %editc(DF6314:'Z')
109900110520     c                   if        %subst(CAMPO16:1:13) = *blank
110000110520     c                   eval      %subst(CAMPO16:13:1) = '0'
110100110520     c                   enD
110200110520     c                   if        %subst(CAMPO16:14:1) = *blank
110300110520     c                   eval      %subst(CAMPO16:14:1) = '0'
110400110520     c                   enD
110500110520     c                   if        %subst(CAMPO16:15:1) = *blank
110600110520     c                   eval      %subst(CAMPO16:15:1) = '0'
110700110520     c                   enD
110800110316      *
110900110519     c                   eval         segmento = MEA_sgm
111000110519     c                              + %Trim(DF6311)
111100110519     c                              + '+'
111200110519     c                              + %Trim(DF6313)
111300110519     c                              + '+'
111400110519     c                              + %Trim(DF6411)
111500110519     c                              + ':'
111600110519     c                              + %Trim(%subst(CAMPO16:1:13))
111700110519      **
111800110519     c                   iF           DF6311 ='VOL'
111900110519     c                   eval         segmento = %Trim(segmento)
112000110519     c                              + '.'
112100110519     c                              + %subst(CAMPO16:14:2)
112200110519     c                   enD
112300110316      **
112400110316     C                   EXSR      TIVGD
112500110316      *
112600110316     C                   Endsr
112700110316      **?------------------------------------------------------------------ */
112800110316      **    Scrittura  del segmento GID                Edi Standard
112900110316      **?------------------------------------------------------------------ */
113000110316     C     Segm_GID      Begsr
113100110316      *
113200110316     c                   eval         segmento = GID_sgm
113300110316     c                              + %Trim(%EDITC(DE1496:'Z'))
113400110316     c                              + '+'
113500110316     c                              + %Trim(%EDITC(DE7224:'Z'))
113600110519     c                              + ':'
113700110519     c                              + %Trim(DE7065)
113800110316
113900110316     C                   EXSR      TIVGD
114000110316      *
114100110316     C                   Endsr
114200110316      **?------------------------------------------------------------------ */
114300110519      **    Scrittura  del segmento PIA                Edi Standard
114400110316      **?------------------------------------------------------------------ */
114500110519     C     Segm_PIA      Begsr
114600110411      *
114700110519      *
114800110519     C                   Endsr
114900110519      **?------------------------------------------------------------------ */
115000110519      **    Scrittura  del segmento PCI                Edi Standard
115100110519      **?------------------------------------------------------------------ */
115200110519     C     Segm_PCI      Begsr
115300110519      *
115400110411      *  Se c'è almeno un Barcode
115500110519     C                   IF        DG7102 <> *BLANK
115600110524      *
115700110524     c                   eval      campo_alfa = DG7102
115800111018     c                   eval      lunghezza_campo=%len(DG7102)
115900110524     c                   exsr      Char_Speciali
116000110524     c                   eval      DG7102 = campo_alfa
116100110524      *
116200120330      *  In presenza del Qualificatore
116300120330     c                   if             DG4233 <> *blank
116400120330      *
116500110316     c                   eval         segmento = PCI_sgm
116600120330     c                              + %Trim(DG4233)
116700120330     c                              + '+'
116800120330     c                              + %Trim(DG7102)
116900120330     c                   else
117000120330      *
117100120330     c                   eval         segmento = PCI_sgm
117200120330     c                              + '+'
117300120330     c                              + %Trim(DG7102)
117400120330     c                   end
117500120330      *  2°barcode
117600110519     c                   if             DGA102 <> *blank
117700110524      *
117800110524     c                   eval      campo_alfa = DGA102
117900111018     c                   eval      lunghezza_campo=%len(DGA102)
118000110524     c                   exsr      Char_Speciali
118100110524     c                   eval      DGA102 = campo_alfa
118200110524      *
118300110316     c                   eval         segmento = %Trim(segmento)
118400110316     c                              + ':'
118500110519     c                              + %Trim(DGA102)
118600110316     c                   end
118700110316      *
118800110519     c                   if             DGB102 <> *blank
118900110524      *
119000110524     c                   eval      campo_alfa = DGB102
119100111018     c                   eval      lunghezza_campo=%len(DGB102)
119200110524     c                   exsr      Char_Speciali
119300110524     c                   eval      DGB102 = campo_alfa
119400110524      *
119500110316     c                   eval         segmento = %Trim(segmento)
119600110316     c                              + ':'
119700110519     c                              + %Trim(DGB102)
119800110316     c                   end
119900110316      *
120000110519     c                   if             DGC102 <> *blank
120100110524      *
120200110524     c                   eval      campo_alfa = DGC102
120300111018     c                   eval      lunghezza_campo=%len(DGC102)
120400110524     c                   exsr      Char_Speciali
120500110524     c                   eval      DGC102 = campo_alfa
120600110524      *
120700110316     c                   eval         segmento = %Trim(segmento)
120800110316     c                              + ':'
120900110519     c                              + %Trim(DGC102)
121000110316     c                   end
121100110316      *
121200110519     c                   if             DGD102 <> *blank
121300110524      *
121400110524     c                   eval      campo_alfa = DGD102
121500111018     c                   eval      lunghezza_campo=%len(DGD102)
121600110524     c                   exsr      Char_Speciali
121700110524     c                   eval      DGD102 = campo_alfa
121800110524      *
121900110316     c                   eval         segmento = %Trim(segmento)
122000110316     c                              + ':'
122100110519     c                              + %Trim(DGD102)
122200110316     c                   end
122300110316      *
122400110519     c                   if             DGE102 <> *blank
122500110524      *
122600110524     c                   eval      campo_alfa = DGE102
122700111018     c                   eval      lunghezza_campo=%len(DGE102)
122800110524     c                   exsr      Char_Speciali
122900110524     c                   eval      DGE102 = campo_alfa
123000110524      *
123100110316     c                   eval         segmento = %Trim(segmento)
123200110316     c                              + ':'
123300110519     c                              + %Trim(DGE102)
123400110316     c                   end
123500110316      *
123600110519     c                   if             DGF102 <> *blank
123700110524      *
123800110524     c                   eval      campo_alfa = DGF102
123900111018     c                   eval      lunghezza_campo=%len(DGF102)
124000110524     c                   exsr      Char_Speciali
124100110524     c                   eval      DGF102 = campo_alfa
124200110524      *
124300110316     c                   eval         segmento = %Trim(segmento)
124400110316     c                              + ':'
124500110519     c                              + %Trim(DGF102)
124600110316     c                   end
124700110316      *
124800110519     c                   if             DGG102 <> *blank
124900110524      *
125000110524     c                   eval      campo_alfa = DGG102
125100111018     c                   eval      lunghezza_campo=%len(DGG102)
125200110524     c                   exsr      Char_Speciali
125300110524     c                   eval      DGG102 = campo_alfa
125400110524      *
125500110316     c                   eval         segmento = %Trim(segmento)
125600110316     c                              + ':'
125700110519     c                              + %Trim(DGG102)
125800110316     c                   end
125900110316      *
126000110519     c                   if             DGH102 <> *blank
126100110524      *
126200110524     c                   eval      campo_alfa = DGH102
126300111018     c                   eval      lunghezza_campo=%len(DGH102)
126400110524     c                   exsr      Char_Speciali
126500110524     c                   eval      DGH102 = campo_alfa
126600110524      *
126700110316     c                   eval         segmento = %Trim(segmento)
126800110316     c                              + ':'
126900110519     c                              + %Trim(DGH102)
127000110316     c                   end
127100110316      *
127200110519     c                   if             DGI102 <> *blank
127300110524      *
127400110524     c                   eval      campo_alfa = DGI102
127500111018     c                   eval      lunghezza_campo=%len(DGI102)
127600110524     c                   exsr      Char_Speciali
127700110524     c                   eval      DGI102 = campo_alfa
127800110524      *
127900110316     c                   eval         segmento = %Trim(segmento)
128000110316     c                              + ':'
128100110519     c                              + %Trim(DGI102)
128200110316     c                   end
128300110316
128400110316     C                   EXSR      TIVGD
128500110316      *
128600110316      *  secondi 10 elementi
128700110519     c                   if             DGL102 <> *blank
128800110524      *
128900110524     c                   eval      campo_alfa = DGL102
129000111018     c                   eval      lunghezza_campo=%len(DGL102)
129100110524     c                   exsr      Char_Speciali
129200110524     c                   eval      DGL102 = campo_alfa
129300110524      *
129400120330      *  In presenza del Qualificatore
129500120330     c                   if             DG4233 <> *blank
129600120330      *
129700110519     c                   eval         segmento = PCI_sgm
129800120330     c                              + %Trim(DG4233)
129900110316     c                              + '+'
130000110519     c                              + %Trim(DGL102)
130100120330     c                   else
130200120330     c                   eval         segmento = PCI_sgm
130300120330     c                              + '+'
130400120330     c                              + %Trim(DGL102)
130500120330     c                   end
130600120330      *  2°barcode
130700110519     c                   if             DGM102 <> *blank
130800110524      *
130900110524     c                   eval      campo_alfa = DGM102
131000111018     c                   eval      lunghezza_campo=%len(DGM102)
131100110524     c                   exsr      Char_Speciali
131200110524     c                   eval      DGM102 = campo_alfa
131300110524      *
131400110316     c                   eval         segmento = %Trim(segmento)
131500110316     c                              + ':'
131600110519     c                              + %Trim(DGM102)
131700110316     c                   end
131800110316      *
131900110519     c                   if             DGN102 <> *blank
132000110524      *
132100110524     c                   eval      campo_alfa = DGN102
132200111018     c                   eval      lunghezza_campo=%len(DGN102)
132300110524     c                   exsr      Char_Speciali
132400110524     c                   eval      DGN102 = campo_alfa
132500110524      *
132600110316     c                   eval         segmento = %Trim(segmento)
132700110316     c                              + ':'
132800110519     c                              + %Trim(DGN102)
132900110316     c                   end
133000110316      *
133100110519     c                   if             DGO102 <> *blank
133200110524      *
133300110524     c                   eval      campo_alfa = DGO102
133400111018     c                   eval      lunghezza_campo=%len(DGO102)
133500110524     c                   exsr      Char_Speciali
133600110524     c                   eval      DGO102 = campo_alfa
133700110524      *
133800110316     c                   eval         segmento = %Trim(segmento)
133900110316     c                              + ':'
134000110519     c                              + %Trim(DGO102)
134100110316     c                   end
134200110316      *
134300110519     c                   if             DGP102 <> *blank
134400110524      *
134500110524     c                   eval      campo_alfa = DGP102
134600111018     c                   eval      lunghezza_campo=%len(DGP102)
134700110524     c                   exsr      Char_Speciali
134800110524     c                   eval      DGP102 = campo_alfa
134900110524      *
135000110316     c                   eval         segmento = %Trim(segmento)
135100110316     c                              + ':'
135200110519     c                              + %Trim(DGP102)
135300110316     c                   end
135400110316      *
135500110519     c                   if             DGQ102 <> *blank
135600110524      *
135700110524     c                   eval      campo_alfa = DGQ102
135800111018     c                   eval      lunghezza_campo=%len(DGQ102)
135900110524     c                   exsr      Char_Speciali
136000110524     c                   eval      DGQ102 = campo_alfa
136100110524      *
136200110316     c                   eval         segmento = %Trim(segmento)
136300110316     c                              + ':'
136400110519     c                              + %Trim(DGQ102)
136500110316     c                   end
136600110316      *
136700110519     c                   if             DGR102 <> *blank
136800110524      *
136900110524     c                   eval      campo_alfa = DGR102
137000111018     c                   eval      lunghezza_campo=%len(DGR102)
137100110524     c                   exsr      Char_Speciali
137200110524     c                   eval      DGR102 = campo_alfa
137300110524      *
137400110316     c                   eval         segmento = %Trim(segmento)
137500110316     c                              + ':'
137600110519     c                              + %Trim(DGR102)
137700110316     c                   end
137800110316      *
137900110519     c                   if             DGS102 <> *blank
138000110524      *
138100110524     c                   eval      campo_alfa = DGS102
138200111018     c                   eval      lunghezza_campo=%len(DGS102)
138300110524     c                   exsr      Char_Speciali
138400110524     c                   eval      DGS102 = campo_alfa
138500110524      *
138600110316     c                   eval         segmento = %Trim(segmento)
138700110316     c                              + ':'
138800110519     c                              + %Trim(DGS102)
138900110316     c                   end
139000110316      *
139100110519     c                   if             DGT102 <> *blank
139200110524      *
139300110524     c                   eval      campo_alfa = DGT102
139400111018     c                   eval      lunghezza_campo=%len(DGT102)
139500110524     c                   exsr      Char_Speciali
139600110524     c                   eval      DGT102 = campo_alfa
139700110524      *
139800110316     c                   eval         segmento = %Trim(segmento)
139900110316     c                              + ':'
140000110519     c                              + %Trim(DGT102)
140100110316     c                   end
140200110316      *
140300110519     c                   if             DGU102 <> *blank
140400110524      *
140500110524     c                   eval      campo_alfa = DGU102
140600111018     c                   eval      lunghezza_campo=%len(DGU102)
140700110524     c                   exsr      Char_Speciali
140800110524     c                   eval      DGU102 = campo_alfa
140900110524      *
141000110316     c                   eval         segmento = %Trim(segmento)
141100110316     c                              + ':'
141200110519     c                              + %Trim(DGU102)
141300110316     c                   end
141400110316      *
141500110316     C                   EXSR      TIVGD
141600110316     c                   end
141700110316      *
141800110411     c                   end
141900110411      *
142000110316     C                   Endsr
142100110316      **?------------------------------------------------------------------ */
142200110316      **    Scrittura  del segmento UNT                Edi Standard
142300110316      **?------------------------------------------------------------------ */
142400110316     C     Segm_UNT      Begsr
142500110316      *
142600110316     c                   eval         segmento = UNT_sgm
142700110524     c                              + %Trim(%EDITC(Conta_righe - 1:'Z'))
142800110316     c                              + '+1'
142900110316
143000110316     C                   EXSR      TIVGD
143100110316      *
143200110316     C                   Endsr
143300110316      **?------------------------------------------------------------------ */
143400110316      **    Scrittura  del segmento UNZ                Edi Standard
143500110316      **?------------------------------------------------------------------ */
143600110316     C     Segm_UNZ      Begsr
143700110316      *
143800110316     c                   eval         segmento = UNZ_sgm
143900110412     c                              + %Trim(TAPRG)
144000110316
144100110316     C                   EXSR      TIVGD
144200110525      *
144300110525      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
144400110525     c                   if        compatta_80  = 'S'
144500110525     c                   exsr      Scarica_su_VGD
144600110525     c                   end
144700110405      * quindi chiude
144800110405      *   il TIVGD
144900110405     C                   EXSR      vgd_CLOSE
145000110316      *
145100110316     C                   Endsr
145200110316      **?------------------------------------------------------------------ */
145300110405      **    FINTO ROLLBACK
145400110316      **?------------------------------------------------------------------ */
145500110405     C     ROLL_BCK      Begsr
145600110316      *
145700110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
145800110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
145900110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
146000110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
146100110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
146200110405      *
146300110405      *
146400110405     C                   Endsr
146500110520      **?------------------------------------------------------------------ */
146600110520      **    Caratteri particolari
146700110520      **?------------------------------------------------------------------ */
146800110520     C     Char_Speciali Begsr
146900110520      *
147000110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
147100110520      *   dichiarati nei segmenti    (+:'?)
147200110520      *   [più, due punti, apostrofo, punto interrogativo]
147300110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
147400110520      *   considerati caratteri Funzionali che, qualora debbano essere
147500110520      *   utilizzati come semplici caratteri di testo, dovranno essere
147600110520      *   preceduti da un (?).
147700110520      *
147800110520     c                   movel(p)  campo_alfa    campo_test
147900110520     c                   clear                   campo_ritorno
148000110520      *
148100110520     C                   call      'TRTCT00R3'
148200110520     c                   parm                    campo_test      512
148300110520     c                   parm                    campo_ritorno   512
148400110621     c                   parm                    esito
148500110520      *
148600110520     c                   movel(p)  campo_ritorno campo_alfa      512
148700111018      ******
148800111018      *  occorre fare attenzione se l'ultimo carattere è un (?) che precede
148900111018      *   uno dei carratteri speciali, questo può procurare problemi a chi riceve
149000111018      *    quindi si deve sostituire il (?) con un blank e togliere tutto ciò che
149100111018      *   sta sulla destra da lì in poi.
149200111018     c                   if        %subst(campo_alfa:lunghezza_campo-1:1) <>
149300111018     c                                     Punto_interrogativo   and
149400111018     c                             %subst(campo_alfa:lunghezza_campo:1)
149500111018     c                                 =   Punto_interrogativo
149600111018      *   Sostituisce il (?) nell'ultimo byte del campo se non aveva nel byte
149700111018      *    precedente un (?) e gli mette blank al posto del (?)
149800111018     c                   eval      %subst(campo_alfa:lunghezza_campo:1)
149900111018     c                                 =   un_blank
150000111018      * si è fatto ciò poichè altrimenti quando si va a comporre il segmento EDI
150100111018      *  il carattere funzionale (+ o : o ') potrebbero venire annullati dalla
150200111018      *  presenza di un (?) davanti creando problemi di traduzione.
150300111018     c                   end
150400110520      *
150500110520     C                   Endsr
150600110405      **?------------------------------------------------------------------ */
150700110405      **    Scrittura  del segmento                    Edi Standard
150800110405      **?------------------------------------------------------------------ */
150900110405     C     tivgd         Begsr
151000110519      *
151100110519      *   chiude il segmento con l'apostrofo
151200110520     c                   if         Conta_righe > 0
151300110519     c                   eval         segmento = %Trim(segmento)
151400110519     c                              + ''''
151500110520     c                   end
151600110405      *
151700110316     c                   eval       Conta_righe = 1 + Conta_righe
151800110525      *
151900110525      *  se deve scrivere il file di scarico segmento x ogni linea
152000110525     c                   if        compatta_80  <>'S'
152100110405     c                   exsr      VGD_WRITE
152200110525     c                   else
152300110525      *
152400110525      *  se invece deve scrivere a 80 colonne
152500110525     c                   exsr      WRITE_80COL
152600110525      *
152700110525     c                   end
152800110316      *
152900110316     c                   endsr
153000110405      **?__________________________________________________________________ */
153100110405      *    Apertura del TIVGD
153200110316      **?__________________________________________________________________ */
153300110405     C     VGD_OPEN      Begsr
153400110405      *
153500110617      *  recupera il progressivo
153600110617     c                   exsr      Piglia_progr
153700110617      *
153800110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
153900110405     C                   clear                   trul47ds
154000110405     C                   eval      d47opz  = 'I'
154100110617     C                   eval      d47tip  = TipoF
154200110405     C                   eval      d47lck  = 'N'
154300110405     C                   eval      d47chkj = 'N'
154400110519     C                   eval      d47pgm  = 'TRTCT91R1'
154500110405     C                   call      'TRUL47R'
154600110405     C                   parm                    trul47ds
154700110405      *
154800110405     c                   endsr
154900110617      **?__________________________________________________________________ */
155000110617      *   Scrittura del TIVGD
155100110617      **?__________________________________________________________________ */
155200110617     C     VGD_WRITE     Begsr
155300110617      *
155400110405     c                   clear                   tivgd000
155500110405     c                   eval      vgddta = %TrimR(Segmento)
155600110617     c                   eval      vgdtip = TipoF
155700110405     c                   eval      vgdksu = CLIENTE
155800110617     C                   eval      vgdprg = Progressivo
155900110405     c                   eval      vgdtsc = 'WW'
156000110519     c                   eval      vgdpgm = 'TRTCT91R1'
156100110405     c                   eval      vgddat = udtymd
156200110405      *
156300110405     C                   WRITE     tivgd000
156400110405      *
156500110405     c                   endsr
156600110405      **?__________________________________________________________________ */
156700110405      *    Chiusura del TIVGD
156800110405      **?__________________________________________________________________ */
156900110405     C     VGD_CLOSE     Begsr
157000110405      *
157100110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
157200110405     C                   clear                   trul47ds
157300110405     C                   eval      d47opz  = 'F'
157400110617     C                   eval      d47tip  = TipoF
157500110405     C                   call      'TRUL47R'
157600110405     C                   parm                    trul47ds
157700110405      *
157800110405     c                   endsr
157900110525      **?__________________________________________________________________ */
158000110525      *    Pulisce in QTEMP il file contenitore di 80 colonne
158100110405      **?__________________________________________________________________ */
158200110525     C     CLEAR_STD80F  Begsr
158300110525      *
158400110525      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
158500110525      *   a 80 colonne.
158600110525     c                   call      'TRTCT00R4C'
158700110525      *
158800110525     c                   endsr
158900110525      **?__________________________________________________________________ */
159000110525      *    Carica in  QTEMP il file contenitore di 80 colonne
159100110525      **?__________________________________________________________________ */
159200110525     C     WRITE_80COL   Begsr
159300110525      *
159400110525      * sposta la stringa del segmento appena composta
159500110525     c                   movel     segmento      segmento1950
159600110525      *
159700110525      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
159800110525     c                   call      'TRTCT00R4'
159900110525     c                   parm                    segmento1950
160000110525      *
160100110525     c                   endsr
160200110525      **?__________________________________________________________________ */
160300110525      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
160400110525      **?__________________________________________________________________ */
160500110525     C     Scarica_su_VGDBegsr
160600110525      *
160700110525     c                   open      EDISTD80F
160800110525     c                   read      EDISTD80F
160900110525      *
161000110525     c                   dow       not %EoF(EDISTD80F)
161100110525      *
161200110525     c                   eval      segmento = RIGA80STD
161300110525     c                   exsr      VGD_WRITE
161400110617      *
161500110525     c                   read      EDISTD80F
161600110525     c                   end
161700110525      *
161800110525     c                   endsr
161900110617      **?__________________________________________________________________ */
162000110617      *   Prende il Progressivo
162100110617      **?__________________________________________________________________ */
162200110617     C     Piglia_progr  Begsr
162300110617      *
162400110617     C                   MOVEL     CLIENTE       virKSC
162500110617     C                   MOVEL     TipoF         virTIP
162600110617     c     kvir02        chain     tivir02l
162700110617     c                   move      'OF'          Var_ISV
162800110617     c                   if        %Found(tivir02l)
162900110617     c                   move      VirFI1        Var_ISV
163000110617     C                   End
163100110617
163200110617      *       prende numeratore strategi
163300110617     C                   exsr      calprog
163400110617      *
163500110617     c                   endsr
163600110617     C*------------------------------------------------------*
163700110617     C*  prepara il file
163800110617     C*------------------------------------------------------*
163900110617     C     calprog       begsr
164000110617     C*
164100110617     C                   open      tis7prgf
164200110617     C                   read(e)   tis7prgf
164300110617     C*
164400110617     C                   if        not %error
164500110617     C                   eval      dwlprg = f_tis7prgf
164600110617     C*
164700110617     C                   move(p)   dwlprg        wrkprg
164800110617     C                   add       1             wrkprg
164900110617     C                   move(p)   wrkprg        dwlprg
165000110617     C                   movel     Var_ISV       dwlprg
165100110617     C*
165200110617     C                   eval       f_tis7prgf = dwlprg
165300110617     C                   eval      Progressivo = dwlprg
165400110617     C*
165500110617     C                   update    tis7prg0
165600110617     C                   endif
165700110617     C*
165800110617     C                   close     tis7prgf
165900110617     C*
166000110617     C                   endsr
166100110617      **?__________________________________________________________________ */
166200110621      * ?------------------------------------------------------------------ */
166300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
166400110621      * ?------------------------------------------------------------------ */
166500110621     C     *pssr         BEGSR
166600110621     C
166700110621     C                   eval      esito = 'E'
166800110621     C                   ENDSR     '*CANCL'
166900110621     C
167000110621      *  ------------------------------------------------------------------ */
