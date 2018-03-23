000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200110519     H* TRTCT81R1        ---------------------------------------------------*
000300110519      **?__________________________________________________________________ */
000400110519     H*                                                                     *
000500110519     H* Scrittura del IFCSUM da Manifest  x Partner  (EXPORT)               *
000600110519     H*  versione IFCSUM S93A UN/NIG001                                     *
000700050927      **?__________________________________________________________________ */
000800110519      *
000900080424     ftivgd00f  if a E             DISK    commit
001000110525     fEdiStd80F if   E           k DISK    usropn
001100110519      *
001200110617     Ftivir02L  IF   E           K DISK
001300110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001400110617     F                                     PREFIX(f_) usropn
001500110519      **?__________________________________________________________________ */
001600110617     D TipoF           C                   CONST('EU')
001700110617     D dwlprg          s             10    INZ(*all'0')
001800110617     D wrkprg          s              8  0 INZ(*zeros)
001900060814     d trul47ds      e ds
002000060814     D W0140           S             14  0 inz
002100110316     D Segmento        s           2048
002200110525     D Segmento1950    s           1950
002300110316     D Conta_righe     s              5i 0
002400110621     d  esito          s              1
002500111018     d lunghezza_campo...
002600111018     d                 s              3i 0
002700110617      *---------------------------------------------------------------------*
002800110617     D Var_ISV         s                   like(Virfi1)
002900110316     D*---------------------------------------------------------------------*
003000110316     D* DS
003100110316     D* .. per scompozione dati da spedire a seconda del tipo record
003200110316     I*---------------------------------------------------------------------*
003300110518     D* .. per scompozione dati ricevuti a seconda del tipo record
003400110519     D   TT00ds      E DS                  EXTNAME(EDTT00DS)
003500110519     D   TT05ds      E DS                  EXTNAME(EDTT05DS)
003600110519     D  T05sk                 13    362
003700110518     D  T05                   13    362
003800110518     D                                     DIM(5)
003900110519     D   TT10ds      E DS                  EXTNAME(EDTT10DS)
004000110518     D  T10                   10     53
004100110518     D                                     DIM(2)
004200110519     D   TT15ds      E DS                  EXTNAME(EDTT15DS)
004300110519     D   TT20ds      E DS                  EXTNAME(EDTT20DS)
004400110518     D  T20                  106    249
004500110518     D                                     DIM(3)
004600110518     D  T20D                 250    303
004700110518     D                                     DIM(3)
004800110519     D   TT25ds      E DS                  EXTNAME(EDTT25DS)
004900110518     D  T25                   68    123
005000110518     D                                     DIM(2)
005100110519     D   TT30ds      E DS                  EXTNAME(EDTT30DS)
005200110518     D*  Ds per reperimento dati peso/Volume intera spedizione
005300110518      * .. per scompozione dati ricevuti a seconda del tipo record
005400110519     D   TD00ds      E DS                  EXTNAME(EDTD00DS)
005500110518     D  D00                   77    142    DIM(3)
005600110519     D   TD05ds      E DS                  EXTNAME(EDTD05DS)
005700110519     D  D05sk                 13    362
005800110518     D  D05                   13    362    DIM(5)
005900121018     D  D05Ask               366    715
006000110518     D  D05A                 366    715    DIM(5)
006100110519     D   TD10ds      E DS                  EXTNAME(EDTD10DS)
006200110518     D  D10                   73    186    DIM(3)
006300110519     D   TD15ds      E DS                  EXTNAME(EDTD15DS)
006400110518     D  D15                  445    500    DIM(2)
006500110519     D   TD20ds      E DS                  EXTNAME(EDTD20DS)
006600110519     D   TD25ds      E DS                  EXTNAME(EDTD25DS)
006700110519     D   TD30ds      E DS                  EXTNAME(EDTD30DS)
006800110518     D  D30                   10    709    DIM(20)
006900110519     D   TD35ds      E DS                  EXTNAME(EDTD35DS)
007000110519     D   TD40ds      E DS                  EXTNAME(EDTD40DS)
007100110518      *-------
007200110520     D UNA_sgm         C                   CONST('UNA:+.? ''')
007300110518     D UNB_sgm         C                   CONST('UNB+UNOA:2+ITBAR005:ZZ+')
007400110518     D UNH_sgm         C                   CONST('UNH+1+IFCSUM:S:93A:UN:NIG001-
007500110519     D                                     ')
007600110519     D BGM_sgm         C                   CONST('BGM+')
007700110518     D CNI_sgm         C                   CONST('CNI+')
007800110518     D CNT_sgm         C                   CONST('CNT+')
007900110518     D COM_sgm         C                   CONST('COM+')
008000110519     D CTA_sgm         C                   CONST('CTA+')
008100110518     D DGS_sgm         C                   CONST('DGS+')
008200110518     D DTM_sgm         C                   CONST('DTM+')
008300110518     D EQD_sgm         C                   CONST('EQD+')
008400110518     D EQN_sgm         C                   CONST('EQN+')
008500110518     D FTX_sgm         C                   CONST('FTX+')
008600110519     D GID_sgm         C                   CONST('GID+')
008700110316     D LOC_sgm         C                   CONST('LOC+')
008800110518     D MEA_sgm         C                   CONST('MEA+')
008900110518     D MOA_sgm         C                   CONST('MOA+')
009000110518     D NAD_sgm         C                   CONST('NAD+')
009100110518     D PIA_sgm         C                   CONST('PIA+')
009200110519     D PCI_sgm         C                   CONST('PCI+')
009300110518     D RFF_sgm         C                   CONST('RFF+')
009400110518     D SEL_sgm         C                   CONST('SEL+')
009500110518     D TDT_sgm         C                   CONST('TDT+')
009600110518     D TOD_sgm         C                   CONST('TOD+')
009700110518     D TSR_sgm         C                   CONST('TSR+')
009800110518     D UNT_sgm         C                   CONST('UNT+')
009900110411     D UNZ_sgm         C                   CONST('UNZ+1+')
010000111018     D Punto_interrogativo...
010100111018     D                 C                   CONST('?')
010200111018     d un_blank        C                   CONST(' ')
010300050927      **?__________________________________________________________________ */
010400050704      *    main
010500110316      **?__________________________________________________________________ */
010600110316     c     *entry        plist                                                   esempio
010700110525     C                   PARM                    compatta_80       1            "S/ "
010800110316     C                   PARM                    CLIENTE           8            "03400002"
010900110316     C                   PARM                    Suffisso          2            "AZ"
011000110518     C                   PARM                    Tipo_DS           4            "TT00"
011100110518     C                   PARM                    DsGenerica     1950             EDTT00DS
011200110322     C                   PARM                    Numero_Righe      5 0
011300110617     C                   PARM                    Progressivo      10
011400110621     c                   PARM                    esito             1
011500110322     c*
011600110617     c     kvir02        klist
011700110617     C                   kfld                    virKSC
011800110617     C                   kfld                    virTIP
011900110617      *
012000110322     C                   z-add     Numero_Righe  Conta_righe
012100110316     c*
012200110316     C                   TIME                    ORADAT           14 0
012300110316     C                   MOVEL     ORADAT        ORATR             6 0
012400110316     c                   move      *date         udtymd            8 0
012500110316     C                   TIME                    W0140
012600110316     C                   MOVE      W0140         W0080             8 0
012700050927      **?__________________________________________________________________ */
012800110316      * Decodifica la DS in base ai Parametri
012900110518      **?__________________________________________________________________ */
013000110519     c                   clear                   segmento
013100110316      **
013200110316     c                   select
013300110316
013400110518     c                   when      Tipo_DS = 'TT00'
013500110518     c                   eval        TT00ds = DsGenerica
013600110316     c                   exsr      segm_UNA
013700110316     c                   exsr      segm_UNB
013800110316     c                   exsr      segm_UNH
013900110316     c                   exsr      segm_BGM
014000110316
014100110518     c                   when      Tipo_DS = 'TT05'
014200110518     c                   eval        TT05ds = DsGenerica
014300110519     c                   exsr      segm_FTX
014400110316
014500110518     c                   when      Tipo_DS = 'TT10'
014600110518     c                   eval        TT10ds = DsGenerica
014700110518     c                   exsr      segm_CNT
014800110316
014900110518     c                   when      Tipo_DS = 'TT15'
015000110518     c                   eval        TT15ds = DsGenerica
015100110518     c                   exsr      segm_RFF
015200110518     c                   exsr      segm_DTM
015300110316
015400110518     c                   when      Tipo_DS = 'TT20'
015500110518     c                   eval        TT20ds = DsGenerica
015600110518     c                   exsr      segm_TDT
015700110518     c                   exsr      segm_LOC
015800110518     c                   exsr      segm_DTM
015900110519
016000110519     c                   when      Tipo_DS = 'TT25'
016100110519     c                   eval        TT25ds = DsGenerica
016200110519     c                   exsr      segm_NAD
016300110519     c                   if          TF3412 <> ' ' or TF3148 <> ' '
016400110519     c                   exsr      segm_CTA
016500110519     c                   exsr      segm_COM
016600110519     c                   end
016700110519
016800110519     c                   when      Tipo_DS = 'TT30'
016900110519     c                   eval        TT30ds = DsGenerica
017000110519     c                   exsr      segm_EQD
017100110519     c                   exsr      segm_EQN
017200110519     c                   exsr      segm_SEL
017300110519      *
017400110519      *------------
017500110405      * da qui inizia il dettaglio
017600110518     c                   when      Tipo_DS = 'TD00'
017700110518     c                   eval        TD00ds = DsGenerica
017800110316     c                   exsr      segm_CNI
017900110519     c                   exsr      segm_DTM
018000110519     c                   exsr      segm_TSR
018100110519     c                   exsr      segm_MOA
018200110316
018300110518     c                   when      Tipo_DS = 'TD05'
018400110518     c                   eval        TD05ds = DsGenerica
018500110519     c                   exsr      segm_FTX
018600110316
018700110518     c                   when      Tipo_DS = 'TD10'
018800110518     c                   eval        TD10ds = DsGenerica
018900110519     c                   exsr      segm_TOD
019000110519     c                   exsr      segm_LOC
019100110519     c                   exsr      segm_RFF
019200110519     c                   exsr      segm_TDT
019300110316
019400110518     c                   when      Tipo_DS = 'TD15'
019500110518     c                   eval        TD15ds = DsGenerica
019600110519     c                   exsr      segm_NAD
019700110519     c                   if          DD3412 <> ' ' or DD3148 <> ' '
019800110519     c                   exsr      segm_CTA
019900110519     c                   exsr      segm_COM
020000110519     c                   end
020100110316
020200110518     c                   when      Tipo_DS = 'TD20'
020300110518     c                   eval        TD20ds = DsGenerica
020400110316     c                   exsr      segm_GID
020500110519     c                   exsr      segm_PIA
020600110519     c                   exsr      segm_FTX
020700110519
020800110519     c                   when      Tipo_DS = 'TD25'
020900110519     c                   eval        TD25ds = DsGenerica
021000110519     c                   exsr      segm_MEA
021100110519
021200110519     c                   when      Tipo_DS = 'TD30'
021300110519     c                   eval        TD30ds = DsGenerica
021400110519     c                   exsr      segm_PCI
021500110519
021600110519     c                   when      Tipo_DS = 'TD35'
021700110519     c                   eval        TD35ds = DsGenerica
021800110519
021900110519     c                   when      Tipo_DS = 'TD40'
022000110519     c                   eval        TD40ds = DsGenerica
022100110316
022200110316     c                   when      Tipo_DS = 'FINE'
022300110412     c                   eval        TAPRG = DsGenerica
022400110316     c                   exsr      segm_UNT
022500110316     c                   exsr      segm_UNZ
022600110405      * conferma la scrittura del TIVGD
022700110405     c                   commit
022800110405
022900110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
023000110405      *  si deve impostare in questo punto del programma.
023100110405     c                   when      Tipo_DS = 'RLBK'
023200110405     c                   exsr      ROLL_BCK
023300110316
023400110316     C                   ENDSL
023500050707      *
023600110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
023700110322     C                   z-add     Conta_righe   Numero_Righe
023800110322      *
023900050705     c                   seton                                        lr
024000110316      **?------------------------------------------------------------------ */
024100110316      **    Scrittura  del segmento UNA                Edi Standard
024200110316      **?------------------------------------------------------------------ */
024300110316     C     Segm_UNA      Begsr
024400110519      * apre
024500110519      *   il TIVGD
024600110519     C                   EXSR      vgd_OPEN
024700110525      *
024800110525      *  Se deve compattare pulisce il file di lavoro in QTEMP EDISTD80F
024900110525     c                   if        compatta_80  ='S'
025000110525     C                   EXSR      CLEAR_STD80F
025100110525     c                   end
025200110525      *
025300110318     c                   eval         segmento = UNA_sgm
025400110518     C                   EXSR      TIVGD
025500110316      *
025600110316     C                   Endsr
025700110316      **?------------------------------------------------------------------ */
025800110316      **    Scrittura  del segmento UNB                Edi Standard
025900110316      **?------------------------------------------------------------------ */
026000110316     C     Segm_UNB      Begsr
026100110316      *
026200110318     c                   eval         segmento = UNB_sgm
026300110316     c                              + %Trim(TA0010)
026400110519      *
026500110322      *  Aggiunge il qualificatore se c'è
026600110322     c                   if             TAA007 <> *blank
026700110322     c                   eval         segmento = %Trim(segmento)
026800110316     c                              + ':'
026900110316     c                              + %Trim(TAA007)
027000110322     c                   end
027100110519      *
027200110322     c                   eval         segmento = %Trim(segmento)
027300110316     c                              + '+'
027400110519     c                              + %Trim(%subst(%editc(TA0017:'X'):3:6))
027500110316     c                              + ':'
027600110519     c                              + %Editc(TA0019:'X')
027700110316     c                              + '+'
027800110518     c                              + %Trim(TAPRG)
027900110316
028000110518     C                   EXSR      TIVGD
028100110316      *
028200110316     C                   Endsr
028300110316      **?------------------------------------------------------------------ */
028400110316      **    Scrittura  del segmento UNH                Edi Standard
028500110316      **?------------------------------------------------------------------ */
028600110316     C     Segm_UNH      Begsr
028700110316      *
028800110318     c                   eval         segmento = UNH_sgm
028900110316
029000110518     C                   EXSR      TIVGD
029100110316      *
029200110316     C                   Endsr
029300110316      **?------------------------------------------------------------------ */
029400110316      **    Scrittura  del segmento BGM                Edi Standard
029500110316      **?------------------------------------------------------------------ */
029600110316     C     Segm_BGM      Begsr
029700110316      *
029800110318     c                   eval         segmento = BGM_sgm
029900110519     c                              + %Trim(TA1001)
030000110519     c                              + '+'
030100110518     c                              + %Trim(TA1004)
030200110316     c                              + '+'
030300110318     c                              + %Trim(TA1225)
030400110316
030500110518     C                   EXSR      TIVGD
030600110316      *
030700110316     C                   Endsr
030800110519      **?------------------------------------------------------------------ */
030900110519      **    Scrittura  del segmento FTX                Edi Standard
031000110519      **?------------------------------------------------------------------ */
031100110519     C     Segm_FTX      Begsr
031200110519      *
031300110519      *  testata
031400110519     c                   if        Tipo_DS = 'TT05'
031500110519     c                   eval         segmento = FTX_sgm
031600110519      *
031700110519     c                   if          T05sk <> *blank
031800110524      *
031900110524     c                   eval      campo_alfa = TB4440
032000111018     c                   eval      lunghezza_campo=%len(TB4440)
032100110524     c                   exsr      Char_Speciali
032200110524     c                   eval      TB4440 = campo_alfa
032300110519      *
032400110519     c                   eval         segmento = %Trim(segmento)
032500110519     c                              + %Trim(TB4451)
032600110519     c                              + '+++'
032700110519     c                              + %Trim(TB4440)
032800110519      *
032900110519     c                   if          T05(2) <> *blank
033000110524      *
033100110524     c                   eval      campo_alfa = TBA440
033200111018     c                   eval      lunghezza_campo=%len(TBA440)
033300110524     c                   exsr      Char_Speciali
033400110524     c                   eval      TBA440 = campo_alfa
033500110524      *
033600110519     c                   eval         segmento = %Trim(segmento)
033700110519     c                              + ':'
033800110519     c                              + %Trim(TBA440)
033900110519     c                   end
034000110519      *
034100110519     c                   if          T05(3) <> *blank
034200110524      *
034300110524     c                   eval      campo_alfa = TBB440
034400111018     c                   eval      lunghezza_campo=%len(TBB440)
034500110524     c                   exsr      Char_Speciali
034600110524     c                   eval      TBB440 = campo_alfa
034700110524      *
034800110519     c                   eval         segmento = %Trim(segmento)
034900110519     c                              + ':'
035000110519     c                              + %Trim(TBB440)
035100110519     c                   end
035200110519      *
035300110519     c                   if          T05(4) <> *blank
035400110524      *
035500110524     c                   eval      campo_alfa = TBC440
035600111018     c                   eval      lunghezza_campo=%len(TBC440)
035700110524     c                   exsr      Char_Speciali
035800110524     c                   eval      TBC440 = campo_alfa
035900110524      *
036000110519     c                   eval         segmento = %Trim(segmento)
036100110519     c                              + ':'
036200110519     c                              + %Trim(TBC440)
036300110519     c                   end
036400110519      *
036500110519     c                   if          T05(5) <> *blank
036600110524      *
036700110524     c                   eval      campo_alfa = TBD440
036800111018     c                   eval      lunghezza_campo=%len(TBD440)
036900110524     c                   exsr      Char_Speciali
037000110524     c                   eval      TBD440 = campo_alfa
037100110524      *
037200110519     c                   eval         segmento = %Trim(segmento)
037300110519     c                              + ':'
037400110519     c                              + %Trim(TBD440)
037500110519     c                   end
037600110519      *
037700110519     C                   EXSR      TIVGD
037800110519     c                   end
037900110519      *
038000110519      * dettaglio
038100110519     c                   elseIf    Tipo_DS = 'TD05'
038200110519      *
038300121018      *  Invia le note di di consegna oppure il tipo contrassegno.
038400121018      *   prima i 5 del secondo ciclo perchè potrebbe esserci il CONTRASSEGNO
038500121018     c                   if          D05Ask<> *blank
038600121018      *
038700121018     c                   eval      campo_alfa = DBF440
038800121018     c                   eval      lunghezza_campo=%len(DBF440)
038900121018     c                   exsr      Char_Speciali
039000121018     c                   eval      DBF440 = campo_alfa
039100121018      *
039200121018     c                   eval         segmento = FTX_sgm
039300121018     c                              + %Trim(DBE451)
039400121018     c                              + '+++'
039500121018     c                              + %Trim(DBF440)
039600121018      *
039700121018     c                   if          D05A(2) <> *blank
039800121018      *
039900121018     c                   eval      campo_alfa = DBG440
040000121018     c                   eval      lunghezza_campo=%len(DBG440)
040100121018     c                   exsr      Char_Speciali
040200121018     c                   eval      DBG440 = campo_alfa
040300121018      *
040400121018     c                   eval         segmento = %Trim(segmento)
040500121018     c                              + ':'
040600121018     c                              + %Trim(DBG440)
040700121018     c                   end
040800121018      *
040900121018     c                   if          D05A(3) <> *blank
041000121018      *
041100121018     c                   eval      campo_alfa = DBH440
041200121018     c                   eval      lunghezza_campo=%len(DBH440)
041300121018     c                   exsr      Char_Speciali
041400121018     c                   eval      DBH440 = campo_alfa
041500121018      *
041600121018     c                   eval         segmento = %Trim(segmento)
041700121018     c                              + ':'
041800121018     c                              + %Trim(DBH440)
041900121018     c                   end
042000121018      *
042100121018     c                   if          D05A(4) <> *blank
042200121018      *
042300121018     c                   eval      campo_alfa = DBI440
042400121018     c                   eval      lunghezza_campo=%len(DBI440)
042500121018     c                   exsr      Char_Speciali
042600121018     c                   eval      DBI440 = campo_alfa
042700121018      *
042800121018     c                   eval         segmento = %Trim(segmento)
042900121018     c                              + ':'
043000121018     c                              + %Trim(DBI440)
043100121018     c                   end
043200121018      *
043300121018     c                   if          D05A(5) <> *blank
043400121018      *
043500121018     c                   eval      campo_alfa = DBL440
043600121018     c                   eval      lunghezza_campo=%len(DBL440)
043700121018     c                   exsr      Char_Speciali
043800121018     c                   eval      DBL440 = campo_alfa
043900121018      *
044000121018     c                   eval         segmento = %Trim(segmento)
044100121018     c                              + ':'
044200121018     c                              + %Trim(DBL440)
044300121018     c                   end
044400121018      *
044500121018     C                   EXSR      TIVGD
044600121018     c                   end
044700121018      *
044800121018      * -----  NOTE DA INVIARE
044900110519     c                   if          D05sk <> *blank
045000110524      *
045100110524     c                   eval      campo_alfa = DB4440
045200111018     c                   eval      lunghezza_campo=%len(DB4440)
045300110524     c                   exsr      Char_Speciali
045400110524     c                   eval      DB4440 = campo_alfa
045500110519      *
045600110519     c                   eval         segmento = FTX_sgm
045700110519     c                              + %Trim(DB4451)
045800110519     c                              + '+++'
045900110519     c                              + %Trim(DB4440)
046000110519      *
046100110519     c                   if          D05(2) <> *blank
046200110524      *
046300110524     c                   eval      campo_alfa = DBA440
046400111018     c                   eval      lunghezza_campo=%len(DBA440)
046500110524     c                   exsr      Char_Speciali
046600110524     c                   eval      DBA440 = campo_alfa
046700110524      *
046800110519     c                   eval         segmento = %Trim(segmento)
046900110519     c                              + ':'
047000110519     c                              + %Trim(DBA440)
047100110519     c                   end
047200110519      *
047300110519     c                   if          D05(3) <> *blank
047400110524      *
047500110524     c                   eval      campo_alfa = DBB440
047600111018     c                   eval      lunghezza_campo=%len(DBB440)
047700110524     c                   exsr      Char_Speciali
047800110524     c                   eval      DBB440 = campo_alfa
047900110524      *
048000110519     c                   eval         segmento = %Trim(segmento)
048100110519     c                              + ':'
048200110519     c                              + %Trim(DBB440)
048300110519     c                   end
048400110519      *
048500110519     c                   if          D05(4) <> *blank
048600110524      *
048700110524     c                   eval      campo_alfa = DBC440
048800111018     c                   eval      lunghezza_campo=%len(DBC440)
048900110524     c                   exsr      Char_Speciali
049000110524     c                   eval      DBC440 = campo_alfa
049100110524      *
049200110519     c                   eval         segmento = %Trim(segmento)
049300110519     c                              + ':'
049400110519     c                              + %Trim(DBC440)
049500110519     c                   end
049600110519      *
049700110519     c                   if          D05(5) <> *blank
049800110524      *
049900110524     c                   eval      campo_alfa = DBD440
050000111018     c                   eval      lunghezza_campo=%len(DBD440)
050100110524     c                   exsr      Char_Speciali
050200110524     c                   eval      DBD440 = campo_alfa
050300110524      *
050400110519     c                   eval         segmento = %Trim(segmento)
050500110519     c                              + ':'
050600110519     c                              + %Trim(DBD440)
050700110519     c                   end
050800110519      *
050900110519     C                   EXSR      TIVGD
051000110519     c                   end
051100110519      * dettaglio
051200110519     c                   elseIf    Tipo_DS = 'TD20'
051300110519      *
051400110519     c                   if          DE4451 <> *blank
051500110524      *
051600110524     c                   eval      campo_alfa = DE4440
051700111018     c                   eval      lunghezza_campo=%len(DE4440)
051800110524     c                   exsr      Char_Speciali
051900110524     c                   eval      DE4440 = campo_alfa
052000110524      *
052100110519     c                   eval         segmento = FTX_sgm
052200110519     c                              + %Trim(DE4451)
052300110519     c                              + '+++'
052400110519     c                              + %Trim(DE4440)
052500110519      *
052600110519     C                   EXSR      TIVGD
052700110519     c                   end
052800110519      *
052900110519     c                   end
053000110519      *
053100110519     C                   Endsr
053200110518      **?------------------------------------------------------------------ */
053300110518      **    Scrittura  del segmento CNT                Edi Standard
053400110518      **?------------------------------------------------------------------ */
053500110518     C     Segm_CNT      Begsr
053600110518      *
053700110518     c                   eval         segmento = CNT_sgm
053800110518     c                              + %Trim(TC6069)
053900110520     c                              + ':'
054000110518     c                              + %Trim(%editc(TC6066:'Z'))
054100110519
054200110518     C                   EXSR      TIVGD
054300110518      *
054400110518      *  se c'è anche l'altro contatore:
054500110518     c                   if        TCA069 <> *blank
054600110518      *
054700110518     c                   eval         segmento = CNT_sgm
054800110518     c                              + %Trim(TCA069)
054900110520     c                              + ':'
055000110518     c                              + %Trim(%editc(TCA066:'Z'))
055100110518
055200110518     C                   EXSR      TIVGD
055300110518     C                   End
055400110518      *
055500110518     C                   Endsr
055600110518      **?------------------------------------------------------------------ */
055700110518      **    Scrittura  del segmento RFF                Edi Standard
055800110518      **?------------------------------------------------------------------ */
055900110518     C     Segm_RFF      Begsr
056000110518      *
056100110518      *   testata
056200110518     c                   if        Tipo_DS = 'TT15'
056300110518      *
056400110519     c                   if        TD1153 <> *blank
056500110519     c                   eval         segmento = RFF_sgm
056600110518     c                              + %Trim(TD1153)
056700110518     c                              + ':'
056800110518     c                              + %Trim(TD1154)
056900110519      *
057000110519     C                   EXSR      TIVGD
057100110518     c                   end
057200110519      *
057300110518      *   dettaglio
057400110519     c                   elseIf    Tipo_DS = 'TD10'
057500110519      *
057600110519     c                   if        DC1153 <> *blank
057700110518     c                   eval         segmento = RFF_sgm
057800110519     c                              + %Trim(DC1153)
057900110518     c                              + ':'
058000110519     c                              + %Trim(DC1154)
058100110519      *
058200110519     C                   EXSR      TIVGD
058300110518     c                   end
058400110519      *
058500110519     c                   if        DCA153 <> *blank
058600110519     c                   eval         segmento = RFF_sgm
058700110519     c                              + %Trim(DCA153)
058800110519     c                              + ':'
058900110519     c                              + %Trim(DCA154)
059000110519      *
059100110519     C                   EXSR      TIVGD
059200110519     c                   end
059300110519      *
059400110519     c                   if        DCB153 <> *blank
059500110519     c                   eval         segmento = RFF_sgm
059600110519     c                              + %Trim(DCB153)
059700110519     c                              + ':'
059800110519     c                              + %Trim(DCB154)
059900110519      *
060000110519     C                   EXSR      TIVGD
060100110519     c                   end
060200110519      *
060300110518     c                   end
060400110518      *
060500110518     C                   Endsr
060600110518      **?------------------------------------------------------------------ */
060700110519      **    Scrittura  del segmento TDT                Edi Standard
060800110316      **?------------------------------------------------------------------ */
060900110519     C     Segm_TDT      Begsr
061000110316      *
061100110520     c                   if        Tipo_DS = 'TT20'
061200110520      ***
061300110520     c                   eval         segmento = TDT_sgm
061400110520     c                              + %Trim(TE8051)
061500110520     c                              + '++'
061600110520     c                              + %Trim(TE8067)
061700110520      ***
061800110520     C                   EXSR      TIVGD
061900110520     C                   End
062000110519      *
062100110519     C                   Endsr
062200110519      **?------------------------------------------------------------------ */
062300110519      **    Scrittura  del segmento DTM                Edi Standard
062400110519      **?------------------------------------------------------------------ */
062500110519     C     Segm_DTM      Begsr
062600110519      *
062700110405      *  in testata
062800110518     c                   if        Tipo_DS = 'TT15'
062900110519      ***
063000110518     c                   if         TD2005 <> *blank
063100110518     c                   eval         segmento = DTM_sgm
063200110518     c                              + %Trim(TD2005)
063300110316     c                              + ':'
063400110518     c                              + %Trim(TD2380)
063500110316     c                              + ':'
063600110518     c                              + %Trim(TD2379)
063700110519      *
063800110518     C                   EXSR      TIVGD
063900110518     c                   end
064000110519      *
064100110518      ** altre date testata
064200110518     c                   elseIf    Tipo_DS = 'TT20'
064300110519      *
064400110518     c                   If          TE2005 <> *blank
064500110518     c                   eval         segmento = DTM_sgm
064600110518     c                              + %Trim(TE2005)
064700110518     c                              + ':'
064800110518     c                              + %Trim(TE2380)
064900110518     c                              + ':'
065000110518     c                              + %Trim(TE2379)
065100110519     c*
065200110518     C                   EXSR      TIVGD
065300110518     c                   end
065400110518      *-
065500110518     c                   If          TEA005 <> *blank
065600110518     c                   eval         segmento = DTM_sgm
065700110518     c                              + %Trim(TEA005)
065800110518     c                              + ':'
065900110518     c                              + %Trim(TEA380)
066000110518     c                              + ':'
066100110518     c                              + %Trim(TEA379)
066200110519      *
066300110518     C                   EXSR      TIVGD
066400110518     c                   end
066500110518      *-
066600110518     c                   If          TEB005 <> *blank
066700110518     c                   eval         segmento = DTM_sgm
066800110518     c                              + %Trim(TEB005)
066900110518     c                              + ':'
067000110518     c                              + %Trim(TEB380)
067100110518     c                              + ':'
067200110518     c                              + %Trim(TEB379)
067300110519      *
067400110518     C                   EXSR      TIVGD
067500110518     c                   end
067600110405      *  in dettaglio
067700110519     c                   elseIf    Tipo_DS = 'TD00'
067800110519      ***
067900110520     c                   If          DA2005 <> *blank
068000110518     c                   eval         segmento = DTM_sgm
068100110519     c                              + %Trim(DA2005)
068200110316     c                              + ':'
068300110519     c                              + %Trim(DA2380)
068400110316     c                              + ':'
068500110519     c                              + %Trim(DA2379)
068600110519      *
068700110519     C                   EXSR      TIVGD
068800110520     c                   end
068900110520      *
069000110518     c                   end
069100110316      *
069200110316     C                   Endsr
069300110316      **?------------------------------------------------------------------ */
069400110519      **    Scrittura  del segmento TSR                Edi Standard
069500110316      **?------------------------------------------------------------------ */
069600110519     C     Segm_TSR      Begsr
069700110316      *
069800110519     c                   if        DA4219 <> *blank
069900110519     c                   eval         segmento = TSR_sgm
070000110519     c                              + '++'
070100110519     c                              + %Trim(DA4219)
070200110519     c                   if        DA7085 <> *blank
070300110519     c                   eval         segmento = %trim(segmento)
070400110519     c                              + '+'
070500110519     c                              + %Trim(DA7085)
070600110519     c                   end
070700110520      *
070800110520     C                   EXSR      TIVGD
070900110519     c                   end
071000110519      *
071100110519     C                   Endsr
071200110519      **?------------------------------------------------------------------ */
071300110519      **    Scrittura  del segmento MOA                Edi Standard
071400110519      **?------------------------------------------------------------------ */
071500110519     C     Segm_MOA      Begsr
071600110519      *
071700110519     c                   if        DA5025 <> *blank
071800110519     c                   clear                   campo16          16
071900111128     c                   eval       campo16 = %editc(DA5004:'Z')
072000111128     c                   IF           %subst(CAMPO16:14:1) = ' '
072100111128     c                   eval         %subst(CAMPO16:14:1) = '0'
072200111128     c                   end
072300111128     c                   IF           %subst(CAMPO16:13:1) = ' '
072400111128     c                   eval         %subst(CAMPO16:13:1) = '0'
072500111128     c                   end
072600110519     c                   eval         segmento = MOA_sgm
072700110519     c                              + %Trim(DA5025)
072800110519     c                              + ':'
072900110519     c                              + %Trim(%subst(CAMPO16:1:13))
073000110519     c                              + '.'
073100110519     c                              + %subst(CAMPO16:14:2)
073200110519     c                              + ':'
073300110519     c                              + %Trim(DA6345)
073400110519      *
073500110519     C                   EXSR      TIVGD
073600110519     C                   End
073700110519      *
073800110519     c                   if        DAA025 <> *blank
073900110519     c                   clear                   campo16
074000111128     c                   eval       campo16 = %editc(DAA004:'Z')
074100111128     c                   IF           %subst(CAMPO16:14:1) = ' '
074200111128     c                   eval         %subst(CAMPO16:14:1) = '0'
074300111128     c                   end
074400111128     c                   IF           %subst(CAMPO16:13:1) = ' '
074500111128     c                   eval         %subst(CAMPO16:13:1) = '0'
074600111128     c                   end
074700110519     c                   eval         segmento = MOA_sgm
074800110519     c                              + %Trim(DAA025)
074900110519     c                              + ':'
075000110519     c                              + %Trim(%subst(CAMPO16:1:13))
075100110519     c                              + '.'
075200110519     c                              + %subst(CAMPO16:14:2)
075300110519     c                              + ':'
075400110519     c                              + %Trim(DAA345)
075500110519      *
075600110519     C                   EXSR      TIVGD
075700110519     C                   End
075800110519      *
075900110519     c                   if        DAB025 <> *blank
076000110519     c                   clear                   campo16
076100111128     c                   eval       campo16 = %editc(DAB004:'Z')
076200111128     c                   IF           %subst(CAMPO16:14:1) = ' '
076300111128     c                   eval         %subst(CAMPO16:14:1) = '0'
076400111128     c                   end
076500111128     c                   IF           %subst(CAMPO16:13:1) = ' '
076600111128     c                   eval         %subst(CAMPO16:13:1) = '0'
076700111128     c                   end
076800110519     c                   eval         segmento = MOA_sgm
076900110519     c                              + %Trim(DAB025)
077000110519     c                              + ':'
077100110519     c                              + %Trim(%subst(CAMPO16:1:13))
077200110519     c                              + '.'
077300110519     c                              + %subst(CAMPO16:14:2)
077400110519     c                              + ':'
077500110519     c                              + %Trim(DAB345)
077600110519      *
077700110519     C                   EXSR      TIVGD
077800110519     C                   End
077900110519      *
078000110519     C                   Endsr
078100110519      **?------------------------------------------------------------------ */
078200110519      **    Scrittura  del segmento TOD                Edi Standard
078300110519      **?------------------------------------------------------------------ */
078400110519     C     Segm_TOD      Begsr
078500110519      *
078600110519     c                   eval         segmento = TOD_sgm
078700110519     c                              + %Trim(DC4055)
078800110519     c                              + '+'
078900110519     c                              + %Trim(DC4215)
079000110519     c                              + '+'
079100110519     c                              + %Trim(DC4053)
079200110519     c                              + ':'
079300110519     c                              + %Trim(DC1131)
079400110519      *
079500110519     C                   EXSR      TIVGD
079600110519      *
079700110519     C                   Endsr
079800110519      **?------------------------------------------------------------------ */
079900110519      **    Scrittura  del segmento LOC                Edi Standard
080000110519      **?------------------------------------------------------------------ */
080100110519     C     Segm_LOC      Begsr
080200110519      *
080300110519     c                   if        Tipo_DS = 'TT20'
080400110519      *
080500110519      *   solo se presenti dei dati
080600110519     c                   if        TE3227 <> ' '
080700110524      *
080800110524     c                   eval      campo_alfa = TE3225
080900111018     c                   eval      lunghezza_campo=%len(TE3225)
081000110524     c                   exsr      Char_Speciali
081100110524     c                   eval      TE3225 = campo_alfa
081200110524      *
081300110519     c                   eval         segmento = LOC_sgm
081400110519     c                              + %Trim(TE3227)
081500110519     c                              + '+'
081600110519     c                              + %Trim(TE3225)
081700110519      *
081800110519     C                   EXSR      TIVGD
081900110519     C                   End
082000110519      *
082100110519     C                   End
082200110519      *
082300110519     C                   Endsr
082400110519      **?------------------------------------------------------------------ */
082500110518      **    Scrittura  del segmento NAD                Edi Standard
082600110518      **?------------------------------------------------------------------ */
082700110518     C     Segm_NAD      Begsr
082800110518      *
082900110316     c                   eval         segmento = NAD_sgm
083000110316      *
083100110405      *  in testata
083200110519     c                   if        Tipo_DS = 'TT25'
083300110524      *
083400110524     c                   eval      campo_alfa = TF3039
083500111018     c                   eval      lunghezza_campo=%len(TF3039)
083600110524     c                   exsr      Char_Speciali
083700110524     c                   eval      TF3039 = campo_alfa
083800110524      *
083900110316     c                   eval         segmento = %Trim(segmento)
084000110519     c                              + %Trim(TF3035)
084100110519     c                              + '+'
084200110519     c                              + %Trim(TF3039)
084300110519      *
084400110519     C                   EXSR      TIVGD
084500110316      *
084600110405      *  in dettaglio
084700110519     c                   elseif    Tipo_DS = 'TD15'
084800110524      *
084900110524     c                   eval      campo_alfa = DD3039
085000111018     c                   eval      lunghezza_campo=%len(DD3039)
085100110524     c                   exsr      Char_Speciali
085200110524     c                   eval      DD3039 = campo_alfa
085300110524      *
085400110524     c                   eval      campo_alfa = DD3124
085500111018     c                   eval      lunghezza_campo=%len(DD3124)
085600110524     c                   exsr      Char_Speciali
085700110524     c                   eval      DD3124 = campo_alfa
085800110524      *
085900110524     c                   eval      campo_alfa = DD3036
086000111018     c                   eval      lunghezza_campo=%len(DD3036)
086100110524     c                   exsr      Char_Speciali
086200110524     c                   eval      DD3036 = campo_alfa
086300110524      *
086400110316     c                   eval         segmento = %Trim(segmento)
086500110519     c                              + %Trim(DD3035)
086600110519     c                              + '+'
086700110519     c                              + %Trim(DD3039)
086800110519     c                              + '+'
086900110519     c                              + %Trim(DD3124)
087000110519     c                              + '+'
087100110519     c                              + %Trim(DD3036)
087200110316      *
087300110519     c                   if             DDA036 <> *blank
087400110524      *
087500110524     c                   eval      campo_alfa = DDA036
087600111018     c                   eval      lunghezza_campo=%len(DDA036)
087700110524     c                   exsr      Char_Speciali
087800110524     c                   eval      DDA036 = campo_alfa
087900110524      *
088000110322     c                   eval         segmento = %Trim(segmento)
088100110519     c                              + ':'
088200110519     c                              + %Trim(DDA036)
088300110519     c                   end
088400110519      *
088500110524     c                   eval      campo_alfa = DD3042
088600111018     c                   eval      lunghezza_campo=%len(DD3042)
088700110524     c                   exsr      Char_Speciali
088800110524     c                   eval      DD3042 = campo_alfa
088900110524      *
089000110519     c                   eval         segmento = %Trim(segmento)
089100110519     c                              + '+'
089200110519     c                              + %Trim(DD3042)
089300110519      *
089400110519     c                   if             DDA042 <> *blank
089500110524      *
089600110524     c                   eval      campo_alfa = DDA042
089700111018     c                   eval      lunghezza_campo=%len(DDA042)
089800110524     c                   exsr      Char_Speciali
089900110524     c                   eval      DDA042 = campo_alfa
090000110524      *
090100110519     c                   eval         segmento = %Trim(segmento)
090200110519     c                              + ':'
090300110519     c                              + %Trim(DDA042)
090400110519     c                   end
090500110524      *
090600110524     c                   eval      campo_alfa = DD3164
090700111018     c                   eval      lunghezza_campo=%len(DD3164)
090800110524     c                   exsr      Char_Speciali
090900110524     c                   eval      DD3164 = campo_alfa
091000110524      *
091100110524     c                   eval      campo_alfa = DD3229
091200111018     c                   eval      lunghezza_campo=%len(DD3229)
091300110524     c                   exsr      Char_Speciali
091400110524     c                   eval      DD3229 = campo_alfa
091500110524      *
091600110524     c                   eval      campo_alfa = DD3251
091700111018     c                   eval      lunghezza_campo=%len(DD3251)
091800110524     c                   exsr      Char_Speciali
091900110524     c                   eval      DD3251 = campo_alfa
092000110524      *
092100110519     c                   eval         segmento = %Trim(segmento)
092200110519     c                              + '+'
092300110519     c                              + %Trim(DD3164)
092400110519     c                              + '+'
092500110519     c                              + %Trim(DD3229)
092600110519     c                              + '+'
092700110519     c                              + %Trim(DD3251)
092800110519     c                              + '+'
092900110519     c                              + %Trim(DD3207)
093000110519
093100110405     C                   EXSR      TIVGD
093200110322     c                   end
093300110316      *
093400110316     C                   Endsr
093500110405      **?------------------------------------------------------------------ */
093600110519      **    Scrittura  del segmento CTA                Edi Standard
093700110519      **?------------------------------------------------------------------ */
093800110519     C     Segm_CTA      Begsr
093900110519      *
094000110519     c                   eval         segmento = CTA_sgm
094100110519      *  in testata
094200110519     c                   if        Tipo_DS = 'TT25'
094300110519      *
094400110524     c                   eval      campo_alfa = TF3412
094500111018     c                   eval      lunghezza_campo=%len(TF3412)
094600110524     c                   exsr      Char_Speciali
094700110524     c                   eval      TF3412 = campo_alfa
094800110524      *
094900110519     c                   if        TF3139 <> ' '
095000110519     c                   eval         segmento = %Trim(segmento)
095100110519     c                              + %Trim(TF3139)
095200110519     c                              + '+:'
095300110519     c                              + %Trim(TF3412)
095400110519      *
095500110519     C                   EXSR      TIVGD
095600110519     c                   end
095700110519      *
095800110519      *  in dettaglio
095900110519     c                   elseif    Tipo_DS = 'TD15'
096000110524      *
096100110524     c                   eval      campo_alfa = DD3412
096200111018     c                   eval      lunghezza_campo=%len(DD3412)
096300110524     c                   exsr      Char_Speciali
096400110524     c                   eval      DD3412 = campo_alfa
096500110524      *
096600110519     c                   eval         segmento = %Trim(segmento)
096700110519     c                              + %Trim(DD3139)
096800110519     c                              + '+:'
096900110519     c                              + %Trim(DD3412)
097000110519
097100110519     C                   EXSR      TIVGD
097200110519     c                   end
097300110519      *
097400110519     C                   Endsr
097500110519      **?------------------------------------------------------------------ */
097600110519      **    Scrittura  del segmento COM                Edi Standard
097700110519      **?------------------------------------------------------------------ */
097800110519     C     Segm_COM      Begsr
097900110519      *
098000110519     c                   eval         segmento = COM_sgm
098100110519      *  in testata
098200110519     c                   if        Tipo_DS = 'TT25'
098300110519      *
098400110519     c                   if        TF3148 <> ' '
098500110524      *
098600110524     c                   eval      campo_alfa = TF3148
098700111018     c                   eval      lunghezza_campo=%len(TF3148)
098800110524     c                   exsr      Char_Speciali
098900110524     c                   eval      TF3148 = campo_alfa
099000110524      *
099100110519     c                   eval         segmento = %Trim(segmento)
099200110519     c                              + %Trim(TF3148)
099300110519     c                              + ':'
099400110519     c                              + %Trim(TF3155)
099500110519      *
099600110519     C                   EXSR      TIVGD
099700110519     c                   end
099800110519      *
099900110519     c                   if        TFA148 <> ' '
100000110524      *
100100110524     c                   eval      campo_alfa = TFA148
100200111018     c                   eval      lunghezza_campo=%len(TFA148)
100300110524     c                   exsr      Char_Speciali
100400110524     c                   eval      TFA148 = campo_alfa
100500110524      *
100600110519     c                   eval         segmento = %Trim(segmento)
100700110519     c                              + %Trim(TFA148)
100800110519     c                              + ':'
100900110519     c                              + %Trim(TFA155)
101000110519      *
101100110519     C                   EXSR      TIVGD
101200110519     c                   end
101300110519      *
101400110519      *  in dettaglio
101500110519     c                   elseif    Tipo_DS = 'TD15'
101600110524      *
101700110524     c                   eval      campo_alfa = DD3148
101800111018     c                   eval      lunghezza_campo=%len(DD3148)
101900110524     c                   exsr      Char_Speciali
102000110524     c                   eval      DD3148 = campo_alfa
102100110524      *
102200110519     c                   eval         segmento = %Trim(segmento)
102300110519     c                              + %Trim(DD3148)
102400110519      *
102500110519     c                   if        DD3155 <> ' '
102600110519     c                   eval         segmento = %Trim(segmento)
102700110519     c                              + ':'
102800110519     c                              + %Trim(DD3155)
102900110519     c                   end
103000110519
103100110519     C                   EXSR      TIVGD
103200110519      *---
103300110519     c                   if        DDA148 <> ' '
103400110524      *
103500110524     c                   eval      campo_alfa = DDA148
103600111018     c                   eval      lunghezza_campo=%len(DDA148)
103700110524     c                   exsr      Char_Speciali
103800110524     c                   eval      DDA148 = campo_alfa
103900110524      *
104000110519     c                   eval         segmento = COM_sgm
104100110519     c                              + %Trim(DDA148)
104200110519      *
104300110519     c                   if        DDA155 <> ' '
104400110519     c                   eval         segmento = %Trim(segmento)
104500110519     c                              + ':'
104600110519     c                              + %Trim(DDA155)
104700110519     c                   end
104800110519
104900110519     C                   EXSR      TIVGD
105000110519     c                   end
105100110519
105200110519     c                   end
105300110519      *
105400110519     C                   Endsr
105500110519      **?------------------------------------------------------------------ */
105600110519      **    Scrittura  del segmento EQD                Edi Standard
105700110405      **?------------------------------------------------------------------ */
105800110519     C     Segm_EQD      Begsr
105900110405      *
106000110519      *
106100110519     C                   Endsr
106200110519      **?------------------------------------------------------------------ */
106300110519      **    Scrittura  del segmento EQN                Edi Standard
106400110519      **?------------------------------------------------------------------ */
106500110519     C     Segm_EQN      Begsr
106600110519      *
106700110519      *
106800110519     C                   Endsr
106900110519      **?------------------------------------------------------------------ */
107000110519      **    Scrittura  del segmento SEL                Edi Standard
107100110519      **?------------------------------------------------------------------ */
107200110519     C     Segm_SEL      Begsr
107300110519      *
107400110519      *
107500110519     C                   Endsr
107600110519      **?------------------------------------------------------------------ */
107700110519      **    Scrittura  del segmento CNI                Edi Standard
107800110519      **?------------------------------------------------------------------ */
107900110519     C     Segm_CNI      Begsr
108000110519      *
108100110316     c                   eval         segmento = CNI_sgm
108200110316     c                              + %Trim(%EDITC(DA1490:'Z'))
108300110316     c                              + '+'
108400110316     c                              + %Trim(DA1004)
108500110519     c                              + '+'
108600110519     c                              + %Trim(%EDITC(DA1312:'Z'))
108700110316
108800110316     C                   EXSR      TIVGD
108900110316      *
109000110316     C                   Endsr
109100110316      **?------------------------------------------------------------------ */
109200110519      **    Scrittura  del segmento MEA                Edi Standard
109300110316      **?------------------------------------------------------------------ */
109400110519     C     Segm_MEA      Begsr
109500110520      *
109600110519     c                   clear                   campo16          16
109700110519     c                   eval       campo16 = %editc(DF6314:'Z')
109800110520     c                   if        %subst(CAMPO16:1:13) = *blank
109900110520     c                   eval      %subst(CAMPO16:13:1) = '0'
110000110520     c                   enD
110100110520     c                   if        %subst(CAMPO16:14:1) = *blank
110200110520     c                   eval      %subst(CAMPO16:14:1) = '0'
110300110520     c                   enD
110400110520     c                   if        %subst(CAMPO16:15:1) = *blank
110500110520     c                   eval      %subst(CAMPO16:15:1) = '0'
110600110520     c                   enD
110700110316      *
110800110519     c                   eval         segmento = MEA_sgm
110900110519     c                              + %Trim(DF6311)
111000110519     c                              + '+'
111100110519     c                              + %Trim(DF6313)
111200110519     c                              + '+'
111300110519     c                              + %Trim(DF6411)
111400110519     c                              + ':'
111500110519     c                              + %Trim(%subst(CAMPO16:1:13))
111600110519      **
111700110519     c                   iF           DF6311 ='VOL'
111800110519     c                   eval         segmento = %Trim(segmento)
111900110519     c                              + '.'
112000110519     c                              + %subst(CAMPO16:14:2)
112100110519     c                   enD
112200110316      **
112300110316     C                   EXSR      TIVGD
112400110316      *
112500110316     C                   Endsr
112600110316      **?------------------------------------------------------------------ */
112700110316      **    Scrittura  del segmento GID                Edi Standard
112800110316      **?------------------------------------------------------------------ */
112900110316     C     Segm_GID      Begsr
113000110316      *
113100110316     c                   eval         segmento = GID_sgm
113200110316     c                              + %Trim(%EDITC(DE1496:'Z'))
113300110316     c                              + '+'
113400110316     c                              + %Trim(%EDITC(DE7224:'Z'))
113500110519     c                              + ':'
113600110519     c                              + %Trim(DE7065)
113700110316
113800110316     C                   EXSR      TIVGD
113900110316      *
114000110316     C                   Endsr
114100110316      **?------------------------------------------------------------------ */
114200110519      **    Scrittura  del segmento PIA                Edi Standard
114300110316      **?------------------------------------------------------------------ */
114400110519     C     Segm_PIA      Begsr
114500110411      *
114600110519      *
114700110519     C                   Endsr
114800110519      **?------------------------------------------------------------------ */
114900110519      **    Scrittura  del segmento PCI                Edi Standard
115000110519      **?------------------------------------------------------------------ */
115100110519     C     Segm_PCI      Begsr
115200110519      *
115300110411      *  Se c'è almeno un Barcode
115400110519     C                   IF        DG7102 <> *BLANK
115500110524      *
115600110524     c                   eval      campo_alfa = DG7102
115700111018     c                   eval      lunghezza_campo=%len(DG7102)
115800110524     c                   exsr      Char_Speciali
115900110524     c                   eval      DG7102 = campo_alfa
116000110524      *
116100120330      *  In presenza del Qualificatore
116200120330     c                   if             DG4233 <> *blank
116300120330      *
116400110316     c                   eval         segmento = PCI_sgm
116500120330     c                              + %Trim(DG4233)
116600120330     c                              + '+'
116700120330     c                              + %Trim(DG7102)
116800120330     c                   else
116900120330      *
117000120330     c                   eval         segmento = PCI_sgm
117100120330     c                              + '+'
117200120330     c                              + %Trim(DG7102)
117300120330     c                   end
117400120330      *  2°barcode
117500110519     c                   if             DGA102 <> *blank
117600110524      *
117700110524     c                   eval      campo_alfa = DGA102
117800111018     c                   eval      lunghezza_campo=%len(DGA102)
117900110524     c                   exsr      Char_Speciali
118000110524     c                   eval      DGA102 = campo_alfa
118100110524      *
118200110316     c                   eval         segmento = %Trim(segmento)
118300110316     c                              + ':'
118400110519     c                              + %Trim(DGA102)
118500110316     c                   end
118600110316      *
118700110519     c                   if             DGB102 <> *blank
118800110524      *
118900110524     c                   eval      campo_alfa = DGB102
119000111018     c                   eval      lunghezza_campo=%len(DGB102)
119100110524     c                   exsr      Char_Speciali
119200110524     c                   eval      DGB102 = campo_alfa
119300110524      *
119400110316     c                   eval         segmento = %Trim(segmento)
119500110316     c                              + ':'
119600110519     c                              + %Trim(DGB102)
119700110316     c                   end
119800110316      *
119900110519     c                   if             DGC102 <> *blank
120000110524      *
120100110524     c                   eval      campo_alfa = DGC102
120200111018     c                   eval      lunghezza_campo=%len(DGC102)
120300110524     c                   exsr      Char_Speciali
120400110524     c                   eval      DGC102 = campo_alfa
120500110524      *
120600110316     c                   eval         segmento = %Trim(segmento)
120700110316     c                              + ':'
120800110519     c                              + %Trim(DGC102)
120900110316     c                   end
121000110316      *
121100110519     c                   if             DGD102 <> *blank
121200110524      *
121300110524     c                   eval      campo_alfa = DGD102
121400111018     c                   eval      lunghezza_campo=%len(DGD102)
121500110524     c                   exsr      Char_Speciali
121600110524     c                   eval      DGD102 = campo_alfa
121700110524      *
121800110316     c                   eval         segmento = %Trim(segmento)
121900110316     c                              + ':'
122000110519     c                              + %Trim(DGD102)
122100110316     c                   end
122200110316      *
122300110519     c                   if             DGE102 <> *blank
122400110524      *
122500110524     c                   eval      campo_alfa = DGE102
122600111018     c                   eval      lunghezza_campo=%len(DGE102)
122700110524     c                   exsr      Char_Speciali
122800110524     c                   eval      DGE102 = campo_alfa
122900110524      *
123000110316     c                   eval         segmento = %Trim(segmento)
123100110316     c                              + ':'
123200110519     c                              + %Trim(DGE102)
123300110316     c                   end
123400110316      *
123500110519     c                   if             DGF102 <> *blank
123600110524      *
123700110524     c                   eval      campo_alfa = DGF102
123800111018     c                   eval      lunghezza_campo=%len(DGF102)
123900110524     c                   exsr      Char_Speciali
124000110524     c                   eval      DGF102 = campo_alfa
124100110524      *
124200110316     c                   eval         segmento = %Trim(segmento)
124300110316     c                              + ':'
124400110519     c                              + %Trim(DGF102)
124500110316     c                   end
124600110316      *
124700110519     c                   if             DGG102 <> *blank
124800110524      *
124900110524     c                   eval      campo_alfa = DGG102
125000111018     c                   eval      lunghezza_campo=%len(DGG102)
125100110524     c                   exsr      Char_Speciali
125200110524     c                   eval      DGG102 = campo_alfa
125300110524      *
125400110316     c                   eval         segmento = %Trim(segmento)
125500110316     c                              + ':'
125600110519     c                              + %Trim(DGG102)
125700110316     c                   end
125800110316      *
125900110519     c                   if             DGH102 <> *blank
126000110524      *
126100110524     c                   eval      campo_alfa = DGH102
126200111018     c                   eval      lunghezza_campo=%len(DGH102)
126300110524     c                   exsr      Char_Speciali
126400110524     c                   eval      DGH102 = campo_alfa
126500110524      *
126600110316     c                   eval         segmento = %Trim(segmento)
126700110316     c                              + ':'
126800110519     c                              + %Trim(DGH102)
126900110316     c                   end
127000110316      *
127100110519     c                   if             DGI102 <> *blank
127200110524      *
127300110524     c                   eval      campo_alfa = DGI102
127400111018     c                   eval      lunghezza_campo=%len(DGI102)
127500110524     c                   exsr      Char_Speciali
127600110524     c                   eval      DGI102 = campo_alfa
127700110524      *
127800110316     c                   eval         segmento = %Trim(segmento)
127900110316     c                              + ':'
128000110519     c                              + %Trim(DGI102)
128100110316     c                   end
128200110316
128300110316     C                   EXSR      TIVGD
128400110316      *
128500110316      *  secondi 10 elementi
128600110519     c                   if             DGL102 <> *blank
128700110524      *
128800110524     c                   eval      campo_alfa = DGL102
128900111018     c                   eval      lunghezza_campo=%len(DGL102)
129000110524     c                   exsr      Char_Speciali
129100110524     c                   eval      DGL102 = campo_alfa
129200110524      *
129300120330      *  In presenza del Qualificatore
129400120330     c                   if             DG4233 <> *blank
129500120330      *
129600110519     c                   eval         segmento = PCI_sgm
129700120330     c                              + %Trim(DG4233)
129800110316     c                              + '+'
129900110519     c                              + %Trim(DGL102)
130000120330     c                   else
130100120330     c                   eval         segmento = PCI_sgm
130200120330     c                              + '+'
130300120330     c                              + %Trim(DGL102)
130400120330     c                   end
130500120330      *  2°barcode
130600110519     c                   if             DGM102 <> *blank
130700110524      *
130800110524     c                   eval      campo_alfa = DGM102
130900111018     c                   eval      lunghezza_campo=%len(DGM102)
131000110524     c                   exsr      Char_Speciali
131100110524     c                   eval      DGM102 = campo_alfa
131200110524      *
131300110316     c                   eval         segmento = %Trim(segmento)
131400110316     c                              + ':'
131500110519     c                              + %Trim(DGM102)
131600110316     c                   end
131700110316      *
131800110519     c                   if             DGN102 <> *blank
131900110524      *
132000110524     c                   eval      campo_alfa = DGN102
132100111018     c                   eval      lunghezza_campo=%len(DGN102)
132200110524     c                   exsr      Char_Speciali
132300110524     c                   eval      DGN102 = campo_alfa
132400110524      *
132500110316     c                   eval         segmento = %Trim(segmento)
132600110316     c                              + ':'
132700110519     c                              + %Trim(DGN102)
132800110316     c                   end
132900110316      *
133000110519     c                   if             DGO102 <> *blank
133100110524      *
133200110524     c                   eval      campo_alfa = DGO102
133300111018     c                   eval      lunghezza_campo=%len(DGO102)
133400110524     c                   exsr      Char_Speciali
133500110524     c                   eval      DGO102 = campo_alfa
133600110524      *
133700110316     c                   eval         segmento = %Trim(segmento)
133800110316     c                              + ':'
133900110519     c                              + %Trim(DGO102)
134000110316     c                   end
134100110316      *
134200110519     c                   if             DGP102 <> *blank
134300110524      *
134400110524     c                   eval      campo_alfa = DGP102
134500111018     c                   eval      lunghezza_campo=%len(DGP102)
134600110524     c                   exsr      Char_Speciali
134700110524     c                   eval      DGP102 = campo_alfa
134800110524      *
134900110316     c                   eval         segmento = %Trim(segmento)
135000110316     c                              + ':'
135100110519     c                              + %Trim(DGP102)
135200110316     c                   end
135300110316      *
135400110519     c                   if             DGQ102 <> *blank
135500110524      *
135600110524     c                   eval      campo_alfa = DGQ102
135700111018     c                   eval      lunghezza_campo=%len(DGQ102)
135800110524     c                   exsr      Char_Speciali
135900110524     c                   eval      DGQ102 = campo_alfa
136000110524      *
136100110316     c                   eval         segmento = %Trim(segmento)
136200110316     c                              + ':'
136300110519     c                              + %Trim(DGQ102)
136400110316     c                   end
136500110316      *
136600110519     c                   if             DGR102 <> *blank
136700110524      *
136800110524     c                   eval      campo_alfa = DGR102
136900111018     c                   eval      lunghezza_campo=%len(DGR102)
137000110524     c                   exsr      Char_Speciali
137100110524     c                   eval      DGR102 = campo_alfa
137200110524      *
137300110316     c                   eval         segmento = %Trim(segmento)
137400110316     c                              + ':'
137500110519     c                              + %Trim(DGR102)
137600110316     c                   end
137700110316      *
137800110519     c                   if             DGS102 <> *blank
137900110524      *
138000110524     c                   eval      campo_alfa = DGS102
138100111018     c                   eval      lunghezza_campo=%len(DGS102)
138200110524     c                   exsr      Char_Speciali
138300110524     c                   eval      DGS102 = campo_alfa
138400110524      *
138500110316     c                   eval         segmento = %Trim(segmento)
138600110316     c                              + ':'
138700110519     c                              + %Trim(DGS102)
138800110316     c                   end
138900110316      *
139000110519     c                   if             DGT102 <> *blank
139100110524      *
139200110524     c                   eval      campo_alfa = DGT102
139300111018     c                   eval      lunghezza_campo=%len(DGT102)
139400110524     c                   exsr      Char_Speciali
139500110524     c                   eval      DGT102 = campo_alfa
139600110524      *
139700110316     c                   eval         segmento = %Trim(segmento)
139800110316     c                              + ':'
139900110519     c                              + %Trim(DGT102)
140000110316     c                   end
140100110316      *
140200110519     c                   if             DGU102 <> *blank
140300110524      *
140400110524     c                   eval      campo_alfa = DGU102
140500111018     c                   eval      lunghezza_campo=%len(DGU102)
140600110524     c                   exsr      Char_Speciali
140700110524     c                   eval      DGU102 = campo_alfa
140800110524      *
140900110316     c                   eval         segmento = %Trim(segmento)
141000110316     c                              + ':'
141100110519     c                              + %Trim(DGU102)
141200110316     c                   end
141300110316      *
141400110316     C                   EXSR      TIVGD
141500110316     c                   end
141600110316      *
141700110411     c                   end
141800110411      *
141900110316     C                   Endsr
142000110316      **?------------------------------------------------------------------ */
142100110316      **    Scrittura  del segmento UNT                Edi Standard
142200110316      **?------------------------------------------------------------------ */
142300110316     C     Segm_UNT      Begsr
142400110316      *
142500110316     c                   eval         segmento = UNT_sgm
142600110524     c                              + %Trim(%EDITC(Conta_righe - 1:'Z'))
142700110316     c                              + '+1'
142800110316
142900110316     C                   EXSR      TIVGD
143000110316      *
143100110316     C                   Endsr
143200110316      **?------------------------------------------------------------------ */
143300110316      **    Scrittura  del segmento UNZ                Edi Standard
143400110316      **?------------------------------------------------------------------ */
143500110316     C     Segm_UNZ      Begsr
143600110316      *
143700110316     c                   eval         segmento = UNZ_sgm
143800110412     c                              + %Trim(TAPRG)
143900110316
144000110316     C                   EXSR      TIVGD
144100110525      *
144200110525      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
144300110525     c                   if        compatta_80  = 'S'
144400110525     c                   exsr      Scarica_su_VGD
144500110525     c                   end
144600110405      * quindi chiude
144700110405      *   il TIVGD
144800110405     C                   EXSR      vgd_CLOSE
144900110316      *
145000110316     C                   Endsr
145100110316      **?------------------------------------------------------------------ */
145200110405      **    FINTO ROLLBACK
145300110316      **?------------------------------------------------------------------ */
145400110405     C     ROLL_BCK      Begsr
145500110316      *
145600110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
145700110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
145800110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
145900110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
146000110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
146100110405      *
146200110405      *
146300110405     C                   Endsr
146400110520      **?------------------------------------------------------------------ */
146500110520      **    Caratteri particolari
146600110520      **?------------------------------------------------------------------ */
146700110520     C     Char_Speciali Begsr
146800110520      *
146900110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
147000110520      *   dichiarati nei segmenti    (+:'?)
147100110520      *   [più, due punti, apostrofo, punto interrogativo]
147200110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
147300110520      *   considerati caratteri Funzionali che, qualora debbano essere
147400110520      *   utilizzati come semplici caratteri di testo, dovranno essere
147500110520      *   preceduti da un (?).
147600110520      *
147700110520     c                   movel(p)  campo_alfa    campo_test
147800110520     c                   clear                   campo_ritorno
147900110520      *
148000110520     C                   call      'TRTCT00R3'
148100110520     c                   parm                    campo_test      512
148200110520     c                   parm                    campo_ritorno   512
148300110621     c                   parm                    esito
148400110520      *
148500110520     c                   movel(p)  campo_ritorno campo_alfa      512
148600111018      ******
148700111018      *  occorre fare attenzione se l'ultimo carattere è un (?) che precede
148800111018      *   uno dei carratteri speciali, questo può procurare problemi a chi riceve
148900111018      *    quindi si deve sostituire il (?) con un blank e togliere tutto ciò che
149000111018      *   sta sulla destra da lì in poi.
149100111018     c                   if        %subst(campo_alfa:lunghezza_campo-1:1) <>
149200111018     c                                     Punto_interrogativo   and
149300111018     c                             %subst(campo_alfa:lunghezza_campo:1)
149400111018     c                                 =   Punto_interrogativo
149500111018      *   Sostituisce il (?) nell'ultimo byte del campo se non aveva nel byte
149600111018      *    precedente un (?) e gli mette blank al posto del (?)
149700111018     c                   eval      %subst(campo_alfa:lunghezza_campo:1)
149800111018     c                                 =   un_blank
149900111018      * si è fatto ciò poichè altrimenti quando si va a comporre il segmento EDI
150000111018      *  il carattere funzionale (+ o : o ') potrebbero venire annullati dalla
150100111018      *  presenza di un (?) davanti creando problemi di traduzione.
150200111018     c                   end
150300110520      *
150400110520     C                   Endsr
150500110405      **?------------------------------------------------------------------ */
150600110405      **    Scrittura  del segmento                    Edi Standard
150700110405      **?------------------------------------------------------------------ */
150800110405     C     tivgd         Begsr
150900110519      *
151000110519      *   chiude il segmento con l'apostrofo
151100110520     c                   if         Conta_righe > 0
151200110519     c                   eval         segmento = %Trim(segmento)
151300110519     c                              + ''''
151400110520     c                   end
151500110405      *
151600110316     c                   eval       Conta_righe = 1 + Conta_righe
151700110525      *
151800110525      *  se deve scrivere il file di scarico segmento x ogni linea
151900110525     c                   if        compatta_80  <>'S'
152000110405     c                   exsr      VGD_WRITE
152100110525     c                   else
152200110525      *
152300110525      *  se invece deve scrivere a 80 colonne
152400110525     c                   exsr      WRITE_80COL
152500110525      *
152600110525     c                   end
152700110316      *
152800110316     c                   endsr
152900110405      **?__________________________________________________________________ */
153000110405      *    Apertura del TIVGD
153100110316      **?__________________________________________________________________ */
153200110405     C     VGD_OPEN      Begsr
153300110405      *
153400110617      *  recupera il progressivo
153500110617     c                   exsr      Piglia_progr
153600110617      *
153700110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
153800110405     C                   clear                   trul47ds
153900110405     C                   eval      d47opz  = 'I'
154000110617     C                   eval      d47tip  = TipoF
154100110405     C                   eval      d47lck  = 'N'
154200110405     C                   eval      d47chkj = 'N'
154300110519     C                   eval      d47pgm  = 'TRTCT91R1'
154400110405     C                   call      'TRUL47R'
154500110405     C                   parm                    trul47ds
154600110405      *
154700110405     c                   endsr
154800110617      **?__________________________________________________________________ */
154900110617      *   Scrittura del TIVGD
155000110617      **?__________________________________________________________________ */
155100110617     C     VGD_WRITE     Begsr
155200110617      *
155300110405     c                   clear                   tivgd000
155400110405     c                   eval      vgddta = %TrimR(Segmento)
155500110617     c                   eval      vgdtip = TipoF
155600110405     c                   eval      vgdksu = CLIENTE
155700110617     C                   eval      vgdprg = Progressivo
155800110405     c                   eval      vgdtsc = 'WW'
155900110519     c                   eval      vgdpgm = 'TRTCT91R1'
156000110405     c                   eval      vgddat = udtymd
156100110405      *
156200110405     C                   WRITE     tivgd000
156300110405      *
156400110405     c                   endsr
156500110405      **?__________________________________________________________________ */
156600110405      *    Chiusura del TIVGD
156700110405      **?__________________________________________________________________ */
156800110405     C     VGD_CLOSE     Begsr
156900110405      *
157000110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
157100110405     C                   clear                   trul47ds
157200110405     C                   eval      d47opz  = 'F'
157300110617     C                   eval      d47tip  = TipoF
157400110405     C                   call      'TRUL47R'
157500110405     C                   parm                    trul47ds
157600110405      *
157700110405     c                   endsr
157800110525      **?__________________________________________________________________ */
157900110525      *    Pulisce in QTEMP il file contenitore di 80 colonne
158000110405      **?__________________________________________________________________ */
158100110525     C     CLEAR_STD80F  Begsr
158200110525      *
158300110525      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
158400110525      *   a 80 colonne.
158500110525     c                   call      'TRTCT00R4C'
158600110525      *
158700110525     c                   endsr
158800110525      **?__________________________________________________________________ */
158900110525      *    Carica in  QTEMP il file contenitore di 80 colonne
159000110525      **?__________________________________________________________________ */
159100110525     C     WRITE_80COL   Begsr
159200110525      *
159300110525      * sposta la stringa del segmento appena composta
159400110525     c                   movel     segmento      segmento1950
159500110525      *
159600110525      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
159700110525     c                   call      'TRTCT00R4'
159800110525     c                   parm                    segmento1950
159900110525      *
160000110525     c                   endsr
160100110525      **?__________________________________________________________________ */
160200110525      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
160300110525      **?__________________________________________________________________ */
160400110525     C     Scarica_su_VGDBegsr
160500110525      *
160600110525     c                   open      EDISTD80F
160700110525     c                   read      EDISTD80F
160800110525      *
160900110525     c                   dow       not %EoF(EDISTD80F)
161000110525      *
161100110525     c                   eval      segmento = RIGA80STD
161200110525     c                   exsr      VGD_WRITE
161300110617      *
161400110525     c                   read      EDISTD80F
161500110525     c                   end
161600110525      *
161700110525     c                   endsr
161800110617      **?__________________________________________________________________ */
161900110617      *   Prende il Progressivo
162000110617      **?__________________________________________________________________ */
162100110617     C     Piglia_progr  Begsr
162200110617      *
162300110617     C                   MOVEL     CLIENTE       virKSC
162400110617     C                   MOVEL     TipoF         virTIP
162500110617     c     kvir02        chain     tivir02l
162600110617     c                   move      'OF'          Var_ISV
162700110617     c                   if        %Found(tivir02l)
162800110617     c                   move      VirFI1        Var_ISV
162900110617     C                   End
163000110617
163100110617      *       prende numeratore strategi
163200110617     C                   exsr      calprog
163300110617      *
163400110617     c                   endsr
163500110617     C*------------------------------------------------------*
163600110617     C*  prepara il file
163700110617     C*------------------------------------------------------*
163800110617     C     calprog       begsr
163900110617     C*
164000110617     C                   open      tis7prgf
164100110617     C                   read(e)   tis7prgf
164200110617     C*
164300110617     C                   if        not %error
164400110617     C                   eval      dwlprg = f_tis7prgf
164500110617     C*
164600110617     C                   move(p)   dwlprg        wrkprg
164700110617     C                   add       1             wrkprg
164800110617     C                   move(p)   wrkprg        dwlprg
164900110617     C                   movel     Var_ISV       dwlprg
165000110617     C*
165100110617     C                   eval       f_tis7prgf = dwlprg
165200110617     C                   eval      Progressivo = dwlprg
165300110617     C*
165400110617     C                   update    tis7prg0
165500110617     C                   endif
165600110617     C*
165700110617     C                   close     tis7prgf
165800110617     C*
165900110617     C                   endsr
166000110617      **?__________________________________________________________________ */
166100110621      * ?------------------------------------------------------------------ */
166200110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
166300110621      * ?------------------------------------------------------------------ */
166400110621     C     *pssr         BEGSR
166500110621     C
166600110621     C                   eval      esito = 'E'
166700110621     C                   ENDSR     '*CANCL'
166800110621     C
166900110621      *  ------------------------------------------------------------------ */
