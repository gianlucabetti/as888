000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200111221     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000030924
001100080924     D fnvac00t      e ds
001200071121
001300071121     D*------------
001400071121     D* DECODIFICHE COSTANTI
001500071121     D*------------
001600071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700071121
001800071121
001900071121     D*------------
002000071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002100071121     D*------------
002200071121     D iHtml           s             10    inz('<html>')
002300071121     D fHtml           s             10    inz('</html>')
002400071121     D iBody           s             10    inz('<body>')
002500071121     D fBody           s             10    inz('</body>')
002600071121     D iTable          s             20    inz('<table border="1">')
002700071121     D fTable          s             10    inz('</table>')
002800071121     D iThead          s             10    inz('<thead>')
002900071121     D fThead          s             10    inz('</thead>')
003000071121     D iRow            s              5    inz('<tr>')
003100071121     D fRow            s              5    inz('</tr>')
003200071121     D iCol            s              5    inz('<td>')
003300071121     D fCol            s              5    inz('</td>')
003400071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003500071121     D fColBold        s              5    inz('</th>')
003600071122     D cApi            s              1    inz('''')
003700071121
003800071121
003900071121     D*------------
004000071121     D* VARIABILI D WRK
004100071121     D*------------
004200030924     D prmppt          S             50
004300030924     D prmesito        S              1
004400030924     D wrkesito        S                   like(prmesito)
004500071122     D wBufferOut      s          10000    varying inz
004600100910     D wDataSts        S              8  0 inz(*zeros)
004700100907     D wConsOkSiNo     S              1    inz(*blanks)
004800100907     D wConsOkSiNoDes  S            300    inz(*blanks)
004900140306     D wIND            s              9    inz(*blanks)
005000140306     D wLOD            s             35    inz(*blanks)
005100100517     D wCAD            s              9    inz(*blanks)
005200140306     D wPRD            s              9    inz(*blanks)
005300071211     D*
005400100910
005500100910
005600100910     D*------------------
005700100910     D* LINKING A DEFINIZIONI ESTERNE
005800100910     D*------------------
005900100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006000100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006100111228     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006200111228     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006300100910
006400030924
006500030325
006600030924     C* MAIN/
006700071121     C*
006800071121     C* Verifico se già presenti dati nel file d output
006900071121     C                   EXSR      chkRECFILMBR
007000071121     C*
007100071121     C* Effettuo elaborazione
007200071121     C                   EXSR      traduci
007300071121     C*
007400921023     C                   SETON                                        LR
007500991027
007600071121
007700071121
007800071121     C     chkRECFILMBR  BEGSR
007900071121     C*
008000071121     C                   READ      TIVAWWWT                               55
008100071121     C*
008200071121     C                   ENDSR
008300071121
008400071121
008500030924
008600991027     C     traduci       BEGSR
008700071121     C*
008800071121     C* Head iniziali della struttura HTML
008900100908     C                   IF        %subst(prmppt:2:1) = 'H'
009000071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009100071210     C   55              WRITE     TIVAW000
009200100908     C                   ENDIF
009300071121     C*
009400071121     C* Inizializzazone formato e variabili x output
009500071121     C                   clear                   TIVAW000
009600071121     C                   clear                   wBufferOut
009700100907     C                   clear                   wConsOkSiNo
009800100907     C                   clear                   wConsOkSiNoDes
009900071121     C*
010000071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010100071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010200100908     C*
010300100908     C                   SELECT
010400100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
010500071121     C                   EVAL      wBufferOut = iThead + iRow
010600111228     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
010700140306     C                             +iColBold+ 'RIF. MITTENTE'          +fColBold
010800140306     C                             +iColBold+ 'RIF. MITTENTE ALFA'     +fColBold
010900111228     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
011000111228     C                             +iColBold+ 'DESTINATARIO'           +fColBold
011100140306     C                             +iColBold+ 'INDIRIZZO'              +fColBold
011200140306     C                             +iColBold+ 'LOCALITA'               +fColBold
011300111228     C                             +iColBold+ 'CAP'                    +fColBold
011400140306     C                             +iColBold+ 'PROVINCIA'              +fColBold
011500111228     C                             +iColBold+ 'STATO'                  +fColBold
011600111228     C                             +iColBold+ 'DATA STATO'             +fColBold
011700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
011800100908     C                   EVAL      wBufferOut =
011900140306     C                              '"'+'CODICE CLIENTE'         +'";'
012000140306     C                             +'"'+'RIF. MITTENTE'          +'";'
012100140306     C                             +'"'+'RIF. MITTENTE ALFA'     +'";'
012200140306     C                             +'"'+'DATA SPEDIZIONE'        +'";'
012300140306     C                             +'"'+'DESTINATARIO'           +'";'
012400140306     C                             +'"'+'INDIRIZZO'              +'";'
012500140306     C                             +'"'+'LOCALITA'               +'";'
012600140306     C                             +'"'+'CAP'                    +'";'
012700140306     C                             +'"'+'PROVINCIA'              +'";'
012800140306     C                             +'"'+'STATO'                  +'";'
012900140306     C                             +'"'+'DATA STATO'             +'";'
013000100908     C                   ENDSL
013100100908     C*
013200071121     C                   EVAL      VAWDTA = wBufferOut
013300071121     C                   WRITE     TIVAW000
013400071121     C                   ENDIF
013500991027     C*
013600080924     C                   READ      TIVGDTMP
013700060119     C*
013800080924     C                   DOW       not %eof(TIVGDTMP)
013900080924     C*
014000080924     C                   eval      fnvac00t = vgdDTA
014100071121     C*
014200071121     C                   clear                   TIVAW000
014300071121     C                   clear                   wBufferOut
014400100907     C                   clear                   wConsOkSiNo
014500100907     C                   clear                   wConsOkSiNoDes
014600100910     C*
014700100910     C* Solo bolle originali e valide ai fini della spedizione
014800100910     C                   movel     *blanks       wEsito1           1
014900111223     C                   movel     *blanks       wEsito2           1
015000111223     C                   z-add     *zeros        wrkDCM            8 0
015100111223     C                   movel     *blanks       wrkCCA            1
015200100910     C*
015300100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015400100910     C                                                vacAAS
015500100910     C                                               :vacLNP
015600100910     C                                               :vacNRS
015700100910     C                                               :vacNSP
015800100910     C                                               :pOutLblTyp
015900100910     C                                               :pOutAnnoBO
016000100910     C                                               :pOutLineaParBO
016100100910     C                                               :pOutSerieBO
016200100910     C                                               :pOutNumSpedBO
016300100910     C                                               :pOutDcmBO
016400100910     C                                               :pOutCcaBO
016500100910     C                                               :pOutRblBO))
016600100910     C*
016700100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
016800111223     C*
016900111223     C* Chiamata metodo GetLastChild
017000111223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
017100111223     C                                                pOutAnnoBO
017200111223     C                                               :pOutLineaParBO
017300111223     C                                               :pOutSerieBO
017400111223     C                                               :pOutNumSpedBO
017500111223     C                                               :pOutAnnoFI
017600111223     C                                               :pOutLineaParFI
017700111223     C                                               :pOutSerieFI
017800111223     C                                               :pOutNumSpedFI
017900111223     C                                               :pOutDcmFI
018000111223     C                                               :pOutCcaFI))
018100111223     C*
018200111223     C                   if        wEsito2 = '0'
018300111223     C                   eval      wrkDCM = pOutDcmFI
018400111223     C                   eval      wrkCCA = pOutCcaFI
018500111223     C                   else
018600111223     C                   eval      wrkDCM = pOutDcmBO
018700111223     C                   eval      wrkCCA = pOutCcaBO
018800111223     C                   endif
018900071210     C*
019000071210     C* Inizializzo campi d wrk
019100140306     C                   EVAL      wIND = *blanks
019200071213     C                   EVAL      wLOD = *blanks
019300140306     C                   EVAL      wCAD = *blanks
019400140306     C                   EVAL      wPRD = *blanks
019500071210     C*
019600071210     C* Aggancio la bolla relativa x il completamento dei dati
019700071210     C     KEYtas30      chain     titas30c
019800071210     C                   if        %found(titas30c)
019900071210     C*
020000071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
020100140306     C                   EVAL      wIND = tasIND
020200071213     C                   EVAL      wLOD = tasLOD
020300140306     C                   EVAL      wCAD = tasCAD
020400140306     C                   EVAL      wPRD = tasPRD
020500071213     C*
020600071213     C                   endif
020700071121     C*
020800071121     C* Compondo la stringa dati output relativa al record corrente
020900071121     C                   EXSR      wrifile
021000100910     C*
021100100910     C                   endif
021200040719     C*
021300040719     C* In ogni caso storicizzo e cancello l'input sempre
021400080924     C                   DELETE    TIVGD000
021500040719     C*
021600040719     C* Proseguo con la lettura dell'input
021700080924     C                   READ      TIVGDTMP
021800060119     C*
021900030325     C                   ENDDO
022000071121     C*
022100071121     C* Head finali della struttura HTML
022200071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
022300071210     C***                WRITE     TIVAW000
022400991027     C*
022500030325     C                   EVAL      wrkesito = '0'
022600991027     C*
022700910830     C                   ENDSR
022800100907
022900100907
023000100907
023100100907     C     calSTS        BEGSR
023200100910     C*
023300111223     C                   EVAL      wDataSts       = wrkDCM
023400100907     C*
023500100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
023600111223     C                   IF        wrkDCM > *zeros AND
023700111223     C                             (wrkCCA = *blanks OR wrkCCA = '1')
023800100907     C                   EVAL      wConsOkSiNo    = 'S'
023900111228     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
024000100907     C                   ELSE
024100100907     C                   EVAL      wConsOkSiNo    = 'N'
024200100907     C*
024300100907     C* A questo punto stabilisco la descrizione
024400100910     C                   SELECT
024500111223     C                   WHEN      wrkCCA = '1'
024600111228     C                   EVAL      wConsOkSiNoDes = 'Dirottamento'
024700111223     C                   WHEN      wrkCCA = '2'
024800111228     C                   EVAL      wConsOkSiNoDes = 'Reso al mittente'
024900100907     C                   EVAL      wConsOkSiNo    = 'R'
025000111223     C                   WHEN      wrkCCA = '3'
025100111228     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce'
025200111223     C                   WHEN      wrkCCA = '4'
025300111228     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
025400111223     C                   WHEN      wrkCCA = '5'
025500111228     C                   EVAL      wConsOkSiNoDes = 'Chiusura con anomalia'
025600111223     C                   WHEN      wrkCCA = '6'
025700111228     C                   EVAL      wConsOkSiNoDes = 'Reso al mittente su avaria'
025800111228     C                   WHEN      wrkCCA = '7'
025900111228     C                   EVAL      wConsOkSiNoDes = 'Merce mai affidata'
026000111223     C                   WHEN      wrkCCA = 'A'
026100111228     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica anomalia'
026200111223     C                   WHEN      wrkCCA = 'C'
026300111228     C                   EVAL      wConsOkSiNoDes = 'In consegna'
026400111223     C                   WHEN      wrkCCA = 'R'
026500111228     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata'
026600111223     C                   WHEN      wrkCCA = 'S'
026700111228     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
026800111228     C                   OTHER
026900111228     C                   EVAL      wConsOkSiNoDes = '*****'
027000100910     C                   ENDSL
027100100907     C*
027200111223     C                   IF        (vacDLA > wrkDCM AND vacDLA >  vacDAG)
027300100910     C                   EVAL      wDataSts       = vacDLA
027400111228     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
027500100907     C                   ENDIF
027600111223     C                   IF        (vacDAG > wrkDCM AND vacDAG >= vacDLA)
027700100910     C                   EVAL      wDataSts       = vacDAG
027800111228     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
027900100907     C                   ENDIF
028000111223     C                   IF        wrkDCM = *zeros AND
028100100910     C                             vacDLA = *zeros AND
028200100910     C                             vacDAG = *zeros AND
028300111223     C                             wrkCCA = *blanks
028400100910     C                   EVAL      wDataSts       = datcor
028500100910     C*
028600100910     C* Verifico la data consegna richiesta
028700100910     C                   IF        vacDCR > datcor
028800111228     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
028900111228     C                              UBFMTDATE_Convert(
029000111228     C                                  %editc(vacDCR:'4') :
029100111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029200100910     C                   ELSE
029300111228     C                   EVAL      wConsOkSiNoDes = 'In lavorazione'
029400100910     C                   ENDIF
029500100910     C                   ENDIF
029600100907     C*
029700100907     C                   ENDIF
029800100907     C*
029900100907     C                   ENDSR
030000071121
030100071121
030200071121
030300071121     C     wrifile       BEGSR
030400071121     C*
030500100907     C* Determino lo status della spedizione
030600100907     C                   EXSR      calSTS
030700100908     C*
030800100908     C                   SELECT
030900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
031000071121     C                   EVAL      wBufferOut = iRow +
031100111223     C                              iCol+%trim(%editc(vacCCM:'X'))        +fCol+
031200140306     C                              iCol+%trim(%editc(vacRMN:'Q'))        +fCol+
031300140306     C                              iCol+%trim(vacRMA)                    +fCol+
031400111228     C                              iCol+%trim(
031500111228     C                              UBFMTDATE_Convert(
031600111228     C                                  %editc(vacAAS*10000+vacMGS:'4') :
031700111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031800111228     C                                                                  ) +fCol+
031900111223     C                              iCol+%trim(vacRSD)                    +fCol+
032000140306     C                              iCol+%trim(wIND)                      +fCol+
032100140306     C                              iCol+%trim(wLOD)                      +fCol+
032200111223     C                              iCol+%trim(wCAD)                      +fCol+
032300140306     C                              iCol+%trim(wPRD)                      +fCol+
032400111223     C                              iCol+%trim(wConsOkSiNoDes)            +fCol+
032500111228     C                              iCol+%trim(
032600111228     C                              UBFMTDATE_Convert(
032700111228     C                                  %editc(wDataSts:'4') :
032800111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
032900111228     C                                                                  ) +fCol
033000100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
033100100908     C                   EVAL      wBufferOut =
033200100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
033300140306     C                              +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
033400140306     C                              +'"'+%trim(vacRMA)                    +'";'
033500111228     C                              +'"'+%trim(
033600111228     C                              UBFMTDATE_Convert(
033700111228     C                                  %editc(vacAAS*10000+vacMGS:'4') :
033800111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033900111228     C                                                                  ) +'";'
034000100910     C                              +'"'+%trim(vacRSD)                    +'";'
034100140306     C                              +'"'+%trim(wIND)                      +'";'
034200140306     C                              +'"'+%trim(wLOD)                      +'";'
034300100910     C                              +'"'+%trim(wCAD)                      +'";'
034400140306     C                              +'"'+%trim(wPRD)                      +'";'
034500100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
034600111228     C                              +'"'+%trim(
034700111228     C                              UBFMTDATE_Convert(
034800111228     C                                  %editc(wDataSts:'4') :
034900111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
035000111228     C                                                                  ) +'"'
035100100908     C                   ENDSL
035200071121     C*
035300071121     C                   EVAL      VAWDTA = wBufferOut
035400071121     C                   WRITE     TIVAW000
035500071121     C*
035600071121     C                   ENDSR
035700071121
035800071121
035900071121
036000991027
036100991027      /TITLE Operazioni iniziali.
036200991027     C     *inzsr        BEGSR
036300991027     C*
036400991027     C     *ENTRY        PLIST
036500991027     C                   parm                    prmppt
036600991027     C     wrkesito      parm      wrkesito      prmesito
036700100910     C*
036800100910     C* CALCOLA LA DATA CORRENTE
036900100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
037000100910     C                   eval      datcor = %dec(%date() : *ISO)
037100071210     C*
037200071213     C* TITAS30C - Parziale
037300071210     C     KEYtas30      KLIST
037400071210     C                   KFLD                    vacAAS
037500071210     C                   KFLD                    vacLNP
037600071210     C                   KFLD                    vacNRS
037700071210     C                   KFLD                    vacNSP
037800991027     C*
037900991027     C                   ENDSR
038000071121     C*
038100071121** DEC - COSTANTI
038200071121<?xml version="1.0"?>
038300071121<?mso-application progid="Excel.Sheet"?>
038400071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
038500071121xmlns:o="urn:schemas-microsoft-com:office:office"
038600071121xmlns:x="urn:schemas-microsoft-com:office:excel"
038700071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
038800071121xmlns:html="http://www.w3.org/TR/REC-html40">
038900071121</Workbook>
039000071121<Worksheet ss:Name="PAG1">
039100071121</Worksheet>
039200071121<Table border="1">
039300071121</Table>
039400071121<Row>
039500071121</Row>
039600071121<Cell><Data ss:Type="String">
039700071121<Cell><Data ss:Type="Number">
039800071121</Data></Cell>
039900071121
040000071121
040100071121
