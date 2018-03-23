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
004900100517     D wCAD            s              9    inz(*blanks)
005000071210     D wLOD            s             35    inz(*blanks)
005100071211     D*
005200100910
005300100910
005400100910     D*------------------
005500100910     D* LINKING A DEFINIZIONI ESTERNE
005600100910     D*------------------
005700100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005800100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005900111228     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006000111228     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006100100910
006200030924
006300030325
006400030924     C* MAIN/
006500071121     C*
006600071121     C* Verifico se già presenti dati nel file d output
006700071121     C                   EXSR      chkRECFILMBR
006800071121     C*
006900071121     C* Effettuo elaborazione
007000071121     C                   EXSR      traduci
007100071121     C*
007200921023     C                   SETON                                        LR
007300991027
007400071121
007500071121
007600071121     C     chkRECFILMBR  BEGSR
007700071121     C*
007800071121     C                   READ      TIVAWWWT                               55
007900071121     C*
008000071121     C                   ENDSR
008100071121
008200071121
008300030924
008400991027     C     traduci       BEGSR
008500071121     C*
008600071121     C* Head iniziali della struttura HTML
008700100908     C                   IF        %subst(prmppt:2:1) = 'H'
008800071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
008900071210     C   55              WRITE     TIVAW000
009000100908     C                   ENDIF
009100071121     C*
009200071121     C* Inizializzazone formato e variabili x output
009300071121     C                   clear                   TIVAW000
009400071121     C                   clear                   wBufferOut
009500100907     C                   clear                   wConsOkSiNo
009600100907     C                   clear                   wConsOkSiNoDes
009700071121     C*
009800071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009900071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010000100908     C*
010100100908     C                   SELECT
010200100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
010300071121     C                   EVAL      wBufferOut = iThead + iRow
010400111228     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
010500111228     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
010600111228     C                             +iColBold+ 'DESTINATARIO'           +fColBold
010700111228     C                             +iColBold+ 'CAP'                    +fColBold
010800111228     C                             +iColBold+ 'LOCALITA'               +fColBold
010900111228     C                             +iColBold+ 'RIF. MITTENTE'          +fColBold
011000111228     C                             +iColBold+ 'RIF. MITTENTE ALFA'     +fColBold
011100111228     C                             +iColBold+ 'STATO'                  +fColBold
011200111228     C                             +iColBold+ 'DATA STATO'             +fColBold
011300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
011400100908     C                   EVAL      wBufferOut =
011500111228     C                              '"'+'CODICE CLIENTE'         +'";'
011600111228     C                             +'"'+'DATA SPEDIZIONE'        +'";'
011700111228     C                             +'"'+'DESTINATARIO'           +'";'
011800111228     C                             +'"'+'CAP'                    +'";'
011900111228     C                             +'"'+'LOCALITA'               +'";'
012000111228     C                             +'"'+'RIF. MITTENTE'          +'";'
012100111228     C                             +'"'+'RIF. MITTENTE ALFA'     +'";'
012200111228     C                             +'"'+'STATO'                  +'";'
012300111228     C                             +'"'+'DATA STATO'             +'"'
012400100908     C                   ENDSL
012500100908     C*
012600071121     C                   EVAL      VAWDTA = wBufferOut
012700071121     C                   WRITE     TIVAW000
012800071121     C                   ENDIF
012900991027     C*
013000080924     C                   READ      TIVGDTMP
013100060119     C*
013200080924     C                   DOW       not %eof(TIVGDTMP)
013300080924     C*
013400080924     C                   eval      fnvac00t = vgdDTA
013500071121     C*
013600071121     C                   clear                   TIVAW000
013700071121     C                   clear                   wBufferOut
013800100907     C                   clear                   wConsOkSiNo
013900100907     C                   clear                   wConsOkSiNoDes
014000100910     C*
014100100910     C* Solo bolle originali e valide ai fini della spedizione
014200100910     C                   movel     *blanks       wEsito1           1
014300111223     C                   movel     *blanks       wEsito2           1
014400111223     C                   z-add     *zeros        wrkDCM            8 0
014500111223     C                   movel     *blanks       wrkCCA            1
014600100910     C*
014700100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
014800100910     C                                                vacAAS
014900100910     C                                               :vacLNP
015000100910     C                                               :vacNRS
015100100910     C                                               :vacNSP
015200100910     C                                               :pOutLblTyp
015300100910     C                                               :pOutAnnoBO
015400100910     C                                               :pOutLineaParBO
015500100910     C                                               :pOutSerieBO
015600100910     C                                               :pOutNumSpedBO
015700100910     C                                               :pOutDcmBO
015800100910     C                                               :pOutCcaBO
015900100910     C                                               :pOutRblBO))
016000100910     C*
016100100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
016200111223     C*
016300111223     C* Chiamata metodo GetLastChild
016400111223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
016500111223     C                                                pOutAnnoBO
016600111223     C                                               :pOutLineaParBO
016700111223     C                                               :pOutSerieBO
016800111223     C                                               :pOutNumSpedBO
016900111223     C                                               :pOutAnnoFI
017000111223     C                                               :pOutLineaParFI
017100111223     C                                               :pOutSerieFI
017200111223     C                                               :pOutNumSpedFI
017300111223     C                                               :pOutDcmFI
017400111223     C                                               :pOutCcaFI))
017500111223     C*
017600111223     C                   if        wEsito2 = '0'
017700111223     C                   eval      wrkDCM = pOutDcmFI
017800111223     C                   eval      wrkCCA = pOutCcaFI
017900111223     C                   else
018000111223     C                   eval      wrkDCM = pOutDcmBO
018100111223     C                   eval      wrkCCA = pOutCcaBO
018200111223     C                   endif
018300071210     C*
018400071210     C* Inizializzo campi d wrk
018500100517     C                   EVAL      wCAD = *blanks
018600071213     C                   EVAL      wLOD = *blanks
018700071210     C*
018800071210     C* Aggancio la bolla relativa x il completamento dei dati
018900071210     C     KEYtas30      chain     titas30c
019000071210     C                   if        %found(titas30c)
019100071210     C*
019200071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
019300100517     C                   EVAL      wCAD = tasCAD
019400071213     C                   EVAL      wLOD = tasLOD
019500071213     C*
019600071213     C                   endif
019700071121     C*
019800071121     C* Compondo la stringa dati output relativa al record corrente
019900071121     C                   EXSR      wrifile
020000100910     C*
020100100910     C                   endif
020200040719     C*
020300040719     C* In ogni caso storicizzo e cancello l'input sempre
020400080924     C                   DELETE    TIVGD000
020500040719     C*
020600040719     C* Proseguo con la lettura dell'input
020700080924     C                   READ      TIVGDTMP
020800060119     C*
020900030325     C                   ENDDO
021000071121     C*
021100071121     C* Head finali della struttura HTML
021200071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
021300071210     C***                WRITE     TIVAW000
021400991027     C*
021500030325     C                   EVAL      wrkesito = '0'
021600991027     C*
021700910830     C                   ENDSR
021800100907
021900100907
022000100907
022100100907     C     calSTS        BEGSR
022200100910     C*
022300111223     C                   EVAL      wDataSts       = wrkDCM
022400100907     C*
022500100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
022600111223     C                   IF        wrkDCM > *zeros AND
022700111223     C                             (wrkCCA = *blanks OR wrkCCA = '1')
022800100907     C                   EVAL      wConsOkSiNo    = 'S'
022900111228     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
023000100907     C                   ELSE
023100100907     C                   EVAL      wConsOkSiNo    = 'N'
023200100907     C*
023300100907     C* A questo punto stabilisco la descrizione
023400100910     C                   SELECT
023500111223     C                   WHEN      wrkCCA = '1'
023600111228     C                   EVAL      wConsOkSiNoDes = 'Dirottamento'
023700111223     C                   WHEN      wrkCCA = '2'
023800111228     C                   EVAL      wConsOkSiNoDes = 'Reso al mittente'
023900100907     C                   EVAL      wConsOkSiNo    = 'R'
024000111223     C                   WHEN      wrkCCA = '3'
024100111228     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce'
024200111223     C                   WHEN      wrkCCA = '4'
024300111228     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
024400111223     C                   WHEN      wrkCCA = '5'
024500111228     C                   EVAL      wConsOkSiNoDes = 'Chiusura con anomalia'
024600111223     C                   WHEN      wrkCCA = '6'
024700111228     C                   EVAL      wConsOkSiNoDes = 'Reso al mittente su avaria'
024800111228     C                   WHEN      wrkCCA = '7'
024900111228     C                   EVAL      wConsOkSiNoDes = 'Merce mai affidata'
025000111223     C                   WHEN      wrkCCA = 'A'
025100111228     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica anomalia'
025200111223     C                   WHEN      wrkCCA = 'C'
025300111228     C                   EVAL      wConsOkSiNoDes = 'In consegna'
025400111223     C                   WHEN      wrkCCA = 'R'
025500111228     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata'
025600111223     C                   WHEN      wrkCCA = 'S'
025700111228     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
025800111228     C                   OTHER
025900111228     C                   EVAL      wConsOkSiNoDes = '*****'
026000100910     C                   ENDSL
026100100907     C*
026200111223     C                   IF        (vacDLA > wrkDCM AND vacDLA >  vacDAG)
026300100910     C                   EVAL      wDataSts       = vacDLA
026400111228     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
026500100907     C                   ENDIF
026600111223     C                   IF        (vacDAG > wrkDCM AND vacDAG >= vacDLA)
026700100910     C                   EVAL      wDataSts       = vacDAG
026800111228     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
026900100907     C                   ENDIF
027000111223     C                   IF        wrkDCM = *zeros AND
027100100910     C                             vacDLA = *zeros AND
027200100910     C                             vacDAG = *zeros AND
027300111223     C                             wrkCCA = *blanks
027400100910     C                   EVAL      wDataSts       = datcor
027500100910     C*
027600100910     C* Verifico la data consegna richiesta
027700100910     C                   IF        vacDCR > datcor
027800111228     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
027900111228     C                              UBFMTDATE_Convert(
028000111228     C                                  %editc(vacDCR:'4') :
028100111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028200100910     C                   ELSE
028300111228     C                   EVAL      wConsOkSiNoDes = 'In lavorazione'
028400100910     C                   ENDIF
028500100910     C                   ENDIF
028600100907     C*
028700100907     C                   ENDIF
028800100907     C*
028900100907     C                   ENDSR
029000071121
029100071121
029200071121
029300071121     C     wrifile       BEGSR
029400071121     C*
029500100907     C* Determino lo status della spedizione
029600100907     C                   EXSR      calSTS
029700100908     C*
029800100908     C                   SELECT
029900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
030000071121     C                   EVAL      wBufferOut = iRow +
030100111223     C                              iCol+%trim(%editc(vacCCM:'X'))        +fCol+
030200111228     C                              iCol+%trim(
030300111228     C                              UBFMTDATE_Convert(
030400111228     C                                  %editc(vacAAS*10000+vacMGS:'4') :
030500111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
030600111228     C                                                                  ) +fCol+
030700111223     C                              iCol+%trim(vacRSD)                    +fCol+
030800111223     C                              iCol+%trim(wCAD)                      +fCol+
030900111223     C                              iCol+%trim(wLOD)                      +fCol+
031000111223     C                              iCol+%trim(%editc(vacRMN:'Q'))        +fCol+
031100111223     C                              iCol+%trim(vacRMA)                    +fCol+
031200111223     C                              iCol+%trim(wConsOkSiNoDes)            +fCol+
031300111228     C                              iCol+%trim(
031400111228     C                              UBFMTDATE_Convert(
031500111228     C                                  %editc(wDataSts:'4') :
031600111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031700111228     C                                                                  ) +fCol
031800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
031900100908     C                   EVAL      wBufferOut =
032000100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
032100111228     C                              +'"'+%trim(
032200111228     C                              UBFMTDATE_Convert(
032300111228     C                                  %editc(vacAAS*10000+vacMGS:'4') :
032400111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
032500111228     C                                                                  ) +'";'
032600100910     C                              +'"'+%trim(vacRSD)                    +'";'
032700100910     C                              +'"'+%trim(wCAD)                      +'";'
032800100910     C                              +'"'+%trim(wLOD)                      +'";'
032900100910     C                              +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
033000111221     C                              +'"'+%trim(vacRMA)                    +'";'
033100100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
033200111228     C                              +'"'+%trim(
033300111228     C                              UBFMTDATE_Convert(
033400111228     C                                  %editc(wDataSts:'4') :
033500111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033600111228     C                                                                  ) +'"'
033700100908     C                   ENDSL
033800071121     C*
033900071121     C                   EVAL      VAWDTA = wBufferOut
034000071121     C                   WRITE     TIVAW000
034100071121     C*
034200071121     C                   ENDSR
034300071121
034400071121
034500071121
034600991027
034700991027      /TITLE Operazioni iniziali.
034800991027     C     *inzsr        BEGSR
034900991027     C*
035000991027     C     *ENTRY        PLIST
035100991027     C                   parm                    prmppt
035200991027     C     wrkesito      parm      wrkesito      prmesito
035300100910     C*
035400100910     C* CALCOLA LA DATA CORRENTE
035500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
035600100910     C                   eval      datcor = %dec(%date() : *ISO)
035700071210     C*
035800071213     C* TITAS30C - Parziale
035900071210     C     KEYtas30      KLIST
036000071210     C                   KFLD                    vacAAS
036100071210     C                   KFLD                    vacLNP
036200071210     C                   KFLD                    vacNRS
036300071210     C                   KFLD                    vacNSP
036400991027     C*
036500991027     C                   ENDSR
036600071121     C*
036700071121** DEC - COSTANTI
036800071121<?xml version="1.0"?>
036900071121<?mso-application progid="Excel.Sheet"?>
037000071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
037100071121xmlns:o="urn:schemas-microsoft-com:office:office"
037200071121xmlns:x="urn:schemas-microsoft-com:office:excel"
037300071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
037400071121xmlns:html="http://www.w3.org/TR/REC-html40">
037500071121</Workbook>
037600071121<Worksheet ss:Name="PAG1">
037700071121</Worksheet>
037800071121<Table border="1">
037900071121</Table>
038000071121<Row>
038100071121</Row>
038200071121<Cell><Data ss:Type="String">
038300071121<Cell><Data ss:Type="Number">
038400071121</Data></Cell>
038500071121
038600071121
038700071121
