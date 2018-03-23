000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000110519     FTITA430C  IF   E           K DISK
001100030924
001200080924     D fnvac00t      e ds
001300071121
001400071121     D*------------
001500071121     D* DECODIFICHE COSTANTI
001600071121     D*------------
001700071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001800071121
001900071121
002000071121     D*------------
002100071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002200071121     D*------------
002300071121     D iHtml           s             10    inz('<html>')
002400071121     D fHtml           s             10    inz('</html>')
002500071121     D iBody           s             10    inz('<body>')
002600071121     D fBody           s             10    inz('</body>')
002700071121     D iTable          s             20    inz('<table border="1">')
002800071121     D fTable          s             10    inz('</table>')
002900071121     D iThead          s             10    inz('<thead>')
003000071121     D fThead          s             10    inz('</thead>')
003100071121     D iRow            s              5    inz('<tr>')
003200071121     D fRow            s              5    inz('</tr>')
003300071121     D iCol            s              5    inz('<td>')
003400071121     D fCol            s              5    inz('</td>')
003500071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003600071121     D fColBold        s              5    inz('</th>')
003700071122     D cApi            s              1    inz('''')
003800071121
003900071121
004000071121     D*------------
004100071121     D* VARIABILI D WRK
004200071121     D*------------
004300030924     D prmppt          S             50
004400030924     D prmesito        S              1
004500030924     D wrkesito        S                   like(prmesito)
004600071122     D wBufferOut      s          10000    varying inz
004700100910     D wDataSts        S              8  0 inz(*zeros)
004800141128     D wOraSts         S              4  0 inz(*zeros)
004900100907     D wConsOkSiNo     S              1    inz(*blanks)
005000100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005100100907     D wLinkDir        S            300    inz(*blanks)
005200160727     D wDRT            s             10    inz(*blanks)
005300110519     D wDSP            s             10    inz(*blanks)
005400110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005500110519     D wIND            s                   inz(*blanks) like(tasIND)
005600110519     D wCAD            s                   inz(*blanks) like(tasIND)
005700110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005800110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005900071211     D*
006000100910
006100100910
006200100910     D*------------------
006300100910     D* LINKING A DEFINIZIONI ESTERNE
006400100910     D*------------------
006500100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006600100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006700100910
006800030924
006900030325
007000030924     C* MAIN/
007100071121     C*
007200071121     C* Verifico se già presenti dati nel file d output
007300071121     C                   EXSR      chkRECFILMBR
007400071121     C*
007500071121     C* Effettuo elaborazione
007600071121     C                   EXSR      traduci
007700071121     C*
007800921023     C                   SETON                                        LR
007900991027
008000071121
008100071121
008200071121     C     chkRECFILMBR  BEGSR
008300071121     C*
008400071121     C                   READ      TIVAWWWT                               55
008500071121     C*
008600071121     C                   ENDSR
008700071121
008800071121
008900030924
009000991027     C     traduci       BEGSR
009100071121     C*
009200071121     C* Head iniziali della struttura HTML
009300100908     C                   IF        %subst(prmppt:2:1) = 'H'
009400071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009500071210     C   55              WRITE     TIVAW000
009600100908     C                   ENDIF
009700071121     C*
009800071121     C* Inizializzazone formato e variabili x output
009900071121     C                   clear                   TIVAW000
010000071121     C                   clear                   wBufferOut
010100100907     C                   clear                   wLinkDir
010200100907     C                   clear                   wConsOkSiNo
010300100907     C                   clear                   wConsOkSiNoDes
010400071121     C*
010500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010700100908     C*
010800100908     C                   SELECT
010900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
011000071121     C                   EVAL      wBufferOut = iThead + iRow
011100110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
011200160727     C                             +iColBold+ 'LINEA DI PARTENZA'      +fColBold
011300160727     C                             +iColBold+ 'DATA RITIRO'            +fColBold
011400110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
011500110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
011600110519     C                             +iColBold+ 'RAG. SOC. DESTINATARIO' +fColBold
011700110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
011800110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
011900110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
012000110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
012100110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
012200110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
012300110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
012400131015     C                             +iColBold+ 'TOT. COLLI'             +fColBold
012500110519     C                             +iColBold+ 'STATO'                  +fColBold
012600110519     C                             +iColBold+ 'DATA STATO'             +fColBold
012700141128     C                             +iColBold+ 'ORA STATO'              +fColBold
012800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
012900100908     C                   EVAL      wBufferOut =
013000110519     C                              '"'+      'CODICE CLIENTE'         +'";'
013100160727     C                             +'"'+      'LINEA PARTENZA'         +'";'
013200160727     C                             +'"'+      'DATA RITIRO'            +'";'
013300110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
013400110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
013500110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
013600110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
013700110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
013800110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
013900110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
014000110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
014100110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
014200110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
014300131015     C                             +'"'+      'TOT. COLLI'             +'";'
014400110519     C                             +'"'+      'STATO'                  +'";'
014500110519     C                             +'"'+      'DATA STATO'             +'";'
014600141128     C                             +'"'+      'ORA STATO'              +'";'
014700100908     C                   ENDSL
014800100908     C*
014900071121     C                   EVAL      VAWDTA = wBufferOut
015000071121     C                   WRITE     TIVAW000
015100071121     C                   ENDIF
015200991027     C*
015300080924     C                   READ      TIVGDTMP
015400060119     C*
015500080924     C                   DOW       not %eof(TIVGDTMP)
015600080924     C*
015700080924     C                   eval      fnvac00t = vgdDTA
015800071121     C*
015900071121     C                   clear                   TIVAW000
016000071121     C                   clear                   wBufferOut
016100100907     C                   clear                   wLinkDir
016200100907     C                   clear                   wConsOkSiNo
016300100907     C                   clear                   wConsOkSiNoDes
016400100910     C*
016500100910     C* Solo bolle originali e valide ai fini della spedizione
016600100910     C                   movel     *blanks       wEsito1           1
016700100910     C*
016800100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
016900100910     C                                                vacAAS
017000100910     C                                               :vacLNP
017100100910     C                                               :vacNRS
017200100910     C                                               :vacNSP
017300100910     C                                               :pOutLblTyp
017400100910     C                                               :pOutAnnoBO
017500100910     C                                               :pOutLineaParBO
017600100910     C                                               :pOutSerieBO
017700100910     C                                               :pOutNumSpedBO
017800100910     C                                               :pOutDcmBO
017900100910     C                                               :pOutCcaBO
018000100910     C                                               :pOutRblBO))
018100100910     C*
018200100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
018300071210     C*
018400071210     C* Inizializzo campi d wrk
018500160727     C                   EVAL      wDRT = *blanks
018600110519     C                   EVAL      wDSP = *blanks
018700110519     C                   EVAL      wRD2 = *blanks
018800110519     C                   EVAL      wIND = *blanks
018900110519     C                   EVAL      wCAD = *blanks
019000110519     C                   EVAL      wLOD = *blanks
019100110519     C                   EVAL      wPRD = *blanks
019200071210     C*
019300071210     C* Aggancio la bolla relativa x il completamento dei dati
019400071210     C     KEYtas30      chain     titas30c
019500071210     C                   if        %found(titas30c)
019600110519     C*
019700110519     C                   eval      ta4TRC = 'D'
019800110519     C     KEYta430      chain     tita430c
019900110519     C                   if        %found(tita430c)
020000110519     C                   eval      wRD2 = '.' + ta4NOT
020100110519     C                   endif
020200071210     C*
020300071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
020400160727     C                   eval      wDRT = %editc(tasDRT:'X')
020500110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
020600110519     C                   eval      wIND = tasIND
020700110519     C                   eval      wCAD = tasCAD
020800110519     C                   eval      wLOD = tasLOD
020900110519     C                   eval      wPRD = tasPRD
021000071213     C*
021100071213     C                   endif
021200071121     C*
021300071121     C* Compondo la stringa dati output relativa al record corrente
021400071121     C                   EXSR      wrifile
021500100910     C*
021600100910     C                   endif
021700040719     C*
021800040719     C* In ogni caso storicizzo e cancello l'input sempre
021900080924     C                   DELETE    TIVGD000
022000040719     C*
022100040719     C* Proseguo con la lettura dell'input
022200080924     C                   READ      TIVGDTMP
022300060119     C*
022400030325     C                   ENDDO
022500071121     C*
022600071121     C* Head finali della struttura HTML
022700071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
022800071210     C***                WRITE     TIVAW000
022900991027     C*
023000030325     C                   EVAL      wrkesito = '0'
023100991027     C*
023200910830     C                   ENDSR
023300100907
023400100907
023500100907
023600100907     C     calSTS        BEGSR
023700100910     C*
023800120926     C                   MOVEL     *blanks       wMIC              1
023900121114     C                   MOVEL     *blanks       wGIAC             1
024000100910     C                   EVAL      wDataSts       = vacDCM
024100141128     C                   EVAL      wOraSts        = vacHMC
024200100907     C*
024300100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
024400100907     C                   IF        VACDCM > *zeros AND
024500100907     C                             (VACCCA = *blanks OR VACCCA = '1')
024600100907     C                   EVAL      wConsOkSiNo    = 'S'
024700110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
024800100907     C                   ELSE
024900100907     C                   EVAL      wConsOkSiNo    = 'N'
025000100907     C*
025100100907     C* A questo punto stabilisco la descrizione
025200100910     C                   SELECT
025300100910     C                   WHEN      vacCCA = '1'
025400110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
025500100910     C                   WHEN      vacCCA = '2'
025600110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
025700100907     C                   EVAL      wConsOkSiNo    = 'R'
025800100910     C                   WHEN      vacCCA = '3'
025900110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
026000110519     C                   EVAL      wConsOkSiNo    = 'R'
026100100910     C                   WHEN      vacCCA = '4'
026200110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
026300110519     C                   EVAL      wConsOkSiNo    = 'R'
026400100910     C                   WHEN      vacCCA = '5'
026500110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
026600110519     C                   EVAL      wConsOkSiNo    = 'R'
026700100910     C                   WHEN      vacCCA = '6'
026800110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
026900110519     C                   EVAL      wConsOkSiNo    = 'R'
027000100910     C                   WHEN      vacCCA = 'A'
027100110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
027200110519     C                   EVAL      wConsOkSiNo    = 'R'
027300100910     C                   WHEN      vacCCA = 'C'
027400110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
027500120926     C                   EVAL      wMIC = 'S'
027600100910     C                   WHEN      vacCCA = 'R'
027700110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
027800100910     C                   WHEN      vacCCA = 'S'
027900110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
028000110519     C                   EVAL      wConsOkSiNo    = 'R'
028100100910     C                   ENDSL
028200100907     C*
028300100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
028400100910     C                   EVAL      wDataSts       = vacDLA
028500110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
028600100907     C                   ENDIF
028700100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
028800100910     C                   EVAL      wDataSts       = vacDAG
028900110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
029000121114     C                   EVAL      wGIAC = 'S'
029100100907     C                   ENDIF
029200100910     C                   IF        vacDCM = *zeros AND
029300100910     C                             vacDLA = *zeros AND
029400100910     C                             vacDAG = *zeros AND
029500100910     C                             vacCCA = *blanks
029600100910     C                   EVAL      wDataSts       = datcor
029700100910     C*
029800100910     C* Verifico la data consegna richiesta
029900100910     C                   IF        vacDCR > datcor
030000110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
030100110519     C                                              'data ' + %editc(vacDCR:'X')
030200100910     C                   ELSE
030300110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
030400100910     C                   ENDIF
030500100910     C                   ENDIF
030600100907     C*
030700100907     C                   ENDIF
030800100907     C*
030900100907     C                   ENDSR
031000100907
031100100907
031200100907
031300100907     C     makLNK        BEGSR
031400100907     C*
031500160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/' +
031600100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
031700100907     C                                        %trim(%char(vacRMN))             +
031800100907     C                                        '&ksu=0895741&lang=fr'
031900100907     C*
032000100907     C                   ENDSR
032100100907
032200071121
032300071121
032400071121
032500071121     C     wrifile       BEGSR
032600120926     C*
032700120926     C                   SETOFF                                       31
032800071121     C*
032900100907     C* Determino lo status della spedizione
033000100907     C                   EXSR      calSTS
033100100907     C*
033200100907     C* Costruisco il link diretto
033300110519     C***                EXSR      makLNK
033400100908     C*
033500100908     C                   SELECT
033600100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
033700071121     C                   EVAL      wBufferOut = iRow +
033800100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
033900160727     C                                  iCol+%trim(%editc(vacLNP:'X'))    +fCol+
034000160727     C                                  iCol+%trim(wDRT)                  +fCol+
034100110519     C                                  iCol+%trim(wDSP)                  +fCol+
034200110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
034300100907     C                                  iCol+%trim(vacRSD)                +fCol+
034400110519     C                                  iCol+%trim(wRD2)                  +fCol+
034500110519     C                                  iCol+%trim(wIND)                  +fCol+
034600100517     C                                  iCol+%trim(wCAD)                  +fCol+
034700100517     C                                  iCol+%trim(wLOD)                  +fCol+
034800110519     C                                  iCol+%trim(wPRD)                  +fCol+
034900100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
035000110519     C                                  iCol+%trim(vacRMA)                +fCol+
035100131015     C                                  iCol+%trim(%editc(vacNCL:'X'))    +fCol+
035200100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
035300141128     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
035400141128     C                                  iCol+%trim(%editc(wOraSts:'X'))   +fCol
035500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
035600100908     C                   EVAL      wBufferOut =
035700110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
035800160727     C                              +'"'+   %trim(%editc(vacLNP:'X'))     +'";'
035900160727     C                              +'"'+   %trim(wDRT)                   +'";'
036000110519     C                              +'"'+   %trim(wDSP)                   +'";'
036100110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
036200110519     C                              +'"'+   %trim(vacRSD)                 +'";'
036300110519     C                              +'"'+   %trim(wRD2)                   +'";'
036400110519     C                              +'"'+   %trim(wIND)                   +'";'
036500110519     C                              +'"'+   %trim(wCAD)                   +'";'
036600110519     C                              +'"'+   %trim(wLOD)                   +'";'
036700110519     C                              +'"'+   %trim(wPRD)                   +'";'
036800110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
036900110519     C                              +'"'+   %trim(vacRMA)                 +'";'
037000131015     C                              +'"'+   %trim(%editc(vacNCL:'X'))     +'";'
037100110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
037200110519     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
037300141128     C                              +'"'+   %trim(%editc(wOraSts:'X'))    +'";'
037400100908     C                   ENDSL
037500071121     C*
037600120926     C* Se nei parametri è richiesto no MIC => gestisco
037700120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
037800120926     C                   SETON                                        31
037900120926     C                   ENDIF
038000121114     C*
038100121114     C* Se nei parametri è richiesto solo GIACIENZE => gestisco
038200121114     C                   IF        %subst(prmppt:4:1) = 'S' and wGIAC <> 'S'
038300121114     C                   SETON                                        31
038400121114     C                   ENDIF
038500120926     C*
038600071121     C                   EVAL      VAWDTA = wBufferOut
038700110519     C*
038800110519     C* Restituisco solo le spedizioni "pendenti"
038900110713     C***                IF        wConsOkSiNo = 'N'
039000120926     C  N31              WRITE     TIVAW000
039100110713     C***                ENDIF
039200071121     C*
039300071121     C                   ENDSR
039400071121
039500071121
039600071121
039700991027
039800991027      /TITLE Operazioni iniziali.
039900991027     C     *inzsr        BEGSR
040000991027     C*
040100991027     C     *ENTRY        PLIST
040200991027     C                   parm                    prmppt
040300991027     C     wrkesito      parm      wrkesito      prmesito
040400100910     C*
040500100910     C* CALCOLA LA DATA CORRENTE
040600100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
040700100910     C                   eval      datcor = %dec(%date() : *ISO)
040800071210     C*
040900071213     C* TITAS30C - Parziale
041000071210     C     KEYtas30      KLIST
041100071210     C                   KFLD                    vacAAS
041200071210     C                   KFLD                    vacLNP
041300071210     C                   KFLD                    vacNRS
041400071210     C                   KFLD                    vacNSP
041500110519     C*
041600110519     C* TITA430C - Completa
041700110519     C     KEYta430      KLIST
041800110519     C                   KFLD                    vacAAS
041900110519     C                   KFLD                    vacLNP
042000110519     C                   KFLD                    vacNRS
042100110519     C                   KFLD                    vacNSP
042200110519     C                   KFLD                    ta4TRC
042300991027     C*
042400991027     C                   ENDSR
042500071121     C*
042600071121** DEC - COSTANTI
042700071121<?xml version="1.0"?>
042800071121<?mso-application progid="Excel.Sheet"?>
042900071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
043000071121xmlns:o="urn:schemas-microsoft-com:office:office"
043100071121xmlns:x="urn:schemas-microsoft-com:office:excel"
043200071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
043300071121xmlns:html="http://www.w3.org/TR/REC-html40">
043400071121</Workbook>
043500071121<Worksheet ss:Name="PAG1">
043600071121</Worksheet>
043700071121<Table border="1">
043800071121</Table>
043900071121<Row>
044000071121</Row>
044100071121<Cell><Data ss:Type="String">
044200071121<Cell><Data ss:Type="Number">
044300071121</Data></Cell>
044400071121
044500071121
044600071121
