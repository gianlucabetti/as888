000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700130417     D fnvab00t      e ds
000800071121
000900071121     D*------------
001000071121     D* DECODIFICHE COSTANTI
001100071121     D*------------
001200071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001300071121
001400071121
001500071121     D*------------
001600071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
001700071121     D*------------
001800071121     D iHtml           s             10    inz('<html>')
001900071121     D fHtml           s             10    inz('</html>')
002000071121     D iBody           s             10    inz('<body>')
002100071121     D fBody           s             10    inz('</body>')
002200071121     D iTable          s             20    inz('<table border="1">')
002300071121     D fTable          s             10    inz('</table>')
002400071121     D iThead          s             10    inz('<thead>')
002500071121     D fThead          s             10    inz('</thead>')
002600071121     D iRow            s              5    inz('<tr>')
002700071121     D fRow            s              5    inz('</tr>')
002800071121     D iCol            s              5    inz('<td>')
002900071121     D fCol            s              5    inz('</td>')
003000071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003100071121     D fColBold        s              5    inz('</th>')
003200071122     D cApi            s              1    inz('''')
003300071121
003400071121
003500071121     D*------------
003600071121     D* VARIABILI D WRK
003700071121     D*------------
003800030924     D prmppt          S             50
003900030924     D prmesito        S              1
004000030924     D wrkesito        S                   like(prmesito)
004100071122     D wBufferOut      s          10000    varying inz
004200100910     D wDataSts        S              8  0 inz(*zeros)
004300100907     D wConsOkSiNo     S              1    inz(*blanks)
004400100907     D wConsOkSiNoDes  S            300    inz(*blanks)
004500100907     D wLinkDir        S            300    inz(*blanks)
004600130417     D wDSP            s             10    inz(*blanks)
004700130409     D wKSC            s              7  0 inz(*zeros)
004800071211     D*
004900100910
005000130409
005100130409     D*-------------------
005200130409     D* COSTANTI
005300130409     D*-------------------
005400130409     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005500130409     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005600130409
005700030924
005800030325
005900030924     C* MAIN/
006000071121     C*
006100071121     C* Verifico se già presenti dati nel file d output
006200071121     C                   EXSR      chkRECFILMBR
006300071121     C*
006400071121     C* Effettuo elaborazione
006500071121     C                   EXSR      traduci
006600071121     C*
006700921023     C                   SETON                                        LR
006800991027
006900071121
007000071121
007100071121     C     chkRECFILMBR  BEGSR
007200071121     C*
007300071121     C                   READ      TIVAWWWT                               55
007400071121     C*
007500071121     C                   ENDSR
007600071121
007700071121
007800030924
007900991027     C     traduci       BEGSR
008000071121     C*
008100071121     C* Head iniziali della struttura HTML
008200100908     C                   IF        %subst(prmppt:2:1) = 'H'
008300071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
008400071210     C   55              WRITE     TIVAW000
008500100908     C                   ENDIF
008600071121     C*
008700071121     C* Inizializzazone formato e variabili x output
008800071121     C                   clear                   TIVAW000
008900071121     C                   clear                   wBufferOut
009000100907     C                   clear                   wLinkDir
009100100907     C                   clear                   wConsOkSiNo
009200100907     C                   clear                   wConsOkSiNoDes
009300130409     C*
009400130409     C* Verifico la lingua richiesta
009500130409     C                   setoff                                       505152
009600130409     C                   select
009700130409     C                   when      %subst(prmppt:1:1) = *blanks
009800130409     C                   setoff                                       50
009900130409     C                   when      %subst(prmppt:1:1) = 'I'
010000130409     C                   seton                                        50
010100130409     C                   seton                                        51
010200130409     C                   when      %subst(prmppt:1:1) = 'E'
010300130409     C                   seton                                        50
010400130409     C                   seton                                        52
010500130409     C                   endsl
010600071121     C*
010700071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010800130409     C                   IF        *in50 AND *IN55 = *ON
010900100908     C*
011000100908     C                   SELECT
011100100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
011200130409     C   51              EVAL      wBufferOut = iThead + iRow
011300130409     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
011400130409 xxx C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
011500130409     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
011600130409     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
011700130409     C                             +iColBold+ 'STATO'                  +fColBold
011800130409 xxx C                             +iColBold+ 'DATA STATO'             +fColBold
011900130409     C                             +iColBold+ 'LINEA PARTENZA'         +fColBold
012000110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
012100121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
012200130409 xxx C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
012300110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
012400110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
012500110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
012600110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
012700121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
012800121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
012900121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
013000121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
013100121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
013200130409     C                             +iColBold+ 'LINK'                   +fColBold
013300130409     C   52              EVAL      wBufferOut = iThead + iRow
013400130409     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
013500130409 xxx C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
013600130409     C                             +iColBold+ '1 SHIPMENT REFERENCE'   +fColBold
013700130409     C                             +iColBold+ '2 SHIPMENT REFERENCE'   +fColBold
013800130409     C                             +iColBold+ 'STATUS'                 +fColBold
013900130409 xxx C                             +iColBold+ 'STATUS DATE'            +fColBold
014000130409     C                             +iColBold+ 'DEPARTURE DEPOT'        +fColBold
014100130409     C                             +iColBold+ 'ARRIVAL DEPOT'          +fColBold
014200130409     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
014300130409 xxx C                             +iColBold+ 'CONSIGNEE PHONE'        +fColBold
014400130409     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
014500130409     C                             +iColBold+ 'CONSIGNEE ZIPCODE'      +fColBold
014600130409     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
014700130409     C                             +iColBold+ 'CONSIGNEE AREA'         +fColBold
014800130409     C                             +iColBold+ 'ORIGINAL SENDER NAME'   +fColBold
014900130409     C                             +iColBold+ 'ORIGINAL SENDER ADDRESS'+fColBold
015000130409     C                             +iColBold+ 'ORIGINAL SENDER ZIPCODE'+fColBold
015100130409     C                             +iColBold+ 'ORIGINAL SENDER CITY'   +fColBold
015200130409     C                             +iColBold+ 'ORIGINAL SENDER AREA'   +fColBold
015300130409     C                             +iColBold+ 'LINK'                   +fColBold
015400100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015500130409     C   51              EVAL      wBufferOut =
015600130409     C                              '"'+      'CODICE CLIENTE'         +'";'
015700130409 xxx C                             +'"'+      'DATA SPEDIZIONE'        +'";'
015800130409     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
015900130409     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
016000130409     C                             +'"'+      'STATO'                  +'";'
016100130409 xxx C                             +'"'+      'DATA STATO'             +'";'
016200130409     C                             +'"'+      'LINEA PARTENZA'         +'";'
016300110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
016400110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
016500130409 xxx C                             +'"'+      'TEL. DESTINATARIO'      +'";'
016600110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
016700110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
016800110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
016900110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
017000121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
017100121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
017200121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
017300121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
017400121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
017500130409     C                             +'"'+      'LINK'                   +'";'
017600130409     C   52              EVAL      wBufferOut =
017700130409     C                              '"'+      'CUSTOMER CODE'          +'";'
017800130409 xxx C                             +'"'+      'SHIPMENT DATE'          +'";'
017900130409     C                             +'"'+      '1 SHIPMENT REFERENCE'   +'";'
018000130409     C                             +'"'+      '2 SHIPMENT REFERENCE'   +'";'
018100130409     C                             +'"'+      'STATUS'                 +'";'
018200130409 xxx C                             +'"'+      'STATUS DATE'            +'";'
018300130409     C                             +'"'+      'DEPARTURE DEPOT'        +'";'
018400130409     C                             +'"'+      'ARRIVAL DEPOT'          +'";'
018500130409     C                             +'"'+      'CONSIGNEE NAME'         +'";'
018600130409 xxx C                             +'"'+      'CONSIGNEE PHONE'        +'";'
018700130409     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
018800130409     C                             +'"'+      'CONSIGNEE ZIPCODE'      +'";'
018900130409     C                             +'"'+      'CONSIGNEE CITY'         +'";'
019000130409     C                             +'"'+      'CONSIGNEE AREA'         +'";'
019100130409     C                             +'"'+      'ORIGINAL SENDER NAME'   +'";'
019200130409     C                             +'"'+      'ORIGINAL SENDER ADDRESS'+'";'
019300130409     C                             +'"'+      'ORIGINAL SENDER ZIPCODE'+'";'
019400130409     C                             +'"'+      'ORIGINAL SENDER CITY'   +'";'
019500130409     C                             +'"'+      'ORIGINAL SENDER AREA'   +'";'
019600130409     C                             +'"'+      'LINK'                   +'";'
019700100908     C                   ENDSL
019800100908     C*
019900071121     C                   EVAL      VAWDTA = wBufferOut
020000071121     C                   WRITE     TIVAW000
020100071121     C                   ENDIF
020200991027     C*
020300080924     C                   READ      TIVGDTMP
020400060119     C*
020500080924     C                   DOW       not %eof(TIVGDTMP)
020600080924     C*
020700130417     C                   eval      fnvab00t = vgdDTA
020800071121     C*
020900071121     C                   clear                   TIVAW000
021000071121     C                   clear                   wBufferOut
021100100907     C                   clear                   wLinkDir
021200100907     C                   clear                   wConsOkSiNo
021300100907     C                   clear                   wConsOkSiNoDes
021400071210     C*
021500130417     C* Costruisco la data spedizione
021600130417     C                   eval      wDSP = %editc((vabAAS*10000+vabMGS):'X')
021700071121     C*
021800071121     C* Compondo la stringa dati output relativa al record corrente
021900071121     C                   EXSR      wrifile
022000040719     C*
022100040719     C* In ogni caso storicizzo e cancello l'input sempre
022200080924     C                   DELETE    TIVGD000
022300040719     C*
022400040719     C* Proseguo con la lettura dell'input
022500080924     C                   READ      TIVGDTMP
022600060119     C*
022700030325     C                   ENDDO
022800071121     C*
022900071121     C* Head finali della struttura HTML
023000071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
023100071210     C***                WRITE     TIVAW000
023200991027     C*
023300030325     C                   EVAL      wrkesito = '0'
023400991027     C*
023500910830     C                   ENDSR
023600100907
023700100907
023800100907
023900100907     C     calSTS        BEGSR
024000100910     C*
024100130417     C                   EVAL      wDataSts       = datcor
024200100907     C*
024300100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
024400130417     C                   EVAL      wConsOkSiNo    = 'N'
024500130417     C   51              EVAL      wConsOkSiNoDes =
024600130417     C                                  'In attesa di affidamento merce'
024700130417     C   52              EVAL      wConsOkSiNoDes =
024800130417     C                                  'Waiting for entrusting goods'
024900100907     C*
025000100907     C                   ENDSR
025100130409
025200130409
025300130409
025400130409     C     makLNK        BEGSR
025500130409     C*
025600130409     C* Se richiesta inclusione link T&T
025700130409     C                   if        %subst(prmppt:6:7) <> *blanks
025800130409     C*
025900130409     C* Porto in minuscolo il parametro relativo alla lingua del link
026000130409     C                   movel     *blanks       parLang           2
026100130409     C                   eval      parLang = %subst(prmppt:13:2)
026200130409     C     maiu:minu     xlate     parLang       parLang
026300130409     C*
026400160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
026500130409     C                                        'sped_ricdocmit_load.hsm?docmit='+
026600130417     C                                        %trim(%char(vabRMN))             +
026700130409     C                                        '&ksu='+%subst(prmppt:6:7)+
026800130409     C                                        '&lang='+parLang
026900130409     C*
027000130409     C                   endif
027100130409     C*
027200130409     C                   ENDSR
027300100907
027400071121
027500071121
027600071121
027700071121     C     wrifile       BEGSR
027800120926     C*
027900120926     C                   SETOFF                                       31
028000071121     C*
028100100907     C* Determino lo status della spedizione
028200100907     C                   EXSR      calSTS
028300100907     C*
028400100907     C* Costruisco il link diretto
028500130417     C***                EXSR      makLNK
028600130409     C*
028700130409     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
028800130409     C                   IF        %subst(prmppt:15:1) = 'P'
028900130417     C                   MOVE      vabCCM        CCM               4 0
029000130409     C                   MOVE      wKSC          KSC               4 0
029100130409     C                   IF        CCM =  8888 or  CCM =  9999
029200130409     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
029300130417     C                   EVAL      vabCCM = wKSC
029400130409     C                   ENDIF
029500130409     C                   ENDIF
029600130409     C                   ENDIF
029700100908     C*
029800100908     C                   SELECT
029900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
030000071121     C                   EVAL      wBufferOut = iRow +
030100130417     C                                  iCol+%trim(%editc(vabCCM:'X'))    +fCol+
030200130409 xxx C                                  iCol+%trim(wDSP)                  +fCol+
030300130417     C                                  iCol+%trim(%editc(vabRMN:'Q'))    +fCol+
030400130417     C                                  iCol+%trim(vabRMA)                +fCol+
030500130409     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
030600130409 xxx C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
030700130417     C                                  iCol+%trim(%editc(vabLNP:'X'))    +fCol+
030800130417     C                                  iCol+%trim(%editc(vabLNA:'X'))    +fCol+
030900130417     C                                  iCol+%trim(vabRSD)                +fCol+
031000130409 xxx C                                  iCol+%trim(' ')                   +fCol+
031100130417     C                                  iCol+%trim(vabIND)                +fCol+
031200130417     C                                  iCol+%trim(vabCAD)                +fCol+
031300130417     C                                  iCol+%trim(vabLOD)                +fCol+
031400130417     C                                  iCol+%trim(vabPRD)                +fCol+
031500130417     C                                  iCol+%trim(vabRMO)                +fCol+
031600130417     C                                  iCol+%trim(' ')                   +fCol+
031700130417     C                                  iCol+%trim(' ')                   +fCol+
031800130417     C                                  iCol+%trim(' ')                   +fCol+
031900130417     C                                  iCol+%trim(' ')                   +fCol+
032000130409     C                                  iCol+%trim(wLinkDir)              +fCol
032100100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
032200100908     C                   EVAL      wBufferOut =
032300130417     C                               '"'+   %trim(%editc(vabCCM:'X'))     +'";'
032400130409 xxx C                              +'"'+   %trim(wDSP)                   +'";'
032500130417     C                              +'"'+   %trim(%editc(vabRMN:'Q'))     +'";'
032600130417     C                              +'"'+   %trim(vabRMA)                 +'";'
032700130409     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
032800130409 xxx C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
032900130417     C                              +'"'+   %trim(%editc(vabLNP:'X'))     +'";'
033000130417     C                              +'"'+   %trim(%editc(vabLNA:'X'))     +'";'
033100130417     C                              +'"'+   %trim(vabRSD)                 +'";'
033200130417 xxx C                              +'"'+   %trim(' ')                    +'";'
033300130417     C                              +'"'+   %trim(vabIND)                 +'";'
033400130417     C                              +'"'+   %trim(vabCAD)                 +'";'
033500130417     C                              +'"'+   %trim(vabLOD)                 +'";'
033600130417     C                              +'"'+   %trim(vabPRD)                 +'";'
033700130417     C                              +'"'+   %trim(vabRMO)                 +'";'
033800130417     C                              +'"'+   %trim(' ')                    +'";'
033900130417     C                              +'"'+   %trim(' ')                    +'";'
034000130417     C                              +'"'+   %trim(' ')                    +'";'
034100130417     C                              +'"'+   %trim(' ')                    +'";'
034200130409     C                              +'"'+   %trim(wLinkDir)               +'";'
034300100908     C                   ENDSL
034400120926     C*
034500071121     C                   EVAL      VAWDTA = wBufferOut
034600110519     C*
034700120926     C  N31              WRITE     TIVAW000
034800071121     C*
034900071121     C                   ENDSR
035000071121
035100071121
035200071121
035300991027
035400991027      /TITLE Operazioni iniziali.
035500991027     C     *inzsr        BEGSR
035600991027     C*
035700991027     C     *ENTRY        PLIST
035800991027     C                   parm                    prmppt
035900991027     C     wrkesito      parm      wrkesito      prmesito
036000100910     C*
036100100910     C* CALCOLA LA DATA CORRENTE
036200100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
036300100910     C                   eval      datcor = %dec(%date() : *ISO)
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
