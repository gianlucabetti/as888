000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200120604     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000120604     FFIAR531C  IF   E           K DISK
001100030924
001200080924     D fnvac00t      e ds
001300120604     D DAR5GEN       e ds
001400071121
001500071121     D*------------
001600071121     D* DECODIFICHE COSTANTI
001700071121     D*------------
001800071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001900071121
002000071121
002100071121     D*------------
002200071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002300071121     D*------------
002400071121     D iHtml           s             10    inz('<html>')
002500071121     D fHtml           s             10    inz('</html>')
002600071121     D iBody           s             10    inz('<body>')
002700071121     D fBody           s             10    inz('</body>')
002800071121     D iTable          s             20    inz('<table border="1">')
002900071121     D fTable          s             10    inz('</table>')
003000071121     D iThead          s             10    inz('<thead>')
003100071121     D fThead          s             10    inz('</thead>')
003200071121     D iRow            s              5    inz('<tr>')
003300071121     D fRow            s              5    inz('</tr>')
003400071121     D iCol            s              5    inz('<td>')
003500071121     D fCol            s              5    inz('</td>')
003600071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003700071121     D fColBold        s              5    inz('</th>')
003800071122     D cApi            s              1    inz('''')
003900071121
004000071121
004100071121     D*------------
004200071121     D* VARIABILI D WRK
004300071121     D*------------
004400030924     D prmppt          S             50
004500030924     D prmesito        S              1
004600030924     D wrkesito        S                   like(prmesito)
004700071122     D wBufferOut      s          10000    varying inz
004800120604     D wDataSts        S              8  0 inz(*zeros)
004900100907     D wConsOkSiNo     S              1    inz(*blanks)
005000100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005100100907     D wLinkDir        S            300    inz(*blanks)
005200120604     D wTEL            s                   inz(*blanks) like(§AR5TELD)
005300110519     D wIND            s                   inz(*blanks) like(tasIND)
005400110519     D wCAD            s                   inz(*blanks) like(tasIND)
005500110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005600110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005700120604     D wDataSped       s             10    inz
005800120604     D wDataCRich      s             10    inz
005900120604     D wDataStato      s             10    inz
006000071211     D*
006100100910
006200100910
006300100910     D*------------------
006400100910     D* LINKING A DEFINIZIONI ESTERNE
006500100910     D*------------------
006600100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006700100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006800120604     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006900120604     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
007000120604
007100120604     D*-------------------
007200120604     D* COSTANTI
007300120604     D*-------------------
007400120604     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500120604     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600120604
007700100910
007800030924
007900030325
008000030924     C* MAIN/
008100071121     C*
008200071121     C* Verifico se già presenti dati nel file d output
008300071121     C                   EXSR      chkRECFILMBR
008400071121     C*
008500071121     C* Effettuo elaborazione
008600071121     C                   EXSR      traduci
008700071121     C*
008800921023     C                   SETON                                        LR
008900991027
009000071121
009100071121
009200071121     C     chkRECFILMBR  BEGSR
009300071121     C*
009400071121     C                   READ      TIVAWWWT                               55
009500071121     C*
009600071121     C                   ENDSR
009700071121
009800071121
009900030924
010000991027     C     traduci       BEGSR
010100071121     C*
010200071121     C* Head iniziali della struttura HTML
010300100908     C                   IF        %subst(prmppt:2:1) = 'H'
010400071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
010500071210     C   55              WRITE     TIVAW000
010600100908     C                   ENDIF
010700071121     C*
010800071121     C* Inizializzazone formato e variabili x output
010900071121     C                   clear                   TIVAW000
011000071121     C                   clear                   wBufferOut
011100100907     C                   clear                   wLinkDir
011200100907     C                   clear                   wConsOkSiNo
011300100907     C                   clear                   wConsOkSiNoDes
011400071121     C*
011500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
011700100908     C*
011800100908     C                   SELECT
011900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
012000071121     C                   EVAL      wBufferOut = iThead + iRow
012100110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
012200110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
012300110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
012400110519     C                             +iColBold+ 'RAG. SOC. DESTINATARIO' +fColBold
012500110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
012600110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
012700110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
012800110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
012900110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
013000110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013100110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
013200110519     C                             +iColBold+ 'STATO'                  +fColBold
013300110519     C                             +iColBold+ 'DATA STATO'             +fColBold
013400120604     C                             +iColBold+ 'LINK'                   +fColBold
013500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
013600100908     C                   EVAL      wBufferOut =
013700110519     C                              '"'+      'CODICE CLIENTE'         +'";'
013800110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
013900110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
014000110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
014100110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
014200110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
014300110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
014400110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
014500110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
014600110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
014700110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
014800110519     C                             +'"'+      'STATO'                  +'";'
014900110519     C                             +'"'+      'DATA STATO'             +'";'
015000120604     C                             +'"'+      'LINK'                   +'";'
015100100908     C                   ENDSL
015200100908     C*
015300071121     C                   EVAL      VAWDTA = wBufferOut
015400071121     C                   WRITE     TIVAW000
015500071121     C                   ENDIF
015600991027     C*
015700080924     C                   READ      TIVGDTMP
015800060119     C*
015900080924     C                   DOW       not %eof(TIVGDTMP)
016000080924     C*
016100080924     C                   eval      fnvac00t = vgdDTA
016200071121     C*
016300071121     C                   clear                   TIVAW000
016400071121     C                   clear                   wBufferOut
016500100907     C                   clear                   wLinkDir
016600100907     C                   clear                   wConsOkSiNo
016700100907     C                   clear                   wConsOkSiNoDes
016800100910     C*
016900100910     C* Solo bolle originali e valide ai fini della spedizione
017000100910     C                   movel     *blanks       wEsito1           1
017100100910     C*
017200100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
017300100910     C                                                vacAAS
017400100910     C                                               :vacLNP
017500100910     C                                               :vacNRS
017600100910     C                                               :vacNSP
017700100910     C                                               :pOutLblTyp
017800100910     C                                               :pOutAnnoBO
017900100910     C                                               :pOutLineaParBO
018000100910     C                                               :pOutSerieBO
018100100910     C                                               :pOutNumSpedBO
018200100910     C                                               :pOutDcmBO
018300100910     C                                               :pOutCcaBO
018400100910     C                                               :pOutRblBO))
018500100910     C*
018600100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
018700071210     C*
018800071210     C* Inizializzo campi d wrk
018900120604     C                   EVAL      wDataSped  = *blanks
019000120604     C                   EVAL      wDataCRich = *blanks
019100120604     C                   EVAL      wDataStato = *blanks
019200120604     C                   EVAL      wTEL = *blanks
019300110519     C                   EVAL      wIND = *blanks
019400110519     C                   EVAL      wCAD = *blanks
019500110519     C                   EVAL      wLOD = *blanks
019600110519     C                   EVAL      wPRD = *blanks
019700071210     C*
019800071210     C* Aggancio la bolla relativa x il completamento dei dati
019900071210     C     KEYtas30      chain     titas30c
020000071210     C                   if        %found(titas30c)
020100110519     C*
020200120604     C                   eval      ar5TRD = 'GEN'
020300120604     C     KEYar531_1P   chain     fiar531c
020400120604     C                   if        %found(fiar531c)
020500120604     C                   eval      DAR5GEN = ar5UNI
020600120604     C                   eval      wTEL = §AR5TELD
020700110519     C                   endif
020800071210     C*
020900071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
021000110519     C                   eval      wIND = tasIND
021100110519     C                   eval      wCAD = tasCAD
021200110519     C                   eval      wLOD = tasLOD
021300110519     C                   eval      wPRD = tasPRD
021400071213     C*
021500071213     C                   endif
021600071121     C*
021700071121     C* Compondo la stringa dati output relativa al record corrente
021800071121     C                   EXSR      wrifile
021900100910     C*
022000100910     C                   endif
022100040719     C*
022200040719     C* In ogni caso storicizzo e cancello l'input sempre
022300080924     C                   DELETE    TIVGD000
022400040719     C*
022500040719     C* Proseguo con la lettura dell'input
022600080924     C                   READ      TIVGDTMP
022700060119     C*
022800030325     C                   ENDDO
022900071121     C*
023000071121     C* Head finali della struttura HTML
023100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
023200071210     C***                WRITE     TIVAW000
023300991027     C*
023400030325     C                   EVAL      wrkesito = '0'
023500991027     C*
023600910830     C                   ENDSR
023700100907
023800100907
023900100907
024000100907     C     calSTS        BEGSR
024100100910     C*
024200100910     C                   EVAL      wDataSts       = vacDCM
024300100907     C*
024400100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
024500100907     C                   IF        VACDCM > *zeros AND
024600100907     C                             (VACCCA = *blanks OR VACCCA = '1')
024700100907     C                   EVAL      wConsOkSiNo    = 'S'
024800110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
024900100907     C                   ELSE
025000100907     C                   EVAL      wConsOkSiNo    = 'N'
025100100907     C*
025200100907     C* A questo punto stabilisco la descrizione
025300100910     C                   SELECT
025400100910     C                   WHEN      vacCCA = '1'
025500110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
025600100910     C                   WHEN      vacCCA = '2'
025700110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
025800100907     C                   EVAL      wConsOkSiNo    = 'R'
025900100910     C                   WHEN      vacCCA = '3'
026000110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
026100110519     C                   EVAL      wConsOkSiNo    = 'R'
026200100910     C                   WHEN      vacCCA = '4'
026300110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
026400110519     C                   EVAL      wConsOkSiNo    = 'R'
026500100910     C                   WHEN      vacCCA = '5'
026600110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
026700110519     C                   EVAL      wConsOkSiNo    = 'R'
026800100910     C                   WHEN      vacCCA = '6'
026900110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
027000110519     C                   EVAL      wConsOkSiNo    = 'R'
027100100910     C                   WHEN      vacCCA = 'A'
027200110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
027300110519     C                   EVAL      wConsOkSiNo    = 'R'
027400100910     C                   WHEN      vacCCA = 'C'
027500110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
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
029000100907     C                   ENDIF
029100100910     C                   IF        vacDCM = *zeros AND
029200100910     C                             vacDLA = *zeros AND
029300100910     C                             vacDAG = *zeros AND
029400100910     C                             vacCCA = *blanks
029500100910     C                   EVAL      wDataSts       = datcor
029600100910     C*
029700100910     C* Verifico la data consegna richiesta
029800100910     C                   IF        vacDCR > datcor
029900120604     C                   EVAL      wDataCRich =  %trim(UBFMTDATE_Convert(
030000120604     C                              %editc(vacDCR:'4')
030100120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
030200110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
030300120604     C                                              'data ' + wDataCRich
030400100910     C                   ELSE
030500110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
030600100910     C                   ENDIF
030700100910     C                   ENDIF
030800100907     C*
030900100907     C                   ENDIF
031000100907     C*
031100100907     C                   ENDSR
031200100907
031300100907
031400100907
031500100907     C     makLNK        BEGSR
031600120604     C*
031700120604     C* Porto in minuscolo il parametro relativo alla lingua del link
031800120604     C                   movel     *blanks       parLang           2
031900120604     C                   movel     *blanks       parKSU            7
032000120604     C*
032100120604     C                   eval      parKSU  = %subst(prmppt:6:7)
032200120604     C                   if        parKSU  = *blanks
032300120604     C                   eval      parKSU  = %editc(vacCCM:'X')
032400120604     C                   endif
032500120604     C*
032600120604     C                   eval      parLang = %subst(prmppt:13:2)
032700120604     C                   if        parLang <> *blanks
032800120604     C     maiu:minu     xlate     parLang       parLang
032900120604     C                   else
033000120604     C                   eval      parLang = 'it'
033100120604     C                   endif
033200100907     C*
033300130110     C                   IF        %subst(prmppt:17:1) = 'S' AND
033400130110     C                             %subst(prmppt:2:1) = 'C'
033500130110     C                   EVAL      wLinkDir = '=COLLEG.IPERTESTUALE(""'        +
033600160927     C                                        'http://wsrbrt.it/vas/'          +
033700130110     C                                        'sped_ricdocmit_load.hsm?docmit='+
033800130110     C                                        %trim(%char(vacRMN))             +
033900130110     C                                        '&ksu='+parKSU+'&lang='+parLang  +
034000130110     C                                        '"";""Click - Tracking '         +
034100130110     C                                        %trim(%char(vacRMN)) + ' "")'
034200130110     C                   ELSE
034300160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/'         +
034400100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
034500100907     C                                        %trim(%char(vacRMN))             +
034600120604     C                                        '&ksu='+parKSU+'&lang='+parLang
034700130110     C                   ENDIF
034800100907     C*
034900100907     C                   ENDSR
035000100907
035100071121
035200071121
035300071121
035400071121     C     wrifile       BEGSR
035500071121     C*
035600100907     C* Determino lo status della spedizione
035700100907     C                   EXSR      calSTS
035800100907     C*
035900130110     C* Costruisco il link diretto (solo se richiesto espressamente)
036000130110     C                   IF        %subst(prmppt:16:1) = 'S'
036100120604     C                   EXSR      makLNK
036200130110     C                   ENDIF
036300120604     C*
036400120604     C* Formatto le date interessate
036500120604     C*
036600120604     C                   EVAL      wDataSped  =  %trim(UBFMTDATE_Convert(
036700120604     C                             %editc(tasAAS*10000+tasMGS:'4')
036800120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
036900120604     C                   EVAL      wDataStato =  %trim(UBFMTDATE_Convert(
037000120604     C                              %editc(wDataSts:'4')
037100120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
037200100908     C*
037300100908     C                   SELECT
037400100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
037500071121     C                   EVAL      wBufferOut = iRow +
037600100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
037700120604     C                                  iCol+%trim(wDataSped)             +fCol+
037800110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
037900100907     C                                  iCol+%trim(vacRSD)                +fCol+
038000120604     C                                  iCol+%trim(wTEL)                  +fCol+
038100110519     C                                  iCol+%trim(wIND)                  +fCol+
038200100517     C                                  iCol+%trim(wCAD)                  +fCol+
038300100517     C                                  iCol+%trim(wLOD)                  +fCol+
038400110519     C                                  iCol+%trim(wPRD)                  +fCol+
038500100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
038600110519     C                                  iCol+%trim(vacRMA)                +fCol+
038700100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
038800120604     C                                  iCol+%trim(wDataStato)            +fCol+
038900120604     C                                  iCol+%trim(wLinkDir)              +fCol
039000100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
039100100908     C                   EVAL      wBufferOut =
039200110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
039300120604     C                              +'"'+   %trim(wDataSped)              +'";'
039400110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
039500110519     C                              +'"'+   %trim(vacRSD)                 +'";'
039600120604     C                              +'"'+   %trim(wTEL)                   +'";'
039700110519     C                              +'"'+   %trim(wIND)                   +'";'
039800110519     C                              +'"'+   %trim(wCAD)                   +'";'
039900110519     C                              +'"'+   %trim(wLOD)                   +'";'
040000110519     C                              +'"'+   %trim(wPRD)                   +'";'
040100110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
040200110519     C                              +'"'+   %trim(vacRMA)                 +'";'
040300110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
040400120604     C                              +'"'+   %trim(wDataStato)             +'";'
040500120604     C                              +'"'+   %trim(wLinkDir)               +'"'
040600100908     C                   ENDSL
040700071121     C*
040800071121     C                   EVAL      VAWDTA = wBufferOut
040900110519     C*
041000120604     C* Verifico cosa restituire
041100120604     C                   SELECT
041200120605     C                   WHEN      %subst(prmppt:15:1) = 'C' AND
041300120604     C                             (wConsOkSiNo = 'S' OR wConsOkSiNo = 'R')
041400120604     C                   WRITE     TIVAW000
041500120605     C                   WHEN      %subst(prmppt:15:1) = 'P' AND
041600120604     C                             (wConsOkSiNo = 'N')
041700120604     C                   WRITE     TIVAW000
041800120606     C                   WHEN      %subst(prmppt:15:1) = *blanks
041900071121     C                   WRITE     TIVAW000
042000120604     C                   ENDSL
042100071121     C*
042200071121     C                   ENDSR
042300071121
042400071121
042500071121
042600991027
042700991027      /TITLE Operazioni iniziali.
042800991027     C     *inzsr        BEGSR
042900991027     C*
043000991027     C     *ENTRY        PLIST
043100991027     C                   parm                    prmppt
043200991027     C     wrkesito      parm      wrkesito      prmesito
043300100910     C*
043400100910     C* CALCOLA LA DATA CORRENTE
043500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
043600100910     C                   eval      datcor = %dec(%date() : *ISO)
043700071210     C*
043800071213     C* TITAS30C - Parziale
043900071210     C     KEYtas30      KLIST
044000071210     C                   KFLD                    vacAAS
044100071210     C                   KFLD                    vacLNP
044200071210     C                   KFLD                    vacNRS
044300071210     C                   KFLD                    vacNSP
044400110519     C*
044500120604     C* FIAR531C - Parziale
044600120604     C     KEYar531_1P   KLIST
044700110519     C                   KFLD                    vacAAS
044800110519     C                   KFLD                    vacLNP
044900110519     C                   KFLD                    vacNRS
045000110519     C                   KFLD                    vacNSP
045100120604     C                   KFLD                    ar5TRD
045200991027     C*
045300991027     C                   ENDSR
045400071121     C*
045500071121** DEC - COSTANTI
045600071121<?xml version="1.0"?>
045700071121<?mso-application progid="Excel.Sheet"?>
045800071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
045900071121xmlns:o="urn:schemas-microsoft-com:office:office"
046000071121xmlns:x="urn:schemas-microsoft-com:office:excel"
046100071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
046200071121xmlns:html="http://www.w3.org/TR/REC-html40">
046300071121</Workbook>
046400071121<Worksheet ss:Name="PAG1">
046500071121</Worksheet>
046600071121<Table border="1">
046700071121</Table>
046800071121<Row>
046900071121</Row>
047000071121<Cell><Data ss:Type="String">
047100071121<Cell><Data ss:Type="Number">
047200071121</Data></Cell>
047300071121
047400071121
047500071121
