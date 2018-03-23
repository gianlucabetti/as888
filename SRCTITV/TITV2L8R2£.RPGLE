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
004900150707     D wOraSts         S              4  0 inz(*zeros)
005000100907     D wConsOkSiNo     S              1    inz(*blanks)
005100100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005200100907     D wLinkDir        S            300    inz(*blanks)
005300120604     D wTEL            s                   inz(*blanks) like(§AR5TELD)
005400110519     D wIND            s                   inz(*blanks) like(tasIND)
005500110519     D wCAD            s                   inz(*blanks) like(tasIND)
005600110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005700110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005800120604     D wDataSped       s             10    inz
005900120604     D wDataCRich      s             10    inz
006000120604     D wDataStato      s             10    inz
006100150707     D wOraStato       s              5    inz
006200071211     D*
006300100910
006400100910
006500100910     D*------------------
006600100910     D* LINKING A DEFINIZIONI ESTERNE
006700100910     D*------------------
006800100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006900100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007000120604     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
007100120604     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
007200120604
007300120604     D*-------------------
007400120604     D* COSTANTI
007500120604     D*-------------------
007600120604     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700120604     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800120604
007900100910
008000030924
008100030325
008200030924     C* MAIN/
008300071121     C*
008400071121     C* Verifico se già presenti dati nel file d output
008500071121     C                   EXSR      chkRECFILMBR
008600071121     C*
008700071121     C* Effettuo elaborazione
008800071121     C                   EXSR      traduci
008900071121     C*
009000921023     C                   SETON                                        LR
009100991027
009200071121
009300071121
009400071121     C     chkRECFILMBR  BEGSR
009500071121     C*
009600071121     C                   READ      TIVAWWWT                               55
009700071121     C*
009800071121     C                   ENDSR
009900071121
010000071121
010100030924
010200991027     C     traduci       BEGSR
010300071121     C*
010400071121     C* Head iniziali della struttura HTML
010500100908     C                   IF        %subst(prmppt:2:1) = 'H'
010600071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
010700071210     C   55              WRITE     TIVAW000
010800100908     C                   ENDIF
010900071121     C*
011000071121     C* Inizializzazone formato e variabili x output
011100071121     C                   clear                   TIVAW000
011200071121     C                   clear                   wBufferOut
011300100907     C                   clear                   wLinkDir
011400100907     C                   clear                   wConsOkSiNo
011500100907     C                   clear                   wConsOkSiNoDes
011600071121     C*
011700071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011800071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
011900100908     C*
012000100908     C                   SELECT
012100100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
012200071121     C                   EVAL      wBufferOut = iThead + iRow
012300110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
012400110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
012500110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
012600110519     C                             +iColBold+ 'RAG. SOC. DESTINATARIO' +fColBold
012700110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
012800110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
012900110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
013000110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
013100110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
013200110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013300110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
013400170427     C                             +iColBold+ 'CONSEGNA PARTICOLARE 1' +fColBold
013500110519     C                             +iColBold+ 'STATO'                  +fColBold
013600110519     C                             +iColBold+ 'DATA STATO'             +fColBold
013700150707     C                             +iColBold+ 'ORA STATO'              +fColBold
013800120604     C                             +iColBold+ 'LINK'                   +fColBold
013900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
014000100908     C                   EVAL      wBufferOut =
014100110519     C                              '"'+      'CODICE CLIENTE'         +'";'
014200110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
014300110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
014400110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
014500110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
014600110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
014700110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
014800110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
014900110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
015000110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
015100110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
015200170427     C                             +'"'+      'CONSEGNA PARTICOLARE 1' +'";'
015300110519     C                             +'"'+      'STATO'                  +'";'
015400110519     C                             +'"'+      'DATA STATO'             +'";'
015500150707     C                             +'"'+      'ORA STATO'              +'";'
015600120604     C                             +'"'+      'LINK'                   +'";'
015700100908     C                   ENDSL
015800100908     C*
015900071121     C                   EVAL      VAWDTA = wBufferOut
016000071121     C                   WRITE     TIVAW000
016100071121     C                   ENDIF
016200991027     C*
016300080924     C                   READ      TIVGDTMP
016400060119     C*
016500080924     C                   DOW       not %eof(TIVGDTMP)
016600080924     C*
016700080924     C                   eval      fnvac00t = vgdDTA
016800071121     C*
016900071121     C                   clear                   TIVAW000
017000071121     C                   clear                   wBufferOut
017100100907     C                   clear                   wLinkDir
017200100907     C                   clear                   wConsOkSiNo
017300100907     C                   clear                   wConsOkSiNoDes
017400100910     C*
017500100910     C* Solo bolle originali e valide ai fini della spedizione
017600100910     C                   movel     *blanks       wEsito1           1
017700100910     C*
017800100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
017900100910     C                                                vacAAS
018000100910     C                                               :vacLNP
018100100910     C                                               :vacNRS
018200100910     C                                               :vacNSP
018300100910     C                                               :pOutLblTyp
018400100910     C                                               :pOutAnnoBO
018500100910     C                                               :pOutLineaParBO
018600100910     C                                               :pOutSerieBO
018700100910     C                                               :pOutNumSpedBO
018800100910     C                                               :pOutDcmBO
018900100910     C                                               :pOutCcaBO
019000100910     C                                               :pOutRblBO))
019100100910     C*
019200100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
019300071210     C*
019400071210     C* Inizializzo campi d wrk
019500120604     C                   EVAL      wDataSped  = *blanks
019600120604     C                   EVAL      wDataCRich = *blanks
019700120604     C                   EVAL      wDataStato = *blanks
019800150707     C                   EVAL      wOraStato  = *blanks
019900120604     C                   EVAL      wTEL = *blanks
020000110519     C                   EVAL      wIND = *blanks
020100110519     C                   EVAL      wCAD = *blanks
020200110519     C                   EVAL      wLOD = *blanks
020300110519     C                   EVAL      wPRD = *blanks
020400071210     C*
020500071210     C* Aggancio la bolla relativa x il completamento dei dati
020600071210     C     KEYtas30      chain     titas30c
020700071210     C                   if        %found(titas30c)
020800110519     C*
020900120604     C                   eval      ar5TRD = 'GEN'
021000120604     C     KEYar531_1P   chain     fiar531c
021100120604     C                   if        %found(fiar531c)
021200120604     C                   eval      DAR5GEN = ar5UNI
021300120604     C                   eval      wTEL = §AR5TELD
021400110519     C                   endif
021500071210     C*
021600071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
021700110519     C                   eval      wIND = tasIND
021800110519     C                   eval      wCAD = tasCAD
021900110519     C                   eval      wLOD = tasLOD
022000110519     C                   eval      wPRD = tasPRD
022100071213     C*
022200071213     C                   endif
022300071121     C*
022400071121     C* Compondo la stringa dati output relativa al record corrente
022500071121     C                   EXSR      wrifile
022600100910     C*
022700100910     C                   endif
022800040719     C*
022900040719     C* In ogni caso storicizzo e cancello l'input sempre
023000080924     C                   DELETE    TIVGD000
023100040719     C*
023200040719     C* Proseguo con la lettura dell'input
023300080924     C                   READ      TIVGDTMP
023400060119     C*
023500030325     C                   ENDDO
023600071121     C*
023700071121     C* Head finali della struttura HTML
023800071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
023900071210     C***                WRITE     TIVAW000
024000991027     C*
024100030325     C                   EVAL      wrkesito = '0'
024200991027     C*
024300910830     C                   ENDSR
024400100907
024500100907
024600100907
024700100907     C     calSTS        BEGSR
024800100910     C*
024900100910     C                   EVAL      wDataSts       = vacDCM
025000150707     C                   EVAL      wOraSts        = vacHMC
025100100907     C*
025200100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
025300100907     C                   IF        VACDCM > *zeros AND
025400100907     C                             (VACCCA = *blanks OR VACCCA = '1')
025500100907     C                   EVAL      wConsOkSiNo    = 'S'
025600110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
025700100907     C                   ELSE
025800100907     C                   EVAL      wConsOkSiNo    = 'N'
025900100907     C*
026000100907     C* A questo punto stabilisco la descrizione
026100100910     C                   SELECT
026200100910     C                   WHEN      vacCCA = '1'
026300110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
026400100910     C                   WHEN      vacCCA = '2'
026500110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
026600100907     C                   EVAL      wConsOkSiNo    = 'R'
026700100910     C                   WHEN      vacCCA = '3'
026800110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
026900110519     C                   EVAL      wConsOkSiNo    = 'R'
027000100910     C                   WHEN      vacCCA = '4'
027100110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
027200110519     C                   EVAL      wConsOkSiNo    = 'R'
027300100910     C                   WHEN      vacCCA = '5'
027400110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
027500110519     C                   EVAL      wConsOkSiNo    = 'R'
027600100910     C                   WHEN      vacCCA = '6'
027700110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
027800110519     C                   EVAL      wConsOkSiNo    = 'R'
027900100910     C                   WHEN      vacCCA = 'A'
028000110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
028100160907     C***                EVAL      wConsOkSiNo    = 'R'
028200100910     C                   WHEN      vacCCA = 'C'
028300110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
028400100910     C                   WHEN      vacCCA = 'R'
028500110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
028600100910     C                   WHEN      vacCCA = 'S'
028700110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
028800110519     C                   EVAL      wConsOkSiNo    = 'R'
028900100910     C                   ENDSL
029000100907     C*
029100100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
029200100910     C                   EVAL      wDataSts       = vacDLA
029300150707     C                   EVAL      wOraSts        = *zeros
029400110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
029500100907     C                   ENDIF
029600100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
029700100910     C                   EVAL      wDataSts       = vacDAG
029800150707     C                   EVAL      wOraSts        = *zeros
029900110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
030000100907     C                   ENDIF
030100100910     C                   IF        vacDCM = *zeros AND
030200100910     C                             vacDLA = *zeros AND
030300100910     C                             vacDAG = *zeros AND
030400100910     C                             vacCCA = *blanks
030500100910     C                   EVAL      wDataSts       = datcor
030600150707     C                   EVAL      wOraSts        = *zeros
030700100910     C*
030800100910     C* Verifico la data consegna richiesta
030900100910     C                   IF        vacDCR > datcor
031000120604     C                   EVAL      wDataCRich =  %trim(UBFMTDATE_Convert(
031100120604     C                              %editc(vacDCR:'4')
031200120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
031300110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
031400120604     C                                              'data ' + wDataCRich
031500100910     C                   ELSE
031600110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
031700100910     C                   ENDIF
031800100910     C                   ENDIF
031900100907     C*
032000100907     C                   ENDIF
032100100907     C*
032200100907     C                   ENDSR
032300100907
032400100907
032500100907
032600100907     C     makLNK        BEGSR
032700120604     C*
032800120604     C* Porto in minuscolo il parametro relativo alla lingua del link
032900120604     C                   movel     *blanks       parLang           2
033000120604     C                   movel     *blanks       parKSU            7
033100120604     C*
033200120604     C                   eval      parKSU  = %subst(prmppt:6:7)
033300120604     C                   if        parKSU  = *blanks
033400120604     C                   eval      parKSU  = %editc(vacCCM:'X')
033500120604     C                   endif
033600120604     C*
033700120604     C                   eval      parLang = %subst(prmppt:13:2)
033800120604     C                   if        parLang <> *blanks
033900120604     C     maiu:minu     xlate     parLang       parLang
034000120604     C                   else
034100120604     C                   eval      parLang = 'it'
034200120604     C                   endif
034300100907     C*
034400130110     C                   IF        %subst(prmppt:17:1) = 'S' AND
034500130110     C                             %subst(prmppt:2:1) = 'C'
034600130110     C                   EVAL      wLinkDir = '=COLLEG.IPERTESTUALE(""'        +
034700160927     C                                        'http://vas.brt.it/vas/'         +
034800130110     C                                        'sped_ricdocmit_load.hsm?docmit='+
034900130110     C                                        %trim(%char(vacRMN))             +
035000130110     C                                        '&ksu='+parKSU+'&lang='+parLang  +
035100130110     C                                        '"";""Click - Tracking '         +
035200130110     C                                        %trim(%char(vacRMN)) + ' "")'
035300130110     C                   ELSE
035400160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/'         +
035500100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
035600100907     C                                        %trim(%char(vacRMN))             +
035700120604     C                                        '&ksu='+parKSU+'&lang='+parLang
035800130110     C                   ENDIF
035900100907     C*
036000100907     C                   ENDSR
036100100907
036200071121
036300071121
036400071121
036500071121     C     wrifile       BEGSR
036600071121     C*
036700100907     C* Determino lo status della spedizione
036800100907     C                   EXSR      calSTS
036900100907     C*
037000130110     C* Costruisco il link diretto (solo se richiesto espressamente)
037100130110     C                   IF        %subst(prmppt:16:1) = 'S'
037200120604     C                   EXSR      makLNK
037300130110     C                   ENDIF
037400120604     C*
037500120604     C* Formatto le date interessate
037600120604     C*
037700120604     C                   EVAL      wDataSped  =  %trim(UBFMTDATE_Convert(
037800120604     C                             %editc(tasAAS*10000+tasMGS:'4')
037900120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
038000120604     C                   EVAL      wDataStato =  %trim(UBFMTDATE_Convert(
038100120604     C                              %editc(wDataSts:'4')
038200120604     C                             :'YYYYMMDD':'DD/MM/YYYY'))
038300150707     C                   IF        wOraSts > *zeros
038400150707     C                   EVAL      wOraStato =  %subst(%editc(wOraSts:'X'):1:2)+':'
038500150707     C                                          %subst(%editc(wOraSts:'X'):3:2)
038600150707     C                   ENDIF
038700100908     C*
038800100908     C                   SELECT
038900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
039000071121     C                   EVAL      wBufferOut = iRow +
039100100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
039200120604     C                                  iCol+%trim(wDataSped)             +fCol+
039300110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
039400100907     C                                  iCol+%trim(vacRSD)                +fCol+
039500120604     C                                  iCol+%trim(wTEL)                  +fCol+
039600110519     C                                  iCol+%trim(wIND)                  +fCol+
039700100517     C                                  iCol+%trim(wCAD)                  +fCol+
039800100517     C                                  iCol+%trim(wLOD)                  +fCol+
039900110519     C                                  iCol+%trim(wPRD)                  +fCol+
040000100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
040100110519     C                                  iCol+%trim(vacRMA)                +fCol+
040200170427     C                                  iCol+%trim(vacTC1)                +fCol+
040300100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
040400120604     C                                  iCol+%trim(wDataStato)            +fCol+
040500150707     C                                  iCol+%trim(wOraStato)             +fCol+
040600120604     C                                  iCol+%trim(wLinkDir)              +fCol
040700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
040800100908     C                   EVAL      wBufferOut =
040900110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
041000120604     C                              +'"'+   %trim(wDataSped)              +'";'
041100110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
041200110519     C                              +'"'+   %trim(vacRSD)                 +'";'
041300120604     C                              +'"'+   %trim(wTEL)                   +'";'
041400110519     C                              +'"'+   %trim(wIND)                   +'";'
041500110519     C                              +'"'+   %trim(wCAD)                   +'";'
041600110519     C                              +'"'+   %trim(wLOD)                   +'";'
041700110519     C                              +'"'+   %trim(wPRD)                   +'";'
041800110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
041900110519     C                              +'"'+   %trim(vacRMA)                 +'";'
042000110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
042100120604     C                              +'"'+   %trim(wDataStato)             +'";'
042200150707     C                              +'"'+   %trim(wOraStato)              +'";'
042300120604     C                              +'"'+   %trim(wLinkDir)               +'"'
042400100908     C                   ENDSL
042500071121     C*
042600071121     C                   EVAL      VAWDTA = wBufferOut
042700110519     C*
042800120604     C* Verifico cosa restituire
042900120604     C                   SELECT
043000120605     C                   WHEN      %subst(prmppt:15:1) = 'C' AND
043100120604     C                             (wConsOkSiNo = 'S' OR wConsOkSiNo = 'R')
043200120604     C                   WRITE     TIVAW000
043300120605     C                   WHEN      %subst(prmppt:15:1) = 'P' AND
043400120604     C                             (wConsOkSiNo = 'N')
043500120604     C                   WRITE     TIVAW000
043600120606     C                   WHEN      %subst(prmppt:15:1) = *blanks
043700071121     C                   WRITE     TIVAW000
043800120604     C                   ENDSL
043900071121     C*
044000071121     C                   ENDSR
044100071121
044200071121
044300071121
044400991027
044500991027      /TITLE Operazioni iniziali.
044600991027     C     *inzsr        BEGSR
044700991027     C*
044800991027     C     *ENTRY        PLIST
044900991027     C                   parm                    prmppt
045000991027     C     wrkesito      parm      wrkesito      prmesito
045100100910     C*
045200100910     C* CALCOLA LA DATA CORRENTE
045300100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
045400100910     C                   eval      datcor = %dec(%date() : *ISO)
045500071210     C*
045600071213     C* TITAS30C - Parziale
045700071210     C     KEYtas30      KLIST
045800071210     C                   KFLD                    vacAAS
045900071210     C                   KFLD                    vacLNP
046000071210     C                   KFLD                    vacNRS
046100071210     C                   KFLD                    vacNSP
046200110519     C*
046300120604     C* FIAR531C - Parziale
046400120604     C     KEYar531_1P   KLIST
046500110519     C                   KFLD                    vacAAS
046600110519     C                   KFLD                    vacLNP
046700110519     C                   KFLD                    vacNRS
046800110519     C                   KFLD                    vacNSP
046900120604     C                   KFLD                    ar5TRD
047000991027     C*
047100991027     C                   ENDSR
047200071121     C*
047300071121** DEC - COSTANTI
047400071121<?xml version="1.0"?>
047500071121<?mso-application progid="Excel.Sheet"?>
047600071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
047700071121xmlns:o="urn:schemas-microsoft-com:office:office"
047800071121xmlns:x="urn:schemas-microsoft-com:office:excel"
047900071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
048000071121xmlns:html="http://www.w3.org/TR/REC-html40">
048100071121</Workbook>
048200071121<Worksheet ss:Name="PAG1">
048300071121</Worksheet>
048400071121<Table border="1">
048500071121</Table>
048600071121<Row>
048700071121</Row>
048800071121<Cell><Data ss:Type="String">
048900071121<Cell><Data ss:Type="Number">
049000071121</Data></Cell>
049100071121
049200071121
049300071121
