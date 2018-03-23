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
001100121207     FTITAA30C  IF   E           K DISK
001200130913     FTIGCP51L  IF   E           K DISK
001300030924
001400080924     D fnvac00t      e ds
001500071121
001600071121     D*------------
001700071121     D* DECODIFICHE COSTANTI
001800071121     D*------------
001900071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002000071121
002100071121
002200071121     D*------------
002300071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002400071121     D*------------
002500071121     D iHtml           s             10    inz('<html>')
002600071121     D fHtml           s             10    inz('</html>')
002700071121     D iBody           s             10    inz('<body>')
002800071121     D fBody           s             10    inz('</body>')
002900071121     D iTable          s             20    inz('<table border="1">')
003000071121     D fTable          s             10    inz('</table>')
003100071121     D iThead          s             10    inz('<thead>')
003200071121     D fThead          s             10    inz('</thead>')
003300071121     D iRow            s              5    inz('<tr>')
003400071121     D fRow            s              5    inz('</tr>')
003500071121     D iCol            s              5    inz('<td>')
003600071121     D fCol            s              5    inz('</td>')
003700071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003800071121     D fColBold        s              5    inz('</th>')
003900071122     D cApi            s              1    inz('''')
004000071121
004100071121
004200071121     D*------------
004300071121     D* VARIABILI D WRK
004400071121     D*------------
004500030924     D prmppt          S             50
004600030924     D prmesito        S              1
004700030924     D wrkesito        S                   like(prmesito)
004800071122     D wBufferOut      s          10000    varying inz
004900100910     D wDataSts        S              8  0 inz(*zeros)
005000100907     D wConsOkSiNo     S              1    inz(*blanks)
005100100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005200100907     D wLinkDir        S            300    inz(*blanks)
005300110519     D wDSP            s             10    inz(*blanks)
005400110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005500110519     D wIND            s                   inz(*blanks) like(tasIND)
005600110519     D wCAD            s                   inz(*blanks) like(tasIND)
005700110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005800110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005900121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
006000121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
006100121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
006200121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
006300121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
006400130426     D wKSC            s              7  0 inz(*zeros)
006500071211     D*
006600100910
006700100910
006800100910     D*------------------
006900100910     D* LINKING A DEFINIZIONI ESTERNE
007000100910     D*------------------
007100100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007200100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007300130426
007400130426
007500130426     D*-------------------
007600130426     D* COSTANTI
007700130426     D*-------------------
007800130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000100910
008100030924
008200030325
008300030924     C* MAIN/
008400071121     C*
008500071121     C* Verifico se già presenti dati nel file d output
008600071121     C                   EXSR      chkRECFILMBR
008700071121     C*
008800071121     C* Effettuo elaborazione
008900071121     C                   EXSR      traduci
009000071121     C*
009100921023     C                   SETON                                        LR
009200991027
009300071121
009400071121
009500071121     C     chkRECFILMBR  BEGSR
009600071121     C*
009700071121     C                   READ      TIVAWWWT                               55
009800071121     C*
009900071121     C                   ENDSR
010000071121
010100071121
010200030924
010300991027     C     traduci       BEGSR
010400071121     C*
010500071121     C* Head iniziali della struttura HTML
010600100908     C                   IF        %subst(prmppt:2:1) = 'H'
010700071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
010800071210     C   55              WRITE     TIVAW000
010900100908     C                   ENDIF
011000071121     C*
011100071121     C* Inizializzazone formato e variabili x output
011200071121     C                   clear                   TIVAW000
011300071121     C                   clear                   wBufferOut
011400100907     C                   clear                   wLinkDir
011500100907     C                   clear                   wConsOkSiNo
011600100907     C                   clear                   wConsOkSiNoDes
011700071121     C*
011800071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011900071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
012000100908     C*
012100100908     C                   SELECT
012200100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
012300071121     C                   EVAL      wBufferOut = iThead + iRow
012400110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
012500110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
012600110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
012700121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
012800110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
012900110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
013000110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
013100110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
013200110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
013300110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013400110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
013500121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
013600121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
013700121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
013800121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
013900121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
014000110519     C                             +iColBold+ 'STATO'                  +fColBold
014100130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
014200110519     C                             +iColBold+ 'DATA STATO'             +fColBold
014300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
014400100908     C                   EVAL      wBufferOut =
014500110519     C                              '"'+      'CODICE CLIENTE'         +'";'
014600110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
014700110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
014800110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
014900110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
015000110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
015100110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
015200110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
015300110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
015400110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
015500110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
015600121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
015700121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
015800121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
015900121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
016000121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
016100110519     C                             +'"'+      'STATO'                  +'";'
016200130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
016300110519     C                             +'"'+      'DATA STATO'             +'";'
016400100908     C                   ENDSL
016500100908     C*
016600071121     C                   EVAL      VAWDTA = wBufferOut
016700071121     C                   WRITE     TIVAW000
016800071121     C                   ENDIF
016900991027     C*
017000080924     C                   READ      TIVGDTMP
017100060119     C*
017200080924     C                   DOW       not %eof(TIVGDTMP)
017300080924     C*
017400080924     C                   eval      fnvac00t = vgdDTA
017500071121     C*
017600071121     C                   clear                   TIVAW000
017700071121     C                   clear                   wBufferOut
017800100907     C                   clear                   wLinkDir
017900100907     C                   clear                   wConsOkSiNo
018000100907     C                   clear                   wConsOkSiNoDes
018100100910     C*
018200100910     C* Solo bolle originali e valide ai fini della spedizione
018300100910     C                   movel     *blanks       wEsito1           1
018400100910     C*
018500100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
018600100910     C                                                vacAAS
018700100910     C                                               :vacLNP
018800100910     C                                               :vacNRS
018900100910     C                                               :vacNSP
019000100910     C                                               :pOutLblTyp
019100100910     C                                               :pOutAnnoBO
019200100910     C                                               :pOutLineaParBO
019300100910     C                                               :pOutSerieBO
019400100910     C                                               :pOutNumSpedBO
019500100910     C                                               :pOutDcmBO
019600100910     C                                               :pOutCcaBO
019700100910     C                                               :pOutRblBO))
019800100910     C*
019900130426     C                   if         pOutRblBO ='N' AND
020000130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020100071210     C*
020200071210     C* Inizializzo campi d wrk
020300121207     C                   EVAL      wDSP     = *blanks
020400121207     C                   EVAL      wRD2     = *blanks
020500121207     C                   EVAL      wIND     = *blanks
020600121207     C                   EVAL      wCAD     = *blanks
020700121207     C                   EVAL      wLOD     = *blanks
020800121207     C                   EVAL      wPRD     = *blanks
020900121207     C                   EVAL      wRSCMORI = *blanks
021000121207     C                   EVAL      wINDMORI = *blanks
021100121207     C                   EVAL      wCAPMORI = *blanks
021200121207     C                   EVAL      wLOCMORI = *blanks
021300121207     C                   EVAL      wPRVMORI = *blanks
021400121207     C*
021500071210     C* Aggancio la bolla relativa x il completamento dei dati
021600071210     C     KEYtas30      chain     titas30c
021700071210     C                   if        %found(titas30c)
021800110519     C*
021900121207     C* Reperisco l'estensione ragione sociale destinatario
022000110519     C                   eval      ta4TRC = 'D'
022100110519     C     KEYta430      chain     tita430c
022200110519     C                   if        %found(tita430c)
022300110519     C                   eval      wRD2 = '.' + ta4NOT
022400110519     C                   endif
022500071210     C*
022600071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
022700110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
022800110519     C                   eval      wIND = tasIND
022900110519     C                   eval      wCAD = tasCAD
023000110519     C                   eval      wLOD = tasLOD
023100110519     C                   eval      wPRD = tasPRD
023200121207     C*
023300121207     C* Reperisco i dati del mittente originale
023400121207     C                   eval      taaTRC = 'O'
023500121207     C     KEYtaa30      chain     titaa30c
023600121207     C                   if        %found(titaa30c)
023700121207     C                   eval      wRSCMORI = taaRSC
023800121207     C                   eval      wINDMORI = taaIND
023900121207     C                   eval      wCAPMORI = taaCAP
024000121207     C                   eval      wLOCMORI = taaLOC
024100121207     C                   eval      wPRVMORI = taaPRV
024200121207     C                   endif
024300130913     C*
024400130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
024500130913     C* (costruisco la chiave sul file TIGCP51L)
024600130913     C                   setoff                                       70
024700130913     C*
024800130913     C                   EVAL      gcpFRG = *zeros
024900130913     C     KEYgcp51      SETLL     tigcp51l
025000130913     C                   IF        %equal(tigcp51l)
025100130913     C     KEYgcp51      READE     tigcp51l
025200130913     C                   DOW       not %eof(tigcp51l)
025300130913     C*
025400130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
025500130913     C                   IF        gcpDUR > *zeros AND
025600130913     C                             gcpDDM = *zeros AND
025700130913     C                             gcpFAS < 030
025800130913     C                   seton                                        70
025900130913     C                   ENDIF
026000130913     C*
026100130913     C* Proseguo la lettura delle giacenze x la bolla corrente
026200130913     C     KEYgcp51      READE     tigcp51l
026300130913     C                   ENDDO
026400130913     C                   ENDIF
026500071213     C*
026600071213     C                   endif
026700071121     C*
026800071121     C* Compondo la stringa dati output relativa al record corrente
026900071121     C                   EXSR      wrifile
027000100910     C*
027100100910     C                   endif
027200040719     C*
027300040719     C* In ogni caso storicizzo e cancello l'input sempre
027400080924     C                   DELETE    TIVGD000
027500040719     C*
027600040719     C* Proseguo con la lettura dell'input
027700080924     C                   READ      TIVGDTMP
027800060119     C*
027900030325     C                   ENDDO
028000071121     C*
028100071121     C* Head finali della struttura HTML
028200071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
028300071210     C***                WRITE     TIVAW000
028400991027     C*
028500030325     C                   EVAL      wrkesito = '0'
028600991027     C*
028700910830     C                   ENDSR
028800100907
028900100907
029000100907
029100100907     C     calSTS        BEGSR
029200100910     C*
029300120926     C                   MOVEL     *blanks       wMIC              1
029400121114     C                   MOVEL     *blanks       wGIAC             1
029500121207     C                   MOVEL     *blanks       wLAVV             1
029600100910     C                   EVAL      wDataSts       = vacDCM
029700100907     C*
029800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
029900100907     C                   IF        VACDCM > *zeros AND
030000100907     C                             (VACCCA = *blanks OR VACCCA = '1')
030100100907     C                   EVAL      wConsOkSiNo    = 'S'
030200110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
030300100907     C                   ELSE
030400100907     C                   EVAL      wConsOkSiNo    = 'N'
030500100907     C*
030600100907     C* A questo punto stabilisco la descrizione
030700100910     C                   SELECT
030800100910     C                   WHEN      vacCCA = '1'
030900110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
031000100910     C                   WHEN      vacCCA = '2'
031100110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
031200100907     C                   EVAL      wConsOkSiNo    = 'R'
031300100910     C                   WHEN      vacCCA = '3'
031400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
031500110519     C                   EVAL      wConsOkSiNo    = 'R'
031600100910     C                   WHEN      vacCCA = '4'
031700110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
031800110519     C                   EVAL      wConsOkSiNo    = 'R'
031900100910     C                   WHEN      vacCCA = '5'
032000110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
032100110519     C                   EVAL      wConsOkSiNo    = 'R'
032200100910     C                   WHEN      vacCCA = '6'
032300110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
032400110519     C                   EVAL      wConsOkSiNo    = 'R'
032500100910     C                   WHEN      vacCCA = 'A'
032600110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
032700110519     C                   EVAL      wConsOkSiNo    = 'R'
032800100910     C                   WHEN      vacCCA = 'C'
032900110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
033000120926     C                   EVAL      wMIC = 'S'
033100100910     C                   WHEN      vacCCA = 'R'
033200110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
033300100910     C                   WHEN      vacCCA = 'S'
033400110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
033500110519     C                   EVAL      wConsOkSiNo    = 'R'
033600100910     C                   ENDSL
033700100907     C*
033800100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
033900100910     C                   EVAL      wDataSts       = vacDLA
034000110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
034100121207     C                   EVAL      wLAVV = 'S'
034200100907     C                   ENDIF
034300100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
034400100910     C                   EVAL      wDataSts       = vacDAG
034500110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
034600121114     C                   EVAL      wGIAC = 'S'
034700100907     C                   ENDIF
034800100910     C                   IF        vacDCM = *zeros AND
034900100910     C                             vacDLA = *zeros AND
035000100910     C                             vacDAG = *zeros AND
035100100910     C                             vacCCA = *blanks
035200100910     C                   EVAL      wDataSts       = datcor
035300100910     C*
035400100910     C* Verifico la data consegna richiesta
035500100910     C                   IF        vacDCR > datcor
035600110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
035700110519     C                                              'data ' + %editc(vacDCR:'X')
035800100910     C                   ELSE
035900110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
036000100910     C                   ENDIF
036100100910     C                   ENDIF
036200130913     C*
036300130913     C* Verifico lo stato delle disposizioni di giacenza
036400130913     C                   MOVEL     *blanks       wGiacDisp        10
036500130913     C   70              EVAL      wGiacDisp = 'IN ATTESA'
036600100907     C*
036700100907     C                   ENDIF
036800100907     C*
036900100907     C                   ENDSR
037000100907
037100100907
037200100907
037300100907     C     makLNK        BEGSR
037400130426     C*
037500130426     C* Se richiesta inclusione link T&T
037600130426     C                   if        %subst(prmppt:6:7) <> *blanks
037700130426     C*
037800130426     C* Porto in minuscolo il parametro relativo alla lingua del link
037900130426     C                   movel     *blanks       parLang           2
038000130426     C                   eval      parLang = %subst(prmppt:13:2)
038100130426     C     maiu:minu     xlate     parLang       parLang
038200130426     C*
038300160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
038400130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
038500130426     C                                        %trim(%char(vacRMN))             +
038600130426     C                                        '&ksu='+%subst(prmppt:6:7)+
038700130426     C                                        '&lang='+parLang
038800130426     C*
038900130426     C                   endif
039000100907     C*
039100100907     C                   ENDSR
039200100907
039300071121
039400071121
039500071121
039600071121     C     wrifile       BEGSR
039700120926     C*
039800120926     C                   SETOFF                                       31
039900071121     C*
040000100907     C* Determino lo status della spedizione
040100100907     C                   EXSR      calSTS
040200100907     C*
040300100907     C* Costruisco il link diretto
040400130426     C                   EXSR      makLNK
040500130426     C*
040600130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
040700130426     C                   IF        %subst(prmppt:15:1) = 'P'
040800130426     C                   MOVE      vacCCM        CCM               4 0
040900130426     C                   MOVE      wKSC          KSC               4 0
041000130426     C                   IF        CCM =  8888 or  CCM =  9999
041100130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
041200130426     C                   EVAL      vacCCM = wKSC
041300130426     C                   ENDIF
041400130426     C                   ENDIF
041500130426     C                   ENDIF
041600100908     C*
041700100908     C                   SELECT
041800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
041900071121     C                   EVAL      wBufferOut = iRow +
042000100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
042100110519     C                                  iCol+%trim(wDSP)                  +fCol+
042200110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
042300100907     C                                  iCol+%trim(vacRSD)                +fCol+
042400110519     C                                  iCol+%trim(wRD2)                  +fCol+
042500110519     C                                  iCol+%trim(wIND)                  +fCol+
042600100517     C                                  iCol+%trim(wCAD)                  +fCol+
042700100517     C                                  iCol+%trim(wLOD)                  +fCol+
042800110519     C                                  iCol+%trim(wPRD)                  +fCol+
042900100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
043000110519     C                                  iCol+%trim(vacRMA)                +fCol+
043100121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
043200121207     C                                  iCol+%trim(wINDMORI)              +fCol+
043300121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
043400121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
043500121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
043600100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
043700130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
043800110519     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol
043900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
044000100908     C                   EVAL      wBufferOut =
044100110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
044200110519     C                              +'"'+   %trim(wDSP)                   +'";'
044300110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
044400110519     C                              +'"'+   %trim(vacRSD)                 +'";'
044500110519     C                              +'"'+   %trim(wRD2)                   +'";'
044600110519     C                              +'"'+   %trim(wIND)                   +'";'
044700110519     C                              +'"'+   %trim(wCAD)                   +'";'
044800110519     C                              +'"'+   %trim(wLOD)                   +'";'
044900110519     C                              +'"'+   %trim(wPRD)                   +'";'
045000110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
045100110519     C                              +'"'+   %trim(vacRMA)                 +'";'
045200121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
045300121207     C                              +'"'+   %trim(wINDMORI)               +'";'
045400121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
045500121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
045600121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
045700110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
045800130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
045900110519     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
046000100908     C                   ENDSL
046100071121     C*
046200120926     C* Se nei parametri è richiesto no MIC => gestisco
046300120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
046400120926     C                   SETON                                        31
046500120926     C                   ENDIF
046600121114     C*
046700121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
046800121207     C                   IF        %subst(prmppt:4:1) = 'S' and
046900121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
047000121114     C                   SETON                                        31
047100121114     C                   ENDIF
047200120926     C*
047300071121     C                   EVAL      VAWDTA = wBufferOut
047400110519     C*
047500110519     C* Restituisco solo le spedizioni "pendenti"
047600110713     C***                IF        wConsOkSiNo = 'N'
047700120926     C  N31              WRITE     TIVAW000
047800110713     C***                ENDIF
047900071121     C*
048000071121     C                   ENDSR
048100071121
048200071121
048300071121
048400991027
048500991027      /TITLE Operazioni iniziali.
048600991027     C     *inzsr        BEGSR
048700991027     C*
048800991027     C     *ENTRY        PLIST
048900991027     C                   parm                    prmppt
049000991027     C     wrkesito      parm      wrkesito      prmesito
049100100910     C*
049200100910     C* CALCOLA LA DATA CORRENTE
049300100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
049400100910     C                   eval      datcor = %dec(%date() : *ISO)
049500071210     C*
049600071213     C* TITAS30C - Parziale
049700071210     C     KEYtas30      KLIST
049800071210     C                   KFLD                    vacAAS
049900071210     C                   KFLD                    vacLNP
050000071210     C                   KFLD                    vacNRS
050100071210     C                   KFLD                    vacNSP
050200110519     C*
050300110519     C* TITA430C - Completa
050400110519     C     KEYta430      KLIST
050500110519     C                   KFLD                    vacAAS
050600110519     C                   KFLD                    vacLNP
050700110519     C                   KFLD                    vacNRS
050800110519     C                   KFLD                    vacNSP
050900110519     C                   KFLD                    ta4TRC
051000121207     C*
051100121207     C* TITAA30C - Completa
051200121207     C     KEYtaa30      KLIST
051300121207     C                   KFLD                    vacAAS
051400121207     C                   KFLD                    vacLNP
051500121207     C                   KFLD                    vacNRS
051600121207     C                   KFLD                    vacNSP
051700121207     C                   KFLD                    taaTRC
051800130913     C*
051900130913     C* TIGCP51L - Completa
052000130913     C     KEYgcp51      KLIST
052100130913     C                   KFLD                    vacAAS
052200130913     C                   KFLD                    vacLNP
052300130913     C                   KFLD                    vacNRS
052400130913     C                   KFLD                    vacNSP
052500130913     C                   KFLD                    gcpFRG
052600991027     C*
052700991027     C                   ENDSR
052800071121     C*
052900071121** DEC - COSTANTI
053000071121<?xml version="1.0"?>
053100071121<?mso-application progid="Excel.Sheet"?>
053200071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
053300071121xmlns:o="urn:schemas-microsoft-com:office:office"
053400071121xmlns:x="urn:schemas-microsoft-com:office:excel"
053500071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
053600071121xmlns:html="http://www.w3.org/TR/REC-html40">
053700071121</Workbook>
053800071121<Worksheet ss:Name="PAG1">
053900071121</Worksheet>
054000071121<Table border="1">
054100071121</Table>
054200071121<Row>
054300071121</Row>
054400071121<Cell><Data ss:Type="String">
054500071121<Cell><Data ss:Type="Number">
054600071121</Data></Cell>
054700071121
054800071121
054900071121
