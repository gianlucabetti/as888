000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200140127     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
005900140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006000140127     D wNCA            s                   inz(*blanks) like(tasNCA)
006100140127     D wSPEDBRT        s             12  0 inz
006200121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
006300121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
006400121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
006500121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
006600121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
006700140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
006800130426     D wKSC            s              7  0 inz(*zeros)
006900071211     D*
007000100910
007100100910
007200100910     D*------------------
007300100910     D* LINKING A DEFINIZIONI ESTERNE
007400100910     D*------------------
007500100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007600100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007700130426
007800130426
007900130426     D*-------------------
008000130426     D* COSTANTI
008100130426     D*-------------------
008200130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400100910
008500030924
008600030325
008700030924     C* MAIN/
008800071121     C*
008900071121     C* Verifico se già presenti dati nel file d output
009000071121     C                   EXSR      chkRECFILMBR
009100071121     C*
009200071121     C* Effettuo elaborazione
009300071121     C                   EXSR      traduci
009400071121     C*
009500921023     C                   SETON                                        LR
009600991027
009700071121
009800071121
009900071121     C     chkRECFILMBR  BEGSR
010000071121     C*
010100071121     C                   READ      TIVAWWWT                               55
010200071121     C*
010300071121     C                   ENDSR
010400071121
010500071121
010600030924
010700991027     C     traduci       BEGSR
010800071121     C*
010900071121     C* Head iniziali della struttura HTML
011000100908     C                   IF        %subst(prmppt:2:1) = 'H'
011100071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
011200071210     C   55              WRITE     TIVAW000
011300100908     C                   ENDIF
011400071121     C*
011500071121     C* Inizializzazone formato e variabili x output
011600071121     C                   clear                   TIVAW000
011700071121     C                   clear                   wBufferOut
011800100907     C                   clear                   wLinkDir
011900100907     C                   clear                   wConsOkSiNo
012000100907     C                   clear                   wConsOkSiNoDes
012100071121     C*
012200071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
012300071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
012400100908     C*
012500100908     C                   SELECT
012600100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
012700071121     C                   EVAL      wBufferOut = iThead + iRow
012800110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
012900110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
013000110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
013100121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
013200110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
013300110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
013400110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
013500110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
013600110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
013700110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013800110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
013900121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
014000121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
014100121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
014200121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
014300121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
014400110519     C                             +iColBold+ 'STATO'                  +fColBold
014500130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
014600110519     C                             +iColBold+ 'DATA STATO'             +fColBold
014700140127     C                             +iColBold+ 'SEGNACOLLO DA'          +fColBold
014800140127     C                             +iColBold+ 'SEGNACOLLO A'           +fColBold
014900140127     C                             +iColBold+ 'NUM. SPED. BRT'         +fColBold
015000151125     C                             +iColBold+ 'ANNO SPED. BRT'         +fColBold
015100151125     C                             +iColBold+ 'LNP SPED. BRT'          +fColBold
015200151125     C                             +iColBold+ 'NRS SPED. BRT'          +fColBold
015300151125     C                             +iColBold+ 'NSP SPED. BRT'          +fColBold
015400140127     C                             +iColBold+ 'FIRMA CONSEGNA'         +fColBold
015500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015600100908     C                   EVAL      wBufferOut =
015700110519     C                              '"'+      'CODICE CLIENTE'         +'";'
015800110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
015900110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
016000110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
016100110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
016200110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
016300110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
016400110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
016500110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
016600110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
016700110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
016800121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
016900121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
017000121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
017100121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
017200121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
017300110519     C                             +'"'+      'STATO'                  +'";'
017400130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
017500110519     C                             +'"'+      'DATA STATO'             +'";'
017600140127     C                             +'"'+      'SEGNACOLLO DA'          +'";'
017700140127     C                             +'"'+      'SEGNACOLLO A'           +'";'
017800140127     C                             +'"'+      'NUM. SPED. BRT'         +'";'
017900151125     C                             +'"'+      'ANNO SPED. BRT'         +'";'
018000151125     C                             +'"'+      'LNP SPED. BRT'          +'";'
018100151125     C                             +'"'+      'NRS SPED. BRT'          +'";'
018200151125     C                             +'"'+      'NSP SPED. BRT'          +'";'
018300140127     C                             +'"'+      'FIRMA CONSEGNA'         +'";'
018400140127     C                   ENDSL
018500100908     C*
018600071121     C                   EVAL      VAWDTA = wBufferOut
018700071121     C                   WRITE     TIVAW000
018800071121     C                   ENDIF
018900991027     C*
019000080924     C                   READ      TIVGDTMP
019100060119     C*
019200080924     C                   DOW       not %eof(TIVGDTMP)
019300080924     C*
019400080924     C                   eval      fnvac00t = vgdDTA
019500071121     C*
019600071121     C                   clear                   TIVAW000
019700071121     C                   clear                   wBufferOut
019800100907     C                   clear                   wLinkDir
019900100907     C                   clear                   wConsOkSiNo
020000100907     C                   clear                   wConsOkSiNoDes
020100100910     C*
020200100910     C* Solo bolle originali e valide ai fini della spedizione
020300100910     C                   movel     *blanks       wEsito1           1
020400100910     C*
020500100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
020600100910     C                                                vacAAS
020700100910     C                                               :vacLNP
020800100910     C                                               :vacNRS
020900100910     C                                               :vacNSP
021000100910     C                                               :pOutLblTyp
021100100910     C                                               :pOutAnnoBO
021200100910     C                                               :pOutLineaParBO
021300100910     C                                               :pOutSerieBO
021400100910     C                                               :pOutNumSpedBO
021500100910     C                                               :pOutDcmBO
021600100910     C                                               :pOutCcaBO
021700100910     C                                               :pOutRblBO))
021800100910     C*
021900130426     C                   if         pOutRblBO ='N' AND
022000130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
022100071210     C*
022200071210     C* Inizializzo campi d wrk
022300140127     C                   EVAL      wDSP       = *blanks
022400140127     C                   EVAL      wRD2       = *blanks
022500140127     C                   EVAL      wIND       = *blanks
022600140127     C                   EVAL      wCAD       = *blanks
022700140127     C                   EVAL      wLOD       = *blanks
022800140127     C                   EVAL      wPRD       = *blanks
022900140127     C                   EVAL      wNCD       = *zeros
023000140127     C                   EVAL      wNCA       = *zeros
023100140127     C                   EVAL      wSPEDBRT   = *zeros
023200140127     C                   EVAL      wRSCMORI   = *blanks
023300140127     C                   EVAL      wINDMORI   = *blanks
023400140127     C                   EVAL      wCAPMORI   = *blanks
023500140127     C                   EVAL      wLOCMORI   = *blanks
023600140127     C                   EVAL      wPRVMORI   = *blanks
023700140129     C                   EVAL      wFIRMATAR  = *blanks
023800121207     C*
023900071210     C* Aggancio la bolla relativa x il completamento dei dati
024000071210     C     KEYtas30      chain     titas30c
024100071210     C                   if        %found(titas30c)
024200110519     C*
024300121207     C* Reperisco l'estensione ragione sociale destinatario
024400110519     C                   eval      ta4TRC = 'D'
024500140129     C     KEYta430C     chain     tita430c
024600110519     C                   if        %found(tita430c)
024700110519     C                   eval      wRD2 = '.' + ta4NOT
024800110519     C                   endif
024900140127     C*
025000140127     C* Reperisco, se c'è, la firma del consegnatario
025100140129     C                   eval      ta4TRC = '1'
025200140129     C     KEYta430C     chain     tita430c
025300140129     C                   if        %found(tita430c)
025400140129     C                   eval      wFIRMATAR = ta4NOT
025500140127     C                   endif
025600071210     C*
025700071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
025800140127     C                   eval      wDSP     = %editc((tasAAS*10000+tasMGS):'X')
025900140127     C                   eval      wIND     = tasIND
026000140127     C                   eval      wCAD     = tasCAD
026100140127     C                   eval      wLOD     = tasLOD
026200140127     C                   eval      wPRD     = tasPRD
026300140127     C                   eval      wNCD     = tasNCD
026400140127     C                   eval      wNCA     = tasNCA
026500140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
026600140127     C                                        tasNRS*10000000+
026700140127     C                                        tasNSP
026800121207     C*
026900121207     C* Reperisco i dati del mittente originale
027000121207     C                   eval      taaTRC = 'O'
027100121207     C     KEYtaa30      chain     titaa30c
027200121207     C                   if        %found(titaa30c)
027300121207     C                   eval      wRSCMORI = taaRSC
027400121207     C                   eval      wINDMORI = taaIND
027500121207     C                   eval      wCAPMORI = taaCAP
027600121207     C                   eval      wLOCMORI = taaLOC
027700121207     C                   eval      wPRVMORI = taaPRV
027800121207     C                   endif
027900130913     C*
028000130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
028100130913     C* (costruisco la chiave sul file TIGCP51L)
028200130913     C                   setoff                                       70
028300151125     C                   MOVEL     *blanks       wGiacDisp        10
028400130913     C*
028500130913     C                   EVAL      gcpFRG = *zeros
028600130913     C     KEYgcp51      SETLL     tigcp51l
028700130913     C                   IF        %equal(tigcp51l)
028800130913     C     KEYgcp51      READE     tigcp51l
028900130913     C                   DOW       not %eof(tigcp51l)
029000130913     C*
029100130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
029200130913     C                   IF        gcpDUR > *zeros AND
029300130913     C                             gcpDDM = *zeros AND
029400130913     C                             gcpFAS < 030
029500130913     C                   seton                                        70
029600130913     C                   ENDIF
029700130913     C*
029800130913     C* Proseguo la lettura delle giacenze x la bolla corrente
029900130913     C     KEYgcp51      READE     tigcp51l
030000130913     C                   ENDDO
030100130913     C                   ENDIF
030200071213     C*
030300071213     C                   endif
030400071121     C*
030500071121     C* Compondo la stringa dati output relativa al record corrente
030600071121     C                   EXSR      wrifile
030700100910     C*
030800100910     C                   endif
030900040719     C*
031000040719     C* In ogni caso storicizzo e cancello l'input sempre
031100080924     C                   DELETE    TIVGD000
031200040719     C*
031300040719     C* Proseguo con la lettura dell'input
031400080924     C                   READ      TIVGDTMP
031500060119     C*
031600030325     C                   ENDDO
031700071121     C*
031800071121     C* Head finali della struttura HTML
031900071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
032000071210     C***                WRITE     TIVAW000
032100991027     C*
032200030325     C                   EVAL      wrkesito = '0'
032300991027     C*
032400910830     C                   ENDSR
032500100907
032600100907
032700100907
032800100907     C     calSTS        BEGSR
032900100910     C*
033000120926     C                   MOVEL     *blanks       wMIC              1
033100121114     C                   MOVEL     *blanks       wGIAC             1
033200121207     C                   MOVEL     *blanks       wLAVV             1
033300140129     C                   MOVEL     *blanks       wPEND             1
033400100910     C                   EVAL      wDataSts       = vacDCM
033500100907     C*
033600100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
033700100907     C                   IF        VACDCM > *zeros AND
033800100907     C                             (VACCCA = *blanks OR VACCCA = '1')
033900100907     C                   EVAL      wConsOkSiNo    = 'S'
034000110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
034100100907     C                   ELSE
034200100907     C                   EVAL      wConsOkSiNo    = 'N'
034300100907     C*
034400100907     C* A questo punto stabilisco la descrizione
034500100910     C                   SELECT
034600100910     C                   WHEN      vacCCA = '1'
034700110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
034800100910     C                   WHEN      vacCCA = '2'
034900110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
035000100907     C                   EVAL      wConsOkSiNo    = 'R'
035100100910     C                   WHEN      vacCCA = '3'
035200110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
035300110519     C                   EVAL      wConsOkSiNo    = 'R'
035400100910     C                   WHEN      vacCCA = '4'
035500110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
035600110519     C                   EVAL      wConsOkSiNo    = 'R'
035700100910     C                   WHEN      vacCCA = '5'
035800110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
035900110519     C                   EVAL      wConsOkSiNo    = 'R'
036000100910     C                   WHEN      vacCCA = '6'
036100110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
036200110519     C                   EVAL      wConsOkSiNo    = 'R'
036300100910     C                   WHEN      vacCCA = 'A'
036400110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
036500110519     C                   EVAL      wConsOkSiNo    = 'R'
036600100910     C                   WHEN      vacCCA = 'C'
036700110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
036800120926     C                   EVAL      wMIC = 'S'
036900100910     C                   WHEN      vacCCA = 'R'
037000110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
037100100910     C                   WHEN      vacCCA = 'S'
037200110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
037300110519     C                   EVAL      wConsOkSiNo    = 'R'
037400100910     C                   ENDSL
037500100907     C*
037600100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
037700100910     C                   EVAL      wDataSts       = vacDLA
037800110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
037900121207     C                   EVAL      wLAVV = 'S'
038000100907     C                   ENDIF
038100100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
038200100910     C                   EVAL      wDataSts       = vacDAG
038300110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
038400121114     C                   EVAL      wGIAC = 'S'
038500100907     C                   ENDIF
038600100910     C                   IF        vacDCM = *zeros AND
038700100910     C                             vacDLA = *zeros AND
038800100910     C                             vacDAG = *zeros AND
038900100910     C                             vacCCA = *blanks
039000100910     C                   EVAL      wDataSts       = datcor
039100100910     C*
039200100910     C* Verifico la data consegna richiesta
039300100910     C                   IF        vacDCR > datcor
039400110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
039500110519     C                                              'data ' + %editc(vacDCR:'X')
039600100910     C                   ELSE
039700110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
039800140129     C                   EVAL      wPEND          = 'S'
039900100910     C                   ENDIF
040000100910     C                   ENDIF
040100130913     C*
040200130913     C* Verifico lo stato delle disposizioni di giacenza
040300130913     C   70              EVAL      wGiacDisp = 'IN ATTESA'
040400100907     C*
040500100907     C                   ENDIF
040600100907     C*
040700100907     C                   ENDSR
040800100907
040900100907
041000100907
041100100907     C     makLNK        BEGSR
041200130426     C*
041300130426     C* Se richiesta inclusione link T&T
041400130426     C                   if        %subst(prmppt:6:7) <> *blanks
041500130426     C*
041600130426     C* Porto in minuscolo il parametro relativo alla lingua del link
041700130426     C                   movel     *blanks       parLang           2
041800130426     C                   eval      parLang = %subst(prmppt:13:2)
041900130426     C     maiu:minu     xlate     parLang       parLang
042000130426     C*
042100160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
042200130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
042300130426     C                                        %trim(%char(vacRMN))             +
042400130426     C                                        '&ksu='+%subst(prmppt:6:7)+
042500130426     C                                        '&lang='+parLang
042600130426     C*
042700130426     C                   endif
042800100907     C*
042900100907     C                   ENDSR
043000100907
043100071121
043200071121
043300071121
043400071121     C     wrifile       BEGSR
043500120926     C*
043600120926     C                   SETOFF                                       31
043700071121     C*
043800100907     C* Determino lo status della spedizione
043900100907     C                   EXSR      calSTS
044000100907     C*
044100100907     C* Costruisco il link diretto
044200130426     C                   EXSR      makLNK
044300130426     C*
044400130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
044500130426     C                   IF        %subst(prmppt:15:1) = 'P'
044600130426     C                   MOVE      vacCCM        CCM               4 0
044700130426     C                   MOVE      wKSC          KSC               4 0
044800130426     C                   IF        CCM =  8888 or  CCM =  9999
044900130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
045000130426     C                   EVAL      vacCCM = wKSC
045100130426     C                   ENDIF
045200130426     C                   ENDIF
045300130426     C                   ENDIF
045400100908     C*
045500100908     C                   SELECT
045600100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
045700071121     C                   EVAL      wBufferOut = iRow +
045800100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
045900110519     C                                  iCol+%trim(wDSP)                  +fCol+
046000110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
046100100907     C                                  iCol+%trim(vacRSD)                +fCol+
046200110519     C                                  iCol+%trim(wRD2)                  +fCol+
046300110519     C                                  iCol+%trim(wIND)                  +fCol+
046400100517     C                                  iCol+%trim(wCAD)                  +fCol+
046500100517     C                                  iCol+%trim(wLOD)                  +fCol+
046600110519     C                                  iCol+%trim(wPRD)                  +fCol+
046700100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
046800110519     C                                  iCol+%trim(vacRMA)                +fCol+
046900121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
047000121207     C                                  iCol+%trim(wINDMORI)              +fCol+
047100121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
047200121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
047300121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
047400100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
047500130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
047600140127     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
047700140127     C                                  iCol+%trim(%editc(wNCD:'X'))      +fCol+
047800140127     C                                  iCol+%trim(%editc(wNCA:'X'))      +fCol+
047900140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
048000151125     C                                  iCol+%trim(%editc(vacAAS:'X'))    +fCol+
048100151125     C                                  iCol+%trim(%editc(vacLNP:'X'))    +fCol+
048200151125     C                                  iCol+%trim(%editc(vacNRS:'X'))    +fCol+
048300151125     C                                  iCol+%trim(%editc(vacNSP:'X'))    +fCol+
048400140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
048500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
048600100908     C                   EVAL      wBufferOut =
048700110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
048800110519     C                              +'"'+   %trim(wDSP)                   +'";'
048900110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
049000110519     C                              +'"'+   %trim(vacRSD)                 +'";'
049100110519     C                              +'"'+   %trim(wRD2)                   +'";'
049200110519     C                              +'"'+   %trim(wIND)                   +'";'
049300110519     C                              +'"'+   %trim(wCAD)                   +'";'
049400110519     C                              +'"'+   %trim(wLOD)                   +'";'
049500110519     C                              +'"'+   %trim(wPRD)                   +'";'
049600110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
049700110519     C                              +'"'+   %trim(vacRMA)                 +'";'
049800121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
049900121207     C                              +'"'+   %trim(wINDMORI)               +'";'
050000121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
050100121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
050200121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
050300110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
050400130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
050500140127     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
050600140127     C                              +'"'+   %trim(%editc(wNCD:'X'))       +'";'
050700140127     C                              +'"'+   %trim(%editc(wNCA:'X'))       +'";'
050800140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
050900151125     C                              +'"'+   %trim(%editc(vacAAS:'X'))     +'";'
051000151125     C                              +'"'+   %trim(%editc(vacLNP:'X'))     +'";'
051100151125     C                              +'"'+   %trim(%editc(vacNRS:'X'))     +'";'
051200151125     C                              +'"'+   %trim(%editc(vacNSP:'X'))     +'";'
051300140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
051400100908     C                   ENDSL
051500071121     C*
051600120926     C* Se nei parametri è richiesto no MIC => gestisco
051700120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
051800120926     C                   SETON                                        31
051900120926     C                   ENDIF
052000121114     C*
052100121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
052200121207     C                   IF        %subst(prmppt:4:1) = 'S' and
052300121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
052400121114     C                   SETON                                        31
052500121114     C                   ENDIF
052600140124     C*
052700140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
052800140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
052900140124     C                   SETON                                        31
053000140124     C                   ENDIF
053100120926     C*
053200071121     C                   EVAL      VAWDTA = wBufferOut
053300110519     C*
053400110519     C* Restituisco solo le spedizioni "pendenti"
053500110713     C***                IF        wConsOkSiNo = 'N'
053600120926     C  N31              WRITE     TIVAW000
053700110713     C***                ENDIF
053800071121     C*
053900071121     C                   ENDSR
054000071121
054100071121
054200071121
054300991027
054400991027      /TITLE Operazioni iniziali.
054500991027     C     *inzsr        BEGSR
054600991027     C*
054700991027     C     *ENTRY        PLIST
054800991027     C                   parm                    prmppt
054900991027     C     wrkesito      parm      wrkesito      prmesito
055000100910     C*
055100100910     C* CALCOLA LA DATA CORRENTE
055200100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055300100910     C                   eval      datcor = %dec(%date() : *ISO)
055400071210     C*
055500071213     C* TITAS30C - Parziale
055600071210     C     KEYtas30      KLIST
055700071210     C                   KFLD                    vacAAS
055800071210     C                   KFLD                    vacLNP
055900071210     C                   KFLD                    vacNRS
056000071210     C                   KFLD                    vacNSP
056100110519     C*
056200110519     C* TITA430C - Completa
056300140129     C     KEYta430C     KLIST
056400110519     C                   KFLD                    vacAAS
056500110519     C                   KFLD                    vacLNP
056600110519     C                   KFLD                    vacNRS
056700110519     C                   KFLD                    vacNSP
056800110519     C                   KFLD                    ta4TRC
056900121207     C*
057000121207     C* TITAA30C - Completa
057100121207     C     KEYtaa30      KLIST
057200121207     C                   KFLD                    vacAAS
057300121207     C                   KFLD                    vacLNP
057400121207     C                   KFLD                    vacNRS
057500121207     C                   KFLD                    vacNSP
057600121207     C                   KFLD                    taaTRC
057700130913     C*
057800130913     C* TIGCP51L - Completa
057900130913     C     KEYgcp51      KLIST
058000130913     C                   KFLD                    vacAAS
058100130913     C                   KFLD                    vacLNP
058200130913     C                   KFLD                    vacNRS
058300130913     C                   KFLD                    vacNSP
058400130913     C                   KFLD                    gcpFRG
058500991027     C*
058600991027     C                   ENDSR
058700071121     C*
058800071121** DEC - COSTANTI
058900071121<?xml version="1.0"?>
059000071121<?mso-application progid="Excel.Sheet"?>
059100071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
059200071121xmlns:o="urn:schemas-microsoft-com:office:office"
059300071121xmlns:x="urn:schemas-microsoft-com:office:excel"
059400071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
059500071121xmlns:html="http://www.w3.org/TR/REC-html40">
059600071121</Workbook>
059700071121<Worksheet ss:Name="PAG1">
059800071121</Worksheet>
059900071121<Table border="1">
060000071121</Table>
060100071121<Row>
060200071121</Row>
060300071121<Cell><Data ss:Type="String">
060400071121<Cell><Data ss:Type="Number">
060500071121</Data></Cell>
060600071121
060700071121
060800071121
