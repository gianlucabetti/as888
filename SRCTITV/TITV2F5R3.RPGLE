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
015000140127     C                             +iColBold+ 'FIRMA CONSEGNA'         +fColBold
015100100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015200100908     C                   EVAL      wBufferOut =
015300110519     C                              '"'+      'CODICE CLIENTE'         +'";'
015400110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
015500110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
015600110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
015700110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
015800110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
015900110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
016000110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
016100110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
016200110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
016300110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
016400121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
016500121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
016600121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
016700121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
016800121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
016900110519     C                             +'"'+      'STATO'                  +'";'
017000130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
017100110519     C                             +'"'+      'DATA STATO'             +'";'
017200140127     C                             +'"'+      'SEGNACOLLO DA'          +'";'
017300140127     C                             +'"'+      'SEGNACOLLO A'           +'";'
017400140127     C                             +'"'+      'NUM. SPED. BRT'         +'";'
017500140127     C                             +'"'+      'FIRMA CONSEGNA'         +'";'
017600140127     C                   ENDSL
017700100908     C*
017800071121     C                   EVAL      VAWDTA = wBufferOut
017900071121     C                   WRITE     TIVAW000
018000071121     C                   ENDIF
018100991027     C*
018200080924     C                   READ      TIVGDTMP
018300060119     C*
018400080924     C                   DOW       not %eof(TIVGDTMP)
018500080924     C*
018600080924     C                   eval      fnvac00t = vgdDTA
018700071121     C*
018800071121     C                   clear                   TIVAW000
018900071121     C                   clear                   wBufferOut
019000100907     C                   clear                   wLinkDir
019100100907     C                   clear                   wConsOkSiNo
019200100907     C                   clear                   wConsOkSiNoDes
019300100910     C*
019400100910     C* Solo bolle originali e valide ai fini della spedizione
019500100910     C                   movel     *blanks       wEsito1           1
019600100910     C*
019700100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019800100910     C                                                vacAAS
019900100910     C                                               :vacLNP
020000100910     C                                               :vacNRS
020100100910     C                                               :vacNSP
020200100910     C                                               :pOutLblTyp
020300100910     C                                               :pOutAnnoBO
020400100910     C                                               :pOutLineaParBO
020500100910     C                                               :pOutSerieBO
020600100910     C                                               :pOutNumSpedBO
020700100910     C                                               :pOutDcmBO
020800100910     C                                               :pOutCcaBO
020900100910     C                                               :pOutRblBO))
021000100910     C*
021100130426     C                   if         pOutRblBO ='N' AND
021200130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
021300071210     C*
021400071210     C* Inizializzo campi d wrk
021500140127     C                   EVAL      wDSP       = *blanks
021600140127     C                   EVAL      wRD2       = *blanks
021700140127     C                   EVAL      wIND       = *blanks
021800140127     C                   EVAL      wCAD       = *blanks
021900140127     C                   EVAL      wLOD       = *blanks
022000140127     C                   EVAL      wPRD       = *blanks
022100140127     C                   EVAL      wNCD       = *zeros
022200140127     C                   EVAL      wNCA       = *zeros
022300140127     C                   EVAL      wSPEDBRT   = *zeros
022400140127     C                   EVAL      wRSCMORI   = *blanks
022500140127     C                   EVAL      wINDMORI   = *blanks
022600140127     C                   EVAL      wCAPMORI   = *blanks
022700140127     C                   EVAL      wLOCMORI   = *blanks
022800140127     C                   EVAL      wPRVMORI   = *blanks
022900140129     C                   EVAL      wFIRMATAR  = *blanks
023000121207     C*
023100071210     C* Aggancio la bolla relativa x il completamento dei dati
023200071210     C     KEYtas30      chain     titas30c
023300071210     C                   if        %found(titas30c)
023400110519     C*
023500121207     C* Reperisco l'estensione ragione sociale destinatario
023600110519     C                   eval      ta4TRC = 'D'
023700140129     C     KEYta430C     chain     tita430c
023800110519     C                   if        %found(tita430c)
023900110519     C                   eval      wRD2 = '.' + ta4NOT
024000110519     C                   endif
024100140127     C*
024200140127     C* Reperisco, se c'è, la firma del consegnatario
024300140129     C                   eval      ta4TRC = '1'
024400140129     C     KEYta430C     chain     tita430c
024500140129     C                   if        %found(tita430c)
024600140129     C                   eval      wFIRMATAR = ta4NOT
024700140127     C                   endif
024800071210     C*
024900071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
025000140127     C                   eval      wDSP     = %editc((tasAAS*10000+tasMGS):'X')
025100140127     C                   eval      wIND     = tasIND
025200140127     C                   eval      wCAD     = tasCAD
025300140127     C                   eval      wLOD     = tasLOD
025400140127     C                   eval      wPRD     = tasPRD
025500140127     C                   eval      wNCD     = tasNCD
025600140127     C                   eval      wNCA     = tasNCA
025700140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
025800140127     C                                        tasNRS*10000000+
025900140127     C                                        tasNSP
026000121207     C*
026100121207     C* Reperisco i dati del mittente originale
026200121207     C                   eval      taaTRC = 'O'
026300121207     C     KEYtaa30      chain     titaa30c
026400121207     C                   if        %found(titaa30c)
026500121207     C                   eval      wRSCMORI = taaRSC
026600121207     C                   eval      wINDMORI = taaIND
026700121207     C                   eval      wCAPMORI = taaCAP
026800121207     C                   eval      wLOCMORI = taaLOC
026900121207     C                   eval      wPRVMORI = taaPRV
027000121207     C                   endif
027100130913     C*
027200130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
027300130913     C* (costruisco la chiave sul file TIGCP51L)
027400130913     C                   setoff                                       70
027500151125     C                   MOVEL     *blanks       wGiacDisp        10
027600130913     C*
027700130913     C                   EVAL      gcpFRG = *zeros
027800130913     C     KEYgcp51      SETLL     tigcp51l
027900130913     C                   IF        %equal(tigcp51l)
028000130913     C     KEYgcp51      READE     tigcp51l
028100130913     C                   DOW       not %eof(tigcp51l)
028200130913     C*
028300130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
028400130913     C                   IF        gcpDUR > *zeros AND
028500130913     C                             gcpDDM = *zeros AND
028600130913     C                             gcpFAS < 030
028700130913     C                   seton                                        70
028800130913     C                   ENDIF
028900130913     C*
029000130913     C* Proseguo la lettura delle giacenze x la bolla corrente
029100130913     C     KEYgcp51      READE     tigcp51l
029200130913     C                   ENDDO
029300130913     C                   ENDIF
029400071213     C*
029500071213     C                   endif
029600071121     C*
029700071121     C* Compondo la stringa dati output relativa al record corrente
029800071121     C                   EXSR      wrifile
029900100910     C*
030000100910     C                   endif
030100040719     C*
030200040719     C* In ogni caso storicizzo e cancello l'input sempre
030300080924     C                   DELETE    TIVGD000
030400040719     C*
030500040719     C* Proseguo con la lettura dell'input
030600080924     C                   READ      TIVGDTMP
030700060119     C*
030800030325     C                   ENDDO
030900071121     C*
031000071121     C* Head finali della struttura HTML
031100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
031200071210     C***                WRITE     TIVAW000
031300991027     C*
031400030325     C                   EVAL      wrkesito = '0'
031500991027     C*
031600910830     C                   ENDSR
031700100907
031800100907
031900100907
032000100907     C     calSTS        BEGSR
032100100910     C*
032200120926     C                   MOVEL     *blanks       wMIC              1
032300121114     C                   MOVEL     *blanks       wGIAC             1
032400121207     C                   MOVEL     *blanks       wLAVV             1
032500140129     C                   MOVEL     *blanks       wPEND             1
032600100910     C                   EVAL      wDataSts       = vacDCM
032700100907     C*
032800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
032900100907     C                   IF        VACDCM > *zeros AND
033000100907     C                             (VACCCA = *blanks OR VACCCA = '1')
033100100907     C                   EVAL      wConsOkSiNo    = 'S'
033200110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
033300100907     C                   ELSE
033400100907     C                   EVAL      wConsOkSiNo    = 'N'
033500100907     C*
033600100907     C* A questo punto stabilisco la descrizione
033700100910     C                   SELECT
033800100910     C                   WHEN      vacCCA = '1'
033900110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
034000100910     C                   WHEN      vacCCA = '2'
034100110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
034200100907     C                   EVAL      wConsOkSiNo    = 'R'
034300100910     C                   WHEN      vacCCA = '3'
034400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
034500110519     C                   EVAL      wConsOkSiNo    = 'R'
034600100910     C                   WHEN      vacCCA = '4'
034700110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
034800110519     C                   EVAL      wConsOkSiNo    = 'R'
034900100910     C                   WHEN      vacCCA = '5'
035000110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
035100110519     C                   EVAL      wConsOkSiNo    = 'R'
035200100910     C                   WHEN      vacCCA = '6'
035300110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
035400110519     C                   EVAL      wConsOkSiNo    = 'R'
035500100910     C                   WHEN      vacCCA = 'A'
035600110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
035700110519     C                   EVAL      wConsOkSiNo    = 'R'
035800100910     C                   WHEN      vacCCA = 'C'
035900110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
036000120926     C                   EVAL      wMIC = 'S'
036100100910     C                   WHEN      vacCCA = 'R'
036200110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
036300100910     C                   WHEN      vacCCA = 'S'
036400110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
036500110519     C                   EVAL      wConsOkSiNo    = 'R'
036600100910     C                   ENDSL
036700100907     C*
036800100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
036900100910     C                   EVAL      wDataSts       = vacDLA
037000110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
037100121207     C                   EVAL      wLAVV = 'S'
037200100907     C                   ENDIF
037300100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
037400100910     C                   EVAL      wDataSts       = vacDAG
037500110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
037600121114     C                   EVAL      wGIAC = 'S'
037700100907     C                   ENDIF
037800100910     C                   IF        vacDCM = *zeros AND
037900100910     C                             vacDLA = *zeros AND
038000100910     C                             vacDAG = *zeros AND
038100100910     C                             vacCCA = *blanks
038200100910     C                   EVAL      wDataSts       = datcor
038300100910     C*
038400100910     C* Verifico la data consegna richiesta
038500100910     C                   IF        vacDCR > datcor
038600110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
038700110519     C                                              'data ' + %editc(vacDCR:'X')
038800100910     C                   ELSE
038900110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
039000140129     C                   EVAL      wPEND          = 'S'
039100100910     C                   ENDIF
039200100910     C                   ENDIF
039300130913     C*
039400130913     C* Verifico lo stato delle disposizioni di giacenza
039500130913     C   70              EVAL      wGiacDisp = 'IN ATTESA'
039600100907     C*
039700100907     C                   ENDIF
039800100907     C*
039900100907     C                   ENDSR
040000100907
040100100907
040200100907
040300100907     C     makLNK        BEGSR
040400130426     C*
040500130426     C* Se richiesta inclusione link T&T
040600130426     C                   if        %subst(prmppt:6:7) <> *blanks
040700130426     C*
040800130426     C* Porto in minuscolo il parametro relativo alla lingua del link
040900130426     C                   movel     *blanks       parLang           2
041000130426     C                   eval      parLang = %subst(prmppt:13:2)
041100130426     C     maiu:minu     xlate     parLang       parLang
041200130426     C*
041300160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
041400130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
041500130426     C                                        %trim(%char(vacRMN))             +
041600130426     C                                        '&ksu='+%subst(prmppt:6:7)+
041700130426     C                                        '&lang='+parLang
041800130426     C*
041900130426     C                   endif
042000100907     C*
042100100907     C                   ENDSR
042200100907
042300071121
042400071121
042500071121
042600071121     C     wrifile       BEGSR
042700120926     C*
042800120926     C                   SETOFF                                       31
042900071121     C*
043000100907     C* Determino lo status della spedizione
043100100907     C                   EXSR      calSTS
043200100907     C*
043300100907     C* Costruisco il link diretto
043400130426     C                   EXSR      makLNK
043500130426     C*
043600130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
043700130426     C                   IF        %subst(prmppt:15:1) = 'P'
043800130426     C                   MOVE      vacCCM        CCM               4 0
043900130426     C                   MOVE      wKSC          KSC               4 0
044000130426     C                   IF        CCM =  8888 or  CCM =  9999
044100130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
044200130426     C                   EVAL      vacCCM = wKSC
044300130426     C                   ENDIF
044400130426     C                   ENDIF
044500130426     C                   ENDIF
044600100908     C*
044700100908     C                   SELECT
044800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
044900071121     C                   EVAL      wBufferOut = iRow +
045000100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
045100110519     C                                  iCol+%trim(wDSP)                  +fCol+
045200110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
045300100907     C                                  iCol+%trim(vacRSD)                +fCol+
045400110519     C                                  iCol+%trim(wRD2)                  +fCol+
045500110519     C                                  iCol+%trim(wIND)                  +fCol+
045600100517     C                                  iCol+%trim(wCAD)                  +fCol+
045700100517     C                                  iCol+%trim(wLOD)                  +fCol+
045800110519     C                                  iCol+%trim(wPRD)                  +fCol+
045900100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
046000110519     C                                  iCol+%trim(vacRMA)                +fCol+
046100121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
046200121207     C                                  iCol+%trim(wINDMORI)              +fCol+
046300121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
046400121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
046500121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
046600100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
046700130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
046800140127     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
046900140127     C                                  iCol+%trim(%editc(wNCD:'X'))      +fCol+
047000140127     C                                  iCol+%trim(%editc(wNCA:'X'))      +fCol+
047100140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
047200140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
047300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
047400100908     C                   EVAL      wBufferOut =
047500110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
047600110519     C                              +'"'+   %trim(wDSP)                   +'";'
047700110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
047800110519     C                              +'"'+   %trim(vacRSD)                 +'";'
047900110519     C                              +'"'+   %trim(wRD2)                   +'";'
048000110519     C                              +'"'+   %trim(wIND)                   +'";'
048100110519     C                              +'"'+   %trim(wCAD)                   +'";'
048200110519     C                              +'"'+   %trim(wLOD)                   +'";'
048300110519     C                              +'"'+   %trim(wPRD)                   +'";'
048400110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
048500110519     C                              +'"'+   %trim(vacRMA)                 +'";'
048600121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
048700121207     C                              +'"'+   %trim(wINDMORI)               +'";'
048800121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
048900121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
049000121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
049100110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
049200130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
049300140127     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
049400140127     C                              +'"'+   %trim(%editc(wNCD:'X'))       +'";'
049500140127     C                              +'"'+   %trim(%editc(wNCA:'X'))       +'";'
049600140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
049700140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
049800100908     C                   ENDSL
049900071121     C*
050000120926     C* Se nei parametri è richiesto no MIC => gestisco
050100120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
050200120926     C                   SETON                                        31
050300120926     C                   ENDIF
050400121114     C*
050500121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
050600121207     C                   IF        %subst(prmppt:4:1) = 'S' and
050700121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
050800121114     C                   SETON                                        31
050900121114     C                   ENDIF
051000140124     C*
051100140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
051200140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
051300140124     C                   SETON                                        31
051400140124     C                   ENDIF
051500120926     C*
051600071121     C                   EVAL      VAWDTA = wBufferOut
051700110519     C*
051800110519     C* Restituisco solo le spedizioni "pendenti"
051900110713     C***                IF        wConsOkSiNo = 'N'
052000120926     C  N31              WRITE     TIVAW000
052100110713     C***                ENDIF
052200071121     C*
052300071121     C                   ENDSR
052400071121
052500071121
052600071121
052700991027
052800991027      /TITLE Operazioni iniziali.
052900991027     C     *inzsr        BEGSR
053000991027     C*
053100991027     C     *ENTRY        PLIST
053200991027     C                   parm                    prmppt
053300991027     C     wrkesito      parm      wrkesito      prmesito
053400100910     C*
053500100910     C* CALCOLA LA DATA CORRENTE
053600100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
053700100910     C                   eval      datcor = %dec(%date() : *ISO)
053800071210     C*
053900071213     C* TITAS30C - Parziale
054000071210     C     KEYtas30      KLIST
054100071210     C                   KFLD                    vacAAS
054200071210     C                   KFLD                    vacLNP
054300071210     C                   KFLD                    vacNRS
054400071210     C                   KFLD                    vacNSP
054500110519     C*
054600110519     C* TITA430C - Completa
054700140129     C     KEYta430C     KLIST
054800110519     C                   KFLD                    vacAAS
054900110519     C                   KFLD                    vacLNP
055000110519     C                   KFLD                    vacNRS
055100110519     C                   KFLD                    vacNSP
055200110519     C                   KFLD                    ta4TRC
055300121207     C*
055400121207     C* TITAA30C - Completa
055500121207     C     KEYtaa30      KLIST
055600121207     C                   KFLD                    vacAAS
055700121207     C                   KFLD                    vacLNP
055800121207     C                   KFLD                    vacNRS
055900121207     C                   KFLD                    vacNSP
056000121207     C                   KFLD                    taaTRC
056100130913     C*
056200130913     C* TIGCP51L - Completa
056300130913     C     KEYgcp51      KLIST
056400130913     C                   KFLD                    vacAAS
056500130913     C                   KFLD                    vacLNP
056600130913     C                   KFLD                    vacNRS
056700130913     C                   KFLD                    vacNSP
056800130913     C                   KFLD                    gcpFRG
056900991027     C*
057000991027     C                   ENDSR
057100071121     C*
057200071121** DEC - COSTANTI
057300071121<?xml version="1.0"?>
057400071121<?mso-application progid="Excel.Sheet"?>
057500071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
057600071121xmlns:o="urn:schemas-microsoft-com:office:office"
057700071121xmlns:x="urn:schemas-microsoft-com:office:excel"
057800071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
057900071121xmlns:html="http://www.w3.org/TR/REC-html40">
058000071121</Workbook>
058100071121<Worksheet ss:Name="PAG1">
058200071121</Worksheet>
058300071121<Table border="1">
058400071121</Table>
058500071121<Row>
058600071121</Row>
058700071121<Cell><Data ss:Type="String">
058800071121<Cell><Data ss:Type="Number">
058900071121</Data></Cell>
059000071121
059100071121
059200071121
