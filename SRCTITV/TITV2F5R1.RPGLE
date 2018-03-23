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
001300140129     FFNDCT02L  IF   E           K DISK
001400030924
001500080924     D fnvac00t      e ds
001600071121
001700071121     D*------------
001800071121     D* DECODIFICHE COSTANTI
001900071121     D*------------
002000071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002100071121
002200071121
002300071121     D*------------
002400071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002500071121     D*------------
002600071121     D iHtml           s             10    inz('<html>')
002700071121     D fHtml           s             10    inz('</html>')
002800071121     D iBody           s             10    inz('<body>')
002900071121     D fBody           s             10    inz('</body>')
003000071121     D iTable          s             20    inz('<table border="1">')
003100071121     D fTable          s             10    inz('</table>')
003200071121     D iThead          s             10    inz('<thead>')
003300071121     D fThead          s             10    inz('</thead>')
003400071121     D iRow            s              5    inz('<tr>')
003500071121     D fRow            s              5    inz('</tr>')
003600071121     D iCol            s              5    inz('<td>')
003700071121     D fCol            s              5    inz('</td>')
003800071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003900071121     D fColBold        s              5    inz('</th>')
004000071122     D cApi            s              1    inz('''')
004100071121
004200071121
004300071121     D*------------
004400071121     D* VARIABILI D WRK
004500071121     D*------------
004600030924     D prmppt          S             50
004700030924     D prmesito        S              1
004800030924     D wrkesito        S                   like(prmesito)
004900071122     D wBufferOut      s          10000    varying inz
005000100910     D wDataSts        S              8  0 inz(*zeros)
005100100907     D wConsOkSiNo     S              1    inz(*blanks)
005200100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005300140129     D wDanni          S              1    inz(*blanks)
005400100907     D wLinkDir        S            300    inz(*blanks)
005500110519     D wDSP            s             10    inz(*blanks)
005600110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005700110519     D wIND            s                   inz(*blanks) like(tasIND)
005800110519     D wCAD            s                   inz(*blanks) like(tasIND)
005900110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006000110519     D wPRD            s                   inz(*blanks) like(tasPRD)
006100121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
006200121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
006300121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
006400121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
006500121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
006600130426     D wKSC            s              7  0 inz(*zeros)
006700071211     D*
006800100910
006900100910
007000100910     D*------------------
007100100910     D* LINKING A DEFINIZIONI ESTERNE
007200100910     D*------------------
007300100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007400100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007500130426
007600130426
007700130426     D*-------------------
007800130426     D* COSTANTI
007900130426     D*-------------------
008000130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200100910
008300030924
008400030325
008500030924     C* MAIN/
008600071121     C*
008700071121     C* Verifico se già presenti dati nel file d output
008800071121     C                   EXSR      chkRECFILMBR
008900071121     C*
009000071121     C* Effettuo elaborazione
009100071121     C                   EXSR      traduci
009200071121     C*
009300921023     C                   SETON                                        LR
009400991027
009500071121
009600071121
009700071121     C     chkRECFILMBR  BEGSR
009800071121     C*
009900071121     C                   READ      TIVAWWWT                               55
010000071121     C*
010100071121     C                   ENDSR
010200071121
010300071121
010400030924
010500991027     C     traduci       BEGSR
010600071121     C*
010700071121     C* Head iniziali della struttura HTML
010800100908     C                   IF        %subst(prmppt:2:1) = 'H'
010900071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
011000071210     C   55              WRITE     TIVAW000
011100100908     C                   ENDIF
011200071121     C*
011300071121     C* Inizializzazone formato e variabili x output
011400071121     C                   clear                   TIVAW000
011500071121     C                   clear                   wBufferOut
011600100907     C                   clear                   wLinkDir
011700100907     C                   clear                   wConsOkSiNo
011800100907     C                   clear                   wConsOkSiNoDes
011900071121     C*
012000071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
012100071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
012200100908     C*
012300100908     C                   SELECT
012400100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
012500071121     C                   EVAL      wBufferOut = iThead + iRow
012600110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
012700110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
012800110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
012900121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
013000110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
013100110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
013200110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
013300110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
013400110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
013500110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013600110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
013700121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
013800121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
013900121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
014000121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
014100121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
014200110519     C                             +iColBold+ 'STATO'                  +fColBold
014300130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
014400110519     C                             +iColBold+ 'DATA STATO'             +fColBold
014500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
014600100908     C                   EVAL      wBufferOut =
014700110519     C                              '"'+      'CODICE CLIENTE'         +'";'
014800110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
014900110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
015000110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
015100110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
015200110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
015300110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
015400110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
015500110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
015600110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
015700110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
015800121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
015900121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
016000121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
016100121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
016200121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
016300110519     C                             +'"'+      'STATO'                  +'";'
016400130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
016500110519     C                             +'"'+      'DATA STATO'             +'";'
016600100908     C                   ENDSL
016700100908     C*
016800071121     C                   EVAL      VAWDTA = wBufferOut
016900071121     C                   WRITE     TIVAW000
017000071121     C                   ENDIF
017100991027     C*
017200080924     C                   READ      TIVGDTMP
017300060119     C*
017400080924     C                   DOW       not %eof(TIVGDTMP)
017500080924     C*
017600080924     C                   eval      fnvac00t = vgdDTA
017700071121     C*
017800071121     C                   clear                   TIVAW000
017900071121     C                   clear                   wBufferOut
018000100907     C                   clear                   wLinkDir
018100100907     C                   clear                   wConsOkSiNo
018200100907     C                   clear                   wConsOkSiNoDes
018300140129     C                   clear                   wDanni
018400100910     C*
018500100910     C* Solo bolle originali e valide ai fini della spedizione
018600100910     C                   movel     *blanks       wEsito1           1
018700100910     C*
018800100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
018900100910     C                                                vacAAS
019000100910     C                                               :vacLNP
019100100910     C                                               :vacNRS
019200100910     C                                               :vacNSP
019300100910     C                                               :pOutLblTyp
019400100910     C                                               :pOutAnnoBO
019500100910     C                                               :pOutLineaParBO
019600100910     C                                               :pOutSerieBO
019700100910     C                                               :pOutNumSpedBO
019800100910     C                                               :pOutDcmBO
019900100910     C                                               :pOutCcaBO
020000100910     C                                               :pOutRblBO))
020100100910     C*
020200130426     C                   if         pOutRblBO ='N' AND
020300130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020400071210     C*
020500071210     C* Inizializzo campi d wrk
020600121207     C                   EVAL      wDSP     = *blanks
020700121207     C                   EVAL      wRD2     = *blanks
020800121207     C                   EVAL      wIND     = *blanks
020900121207     C                   EVAL      wCAD     = *blanks
021000121207     C                   EVAL      wLOD     = *blanks
021100121207     C                   EVAL      wPRD     = *blanks
021200121207     C                   EVAL      wRSCMORI = *blanks
021300121207     C                   EVAL      wINDMORI = *blanks
021400121207     C                   EVAL      wCAPMORI = *blanks
021500121207     C                   EVAL      wLOCMORI = *blanks
021600121207     C                   EVAL      wPRVMORI = *blanks
021700121207     C*
021800071210     C* Aggancio la bolla relativa x il completamento dei dati
021900071210     C     KEYtas30      chain     titas30c
022000071210     C                   if        %found(titas30c)
022100110519     C*
022200121207     C* Reperisco l'estensione ragione sociale destinatario
022300110519     C                   eval      ta4TRC = 'D'
022400110519     C     KEYta430      chain     tita430c
022500110519     C                   if        %found(tita430c)
022600110519     C                   eval      wRD2 = '.' + ta4NOT
022700110519     C                   endif
022800071210     C*
022900071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
023000110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
023100110519     C                   eval      wIND = tasIND
023200110519     C                   eval      wCAD = tasCAD
023300110519     C                   eval      wLOD = tasLOD
023400110519     C                   eval      wPRD = tasPRD
023500140129     C*
023600140129     C* Reperisco se presenti C/A in essere
023700140129     C     KEYdct02C     SETLL     fndct02l
023800140129     C                   IF        %equal(fndct02l)
023900140129     C     KEYdct02C     READE     fndct02l
024000140129     C                   DOW       not %eof(fndct02l)
024100140129     C* Se C/A NN annullata la considero
024200140129     C                   IF        dctATB = *blanks
024300140129     C                   EVAL      wDanni = 'S'
024400140129     C                   ENDIF
024500140129     C     KEYdct02C     READE     fndct02l
024600140129     C                   ENDDO
024700140129     C                   ENDIF
024800121207     C*
024900121207     C* Reperisco i dati del mittente originale
025000121207     C                   eval      taaTRC = 'O'
025100121207     C     KEYtaa30      chain     titaa30c
025200121207     C                   if        %found(titaa30c)
025300121207     C                   eval      wRSCMORI = taaRSC
025400121207     C                   eval      wINDMORI = taaIND
025500121207     C                   eval      wCAPMORI = taaCAP
025600121207     C                   eval      wLOCMORI = taaLOC
025700121207     C                   eval      wPRVMORI = taaPRV
025800121207     C                   endif
025900130913     C*
026000130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
026100130913     C* (costruisco la chiave sul file TIGCP51L)
026200130913     C                   setoff                                       70
026300130913     C*
026400130913     C                   EVAL      gcpFRG = *zeros
026500130913     C     KEYgcp51      SETLL     tigcp51l
026600130913     C                   IF        %equal(tigcp51l)
026700130913     C     KEYgcp51      READE     tigcp51l
026800130913     C                   DOW       not %eof(tigcp51l)
026900130913     C*
027000130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
027100130913     C                   IF        gcpDUR > *zeros AND
027200130913     C                             gcpDDM = *zeros AND
027300130913     C                             gcpFAS < 030
027400130913     C                   seton                                        70
027500130913     C                   ENDIF
027600130913     C*
027700130913     C* Proseguo la lettura delle giacenze x la bolla corrente
027800130913     C     KEYgcp51      READE     tigcp51l
027900130913     C                   ENDDO
028000130913     C                   ENDIF
028100071213     C*
028200071213     C                   endif
028300071121     C*
028400071121     C* Compondo la stringa dati output relativa al record corrente
028500071121     C                   EXSR      wrifile
028600100910     C*
028700100910     C                   endif
028800040719     C*
028900040719     C* In ogni caso storicizzo e cancello l'input sempre
029000080924     C                   DELETE    TIVGD000
029100040719     C*
029200040719     C* Proseguo con la lettura dell'input
029300080924     C                   READ      TIVGDTMP
029400060119     C*
029500030325     C                   ENDDO
029600071121     C*
029700071121     C* Head finali della struttura HTML
029800071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
029900071210     C***                WRITE     TIVAW000
030000991027     C*
030100030325     C                   EVAL      wrkesito = '0'
030200991027     C*
030300910830     C                   ENDSR
030400100907
030500100907
030600100907
030700100907     C     calSTS        BEGSR
030800100910     C*
030900120926     C                   MOVEL     *blanks       wMIC              1
031000121114     C                   MOVEL     *blanks       wGIAC             1
031100121207     C                   MOVEL     *blanks       wLAVV             1
031200140128     C                   MOVEL     *blanks       wPEND             1
031300100910     C                   EVAL      wDataSts       = vacDCM
031400100907     C*
031500100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
031600100907     C                   IF        VACDCM > *zeros AND
031700100907     C                             (VACCCA = *blanks OR VACCCA = '1')
031800100907     C                   EVAL      wConsOkSiNo    = 'S'
031900110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
032000100907     C                   ELSE
032100100907     C                   EVAL      wConsOkSiNo    = 'N'
032200100907     C*
032300100907     C* A questo punto stabilisco la descrizione
032400100910     C                   SELECT
032500100910     C                   WHEN      vacCCA = '1'
032600110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
032700100910     C                   WHEN      vacCCA = '2'
032800110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
032900100907     C                   EVAL      wConsOkSiNo    = 'R'
033000100910     C                   WHEN      vacCCA = '3'
033100110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
033200110519     C                   EVAL      wConsOkSiNo    = 'R'
033300100910     C                   WHEN      vacCCA = '4'
033400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
033500110519     C                   EVAL      wConsOkSiNo    = 'R'
033600100910     C                   WHEN      vacCCA = '5'
033700110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
033800110519     C                   EVAL      wConsOkSiNo    = 'R'
033900100910     C                   WHEN      vacCCA = '6'
034000110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
034100110519     C                   EVAL      wConsOkSiNo    = 'R'
034200100910     C                   WHEN      vacCCA = 'A'
034300110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
034400110519     C                   EVAL      wConsOkSiNo    = 'R'
034500100910     C                   WHEN      vacCCA = 'C'
034600110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
034700120926     C                   EVAL      wMIC = 'S'
034800100910     C                   WHEN      vacCCA = 'R'
034900110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
035000100910     C                   WHEN      vacCCA = 'S'
035100110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
035200110519     C                   EVAL      wConsOkSiNo    = 'R'
035300100910     C                   ENDSL
035400100907     C*
035500100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
035600100910     C                   EVAL      wDataSts       = vacDLA
035700110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
035800121207     C                   EVAL      wLAVV = 'S'
035900100907     C                   ENDIF
036000100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
036100100910     C                   EVAL      wDataSts       = vacDAG
036200110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
036300121114     C                   EVAL      wGIAC = 'S'
036400100907     C                   ENDIF
036500100910     C                   IF        vacDCM = *zeros AND
036600100910     C                             vacDLA = *zeros AND
036700100910     C                             vacDAG = *zeros AND
036800100910     C                             vacCCA = *blanks
036900100910     C                   EVAL      wDataSts       = datcor
037000100910     C*
037100100910     C* Verifico la data consegna richiesta
037200100910     C                   IF        vacDCR > datcor
037300110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
037400110519     C                                              'data ' + %editc(vacDCR:'X')
037500100910     C                   ELSE
037600110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
037700140128     C                   EVAL      wPEND          = 'S'
037800100910     C                   ENDIF
037900100910     C                   ENDIF
038000130913     C*
038100130913     C* Verifico lo stato delle disposizioni di giacenza
038200130913     C                   MOVEL     *blanks       wGiacDisp        10
038300130913     C   70              EVAL      wGiacDisp = 'IN ATTESA'
038400100907     C*
038500100907     C                   ENDIF
038600100907     C*
038700100907     C                   ENDSR
038800100907
038900100907
039000100907
039100100907     C     makLNK        BEGSR
039200130426     C*
039300130426     C* Se richiesta inclusione link T&T
039400130426     C                   if        %subst(prmppt:6:7) <> *blanks
039500130426     C*
039600130426     C* Porto in minuscolo il parametro relativo alla lingua del link
039700130426     C                   movel     *blanks       parLang           2
039800130426     C                   eval      parLang = %subst(prmppt:13:2)
039900130426     C     maiu:minu     xlate     parLang       parLang
040000130426     C*
040100160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
040200130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
040300130426     C                                        %trim(%char(vacRMN))             +
040400130426     C                                        '&ksu='+%subst(prmppt:6:7)+
040500130426     C                                        '&lang='+parLang
040600130426     C*
040700130426     C                   endif
040800100907     C*
040900100907     C                   ENDSR
041000100907
041100071121
041200071121
041300071121
041400071121     C     wrifile       BEGSR
041500120926     C*
041600120926     C                   SETOFF                                       31
041700071121     C*
041800100907     C* Determino lo status della spedizione
041900100907     C                   EXSR      calSTS
042000100907     C*
042100100907     C* Costruisco il link diretto
042200130426     C                   EXSR      makLNK
042300130426     C*
042400130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
042500130426     C                   IF        %subst(prmppt:15:1) = 'P'
042600130426     C                   MOVE      vacCCM        CCM               4 0
042700130426     C                   MOVE      wKSC          KSC               4 0
042800130426     C                   IF        CCM =  8888 or  CCM =  9999
042900130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
043000130426     C                   EVAL      vacCCM = wKSC
043100130426     C                   ENDIF
043200130426     C                   ENDIF
043300130426     C                   ENDIF
043400100908     C*
043500100908     C                   SELECT
043600100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
043700071121     C                   EVAL      wBufferOut = iRow +
043800100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
043900110519     C                                  iCol+%trim(wDSP)                  +fCol+
044000110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
044100100907     C                                  iCol+%trim(vacRSD)                +fCol+
044200110519     C                                  iCol+%trim(wRD2)                  +fCol+
044300110519     C                                  iCol+%trim(wIND)                  +fCol+
044400100517     C                                  iCol+%trim(wCAD)                  +fCol+
044500100517     C                                  iCol+%trim(wLOD)                  +fCol+
044600110519     C                                  iCol+%trim(wPRD)                  +fCol+
044700100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
044800110519     C                                  iCol+%trim(vacRMA)                +fCol+
044900121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
045000121207     C                                  iCol+%trim(wINDMORI)              +fCol+
045100121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
045200121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
045300121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
045400100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
045500130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
045600110519     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol
045700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
045800100908     C                   EVAL      wBufferOut =
045900110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
046000110519     C                              +'"'+   %trim(wDSP)                   +'";'
046100110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
046200110519     C                              +'"'+   %trim(vacRSD)                 +'";'
046300110519     C                              +'"'+   %trim(wRD2)                   +'";'
046400110519     C                              +'"'+   %trim(wIND)                   +'";'
046500110519     C                              +'"'+   %trim(wCAD)                   +'";'
046600110519     C                              +'"'+   %trim(wLOD)                   +'";'
046700110519     C                              +'"'+   %trim(wPRD)                   +'";'
046800110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
046900110519     C                              +'"'+   %trim(vacRMA)                 +'";'
047000121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
047100121207     C                              +'"'+   %trim(wINDMORI)               +'";'
047200121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
047300121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
047400121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
047500110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
047600130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
047700110519     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
047800100908     C                   ENDSL
047900071121     C*
048000120926     C* Se nei parametri è richiesto no MIC => gestisco
048100120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
048200120926     C                   SETON                                        31
048300120926     C                   ENDIF
048400121114     C*
048500121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
048600121207     C                   IF        %subst(prmppt:4:1) = 'S' and
048700121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
048800121114     C                   SETON                                        31
048900121114     C                   ENDIF
049000140124     C*
049100140128     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
049200140129     C                   IF        (%subst(prmppt:5:1)='S' and wPEND='S') OR    * tutte pending
049300140129     C                             (%subst(prmppt:5:1)='P' and wPEND='S'        * pending no danni
049400140129     C                              and wDanni <> 'S')                    OR
049500140129     C                             (%subst(prmppt:5:1)=*blanks)
049600140129     C                   ELSE
049700140124     C                   SETON                                        31
049800140124     C                   ENDIF
049900120926     C*
050000071121     C                   EVAL      VAWDTA = wBufferOut
050100110519     C*
050200110519     C* Restituisco solo le spedizioni "pendenti"
050300110713     C***                IF        wConsOkSiNo = 'N'
050400120926     C  N31              WRITE     TIVAW000
050500110713     C***                ENDIF
050600071121     C*
050700071121     C                   ENDSR
050800071121
050900071121
051000071121
051100991027
051200991027      /TITLE Operazioni iniziali.
051300991027     C     *inzsr        BEGSR
051400991027     C*
051500991027     C     *ENTRY        PLIST
051600991027     C                   parm                    prmppt
051700991027     C     wrkesito      parm      wrkesito      prmesito
051800100910     C*
051900100910     C* CALCOLA LA DATA CORRENTE
052000100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
052100100910     C                   eval      datcor = %dec(%date() : *ISO)
052200071210     C*
052300071213     C* TITAS30C - Parziale
052400071210     C     KEYtas30      KLIST
052500071210     C                   KFLD                    vacAAS
052600071210     C                   KFLD                    vacLNP
052700071210     C                   KFLD                    vacNRS
052800071210     C                   KFLD                    vacNSP
052900110519     C*
053000110519     C* TITA430C - Completa
053100110519     C     KEYta430      KLIST
053200110519     C                   KFLD                    vacAAS
053300110519     C                   KFLD                    vacLNP
053400110519     C                   KFLD                    vacNRS
053500110519     C                   KFLD                    vacNSP
053600110519     C                   KFLD                    ta4TRC
053700121207     C*
053800121207     C* TITAA30C - Completa
053900121207     C     KEYtaa30      KLIST
054000121207     C                   KFLD                    vacAAS
054100121207     C                   KFLD                    vacLNP
054200121207     C                   KFLD                    vacNRS
054300121207     C                   KFLD                    vacNSP
054400121207     C                   KFLD                    taaTRC
054500130913     C*
054600130913     C* TIGCP51L - Completa
054700130913     C     KEYgcp51      KLIST
054800130913     C                   KFLD                    vacAAS
054900130913     C                   KFLD                    vacLNP
055000130913     C                   KFLD                    vacNRS
055100130913     C                   KFLD                    vacNSP
055200130913     C                   KFLD                    gcpFRG
055300140129     C*
055400140129     C* FNDCT02L - Completa
055500140129     C     KEYdct02C     KLIST
055600140129     C                   KFLD                    vacAAS
055700140129     C                   KFLD                    vacLNP
055800140129     C                   KFLD                    vacNRS
055900140129     C                   KFLD                    vacNSP
056000991027     C*
056100991027     C                   ENDSR
056200071121     C*
056300071121** DEC - COSTANTI
056400071121<?xml version="1.0"?>
056500071121<?mso-application progid="Excel.Sheet"?>
056600071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
056700071121xmlns:o="urn:schemas-microsoft-com:office:office"
056800071121xmlns:x="urn:schemas-microsoft-com:office:excel"
056900071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
057000071121xmlns:html="http://www.w3.org/TR/REC-html40">
057100071121</Workbook>
057200071121<Worksheet ss:Name="PAG1">
057300071121</Worksheet>
057400071121<Table border="1">
057500071121</Table>
057600071121<Row>
057700071121</Row>
057800071121<Cell><Data ss:Type="String">
057900071121<Cell><Data ss:Type="Number">
058000071121</Data></Cell>
058100071121
058200071121
058300071121
