000100140306     H DECEDIT('0,') DATEDIT(*DMY.)
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
001200160705     FCNIND00F  IF   E           K DISK
001300130913     FTIGCP51L  IF   E           K DISK
001400140129     FFNDCT02L  IF   E           K DISK
001500140310     FTABEL00F  IF   E           K DISK
001600030924
001700080924     D fnvac00t      e ds
001800140310     D ds2A          e ds                  inz
001900160705
002000160705
002100160705     D*-------------------
002200160705     D* DS PIANO DEI CONTI
002300160705     D*-------------------
002400160705     D bs69ds        E DS                  EXTNAME(tibs69ds) INZ
002500160705     D acods         E DS                  EXTNAME(cnaco00f) INZ
002600160705     D indds         E DS                  EXTNAME(cnind00f) INZ
002700160705     D clpds         E DS                  EXTNAME(cnclp00f) INZ
002800160705     D clsds         E DS                  EXTNAME(fncls00f) INZ
002900071121
003000071121     D*------------
003100071121     D* DECODIFICHE COSTANTI
003200071121     D*------------
003300071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
003400071121
003500071121
003600071121     D*------------
003700071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003800071121     D*------------
003900071121     D iHtml           s             10    inz('<html>')
004000071121     D fHtml           s             10    inz('</html>')
004100071121     D iBody           s             10    inz('<body>')
004200071121     D fBody           s             10    inz('</body>')
004300071121     D iTable          s             20    inz('<table border="1">')
004400071121     D fTable          s             10    inz('</table>')
004500071121     D iThead          s             10    inz('<thead>')
004600071121     D fThead          s             10    inz('</thead>')
004700071121     D iRow            s              5    inz('<tr>')
004800071121     D fRow            s              5    inz('</tr>')
004900071121     D iCol            s              5    inz('<td>')
005000071121     D fCol            s              5    inz('</td>')
005100071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
005200071121     D fColBold        s              5    inz('</th>')
005300071122     D cApi            s              1    inz('''')
005400071121
005500071121
005600071121     D*------------
005700071121     D* VARIABILI D WRK
005800071121     D*------------
005900030924     D prmppt          S             50
006000030924     D prmesito        S              1
006100030924     D wrkesito        S                   like(prmesito)
006200071122     D wBufferOut      s          10000    varying inz
006300100910     D wDataSts        S              8  0 inz(*zeros)
006400100907     D wConsOkSiNo     S              1    inz(*blanks)
006500100907     D wConsOkSiNoDes  S            300    inz(*blanks)
006600140129     D wDanni          S              1    inz(*blanks)
006700100907     D wLinkDir        S            300    inz(*blanks)
006800110519     D wDSP            s             10    inz(*blanks)
006900110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
007000110519     D wIND            s                   inz(*blanks) like(tasIND)
007100110519     D wCAD            s                   inz(*blanks) like(tasIND)
007200110519     D wLOD            s                   inz(*blanks) like(tasLOD)
007300110519     D wPRD            s                   inz(*blanks) like(tasPRD)
007400121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007500121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007600121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007700121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007800121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007900130426     D wKSC            s              7  0 inz(*zeros)
008000140310     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
008100140310     D wGiacCau        s                   inz(*blanks) like(§2ADES)
008200071211     D*
008300100910
008400100910
008500100910     D*------------------
008600100910     D* LINKING A DEFINIZIONI ESTERNE
008700100910     D*------------------
008800100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008900100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009000130426
009100130426
009200130426     D*-------------------
009300130426     D* COSTANTI
009400130426     D*-------------------
009500130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100910
009800030924
009900030325
010000030924     C* MAIN/
010100071121     C*
010200071121     C* Verifico se già presenti dati nel file d output
010300071121     C                   EXSR      chkRECFILMBR
010400071121     C*
010500071121     C* Effettuo elaborazione
010600071121     C                   EXSR      traduci
010700071121     C*
010800921023     C                   SETON                                        LR
010900991027
011000071121
011100071121
011200071121     C     chkRECFILMBR  BEGSR
011300071121     C*
011400071121     C                   READ      TIVAWWWT                               55
011500071121     C*
011600071121     C                   ENDSR
011700071121
011800071121
011900030924
012000991027     C     traduci       BEGSR
012100071121     C*
012200071121     C* Head iniziali della struttura HTML
012300100908     C                   IF        %subst(prmppt:2:1) = 'H'
012400071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
012500071210     C   55              WRITE     TIVAW000
012600100908     C                   ENDIF
012700071121     C*
012800071121     C* Inizializzazone formato e variabili x output
012900071121     C                   clear                   TIVAW000
013000071121     C                   clear                   wBufferOut
013100100907     C                   clear                   wLinkDir
013200100907     C                   clear                   wConsOkSiNo
013300100907     C                   clear                   wConsOkSiNoDes
013400071121     C*
013500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
013700100908     C*
013800100908     C                   SELECT
013900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014000071121     C                   EVAL      wBufferOut = iThead + iRow
014100110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
014200110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
014300140306     C                             +iColBold+ 'NUM.SPED. BRT'          +fColBold
014400110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
014500121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
014600110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
014700110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
014800110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
014900110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
015000110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
015100110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
015200110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
015300140306     C                             +iColBold+ 'TOT COLLI'              +fColBold
015400140306     C                             +iColBold+ 'PESO SPED.'             +fColBold
015500121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
015600121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
015700121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
015800121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
015900121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
016000140310     C                             +iColBold+ 'DATA STATO'             +fColBold
016100110519     C                             +iColBold+ 'STATO'                  +fColBold
016200130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
016300140310     C                             +iColBold+ 'CAUSALE GIAC.'          +fColBold
016400100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
016500100908     C                   EVAL      wBufferOut =
016600110519     C                              '"'+      'CODICE CLIENTE'         +'";'
016700110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
016800140306     C                             +'"'+      'NUM.SPED. BRT'          +'";'
016900110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
017000110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
017100110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
017200110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
017300110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
017400110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
017500110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
017600110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
017700110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
017800140306     C                             +'"'+      'TOT COLLI'              +'";'
017900140306     C                             +'"'+      'PESO SPED.'             +'";'
018000121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
018100121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
018200121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
018300121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
018400121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
018500140310     C                             +'"'+      'DATA STATO'             +'";'
018600110519     C                             +'"'+      'STATO'                  +'";'
018700130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
018800140310     C                             +'"'+      'CAUSALE GIAC.'          +'";'
018900100908     C                   ENDSL
019000100908     C*
019100071121     C                   EVAL      VAWDTA = wBufferOut
019200071121     C                   WRITE     TIVAW000
019300071121     C                   ENDIF
019400991027     C*
019500080924     C                   READ      TIVGDTMP
019600060119     C*
019700080924     C                   DOW       not %eof(TIVGDTMP)
019800080924     C*
019900080924     C                   eval      fnvac00t = vgdDTA
020000071121     C*
020100071121     C                   clear                   TIVAW000
020200071121     C                   clear                   wBufferOut
020300100907     C                   clear                   wLinkDir
020400100907     C                   clear                   wConsOkSiNo
020500100907     C                   clear                   wConsOkSiNoDes
020600140129     C                   clear                   wDanni
020700100910     C*
020800100910     C* Solo bolle originali e valide ai fini della spedizione
020900100910     C                   movel     *blanks       wEsito1           1
021000100910     C*
021100100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
021200100910     C                                                vacAAS
021300100910     C                                               :vacLNP
021400100910     C                                               :vacNRS
021500100910     C                                               :vacNSP
021600100910     C                                               :pOutLblTyp
021700100910     C                                               :pOutAnnoBO
021800100910     C                                               :pOutLineaParBO
021900100910     C                                               :pOutSerieBO
022000100910     C                                               :pOutNumSpedBO
022100100910     C                                               :pOutDcmBO
022200100910     C                                               :pOutCcaBO
022300100910     C                                               :pOutRblBO))
022400100910     C*
022500130426     C                   if         pOutRblBO ='N' AND
022600130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
022700071210     C*
022800071210     C* Inizializzo campi d wrk
022900121207     C                   EVAL      wDSP     = *blanks
023000121207     C                   EVAL      wRD2     = *blanks
023100121207     C                   EVAL      wIND     = *blanks
023200121207     C                   EVAL      wCAD     = *blanks
023300121207     C                   EVAL      wLOD     = *blanks
023400121207     C                   EVAL      wPRD     = *blanks
023500121207     C                   EVAL      wRSCMORI = *blanks
023600121207     C                   EVAL      wINDMORI = *blanks
023700121207     C                   EVAL      wCAPMORI = *blanks
023800121207     C                   EVAL      wLOCMORI = *blanks
023900121207     C                   EVAL      wPRVMORI = *blanks
024000140310     C                   EVAL      wGCPCMC  = *blanks
024100140310     C                   EVAL      wGiacCau = *blanks
024200121207     C*
024300071210     C* Aggancio la bolla relativa x il completamento dei dati
024400071210     C     KEYtas30      chain     titas30c
024500071210     C                   if        %found(titas30c)
024600110519     C*
024700121207     C* Reperisco l'estensione ragione sociale destinatario
024800110519     C                   eval      ta4TRC = 'D'
024900110519     C     KEYta430      chain     tita430c
025000110519     C                   if        %found(tita430c)
025100110519     C                   eval      wRD2 = '.' + ta4NOT
025200110519     C                   endif
025300071210     C*
025400071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
025500110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
025600110519     C                   eval      wIND = tasIND
025700110519     C                   eval      wCAD = tasCAD
025800110519     C                   eval      wLOD = tasLOD
025900110519     C                   eval      wPRD = tasPRD
026000140129     C*
026100140129     C* Reperisco se presenti C/A in essere
026200140129     C     KEYdct02C     SETLL     fndct02l
026300140129     C                   IF        %equal(fndct02l)
026400140129     C     KEYdct02C     READE     fndct02l
026500140129     C                   DOW       not %eof(fndct02l)
026600140129     C* Se C/A NN annullata la considero
026700140129     C                   IF        dctATB = *blanks
026800140129     C                   EVAL      wDanni = 'S'
026900140129     C                   ENDIF
027000140129     C     KEYdct02C     READE     fndct02l
027100140129     C                   ENDDO
027200140129     C                   ENDIF
027300121207     C*
027400121207     C* Reperisco i dati del mittente originale
027500121207     C                   eval      taaTRC = 'O'
027600121207     C     KEYtaa30      chain     titaa30c
027700121207     C                   if        %found(titaa30c)
027800121207     C                   eval      wRSCMORI = taaRSC
027900121207     C                   eval      wINDMORI = taaIND
028000121207     C                   eval      wCAPMORI = taaCAP
028100121207     C                   eval      wLOCMORI = taaLOC
028200121207     C                   eval      wPRVMORI = taaPRV
028300160705     C                   else
028400160705     C*
028500160705     C* Se richiesto nei parametri di input, se no mittente originale => mittente
028600160705     C                   if        %subst(prmppt:17:1)='S'
028700160705     C* Reperisco le informazioni anagrafiche
028800160705     C                   clear                   bs69ds
028900160705     C                   clear                   acods
029000160705     C                   clear                   indds
029100160705     C                   clear                   clpds
029200160705     C                   clear                   clsds
029300160705     C                   z-add     vacCCM        I69KAC
029400160705     C                   z-add     vacCCM        I69KIN
029500160705     C                   call      'TIBS69R'
029600160705     C                   parm                    bs69ds
029700160705     C                   parm                    acods
029800160705     C                   parm                    indds
029900160705     C                   parm                    clpds
030000160705     C                   parm                    clsds
030100160705     C     o69err        ifne      '1'
030200160705     C                   eval      wRSCMORI = ACORAG
030300160705     C                   eval      wINDMORI = INDVIA
030400160705     C                   eval      wCAPMORI = %editc(INDCAP:'X')
030500160705     C                   eval      wLOCMORI = INDCIT
030600160705     C                   eval      wPRVMORI = INDPRV
030700160705     C                   endif
030800160705     C                   endif
030900121207     C                   endif
031000130913     C*
031100130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
031200130913     C* (costruisco la chiave sul file TIGCP51L)
031300130913     C                   setoff                                       70
031400130913     C*
031500140310     C                   EVAL      gcpDUR = *zeros
031600130913     C                   EVAL      gcpFRG = *zeros
031700130913     C     KEYgcp51      SETLL     tigcp51l
031800130913     C                   IF        %equal(tigcp51l)
031900130913     C     KEYgcp51      READE     tigcp51l
032000130913     C                   DOW       not %eof(tigcp51l)
032100130913     C*
032200130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
032300130913     C                   IF        gcpDUR > *zeros AND
032400130913     C                             gcpDDM = *zeros AND
032500130913     C                             gcpFAS < 030
032600130913     C                   seton                                        70
032700130913     C                   ENDIF
032800140310     C*
032900140310     C* Reperisco la causale di ultima apertura giacenza
033000140310     C                   EVAL      wGCPCMC = gcpCMC
033100130913     C*
033200130913     C* Proseguo la lettura delle giacenze x la bolla corrente
033300130913     C     KEYgcp51      READE     tigcp51l
033400130913     C                   ENDDO
033500130913     C                   ENDIF
033600071213     C*
033700071213     C                   endif
033800071121     C*
033900071121     C* Compondo la stringa dati output relativa al record corrente
034000071121     C                   EXSR      wrifile
034100100910     C*
034200100910     C                   endif
034300040719     C*
034400040719     C* In ogni caso storicizzo e cancello l'input sempre
034500080924     C                   DELETE    TIVGD000
034600040719     C*
034700040719     C* Proseguo con la lettura dell'input
034800080924     C                   READ      TIVGDTMP
034900060119     C*
035000030325     C                   ENDDO
035100071121     C*
035200071121     C* Head finali della struttura HTML
035300071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
035400071210     C***                WRITE     TIVAW000
035500991027     C*
035600030325     C                   EVAL      wrkesito = '0'
035700991027     C*
035800910830     C                   ENDSR
035900100907
036000100907
036100100907
036200100907     C     calSTS        BEGSR
036300100910     C*
036400120926     C                   MOVEL     *blanks       wMIC              1
036500121114     C                   MOVEL     *blanks       wGIAC             1
036600121207     C                   MOVEL     *blanks       wLAVV             1
036700140128     C                   MOVEL     *blanks       wPEND             1
036800100910     C                   EVAL      wDataSts       = vacDCM
036900140310     C*
037000140310     C* Verifico lo stato delle disposizioni di giacenza
037100140310     C                   MOVEL     *blanks       wGiacDisp        10
037200140310     C   70              EVAL      wGiacDisp = 'IN ATTESA'
037300100907     C*
037400100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
037500100907     C                   IF        VACDCM > *zeros AND
037600100907     C                             (VACCCA = *blanks OR VACCCA = '1')
037700100907     C                   EVAL      wConsOkSiNo    = 'S'
037800110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
037900140310     C                   EVAL      wGiacDisp      = *blanks
038000100907     C                   ELSE
038100100907     C                   EVAL      wConsOkSiNo    = 'N'
038200100907     C*
038300100907     C* A questo punto stabilisco la descrizione
038400100910     C                   SELECT
038500100910     C                   WHEN      vacCCA = '1'
038600110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
038700100910     C                   WHEN      vacCCA = '2'
038800110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
038900100907     C                   EVAL      wConsOkSiNo    = 'R'
039000100910     C                   WHEN      vacCCA = '3'
039100110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
039200110519     C                   EVAL      wConsOkSiNo    = 'R'
039300100910     C                   WHEN      vacCCA = '4'
039400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
039500110519     C                   EVAL      wConsOkSiNo    = 'R'
039600100910     C                   WHEN      vacCCA = '5'
039700110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
039800110519     C                   EVAL      wConsOkSiNo    = 'R'
039900100910     C                   WHEN      vacCCA = '6'
040000110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
040100110519     C                   EVAL      wConsOkSiNo    = 'R'
040200100910     C                   WHEN      vacCCA = 'A'
040300110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
040400110519     C                   EVAL      wConsOkSiNo    = 'R'
040500100910     C                   WHEN      vacCCA = 'C'
040600110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
040700120926     C                   EVAL      wMIC = 'S'
040800100910     C                   WHEN      vacCCA = 'R'
040900110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
041000100910     C                   WHEN      vacCCA = 'S'
041100110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
041200110519     C                   EVAL      wConsOkSiNo    = 'R'
041300100910     C                   ENDSL
041400100907     C*
041500140310     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
041600100910     C                   EVAL      wDataSts       = vacDLA
041700110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
041800121207     C                   EVAL      wLAVV = 'S'
041900100907     C                   ENDIF
042000140310     C                   IF        (gcpDUR > vacDCM AND vacDAG >= vacDLA)
042100140310     C                   EVAL      wDataSts       = gcpDUR
042200110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
042300140310     C                   EXSR      rtvCAU_GIAC
042400121114     C                   EVAL      wGIAC = 'S'
042500140310     C                   ELSE
042600140310     C                   EVAL      wGiacDisp      = *blanks
042700100907     C                   ENDIF
042800100910     C                   IF        vacDCM = *zeros AND
042900100910     C                             vacDLA = *zeros AND
043000140310     C                             gcpDUR = *zeros AND
043100100910     C                             vacCCA = *blanks
043200100910     C                   EVAL      wDataSts       = datcor
043300100910     C*
043400100910     C* Verifico la data consegna richiesta
043500100910     C                   IF        vacDCR > datcor
043600110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
043700110519     C                                              'data ' + %editc(vacDCR:'X')
043800100910     C                   ELSE
043900110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
044000140128     C                   EVAL      wPEND          = 'S'
044100100910     C                   ENDIF
044200100910     C                   ENDIF
044300100907     C*
044400100907     C                   ENDIF
044500100907     C*
044600100907     C                   ENDSR
044700140310
044800140310
044900140310     C     rtvCAU_GIAC   BEGSR
045000140310     C*
045100140310     C* Rererisco la descrizioe della causale di giacenza
045200140310     C                   MOVEL     '1'           tblKUT
045300140310     C                   MOVEL     '2A'          tblCOD
045400140310     C                   MOVEL(P)  wGCPCMC       tblKEY
045500140310     C     KEYtab00_C    CHAIN     TABEL00F                           30
045600140310     C                   IF        %found(tabel00f)
045700140310     C                   MOVEL     TBLUNI        ds2A
045800140310     C                   MOVEL(P)  §2ADES        wGiacCau
045900140310     C                   ENDIF
046000140310     C*
046100140310     C                   ENDSR
046200140310     C*------------------------------------------------------------------------*
046300140310
046400100907
046500100907
046600100907
046700100907     C     makLNK        BEGSR
046800130426     C*
046900130426     C* Se richiesta inclusione link T&T
047000130426     C                   if        %subst(prmppt:6:7) <> *blanks
047100130426     C*
047200130426     C* Porto in minuscolo il parametro relativo alla lingua del link
047300130426     C                   movel     *blanks       parLang           2
047400130426     C                   eval      parLang = %subst(prmppt:13:2)
047500130426     C     maiu:minu     xlate     parLang       parLang
047600130426     C*
047700160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
047800130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
047900130426     C                                        %trim(%char(vacRMN))             +
048000130426     C                                        '&ksu='+%subst(prmppt:6:7)+
048100130426     C                                        '&lang='+parLang
048200130426     C*
048300130426     C                   endif
048400100907     C*
048500100907     C                   ENDSR
048600100907
048700071121
048800071121
048900071121
049000071121     C     wrifile       BEGSR
049100120926     C*
049200120926     C                   SETOFF                                       31
049300071121     C*
049400100907     C* Determino lo status della spedizione
049500100907     C                   EXSR      calSTS
049600100907     C*
049700100907     C* Costruisco il link diretto
049800130426     C                   EXSR      makLNK
049900130426     C*
050000130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
050100130426     C                   IF        %subst(prmppt:15:1) = 'P'
050200130426     C                   MOVE      vacCCM        CCM               4 0
050300130426     C                   MOVE      wKSC          KSC               4 0
050400130426     C                   IF        CCM =  8888 or  CCM =  9999
050500130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
050600130426     C                   EVAL      vacCCM = wKSC
050700130426     C                   ENDIF
050800130426     C                   ENDIF
050900130426     C                   ENDIF
051000100908     C*
051100100908     C                   SELECT
051200100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
051300071121     C                   EVAL      wBufferOut = iRow +
051400100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
051500110519     C                                  iCol+%trim(wDSP)                  +fCol+
051600140306     C                                  iCol+%trim(%editc(vacLNP:'X'))    +
051700140306     C                                       %trim(%editc(vacNRS:'X'))    +
051800140306     C                                       %trim(%editc(vacNSP:'X'))    +fCol+
051900110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
052000100907     C                                  iCol+%trim(vacRSD)                +fCol+
052100110519     C                                  iCol+%trim(wRD2)                  +fCol+
052200110519     C                                  iCol+%trim(wIND)                  +fCol+
052300100517     C                                  iCol+%trim(wCAD)                  +fCol+
052400100517     C                                  iCol+%trim(wLOD)                  +fCol+
052500110519     C                                  iCol+%trim(wPRD)                  +fCol+
052600100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
052700140306     C                                  iCol+%trim(%editc(vacNCL:'3'))    +fCol+
052800140306     C                                  iCol+%trim(%editc(vacPKB:'3'))    +fCol+
052900110519     C                                  iCol+%trim(vacRMA)                +fCol+
053000121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
053100121207     C                                  iCol+%trim(wINDMORI)              +fCol+
053200121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
053300121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
053400121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
053500140310     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
053600100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
053700140310     C                                  iCol+%trim(wGiacDisp)             +fCol+
053800140310     C                                  iCol+%trim(wGiacCau)              +fCol
053900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
054000100908     C                   EVAL      wBufferOut =
054100110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
054200110519     C                              +'"'+   %trim(wDSP)                   +'";'
054300140306     C                              +'"'+   %trim(%editc(vacLNP:'X'))     +
054400140306     C                                      %trim(%editc(vacNRS:'X'))     +
054500140306     C                                      %trim(%editc(vacNSP:'X'))     +'";'
054600110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
054700110519     C                              +'"'+   %trim(vacRSD)                 +'";'
054800110519     C                              +'"'+   %trim(wRD2)                   +'";'
054900110519     C                              +'"'+   %trim(wIND)                   +'";'
055000110519     C                              +'"'+   %trim(wCAD)                   +'";'
055100110519     C                              +'"'+   %trim(wLOD)                   +'";'
055200110519     C                              +'"'+   %trim(wPRD)                   +'";'
055300110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
055400110519     C                              +'"'+   %trim(vacRMA)                 +'";'
055500140306     C                              +'"'+   %trim(%editc(vacNCL:'3'))     +'";'
055600140306     C                              +'"'+   %trim(%editc(vacPKB:'3'))     +'";'
055700121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
055800121207     C                              +'"'+   %trim(wINDMORI)               +'";'
055900121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
056000121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
056100121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
056200140310     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
056300110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
056400130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
056500140310     C                              +'"'+   %trim(wGiacCau)               +'";'
056600100908     C                   ENDSL
056700071121     C*
056800120926     C* Se nei parametri è richiesto no MIC => gestisco
056900120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
057000120926     C                   SETON                                        31
057100120926     C                   ENDIF
057200121114     C*
057300121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
057400121207     C                   IF        %subst(prmppt:4:1) = 'S' and
057500121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
057600121114     C                   SETON                                        31
057700121114     C                   ENDIF
057800140124     C*
057900140128     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
058000140129     C                   IF        (%subst(prmppt:5:1)='S' and wPEND='S') OR    * tutte pending
058100140129     C                             (%subst(prmppt:5:1)='P' and wPEND='S'        * pending no danni
058200140129     C                              and wDanni <> 'S')                    OR
058300140129     C                             (%subst(prmppt:5:1)=*blanks)
058400140129     C                   ELSE
058500140124     C                   SETON                                        31
058600140124     C                   ENDIF
058700120926     C*
058800071121     C                   EVAL      VAWDTA = wBufferOut
058900110519     C*
059000110519     C* Restituisco solo le spedizioni "pendenti"
059100110713     C***                IF        wConsOkSiNo = 'N'
059200120926     C  N31              WRITE     TIVAW000
059300110713     C***                ENDIF
059400071121     C*
059500071121     C                   ENDSR
059600071121
059700071121
059800071121
059900991027
060000991027      /TITLE Operazioni iniziali.
060100991027     C     *inzsr        BEGSR
060200991027     C*
060300991027     C     *ENTRY        PLIST
060400991027     C                   parm                    prmppt
060500991027     C     wrkesito      parm      wrkesito      prmesito
060600100910     C*
060700100910     C* CALCOLA LA DATA CORRENTE
060800100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060900100910     C                   eval      datcor = %dec(%date() : *ISO)
061000071210     C*
061100071213     C* TITAS30C - Parziale
061200071210     C     KEYtas30      KLIST
061300071210     C                   KFLD                    vacAAS
061400071210     C                   KFLD                    vacLNP
061500071210     C                   KFLD                    vacNRS
061600071210     C                   KFLD                    vacNSP
061700110519     C*
061800110519     C* TITA430C - Completa
061900110519     C     KEYta430      KLIST
062000110519     C                   KFLD                    vacAAS
062100110519     C                   KFLD                    vacLNP
062200110519     C                   KFLD                    vacNRS
062300110519     C                   KFLD                    vacNSP
062400110519     C                   KFLD                    ta4TRC
062500121207     C*
062600121207     C* TITAA30C - Completa
062700121207     C     KEYtaa30      KLIST
062800121207     C                   KFLD                    vacAAS
062900121207     C                   KFLD                    vacLNP
063000121207     C                   KFLD                    vacNRS
063100121207     C                   KFLD                    vacNSP
063200121207     C                   KFLD                    taaTRC
063300130913     C*
063400130913     C* TIGCP51L - Completa
063500130913     C     KEYgcp51      KLIST
063600130913     C                   KFLD                    vacAAS
063700130913     C                   KFLD                    vacLNP
063800130913     C                   KFLD                    vacNRS
063900130913     C                   KFLD                    vacNSP
064000130913     C                   KFLD                    gcpFRG
064100140129     C*
064200140129     C* FNDCT02L - Completa
064300140129     C     KEYdct02C     KLIST
064400140129     C                   KFLD                    vacAAS
064500140129     C                   KFLD                    vacLNP
064600140129     C                   KFLD                    vacNRS
064700140129     C                   KFLD                    vacNSP
064800140310     C*
064900140310     C* TABEL00F - Completa
065000140310     C     KEYtab00_C    KLIST
065100140310     C                   KFLD                    tblKUT
065200140310     C                   KFLD                    tblCOD
065300140310     C                   KFLD                    tblKEY
065400160705     C*
065500160705     C* CNIND00F - Completa
065600160705     C     KEYind00_C    KLIST
065700160705     C                   KFLD                    indKUT
065800160705     C                   KFLD                    indKCC
065900160705     C                   KFLD                    indKSC
066000991027     C*
066100991027     C                   ENDSR
066200071121     C*
066300071121** DEC - COSTANTI
066400071121<?xml version="1.0"?>
066500071121<?mso-application progid="Excel.Sheet"?>
066600071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
066700071121xmlns:o="urn:schemas-microsoft-com:office:office"
066800071121xmlns:x="urn:schemas-microsoft-com:office:excel"
066900071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
067000071121xmlns:html="http://www.w3.org/TR/REC-html40">
067100071121</Workbook>
067200071121<Worksheet ss:Name="PAG1">
067300071121</Worksheet>
067400071121<Table border="1">
067500071121</Table>
067600071121<Row>
067700071121</Row>
067800071121<Cell><Data ss:Type="String">
067900071121<Cell><Data ss:Type="Number">
068000071121</Data></Cell>
068100071121
068200071121
068300071121
