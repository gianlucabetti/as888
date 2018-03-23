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
001300140308     FFIAR531C  IF   E           K DISK
001400140308     FTIGNP01L  IF   E           K DISK
001500140311     FTABEL00F  IF   E           K DISK
001600030924
001700080924     D fnvac00t      e ds
001800140308     D dar5gen       e ds
001900140308     D dar5emd       e ds
002000140311     D ds2A          e ds                  inz
002100071121
002200071121     D*------------
002300071121     D* DECODIFICHE COSTANTI
002400071121     D*------------
002500071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002600071121
002700071121
002800071121     D*------------
002900071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003000071121     D*------------
003100071121     D iHtml           s             10    inz('<html>')
003200071121     D fHtml           s             10    inz('</html>')
003300071121     D iBody           s             10    inz('<body>')
003400071121     D fBody           s             10    inz('</body>')
003500071121     D iTable          s             20    inz('<table border="1">')
003600071121     D fTable          s             10    inz('</table>')
003700071121     D iThead          s             10    inz('<thead>')
003800071121     D fThead          s             10    inz('</thead>')
003900071121     D iRow            s              5    inz('<tr>')
004000071121     D fRow            s              5    inz('</tr>')
004100071121     D iCol            s              5    inz('<td>')
004200071121     D fCol            s              5    inz('</td>')
004300071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004400071121     D fColBold        s              5    inz('</th>')
004500071122     D cApi            s              1    inz('''')
004600071121
004700071121
004800071121     D*------------
004900071121     D* VARIABILI D WRK
005000071121     D*------------
005100030924     D prmppt          S             50
005200030924     D prmesito        S              1
005300030924     D wrkesito        S                   like(prmesito)
005400071122     D wBufferOut      s          10000    varying inz
005500100910     D wDataSts        S              8  0 inz(*zeros)
005600140308     D wDataStsAlfa    S             10    inz(*blanks)
005700100907     D wConsOkSiNo     S              1    inz(*blanks)
005800100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005900140613     D wConsOkSiNoCod  S              3    inz(*blanks)
006000100907     D wLinkDir        S            300    inz(*blanks)
006100110519     D wDSP            s             10    inz(*blanks)
006200110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
006300110519     D wIND            s                   inz(*blanks) like(tasIND)
006400110519     D wCAD            s                   inz(*blanks) like(tasIND)
006500110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006600171004     D wPRDNZD         s                   inz(*blanks) like(tasNZD)
006700140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006800140127     D wNCA            s                   inz(*blanks) like(tasNCA)
006900140127     D wSPEDBRT        s             12  0 inz
007000121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007100121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007200121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007300121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007400121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007500140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
007600130426     D wKSC            s              7  0 inz(*zeros)
007700140308     D wNoteGiac       s            300    inz(*blanks)
007800140311     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
007900140311     D wGiacCau        s                   inz(*blanks) like(§2ADES)
008000071211     D*
008100100910
008200100910
008300100910     D*------------------
008400100910     D* LINKING A DEFINIZIONI ESTERNE
008500100910     D*------------------
008600100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008700100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008800140308     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008900140308     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
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
010100140308     C*
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
014300110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
014400121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
014500140308     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
014600140308     C                             +iColBold+ 'REFERENTE CONSEGNA'     +fColBold
014700140308     C                             +iColBold+ 'TEL. REFERENTE'         +fColBold
014800110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
014900110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
015000110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
015100110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
015200110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
015300110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
015400140308     C***                          +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
015500140308     C***                          +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
015600140308     C***                          +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
015700140308     C***                          +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
015800140308     C***                          +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
015900140308     C                             +iColBold+ 'DATA STATO'             +fColBold
016000110519     C                             +iColBold+ 'STATO'                  +fColBold
016100140613     C                             +iColBold+ 'COD. STATO'             +fColBold
016200130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
016300140311     C                             +iColBold+ 'CAUSALE GIAC.'          +fColBold
016400140308     C                             +iColBold+ 'NOTE STATO'             +fColBold
016500140308     C***                          +iColBold+ 'SEGNACOLLO DA'          +fColBold
016600140308     C***                          +iColBold+ 'SEGNACOLLO A'           +fColBold
016700140308     C                             +iColBold+ 'LDV BRT'                +fColBold
016800140127     C                             +iColBold+ 'FIRMA CONSEGNA'         +fColBold
016900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
017000100908     C                   EVAL      wBufferOut =
017100110519     C                              '"'+      'CODICE CLIENTE'         +'";'
017200110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
017300110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
017400140308     C                             +'"'+      'RAG.SOC. DESTINATARIO'  +'";'
017500140308     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
017600140308     C                             +'"'+      'REFERENTE CONSEGNA'     +'";'
017700140308     C                             +'"'+      'TEL. REFERENTE'         +'";'
017800110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
017900110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
018000110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
018100110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
018200110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
018300110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
018400140308     C***                          +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
018500140308     C***                          +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
018600140308     C***                          +'"'+      'CAP MITTENTE ORIG.'     +'";'
018700140308     C***                          +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
018800140308     C***                          +'"'+      'PROV. MITTENTE ORIG.'   +'";'
018900140308     C                             +'"'+      'DATA STATO'             +'";'
019000110519     C                             +'"'+      'STATO'                  +'";'
019100140613     C                             +'"'+      'COD. STATO'             +'";'
019200130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
019300140311     C                             +'"'+      'CAUSALE GIAC.'          +'";'
019400140308     C                             +'"'+      'NOTE STATO'             +'";'
019500140308     C***                          +'"'+      'SEGNACOLLO DA'          +'";'
019600140308     C***                          +'"'+      'SEGNACOLLO A'           +'";'
019700140308     C                             +'"'+      'LDV BRT'                +'";'
019800140127     C                             +'"'+      'FIRMA CONSEGNA'         +'";'
019900140127     C                   ENDSL
020000100908     C*
020100071121     C                   EVAL      VAWDTA = wBufferOut
020200071121     C                   WRITE     TIVAW000
020300071121     C                   ENDIF
020400991027     C*
020500080924     C                   READ      TIVGDTMP
020600060119     C*
020700080924     C                   DOW       not %eof(TIVGDTMP)
020800080924     C*
020900080924     C                   eval      fnvac00t = vgdDTA
021000071121     C*
021100071121     C                   clear                   TIVAW000
021200071121     C                   clear                   wBufferOut
021300100907     C                   clear                   wLinkDir
021400100907     C                   clear                   wConsOkSiNo
021500100907     C                   clear                   wConsOkSiNoDes
021600140308     C                   clear                   dar5gen
021700140308     C                   clear                   dar5emd
021800100910     C*
021900100910     C* Solo bolle originali e valide ai fini della spedizione
022000100910     C                   movel     *blanks       wEsito1           1
022100100910     C*
022200100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
022300100910     C                                                vacAAS
022400100910     C                                               :vacLNP
022500100910     C                                               :vacNRS
022600100910     C                                               :vacNSP
022700100910     C                                               :pOutLblTyp
022800100910     C                                               :pOutAnnoBO
022900100910     C                                               :pOutLineaParBO
023000100910     C                                               :pOutSerieBO
023100100910     C                                               :pOutNumSpedBO
023200100910     C                                               :pOutDcmBO
023300100910     C                                               :pOutCcaBO
023400100910     C                                               :pOutRblBO))
023500100910     C*
023600130426     C                   if         pOutRblBO ='N' AND
023700130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
023800071210     C*
023900071210     C* Inizializzo campi d wrk
024000140127     C                   EVAL      wDSP       = *blanks
024100140127     C                   EVAL      wRD2       = *blanks
024200140127     C                   EVAL      wIND       = *blanks
024300140127     C                   EVAL      wCAD       = *blanks
024400140127     C                   EVAL      wLOD       = *blanks
024500171004     C                   EVAL      wPRDNZD    = *blanks
024600140127     C                   EVAL      wNCD       = *zeros
024700140127     C                   EVAL      wNCA       = *zeros
024800140127     C                   EVAL      wSPEDBRT   = *zeros
024900140127     C                   EVAL      wRSCMORI   = *blanks
025000140127     C                   EVAL      wINDMORI   = *blanks
025100140127     C                   EVAL      wCAPMORI   = *blanks
025200140127     C                   EVAL      wLOCMORI   = *blanks
025300140127     C                   EVAL      wPRVMORI   = *blanks
025400140129     C                   EVAL      wFIRMATAR  = *blanks
025500140308     C                   EVAL      wNoteGiac  = *blanks
025600140311     C                   EVAL      wGCPCMC    = *blanks
025700140311     C                   EVAL      wGiacCau   = *blanks
025800140308     C                   setoff                                       70
025900121207     C*
026000071210     C* Aggancio la bolla relativa x il completamento dei dati
026100071210     C     KEYtas30      chain     titas30c
026200071210     C                   if        %found(titas30c)
026300110519     C*
026400121207     C* Reperisco l'estensione ragione sociale destinatario
026500110519     C                   eval      ta4TRC = 'D'
026600140129     C     KEYta430C     chain     tita430c
026700110519     C                   if        %found(tita430c)
026800110519     C                   eval      wRD2 = '.' + ta4NOT
026900110519     C                   endif
027000140127     C*
027100140127     C* Reperisco, se c'è, la firma del consegnatario
027200140129     C                   eval      ta4TRC = '1'
027300140129     C     KEYta430C     chain     tita430c
027400140129     C                   if        %found(tita430c)
027500140129     C                   eval      wFIRMATAR = ta4NOT
027600140127     C                   endif
027700140308     C*
027800140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
027900140308     C                   eval      ar5TRD = 'GEN'
028000140308     C     KEYar531_1P   chain     fiar531C
028100140308     C                   if        %found(FIAR531C)
028200140308     C                   eval      dar5gen = ar5UNI
028300140308     C                   endif
028400140308     C*
028500140308     C* Reperisco, se c'è, il cellulare del destinatario
028600140308     C                   eval      ar5TRD = 'EMD'
028700140308     C     KEYar531_1P   chain     fiar531C
028800140308     C                   if        %found(FIAR531C)
028900140308     C                   eval      dar5emd = ar5UNI
029000140308     C                   endif
029100071210     C*
029200071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
029300140308     C                   eval      wDSP     =
029400140308     C                              UBFMTDATE_Convert(
029500140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
029600140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029700140127     C                   eval      wIND     = tasIND
029800140127     C                   eval      wCAD     = tasCAD
029900140127     C                   eval      wLOD     = tasLOD
030000171004     C                   if        tasNZD=*blanks
030100171004     C                   eval      wPRDNZD  = tasPRD
030200171004     C                   else
030300171004     C                   eval      wPRDNZD  = tasNZD
030400171004     C                   endif
030500140127     C                   eval      wNCD     = tasNCD
030600140127     C                   eval      wNCA     = tasNCA
030700140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
030800140127     C                                        tasNRS*10000000+
030900140127     C                                        tasNSP
031000121207     C*
031100121207     C* Reperisco i dati del mittente originale
031200140308     C***                eval      taaTRC = 'O'
031300140308     C***      aa30      chain     titaa30c
031400140308     C***                if        %found(titaa30c)
031500140308     C***                eval      wRSCMORI = taaRSC
031600140308     C***                eval      wINDMORI = taaIND
031700140308     C***                eval      wCAPMORI = taaCAP
031800140308     C***                eval      wLOCMORI = taaLOC
031900140308     C***                eval      wPRVMORI = taaPRV
032000140308     C***                endif
032100130913     C*
032200130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
032300130913     C* (costruisco la chiave sul file TIGCP51L)
032400130913     C*
032500140310     C                   EVAL      gcpDUR = *zeros
032600130913     C                   EVAL      gcpFRG = *zeros
032700130913     C     KEYgcp51      SETLL     tigcp51l
032800130913     C                   IF        %equal(tigcp51l)
032900130913     C     KEYgcp51      READE     tigcp51l
033000130913     C                   DOW       not %eof(tigcp51l)
033100130913     C*
033200130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
033300130913     C                   IF        gcpDUR > *zeros AND
033400130913     C                             gcpDDM = *zeros AND
033500130913     C                             gcpFAS < 030
033600130913     C                   seton                                        70
033700130913     C                   ENDIF
033800140311     C*
033900140311     C* Reperisco la causale di ultima apertura giacenza
034000140311     C                   EVAL      wGCPCMC = gcpCMC
034100130913     C*
034200130913     C* Proseguo la lettura delle giacenze x la bolla corrente
034300130913     C     KEYgcp51      READE     tigcp51l
034400130913     C                   ENDDO
034500140308     C*
034600140308     C* Se in giacenza reperisco le note di giacenza
034700140308     C                   EXSR      rtvNOTEGIAC
034800140308     C*
034900140308     C                   ENDIF
035000071213     C*
035100071213     C                   endif
035200071121     C*
035300071121     C* Compondo la stringa dati output relativa al record corrente
035400071121     C                   EXSR      wrifile
035500100910     C*
035600100910     C                   endif
035700040719     C*
035800040719     C* In ogni caso storicizzo e cancello l'input sempre
035900080924     C                   DELETE    TIVGD000
036000040719     C*
036100040719     C* Proseguo con la lettura dell'input
036200080924     C                   READ      TIVGDTMP
036300060119     C*
036400030325     C                   ENDDO
036500071121     C*
036600071121     C* Head finali della struttura HTML
036700071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
036800071210     C***                WRITE     TIVAW000
036900991027     C*
037000030325     C                   EVAL      wrkesito = '0'
037100991027     C*
037200910830     C                   ENDSR
037300100907
037400100907
037500100907
037600100907     C     calSTS        BEGSR
037700100910     C*
037800120926     C                   MOVEL     *blanks       wMIC              1
037900121114     C                   MOVEL     *blanks       wGIAC             1
038000121207     C                   MOVEL     *blanks       wLAVV             1
038100140129     C                   MOVEL     *blanks       wPEND             1
038200100910     C                   EVAL      wDataSts       = vacDCM
038300140308     C*
038400140308     C* Verifico lo stato delle disposizioni di giacenza
038500140308     C                   MOVEL     *blanks       wGiacDisp        10
038600140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
038700100907     C*
038800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
038900100907     C                   IF        VACDCM > *zeros AND
039000100907     C                             (VACCCA = *blanks OR VACCCA = '1')
039100100907     C                   EVAL      wConsOkSiNo    = 'S'
039200140613     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
039300140613     C                   EVAL      wConsOkSiNoCod = '000'
039400140308     C                   EVAL      wGiacDisp      = *blanks
039500140308     C                   EVAL      wNoteGiac      = *blanks
039600100907     C                   ELSE
039700100907     C                   EVAL      wConsOkSiNo    = 'N'
039800140311     C                   EVAL      wFIRMATAR      = *blanks
039900100907     C*
040000100907     C* A questo punto stabilisco la descrizione
040100100910     C                   SELECT
040200100910     C                   WHEN      vacCCA = '1'
040300140613     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
040400140613     C                   EVAL      wConsOkSiNoCod = '002'
040500100910     C                   WHEN      vacCCA = '2'
040600140613     C                   EVAL      wConsOkSiNoDes = 'Resa'
040700140613     C                   EVAL      wConsOkSiNoCod = '003'
040800100907     C                   EVAL      wConsOkSiNo    = 'R'
040900100910     C                   WHEN      vacCCA = '3'
041000110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
041100140613     C                   EVAL      wConsOkSiNoCod = '009'
041200110519     C                   EVAL      wConsOkSiNo    = 'R'
041300100910     C                   WHEN      vacCCA = '4'
041400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
041500110519     C                   EVAL      wConsOkSiNo    = 'R'
041600100910     C                   WHEN      vacCCA = '5'
041700110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
041800140613     C                   EVAL      wConsOkSiNoCod = '010'
041900110519     C                   EVAL      wConsOkSiNo    = 'R'
042000100910     C                   WHEN      vacCCA = '6'
042100110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
042200140613     C                   EVAL      wConsOkSiNoCod = '008'
042300110519     C                   EVAL      wConsOkSiNo    = 'R'
042400100910     C                   WHEN      vacCCA = 'A'
042500110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
042600140613     C                   EVAL      wConsOkSiNoCod = '011'
042700110519     C                   EVAL      wConsOkSiNo    = 'R'
042800100910     C                   WHEN      vacCCA = 'C'
042900140613     C                   EVAL      wConsOkSiNoDes = 'In consegna'
043000140613     C                   EVAL      wConsOkSiNoCod = '001'
043100120926     C                   EVAL      wMIC = 'S'
043200100910     C                   WHEN      vacCCA = 'R'
043300110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
043400140613     C                   EVAL      wConsOkSiNoCod = '013'
043500100910     C                   WHEN      vacCCA = 'S'
043600110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
043700140613     C                   EVAL      wConsOkSiNoCod = '012'
043800110519     C                   EVAL      wConsOkSiNo    = 'R'
043900100910     C                   ENDSL
044000100907     C*
044100140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
044200100910     C                   EVAL      wDataSts       = vacDLA
044300140613     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
044400140613     C                   EVAL      wConsOkSiNoCod = '004'
044500121207     C                   EVAL      wLAVV = 'S'
044600100907     C                   ENDIF
044700140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
044800140308     C                   EVAL      wDataSts       = gcpDUR
044900140613     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
045000140613     C                   EVAL      wConsOkSiNoCod = '005'
045100140311     C                   EXSR      rtvCAU_GIAC
045200121114     C                   EVAL      wGIAC = 'S'
045300140308     C                   ELSE
045400140308     C                   EVAL      wGiacDisp      = *blanks
045500140308     C                   EVAL      wNoteGiac      = *blanks
045600100907     C                   ENDIF
045700100910     C                   IF        vacDCM = *zeros AND
045800100910     C                             vacDLA = *zeros AND
045900140308     C                             gcpDUR = *zeros AND
046000100910     C                             vacCCA = *blanks
046100100910     C                   EVAL      wDataSts       = datcor
046200100910     C*
046300100910     C* Verifico la data consegna richiesta
046400100910     C                   IF        vacDCR > datcor
046500140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
046600140308     C                              UBFMTDATE_Convert(
046700140308     C                                  %editc(vacDCR:'4') :
046800140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
046900140613     C                   EVAL      wConsOkSiNoCod = '006'
047000100910     C                   ELSE
047100110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
047200140613     C                   EVAL      wConsOkSiNoCod = '007'
047300140129     C                   EVAL      wPEND          = 'S'
047400100910     C                   ENDIF
047500100910     C                   ENDIF
047600100907     C*
047700100907     C                   ENDIF
047800100907     C*
047900100907     C                   ENDSR
048000140311
048100140311
048200140311
048300140311     C     rtvCAU_GIAC   BEGSR
048400140311     C*
048500140311     C* Rererisco la descrizioe della causale di giacenza
048600140311     C                   MOVEL     '1'           tblKUT
048700140311     C                   MOVEL     '2A'          tblCOD
048800140311     C                   MOVEL(P)  wGCPCMC       tblKEY
048900140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
049000140311     C                   IF        %found(tabel00f)
049100140311     C                   MOVEL     TBLUNI        ds2A
049200140311     C                   MOVEL(P)  §2ADES        wGiacCau
049300140311     C                   ENDIF
049400140311     C*
049500140311     C                   ENDSR
049600140311     C*------------------------------------------------------------------------*
049700140311
049800140308
049900140308
050000140308     C     rtvNOTEGIAC   BEGSR
050100140308     C*
050200140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
050300140308     C     KEYgnp01_P    setll     tignp01l
050400140308     C                   if        %equal(tignp01l)
050500140308     C                   eval      wNoteGiac = ''''
050600140308     C     KEYgnp01_P    reade     tignp01l
050700140308     C                   dow       not %eof(tignp01l)
050800140308     C*** concateno note giacenza
050900140308     C                   eval      wNoteGiac =
051000140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
051100140308     C     KEYgnp01_P    reade     tignp01l
051200140308     C                   enddo
051300140308     C                   endif
051400140308     C*
051500140308     C                   ENDSR
051600100907
051700100907
051800100907
051900100907     C     makLNK        BEGSR
052000130426     C*
052100130426     C* Se richiesta inclusione link T&T
052200130426     C                   if        %subst(prmppt:6:7) <> *blanks
052300130426     C*
052400130426     C* Porto in minuscolo il parametro relativo alla lingua del link
052500130426     C                   movel     *blanks       parLang           2
052600130426     C                   eval      parLang = %subst(prmppt:13:2)
052700130426     C     maiu:minu     xlate     parLang       parLang
052800130426     C*
052900160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
053000130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
053100130426     C                                        %trim(%char(vacRMN))             +
053200130426     C                                        '&ksu='+%subst(prmppt:6:7)+
053300130426     C                                        '&lang='+parLang
053400130426     C*
053500130426     C                   endif
053600100907     C*
053700100907     C                   ENDSR
053800100907
053900071121
054000071121
054100071121
054200071121     C     wrifile       BEGSR
054300120926     C*
054400120926     C                   SETOFF                                       31
054500071121     C*
054600100907     C* Determino lo status della spedizione
054700100907     C                   EXSR      calSTS
054800100907     C*
054900100907     C* Costruisco il link diretto
055000130426     C                   EXSR      makLNK
055100130426     C*
055200130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
055300130426     C                   IF        %subst(prmppt:15:1) = 'P'
055400130426     C                   MOVE      vacCCM        CCM               4 0
055500130426     C                   MOVE      wKSC          KSC               4 0
055600130426     C                   IF        CCM =  8888 or  CCM =  9999
055700130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
055800130426     C                   EVAL      vacCCM = wKSC
055900130426     C                   ENDIF
056000130426     C                   ENDIF
056100130426     C                   ENDIF
056200100908     C*
056300140308     C                   EVAL      wDataStsAlfa =
056400140308     C                              UBFMTDATE_Convert(
056500140308     C                                  %editc(wDataSts:'4') :
056600140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
056700140308     C*
056800100908     C                   SELECT
056900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
057000071121     C                   EVAL      wBufferOut = iRow +
057100100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
057200140308     C                                  iCol+%trim(wDSP)                  +fCol+
057300110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
057400100907     C                                  iCol+%trim(vacRSD)                +fCol+
057500140308     C                                  iCol+%trim(§AR5TEL)               +fCol+
057600140308     C                                  iCol+%trim(§AR5REF)               +fCol+
057700140308     C                                  iCol+%trim(§AR5TELD)              +fCol+
057800110519     C                                  iCol+%trim(wIND)                  +fCol+
057900100517     C                                  iCol+%trim(wCAD)                  +fCol+
058000100517     C                                  iCol+%trim(wLOD)                  +fCol+
058100171004     C                                  iCol+%trim(wPRDNZD)               +fCol+
058200100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
058300110519     C                                  iCol+%trim(vacRMA)                +fCol+
058400140308     C***                               iCol+%trim(wRSCMORI)              +fCol+
058500140308     C***                               iCol+%trim(wINDMORI)              +fCol+
058600140308     C***                               iCol+%trim(wCAPMORI)              +fCol+
058700140308     C***                               iCol+%trim(wLOCMORI)              +fCol+
058800140308     C***                               iCol+%trim(wPRVMORI)              +fCol+
058900140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
059000100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
059100140613     C                                  iCol+%trim(wConsOkSiNoCod)        +fCol+
059200130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
059300140311     C                                  iCol+%trim(wGiacCau)              +fCol+
059400140308     C                                  iCol+%trim(wNoteGiac)             +fCol+
059500140308     C***                               iCol+%trim(%editc(wNCD:'X'))      +fCol+
059600140308     C***                               iCol+%trim(%editc(wNCA:'X'))      +fCol+
059700140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
059800140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
059900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
060000100908     C                   EVAL      wBufferOut =
060100110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
060200140308     C                              +'"'+   %trim(wDSP)                   +'";'
060300110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
060400110519     C                              +'"'+   %trim(vacRSD)                 +'";'
060500140308     C                              +'"'+   %trim(§AR5TEL)                +'";'
060600140308     C                              +'"'+   %trim(§AR5REF)                +'";'
060700140308     C                              +'"'+   %trim(§AR5TELD)               +'";'
060800110519     C                              +'"'+   %trim(wIND)                   +'";'
060900110519     C                              +'"'+   %trim(wCAD)                   +'";'
061000110519     C                              +'"'+   %trim(wLOD)                   +'";'
061100171004     C                              +'"'+   %trim(wPRDNZD)                +'";'
061200110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
061300110519     C                              +'"'+   %trim(vacRMA)                 +'";'
061400140308     C***                           +'"'+   %trim(wRSCMORI)               +'";'
061500140308     C***                           +'"'+   %trim(wINDMORI)               +'";'
061600140308     C***                           +'"'+   %trim(wCAPMORI)               +'";'
061700140308     C***                           +'"'+   %trim(wLOCMORI)               +'";'
061800140308     C***                           +'"'+   %trim(wPRVMORI)               +'";'
061900140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
062000110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
062100140613     C                              +'"'+   %trim(wConsOkSiNoCod)         +'";'
062200130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
062300140311     C                              +'"'+   %trim(wGiacCau)               +'";'
062400140308     C                              +'"'+   %trim(wNoteGiac)              +'";'
062500140308     C***                           +'"'+   %trim(%editc(wNCD:'X'))       +'";'
062600140308     C***                           +'"'+   %trim(%editc(wNCA:'X'))       +'";'
062700140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
062800140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
062900100908     C                   ENDSL
063000071121     C*
063100120926     C* Se nei parametri è richiesto no MIC => gestisco
063200120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
063300120926     C                   SETON                                        31
063400120926     C                   ENDIF
063500121114     C*
063600121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
063700121207     C                   IF        %subst(prmppt:4:1) = 'S' and
063800121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
063900121114     C                   SETON                                        31
064000121114     C                   ENDIF
064100140124     C*
064200140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
064300140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
064400140124     C                   SETON                                        31
064500140124     C                   ENDIF
064600120926     C*
064700071121     C                   EVAL      VAWDTA = wBufferOut
064800110519     C*
064900110519     C* Restituisco solo le spedizioni "pendenti"
065000110713     C***                IF        wConsOkSiNo = 'N'
065100120926     C  N31              WRITE     TIVAW000
065200110713     C***                ENDIF
065300071121     C*
065400071121     C                   ENDSR
065500071121
065600071121
065700071121
065800991027
065900991027      /TITLE Operazioni iniziali.
066000991027     C     *inzsr        BEGSR
066100991027     C*
066200991027     C     *ENTRY        PLIST
066300991027     C                   parm                    prmppt
066400991027     C     wrkesito      parm      wrkesito      prmesito
066500100910     C*
066600100910     C* CALCOLA LA DATA CORRENTE
066700100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066800100910     C                   eval      datcor = %dec(%date() : *ISO)
066900071210     C*
067000071213     C* TITAS30C - Parziale
067100071210     C     KEYtas30      KLIST
067200071210     C                   KFLD                    vacAAS
067300071210     C                   KFLD                    vacLNP
067400071210     C                   KFLD                    vacNRS
067500071210     C                   KFLD                    vacNSP
067600110519     C*
067700110519     C* TITA430C - Completa
067800140129     C     KEYta430C     KLIST
067900110519     C                   KFLD                    vacAAS
068000110519     C                   KFLD                    vacLNP
068100110519     C                   KFLD                    vacNRS
068200110519     C                   KFLD                    vacNSP
068300110519     C                   KFLD                    ta4TRC
068400121207     C*
068500121207     C* TITAA30C - Completa
068600121207     C     KEYtaa30      KLIST
068700121207     C                   KFLD                    vacAAS
068800121207     C                   KFLD                    vacLNP
068900121207     C                   KFLD                    vacNRS
069000121207     C                   KFLD                    vacNSP
069100121207     C                   KFLD                    taaTRC
069200130913     C*
069300130913     C* TIGCP51L - Completa
069400130913     C     KEYgcp51      KLIST
069500130913     C                   KFLD                    vacAAS
069600130913     C                   KFLD                    vacLNP
069700130913     C                   KFLD                    vacNRS
069800130913     C                   KFLD                    vacNSP
069900130913     C                   KFLD                    gcpFRG
070000140308     C*
070100140308     C* Chiave su FIAR531C - Parziale
070200140308     C     KEYar531_1P   KLIST
070300140308     C                   KFLD                    vacAAS
070400140308     C                   KFLD                    vacLNP
070500140308     C                   KFLD                    vacNRS
070600140308     C                   KFLD                    vacNSP
070700140308     C                   KFLD                    ar5TRD
070800140308     C*
070900140308     C* Chiave su TIGNP01L - Parziale
071000140308     C     KEYgnp01_P    KLIST
071100140308     C                   KFLD                    gcpAGC
071200140308     C                   KFLD                    gcpFGC
071300140308     C                   KFLD                    gcpNGC
071400140308     C                   KFLD                    gcpFRG
071500140311     C*
071600140311     C* TABEL00F - Completa
071700140311     C     KEYtab00_C    KLIST
071800140311     C                   KFLD                    tblKUT
071900140311     C                   KFLD                    tblCOD
072000140311     C                   KFLD                    tblKEY
072100991027     C*
072200991027     C                   ENDSR
072300071121     C*
072400071121** DEC - COSTANTI
072500071121<?xml version="1.0"?>
072600071121<?mso-application progid="Excel.Sheet"?>
072700071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
072800071121xmlns:o="urn:schemas-microsoft-com:office:office"
072900071121xmlns:x="urn:schemas-microsoft-com:office:excel"
073000071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
073100071121xmlns:html="http://www.w3.org/TR/REC-html40">
073200071121</Workbook>
073300071121<Worksheet ss:Name="PAG1">
073400071121</Worksheet>
073500071121<Table border="1">
073600071121</Table>
073700071121<Row>
073800071121</Row>
073900071121<Cell><Data ss:Type="String">
074000071121<Cell><Data ss:Type="Number">
074100071121</Data></Cell>
074200071121
074300071121
074400071121
