000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600071210     FTITAS30C  IF   E           K DISK
000700071210     F                                     rename(titas000:titas0)
000800071210     F                                     rename(titas010:titas1)
000900071210     F                                     rename(titasP00:titasP)
001000071213     FTIGCP51L  IF   E           K DISK
001100030924
001200080924     D fnvac00t      e ds
001300071121
001400071121     D*------------
001500071121     D* DECODIFICHE COSTANTI
001600071121     D*------------
001700071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001800071121
001900071121
002000071121     D*------------------
002100071121     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200071121     D*------------------
002300071121     D WLBDA8          DS                  INZ
002400071121     D  G08DAT                 1      8  0
002500071121     D  G08INV                 9     16  0
002600071121     D  G08ERR                17     17
002700071121     D  G08TGI                18     22  0
002800071121
002900071121     D*------------
003000071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003100071121     D*------------
003200071121     D iHtml           s             10    inz('<html>')
003300071121     D fHtml           s             10    inz('</html>')
003400071121     D iBody           s             10    inz('<body>')
003500071121     D fBody           s             10    inz('</body>')
003600071121     D iTable          s             20    inz('<table border="1">')
003700071121     D fTable          s             10    inz('</table>')
003800071121     D iThead          s             10    inz('<thead>')
003900071121     D fThead          s             10    inz('</thead>')
004000071121     D iRow            s              5    inz('<tr>')
004100071121     D fRow            s              5    inz('</tr>')
004200071121     D iCol            s              5    inz('<td>')
004300071121     D fCol            s              5    inz('</td>')
004400071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004500071121     D fColBold        s              5    inz('</th>')
004600071122     D cApi            s              1    inz('''')
004700071121
004800071121
004900071121     D*------------
005000071121     D* VARIABILI D WRK
005100071121     D*------------
005200030924     D prmppt          S             50
005300030924     D prmesito        S              1
005400030924     D wrkesito        S                   like(prmesito)
005500071210     D wConsOkSiNo     S              1    inz(*blanks)
005600071121     D wConsOkSiNoDes  S            300    inz(*blanks)
005700071122     D wBufferOut      s          10000    varying inz
005800071213     D wKSC            s              7    inz(*blanks)
005900071213     D wPKB            s              8    inz(*blanks)
006000071213     D wVLB            s              6    inz(*blanks)
006100071213     D wDRT            s             10    inz(*blanks)
006200071213     D wDBR            s             10    inz(*blanks)
006300071213     D wDCM            s             10    inz(*blanks)
006400071213     D wDCR            s             10    inz(*blanks)
006500071213     D wDUR            s             10    inz(*blanks)
006600071213     D wDCG            s             10    inz(*blanks)
006700071210     D wLOD            s             35    inz(*blanks)
006800071210     D wIND            s             35    inz(*blanks)
006900071210     D wCAD            s             35    inz(*blanks)
007000071213     D wNRC            s              3    inz(*blanks)
007100071211     D*
007200030924
007300030325
007400030924     C* MAIN/
007500071121     C*
007600071121     C* Verifico se già presenti dati nel file d output
007700071121     C                   EXSR      chkRECFILMBR
007800071121     C*
007900071121     C* Effettuo elaborazione
008000071121     C                   EXSR      traduci
008100071121     C*
008200921023     C                   SETON                                        LR
008300991027
008400071121
008500071121
008600071121     C     chkRECFILMBR  BEGSR
008700071121     C*
008800071121     C                   READ      TIVAWWWT                               55
008900071121     C*
009000071121     C                   ENDSR
009100071121
009200071121
009300030924
009400991027     C     traduci       BEGSR
009500071121     C*
009600071121     C* Head iniziali della struttura HTML
009700071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009800071210     C   55              WRITE     TIVAW000
009900071121     C*
010000071121     C* Inizializzazone formato e variabili x output
010100071121     C                   clear                   TIVAW000
010200071121     C                   clear                   wBufferOut
010300071121     C*
010400071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010500071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010600071121     C                   EVAL      wBufferOut = iThead + iRow
010700071213     C                             +iColBold+ 'TIPO ORDINE'+fColBold
010800110524     C                             +iColBold+ 'RIF. BRT'+fColBold
010900071213     C                             +iColBold+ 'NUMERO DDT'+fColBold
011000071213     C                             +iColBold+ 'DATA AFFIDAMENTO'+fColBold
011100071213     C                             +iColBold+ 'DATA INIZIO TRASPORTO'+fColBold
011200071213     C                             +iColBold+ 'RAGIONE SOCIALE DEST.'+fColBold
011300071213     C                             +iColBold+ 'INDIRIZZO DEST.'+fColBold
011400071213     C                             +iColBold+ 'CAP DEST.'+fColBold
011500071213     C                             +iColBold+ 'LOCALITA DEST.'+fColBold
011600071213     C                             +iColBold+ 'PROVINCIA DEST.'+fColBold
011700071213     C                             +iColBold+ 'COLLI'+fColBold
011800071213     C                             +iColBold+ 'VOLUME'+fColBold
011900071213     C                             +iColBold+ 'PESO LORDO'+fColBold
012000071213     C                             +iColBold+ 'DATA CONSEGNA'+fColBold
012100071213     C                             +iColBold+ 'DATA APPUNTAMENTO'+fColBold
012200071213     C                             +iColBold+ 'DATA ULTIMA GIACENZA'+fColBold
012300071213     C                             +iColBold+ 'CHIUSURA'+fColBold
012400071213     C                             +iColBold+ 'LIVELLO DI SERVIZIO'+fColBold
012500071121     C                   EVAL      VAWDTA = wBufferOut
012600071121     C                   WRITE     TIVAW000
012700071121     C                   ENDIF
012800991027     C*
012900080924     C                   READ      TIVGDTMP
013000060119     C*
013100080924     C                   DOW       not %eof(TIVGDTMP)
013200080924     C*
013300080924     C                   eval      fnvac00t = vgdDTA
013400071121     C*
013500071121     C                   clear                   wConsOkSiNo
013600071121     C                   clear                   wConsOkSiNoDes
013700071121     C                   clear                   TIVAW000
013800071121     C                   clear                   wBufferOut
013900040719     C*
014000071121     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
014100060208     C                   IF        VACDCM > *zeros AND
014200071121     C                             (VACCCA = *blanks OR VACCCA = '1')
014300071121     C                   EVAL      wConsOkSiNo    = 'S'
014400071210     C                   EVAL      wConsOkSiNoDes = 'Consegnata.'
014500071121     C                   ELSE
014600071121     C                   EVAL      wConsOkSiNo    = 'N'
014700071121     C*
014800071121     C* A questo punto stabilisco la descrizione
014900071121     C                   IF        vacCCA = '1'
015000071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione dirottata '+
015100071121     C                                              'ad altra filiale.'
015200071121     C                   ENDIF
015300071121     C                   IF        vacCCA = '2'
015400071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione resa al '+
015500071121     C                                              'mittente.'
015600071210     C                   EVAL      wConsOkSiNo    = 'R'
015700071121     C                   ENDIF
015800071121     C                   IF        vacCCA = '3'
015900071121     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce.'
016000071121     C                   ENDIF
016100071121     C                   IF        vacCCA = '4'
016200071121     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso.'
016300071121     C                   ENDIF
016400071121     C                   IF        vacCCA = '5'
016500071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
016600071121     C                                              'con pratica anomalia.'
016700071121     C                   ENDIF
016800071121     C                   IF        vacCCA = '6'
016900071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
017000071121     C                                              'con avaria resa al '+
017100071121     C                                              'mittente.'
017200071121     C                   ENDIF
017300071121     C                   IF        vacCCA = 'A'
017400071121     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica '+
017500071121     C                                              'anomalia.'
017600071121     C                   ENDIF
017700071121     C                   IF        vacCCA = 'C'
017800071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione in consegna.'
017900071121     C                   ENDIF
018000071121     C                   IF        vacCCA = 'A'
018100071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata '+
018200071121     C                                              'dopo errata imputazione '+
018300071121     C                                              'di consegna.'
018400071121     C                   ENDIF
018500071121     C                   IF        vacCCA = 'S'
018600071121     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta.'
018700071121     C                   ENDIF
018800071121     C*
018900071121     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
019000071121     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso.'
019100071121     C                   ENDIF
019200071121     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
019300071121     C                   EVAL      wConsOkSiNoDes = 'Aperta giacienza.'
019400071121     C                   ENDIF
019500071121     C*
019600071121     C                   ENDIF
019700071210     C*
019800071210     C* Il cliente vuole ricevere solamente gli stati di chiusura spedizione (ok o reso)
019900071210     C                   IF        wConsOkSiNo = 'S' or
020000071210     C                             wConsOkSiNo = 'R'
020100071210     C*
020200071210     C* Inizializzo campi d wrk
020300071213     C                   EVAL      wKSC = *blanks
020400071213     C                   EVAL      wPKB = *blanks
020500071213     C                   EVAL      wVLB = *blanks
020600071213     C                   EVAL      wDRT = *blanks
020700071213     C                   EVAL      wDBR = *blanks
020800071213     C                   EVAL      wDCM = *blanks
020900071213     C                   EVAL      wDCR = *blanks
021000071213     C                   EVAL      wDUR = *blanks
021100071213     C                   EVAL      wDCG = *blanks
021200071213     C                   EVAL      wLOD = *blanks
021300071213     C                   EVAL      wIND = *blanks
021400071213     C                   EVAL      wCAD = *blanks
021500071213     C                   EVAL      wNRC = *blanks
021600071210     C*
021700071210     C* Aggancio la bolla relativa x il completamento dei dati
021800071210     C     KEYtas30      chain     titas30c
021900071210     C                   if        %found(titas30c)
022000071210     C*
022100071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
022200071213     C                   EVAL      wKSC = %trim(%editc(tasKSC:'Q'))
022300071213     C                   EVAL      wPKB = %trim(%editw(tasPKB:'    0 , '))
022400071213     C                   EVAL      wVLB = %trim(%editw(tasVLB:'0 ,   '))
022500071213     C                   EVAL      wDRT = %trim(%editw(tasDRT:'    /  /  '))
022600071213     C                   EVAL      wDBR = %trim(%editw(tasDBR:'    /  /  '))
022700071213     C                   EVAL      wDCM = %trim(%editw(tasDCM:'    /  /  '))
022800071213     C                   EVAL      wDCR = %trim(%editw(tasDCR:'    /  /  '))
022900071213     C                   EVAL      wLOD = tasLOD
023000071213     C                   EVAL      wIND = tasIND
023100071213     C                   EVAL      wCAD = tasCAD
023200071213     C                   EVAL      wNRC = %trim(%editc(tasNRC:'Q'))
023300071213     C*
023400071213     C* Aggancio la giacenza + recente
023500071213     C                   EVAL      gcpFRG = *zeros
023600071213     C     KEYgcp51      chain     tigcp51l
023700071213     C                   if        %found(tigcp51l)
023800071213     C                   EVAL      wDUR = %trim(%editw(gcpDUR:'    /  /  '))
023900071213     C                   EVAL      wDCG = %trim(%editw(gcpDCG:'    /  /  '))
024000071213     C                   endif
024100071213     C*
024200071213     C                   endif
024300071121     C*
024400071121     C* Compondo la stringa dati output relativa al record corrente
024500071121     C                   EXSR      wrifile
024600071210     C*
024700071210     C                   ENDIF
024800040719     C*
024900040719     C* In ogni caso storicizzo e cancello l'input sempre
025000080924     C                   DELETE    TIVGD000
025100040719     C*
025200040719     C* Proseguo con la lettura dell'input
025300080924     C                   READ      TIVGDTMP
025400060119     C*
025500030325     C                   ENDDO
025600071121     C*
025700071121     C* Head finali della struttura HTML
025800071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
025900071210     C***                WRITE     TIVAW000
026000991027     C*
026100030325     C                   EVAL      wrkesito = '0'
026200991027     C*
026300910830     C                   ENDSR
026400071121
026500071121
026600071121
026700071121     C     wrifile       BEGSR
026800071121     C*
026900071121     C                   EVAL      wBufferOut = iRow +
027000071213     C                                  iCol+%trim(wKSC)+fCol+
027100071213     C                                  iCol+%trim(%editc(vacNSP:'Q'))+fCol+
027200071213     C                                  iCol+%trim(%editc(vacRMN:'Q'))+fCol+
027300071213     C                                  iCol+%trim(wDRT)+fCol+
027400071213     C                                  iCol+%trim(wDBR)+fCol+
027500071213     C                                  iCol+%trim(vacRSD)+fCol+
027600071210     C                                  iCol+%trim(wIND)+fCol+
027700071210     C                                  iCol+%trim(wCAD)+fCol+
027800071210     C                                  iCol+%trim(wLOD)+fCol+
027900071213     C                                  iCol+%trim(vacPRD)+fCol+
028000071213     C                                  iCol+%trim(%editc(vacNCL:'Q'))+fCol+
028100071213     C                                  iCol+%trim(wVLB)+fCol+
028200071213     C                                  iCol+%trim(wPKB)+fCol+
028300071213     C                                  iCol+%trim(wDCM)+fCol+
028400071213     C                                  iCol+%trim(wDCR)+fCol+
028500071213     C                                  iCol+%trim(wDUR)+fCol+
028600071213     C                                  iCol+%trim(wDCG)+fCol+
028700071213     C                                  iCol+%trim(wNRC)+fCol
028800071121     C*
028900071121     C                   EVAL      VAWDTA = wBufferOut
029000071121     C                   WRITE     TIVAW000
029100071121     C*
029200071121     C                   ENDSR
029300071121
029400071121
029500071121
029600991027
029700991027      /TITLE Operazioni iniziali.
029800991027     C     *inzsr        BEGSR
029900991027     C*
030000991027     C     *ENTRY        PLIST
030100991027     C                   parm                    prmppt
030200991027     C     wrkesito      parm      wrkesito      prmesito
030300071210     C*
030400071213     C* TITAS30C - Parziale
030500071210     C     KEYtas30      KLIST
030600071210     C                   KFLD                    vacAAS
030700071210     C                   KFLD                    vacLNP
030800071210     C                   KFLD                    vacNRS
030900071210     C                   KFLD                    vacNSP
031000071213     C*
031100071213     C* TIGCP51L - Parziale
031200071213     C     KEYgcp51      KLIST
031300071213     C                   KFLD                    vacAAS
031400071213     C                   KFLD                    vacLNP
031500071213     C                   KFLD                    vacNRS
031600071213     C                   KFLD                    vacNSP
031700071213     C                   KFLD                    gcpFRG
031800991027     C*
031900991027     C                   ENDSR
032000071121     C*
032100071121** DEC - COSTANTI
032200071121<?xml version="1.0"?>
032300071121<?mso-application progid="Excel.Sheet"?>
032400071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
032500071121xmlns:o="urn:schemas-microsoft-com:office:office"
032600071121xmlns:x="urn:schemas-microsoft-com:office:excel"
032700071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
032800071121xmlns:html="http://www.w3.org/TR/REC-html40">
032900071121</Workbook>
033000071121<Worksheet ss:Name="PAG1">
033100071121</Worksheet>
033200071121<Table border="1">
033300071121</Table>
033400071121<Row>
033500071121</Row>
033600071121<Cell><Data ss:Type="String">
033700071121<Cell><Data ss:Type="Number">
033800071121</Data></Cell>
033900071121
034000071121
034100071121
