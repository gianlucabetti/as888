000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600071210     FTITAS30C  IF   E           K DISK
000700071210     F                                     rename(titas000:titas0)
000800071210     F                                     rename(titas010:titas1)
000900071210     F                                     rename(titasP00:titasP)
001000030924
001100080924     D fnvac00t      e ds
001200071121
001300071121     D*------------
001400071121     D* DECODIFICHE COSTANTI
001500071121     D*------------
001600071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700071121
001800071121
001900071121     D*------------------
002000071121     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100071121     D*------------------
002200071121     D WLBDA8          DS                  INZ
002300071121     D  G08DAT                 1      8  0
002400071121     D  G08INV                 9     16  0
002500071121     D  G08ERR                17     17
002600071121     D  G08TGI                18     22  0
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
005400071210     D wConsOkSiNo     S              1    inz(*blanks)
005500071121     D wConsOkSiNoDes  S            300    inz(*blanks)
005600071122     D wBufferOut      s          10000    varying inz
005700071210     D wPKB            s              8    inz(*blanks)
005800071210     D wLOD            s             35    inz(*blanks)
005900071210     D wIND            s             35    inz(*blanks)
006000071210     D wCAD            s             35    inz(*blanks)
006100071211     D*
006200071211     D wDS_RMO         ds
006300071211     D  wDS_RMO_RICEV                10    inz(*blanks)
006400071211     D  wDS_RMO_FILE                 15    inz(*blanks)
006500071211     D*
006600030924
006700030325
006800030924     C* MAIN/
006900071121     C*
007000071121     C* Verifico se già presenti dati nel file d output
007100071121     C                   EXSR      chkRECFILMBR
007200071121     C*
007300071121     C* Effettuo elaborazione
007400071121     C                   EXSR      traduci
007500071121     C*
007600921023     C                   SETON                                        LR
007700991027
007800071121
007900071121
008000071121     C     chkRECFILMBR  BEGSR
008100071121     C*
008200071121     C                   READ      TIVAWWWT                               55
008300071121     C*
008400071121     C                   ENDSR
008500071121
008600071121
008700030924
008800991027     C     traduci       BEGSR
008900071121     C*
009000071121     C* Head iniziali della struttura HTML
009100071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009200071210     C   55              WRITE     TIVAW000
009300071121     C*
009400071121     C* Inizializzazone formato e variabili x output
009500071121     C                   clear                   TIVAW000
009600071121     C                   clear                   wBufferOut
009700071121     C*
009800071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009900071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010000071121     C                   EVAL      wBufferOut = iThead + iRow
010100071210     C                             +iColBold+ 'COD. RICEV.'+fColBold
010200071210     C                             +iColBold+ 'DESTINATARIO'+fColBold
010300071210     C                             +iColBold+ 'INDIRIZZO'+fColBold
010400071210     C                             +iColBold+ 'CAP'+fColBold
010500071210     C                             +iColBold+ 'LOCALITA'+fColBold
010600071210     C                             +iColBold+ 'PROV.'+fColBold
010700071210     C                             +iColBold+ 'N° SPED.'+fColBold
010800071210     C                             +iColBold+ 'LOTTO PRELIEVO'+fColBold
010900071210     C                             +iColBold+ 'COLLI'+fColBold
011000071210     C                             +iColBold+ 'PESO'+fColBold
011100071210     C                             +iColBold+ 'ESITO'+fColBold
011200071210     C                             +iColBold+ 'FILE'+fColBold
011300071121     C                   EVAL      VAWDTA = wBufferOut
011400071121     C                   WRITE     TIVAW000
011500071121     C                   ENDIF
011600991027     C*
011700080924     C                   READ      TIVGDTMP
011800060119     C*
011900080924     C                   DOW       not %eof(TIVGDTMP)
012000080924     C*
012100080924     C                   eval      fnvac00t = vgdDTA
012200071121     C*
012300071121     C                   clear                   wConsOkSiNo
012400071121     C                   clear                   wConsOkSiNoDes
012500071121     C                   clear                   TIVAW000
012600071121     C                   clear                   wBufferOut
012700040719     C*
012800071121     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
012900060208     C                   IF        VACDCM > *zeros AND
013000071121     C                             (VACCCA = *blanks OR VACCCA = '1')
013100071121     C                   EVAL      wConsOkSiNo    = 'S'
013200071210     C                   EVAL      wConsOkSiNoDes = 'Consegnata.'
013300071121     C                   ELSE
013400071121     C                   EVAL      wConsOkSiNo    = 'N'
013500071121     C*
013600071121     C* A questo punto stabilisco la descrizione
013700071121     C                   IF        vacCCA = '1'
013800071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione dirottata '+
013900071121     C                                              'ad altra filiale.'
014000071121     C                   ENDIF
014100071121     C                   IF        vacCCA = '2'
014200071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione resa al '+
014300071121     C                                              'mittente.'
014400071210     C                   EVAL      wConsOkSiNo    = 'R'
014500071121     C                   ENDIF
014600071121     C                   IF        vacCCA = '3'
014700071121     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce.'
014800071121     C                   ENDIF
014900071121     C                   IF        vacCCA = '4'
015000071121     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso.'
015100071121     C                   ENDIF
015200071121     C                   IF        vacCCA = '5'
015300071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
015400071121     C                                              'con pratica anomalia.'
015500071121     C                   ENDIF
015600071121     C                   IF        vacCCA = '6'
015700071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
015800071121     C                                              'con avaria resa al '+
015900071121     C                                              'mittente.'
016000071121     C                   ENDIF
016100071121     C                   IF        vacCCA = 'A'
016200071121     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica '+
016300071121     C                                              'anomalia.'
016400071121     C                   ENDIF
016500071121     C                   IF        vacCCA = 'C'
016600071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione in consegna.'
016700071121     C                   ENDIF
016800071121     C                   IF        vacCCA = 'A'
016900071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata '+
017000071121     C                                              'dopo errata imputazione '+
017100071121     C                                              'di consegna.'
017200071121     C                   ENDIF
017300071121     C                   IF        vacCCA = 'S'
017400071121     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta.'
017500071121     C                   ENDIF
017600071121     C*
017700071121     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
017800071121     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso.'
017900071121     C                   ENDIF
018000071121     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
018100071121     C                   EVAL      wConsOkSiNoDes = 'Aperta giacienza.'
018200071121     C                   ENDIF
018300071121     C*
018400071121     C                   ENDIF
018500071210     C*
018600071210     C* Il cliente vuole ricevere solamente gli stati di chiusura spedizione (ok o reso)
018700071210     C                   IF        wConsOkSiNo = 'S' or
018800071210     C                             wConsOkSiNo = 'R'
018900071210     C*
019000071210     C* Inizializzo campi d wrk
019100071210     C                   EVAL      wPKB = *blanks
019200071210     C                   EVAL      wLOD = *blanks
019300071210     C                   EVAL      wIND = *blanks
019400071210     C                   EVAL      wCAD = *blanks
019500071211     C                   CLEAR                   wDS_RMO
019600071210     C*
019700071210     C* Aggancio la bolla relativa x il completamento dei dati
019800071210     C     KEYtas30      chain     titas30c
019900071210     C                   if        %found(titas30c)
020000071210     C*
020100071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
020200071210     C                   EVAL      wLOD = tasLOD
020300071210     C                   EVAL      wIND = tasIND
020400071210     C                   EVAL      wCAD = tasCAD
020500071210     C                   endif
020600071121     C*
020700071121     C* Compondo la stringa dati output relativa al record corrente
020800071121     C                   EXSR      wrifile
020900071210     C*
021000071210     C                   ENDIF
021100040719     C*
021200040719     C* In ogni caso storicizzo e cancello l'input sempre
021300080924     C                   DELETE    TIVGDTMP
021400040719     C*
021500040719     C* Proseguo con la lettura dell'input
021600080924     C                   READ      TIVGDTMP
021700060119     C*
021800030325     C                   ENDDO
021900071121     C*
022000071121     C* Head finali della struttura HTML
022100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
022200071210     C***                WRITE     TIVAW000
022300991027     C*
022400030325     C                   EVAL      wrkesito = '0'
022500991027     C*
022600910830     C                   ENDSR
022700071121
022800071121
022900071121
023000071121     C     wrifile       BEGSR
023100071122     C*
023200071210     C                   EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
023300071211     C                   EVAL      wDS_RMO_RICEV = %trim(%subst(VACRMO:1:10))
023400071211     C                   EVAL      wDS_RMO_FILE  = %trim(%subst(VACRMO:11))
023500071121     C*
023600071121     C                   EVAL      wBufferOut = iRow +
023700071211     C                                  iCol+%trim(wDS_RMO_RICEV)+fCol+
023800071210     C                                  iCol+%trim(VACRSD)+fCol+
023900071210     C                                  iCol+%trim(wIND)+fCol+
024000071210     C                                  iCol+%trim(wCAD)+fCol+
024100071210     C                                  iCol+%trim(wLOD)+fCol+
024200071210     C                                  iCol+%trim(VACPRD)+fCol+
024300071210     C                                  iCol+%trim(VACRMA)+fCol+
024400071210     C                             iCol+%trim(%editc(VACRMN:'Q'))+fCol+
024500071210     C                             iCol+%trim(%editc(VACNCL:'Q'))+fCol+
024600071210     C                             iCol+%trim(wPKB)+fCol+
024700071210     C                                  iCol+%trim(wConsOkSiNoDes)+fCol+
024800071211     C                                  iCol+%trim(wDS_RMO_FILE)+fCol
024900071121     C*
025000071121     C                   EVAL      VAWDTA = wBufferOut
025100071121     C                   WRITE     TIVAW000
025200071121     C*
025300071121     C                   ENDSR
025400071121
025500071121
025600071121
025700991027
025800991027      /TITLE Operazioni iniziali.
025900991027     C     *inzsr        BEGSR
026000991027     C*
026100991027     C     *ENTRY        PLIST
026200991027     C                   parm                    prmppt
026300991027     C     wrkesito      parm      wrkesito      prmesito
026400071210     C*
026500071210     C* TITAS30C - Completa
026600071210     C     KEYtas30      KLIST
026700071210     C                   KFLD                    vacAAS
026800071210     C                   KFLD                    vacLNP
026900071210     C                   KFLD                    vacNRS
027000071210     C                   KFLD                    vacNSP
027100991027     C*
027200991027     C                   ENDSR
027300071121     C*
027400071121** DEC - COSTANTI
027500071121<?xml version="1.0"?>
027600071121<?mso-application progid="Excel.Sheet"?>
027700071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
027800071121xmlns:o="urn:schemas-microsoft-com:office:office"
027900071121xmlns:x="urn:schemas-microsoft-com:office:excel"
028000071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
028100071121xmlns:html="http://www.w3.org/TR/REC-html40">
028200071121</Workbook>
028300071121<Worksheet ss:Name="PAG1">
028400071121</Worksheet>
028500071121<Table border="1">
028600071121</Table>
028700071121<Row>
028800071121</Row>
028900071121<Cell><Data ss:Type="String">
029000071121<Cell><Data ss:Type="Number">
029100071121</Data></Cell>
029200071121
029300071121
029400071121
