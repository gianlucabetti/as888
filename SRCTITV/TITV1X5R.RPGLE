000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000030924
001100080924     D fnvac00t      e ds
001200071121
001300071121     D*------------
001400071121     D* DECODIFICHE COSTANTI
001500071121     D*------------
001600071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700071121
001800071121
001900071121     D*------------
002000071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002100071121     D*------------
002200071121     D iHtml           s             10    inz('<html>')
002300071121     D fHtml           s             10    inz('</html>')
002400071121     D iBody           s             10    inz('<body>')
002500071121     D fBody           s             10    inz('</body>')
002600071121     D iTable          s             20    inz('<table border="1">')
002700071121     D fTable          s             10    inz('</table>')
002800071121     D iThead          s             10    inz('<thead>')
002900071121     D fThead          s             10    inz('</thead>')
003000071121     D iRow            s              5    inz('<tr>')
003100071121     D fRow            s              5    inz('</tr>')
003200071121     D iCol            s              5    inz('<td>')
003300071121     D fCol            s              5    inz('</td>')
003400071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003500071121     D fColBold        s              5    inz('</th>')
003600071122     D cApi            s              1    inz('''')
003700071121
003800071121
003900071121     D*------------
004000071121     D* VARIABILI D WRK
004100071121     D*------------
004200030924     D prmppt          S             50
004300030924     D prmesito        S              1
004400030924     D wrkesito        S                   like(prmesito)
004500071122     D wBufferOut      s          10000    varying inz
004600100517     D wDBR            s             10    inz(*blanks)
004700100517     D wIND            s             35    inz(*blanks)
004800100517     D wCAD            s              9    inz(*blanks)
004900071210     D wLOD            s             35    inz(*blanks)
005000071211     D*
005100030924
005200030325
005300030924     C* MAIN/
005400071121     C*
005500071121     C* Verifico se già presenti dati nel file d output
005600071121     C                   EXSR      chkRECFILMBR
005700071121     C*
005800071121     C* Effettuo elaborazione
005900071121     C                   EXSR      traduci
006000071121     C*
006100921023     C                   SETON                                        LR
006200991027
006300071121
006400071121
006500071121     C     chkRECFILMBR  BEGSR
006600071121     C*
006700071121     C                   READ      TIVAWWWT                               55
006800071121     C*
006900071121     C                   ENDSR
007000071121
007100071121
007200030924
007300991027     C     traduci       BEGSR
007400071121     C*
007500071121     C* Head iniziali della struttura HTML
007600071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
007700071210     C   55              WRITE     TIVAW000
007800071121     C*
007900071121     C* Inizializzazone formato e variabili x output
008000071121     C                   clear                   TIVAW000
008100071121     C                   clear                   wBufferOut
008200071121     C*
008300071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008400071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008500071121     C                   EVAL      wBufferOut = iThead + iRow
008600100517     C                             +iColBold+ 'DATA PARTENZA'          +fColBold
008700100517     C                             +iColBold+ 'RIFERIMENTO MITTENTE'   +fColBold
008800100517     C                             +iColBold+ 'RAGIONE SOCIALE DEST.'  +fColBold
008900100517     C                             +iColBold+ 'INDIRIZZO DEST.'        +fColBold
009000100517     C                             +iColBold+ 'CAP DEST.'              +fColBold
009100100517     C                             +iColBold+ 'LOCALITA DEST.'         +fColBold
009200100517     C                             +iColBold+ 'PROVINCIA DEST.'        +fColBold
009300071121     C                   EVAL      VAWDTA = wBufferOut
009400071121     C                   WRITE     TIVAW000
009500071121     C                   ENDIF
009600991027     C*
009700080924     C                   READ      TIVGDTMP
009800060119     C*
009900080924     C                   DOW       not %eof(TIVGDTMP)
010000080924     C*
010100080924     C                   eval      fnvac00t = vgdDTA
010200071121     C*
010300071121     C                   clear                   TIVAW000
010400071121     C                   clear                   wBufferOut
010500071210     C*
010600071210     C* Inizializzo campi d wrk
010700071213     C                   EVAL      wDBR = *blanks
010800100517     C                   EVAL      wIND = *blanks
010900100517     C                   EVAL      wCAD = *blanks
011000071213     C                   EVAL      wLOD = *blanks
011100071210     C*
011200071210     C* Aggancio la bolla relativa x il completamento dei dati
011300071210     C     KEYtas30      chain     titas30c
011400071210     C                   if        %found(titas30c)
011500071210     C*
011600071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
011700071213     C                   EVAL      wDBR = %trim(%editw(tasDBR:'    /  /  '))
011800100517     C                   EVAL      wIND = tasIND
011900100517     C                   EVAL      wCAD = tasCAD
012000071213     C                   EVAL      wLOD = tasLOD
012100071213     C*
012200071213     C                   endif
012300071121     C*
012400071121     C* Compondo la stringa dati output relativa al record corrente
012500071121     C                   EXSR      wrifile
012600040719     C*
012700040719     C* In ogni caso storicizzo e cancello l'input sempre
012800080924     C                   DELETE    TIVGD000
012900040719     C*
013000040719     C* Proseguo con la lettura dell'input
013100080924     C                   READ      TIVGDTMP
013200060119     C*
013300030325     C                   ENDDO
013400071121     C*
013500071121     C* Head finali della struttura HTML
013600071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
013700071210     C***                WRITE     TIVAW000
013800991027     C*
013900030325     C                   EVAL      wrkesito = '0'
014000991027     C*
014100910830     C                   ENDSR
014200071121
014300071121
014400071121
014500071121     C     wrifile       BEGSR
014600071121     C*
014700071121     C                   EVAL      wBufferOut = iRow +
014800100517     C                                  iCol+%trim(wDBR)                  +fCol+
014900100517     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
015000100517     C                                  iCol+%trim(vacRSD)                +fCol+
015100100517     C                                  iCol+%trim(wIND)                  +fCol+
015200100517     C                                  iCol+%trim(wCAD)                  +fCol+
015300100517     C                                  iCol+%trim(wLOD)                  +fCol+
015400100517     C                                  iCol+%trim(vacPRD)                +fCol
015500071121     C*
015600071121     C                   EVAL      VAWDTA = wBufferOut
015700071121     C                   WRITE     TIVAW000
015800071121     C*
015900071121     C                   ENDSR
016000071121
016100071121
016200071121
016300991027
016400991027      /TITLE Operazioni iniziali.
016500991027     C     *inzsr        BEGSR
016600991027     C*
016700991027     C     *ENTRY        PLIST
016800991027     C                   parm                    prmppt
016900991027     C     wrkesito      parm      wrkesito      prmesito
017000071210     C*
017100071213     C* TITAS30C - Parziale
017200071210     C     KEYtas30      KLIST
017300071210     C                   KFLD                    vacAAS
017400071210     C                   KFLD                    vacLNP
017500071210     C                   KFLD                    vacNRS
017600071210     C                   KFLD                    vacNSP
017700991027     C*
017800991027     C                   ENDSR
017900071121     C*
018000071121** DEC - COSTANTI
018100071121<?xml version="1.0"?>
018200071121<?mso-application progid="Excel.Sheet"?>
018300071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
018400071121xmlns:o="urn:schemas-microsoft-com:office:office"
018500071121xmlns:x="urn:schemas-microsoft-com:office:excel"
018600071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
018700071121xmlns:html="http://www.w3.org/TR/REC-html40">
018800071121</Workbook>
018900071121<Worksheet ss:Name="PAG1">
019000071121</Worksheet>
019100071121<Table border="1">
019200071121</Table>
019300071121<Row>
019400071121</Row>
019500071121<Cell><Data ss:Type="String">
019600071121<Cell><Data ss:Type="Number">
019700071121</Data></Cell>
019800071121
019900071121
020000071121
