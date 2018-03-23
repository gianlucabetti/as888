000100090316     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700110128     D DSFR_INFO     e ds
000800071121
000900080929
001000080929
001100080929     D*------------
001200080929     D* DECODIFICHE COSTANTI
001300080929     D*------------
001400080929     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001500071121
001600080929
001700080929
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
004500080929     D wBufferOut      s          10000    varying inz
004600071211     D*
004700030924
004800030325
004900030924     C* MAIN/
005000071121     C*
005100071121     C* Verifico se già presenti dati nel file d output
005200071121     C                   EXSR      chkRECFILMBR
005300071121     C*
005400071121     C* Effettuo elaborazione
005500071121     C                   EXSR      traduci
005600071121     C*
005700921023     C                   SETON                                        LR
005800991027
005900071121
006000071121
006100071121     C     chkRECFILMBR  BEGSR
006200071121     C*
006300071121     C                   READ      TIVAWWWT                               55
006400071121     C*
006500071121     C                   ENDSR
006600071121
006700071121
006800030924
006900991027     C     traduci       BEGSR
007000071121     C*
007100071121     C* Head iniziali della struttura HTML
007200071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
007300071210     C   55              WRITE     TIVAW000
007400071121     C*
007500071121     C* Inizializzazone formato e variabili x output
007600071121     C                   clear                   TIVAW000
007700071121     C                   clear                   wBufferOut
007800071121     C*
007900071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008000080929     C                   IF        %subst(prmppt:1:1) = 'N' OR *IN55 = *OFF
008100080929     C                   ELSE
008200080929     C                   EVAL      wBufferOut = iThead + iRow +
008300110128     C                              iColBold+'DATA FATTURA'            +fColBold
008400110128     C                             +iColBold+'CODICE CLIENTE'          +fColBold
008500110128     C                             +iColBold+'RAGIONE SOCIALE'         +fColBold
008600110128     C                             +iColBold+'COLLI ECC. <= 2KG'       +fColBold
008700110128     C                             +iColBold+'COLLI ECC. 2KG => 5KG'   +fColBold
008800110128     C                             +iColBold+'TOT. IMPONIBILE FATT.'   +fColBold
008900110128     C                             +iColBold+'TOT. SPEDIZIONI FATT.'   +fColBold
009000080929     C                   EVAL      VAWDTA = wBufferOut
009100080929     C                   WRITE     TIVAW000
009200071121     C                   ENDIF
009300991027     C*
009400080924     C                   READ      TIVGDTMP
009500060119     C*
009600080924     C                   DOW       not %eof(TIVGDTMP)
009700080924     C*
009800110128     C                   eval      DSFR_INFO = vgdDTA
009900071121     C*
010000071121     C                   clear                   TIVAW000
010100071121     C                   clear                   wBufferOut
010200071121     C*
010300071121     C* Compondo la stringa dati output relativa al record corrente
010400071121     C                   EXSR      wrifile
010500040719     C*
010600040719     C* In ogni caso storicizzo e cancello l'input sempre
010700080924     C                   DELETE    TIVGD000
010800040719     C*
010900040719     C* Proseguo con la lettura dell'input
011000080924     C                   READ      TIVGDTMP
011100060119     C*
011200030325     C                   ENDDO
011300991027     C*
011400030325     C                   EVAL      wrkesito = '0'
011500991027     C*
011600910830     C                   ENDSR
011700071121
011800071121
011900071121
012000071121     C     wrifile       BEGSR
012100071121     C*
012200080929     C                   EVAL      wBufferOut = iRow
012300110128     C                             +iCol+    %trim(%editc(§SFR_DFT  :'X')) +fCol
012400110128     C                             +iCol+    %trim(%editc(§SFR_KSC  :'X')) +fCol
012500110128     C                             +iCol           +%trim(§SFR_DKSC)       +fCol
012600110128     C                             +iCol+'0'+%trim(%editc(§SFR_COLE2:'3')) +fCol
012700110128     C                             +iCol+'0'+%trim(%editc(§SFR_COLE5:'3')) +fCol
012800110128     C                             +iCol+'0'+%trim(%editc(§SFR_IMV  :'3')) +fCol
012900110128     C                             +iCol+'0'+%trim(%editc(§SFR_SPF  :'3')) +fCol
013000071121     C                   EVAL      VAWDTA = wBufferOut
013100071121     C                   WRITE     TIVAW000
013200071121     C*
013300071121     C                   ENDSR
013400071121
013500071121
013600071121
013700991027
013800991027      /TITLE Operazioni iniziali.
013900991027     C     *inzsr        BEGSR
014000991027     C*
014100991027     C     *ENTRY        PLIST
014200991027     C                   parm                    prmppt
014300991027     C     wrkesito      parm      wrkesito      prmesito
014400991027     C*
014500991027     C                   ENDSR
014600071121     C*
014700071121** DEC - COSTANTI
014800071121<?xml version="1.0"?>
014900071121<?mso-application progid="Excel.Sheet"?>
015000071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
015100071121xmlns:o="urn:schemas-microsoft-com:office:office"
015200071121xmlns:x="urn:schemas-microsoft-com:office:excel"
015300071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
015400071121xmlns:html="http://www.w3.org/TR/REC-html40">
015500071121</Workbook>
015600071121<Worksheet ss:Name="PAG1">
015700071121</Worksheet>
015800071121<Table border="1">
015900071121</Table>
016000071121<Row>
016100071121</Row>
016200071121<Cell><Data ss:Type="String">
016300071121<Cell><Data ss:Type="Number">
016400071121</Data></Cell>
016500071121
016600071121
016700071121
