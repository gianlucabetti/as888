000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500090326     FTIVAWWWT  UF A E             DISK    commit
000600030924
000700080929     D dcanom_I      e ds
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
001800071121     D*------------------
001900071121     D* DS "XSRDA8" - CONTROLLA DATA (8)
002000071121     D*------------------
002100071121     D WLBDA8          DS                  INZ
002200071121     D  G08DAT                 1      8  0
002300071121     D  G08INV                 9     16  0
002400071121     D  G08ERR                17     17
002500071121     D  G08TGI                18     22  0
002600071121
002700071121     D*------------
002800071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002900071121     D*------------
003000071121     D iHtml           s             10    inz('<html>')
003100071121     D fHtml           s             10    inz('</html>')
003200071121     D iBody           s             10    inz('<body>')
003300071121     D fBody           s             10    inz('</body>')
003400071121     D iTable          s             20    inz('<table border="1">')
003500071121     D fTable          s             10    inz('</table>')
003600071121     D iThead          s             10    inz('<thead>')
003700071121     D fThead          s             10    inz('</thead>')
003800071121     D iRow            s              5    inz('<tr>')
003900071121     D fRow            s              5    inz('</tr>')
004000071121     D iCol            s              5    inz('<td>')
004100071121     D fCol            s              5    inz('</td>')
004200071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004300071121     D fColBold        s              5    inz('</th>')
004400071122     D cApi            s              1    inz('''')
004500071121
004600071121
004700071121     D*------------
004800071121     D* VARIABILI D WRK
004900071121     D*------------
005000030924     D prmppt          S             50
005100030924     D prmesito        S              1
005200030924     D wrkesito        S                   like(prmesito)
005300080929     D wBufferOut      s          10000    varying inz
005400071211     D*
005500030924
005600030325
005700030924     C* MAIN/
005800071121     C*
005900071121     C* Verifico se già presenti dati nel file d output
006000071121     C                   EXSR      chkRECFILMBR
006100071121     C*
006200071121     C* Effettuo elaborazione
006300071121     C                   EXSR      traduci
006400071121     C*
006500921023     C                   SETON                                        LR
006600991027
006700071121
006800071121
006900071121     C     chkRECFILMBR  BEGSR
007000071121     C*
007100071121     C                   READ      TIVAWWWT                               55
007200071121     C*
007300071121     C                   ENDSR
007400071121
007500071121
007600030924
007700991027     C     traduci       BEGSR
007800071121     C*
007900071121     C* Head iniziali della struttura HTML
008000071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
008100071210     C   55              WRITE     TIVAW000
008200071121     C*
008300071121     C* Inizializzazone formato e variabili x output
008400071121     C                   clear                   TIVAW000
008500071121     C                   clear                   wBufferOut
008600071121     C*
008700071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008800080929     C                   IF        %subst(prmppt:1:1) = 'N' OR *IN55 = *OFF
008900080929     C                   ELSE
009000080929     C                   EVAL      wBufferOut = iThead + iRow +
009100080929     C                              iColBold+'ANNO SPEDIZIONE'         +fColBold
009200080929     C                             +iColBold+'MESE/GIORNO SPEDIZIONE'  +fColBold
009300080929     C                             +iColBold+'LINEA PARTENZA SPEDIZION'+fColBold
009400080929     C                             +iColBold+'SERIE SPEDIZIONE'        +fColBold
009500080929     C                             +iColBold+'NUMERO SPEDIZIONE'       +fColBold
009600080929     C                             +iColBold+'RIFERIM.MITTENTE NUMERIC'+fColBold
009700080929     C                             +iColBold+'RIFERIM.MITTENTE ALFABET'+fColBold
009800080929     C                             +iColBold+'RAGIONE SOCIALE DESTINAT'+fColBold
009900080929     C                             +iColBold+'TIPO ANOMALIA DANNI'     +fColBold
010000080929     C                             +iColBold+'DESCRIZIONE ANOMALIA'    +fColBold
010100080929     C                             +iColBold+'ANNO C.A.'               +fColBold
010200080929     C                             +iColBold+'MESE/GIORNO APERTURA CA' +fColBold
010300080929     C                             +iColBold+'FILIALE C.A.'            +fColBold
010400080929     C                             +iColBold+'NUMERO CA'               +fColBold
010500080929     C                             +iColBold+'DATA CHIUSURA CA'        +fColBold
010600080929     C                   EVAL      VAWDTA = wBufferOut
010700080929     C                   WRITE     TIVAW000
010800080929     C*
010900080929     C                   EVAL      wBufferOut =
011000080929     C                              iColBold+'CAUSALE CHIUSURA CA'     +fColBold
011100080929     C                             +iColBold+'DESCRIZIONE CHIUSURA C.A'+fColBold
011200080929     C                             +iColBold+'FASE AVANZAMENTO CA'     +fColBold
011300080929     C                             +iColBold+'DESCRIZIONE FASE C.A.'   +fColBold
011400080929     C                             +iColBold+'DATA FASE CA'            +fColBold
011500080929     C                             +iColBold+'IMPORTO MASSIMO RISARCIB'+fColBold
011600080929     C                             +iColBold+'DIVISA IMP.MASS.RISARCIB'+fColBold
011700080929     C                             +iColBold+'IMPORTO RICHIESTO'       +fColBold
011800080929     C                             +iColBold+'DIVISA IMPORTO RICHIESTO'+fColBold
011900080929     C                             +iColBold+'IMPORTO LIQUIDATO'       +fColBold
012000080929     C                             +iColBold+'DIVISA IMPORTO LIQUIDATO'+fColBold
012100080929     C                             +iColBold+'NUMERO ASSEGNO'          +fColBold
012200080929     C                             +iColBold+'DATA EMISSIONE ASSEGNO'  +fColBold
012300080929     C                             +iColBold+'ABI NS. PAGAMENTO'       +fColBold
012400080929     C                             +iColBold+'CAB NS. PAGAMENTO'       +fColBold
012500071121     C                   EVAL      VAWDTA = wBufferOut
012600071121     C                   WRITE     TIVAW000
012700071121     C                   ENDIF
012800991027     C*
012900080924     C                   READ      TIVGDTMP
013000060119     C*
013100080924     C                   DOW       not %eof(TIVGDTMP)
013200080924     C*
013300080929     C                   eval      dcanom_I = vgdDTA
013400071121     C*
013500071121     C                   clear                   TIVAW000
013600071121     C                   clear                   wBufferOut
013700071121     C*
013800071121     C* Compondo la stringa dati output relativa al record corrente
013900071121     C                   EXSR      wrifile
014000040719     C*
014100040719     C* In ogni caso storicizzo e cancello l'input sempre
014200080924     C                   DELETE    TIVGD000
014300040719     C*
014400040719     C* Proseguo con la lettura dell'input
014500080924     C                   READ      TIVGDTMP
014600060119     C*
014700030325     C                   ENDDO
014800991027     C*
014900030325     C                   EVAL      wrkesito = '0'
015000991027     C*
015100910830     C                   ENDSR
015200071121
015300071121
015400071121
015500071121     C     wrifile       BEGSR
015600071121     C*
015700080929     C                   EVAL      wBufferOut = iRow
015800080929     C                             +iCol+%trim(§CAIAAS)                    +fCol
015900080929     C                             +iCol+%trim(§CAIMGS)                    +fCol
016000080929     C                             +iCol+%trim(§CAILNP)                    +fCol
016100080929     C                             +iCol+%trim(§CAINRS)                    +fCol
016200080929     C                             +iCol+%trim(§CAINSP)                    +fCol
016300080929     C                             +iCol+%trim(§CAIRMN)                    +fCol
016400080929     C                             +iCol+%trim(§CAIRMA)                    +fCol
016500080929     C                             +iCol+%trim(§CAIRSD)                    +fCol
016600080929     C                             +iCol+%trim(§CAITAD)                    +fCol
016700080929     C                             +iCol+%trim(§CAITADD)                   +fCol
016800080929     C                             +iCol+%trim(§CAIAAC)                    +fCol
016900080929     C                             +iCol+%trim(§CAIMGC)                    +fCol
017000080929     C                             +iCol+%trim(§CAIFIL)                    +fCol
017100080929     C                             +iCol+%trim(§CAINCA)                    +fCol
017200080929     C                             +iCol+%trim(§CAIDCH)                    +fCol
017300080929     C                             +iCol+%trim(§CAICCH)                    +fCol
017400080929     C                             +iCol+%trim(§CAICCHD)                   +fCol
017500080929     C                             +iCol+%trim(§CAIFCA)                    +fCol
017600080929     C                             +iCol+%trim(§CAIFCAD)                   +fCol
017700080929     C                             +iCol+%trim(§CAIDFC)                    +fCol
017800080929     C                             +iCol+%trim(§CAIIPV)                    +fCol
017900080929     C                             +iCol+%trim(§CAIVPV)                    +fCol
018000080929     C                             +iCol+%trim(§CAIIPR)                    +fCol
018100080929     C                             +iCol+%trim(§CAIVPR)                    +fCol
018200080929     C                             +iCol+%trim(§CAIIPL)                    +fCol
018300080929     C                             +iCol+%trim(§CAIVPL)                    +fCol
018400080929     C                             +iCol+%trim(§CAIASN)                    +fCol
018500080929     C                             +iCol+%trim(§CAIDEA)                    +fCol
018600080929     C                             +iCol+%trim(§CAIABI)                    +fCol
018700080929     C                             +iCol+%trim(§CAICAB)                    +fCol
018800071121     C*
018900071121     C                   EVAL      VAWDTA = wBufferOut
019000071121     C                   WRITE     TIVAW000
019100071121     C*
019200071121     C                   ENDSR
019300071121
019400071121
019500071121
019600991027
019700991027      /TITLE Operazioni iniziali.
019800991027     C     *inzsr        BEGSR
019900991027     C*
020000991027     C     *ENTRY        PLIST
020100991027     C                   parm                    prmppt
020200991027     C     wrkesito      parm      wrkesito      prmesito
020300991027     C*
020400991027     C                   ENDSR
020500071121     C*
020600071121** DEC - COSTANTI
020700071121<?xml version="1.0"?>
020800071121<?mso-application progid="Excel.Sheet"?>
020900071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
021000071121xmlns:o="urn:schemas-microsoft-com:office:office"
021100071121xmlns:x="urn:schemas-microsoft-com:office:excel"
021200071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
021300071121xmlns:html="http://www.w3.org/TR/REC-html40">
021400071121</Workbook>
021500071121<Worksheet ss:Name="PAG1">
021600071121</Worksheet>
021700071121<Table border="1">
021800071121</Table>
021900071121<Row>
022000071121</Row>
022100071121<Cell><Data ss:Type="String">
022200071121<Cell><Data ss:Type="Number">
022300071121</Data></Cell>
022400071121
022500071121
022600071121
