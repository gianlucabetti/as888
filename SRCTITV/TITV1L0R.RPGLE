000100090316     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700081210     D dtasco_I      e ds
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
009100081210     C                              iColBold+'ANNO SPED.'              +fColBold
009200081210     C                             +iColBold+'MESE/GIORNO SPED.'       +fColBold
009300081210     C                             +iColBold+'LINEA PARTENZA SPED.'    +fColBold
009400081210     C                             +iColBold+'SERIE SPED.'             +fColBold
009500081210     C                             +iColBold+'NUMERO SPED.'            +fColBold
009600081210     C                             +iColBold+'COD.CLIENTE MITTENTE'    +fColBold
009700081210     C                             +iColBold+'RAG.SOCIALE MITTENTE'    +fColBold
009800081210     C                             +iColBold+'RIFERIM.MITT. NUMERICO'  +fColBold
009900081210     C                             +iColBold+'RIFERIM.MITT. ALFABETICO'+fColBold
010000081210     C                             +iColBold+'RAGIONE SOCIALE DEST.'   +fColBold
010100081210     C                             +iColBold+'INDIRIZZO DEST.'         +fColBold
010200081210     C                             +iColBold+'CAP DEST.'               +fColBold
010300080929     C                   EVAL      VAWDTA = wBufferOut
010400080929     C                   WRITE     TIVAW000
010500080929     C*
010600080929     C                   EVAL      wBufferOut =
010700081210     C                              iColBold+'LOCALITA  DEST.'         +fColBold
010800081210     C                             +iColBold+'PROVINCIA DEST.'         +fColBold
010900081210     C                             +iColBold+'NAZIONE DEST.'           +fColBold
011000081210     C                             +iColBold+'PESO KG DA FATTURARE'    +fColBold
011100081210     C                             +iColBold+'VOLUME DA FATTURARE'     +fColBold
011200081210     C                             +iColBold+'NUMERO COLLI'            +fColBold
011300081210     C                             +iColBold+'DATA CONSEGNA MERCE'     +fColBold
011400081210     C                             +iColBold+'ORA CONSEGNA MERCE'      +fColBold
011500081210     C                             +iColBold+'CODICE CONSEGNA ANOMALA' +fColBold
011600081210     C                             +iColBold+'NON CONFORMITA CLIENTE'  +fColBold
011700071121     C                   EVAL      VAWDTA = wBufferOut
011800071121     C                   WRITE     TIVAW000
011900071121     C                   ENDIF
012000991027     C*
012100080924     C                   READ      TIVGDTMP
012200060119     C*
012300080924     C                   DOW       not %eof(TIVGDTMP)
012400080924     C*
012500081210     C                   eval      dtasco_I = vgdDTA
012600071121     C*
012700071121     C                   clear                   TIVAW000
012800071121     C                   clear                   wBufferOut
012900071121     C*
013000071121     C* Compondo la stringa dati output relativa al record corrente
013100071121     C                   EXSR      wrifile
013200040719     C*
013300040719     C* In ogni caso storicizzo e cancello l'input sempre
013400080924     C                   DELETE    TIVGD000
013500040719     C*
013600040719     C* Proseguo con la lettura dell'input
013700080924     C                   READ      TIVGDTMP
013800060119     C*
013900030325     C                   ENDDO
014000991027     C*
014100030325     C                   EVAL      wrkesito = '0'
014200991027     C*
014300910830     C                   ENDSR
014400071121
014500071121
014600071121
014700071121     C     wrifile       BEGSR
014800071121     C*
014900080929     C                   EVAL      wBufferOut = iRow
015000081210     C                             +iCol+%trim(§TACAAS)                    +fCol
015100081210     C                             +iCol+%trim(§TACMGS)                    +fCol
015200081210     C                             +iCol+%trim(§TACLNP)                    +fCol
015300081210     C                             +iCol+%trim(§TACNRS)                    +fCol
015400081210     C                             +iCol+%trim(§TACNSP)                    +fCol
015500081210     C                             +iCol+%trim(%editc(§TACCCM:'X'))        +fCol
015600081210     C                             +iCol+%trim(§TACRSM)                    +fCol
015700081210     C                             +iCol+%trim(§TACRMN)                    +fCol
015800081210     C                             +iCol+%trim(§TACRMA)                    +fCol
015900081210     C                             +iCol+%trim(§TACRSD)                    +fCol
016000081210     C                             +iCol+%trim(§TACIND)                    +fCol
016100081210     C                             +iCol+%trim(§TACCAD)                    +fCol
016200081210     C                             +iCol+%trim(§TACLOD)                    +fCol
016300081210     C                             +iCol+%trim(§TACPRD)                    +fCol
016400081210     C                             +iCol+%trim(§TACNZD)                    +fCol
016500090121     C                             +iCol+'0'+%trim(%editc(§TACPKF:'3'))    +fCol
016600090121     C                             +iCol+'0'+%trim(%editc(§TACVLF:'3'))    +fCol
016700090121     C                             +iCol+'0'+%trim(%editc(§TACNCL:'3'))    +fCol
016800081210     C                             +iCol+%trim(%editc(§TACDCM:'X'))        +fCol
016900081210     C                             +iCol+%trim(%editc(§TACHMC:'X'))        +fCol
017000081210     C                             +iCol+%trim(§TACCCA)                    +fCol
017100081210     C                             +iCol+%trim(%editc(§TACNRC:'N'))        +fCol
017200071121     C*
017300071121     C                   EVAL      VAWDTA = wBufferOut
017400071121     C                   WRITE     TIVAW000
017500071121     C*
017600071121     C                   ENDSR
017700071121
017800071121
017900071121
018000991027
018100991027      /TITLE Operazioni iniziali.
018200991027     C     *inzsr        BEGSR
018300991027     C*
018400991027     C     *ENTRY        PLIST
018500991027     C                   parm                    prmppt
018600991027     C     wrkesito      parm      wrkesito      prmesito
018700991027     C*
018800991027     C                   ENDSR
018900071121     C*
019000071121** DEC - COSTANTI
019100071121<?xml version="1.0"?>
019200071121<?mso-application progid="Excel.Sheet"?>
019300071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
019400071121xmlns:o="urn:schemas-microsoft-com:office:office"
019500071121xmlns:x="urn:schemas-microsoft-com:office:excel"
019600071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
019700071121xmlns:html="http://www.w3.org/TR/REC-html40">
019800071121</Workbook>
019900071121<Worksheet ss:Name="PAG1">
020000071121</Worksheet>
020100071121<Table border="1">
020200071121</Table>
020300071121<Row>
020400071121</Row>
020500071121<Cell><Data ss:Type="String">
020600071121<Cell><Data ss:Type="Number">
020700071121</Data></Cell>
020800071121
020900071121
021000071121
