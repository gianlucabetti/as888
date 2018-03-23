000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600071122     FTITAS30C  IF   E           K DISK
000700071122     F                                     rename(titas000:titas0)
000800071122     F                                     rename(titas010:titas1)
000900071122     F                                     rename(titasP00:titasP)
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
005400071121     D wConsOkSiNo     S              1    inz(*blanks)
005500071121     D wConsOkSiNoDes  S            300    inz(*blanks)
005600071122     D wBufferOut      s          10000    varying inz
005700071122     D wIndirizzo      s             35    inz(*blanks)
005800071122     D wLocalita       s             35    inz(*blanks)
005900071122     D wDSP            s                   like(VACDCM) inz(*zeros)
006000071122     D wDCM            s                   like(VACDCM) inz(*zeros)
006100071122     D wHMC            s                   like(VACHMC) inz(*zeros)
006200030924
006300030325
006400030924     C* MAIN/
006500071121     C*
006600071121     C* Verifico se già presenti dati nel file d output
006700071121     C                   EXSR      chkRECFILMBR
006800071121     C*
006900071121     C* Effettuo elaborazione
007000071121     C                   EXSR      traduci
007100071121     C*
007200921023     C                   SETON                                        LR
007300991027
007400071121
007500071121
007600071121     C     chkRECFILMBR  BEGSR
007700071121     C*
007800071121     C                   READ      TIVAWWWT                               55
007900071121     C*
008000071121     C                   ENDSR
008100071121
008200071121
008300030924
008400991027     C     traduci       BEGSR
008500071121     C*
008600071121     C* Head iniziali della struttura HTML
008700071121     C                   EVAL      VAWDTA = iHtml + iBody + iTable
008800071121     C                   WRITE     TIVAW000
008900071121     C*
009000071121     C* Inizializzazone formato e variabili x output
009100071121     C                   clear                   TIVAW000
009200071121     C                   clear                   wBufferOut
009300071121     C*
009400071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009500071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
009600071121     C                   EVAL      wBufferOut = iThead + iRow
009700071121     C                             +iColBold+ 'N.Bolla'+fColBold
009800071121     C                             +iColBold+ 'N.Sped'+fColBold
009900071121     C                             +iColBold+ 'Data Sped'+fColBold
010000071121     C                             +iColBold+ 'Data Cons'+fColBold
010100071121     C                             +iColBold+ 'Stato'+fColBold
010200071121     C                             +iColBold+ 'Motivo mancata cons'+fColBold
010300071121     C                             +iColBold+ 'Ora Cons'+fColBold
010400071121     C                             +iColBold+ 'Destinatario'+fColBold
010500071121     C                             +iColBold+ 'Indirizzo'+fColBold
010600071121     C                             +iColBold+ 'Città'+fColBold
010700071121     C                             +iColBold+ 'Pr'+fColBold
010800071121     C                   EVAL      VAWDTA = wBufferOut
010900071121     C                   WRITE     TIVAW000
011000071121     C                   ENDIF
011100991027     C*
011200080924     C                   READ      TIVGDTMP
011300060119     C*
011400080924     C                   DOW       not %eof(TIVGDTMP)
011500080924     C*
011600080924     C                   eval      fnvac00t = vgdDTA
011700071121     C*
011800071121     C                   clear                   wConsOkSiNo
011900071121     C                   clear                   wConsOkSiNoDes
012000071121     C                   clear                   TIVAW000
012100071121     C                   clear                   wBufferOut
012200071122     C                   clear                   wIndirizzo
012300071122     C                   clear                   wLocalita
012400071122     C*
012500071122     C* Compongo subito la data spedizine
012600071122     C                   eval      wDSP = vacAAS*10000+vacMGS
012700040719     C*
012800071121     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
012900060208     C                   IF        VACDCM > *zeros AND
013000071121     C                             (VACCCA = *blanks OR VACCCA = '1')
013100071121     C                   EVAL      wConsOkSiNo    = 'S'
013200071121     C                   ELSE
013300071121     C                   EVAL      wConsOkSiNo    = 'N'
013400071121     C*
013500071121     C* A questo punto stabilisco la descrizione
013600071121     C                   IF        vacCCA = '1'
013700071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione dirottata '+
013800071121     C                                              'ad altra filiale.'
013900071121     C                   ENDIF
014000071121     C                   IF        vacCCA = '2'
014100071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione resa al '+
014200071121     C                                              'mittente.'
014300071121     C                   ENDIF
014400071121     C                   IF        vacCCA = '3'
014500071121     C                   EVAL      wConsOkSiNoDes = 'Rapinata merce.'
014600071121     C                   ENDIF
014700071121     C                   IF        vacCCA = '4'
014800071121     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso.'
014900071121     C                   ENDIF
015000071121     C                   IF        vacCCA = '5'
015100071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
015200071121     C                                              'con pratica anomalia.'
015300071121     C                   ENDIF
015400071121     C                   IF        vacCCA = '6'
015500071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione chiusa '+
015600071121     C                                              'con avaria resa al '+
015700071121     C                                              'mittente.'
015800071121     C                   ENDIF
015900071121     C                   IF        vacCCA = 'A'
016000071121     C                   EVAL      wConsOkSiNoDes = 'Apertura pratica '+
016100071121     C                                              'anomalia.'
016200071121     C                   ENDIF
016300071121     C                   IF        vacCCA = 'C'
016400071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione in consegna.'
016500071121     C                   ENDIF
016600071121     C                   IF        vacCCA = 'A'
016700071121     C                   EVAL      wConsOkSiNoDes = 'Spedizione ripristinata '+
016800071121     C                                              'dopo errata imputazione '+
016900071121     C                                              'di consegna.'
017000071121     C                   ENDIF
017100071121     C                   IF        vacCCA = 'S'
017200071121     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta.'
017300071121     C                   ENDIF
017400071121     C*
017500071121     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
017600071121     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso.'
017700071121     C                   ENDIF
017800071121     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
017900071121     C                   EVAL      wConsOkSiNoDes = 'Aperta giacienza.'
018000071121     C                   ENDIF
018100071121     C*
018200071121     C                   ENDIF
018300071122     C*
018400071122     C* Aggancio la bolla relativa x il completamento dei dati
018500071122     C     KEYtas30      chain     titas30c
018600071122     C                   if        %found(titas30c)
018700071122     C*
018800071122     C* reperisco i dati del destinatario nn già presenti sul FNVAC
018900071122     C                   eval      wIndirizzo = tasIND
019000071122     C                   eval      wLocalita  = tasLOD
019100071122     C                   endif
019200071121     C*
019300071121     C* Compondo la stringa dati output relativa al record corrente
019400071121     C                   EXSR      wrifile
019500040719     C*
019600040719     C* In ogni caso storicizzo e cancello l'input sempre
019700080924     C                   DELETE    TIVGD000
019800040719     C*
019900040719     C* Proseguo con la lettura dell'input
020000080924     C                   READ      TIVGDTMP
020100060119     C*
020200030325     C                   ENDDO
020300071121     C*
020400071121     C* Head finali della struttura HTML
020500080129     C****               EVAL      VAWDTA = fTable + fBody + fHtml
020600080129     C****               WRITE     TIVAW000
020700991027     C*
020800030325     C                   EVAL      wrkesito = '0'
020900991027     C*
021000910830     C                   ENDSR
021100071121
021200071121
021300071121
021400071121     C     wrifile       BEGSR
021500071122     C*
021600071122     C* Se spedizione nn consegnata OK => azzero cmq la data consegna (ovvero nn indico
021700071122     C* la data stato)
021800071122     C                   IF        wConsOkSiNo = 'S'
021900071122     C                   EVAL      wDCM = VACDCM
022000071122     C                   EVAL      wHMC = VACHMC
022100071122     C                   ELSE
022200071122     C                   EVAL      wDCM = *zeros
022300071122     C                   EVAL      wHMC = *zeros
022400071122     C                   ENDIF
022500071121     C*
022600071121     C                   EVAL      wBufferOut = iRow +
022700071122     C                             iCol+%trim(%editc(VACRMN:'Q'))+fCol+
022800071122     C                             iCol+cApi+%trim(%editw(VACAAS:'0    '))+
022900071122     C                                  %trim(%editw(VACLNP:'0   '))+
023000071122     C                                  %trim(%editw(VACNRS:'0  '))+
023100071122     C                                  %trim(%editw(VACNSP:'0       '))+fCol+
023200071122     C                             iCol+%trim(%editw(wDSP:'    /  /  '))+fCol+
023300071122     C                             iCol+%trim(%editw(wDCM:'    /  /  '))+fCol+
023400071121     C                                  iCol+%trim(wConsOkSiNo)+fCol+
023500071121     C                                  iCol+%trim(wConsOkSiNoDes)+fCol+
023600071122     C                             iCol+%trim(%editw(wHMC:'  :  '))+fCol+
023700071121     C                                  iCol+%trim(VACRSD)+fCol+
023800071122     C                                  iCol+%trim(wIndirizzo)+fCol+
023900071122     C                                  iCol+%trim(wLocalita)+fCol+
024000071121     C                                  iCol+%trim(VACPRD)+fCol
024100071121     C*
024200071121     C                   EVAL      VAWDTA = wBufferOut
024300071121     C                   WRITE     TIVAW000
024400071121     C*
024500071121     C                   ENDSR
024600071121
024700071121
024800071121
024900991027
025000991027      /TITLE Operazioni iniziali.
025100991027     C     *inzsr        BEGSR
025200991027     C*
025300991027     C     *ENTRY        PLIST
025400991027     C                   parm                    prmppt
025500991027     C     wrkesito      parm      wrkesito      prmesito
025600071122     C*
025700071122     C* TITAS30C - Completa
025800071122     C     KEYtas30      KLIST
025900071122     C                   KFLD                    vacAAS
026000071122     C                   KFLD                    vacLNP
026100071122     C                   KFLD                    vacNRS
026200071122     C                   KFLD                    vacNSP
026300991027     C*
026400991027     C                   ENDSR
026500071121     C*
026600071121** DEC - COSTANTI
026700071121<?xml version="1.0"?>
026800071121<?mso-application progid="Excel.Sheet"?>
026900071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
027000071121xmlns:o="urn:schemas-microsoft-com:office:office"
027100071121xmlns:x="urn:schemas-microsoft-com:office:excel"
027200071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
027300071121xmlns:html="http://www.w3.org/TR/REC-html40">
027400071121</Workbook>
027500071121<Worksheet ss:Name="PAG1">
027600071121</Worksheet>
027700071121<Table border="1">
027800071121</Table>
027900071121<Row>
028000071121</Row>
028100071121<Cell><Data ss:Type="String">
028200071121<Cell><Data ss:Type="Number">
028300071121</Data></Cell>
028400071121
028500071121
028600071121
