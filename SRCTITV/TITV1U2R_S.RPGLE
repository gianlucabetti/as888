000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090504     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
001300090504
001400090504
001500090504     D*------------------
001600090504     D* LINKING A DEFINIZIONI ESTERNE
001700090504     D*------------------
001800090504     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
001900090504     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002000090504
002100090504
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
004900090504     D wBufferOut      s          10000    varying inz
005000091228     D wDataCaus       s              8  0 inz
005100091228     D wCausaleCli     s             10    inz
005200071211     D*
005300030924
005400030325
005500030924     C* MAIN/
005600071121     C*
005700071121     C* Verifico se già presenti dati nel file d output
005800071121     C                   EXSR      chkRECFILMBR
005900071121     C*
006000071121     C* Effettuo elaborazione
006100071121     C                   EXSR      traduci
006200071121     C*
006300921023     C                   SETON                                        LR
006400991027
006500071121
006600071121
006700071121     C     chkRECFILMBR  BEGSR
006800071121     C*
006900071121     C                   READ      TIVAWWWT                               55
007000071121     C*
007100071121     C                   ENDSR
007200071121
007300071121
007400030924
007500991027     C     traduci       BEGSR
007600071121     C*
007700071121     C* Head iniziali della struttura HTML
007800071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
007900071210     C   55              WRITE     TIVAW000
008000071121     C*
008100071121     C* Inizializzazone formato e variabili x output
008200071121     C                   clear                   TIVAW000
008300071121     C                   clear                   wBufferOut
008400071121     C*
008500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008700071121     C                   EVAL      wBufferOut = iThead + iRow
008800091228     C                             +iColBold+ 'N.BOLLA'+fColBold
008900091228     C                             +iColBold+ 'N.BOLLA'+fColBold
009000091228     C                             +iColBold+ 'ANNO'+fColBold
009100091228     C                             +iColBold+ 'MESE/GIORNO'+fColBold
009200091228     C                             +iColBold+ 'RAGIONE SOCIALE DEST.'+fColBold
009300071213     C                             +iColBold+ 'INDIRIZZO DEST.'+fColBold
009400071213     C                             +iColBold+ 'CAP DEST.'+fColBold
009500071213     C                             +iColBold+ 'LOCALITA DEST.'+fColBold
009600071213     C                             +iColBold+ 'PROVINCIA DEST.'+fColBold
009700091228     C                             +iColBold+ 'N.COLLI'+fColBold
009800091228     C                             +iColBold+ 'PESO'+fColBold
009900091228     C                             +iColBold+ 'CODICE CLIENTE'+fColBold
010000090504     C                             +iColBold+ 'DATA CONSEGNA'+fColBold
010100091228     C                             +iColBold+ 'CAUSALE'+fColBold
010200071121     C                   EVAL      VAWDTA = wBufferOut
010300071121     C                   WRITE     TIVAW000
010400071121     C                   ENDIF
010500991027     C*
010600080924     C                   READ      TIVGDTMP
010700060119     C*
010800080924     C                   DOW       not %eof(TIVGDTMP)
010900080924     C*
011000090506     C                   setoff                                       54
011100080924     C                   eval      fnvac00t = vgdDTA
011200071121     C*
011300090504     C                   clear                   TIVAW000
011400090504     C                   clear                   TITAS0
011500071121     C                   clear                   wBufferOut
011600091228     C                   clear                   wDataCaus
011700091228     C                   clear                   wCausaleCli
011800040719     C*
011900090504     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
012000090504     C                   movel     *blanks       wEsito1           1
012100090504     C                   movel     *blanks       wEsito2           1
012200100107     C                   movel     *blanks       wEsito3           1
012300090504     C*
012400090504     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
012500090504     C                                                vacAAS
012600090504     C                                               :vacLNP
012700090504     C                                               :vacNRS
012800090504     C                                               :vacNSP
012900090504     C                                               :pOutLblTyp
013000090504     C                                               :pOutAnnoBO
013100090504     C                                               :pOutLineaParBO
013200090504     C                                               :pOutSerieBO
013300090504     C                                               :pOutNumSpedBO
013400090504     C                                               :pOutDcmBO
013500090504     C                                               :pOutCcaBO
013600090504     C                                               :pOutRblBO))
013700090504     C*
013800090504     C* Chiamata metodo GetLastChild
013900090504     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
014000090504     C                                                vacAAS
014100090504     C                                               :vacLNP
014200090504     C                                               :vacNRS
014300090504     C                                               :vacNSP
014400090504     C                                               :pOutAnnoFI
014500090504     C                                               :pOutLineaParFI
014600090504     C                                               :pOutSerieFI
014700090504     C                                               :pOutNumSpedFI
014800090504     C                                               :pOutDcmFI
014900090504     C                                               :pOutCcaFI))
015000100107     C*
015100100107     C* Chiamata metodo GetFirstChild
015200100107     C                   eval      wEsito3 = %char(UBLBLSPE_GetFirstChild(
015300100107     C                                                vacAAS
015400100107     C                                               :vacLNP
015500100107     C                                               :vacNRS
015600100107     C                                               :vacNSP
015700100107     C                                               :pOutAnnoFFI
015800100107     C                                               :pOutLineaPaFFI
015900100107     C                                               :pOutSerieFFI
016000100107     C                                               :pOutNumSpedFFI
016100100107     C                                               :pOutDcmFFI
016200100107     C                                               :pOutCcaFFI))
016300100107     C* ...dirottamento
016400100107     C                   if        pOutLblTyp = 'O'    AND
016500100107     C                             vacDCM     = *zeros AND
016600100107     C                             vacCCA     = '1'
016700100107     C* Aggancio la bolla "figlia" relativa x il completamento dei dati
016800100107     C     KEYtas30_PF   chain     titas30c
016900100107     C                   if        %found(titas30c)
017000100107     C                   eval      wDataCaus   = tasAAS*10000+tasMGS
017100100107     C                   eval      wCausaleCli = 'D'
017200100107     C                   seton                                        54
017300100107     C                   endif
017400100107     C*
017500100107     C                   else
017600100107     C*
017700100107     C* Aggancio la bolla relativa x il completamento dei dati
017800100107     C     KEYtas30_P    chain     titas30c
017900100107     C                   if        %found(titas30c)
018000100107     C*
018100100107     C                   select
018200091228     C*
018300091228     C* ...lasciato avviso
018400091228     C                   when      vacDLA >  vacDAG AND
018500091228     C                             vacDLA >  vacDCM
018600091228     C*
018700091228     C* ...giacenza
018800091228     C                   when      vacDAG >= vacDLA AND
018900091228     C                             vacDAG >= vacDCM
019000091228     C                   eval      wDataCaus   = vacDAG
019100091228     C                   eval      wCausaleCli = 'G'
019200091228     C                   seton                                        54
019300091228     C*
019400091228     C* ..."others"
019500091228     C                   when      vacDCM >  vacDLA AND
019600091228     C                             vacDCM >  vacDAG
019700091228     C                   eval      wDataCaus   = vacDCM
019800091228     C                   select
019900091228     C* ...messe in consegna
020000091228     C                   when      vacCCA = 'C'
020100091228     C                   if        pOutCcaBO = '2' OR
020200091228     C                             pOutCcaFI = '2'
020300091228     C                   eval      wCausaleCli = 'RM'
020400091228     C                   seton                                        54
020500091228     C                   else
020600091228     C                   eval      wCausaleCli = 'M'
020700091228     C                   seton                                        54
020800091228     C                   endif
020900091228     C* ...chiusura spedizione con consegna
021000091228     C                   when      vacCCA = *blanks OR
021100100105     C                             vacCCA = '2'     OR
021200100105     C                             vacCCA = '6'
021300091228     C                   if        pOutCcaBO = '2' OR
021400100105     C                             pOutCcaFI = '2' OR
021500100105     C                             pOutCcaBO = '6' OR
021600100105     C                             pOutCcaFI = '6'
021700091228     C                   eval      wCausaleCli = 'R'
021800091228     C                   seton                                        54
021900091228     C                   else
022000091228     C                   eval      wCausaleCli = 'C'
022100091228     C                   seton                                        54
022200091228     C                   endif
022300100218     C* ...chiusura spedizione con consegna
022400100218     C                   when      vacCCA = '5'
022500100218     C                   eval      wCausaleCli = '5'
022600100218     C                   seton                                        54
022700100105     C* ...apertura pratica anomalia
022800100105     C                   when      vacCCA = 'A'
022900100105     C                   eval      wCausaleCli = 'A'
023000100105     C                   seton                                        54
023100100105     C* ...merce distrutta
023200100105     C                   when      vacCCA = 'S'
023300100105     C                   eval      wCausaleCli = 'S'
023400100105     C                   seton                                        54
023500091228     C*
023600091228     C                   endsl
023700091228     C                   endsl
023800100107     C*
023900100107     C                   endif
024000100107     C*
024100100107     C                   endif
024200071121     C*
024300071121     C* Compondo la stringa dati output relativa al record corrente
024400091228     C   54              EXSR      wrifile
024500040719     C*
024600040719     C* In ogni caso storicizzo e cancello l'input sempre
024700080924     C                   DELETE    TIVGD000
024800040719     C*
024900040719     C* Proseguo con la lettura dell'input
025000080924     C                   READ      TIVGDTMP
025100060119     C*
025200030325     C                   ENDDO
025300071121     C*
025400071121     C* Head finali della struttura HTML
025500071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
025600071210     C***                WRITE     TIVAW000
025700991027     C*
025800030325     C                   EVAL      wrkesito = '0'
025900991027     C*
026000910830     C                   ENDSR
026100071121
026200071121
026300071121
026400071121     C     wrifile       BEGSR
026500071121     C*
026600071121     C                   EVAL      wBufferOut = iRow +
026700091228     C                                  iCol+%trim(%char(vacRMN))+fCol+
026800091228     C                                  iCol+%trim(vacRMA)+fCol+
026900091228     C                                  iCol+%trim(%char(vacAAS))+fCol+
027000091228     C                                  iCol+%trim(%char(vacMGS))+fCol+
027100090504     C                                  iCol+%trim(tasRSD)+fCol+
027200090504     C                                  iCol+%trim(tasIND)+fCol+
027300090504     C                                  iCol+%trim(tasCAD)+fCol+
027400090504     C                                  iCol+%trim(tasLOD)+fCol+
027500090504     C                                  iCol+%trim(tasPRD)+fCol+
027600091228     C                                  iCol+%trim(%char(vacNCL))+fCol+
027700091228     C                                  iCol+%trim(%char(vacPKB))+fCol+
027800091228     C                                  iCol+%trim(%char(vacCCM))+fCol+
027900091228     C                                  iCol+%trim(%editc(wDataCaus:'Q'))+fCol+
028000091228     C                                  iCol+%trim(wCausaleCli)+fCol
028100071121     C*
028200071121     C                   EVAL      VAWDTA = wBufferOut
028300071121     C                   WRITE     TIVAW000
028400071121     C*
028500071121     C                   ENDSR
028600071121
028700071121
028800991027
028900991027      /TITLE Operazioni iniziali.
029000991027     C     *inzsr        BEGSR
029100991027     C*
029200991027     C     *ENTRY        PLIST
029300991027     C                   parm                    prmppt
029400991027     C     wrkesito      parm      wrkesito      prmesito
029500071210     C*
029600071213     C* TITAS30C - Parziale
029700090504     C     KEYtas30_P    KLIST
029800090505     C                   KFLD                    vacAAS
029900090505     C                   KFLD                    vacLNP
030000090505     C                   KFLD                    vacNRS
030100090505     C                   KFLD                    vacNSP
030200100107     C*
030300100107     C* TITAS30C - Parziale
030400100107     C     KEYtas30_PF   KLIST
030500100107     C                   KFLD                    pOutAnnoFFI
030600100107     C                   KFLD                    pOutLineaPaFFI
030700100107     C                   KFLD                    pOutSerieFFI
030800100107     C                   KFLD                    pOutNumSpedFFI
030900991027     C*
031000991027     C                   ENDSR
031100071121     C*
