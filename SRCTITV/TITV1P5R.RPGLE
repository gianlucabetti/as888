000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090504     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600071210     FTITAS30C  IF   E           K DISK
000700071210     F                                     rename(titas000:titas0)
000800071210     F                                     rename(titas010:titas1)
000900071210     F                                     rename(titasP00:titasP)
001000090504     FTITA430C  IF   E           K DISK
001100071213     FTIGCP51L  IF   E           K DISK
001200090504     FTABEL00F  IF   E           K DISK
001300030924
001400080924     D fnvac00t      e ds
001500090504     D ds2a          e ds
001600090504     D sk2A            s              3    dim(200)
001700090505     D sk2Adesc        s                   dim(200) like(§2ADES)
001800090504     D w2ADES          s                   like(§2ADES)
001900090504     D wNOTE1          s                   like(TA4NOT)
002000090504     D wNOTE2          s                   like(TA4NOT)
002100071121
002200090504
002300090504
002400090504     D*------------------
002500090504     D* LINKING A DEFINIZIONI ESTERNE
002600090504     D*------------------
002700090504     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002800090504     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002900090504
003000090504
003100071121
003200071121     D*------------
003300071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003400071121     D*------------
003500071121     D iHtml           s             10    inz('<html>')
003600071121     D fHtml           s             10    inz('</html>')
003700071121     D iBody           s             10    inz('<body>')
003800071121     D fBody           s             10    inz('</body>')
003900071121     D iTable          s             20    inz('<table border="1">')
004000071121     D fTable          s             10    inz('</table>')
004100071121     D iThead          s             10    inz('<thead>')
004200071121     D fThead          s             10    inz('</thead>')
004300071121     D iRow            s              5    inz('<tr>')
004400071121     D fRow            s              5    inz('</tr>')
004500071121     D iCol            s              5    inz('<td>')
004600071121     D fCol            s              5    inz('</td>')
004700071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004800071121     D fColBold        s              5    inz('</th>')
004900071122     D cApi            s              1    inz('''')
005000071121
005100071121
005200071121     D*------------
005300071121     D* VARIABILI D WRK
005400071121     D*------------
005500030924     D prmppt          S             50
005600030924     D prmesito        S              1
005700030924     D wrkesito        S                   like(prmesito)
005800090504     D wBufferOut      s          10000    varying inz
005900071211     D*
006000030924
006100030325
006200030924     C* MAIN/
006300071121     C*
006400071121     C* Verifico se già presenti dati nel file d output
006500071121     C                   EXSR      chkRECFILMBR
006600071121     C*
006700071121     C* Effettuo elaborazione
006800090504     C                   EXSR      cartab
006900071121     C                   EXSR      traduci
007000071121     C*
007100921023     C                   SETON                                        LR
007200991027
007300071121
007400071121
007500071121     C     chkRECFILMBR  BEGSR
007600071121     C*
007700071121     C                   READ      TIVAWWWT                               55
007800071121     C*
007900071121     C                   ENDSR
008000071121
008100071121
008200030924
008300991027     C     traduci       BEGSR
008400071121     C*
008500071121     C* Head iniziali della struttura HTML
008600071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
008700071210     C   55              WRITE     TIVAW000
008800071121     C*
008900071121     C* Inizializzazone formato e variabili x output
009000071121     C                   clear                   TIVAW000
009100071121     C                   clear                   wBufferOut
009200071121     C*
009300071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009400071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
009500071121     C                   EVAL      wBufferOut = iThead + iRow
009600071213     C                             +iColBold+ 'RAGIONE SOCIALE DEST.'+fColBold
009700071213     C                             +iColBold+ 'INDIRIZZO DEST.'+fColBold
009800071213     C                             +iColBold+ 'CAP DEST.'+fColBold
009900071213     C                             +iColBold+ 'LOCALITA DEST.'+fColBold
010000071213     C                             +iColBold+ 'PROVINCIA DEST.'+fColBold
010100090504     C                             +iColBold+ 'N. BOLLA'+fColBold
010200090504     C                             +iColBold+ 'RIFERIMENTO MITTENTE'+fColBold
010300090504     C                             +iColBold+ 'NOTE 1'+fColBold
010400090504     C                             +iColBold+ 'NOTE 2'+fColBold
010500090504     C                             +iColBold+ 'DATA SPEDIZIONE'+fColBold
010600090504     C                             +iColBold+ 'DATA CONSEGNA'+fColBold
010700090504     C                             +iColBold+ 'DATA LASCIATO AVVISO'+fColBold
010800090504     C                             +iColBold+ 'DATA GIACENZA'+fColBold
010900090504     C                             +iColBold+ 'MOTIVO GIACENZA'+fColBold
011000071121     C                   EVAL      VAWDTA = wBufferOut
011100071121     C                   WRITE     TIVAW000
011200071121     C                   ENDIF
011300991027     C*
011400080924     C                   READ      TIVGDTMP
011500060119     C*
011600080924     C                   DOW       not %eof(TIVGDTMP)
011700080924     C*
011800090506     C                   setoff                                       54
011900080924     C                   eval      fnvac00t = vgdDTA
012000071121     C*
012100090504     C                   clear                   TIVAW000
012200090504     C                   clear                   TITAS0
012300090504     C                   clear                   TIGCP000
012400071121     C                   clear                   wBufferOut
012500090504     C                   clear                   w2ADES
012600040719     C*
012700090504     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
012800090504     C                   movel     *blanks       wEsito1           1
012900090504     C                   movel     *blanks       wEsito2           1
013000090504     C*
013100090504     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
013200090504     C                                                vacAAS
013300090504     C                                               :vacLNP
013400090504     C                                               :vacNRS
013500090504     C                                               :vacNSP
013600090504     C                                               :pOutLblTyp
013700090504     C                                               :pOutAnnoBO
013800090504     C                                               :pOutLineaParBO
013900090504     C                                               :pOutSerieBO
014000090504     C                                               :pOutNumSpedBO
014100090504     C                                               :pOutDcmBO
014200090504     C                                               :pOutCcaBO
014300090504     C                                               :pOutRblBO))
014400090504     C*
014500090504     C                   if        pOutLblTyp = 'O'
014600090504     C*
014700090504     C* Chiamata metodo GetLastChild
014800090504     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
014900090504     C                                                vacAAS
015000090504     C                                               :vacLNP
015100090504     C                                               :vacNRS
015200090504     C                                               :vacNSP
015300090504     C                                               :pOutAnnoFI
015400090504     C                                               :pOutLineaParFI
015500090504     C                                               :pOutSerieFI
015600090504     C                                               :pOutNumSpedFI
015700090504     C                                               :pOutDcmFI
015800090504     C                                               :pOutCcaFI))
015900090504     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
016000090505     C                   if        pOutDcmFI > *zeros AND
016100090505     C                             pOutCcaFI = '2'
016200090506     C                   seton                                        54
016300090505     C                   endif
016400071210     C*
016500071210     C* Aggancio la bolla relativa x il completamento dei dati
016600090504     C     KEYtas30_P    chain     titas30c
016700071210     C                   if        %found(titas30c)
016800090506     C*
016900090506     C* Ottimizzazione operazioni I/O...
017000090506     C                   if        not *in54
017100071213     C*
017200071213     C* Aggancio la giacenza + recente
017300071213     C                   EVAL      gcpFRG = *zeros
017400090504     C     KEYgcp51_P    chain     tigcp51l
017500071213     C                   if        %found(tigcp51l)
017600090504     C                   z-add     1             i
017700090504     C     gcpCMC        lookup    sk2A(i)                                20
017800090504     C                   if        *IN20 = *ON
017900090504     C                   eval      w2ADES = sk2Adesc(i)
018000090504     C                   endif
018100071213     C                   endif
018200090504     C*
018300090504     C* Reperisco le note partenza 1 e 2
018400090504     C                   EVAL      ta4TRC = '8'
018500090504     C     KEYta430C     CHAIN     tita430c
018600090504     C                   IF        %found(tita430c)
018700090504     C                   EVAL      wNOTE1 = TA4NOT
018800090504     C                   ENDIF
018900090504     C*
019000090504     C                   EVAL      ta4TRC = '9'
019100090504     C     KEYta430C     CHAIN     tita430c
019200090504     C                   IF        %found(tita430c)
019300090504     C                   EVAL      wNOTE2 = TA4NOT
019400090504     C                   ENDIF
019500090506     C*
019600090506     C                   endif
019700071121     C*
019800071121     C* Compondo la stringa dati output relativa al record corrente
019900090506     C  N54              EXSR      wrifile
020000090504     C*
020100090504     C                   endif
020200090504     C*
020300090504     C                   endif
020400040719     C*
020500040719     C* In ogni caso storicizzo e cancello l'input sempre
020600080924     C                   DELETE    TIVGD000
020700040719     C*
020800040719     C* Proseguo con la lettura dell'input
020900080924     C                   READ      TIVGDTMP
021000060119     C*
021100030325     C                   ENDDO
021200071121     C*
021300071121     C* Head finali della struttura HTML
021400071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
021500071210     C***                WRITE     TIVAW000
021600991027     C*
021700030325     C                   EVAL      wrkesito = '0'
021800991027     C*
021900910830     C                   ENDSR
022000071121
022100071121
022200071121
022300071121     C     wrifile       BEGSR
022400071121     C*
022500071121     C                   EVAL      wBufferOut = iRow +
022600090504     C                                  iCol+%trim(tasRSD)+fCol+
022700090504     C                                  iCol+%trim(tasIND)+fCol+
022800090504     C                                  iCol+%trim(tasCAD)+fCol+
022900090504     C                                  iCol+%trim(tasLOD)+fCol+
023000090504     C                                  iCol+%trim(tasPRD)+fCol+
023100071213     C                                  iCol+%trim(%editc(vacRMN:'Q'))+fCol+
023200090504     C                                  iCol+%trim(vacRMA)+fCol+
023300090504     C                                  iCol+%trim(wNOTE1)+fCol+
023400090504     C                                  iCol+%trim(wNOTE2)+fCol+
023500090504     C                                  iCol+%trim(%editc(tasAAS*10000+tasMGS:
023600090504     C                                             'Q'))+fCol+
023700090504     C                                  iCol+%trim(%editc(vacDCM:'Q'))+fCol+
023800090504     C                                  iCol+%trim(%editc(vacDLA:'Q'))+fCol+
023900090504     C                                  iCol+%trim(%editc(vacDAG:'Q'))+fCol+
024000090504     C                                  iCol+%trim(w2ADES)+fCol
024100071121     C*
024200071121     C                   EVAL      VAWDTA = wBufferOut
024300071121     C                   WRITE     TIVAW000
024400071121     C*
024500071121     C                   ENDSR
024600090504
024700090504
024800090504
024900090504     C     CARTAB        BEGSR
025000090504     C*
025100090504     C* Carico i codici eventi d tipo "giacenza" e "lasciato avviso" (ovvero queli significativi
025200090504     C* ai fini del tentativo d consegna).
025300090504     C                   clear                   sk2A
025400090504     C                   clear                   sk2Adesc
025500090504     C                   z-add     *zeros        i                 3 0
025600090504     C                   eval      tblKUT = 1
025700090504     C                   eval      tblCOD = '2A'
025800090504     C     KEYtab_P      setll     tabel00f
025900090504     C     KEYtab_P      reade     tabel00f
026000090504     C                   dow       not %eof(tabel00f)
026100090504     C                   movel(p)  tblUNI        ds2a
026200090504     C                   if        §2aFTC = 'G' or
026300090504     C                             §2aFTC = 'A'
026400090504     C                   add       1             i
026500090504     C                   eval      sk2A(i)     = tblKEY
026600090504     C                   eval      sk2Adesc(i) = §2ADES
026700090504     C                   endif
026800090504     C     KEYtab_P      reade     tabel00f
026900090504     C                   enddo
027000090504     C*
027100090504     C                   ENDSR
027200071121
027300071121
027400991027
027500991027      /TITLE Operazioni iniziali.
027600991027     C     *inzsr        BEGSR
027700991027     C*
027800991027     C     *ENTRY        PLIST
027900991027     C                   parm                    prmppt
028000991027     C     wrkesito      parm      wrkesito      prmesito
028100071210     C*
028200071213     C* TITAS30C - Parziale
028300090504     C     KEYtas30_P    KLIST
028400090505     C                   KFLD                    vacAAS
028500090505     C                   KFLD                    vacLNP
028600090505     C                   KFLD                    vacNRS
028700090505     C                   KFLD                    vacNSP
028800071213     C*
028900071213     C* TIGCP51L - Parziale
029000090504     C     KEYgcp51_P    KLIST
029100090505     C                   KFLD                    vacAAS
029200090505     C                   KFLD                    vacLNP
029300090505     C                   KFLD                    vacNRS
029400090505     C                   KFLD                    vacNSP
029500071213     C                   KFLD                    gcpFRG
029600090504     C*
029700090504     C* Chiave su TITA430C - Completa
029800090504     C     KEYta430C     KLIST
029900090505     C                   KFLD                    vacAAS
030000090505     C                   KFLD                    vacLNP
030100090505     C                   KFLD                    vacNRS
030200090505     C                   KFLD                    vacNSP
030300090504     C                   KFLD                    ta4TRC
030400090504     C*
030500090504     C* Chiave su TABEL00F - parziale
030600090504     C     KEYtab_P      KLIST
030700090504     C                   KFLD                    tblKUT
030800090504     C                   KFLD                    tblCOD
030900991027     C*
031000991027     C                   ENDSR
031100071121     C*
