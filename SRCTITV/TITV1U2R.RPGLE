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
002400101122     D* COSTANTI
002500071121     D*------------
002600101201     D sCol            s              1    inz(';') varying
002700101201     D iCol            s              1    inz('"') varying
002800101201     D fCol            s              1    inz('"') varying
002900071121
003000071121
003100071121     D*------------
003200071121     D* VARIABILI D WRK
003300071121     D*------------
003400030924     D prmppt          S             50
003500030924     D prmesito        S              1
003600030924     D wrkesito        S                   like(prmesito)
003700090504     D wBufferOut      s          10000    varying inz
003800091228     D wDataCaus       s              8  0 inz
003900101123     D wOraCaus        s              4  0 inz
004000091228     D wCausaleCli     s             10    inz
004100071211     D*
004200030924
004300030325
004400030924     C* MAIN/
004500071121     C*
004600071121     C* Verifico se già presenti dati nel file d output
004700071121     C                   EXSR      chkRECFILMBR
004800071121     C*
004900071121     C* Effettuo elaborazione
005000071121     C                   EXSR      traduci
005100071121     C*
005200921023     C                   SETON                                        LR
005300991027
005400071121
005500071121
005600071121     C     chkRECFILMBR  BEGSR
005700071121     C*
005800071121     C                   READ      TIVAWWWT                               55
005900071121     C*
006000071121     C                   ENDSR
006100071121
006200071121
006300030924
006400991027     C     traduci       BEGSR
006500071121     C*
006600071121     C* Inizializzazone formato e variabili x output
006700071121     C                   clear                   TIVAW000
006800071121     C                   clear                   wBufferOut
006900101201     C*
007000101201     C* Determino i caratteri separatori/delimitatori/etc...  dai parametri
007100101201     C                   EVAL      iCol = %trim(%subst(prmppt:2:1))
007200101201     C                   EVAL      fCol = %trim(%subst(prmppt:2:1))
007300101201     C                   EVAL      sCol = %trim(%subst(prmppt:3:1))
007400071121     C*
007500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007700101122     C                   EVAL      wBufferOut =
007800101122     C                              iCol+     'N.BOLLA'              +fCol+sCol+
007900101122     C                              iCol+     'N.BOLLA'              +fCol+sCol+
008000101122     C                              iCol+     'ANNO'                 +fCol+sCol+
008100101122     C                              iCol+     'MESE/GIORNO'          +fCol+sCol+
008200101122     C                              iCol+     'RAGIONE SOCIALE DEST.'+fCol+sCol+
008300101122     C                              iCol+     'INDIRIZZO DEST.'      +fCol+sCol+
008400101122     C                              iCol+     'CAP DEST.'            +fCol+sCol+
008500101122     C                              iCol+     'LOCALITA DEST.'       +fCol+sCol+
008600101122     C                              iCol+     'PROVINCIA DEST.'      +fCol+sCol+
008700101122     C                              iCol+     'N.COLLI'              +fCol+sCol+
008800101122     C                              iCol+     'PESO'                 +fCol+sCol+
008900101122     C                              iCol+     'CODICE CLIENTE'       +fCol+sCol+
009000101122     C                              iCol+     'DATA CONSEGNA'        +fCol+sCol+
009100101123     C                              iCol+     'ORA CONSEGNA'         +fCol+sCol+
009200101122     C                              iCol+     'CAUSALE'              +fCol+sCol
009300071121     C                   EVAL      VAWDTA = wBufferOut
009400071121     C                   WRITE     TIVAW000
009500071121     C                   ENDIF
009600991027     C*
009700080924     C                   READ      TIVGDTMP
009800060119     C*
009900080924     C                   DOW       not %eof(TIVGDTMP)
010000080924     C*
010100090506     C                   setoff                                       54
010200080924     C                   eval      fnvac00t = vgdDTA
010300071121     C*
010400090504     C                   clear                   TIVAW000
010500090504     C                   clear                   TITAS0
010600071121     C                   clear                   wBufferOut
010700091228     C                   clear                   wDataCaus
010800101123     C                   clear                   wOraCaus
010900091228     C                   clear                   wCausaleCli
011000040719     C*
011100090504     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
011200090504     C                   movel     *blanks       wEsito1           1
011300090504     C                   movel     *blanks       wEsito2           1
011400100107     C                   movel     *blanks       wEsito3           1
011500090504     C*
011600090504     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011700090504     C                                                vacAAS
011800090504     C                                               :vacLNP
011900090504     C                                               :vacNRS
012000090504     C                                               :vacNSP
012100090504     C                                               :pOutLblTyp
012200090504     C                                               :pOutAnnoBO
012300090504     C                                               :pOutLineaParBO
012400090504     C                                               :pOutSerieBO
012500090504     C                                               :pOutNumSpedBO
012600090504     C                                               :pOutDcmBO
012700090504     C                                               :pOutCcaBO
012800090504     C                                               :pOutRblBO))
012900090504     C*
013000090504     C* Chiamata metodo GetLastChild
013100090504     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
013200090504     C                                                vacAAS
013300090504     C                                               :vacLNP
013400090504     C                                               :vacNRS
013500090504     C                                               :vacNSP
013600090504     C                                               :pOutAnnoFI
013700090504     C                                               :pOutLineaParFI
013800090504     C                                               :pOutSerieFI
013900090504     C                                               :pOutNumSpedFI
014000090504     C                                               :pOutDcmFI
014100090504     C                                               :pOutCcaFI))
014200100107     C*
014300100107     C* Chiamata metodo GetFirstChild
014400100107     C                   eval      wEsito3 = %char(UBLBLSPE_GetFirstChild(
014500100107     C                                                vacAAS
014600100107     C                                               :vacLNP
014700100107     C                                               :vacNRS
014800100107     C                                               :vacNSP
014900100107     C                                               :pOutAnnoFFI
015000100107     C                                               :pOutLineaPaFFI
015100100107     C                                               :pOutSerieFFI
015200100107     C                                               :pOutNumSpedFFI
015300100107     C                                               :pOutDcmFFI
015400100107     C                                               :pOutCcaFFI))
015500100107     C* ...dirottamento
015600100107     C                   if        pOutLblTyp = 'O'    AND
015700100107     C                             vacDCM     = *zeros AND
015800100107     C                             vacCCA     = '1'
015900100107     C* Aggancio la bolla "figlia" relativa x il completamento dei dati
016000100107     C     KEYtas30_PF   chain     titas30c
016100100107     C                   if        %found(titas30c)
016200100107     C                   eval      wDataCaus   = tasAAS*10000+tasMGS
016300100107     C                   eval      wCausaleCli = 'D'
016400100107     C                   seton                                        54
016500100107     C                   endif
016600100107     C*
016700100107     C                   else
016800100107     C*
016900100107     C* Aggancio la bolla relativa x il completamento dei dati
017000100107     C     KEYtas30_P    chain     titas30c
017100100107     C                   if        %found(titas30c)
017200100107     C*
017300100107     C                   select
017400091228     C*
017500091228     C* ...lasciato avviso
017600091228     C                   when      vacDLA >  vacDAG AND
017700091228     C                             vacDLA >  vacDCM
017800091228     C*
017900091228     C* ...giacenza
018000091228     C                   when      vacDAG >= vacDLA AND
018100091228     C                             vacDAG >= vacDCM
018200091228     C                   eval      wDataCaus   = vacDAG
018300091228     C                   eval      wCausaleCli = 'G'
018400091228     C                   seton                                        54
018500091228     C*
018600091228     C* ..."others"
018700150219     C                   when      vacDCM >= vacDLA AND
018800150219     C                             vacDCM >  vacDAG
018900091228     C                   eval      wDataCaus   = vacDCM
019000101123     C                   eval      wOraCaus    = vacHMC
019100091228     C                   select
019200091228     C* ...messe in consegna
019300091228     C                   when      vacCCA = 'C'
019400091228     C                   if        pOutCcaBO = '2' OR
019500091228     C                             pOutCcaFI = '2'
019600091228     C                   eval      wCausaleCli = 'RM'
019700091228     C                   seton                                        54
019800091228     C                   else
019900091228     C                   eval      wCausaleCli = 'M'
020000091228     C                   seton                                        54
020100091228     C                   endif
020200091228     C* ...chiusura spedizione con consegna
020300091228     C                   when      vacCCA = *blanks OR
020400111108     C                             vacCCA = '1'     OR
020500100105     C                             vacCCA = '2'     OR
020600100105     C                             vacCCA = '6'
020700111108     C                   if        pOutCcaBO = '2' OR
020800100105     C                             pOutCcaFI = '2' OR
020900100105     C                             pOutCcaBO = '6' OR
021000100105     C                             pOutCcaFI = '6'
021100091228     C                   eval      wCausaleCli = 'R'
021200091228     C                   seton                                        54
021300091228     C                   else
021400091228     C                   eval      wCausaleCli = 'C'
021500091228     C                   seton                                        54
021600091228     C                   endif
021700100218     C* ...chiusura spedizione con consegna
021800100218     C                   when      vacCCA = '5'
021900100218     C                   eval      wCausaleCli = '5'
022000100218     C                   seton                                        54
022100100105     C* ...apertura pratica anomalia
022200100105     C                   when      vacCCA = 'A'
022300100105     C                   eval      wCausaleCli = 'A'
022400100105     C                   seton                                        54
022500100105     C* ...merce distrutta
022600100105     C                   when      vacCCA = 'S'
022700100105     C                   eval      wCausaleCli = 'S'
022800100105     C                   seton                                        54
022900091228     C*
023000091228     C                   endsl
023100091228     C                   endsl
023200100107     C*
023300100107     C                   endif
023400100107     C*
023500100107     C                   endif
023600071121     C*
023700071121     C* Compondo la stringa dati output relativa al record corrente
023800091228     C   54              EXSR      wrifile
023900040719     C*
024000040719     C* In ogni caso storicizzo e cancello l'input sempre
024100080924     C                   DELETE    TIVGD000
024200040719     C*
024300040719     C* Proseguo con la lettura dell'input
024400080924     C                   READ      TIVGDTMP
024500060119     C*
024600030325     C                   ENDDO
024700991027     C*
024800030325     C                   EVAL      wrkesito = '0'
024900991027     C*
025000910830     C                   ENDSR
025100071121
025200071121
025300071121
025400071121     C     wrifile       BEGSR
025500071121     C*
025600101122     C                   EVAL      wBufferOut =
025700101122     C                             iCol+%trim(%char(vacRMN))        +fCol+sCol+
025800101122     C                             iCol+%trim(vacRMA)               +fCol+sCol+
025900101122     C                             iCol+%trim(%char(vacAAS))        +fCol+sCol+
026000101122     C                             iCol+%trim(%char(vacMGS))        +fCol+sCol+
026100101122     C                             iCol+%trim(tasRSD)               +fCol+sCol+
026200101122     C                             iCol+%trim(tasIND)               +fCol+sCol+
026300101122     C                             iCol+%trim(tasCAD)               +fCol+sCol+
026400101122     C                             iCol+%trim(tasLOD)               +fCol+sCol+
026500101122     C                             iCol+%trim(tasPRD)               +fCol+sCol+
026600101122     C                             iCol+%trim(%char(vacNCL))        +fCol+sCol+
026700101122     C                             iCol+%trim(%char(vacPKB))        +fCol+sCol+
026800101122     C                             iCol+%trim(%char(vacCCM))        +fCol+sCol+
026900101122     C                             iCol+%trim(%editc(wDataCaus:'Q'))+fCol+sCol+
027000101123     C                             iCol+%trim(%editc(wOraCaus:'X')) +fCol+sCol+
027100101122     C                             iCol+%trim(wCausaleCli)          +fCol+sCol
027200071121     C*
027300071121     C                   EVAL      VAWDTA = wBufferOut
027400071121     C                   WRITE     TIVAW000
027500071121     C*
027600071121     C                   ENDSR
027700071121
027800071121
027900991027
028000991027      /TITLE Operazioni iniziali.
028100991027     C     *inzsr        BEGSR
028200991027     C*
028300991027     C     *ENTRY        PLIST
028400991027     C                   parm                    prmppt
028500991027     C     wrkesito      parm      wrkesito      prmesito
028600071210     C*
028700071213     C* TITAS30C - Parziale
028800090504     C     KEYtas30_P    KLIST
028900090505     C                   KFLD                    vacAAS
029000090505     C                   KFLD                    vacLNP
029100090505     C                   KFLD                    vacNRS
029200090505     C                   KFLD                    vacNSP
029300100107     C*
029400100107     C* TITAS30C - Parziale
029500100107     C     KEYtas30_PF   KLIST
029600100107     C                   KFLD                    pOutAnnoFFI
029700100107     C                   KFLD                    pOutLineaPaFFI
029800100107     C                   KFLD                    pOutSerieFFI
029900100107     C                   KFLD                    pOutNumSpedFFI
030000991027     C*
030100991027     C                   ENDSR
030200071121     C*
