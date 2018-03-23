000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200111221     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600121214     FTITAS30C  IF   E           K DISK
000700121214     F                                     rename(titas000:titas0)
000800121214     F                                     rename(titas010:titas1)
000900121214     F                                     rename(titasP00:titasP)
001000030924
001100080924     D fnvac00t      e ds
001200071121
001300071121
001400071121     D*------------
001500071121     D* VARIABILI D WRK
001600071121     D*------------
001700030924     D prmppt          S             50
001800030924     D prmesito        S              1
001900030924     D wrkesito        S                   like(prmesito)
002000071122     D wBufferOut      s          10000    varying inz
002100071211     D*
002200100910
002300100910
002400100910     D*------------------
002500100910     D* LINKING A DEFINIZIONI ESTERNE
002600100910     D*------------------
002700100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002800100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002900111228     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003000111228     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
003100100910
003200030924
003300030325
003400030924     C* MAIN/
003500071121     C*
003600071121     C* Verifico se già presenti dati nel file d output
003700071121     C                   EXSR      chkRECFILMBR
003800071121     C*
003900071121     C* Effettuo elaborazione
004000071121     C                   EXSR      traduci
004100071121     C*
004200921023     C                   SETON                                        LR
004300991027
004400071121
004500071121
004600071121     C     chkRECFILMBR  BEGSR
004700071121     C*
004800071121     C                   READ      TIVAWWWT                               55
004900071121     C*
005000071121     C                   ENDSR
005100071121
005200071121
005300030924
005400991027     C     traduci       BEGSR
005500071121     C*
005600071121     C*
005700071121     C* Inizializzazone formato e variabili x output
005800071121     C                   clear                   TIVAW000
005900071121     C                   clear                   wBufferOut
006000121214     C*
006100121214     C* Inserimento testate
006200121217     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006300100908     C                   EVAL      wBufferOut =
006400121214     C                              '"'+'RIF. MITTENTE'          +'";'
006500121214     C                             +'"'+'DATA CONSEGNA'          +'";'
006600121214     C                             +'"'+'DATA RITIRO'            +'";'
006700121214     C                             +'"'+'NUMERO SPEDIZIONE BRT'  +'";'
006800100908     C*
006900071121     C                   EVAL      VAWDTA = wBufferOut
007000071121     C                   WRITE     TIVAW000
007100121217     C                   ENDIF
007200991027     C*
007300121214     C* Inizio traduzione parte dati
007400080924     C                   READ      TIVGDTMP
007500060119     C*
007600080924     C                   DOW       not %eof(TIVGDTMP)
007700080924     C*
007800080924     C                   eval      fnvac00t = vgdDTA
007900071121     C*
008000071121     C                   clear                   TIVAW000
008100071121     C                   clear                   wBufferOut
008200121214     C                   z-add     *zeros        wDRT              8 0
008300100910     C*
008400100910     C* Solo bolle originali e valide ai fini della spedizione
008500100910     C                   movel     *blanks       wEsito1           1
008600111223     C                   movel     *blanks       wEsito2           1
008700111223     C                   z-add     *zeros        wrkDCM            8 0
008800111223     C                   movel     *blanks       wrkCCA            1
008900100910     C*
009000100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
009100100910     C                                                vacAAS
009200100910     C                                               :vacLNP
009300100910     C                                               :vacNRS
009400100910     C                                               :vacNSP
009500100910     C                                               :pOutLblTyp
009600100910     C                                               :pOutAnnoBO
009700100910     C                                               :pOutLineaParBO
009800100910     C                                               :pOutSerieBO
009900100910     C                                               :pOutNumSpedBO
010000100910     C                                               :pOutDcmBO
010100100910     C                                               :pOutCcaBO
010200100910     C                                               :pOutRblBO))
010300100910     C*
010400100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
010500111223     C*
010600111223     C* Chiamata metodo GetLastChild
010700111223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
010800111223     C                                                pOutAnnoBO
010900111223     C                                               :pOutLineaParBO
011000111223     C                                               :pOutSerieBO
011100111223     C                                               :pOutNumSpedBO
011200111223     C                                               :pOutAnnoFI
011300111223     C                                               :pOutLineaParFI
011400111223     C                                               :pOutSerieFI
011500111223     C                                               :pOutNumSpedFI
011600111223     C                                               :pOutDcmFI
011700111223     C                                               :pOutCcaFI))
011800111223     C*
011900111223     C                   if        wEsito2 = '0'
012000111223     C                   eval      wrkDCM = pOutDcmFI
012100111223     C                   eval      wrkCCA = pOutCcaFI
012200111223     C                   else
012300111223     C                   eval      wrkDCM = pOutDcmBO
012400111223     C                   eval      wrkCCA = pOutCcaBO
012500111223     C                   endif
012600121214     C*
012700121214     C* Ritorno solamente le spedizioni consegnate
012800121214     C                   IF        wrkDCM > *zeros AND
012900121214     C                             (wrkCCA = *blanks OR wrkCCA = '1')
013000071210     C*
013100071210     C* Aggancio la bolla relativa x il completamento dei dati
013200071210     C     KEYtas30      chain     titas30c
013300071210     C                   if        %found(titas30c)
013400071210     C*
013500071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
013600121214     C                   EVAL      wDRT = tasDRT
013700071213     C*
013800071213     C                   endif
013900071121     C*
014000071121     C* Compondo la stringa dati output relativa al record corrente
014100071121     C                   EXSR      wrifile
014200100910     C*
014300100910     C                   endif
014400121214     C                   endif
014500040719     C*
014600040719     C* In ogni caso storicizzo e cancello l'input sempre
014700080924     C                   DELETE    TIVGD000
014800040719     C*
014900040719     C* Proseguo con la lettura dell'input
015000080924     C                   READ      TIVGDTMP
015100060119     C*
015200030325     C                   ENDDO
015300991027     C*
015400030325     C                   EVAL      wrkesito = '0'
015500991027     C*
015600910830     C                   ENDSR
015700071121
015800071121
015900071121
016000071121     C     wrifile       BEGSR
016100100908     C*
016200100908     C                   EVAL      wBufferOut =
016300121214     C                               '"'+%trim(%editc(vacRMN:'Q'))        +'";'
016400121214     C                              +'"'+%trim(
016500111228     C                              UBFMTDATE_Convert(
016600121214     C                                  %editc(wrkDCM:'4') :
016700111228     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
016800111228     C                                                                  ) +'";'
016900121214     C                              +'"'+%trim(
017000121214     C                              UBFMTDATE_Convert(
017100121214     C                                  %editc(wDRT:'4') :
017200121214     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
017300121214     C                                                                  ) +'";'
017400121214     C                              +'"'+%editc(vacAAS:'X')+'/'+
017500121214     C                                   %editc(vacLNP:'X')+'/'+
017600121214     C                                   %editc(vacNRS:'X')+'/'+
017700121214     C                                   %editc(vacNSP:'X')               +'";'
017800071121     C*
017900071121     C                   EVAL      VAWDTA = wBufferOut
018000071121     C                   WRITE     TIVAW000
018100071121     C*
018200071121     C                   ENDSR
018300071121
018400071121
018500071121
018600991027
018700991027      /TITLE Operazioni iniziali.
018800991027     C     *inzsr        BEGSR
018900991027     C*
019000991027     C     *ENTRY        PLIST
019100991027     C                   parm                    prmppt
019200991027     C     wrkesito      parm      wrkesito      prmesito
019300100910     C*
019400100910     C* CALCOLA LA DATA CORRENTE
019500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
019600100910     C                   eval      datcor = %dec(%date() : *ISO)
019700071210     C*
019800071213     C* TITAS30C - Parziale
019900071210     C     KEYtas30      KLIST
020000071210     C                   KFLD                    vacAAS
020100071210     C                   KFLD                    vacLNP
020200071210     C                   KFLD                    vacNRS
020300071210     C                   KFLD                    vacNSP
020400991027     C*
020500991027     C                   ENDSR
020600071121     C*
