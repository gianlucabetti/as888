000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400110307     FTITAS30C  IF   E           K DISK
000500130731     FTITA430C  IF   E           K DISK
000600110307     FFIRAB01L  IF   E           K DISK
000700151019     FFIAR531C  IF   E           K DISK
000800060410     FTIVGDTMP  UF   E             DISK
000900090323     Ftivawwwt  uF A E             DISK    commit
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400030325
001500110307     D wfcln10ds     e ds                  extname(wfcln10f)
001600151019     D DAR5FAT       E DS
001700110307
001800110307
001900110307
002000991027
002100030908     C                   EXSR      chkRECFILMBR
002200991027     C                   EXSR      traduci
002300921023     C                   SETON                                        LR
002400110307
002500110307
002600030908
002700030908     C     chkRECFILMBR  BEGSR
002800030908     C*
002900060410     C                   READ      TIVAWWWT                               55
003000030908     C*
003100030908     C                   ENDSR
003200110307
003300110307
003400110307
003500991027
003600991027     C     traduci       BEGSR
003700051007     C*
003800051007     C* Verifico se richiesto un particolare carattere d separatore campi
003900051007     C                   IF        %subst(prmppt:3:1) <> *blanks
004000051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
004100051007     C                   ELSE
004200051007     C                   MOVEL     ','           wSepCampi         1
004300051007     C                   ENDIF
004400051007     C*
004500051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004600051007     C                   SETON                                        60
004700051007     C                   IF        %subst(prmppt:4:1) = ','
004800051007     C                   SETOFF                                       60
004900051007     C                   ENDIF
005000030325     C*
005100030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005200030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005300140326     C                   EVAL      VAWDTA  = '"CUSTOMER CODE       "'+wSepCampi+
005400140326     C                                       '"CUSTOMER NAME       "'+wSepCampi+
005500110401     C                                       '"RECIPIENT NAME      "'+wSepCampi+
005600110307     C                                       '"RECIPIENT ADDRESS   "'+wSepCampi+
005700110307     C                                       '"RECIPIENT REGION    "'+wSepCampi+
005800110307     C                                       '"RECIPIENT PROVINCE  "'+wSepCampi+
005900110307     C                                       '"RECIPIENT ZIP CODE  "'+wSepCampi+
006000110307     C                                       '"SHIPMENT NUMBER     "'+wSepCampi+
006100110307     C                                       '"DELIVERY NOTE       "'+wSepCampi+
006200110310     C                                       '"NBR. OF PARCELS     "'+wSepCampi+
006300110310     C                                       '"INVOICED WEIGHT (KG)"'+wSepCampi+
006400110310     C                                       '"INVOICED VOLUME (M3)"'+wSepCampi+
006500110307     C                                       '"S=EVENTS FAILURE    "'+wSepCampi+
006600110307     C                                       '"ARRIVAL DATE        "'+wSepCampi+
006700110307     C                                       '"SHIPMENT DATE       "'+wSepCampi+
006800110307     C                                       '"DELIVERY DATE       "'+wSepCampi+
006900110307     C                                       '"SLA                 "'+wSepCampi+
007000110418     C                                       '"# <=    0           "'+wSepCampi+
007100110309     C                                       '"# >     0           "'+wSepCampi+
007200110404     C                                       '"CARRIER FAILURE Y/N "'+wSepCampi+
007300110309     C                                       '"SHIPMENT COST       "'+wSepCampi
007400030325     C*
007500060410     C                   WRITE     TIVAW000
007600030325     C                   ENDIF
007700991027     C*
007800060410     C                   READ      TIVGDTMP
007900060410     C                   DOW       not %eof(TIVGDTMP)
008000060411     C*
008100060411     C* Valorizzo il buffer dal campo flat di inout
008200110307     C                   EVAL      wfcln10ds = vgdDTA
008300110307     C*
008400110307     C* Innanzitutto chaino la bolla direttamente su TITAS
008500110307     C     KEYtas30P     chain     titas30c
008600110307     C                   if        %found(titas30c)
008700110401     C*
008800110401     C* Inizializzo variabili d wrk
008900110401     C                   Z-ADD     1             wCount            4 0
009000110307     C*
009100110307     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
009200110307     C* anche delle bolle accorpate
009300110307     C*
009400110307     C     KEYrab01_C    SETLL     firab01l
009500110307     C                   IF        %equal(firab01l)
009600110307     C     KEYrab01_C    READE     firab01l
009700110307     C                   DOW       not %eof(firab01l)
009800130731     C*
009900130731     C* Eseguo routine valorizzazione dati di output (da file FIRAB)
010000130731     C                   exsr      wriOUT
010100110331     C*
010200110307     C     KEYrab01_C    READE     firab01l
010300110307     C                   ENDDO
010400110307     C*
010500130731     C                   ELSE
010600130731     C*
010700130731     C* Eseguo routine valorizzazione dati di output (singolo)
010800130731     C*
010900130731     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
011000130731     C                   eval      ta4TRC = 'A'
011100130731     C     KEYta430      chain     TITA430C
011200130731     C                   if        %found(TITA430C)
011300130731     C                   eval      rabRMAR = ta4NOT
011400130731     C                   else
011500130731     C                   eval      rabRMAR = *blanks
011600130731     C                   endif
011700130731     C*
011800130731     C                   eval      rabRMN  = tasRMN
011900130731     C                   exsr      wriOUT
012000130731     C*
012100130731     C                   ENDIF
012200110307     C                   endif
012300110307     C*
012400060410     C                   DELETE    TIVGD000
012500991027     C*
012600060410     C                   READ      TIVGDTMP
012700030325     C                   ENDDO
012800991027     C*
012900030325     C                   EVAL      wrkesito = '0'
013000991027     C*
013100910830     C                   ENDSR
013200130731
013300130731
013400130731
013500130731     C     wriOUT        BEGSR
013600130731     C*
013700130731     C* Inizializzo variabili d wrk
013800130731     C                   Z-ADD     *zeros        wDSP              8 0
013900130731     C                   MOVEL     *blanks       wIMV             14
014000130731     C                   MOVEL     *blanks       wPKF              8
014100130731     C                   MOVEL     *blanks       wVLF              8
014200130731     C                   MOVEL     *blanks       wCCMRAG          35
014300151019     C*
014400151019     C* Eseguo eventuali "forzature"
014500151019     C                   EXSR      RTVAR5FAT
014600130731     C*
014700130731     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014800130731     C  N60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 ,   '))
014900130731     C   60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 .   '))
015000130731     C  N60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 , '))
015100130731     C   60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 . '))
015200130731     C  N60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 ,   '))
015300130731     C   60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 .   '))
015400130731     C*
015500130731     C* Compongo la data spedizione come unico valore
015600130731     C                   EVAL      wDSP = tasAAS * 10000 + tasMGS
015700130731     C*
015800130731     C* Verifico codice cliente mittente e imposto "ragione sociale"
015900130731     C                   SELECT
016000130731     C                   WHEN      WCLNCCM = 1161872
016100130731     C                   EVAL      wCCMRAG = 'GE APP.S AND LIGHTING'
016200130731     C                   WHEN      WCLNCCM = 1161985
016300130731     C                   EVAL      wCCMRAG = 'GE POWER CONTROLS'
016400140326     C                   WHEN      WCLNCCM = 1162115
016500140326     C                   EVAL      wCCMRAG = 'GE INDUSTRIAL BELGIUM BVBA'
016600140326     C                   WHEN      WCLNCCM = 1162164
016700140326     C                   EVAL      wCCMRAG = 'GE POWER CONTROLS ITALIA SRL'
016800130731     C                   ENDSL
016900130731     C*
017000130731     C                   Z-ADD     *zeros        wTotSpedOK        7 0
017100130731     C                   EVAL      wTotSpedOK = WCLNANT +
017200130731     C                                          WCLNM12 +
017300130731     C                                          WCLNOKS
017400130731     C*
017500130731     C                   IF        wCount = 1
017600130731     C                   EVAL      VAWDTA =
017700130731     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
017800130731     C                             %trim(       wCCMRAG             )+wSepCampi+
017900130731     C                             %trim(       tasRSD              )+wSepCampi+
018000130731     C                             %trim(       tasIND              )+wSepCampi+
018100130731     C                             %trim(       WCLNREG             )+wSepCampi+
018200130731     C                             %trim(       WCLNPRD             )+wSepCampi+
018300130731     C                             %trim(       WCLNCAD             )+wSepCampi+
018400130731     C                             %trim(%editc(RABRMN         :'Q'))+wSepCampi+
018500130731     C                             %trim(       RABRMAR             )+wSepCampi+
018600130731     C                             %trim(%editc(tasNCL         :'Q'))+wSepCampi+
018700130731     C                             %trim(       wPKF                )+wSepCampi+
018800130731     C                             %trim(       wVLF                )+wSepCampi+
018900130731     C                             %trim(       WCLNANOM            )+wSepCampi+
019000130731     C                             %trim(%editc(tasDRT         :'X'))+wSepCampi+
019100130731     C                             %trim(%editc(wDSP           :'X'))+wSepCampi+
019200130731     C                             %trim(%editc(tasDCM         :'X'))+wSepCampi+
019300130731     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
019400130731     C                             %trim(%editc(wTotSpedOK     :'Q'))+wSepCampi+
019500130731     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
019600130731     C                             ' '                               +wSepCampi+
019700130731     C                             %trim(       wIMV                )+wSepCampi
019800130731     C                   ELSE
019900130731     C                   EVAL      VAWDTA =
020000130731     C                                                                wSepCampi+
020100130731     C                                                                wSepCampi+
020200130731     C                                                                wSepCampi+
020300130731     C                                                                wSepCampi+
020400130731     C                                                                wSepCampi+
020500130731     C                                                                wSepCampi+
020600130731     C                                                                wSepCampi+
020700130731     C                                                                wSepCampi+
020800130731     C                             %trim(       RABRMAR             )+wSepCampi+
020900130731     C                                                                wSepCampi+
021000130731     C                                                                wSepCampi+
021100130731     C                                                                wSepCampi+
021200130731     C                                                                wSepCampi+
021300130731     C                                                                wSepCampi+
021400130731     C                                                                wSepCampi+
021500130731     C                                                                wSepCampi+
021600130731     C                                                                wSepCampi+
021700130731     C                                                                wSepCampi+
021800130731     C                                                                wSepCampi+
021900130731     C                                                                wSepCampi+
022000130731     C                                                                wSepCampi
022100130731     C                   ENDIF
022200130731     C*
022300130731     C                   WRITE     TIVAW000
022400130731     C*
022500130731     C                   EVAL      wCount = wCount + 1
022600130731     C*
022700130731     C                   ENDSR
022800151019
022900151019
023000151019
023100151019     C     RTVAR5FAT     BEGSR
023200151019     C*
023300151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
023400151019     C                   SETOFF                                       7071
023500151019     C                   EVAL      ar5TRD = 'FAT'
023600151019     C                   CLEAR                   DAR5FAT
023700151019     C     KEYar531_P    CHAIN     FIAR531C
023800151019     C                   IF        %found(FIAR531C)
023900151019     C                   MOVEL     AR5UNI        DAR5FAT
024000151019     C                   IF        你R5PKTAS > *zeros
024100151019     C                   SETON                                        70
024200151019     C                   EVAL      tasPKF = 你R5PKTAS
024300151019     C                   ENDIF
024400151019     C                   IF        你R5VLTAS > *zeros
024500151019     C                   SETON                                        71
024600151019     C***                EVAL      tasVLF = 你R5VLTAS
024700151019     C                   ENDIF
024800151019     C                   ENDIF
024900151019     C*
025000151019     C                   ENDSR
025100110307
025200110307
025300110307
025400991027      /TITLE Operazioni iniziali.
025500991027     C     *inzsr        BEGSR
025600991027     C*
025700991027     C     *ENTRY        PLIST
025800991027     C                   parm                    prmppt
025900991027     C     wrkesito      parm      wrkesito      prmesito
026000110307     C*
026100110307     C* Definizione chiavi
026200110307     C*
026300110307     C* Chiave su TITAS30C - Parziale
026400110307     C     KEYtas30P     KLIST
026500110307     C                   KFLD                    wclnAAS
026600110307     C                   KFLD                    wclnLNP
026700110307     C                   KFLD                    wclnNRS
026800110307     C                   KFLD                    wclnNSP
026900110307     C*
027000110307     C* Chiave su FIRAB01L - Completa
027100110307     C     KEYrab01_C    KLIST
027200110307     C                   KFLD                    wclnAAS
027300110307     C                   KFLD                    wclnLNP
027400110307     C                   KFLD                    wclnNRS
027500110307     C                   KFLD                    wclnNSP
027600130731     C*
027700130731     C* TITA430C - Completa
027800130731     C     KEYta430      KLIST
027900130731     C                   KFLD                    tasAAS
028000130731     C                   KFLD                    tasLNP
028100130731     C                   KFLD                    tasNRS
028200130731     C                   KFLD                    tasNSP
028300130731     C                   KFLD                    ta4TRC
028400151019     C*
028500151019     C* FIAR531C - Parziale
028600151019     C     KEYar531_P    KLIST
028700151019     C                   KFLD                    tasAAS
028800151019     C                   KFLD                    tasLNP
028900151019     C                   KFLD                    tasNRS
029000151019     C                   KFLD                    tasNSP
029100151019     C                   KFLD                    ar5TRD
029200991027     C*
029300991027     C                   ENDSR
