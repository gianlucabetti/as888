000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400110307     FTITAS30C  IF   E           K DISK
000500130731     FTITA430C  IF   E           K DISK
000600110307     FFIRAB01L  IF   E           K DISK
000700060410     FTIVGDTMP  UF   E             DISK
000800090323     Ftivawwwt  uF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300030325
001400110307     D wfcln10ds     e ds                  extname(wfcln10f)
001500110307
001600110307
001700110307
001800991027
001900030908     C                   EXSR      chkRECFILMBR
002000991027     C                   EXSR      traduci
002100921023     C                   SETON                                        LR
002200110307
002300110307
002400030908
002500030908     C     chkRECFILMBR  BEGSR
002600030908     C*
002700060410     C                   READ      TIVAWWWT                               55
002800030908     C*
002900030908     C                   ENDSR
003000110307
003100110307
003200110307
003300991027
003400991027     C     traduci       BEGSR
003500051007     C*
003600051007     C* Verifico se richiesto un particolare carattere d separatore campi
003700051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003800051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003900051007     C                   ELSE
004000051007     C                   MOVEL     ','           wSepCampi         1
004100051007     C                   ENDIF
004200051007     C*
004300051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004400051007     C                   SETON                                        60
004500051007     C                   IF        %subst(prmppt:4:1) = ','
004600051007     C                   SETOFF                                       60
004700051007     C                   ENDIF
004800030325     C*
004900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005100110401     C                   EVAL      VAWDTA  = '"SENDER CODE         "'+wSepCampi+
005200110401     C                                       '"SENDER NAME         "'+wSepCampi+
005300110401     C                                       '"RECIPIENT NAME      "'+wSepCampi+
005400110307     C                                       '"RECIPIENT ADDRESS   "'+wSepCampi+
005500110307     C                                       '"RECIPIENT REGION    "'+wSepCampi+
005600110307     C                                       '"RECIPIENT PROVINCE  "'+wSepCampi+
005700110307     C                                       '"RECIPIENT ZIP CODE  "'+wSepCampi+
005800110307     C                                       '"SHIPMENT NUMBER     "'+wSepCampi+
005900110307     C                                       '"DELIVERY NOTE       "'+wSepCampi+
006000110310     C                                       '"NBR. OF PARCELS     "'+wSepCampi+
006100110310     C                                       '"INVOICED WEIGHT (KG)"'+wSepCampi+
006200110310     C                                       '"INVOICED VOLUME (M3)"'+wSepCampi+
006300110307     C                                       '"S=EVENTS FAILURE    "'+wSepCampi+
006400110307     C                                       '"ARRIVAL DATE        "'+wSepCampi+
006500110307     C                                       '"SHIPMENT DATE       "'+wSepCampi+
006600110307     C                                       '"DELIVERY DATE       "'+wSepCampi+
006700110307     C                                       '"SLA                 "'+wSepCampi+
006800110418     C                                       '"# <=    0           "'+wSepCampi+
006900110309     C                                       '"# >     0           "'+wSepCampi+
007000110404     C                                       '"CARRIER FAILURE Y/N "'+wSepCampi+
007100110309     C                                       '"SHIPMENT COST       "'+wSepCampi
007200030325     C*
007300060410     C                   WRITE     TIVAW000
007400030325     C                   ENDIF
007500991027     C*
007600060410     C                   READ      TIVGDTMP
007700060410     C                   DOW       not %eof(TIVGDTMP)
007800060411     C*
007900060411     C* Valorizzo il buffer dal campo flat di inout
008000110307     C                   EVAL      wfcln10ds = vgdDTA
008100110307     C*
008200110307     C* Innanzitutto chaino la bolla direttamente su TITAS
008300110307     C     KEYtas30P     chain     titas30c
008400110307     C                   if        %found(titas30c)
008500110401     C*
008600110401     C* Inizializzo variabili d wrk
008700110401     C                   Z-ADD     1             wCount            4 0
008800110307     C*
008900110307     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
009000110307     C* anche delle bolle accorpate
009100110307     C*
009200110307     C     KEYrab01_C    SETLL     firab01l
009300110307     C                   IF        %equal(firab01l)
009400110307     C     KEYrab01_C    READE     firab01l
009500110307     C                   DOW       not %eof(firab01l)
009600130731     C*
009700130731     C* Eseguo routine valorizzazione dati di output (da file FIRAB)
009800130731     C                   exsr      wriOUT
009900110331     C*
010000110307     C     KEYrab01_C    READE     firab01l
010100110307     C                   ENDDO
010200110307     C*
010300130731     C                   ELSE
010400130731     C*
010500130731     C* Eseguo routine valorizzazione dati di output (singolo)
010600130731     C*
010700130731     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
010800130731     C                   eval      ta4TRC = 'A'
010900130731     C     KEYta430      chain     TITA430C
011000130731     C                   if        %found(TITA430C)
011100130731     C                   eval      rabRMAR = ta4NOT
011200130731     C                   else
011300130731     C                   eval      rabRMAR = *blanks
011400130731     C                   endif
011500130731     C*
011600130731     C                   eval      rabRMN  = tasRMN
011700130731     C                   exsr      wriOUT
011800130731     C*
011900130731     C                   ENDIF
012000110307     C                   endif
012100110307     C*
012200060410     C                   DELETE    TIVGD000
012300991027     C*
012400060410     C                   READ      TIVGDTMP
012500030325     C                   ENDDO
012600991027     C*
012700030325     C                   EVAL      wrkesito = '0'
012800991027     C*
012900910830     C                   ENDSR
013000130731
013100130731
013200130731
013300130731     C     wriOUT        BEGSR
013400130731     C*
013500130731     C* Inizializzo variabili d wrk
013600130731     C                   Z-ADD     *zeros        wDSP              8 0
013700130731     C                   MOVEL     *blanks       wIMV             14
013800130731     C                   MOVEL     *blanks       wPKF              8
013900130731     C                   MOVEL     *blanks       wVLF              8
014000130731     C                   MOVEL     *blanks       wCCMRAG          35
014100130731     C*
014200130731     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014300130731     C  N60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 ,   '))
014400130731     C   60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 .   '))
014500130731     C  N60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 , '))
014600130731     C   60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 . '))
014700130731     C  N60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 ,   '))
014800130731     C   60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 .   '))
014900130731     C*
015000130731     C* Compongo la data spedizione come unico valore
015100130731     C                   EVAL      wDSP = tasAAS * 10000 + tasMGS
015200130731     C*
015300130731     C* Verifico codice cliente mittente e imposto "ragione sociale"
015400130731     C                   SELECT
015500130731     C                   WHEN      WCLNCCM = 1161872
015600130731     C                   EVAL      wCCMRAG = 'GE APP.S AND LIGHTING'
015700130731     C                   WHEN      WCLNCCM = 1161985
015800130731     C                   EVAL      wCCMRAG = 'GE POWER CONTROLS'
015900130731     C                   ENDSL
016000130731     C*
016100130731     C                   Z-ADD     *zeros        wTotSpedOK        7 0
016200130731     C                   EVAL      wTotSpedOK = WCLNANT +
016300130731     C                                          WCLNM12 +
016400130731     C                                          WCLNOKS
016500130731     C*
016600130731     C                   IF        wCount = 1
016700130731     C                   EVAL      VAWDTA =
016800130731     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
016900130731     C                             %trim(       wCCMRAG             )+wSepCampi+
017000130731     C                             %trim(       tasRSD              )+wSepCampi+
017100130731     C                             %trim(       tasIND              )+wSepCampi+
017200130731     C                             %trim(       WCLNREG             )+wSepCampi+
017300130731     C                             %trim(       WCLNPRD             )+wSepCampi+
017400130731     C                             %trim(       WCLNCAD             )+wSepCampi+
017500130731     C                             %trim(%editc(RABRMN         :'Q'))+wSepCampi+
017600130731     C                             %trim(       RABRMAR             )+wSepCampi+
017700130731     C                             %trim(%editc(tasNCL         :'Q'))+wSepCampi+
017800130731     C                             %trim(       wPKF                )+wSepCampi+
017900130731     C                             %trim(       wVLF                )+wSepCampi+
018000130731     C                             %trim(       WCLNANOM            )+wSepCampi+
018100130731     C                             %trim(%editc(tasDRT         :'X'))+wSepCampi+
018200130731     C                             %trim(%editc(wDSP           :'X'))+wSepCampi+
018300130731     C                             %trim(%editc(tasDCM         :'X'))+wSepCampi+
018400130731     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
018500130731     C                             %trim(%editc(wTotSpedOK     :'Q'))+wSepCampi+
018600130731     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
018700130731     C                             ' '                               +wSepCampi+
018800130731     C                             %trim(       wIMV                )+wSepCampi
018900130731     C                   ELSE
019000130731     C                   EVAL      VAWDTA =
019100130731     C                                                                wSepCampi+
019200130731     C                                                                wSepCampi+
019300130731     C                                                                wSepCampi+
019400130731     C                                                                wSepCampi+
019500130731     C                                                                wSepCampi+
019600130731     C                                                                wSepCampi+
019700130731     C                                                                wSepCampi+
019800130731     C                                                                wSepCampi+
019900130731     C                             %trim(       RABRMAR             )+wSepCampi+
020000130731     C                                                                wSepCampi+
020100130731     C                                                                wSepCampi+
020200130731     C                                                                wSepCampi+
020300130731     C                                                                wSepCampi+
020400130731     C                                                                wSepCampi+
020500130731     C                                                                wSepCampi+
020600130731     C                                                                wSepCampi+
020700130731     C                                                                wSepCampi+
020800130731     C                                                                wSepCampi+
020900130731     C                                                                wSepCampi+
021000130731     C                                                                wSepCampi+
021100130731     C                                                                wSepCampi
021200130731     C                   ENDIF
021300130731     C*
021400130731     C                   WRITE     TIVAW000
021500130731     C*
021600130731     C                   EVAL      wCount = wCount + 1
021700130731     C*
021800130731     C                   ENDSR
021900130731
022000130731
022100130731
022200130731
022300110307
022400110307
022500110307
022600991027      /TITLE Operazioni iniziali.
022700991027     C     *inzsr        BEGSR
022800991027     C*
022900991027     C     *ENTRY        PLIST
023000991027     C                   parm                    prmppt
023100991027     C     wrkesito      parm      wrkesito      prmesito
023200110307     C*
023300110307     C* Definizione chiavi
023400110307     C*
023500110307     C* Chiave su TITAS30C - Parziale
023600110307     C     KEYtas30P     KLIST
023700110307     C                   KFLD                    wclnAAS
023800110307     C                   KFLD                    wclnLNP
023900110307     C                   KFLD                    wclnNRS
024000110307     C                   KFLD                    wclnNSP
024100110307     C*
024200110307     C* Chiave su FIRAB01L - Completa
024300110307     C     KEYrab01_C    KLIST
024400110307     C                   KFLD                    wclnAAS
024500110307     C                   KFLD                    wclnLNP
024600110307     C                   KFLD                    wclnNRS
024700110307     C                   KFLD                    wclnNSP
024800130731     C*
024900130731     C* TITA430C - Completa
025000130731     C     KEYta430      KLIST
025100130731     C                   KFLD                    tasAAS
025200130731     C                   KFLD                    tasLNP
025300130731     C                   KFLD                    tasNRS
025400130731     C                   KFLD                    tasNSP
025500130731     C                   KFLD                    ta4TRC
025600991027     C*
025700991027     C                   ENDSR
