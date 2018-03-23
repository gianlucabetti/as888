000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400110307     FTITAS30C  IF   E           K DISK
000500151019     FFIAR531C  IF   E           K DISK
000600060410     FTIVGDTMP  UF   E             DISK
000700090323     Ftivawwwt  uF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300110307     D wfcln10ds     e ds                  extname(wfcln10f)
001400151019     D DAR5FAT       E DS
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
008500110309     C*
008600110309     C* Inizializzo variabili d wrk
008700110307     C                   Z-ADD     *zeros        wDSP              8 0
008800110309     C                   MOVEL     *blanks       wIMV             14
008900110310     C                   MOVEL     *blanks       wPKF              8
009000110310     C                   MOVEL     *blanks       wVLF              8
009100110401     C                   MOVEL     *blanks       wCCMRAG          35
009200151019     C*
009300151019     C* Eseguo eventuali "forzature"
009400151019     C                   EXSR      RTVAR5FAT
009500110309     C*
009600110309     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
009700110309     C  N60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 ,   '))
009800110309     C   60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 .   '))
009900110310     C  N60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 , '))
010000110310     C   60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 . '))
010100110310     C  N60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 ,   '))
010200110310     C   60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 .   '))
010300110309     C*
010400110309     C* Compongo la data spedizione come unico valore
010500110309     C                   EVAL      wDSP = tasAAS * 10000 + tasMGS
010600110401     C*
010700110401     C* Verifico codice cliente mittente e imposto "ragione sociale"
010800110401     C                   SELECT
010900111130     C                   WHEN      WCLNCCM = 1162192
011000111130     C                   EVAL      wCCMRAG = 'GE HUNGARY KFT'
011100110401     C                   ENDSL
011200060411     C*
011300110418     C                   Z-ADD     *zeros        wTotSpedOK        7 0
011400110418     C                   EVAL      wTotSpedOK = WCLNANT +
011500110418     C                                          WCLNM12 +
011600110418     C                                          WCLNOKS
011700110418     C*
011800110307     C                   EVAL      VAWDTA =
011900110401     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
012000110401     C                             %trim(       wCCMRAG             )+wSepCampi+
012100110307     C                             %trim(       tasRSD              )+wSepCampi+
012200110307     C                             %trim(       tasIND              )+wSepCampi+
012300110307     C                             %trim(       WCLNREG             )+wSepCampi+
012400110307     C                             %trim(       WCLNPRD             )+wSepCampi+
012500110307     C                             %trim(       WCLNCAD             )+wSepCampi+
012600111130     C                             %trim(%editc(tasRMN         :'Q'))+wSepCampi+
012700111130     C                             %trim(       ' '                 )+wSepCampi+
012800110310     C                             %trim(%editc(tasNCL         :'Q'))+wSepCampi+
012900110310     C                             %trim(       wPKF                )+wSepCampi+
013000110310     C                             %trim(       wVLF                )+wSepCampi+
013100110307     C                             %trim(       WCLNANOM            )+wSepCampi+
013200110307     C                             %trim(%editc(tasDRT         :'X'))+wSepCampi+
013300110307     C                             %trim(%editc(wDSP           :'X'))+wSepCampi+
013400110307     C                             %trim(%editc(tasDCM         :'X'))+wSepCampi+
013500110307     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
013600110418     C                             %trim(%editc(wTotSpedOK     :'Q'))+wSepCampi+
013700110309     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
013800110404     C                             ' '                               +wSepCampi+
013900110309     C                             %trim(       wIMV                )+wSepCampi
014000930409     C*
014100060410     C                   WRITE     TIVAW000
014200110307     C*
014300110307     C                   endif
014400110307     C*
014500060410     C                   DELETE    TIVGD000
014600991027     C*
014700060410     C                   READ      TIVGDTMP
014800030325     C                   ENDDO
014900991027     C*
015000030325     C                   EVAL      wrkesito = '0'
015100991027     C*
015200910830     C                   ENDSR
015300151019
015400151019
015500151019
015600151019     C     RTVAR5FAT     BEGSR
015700151019     C*
015800151019     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
015900151019     C                   SETOFF                                       7071
016000151019     C                   EVAL      ar5TRD = 'FAT'
016100151019     C                   CLEAR                   DAR5FAT
016200151019     C     KEYar531_P    CHAIN     FIAR531C
016300151019     C                   IF        %found(FIAR531C)
016400151019     C                   MOVEL     AR5UNI        DAR5FAT
016500151019     C                   IF        你R5PKTAS > *zeros
016600151019     C                   SETON                                        70
016700151019     C                   EVAL      tasPKF = 你R5PKTAS
016800151019     C                   ENDIF
016900151019     C                   IF        你R5VLTAS > *zeros
017000151019     C                   SETON                                        71
017100151019     C***                EVAL      tasVLF = 你R5VLTAS
017200151019     C                   ENDIF
017300151019     C                   ENDIF
017400151019     C*
017500151019     C                   ENDSR
017600110307
017700110307
017800110307
017900991027      /TITLE Operazioni iniziali.
018000991027     C     *inzsr        BEGSR
018100991027     C*
018200991027     C     *ENTRY        PLIST
018300991027     C                   parm                    prmppt
018400991027     C     wrkesito      parm      wrkesito      prmesito
018500110307     C*
018600110307     C* Definizione chiavi
018700110307     C*
018800110307     C* Chiave su TITAS30C - Parziale
018900110307     C     KEYtas30P     KLIST
019000110307     C                   KFLD                    wclnAAS
019100110307     C                   KFLD                    wclnLNP
019200110307     C                   KFLD                    wclnNRS
019300110307     C                   KFLD                    wclnNSP
019400151019     C*
019500151019     C* FIAR531C - Parziale
019600151019     C     KEYar531_P    KLIST
019700151019     C                   KFLD                    tasAAS
019800151019     C                   KFLD                    tasLNP
019900151019     C                   KFLD                    tasNRS
020000151019     C                   KFLD                    tasNSP
020100151019     C                   KFLD                    ar5TRD
020200991027     C*
020300991027     C                   ENDSR
