000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400110307     FTITAS30C  IF   E           K DISK
000500110307     FFIRAB01L  IF   E           K DISK
000600060410     FTIVGDTMP  UF   E             DISK
000700090323     Ftivawwwt  uF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300110307     D wfcln10ds     e ds                  extname(wfcln10f)
001400110307
001500110307
001600110307
001700991027
001800030908     C                   EXSR      chkRECFILMBR
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
002100110307
002200110307
002300030908
002400030908     C     chkRECFILMBR  BEGSR
002500030908     C*
002600060410     C                   READ      TIVAWWWT                               55
002700030908     C*
002800030908     C                   ENDSR
002900110307
003000110307
003100110307
003200991027
003300991027     C     traduci       BEGSR
003400051007     C*
003500051007     C* Verifico se richiesto un particolare carattere d separatore campi
003600051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003700051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003800051007     C                   ELSE
003900051007     C                   MOVEL     ','           wSepCampi         1
004000051007     C                   ENDIF
004100051007     C*
004200051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004300051007     C                   SETON                                        60
004400051007     C                   IF        %subst(prmppt:4:1) = ','
004500051007     C                   SETOFF                                       60
004600051007     C                   ENDIF
004700030325     C*
004800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005000110401     C                   EVAL      VAWDTA  = '"SENDER CODE         "'+wSepCampi+
005100110401     C                                       '"SENDER NAME         "'+wSepCampi+
005200110401     C                                       '"RECIPIENT NAME      "'+wSepCampi+
005300110307     C                                       '"RECIPIENT ADDRESS   "'+wSepCampi+
005400110307     C                                       '"RECIPIENT REGION    "'+wSepCampi+
005500110307     C                                       '"RECIPIENT PROVINCE  "'+wSepCampi+
005600110307     C                                       '"RECIPIENT ZIP CODE  "'+wSepCampi+
005700110307     C                                       '"SHIPMENT NUMBER     "'+wSepCampi+
005800110307     C                                       '"DELIVERY NOTE       "'+wSepCampi+
005900110310     C                                       '"NBR. OF PARCELS     "'+wSepCampi+
006000110310     C                                       '"INVOICED WEIGHT (KG)"'+wSepCampi+
006100110310     C                                       '"INVOICED VOLUME (M3)"'+wSepCampi+
006200110307     C                                       '"S=EVENTS FAILURE    "'+wSepCampi+
006300110307     C                                       '"ARRIVAL DATE        "'+wSepCampi+
006400110307     C                                       '"SHIPMENT DATE       "'+wSepCampi+
006500110307     C                                       '"DELIVERY DATE       "'+wSepCampi+
006600110307     C                                       '"SLA                 "'+wSepCampi+
006700110307     C                                       '"# >= -999 <= -24    "'+wSepCampi+
006800110307     C                                       '"# >   -24 <= -12    "'+wSepCampi+
006900110307     C                                       '"# >   -12 <=   0    "'+wSepCampi+
007000110309     C                                       '"# >     0           "'+wSepCampi+
007100110404     C                                       '"CARRIER FAILURE Y/N "'+wSepCampi+
007200110309     C                                       '"SHIPMENT COST       "'+wSepCampi
007300030325     C*
007400060410     C                   WRITE     TIVAW000
007500030325     C                   ENDIF
007600991027     C*
007700060410     C                   READ      TIVGDTMP
007800060410     C                   DOW       not %eof(TIVGDTMP)
007900060411     C*
008000060411     C* Valorizzo il buffer dal campo flat di inout
008100110307     C                   EVAL      wfcln10ds = vgdDTA
008200110307     C*
008300110307     C* Innanzitutto chaino la bolla direttamente su TITAS
008400110307     C     KEYtas30P     chain     titas30c
008500110307     C                   if        %found(titas30c)
008600110401     C*
008700110401     C* Inizializzo variabili d wrk
008800110401     C                   Z-ADD     1             wCount            4 0
008900110307     C*
009000110307     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
009100110307     C* anche delle bolle accorpate
009200110307     C*
009300110307     C     KEYrab01_C    SETLL     firab01l
009400110307     C                   IF        %equal(firab01l)
009500110307     C     KEYrab01_C    READE     firab01l
009600110307     C                   DOW       not %eof(firab01l)
009700110309     C*
009800110309     C* Inizializzo variabili d wrk
009900110307     C                   Z-ADD     *zeros        wDSP              8 0
010000110309     C                   MOVEL     *blanks       wIMV             14
010100110310     C                   MOVEL     *blanks       wPKF              8
010200110310     C                   MOVEL     *blanks       wVLF              8
010300110401     C                   MOVEL     *blanks       wCCMRAG          35
010400110309     C*
010500110309     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
010600110309     C  N60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 ,   '))
010700110309     C   60              EVAL      wIMV = %trim(%editw(tasIMV:'        0 .   '))
010800110310     C  N60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 , '))
010900110310     C   60              EVAL      wPKF = %trim(%editw(tasPKF:'    0 . '))
011000110310     C  N60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 ,   '))
011100110310     C   60              EVAL      wVLF = %trim(%editw(tasVLF:'  0 .   '))
011200110309     C*
011300110309     C* Compongo la data spedizione come unico valore
011400110309     C                   EVAL      wDSP = tasAAS * 10000 + tasMGS
011500110401     C*
011600110401     C* Verifico codice cliente mittente e imposto "ragione sociale"
011700110401     C                   SELECT
011800110401     C                   WHEN      WCLNCCM = 1161872
011900110401     C                   EVAL      wCCMRAG = 'GE APP.S AND LIGHTING'
012000110401     C                   WHEN      WCLNCCM = 1161985
012100110401     C                   EVAL      wCCMRAG = 'GE POWER CONTROLS'
012200110401     C                   ENDSL
012300060411     C*
012400110331     C                   IF        wCount = 1
012500110307     C                   EVAL      VAWDTA =
012600110401     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
012700110401     C                             %trim(       wCCMRAG             )+wSepCampi+
012800110307     C                             %trim(       tasRSD              )+wSepCampi+
012900110307     C                             %trim(       tasIND              )+wSepCampi+
013000110307     C                             %trim(       WCLNREG             )+wSepCampi+
013100110307     C                             %trim(       WCLNPRD             )+wSepCampi+
013200110307     C                             %trim(       WCLNCAD             )+wSepCampi+
013300110307     C                             %trim(%editc(RABRMN         :'Q'))+wSepCampi+
013400110307     C                             %trim(       RABRMAR             )+wSepCampi+
013500110310     C                             %trim(%editc(tasNCL         :'Q'))+wSepCampi+
013600110310     C                             %trim(       wPKF                )+wSepCampi+
013700110310     C                             %trim(       wVLF                )+wSepCampi+
013800110307     C                             %trim(       WCLNANOM            )+wSepCampi+
013900110307     C                             %trim(%editc(tasDRT         :'X'))+wSepCampi+
014000110307     C                             %trim(%editc(wDSP           :'X'))+wSepCampi+
014100110307     C                             %trim(%editc(tasDCM         :'X'))+wSepCampi+
014200110307     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
014300110307     C                             %trim(%editc(WCLNANT        :'Q'))+wSepCampi+
014400110307     C                             %trim(%editc(WCLNM12        :'Q'))+wSepCampi+
014500110307     C                             %trim(%editc(WCLNOKS        :'Q'))+wSepCampi+
014600110309     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
014700110404     C                             ' '                               +wSepCampi+
014800110309     C                             %trim(       wIMV                )+wSepCampi
014900110331     C                   ELSE
015000110331     C                   EVAL      VAWDTA =
015100110401     C                                                                wSepCampi+
015200110401     C                                                                wSepCampi+
015300110331     C                                                                wSepCampi+
015400110331     C                                                                wSepCampi+
015500110331     C                                                                wSepCampi+
015600110331     C                                                                wSepCampi+
015700110331     C                                                                wSepCampi+
015800110331     C                                                                wSepCampi+
015900110331     C                             %trim(       RABRMAR             )+wSepCampi+
016000110331     C                                                                wSepCampi+
016100110331     C                                                                wSepCampi+
016200110331     C                                                                wSepCampi+
016300110331     C                                                                wSepCampi+
016400110331     C                                                                wSepCampi+
016500110331     C                                                                wSepCampi+
016600110331     C                                                                wSepCampi+
016700110331     C                                                                wSepCampi+
016800110331     C                                                                wSepCampi+
016900110331     C                                                                wSepCampi+
017000110331     C                                                                wSepCampi+
017100110331     C                                                                wSepCampi+
017200110404     C                                                                wSepCampi+
017300110331     C                                                                wSepCampi
017400110331     C                   ENDIF
017500930409     C*
017600060410     C                   WRITE     TIVAW000
017700110331     C*
017800110331     C                   EVAL      wCount = wCount + 1
017900110331     C*
018000110307     C     KEYrab01_C    READE     firab01l
018100110307     C                   ENDDO
018200110307     C*
018300110307     C                   endif
018400110307     C                   endif
018500110307     C*
018600060410     C                   DELETE    TIVGD000
018700991027     C*
018800060410     C                   READ      TIVGDTMP
018900030325     C                   ENDDO
019000991027     C*
019100030325     C                   EVAL      wrkesito = '0'
019200991027     C*
019300910830     C                   ENDSR
019400110307
019500110307
019600110307
019700991027      /TITLE Operazioni iniziali.
019800991027     C     *inzsr        BEGSR
019900991027     C*
020000991027     C     *ENTRY        PLIST
020100991027     C                   parm                    prmppt
020200991027     C     wrkesito      parm      wrkesito      prmesito
020300110307     C*
020400110307     C* Definizione chiavi
020500110307     C*
020600110307     C* Chiave su TITAS30C - Parziale
020700110307     C     KEYtas30P     KLIST
020800110307     C                   KFLD                    wclnAAS
020900110307     C                   KFLD                    wclnLNP
021000110307     C                   KFLD                    wclnNRS
021100110307     C                   KFLD                    wclnNSP
021200110307     C*
021300110307     C* Chiave su FIRAB01L - Completa
021400110307     C     KEYrab01_C    KLIST
021500110307     C                   KFLD                    wclnAAS
021600110307     C                   KFLD                    wclnLNP
021700110307     C                   KFLD                    wclnNRS
021800110307     C                   KFLD                    wclnNSP
021900991027     C*
022000991027     C                   ENDSR
