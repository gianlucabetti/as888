000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500110810     FTITA730C  IF   E           K DISK
000600090325     Ftivgdtmp  uF   E             DISK
000700100209     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090325     D tivafds       e ds                  extname(tivaf00t)
001300070316     D sepFld          s              1
001400110810
001500110810     D wSVN            s                   like(tasSV1)
001600110810     D wVAN            s                   like(tasVA1)
001700110810     D wPKB            s              8
001800110810     D wVLF            s              6
001900071205
002000110810     D wV01n           s             11  3
002100110810     D wV02n           s             11  3
002200110810     D wV03n           s             11  3
002300110810     D wV04n           s             11  3
002400110810     D wV05n           s             11  3
002500110810     D wV06n           s             11  3
002600110810     D wV07n           s             11  3
002700110810     D wV08n           s             11  3
002800110810     D wV09n           s             11  3
002900110810     D wV01a           s             12
003000110810     D wV02a           s             12
003100110810     D wV03a           s             12
003200110810     D wV04a           s             12
003300110810     D wV05a           s             12
003400110810     D wV06a           s             12
003500110810     D wV07a           s             12
003600110810     D wV08a           s             12
003700110810     D wV09a           s             12
003800110810
003900110810
004000991027
004100030908     C                   EXSR      chkRECFILMBR
004200991027     C                   EXSR      traduci
004300921023     C                   SETON                                        LR
004400030908
004500110810
004600110810
004700030908     C     chkRECFILMBR  BEGSR
004800030908     C*
004900100209     C                   READ      TIVAWWWT                               55
005000030908     C*
005100030908     C                   ENDSR
005200110810
005300110810
005400991027
005500991027     C     traduci       BEGSR
005600030325     C*
005700090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005800090427     C* (descrizione campi in inglese)
005900090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
006000100209     C                   EVAL      VAWDTA =
006100110810     C                             '"Shipping date"'                    +sepFld+
006200110810     C                             '"Delivery note number"'             +sepFld+
006300110810     C                             '"Casio Division"'                   +sepFld+
006400110810     C                             '"Consignee name"'                   +sepFld+
006500110810     C                             '"Consignee zip code"'               +sepFld+
006600110810     C                             '"Consignee city"'                   +sepFld+
006700120309     C                             '"BRT shipment code"'                +sepFld+
006800110810     C                             '"No. of  packing units"'            +sepFld+
006900110810     C                             '"Packaging"'                        +sepFld+
007000110810     C                             '"Weight"'                           +sepFld+
007100110810     C                             '"Volume"'                           +sepFld+
007200110810     C                             '"Value of shipment"'                +sepFld+
007300110810     C                             '"Freight charge"'                   +sepFld+
007400110810     C                             '"Fuel charge"'                      +sepFld+
007500110810     C                             '"Appointment charge"'               +sepFld+
007600110810     C                             '"S/Market & Large Shop Dlv charge"' +sepFld+
007700110810     C                             '"Special transport charge"'         +sepFld+
007800110810     C                             '"Cost of holding goods"'            +sepFld+
007900110810     C                             '"Cost of undelivered shipments"'    +sepFld+
008000110810     C                             '"Other charges"'                    +sepFld+
008100111205     C                             '"Insurance premium"'                +sepFld+
008200111205     C                             '"Charged weight/volume"'            +sepFld
008300090427     C*
008400100209     C                   WRITE     TIVAW000
008500090427     C                   ENDIF
008600991027     C*
008700090325     C                   READ      tivgdtmp
008800090325     C                   DOW       not %eof
008900090325     c                   movel     vgddta        tivafds
009000041220     C*
009100110810     C* Inizializzo i campi d wrk
009200110810     C                   Z-ADD     *zeros        wDSP              8 0
009300110810     C                   MOVEL     *blanks       wNSP             19
009400110810     C                   EVAL      wV01n = *zeros
009500110810     C                   EVAL      wV02n = *zeros
009600110810     C                   EVAL      wV03n = *zeros
009700110810     C                   EVAL      wV04n = *zeros
009800110810     C                   EVAL      wV05n = *zeros
009900110810     C                   EVAL      wV06n = *zeros
010000110810     C                   EVAL      wV07n = *zeros
010100110810     C                   EVAL      wV08n = *zeros
010200110810     C                   EVAL      wV09n = *zeros
010300110810     C                   EVAL      wV01a = *blanks
010400110810     C                   EVAL      wV02a = *blanks
010500110810     C                   EVAL      wV03a = *blanks
010600110810     C                   EVAL      wV04a = *blanks
010700110810     C                   EVAL      wV05a = *blanks
010800110810     C                   EVAL      wV06a = *blanks
010900110810     C                   EVAL      wV07a = *blanks
011000110810     C                   EVAL      wV08a = *blanks
011100110810     C                   EVAL      wV09a = *blanks
011200030709     C*
011300030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
011400110810     C     KEYtas30C_C   CHAIN     titas30c
011500030709     C                   IF        %found(titas30c)
011600110810     C*
011700110810     C* Reperisco varie
011800110810     C                   EXSR      EXESVN
011900071205     C*
012000110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
012100110810     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
012200110810     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
012300110810     C  N60              EVAL      wV01a= %trim(%editw(wV01n :'      0 ,   '))
012400110810     C  N60              EVAL      wV02a= %trim(%editw(wV02n :'      0 ,   '))
012500110810     C  N60              EVAL      wV03a= %trim(%editw(wV03n :'      0 ,   '))
012600110810     C  N60              EVAL      wV04a= %trim(%editw(wV04n :'      0 ,   '))
012700110810     C  N60              EVAL      wV05a= %trim(%editw(wV05n :'      0 ,   '))
012800110810     C  N60              EVAL      wV06a= %trim(%editw(wV06n :'      0 ,   '))
012900110810     C  N60              EVAL      wV07a= %trim(%editw(wV07n :'      0 ,   '))
013000110810     C  N60              EVAL      wV08a= %trim(%editw(wV08n :'      0 ,   '))
013100110810     C  N60              EVAL      wV09a= %trim(%editw(wV09n :'      0 ,   '))
013200110810     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
013300110810     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
013400110810     C   60              EVAL      wV01a= %trim(%editw(wV01n :'      0 .   '))
013500110810     C   60              EVAL      wV02a= %trim(%editw(wV02n :'      0 .   '))
013600110810     C   60              EVAL      wV03a= %trim(%editw(wV03n :'      0 .   '))
013700110810     C   60              EVAL      wV04a= %trim(%editw(wV04n :'      0 .   '))
013800110810     C   60              EVAL      wV05a= %trim(%editw(wV05n :'      0 .   '))
013900110810     C   60              EVAL      wV06a= %trim(%editw(wV06n :'      0 .   '))
014000110810     C   60              EVAL      wV07a= %trim(%editw(wV07n :'      0 .   '))
014100110810     C   60              EVAL      wV08a= %trim(%editw(wV08n :'      0 .   '))
014200110810     C   60              EVAL      wV09a= %trim(%editw(wV09n :'      0 .   '))
014300030325     C*
014400110810     C                   EVAL      wDSP = VAFAAS*10000+VAFMGS
014500110810     C                   EVAL      wNSP = %trim(%editc(VAFAAS:'Q'))+'-'+
014600110810     C                                    %trim(%editc(VAFLNP:'Q'))+'-'+
014700110810     C                                    %trim(%editc(VAFNRS:'Q'))+'-'+
014800110810     C                                    %trim(%editc(VAFNSP:'Q'))
014900110810     C*
015000110810     C                   EVAL      VAWDTA  =
015100110810     C                             %trim(%editc(wDSP:'Q'))              +sepFld+
015200110810     C                             %trim(%editc(VAFRMN:'Q'))            +sepFld+
015300110816     C                             %trim(tasNAS)                        +sepFld+
015400110810     C                             %trim(tasRSD)                        +sepFld+
015500110810     C                             %trim(tasCAD)                        +sepFld+
015600110810     C                             %trim(tasLOD)                        +sepFld+
015700110810     C                             %trim(wNSP)                          +sepFld+
015800110810     C                             %trim(%editc(VAFNCL:'Q'))            +sepFld+
015900110810     C                             ' '                                  +sepFld+
016000110810     C                             %trim(wPKB)                          +sepFld+
016100110810     C                             %trim(wVLF)                          +sepFld+
016200110810     C                             '0'                                  +sepFld+
016300110810     C                             %trim(wV01a)                         +sepFld+
016400110810     C                             %trim(wV02a)                         +sepFld+
016500110810     C                             %trim(wV03a)                         +sepFld+
016600110810     C                             %trim(wV04a)                         +sepFld+
016700110810     C                             %trim(wV05a)                         +sepFld+
016800110810     C                             %trim(wV06a)                         +sepFld+
016900110810     C                             %trim(wV07a)                         +sepFld+
017000110810     C                             %trim(wV08a)                         +sepFld+
017100111205     C                             %trim(wV09a)                         +sepFld+
017200111205     C                             %trim(%editc(tasPVL:'Q'))            +sepFld
017300930409     C*
017400100209     C                   WRITE     TIVAW000
017500110810     C                   ENDIF
017600110810     C*
017700090325     C                   delete    tivgd000
017800991027     C*
017900090325     C                   READ      tivgdtmp
018000030325     C                   ENDDO
018100991027     C*
018200030325     C                   EVAL      wrkesito = '0'
018300991027     C*
018400910830     C                   ENDSR
018500110810     C*------------------------------------------------------------------------*
018600110810     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
018700110810     C*------------------------------------------------------------------------*
018800110810     C     EXESVN        BEGSR
018900110810     C*
019000110810     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
019100110810     C                   MOVEL     *blanks       wSVN
019200110810     C                   Z-ADD     *zeros        wVAN
019300110810     C*
019400110810     C* Verifica ed eventuale scrittura del PORTO su TITAS
019500110810     C                   IF        tasPOR <> *zeros
019600110810     C                   EVAL      wSVN = '1'
019700110810     C                   EVAL      wVAN = tasPOR
019800110816     C                   EXSR      CHKSVN
019900110810     C                   ENDIF
020000110810     C*
020100110810     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
020200110810     C                   IF        tasSV1 <> *blanks
020300110810     C                   EVAL      wSVN = tasSV1
020400110810     C                   EVAL      wVAN = tasVA1
020500110810     C                   EXSR      CHKSVN
020600110810     C                   ENDIF
020700110810     C*
020800110810     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
020900110810     C                   IF        tasSV2 <> *blanks
021000110810     C                   EVAL      wSVN = tasSV2
021100110810     C                   EVAL      wVAN = tasVA2
021200110810     C                   EXSR      CHKSVN
021300110810     C                   ENDIF
021400110810     C*
021500110810     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
021600110810     C                   IF        tasSV3 <> *blanks
021700110810     C                   EVAL      wSVN = tasSV3
021800110810     C                   EVAL      wVAN = tasVA3
021900110810     C                   EXSR      CHKSVN
022000110810     C*
022100110810     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
022200110810     C     KEYta730C_P   SETLL     tita730c
022300110810     C                   IF        %equal(tita730c)
022400110810     C     KEYta730C_P   READE     tita730c
022500110810     C                   DOW       not %eof(tita730c)
022600110810     C                   IF        ta7VAN > *zeros
022700110810     C                   EVAL      wSVN = ta7SVN
022800110810     C                   EVAL      wVAN = ta7VAN
022900110810     C                   EXSR      CHKSVN
023000110810     C                   ENDIF
023100110810     C     KEYta730C_P   READE     tita730c
023200110810     C                   ENDDO
023300110810     C                   ENDIF
023400110810     C                   ENDIF
023500110810     C*
023600110810     C                   ENDSR
023700110810     C*------------------------------------------------------------------------*
023800110810     C* CHKSVN - Routine di considerazioni sul varie e relativi "raggrupamenti"
023900110810     C*------------------------------------------------------------------------*
024000110810     C     CHKSVN        BEGSR
024100110810     C*
024200110810     C                   SELECT
024300110810     C                   WHEN      wSVN = '1'
024400110810     C                   ADD       wVAN          wV01n
024500110810     C                   WHEN      wSVN = 'f'
024600110810     C                   ADD       wVAN          wV02n
024700110810     C                   WHEN      wSVN = '+'
024800110810     C                   ADD       wVAN          wV02n
024900110810     C                   WHEN      wSVN = 'A'
025000110810     C                   ADD       wVAN          wV03n
025100110810     C                   WHEN      wSVN = 'S'
025200110810     C                   ADD       wVAN          wV04n
025300110810     C                   WHEN      wSVN = 'F'
025400110810     C                   ADD       wVAN          wV05n
025500110810     C                   WHEN      wSVN = 'I'
025600110810     C                   ADD       wVAN          wV06n
025700110810     C                   WHEN      wSVN = 'c'
025800110810     C                   ADD       wVAN          wV07n
025900110810     C                   WHEN      wSVN = 'E'
026000110810     C                   ADD       wVAN          wV09n
026100110810     C                   WHEN      wSVN = 'R'
026200110810     C                   ADD       wVAN          wV09n
026300110810     C                   WHEN      wSVN = 'd'
026400110810     C                   ADD       wVAN          wV09n
026500110810     C                   OTHER
026600110810     C                   ADD       wVAN          wV08n
026700110810     C                   ENDSL
026800110810     C*
026900110810     C                   ENDSR
027000991027
027100110810
027200110810
027300991027      /TITLE Operazioni iniziali.
027400991027     C     *inzsr        BEGSR
027500991027     C*
027600991027     C     *ENTRY        PLIST
027700991027     C                   parm                    prmppt
027800991027     C     wrkesito      parm      wrkesito      prmesito
027900070316     C*
028000070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
028100070316     C                   eval      sepFld = %subst(prmppt:2:1)
028200070316     C                   if        sepFld = *blanks
028300070316     C                   eval      sepFld = ','
028400070316     C                   endif
028500071205     C*
028600071205     C* Verifico se richiesto un particolare carattere d separatore decimale
028700071205     C                   SETON                                        60
028800071205     C                   IF        %subst(prmppt:3:1) = ','
028900071205     C                   SETOFF                                       60
029000071205     C                   ENDIF
029100030709     C*
029200030709     C* Definizione chiavi
029300030709     C*
029400030709     C* Chiave su TITAS30C - Completa
029500110810     C     KEYtas30C_C   KLIST
029600030709     C                   KFLD                    vafAAS
029700030709     C                   KFLD                    vafLNP
029800030709     C                   KFLD                    vafNRS
029900030709     C                   KFLD                    vafNSP
030000030709     C                   KFLD                    vafTBL
030100110810     C*
030200110810     C* Chiave su tita730c - Parziale
030300110810     C     KEYta730C_P   KLIST
030400110810     C                   KFLD                    vafAAS
030500110810     C                   KFLD                    vafLNP
030600110810     C                   KFLD                    vafNRS
030700110810     C                   KFLD                    vafNSP
030800110810     C                   KFLD                    vafTBL
030900110810     C*
031000991027     C                   ENDSR
