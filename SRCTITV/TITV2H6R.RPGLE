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
001300160208     D DTA4A         E DS
001400070316     D sepFld          s              1
001500110810
001600110810     D wSVN            s                   like(tasSV1)
001700110810     D wVAN            s                   like(tasVA1)
001800110810     D wPKB            s              8
001900110810     D wVLF            s              6
002000071205
002100110810     D wV01n           s             11  3
002200110810     D wV02n           s             11  3
002300110810     D wV03n           s             11  3
002400110810     D wV04n           s             11  3
002500110810     D wV05n           s             11  3
002600110810     D wV06n           s             11  3
002700110810     D wV07n           s             11  3
002800110810     D wV08n           s             11  3
002900110810     D wV09n           s             11  3
003000110810     D wV01a           s             12
003100110810     D wV02a           s             12
003200110810     D wV03a           s             12
003300110810     D wV04a           s             12
003400110810     D wV05a           s             12
003500110810     D wV06a           s             12
003600110810     D wV07a           s             12
003700110810     D wV08a           s             12
003800110810     D wV09a           s             12
003900160208
004000160208     D*--------------------
004100160208     D* DEFINIZIONI A PROCEDURE ESTERNE
004200160208     D*--------------------
004300160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
004400160208
004500110810
004600110810
004700991027
004800030908     C                   EXSR      chkRECFILMBR
004900991027     C                   EXSR      traduci
005000160208     C*
005100160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
005200160208     C                   CALL      'UBTA400R'
005300160208     C                   PARM                    UBTA4IOPZ
005400160208     C                   PARM      'C'           UBTA4ITLA
005500160208     C                   PARM                    UBTA4IAAS
005600160208     C                   PARM                    UBTA4ILNP
005700160208     C                   PARM                    UBTA4INRS
005800160208     C                   PARM                    UBTA4INSP
005900160208     C                   PARM                    UBTA4ITRC
006000160208     C                   PARM                    UBTA4OERR
006100160208     C                   PARM                    UBTA4ODS
006200160208     C                   PARM                    UBTA4OLEN
006300160208     C                   PARM                    UBTA4ODATI
006400160208     C*
006500921023     C                   SETON                                        LR
006600030908
006700110810
006800110810
006900030908     C     chkRECFILMBR  BEGSR
007000030908     C*
007100100209     C                   READ      TIVAWWWT                               55
007200030908     C*
007300030908     C                   ENDSR
007400110810
007500110810
007600991027
007700991027     C     traduci       BEGSR
007800030325     C*
007900090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008000090427     C* (descrizione campi in inglese)
008100090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
008200100209     C                   EVAL      VAWDTA =
008300110810     C                             '"Shipping date"'                    +sepFld+
008400110810     C                             '"Delivery note number"'             +sepFld+
008500110810     C                             '"Casio Division"'                   +sepFld+
008600110810     C                             '"Consignee name"'                   +sepFld+
008700110810     C                             '"Consignee zip code"'               +sepFld+
008800110810     C                             '"Consignee city"'                   +sepFld+
008900120309     C                             '"BRT shipment code"'                +sepFld+
009000110810     C                             '"No. of  packing units"'            +sepFld+
009100110810     C                             '"Packaging"'                        +sepFld+
009200110810     C                             '"Weight"'                           +sepFld+
009300110810     C                             '"Volume"'                           +sepFld+
009400110810     C                             '"Value of shipment"'                +sepFld+
009500110810     C                             '"Freight charge"'                   +sepFld+
009600110810     C                             '"Fuel charge"'                      +sepFld+
009700110810     C                             '"Appointment charge"'               +sepFld+
009800110810     C                             '"S/Market & Large Shop Dlv charge"' +sepFld+
009900110810     C                             '"Special transport charge"'         +sepFld+
010000110810     C                             '"Cost of holding goods"'            +sepFld+
010100110810     C                             '"Cost of undelivered shipments"'    +sepFld+
010200110810     C                             '"Other charges"'                    +sepFld+
010300111205     C                             '"Insurance premium"'                +sepFld+
010400111205     C                             '"Charged weight/volume"'            +sepFld
010500090427     C*
010600100209     C                   WRITE     TIVAW000
010700090427     C                   ENDIF
010800991027     C*
010900090325     C                   READ      tivgdtmp
011000090325     C                   DOW       not %eof
011100090325     c                   movel     vgddta        tivafds
011200041220     C*
011300110810     C* Inizializzo i campi d wrk
011400110810     C                   Z-ADD     *zeros        wDSP              8 0
011500110810     C                   MOVEL     *blanks       wNSP             19
011600110810     C                   EVAL      wV01n = *zeros
011700110810     C                   EVAL      wV02n = *zeros
011800110810     C                   EVAL      wV03n = *zeros
011900110810     C                   EVAL      wV04n = *zeros
012000110810     C                   EVAL      wV05n = *zeros
012100110810     C                   EVAL      wV06n = *zeros
012200110810     C                   EVAL      wV07n = *zeros
012300110810     C                   EVAL      wV08n = *zeros
012400110810     C                   EVAL      wV09n = *zeros
012500110810     C                   EVAL      wV01a = *blanks
012600110810     C                   EVAL      wV02a = *blanks
012700110810     C                   EVAL      wV03a = *blanks
012800110810     C                   EVAL      wV04a = *blanks
012900110810     C                   EVAL      wV05a = *blanks
013000110810     C                   EVAL      wV06a = *blanks
013100110810     C                   EVAL      wV07a = *blanks
013200110810     C                   EVAL      wV08a = *blanks
013300110810     C                   EVAL      wV09a = *blanks
013400030709     C*
013500030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
013600110810     C     KEYtas30C_C   CHAIN     titas30c
013700030709     C                   IF        %found(titas30c)
013800110810     C*
013900110810     C* Reperisco varie
014000110810     C                   EXSR      EXESVN
014100160208     C*
014200160208     C* Reperisco NATURA MERCE
014300160208     C                   EXSR      RTVNAS
014400071205     C*
014500110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014600110810     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
014700110810     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
014800110810     C  N60              EVAL      wV01a= %trim(%editw(wV01n :'      0 ,   '))
014900110810     C  N60              EVAL      wV02a= %trim(%editw(wV02n :'      0 ,   '))
015000110810     C  N60              EVAL      wV03a= %trim(%editw(wV03n :'      0 ,   '))
015100110810     C  N60              EVAL      wV04a= %trim(%editw(wV04n :'      0 ,   '))
015200110810     C  N60              EVAL      wV05a= %trim(%editw(wV05n :'      0 ,   '))
015300110810     C  N60              EVAL      wV06a= %trim(%editw(wV06n :'      0 ,   '))
015400110810     C  N60              EVAL      wV07a= %trim(%editw(wV07n :'      0 ,   '))
015500110810     C  N60              EVAL      wV08a= %trim(%editw(wV08n :'      0 ,   '))
015600110810     C  N60              EVAL      wV09a= %trim(%editw(wV09n :'      0 ,   '))
015700110810     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
015800110810     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
015900110810     C   60              EVAL      wV01a= %trim(%editw(wV01n :'      0 .   '))
016000110810     C   60              EVAL      wV02a= %trim(%editw(wV02n :'      0 .   '))
016100110810     C   60              EVAL      wV03a= %trim(%editw(wV03n :'      0 .   '))
016200110810     C   60              EVAL      wV04a= %trim(%editw(wV04n :'      0 .   '))
016300110810     C   60              EVAL      wV05a= %trim(%editw(wV05n :'      0 .   '))
016400110810     C   60              EVAL      wV06a= %trim(%editw(wV06n :'      0 .   '))
016500110810     C   60              EVAL      wV07a= %trim(%editw(wV07n :'      0 .   '))
016600110810     C   60              EVAL      wV08a= %trim(%editw(wV08n :'      0 .   '))
016700110810     C   60              EVAL      wV09a= %trim(%editw(wV09n :'      0 .   '))
016800030325     C*
016900110810     C                   EVAL      wDSP = VAFAAS*10000+VAFMGS
017000110810     C                   EVAL      wNSP = %trim(%editc(VAFAAS:'Q'))+'-'+
017100110810     C                                    %trim(%editc(VAFLNP:'Q'))+'-'+
017200110810     C                                    %trim(%editc(VAFNRS:'Q'))+'-'+
017300110810     C                                    %trim(%editc(VAFNSP:'Q'))
017400110810     C*
017500110810     C                   EVAL      VAWDTA  =
017600110810     C                             %trim(%editc(wDSP:'Q'))              +sepFld+
017700110810     C                             %trim(%editc(VAFRMN:'Q'))            +sepFld+
017800160208     C                             %trim(§TA4ANAS)                      +sepFld+
017900110810     C                             %trim(tasRSD)                        +sepFld+
018000110810     C                             %trim(tasCAD)                        +sepFld+
018100110810     C                             %trim(tasLOD)                        +sepFld+
018200110810     C                             %trim(wNSP)                          +sepFld+
018300110810     C                             %trim(%editc(VAFNCL:'Q'))            +sepFld+
018400110810     C                             ' '                                  +sepFld+
018500110810     C                             %trim(wPKB)                          +sepFld+
018600110810     C                             %trim(wVLF)                          +sepFld+
018700110810     C                             '0'                                  +sepFld+
018800110810     C                             %trim(wV01a)                         +sepFld+
018900110810     C                             %trim(wV02a)                         +sepFld+
019000110810     C                             %trim(wV03a)                         +sepFld+
019100110810     C                             %trim(wV04a)                         +sepFld+
019200110810     C                             %trim(wV05a)                         +sepFld+
019300110810     C                             %trim(wV06a)                         +sepFld+
019400110810     C                             %trim(wV07a)                         +sepFld+
019500110810     C                             %trim(wV08a)                         +sepFld+
019600111205     C                             %trim(wV09a)                         +sepFld+
019700111205     C                             %trim(%editc(tasPVL:'Q'))            +sepFld
019800930409     C*
019900100209     C                   WRITE     TIVAW000
020000110810     C                   ENDIF
020100110810     C*
020200090325     C                   delete    tivgd000
020300991027     C*
020400090325     C                   READ      tivgdtmp
020500030325     C                   ENDDO
020600991027     C*
020700030325     C                   EVAL      wrkesito = '0'
020800991027     C*
020900910830     C                   ENDSR
021000110810     C*------------------------------------------------------------------------*
021100110810     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
021200110810     C*------------------------------------------------------------------------*
021300110810     C     EXESVN        BEGSR
021400110810     C*
021500110810     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
021600110810     C                   MOVEL     *blanks       wSVN
021700110810     C                   Z-ADD     *zeros        wVAN
021800110810     C*
021900110810     C* Verifica ed eventuale scrittura del PORTO su TITAS
022000110810     C                   IF        tasPOR <> *zeros
022100110810     C                   EVAL      wSVN = '1'
022200110810     C                   EVAL      wVAN = tasPOR
022300110816     C                   EXSR      CHKSVN
022400110810     C                   ENDIF
022500110810     C*
022600110810     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
022700110810     C                   IF        tasSV1 <> *blanks
022800110810     C                   EVAL      wSVN = tasSV1
022900110810     C                   EVAL      wVAN = tasVA1
023000110810     C                   EXSR      CHKSVN
023100110810     C                   ENDIF
023200110810     C*
023300110810     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
023400110810     C                   IF        tasSV2 <> *blanks
023500110810     C                   EVAL      wSVN = tasSV2
023600110810     C                   EVAL      wVAN = tasVA2
023700110810     C                   EXSR      CHKSVN
023800110810     C                   ENDIF
023900110810     C*
024000110810     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
024100110810     C                   IF        tasSV3 <> *blanks
024200110810     C                   EVAL      wSVN = tasSV3
024300110810     C                   EVAL      wVAN = tasVA3
024400110810     C                   EXSR      CHKSVN
024500110810     C*
024600110810     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
024700110810     C     KEYta730C_P   SETLL     tita730c
024800110810     C                   IF        %equal(tita730c)
024900110810     C     KEYta730C_P   READE     tita730c
025000110810     C                   DOW       not %eof(tita730c)
025100110810     C                   IF        ta7VAN > *zeros
025200110810     C                   EVAL      wSVN = ta7SVN
025300110810     C                   EVAL      wVAN = ta7VAN
025400110810     C                   EXSR      CHKSVN
025500110810     C                   ENDIF
025600110810     C     KEYta730C_P   READE     tita730c
025700110810     C                   ENDDO
025800110810     C                   ENDIF
025900110810     C                   ENDIF
026000110810     C*
026100110810     C                   ENDSR
026200110810     C*------------------------------------------------------------------------*
026300110810     C* CHKSVN - Routine di considerazioni sul varie e relativi "raggrupamenti"
026400110810     C*------------------------------------------------------------------------*
026500110810     C     CHKSVN        BEGSR
026600110810     C*
026700110810     C                   SELECT
026800110810     C                   WHEN      wSVN = '1'
026900110810     C                   ADD       wVAN          wV01n
027000110810     C                   WHEN      wSVN = 'f'
027100110810     C                   ADD       wVAN          wV02n
027200110810     C                   WHEN      wSVN = '+'
027300110810     C                   ADD       wVAN          wV02n
027400110810     C                   WHEN      wSVN = 'A'
027500110810     C                   ADD       wVAN          wV03n
027600110810     C                   WHEN      wSVN = 'S'
027700110810     C                   ADD       wVAN          wV04n
027800110810     C                   WHEN      wSVN = 'F'
027900110810     C                   ADD       wVAN          wV05n
028000110810     C                   WHEN      wSVN = 'I'
028100110810     C                   ADD       wVAN          wV06n
028200110810     C                   WHEN      wSVN = 'c'
028300110810     C                   ADD       wVAN          wV07n
028400110810     C                   WHEN      wSVN = 'E'
028500110810     C                   ADD       wVAN          wV09n
028600110810     C                   WHEN      wSVN = 'R'
028700110810     C                   ADD       wVAN          wV09n
028800110810     C                   WHEN      wSVN = 'd'
028900110810     C                   ADD       wVAN          wV09n
029000110810     C                   OTHER
029100110810     C                   ADD       wVAN          wV08n
029200110810     C                   ENDSL
029300110810     C*
029400110810     C                   ENDSR
029500991027
029600160208
029700160208
029800160208     C     RTVNAS        BEGSR
029900160208     C*
030000160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
030100160208     C                   CLEAR                   DTA4A
030200160208     C*
030300160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
030400160208     C                   CALL      'UBTA400R'
030500160208     C                   PARM      *blanks       UBTA4IOPZ
030600160208     C                   PARM      *blanks       UBTA4ITLA
030700160208     C                   PARM      tasAAS        UBTA4IAAS
030800160208     C                   PARM      tasLNP        UBTA4ILNP
030900160208     C                   PARM      tasNRS        UBTA4INRS
031000160208     C                   PARM      tasNSP        UBTA4INSP
031100160208     C                   PARM      'A'           UBTA4ITRC
031200160208     C                   PARM                    UBTA4OERR
031300160208     C                   PARM                    UBTA4ODS
031400160208     C                   PARM                    UBTA4OLEN
031500160208     C                   PARM                    UBTA4ODATI
031600160208     C*
031700160208     C                   IF        UBTA4OERR = *zeros
031800160208     C                   SELECT
031900160208     C* Gestione output tipo record 'A'
032000160208     C                   WHEN      UBTA4ODS = 'DTA4A'
032100160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
032200160208     C                   ENDSL
032300160208     C*
032400160208     C                   ENDIF
032500160208     C*
032600160208     C                   ENDSR
032700110810
032800160208
032900110810
033000991027      /TITLE Operazioni iniziali.
033100991027     C     *inzsr        BEGSR
033200991027     C*
033300991027     C     *ENTRY        PLIST
033400991027     C                   parm                    prmppt
033500991027     C     wrkesito      parm      wrkesito      prmesito
033600070316     C*
033700070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
033800070316     C                   eval      sepFld = %subst(prmppt:2:1)
033900070316     C                   if        sepFld = *blanks
034000070316     C                   eval      sepFld = ','
034100070316     C                   endif
034200071205     C*
034300071205     C* Verifico se richiesto un particolare carattere d separatore decimale
034400071205     C                   SETON                                        60
034500071205     C                   IF        %subst(prmppt:3:1) = ','
034600071205     C                   SETOFF                                       60
034700071205     C                   ENDIF
034800030709     C*
034900030709     C* Definizione chiavi
035000030709     C*
035100030709     C* Chiave su TITAS30C - Completa
035200110810     C     KEYtas30C_C   KLIST
035300030709     C                   KFLD                    vafAAS
035400030709     C                   KFLD                    vafLNP
035500030709     C                   KFLD                    vafNRS
035600030709     C                   KFLD                    vafNSP
035700030709     C                   KFLD                    vafTBL
035800110810     C*
035900110810     C* Chiave su tita730c - Parziale
036000110810     C     KEYta730C_P   KLIST
036100110810     C                   KFLD                    vafAAS
036200110810     C                   KFLD                    vafLNP
036300110810     C                   KFLD                    vafNRS
036400110810     C                   KFLD                    vafNSP
036500110810     C                   KFLD                    vafTBL
036600110810     C*
036700991027     C                   ENDSR
