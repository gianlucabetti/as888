000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400111004     FTNCSB03L  IF   E           K DISK
000500040628     FTITAS30C  IF   E           K DISK
000600050722     FFIAR531C  IF   E           K DISK
000700050908     FTIVGDTMP  UF   E             DISK
000800100610     FTIVAWWWT  UF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300050908     D fnvagds       e ds                  extname(fnvag00t)
001400080918     D ChCSV           s             10A   varying inz(' ')
001500030325
001600050722     D dar5gen       e ds
001700040628
001800040628     D dstas           ds
001900040628     D  tasRSD
002000040628     D  tasIND
002100040628     D  tasCAD
002200040628     D  tasLOD
002300040628     D  tasPRD
002400040628     D  tasNZD
002500050722
002600050722     D dsar5           ds
002700050722     D  ar5UNI
002800991027
002900030908     C                   EXSR      chkRECFILMBR
003000991027     C                   EXSR      traduci
003100921023     C                   SETON                                        LR
003200030908
003300030908     C     chkRECFILMBR  BEGSR
003400030908     C*
003500100610     C                   READ      TIVAWWWT                               55
003600030908     C*
003700030908     C                   ENDSR
003800991027
003900991027     C     traduci       BEGSR
004000080918     C*
004100080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004200080923     C                   IF        %subst(prmppt:4:1) = *blanks
004300100610     C                   EVAL      ChCSV = ';'
004400080918     C                   ELSE
004500080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004600080918     C                   ENDIF
004700030325     C*
004800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004900111003     C*
005000111003     C* Gestisco con *IN40 solamente i campi richiesti dal cliente
005100111003     C                   SETON                                        40
005200110909     C*
005300110909     C* - NOMI CAMPO
005400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005500111003     C                   EVAL      VAWDTA  =
005600080918     C                                       '"VAGAGC"'+ChCSV+
005700080918     C                                       '"VAGMGC"'+ChCSV+
005800080918     C                                       '"VAGFGC"'+ChCSV+
005900080918     C                                       '"VAGNGC"'+ChCSV+
006000080918     C                                       '"VAGFRG"'+ChCSV+
006100080918     C                                       '"VAGCMC"'+ChCSV+
006200080918     C                                       '"VAGDMC"'+ChCSV+
006300080918     C                                       '"VAGNOT"'+ChCSV+
006400080918     C                                       '"VAGRMN"'+ChCSV+
006500080918     C                                       '"VAGRMA"'+ChCSV+
006600111004     C                                       '"TASCAS"'+ChCSV+
006700080918     C                                       '"VAGDUR"'+ChCSV+
006800111003     C                                       '"VAGDCG"'+ChCSV
006900040628     C*
007000040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
007100040628     C                   IF        %subst(prmppt:3:1) = 'D'
007200100610     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
007300080918     C                                           '"VAGRSD"'+ChCSV+
007400080918     C                                           '"VAGIND"'+ChCSV+
007500080918     C                                           '"VAGCAD"'+ChCSV+
007600080918     C                                           '"VAGLOD"'+ChCSV+
007700080918     C                                           '"VAGPRD"'+ChCSV+
007800080918     C                                           '"VAGNZD"'+ChCSV+
007900080918     C                                           '"VAGTEL"'
008000040628     C                   ENDIF
008100030325     C*
008200100610     C                   WRITE     TIVAW000
008300030325     C                   ENDIF
008400110909     C*
008500110909     C* - DESCRIZIONI CAMPO - ITALIANO
008600110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
008700110909     C                   EVAL      VAWDTA  =
008800110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
008900110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
009000110909     C                             '"FILIALE CHE HA APERTO LA GIACENZA"' +ChCSV+
009100110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
009200110909     C                             '"CAUSALE GIACENZA"'                  +ChCSV+
009300110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
009400110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
009500110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
009600110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
009700111004     C                             '"IMPORTO C/ASSEGNO"'                 +ChCSV+
009800110909     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV+
009900111003     C                             '"DATA CHIUSURA GIACENZA IN ARRIVO"'  +ChCSV
010000110909     C*
010100110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
010200110909     C                   IF        %subst(prmppt:3:1) = 'D'
010300110909     C                   EVAL      VAWDTA  =
010400110909     C                             %trim(VAWDTA)                         +ChCSV+
010500110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
010600110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
010700110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
010800110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
010900110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
011000110909     C                             '"NAZIONE DESTINATARIO"'              +ChCSV+
011100110909     C                             '"TELEFONO DESTINATARIO"'
011200110909     C                   ENDIF
011300110909     C*
011400110909     C                   WRITE     TIVAW000
011500110909     C                   ENDIF
011600991027     C*
011700050908     C                   READ      TIVGDTMP
011800050908     C                   DOW       not %eof(TIVGDTMP)
011900050908     C                   eval      fnvagds = vgdDTA
012000991027     C*
012100030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012200060411     C***  '"':''''      XLATE     fnvagds       fnvagds
012300040628     C*
012400111007     C* Chaino la bolla relativa alla giacenza corrente.
012500050722     C                   CLEAR                   dstas
012600050722     C                   CLEAR                   dsar5
012700050722     C                   CLEAR                   dar5gen
012800040628     C     KEYTAS30_1P   CHAIN     TITAS30C
012900040628     C                   IF        %found(TITAS30C)
013000111004     C*
013100111004     C                   z-add     *zeros        wCAS             13 3
013200111004     C*
013300111004     C* Quindi verifico subito esistenza contrassegno
013400111004     C     KEYcsb03_C    CHAIN     tncsb03l
013500111004     C                   IF        %found(tncsb03l)
013600111004     C                   eval      wCAS = csbCAS
013700111004     C                   ENDIF
013800111004     C*
013900050722     C                   EVAL      AR5TRD = 'GEN'
014000050722     C     KEYAR531_1P   CHAIN     FIAR531C
014100050722     C                   IF        %found(FIAR531C)
014200040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
014300060411     C***  '"':''''      XLATE     dstas         dstas
014400060411     C***  '"':''''      XLATE     dsar5         dsar5
014500050722     C                   EVAL      dar5gen = ar5UNI
014600040628     C                   ENDIF
014700050722     C                   ENDIF
014800030325     C*
014900040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
015000040204     C                   IF        %subst(prmppt:2:1) = 'D'
015100111003     C                   EVAL      VAWDTA  =
015200080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
015300080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
015400080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
015500080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
015600080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
015700080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
015800080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
015900080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
016000080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
016100111004     C                                       %trim(%editc(wCAS:'Q'))  +ChCSV+
016200080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
016300111003     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV
016400080918     C*
016500040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
016600040628     C                   IF        %subst(prmppt:3:1) = 'D'
016700100610     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
016800080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
016900080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
017000080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
017100080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
017200080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
017300080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
017400080918     C                                        '"'+%trim(§AR5TELD)+'"'
017500080918     C                   ENDIF
017600040628     C*
017700040204     C                   ELSE
017800040204     C*
017900111003     C                   EVAL      VAWDTA  =
018000080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
018100080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
018200080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
018300080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
018400080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
018500080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
018600080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
018700080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
018800080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
018900111004     C                                       %trim(%editc(wCAS:'Q'))  +ChCSV+
019000080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
019100111003     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV
019200040628     C*
019300040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
019400040628     C                   IF        %subst(prmppt:3:1) = 'D'
019500111003     C                   EVAL      VAWDTA  =
019600111003     C                                              %trim(VAWDTA)    +ChCSV+
019700080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
019800080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
019900080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
020000080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
020100080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
020200080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
020300080918     C                                        '"'+%trim(§AR5TELD)+'"'
020400080918     C                   ENDIF
020500080918     C*
020600040204     C                   ENDIF
020700930409     C*
020800100610     C                   WRITE     TIVAW000
020900050908     C                   DELETE    TIVGD000
021000991027     C*
021100050908     C                   READ      TIVGDTMP
021200030325     C                   ENDDO
021300991027     C*
021400030325     C                   EVAL      wrkesito = '0'
021500991027     C*
021600910830     C                   ENDSR
021700991027
021800991027      /TITLE Operazioni iniziali.
021900991027     C     *inzsr        BEGSR
022000991027     C*
022100991027     C     *ENTRY        PLIST
022200040628     C                   PARM                    prmppt
022300040628     C     wrkesito      PARM      wrkesito      prmesito
022400991027     C*
022500040628     C* DEFINIZIONE CHIAVI
022600040628     C*
022700040628     C* Chiave su TITAS30C - Parziale
022800040628     C     KEYTAS30_1P   KLIST
022900040628     C                   KFLD                    VAGAAS
023000040628     C                   KFLD                    VAGLNP
023100040628     C                   KFLD                    VAGNRS
023200040628     C                   KFLD                    VAGNSP
023300111004     C*
023400111004     C* Chiave su TNCSB03L - Completa
023500111004     C     KEYcsb03_C    KLIST
023600111004     C                   KFLD                    VAGAAS
023700111004     C                   KFLD                    VAGLNP
023800111004     C                   KFLD                    VAGNRS
023900111004     C                   KFLD                    VAGNSP
024000050722     C*
024100050722     C* Chiave su FIAR531C - Parziale
024200050722     C     KEYAR531_1P   KLIST
024300050722     C                   KFLD                    VAGAAS
024400050722     C                   KFLD                    VAGLNP
024500050722     C                   KFLD                    VAGNRS
024600050722     C                   KFLD                    VAGNSP
024700050722     C                   KFLD                    AR5TRD
024800040628     C*
024900991027     C                   ENDSR
