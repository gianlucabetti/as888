000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700100610     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200050908     D fnvagds       e ds                  extname(fnvag00t)
001300080918     D ChCSV           s             10A   varying inz(' ')
001400030325
001500050722     D dar5gen       e ds
001600040628
001700040628     D dstas           ds
001800040628     D  tasRSD
001900040628     D  tasIND
002000040628     D  tasCAD
002100040628     D  tasLOD
002200040628     D  tasPRD
002300040628     D  tasNZD
002400050722
002500050722     D dsar5           ds
002600050722     D  ar5UNI
002700991027
002800030908     C                   EXSR      chkRECFILMBR
002900991027     C                   EXSR      traduci
003000921023     C                   SETON                                        LR
003100030908
003200030908     C     chkRECFILMBR  BEGSR
003300030908     C*
003400100610     C                   READ      TIVAWWWT                               55
003500030908     C*
003600030908     C                   ENDSR
003700991027
003800991027     C     traduci       BEGSR
003900080918     C*
004000080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004100080923     C                   IF        %subst(prmppt:4:1) = *blanks
004200100610     C                   EVAL      ChCSV = ';'
004300080918     C                   ELSE
004400080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004500080918     C                   ENDIF
004600030325     C*
004700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004800110909     C*
004900110909     C* - NOMI CAMPO
005000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005100111003     C                   EVAL      VAWDTA  =
005200080918     C                                       '"VAGAGC"'+ChCSV+
005300080918     C                                       '"VAGMGC"'+ChCSV+
005400080918     C                                       '"VAGFGC"'+ChCSV+
005500080918     C                                       '"VAGNGC"'+ChCSV+
005600080918     C                                       '"VAGCMC"'+ChCSV+
005700080918     C                                       '"VAGDMC"'+ChCSV+
005800080918     C                                       '"VAGNOT"'+ChCSV+
005900080918     C                                       '"VAGRMN"'+ChCSV+
006000080918     C                                       '"VAGRMA"'+ChCSV+
006100111003     C                                       '"VAGDUR"'+ChCSV
006200040628     C*
006300040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
006400040628     C                   IF        %subst(prmppt:3:1) = 'D'
006500131025     C                   EVAL      VAWDTA  = %trim(VAWDTA)+
006600080918     C                                           '"VAGRSD"'+ChCSV+
006700080918     C                                           '"VAGIND"'+ChCSV+
006800080918     C                                           '"VAGCAD"'+ChCSV+
006900080918     C                                           '"VAGLOD"'+ChCSV+
007000080918     C                                           '"VAGPRD"'+ChCSV+
007100080918     C                                           '"VAGNZD"'+ChCSV+
007200131025     C                                           '"VAGTEL"'+ChCSV
007300040628     C                   ENDIF
007400030325     C*
007500100610     C                   WRITE     TIVAW000
007600030325     C                   ENDIF
007700110909     C*
007800110909     C* - DESCRIZIONI CAMPO - ITALIANO
007900110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
008000110909     C                   EVAL      VAWDTA  =
008100131025     C                             '"ANNO AP. GIACENZA"'                 +ChCSV+
008200131025     C                             '"MESE/GIORNO AP. GIACENZA"'          +ChCSV+
008300131025     C                             '"FILIALE AP. GIACENZA"'              +ChCSV+
008400110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
008500110909     C                             '"CAUSALE GIACENZA"'                  +ChCSV+
008600110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
008700110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
008800131025     C                             '"AMAZON TRACKING ID"'                +ChCSV+
008900131025     C                             '"AMAZON ENCRYPTED SHIPMENT ID"'      +ChCSV+
009000131025     C                             '"DATA ULTIMA AP. GIACENZA"'          +ChCSV
009100110909     C*
009200110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009300110909     C                   IF        %subst(prmppt:3:1) = 'D'
009400110909     C                   EVAL      VAWDTA  =
009500131025     C                             %trim(VAWDTA)                         +
009600131025     C                             '"DESTINATARIO"'                      +ChCSV+
009700131025     C                             '"INDIRIZZO DEST."'                   +ChCSV+
009800131025     C                             '"CAP DEST."'                         +ChCSV+
009900131025     C                             '"LOCALITA DEST."'                    +ChCSV+
010000131025     C                             '"PROVINCIA DEST."'                   +ChCSV+
010100131025     C                             '"NAZIONE DEST."'                     +ChCSV+
010200131025     C                             '"TELEFONO DEST."'                    +ChCSV
010300110909     C                   ENDIF
010400110909     C*
010500110909     C                   WRITE     TIVAW000
010600110909     C                   ENDIF
010700991027     C*
010800050908     C                   READ      TIVGDTMP
010900050908     C                   DOW       not %eof(TIVGDTMP)
011000050908     C                   eval      fnvagds = vgdDTA
011100991027     C*
011200030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
011300060411     C***  '"':''''      XLATE     fnvagds       fnvagds
011400040628     C*
011500111007     C* Chaino la bolla relativa alla giacenza corrente.
011600050722     C                   CLEAR                   dstas
011700050722     C                   CLEAR                   dsar5
011800050722     C                   CLEAR                   dar5gen
011900040628     C     KEYTAS30_1P   CHAIN     TITAS30C
012000040628     C                   IF        %found(TITAS30C)
012100131025     C*
012200131025     C* Se nei parametri di input richiesto filtro per specifiche causali giacenza
012300131025     C                   SETOFF                                       40
012400131025     C                   SELECT
012500131025     C                   WHEN      %subst(prmppt:5:1) = 'F' AND                 * sì filtro
012600131025     C                             (VAGCMC = '021' OR
012700131025     C                              VAGCMC = '022' OR
012800131025     C                              VAGCMC = '028')
012900131025     C                   SETON                                        40
013000131025     C                   WHEN      %subst(prmppt:5:1) = *blanks                 * no filtro
013100141202     C                   SETON                                        40
013200131025     C                   ENDSL
013300131025     C*
013400131025     C                   IF        *IN40
013500111004     C*
013600050722     C                   EVAL      AR5TRD = 'GEN'
013700050722     C     KEYAR531_1P   CHAIN     FIAR531C
013800050722     C                   IF        %found(FIAR531C)
013900040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
014000060411     C***  '"':''''      XLATE     dstas         dstas
014100060411     C***  '"':''''      XLATE     dsar5         dsar5
014200050722     C                   EVAL      dar5gen = ar5UNI
014300040628     C                   ENDIF
014400050722     C                   ENDIF
014500030325     C*
014600040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
014700040204     C                   IF        %subst(prmppt:2:1) = 'D'
014800111003     C                   EVAL      VAWDTA  =
014900080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
015000080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
015100080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
015200080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
015300080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
015400080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
015500080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
015600080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
015700080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
015800111003     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV
015900080918     C*
016000040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
016100040628     C                   IF        %subst(prmppt:3:1) = 'D'
016200131025     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +
016300080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
016400080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
016500080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
016600080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
016700080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
016800080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
016900131025     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV
017000080918     C                   ENDIF
017100040628     C*
017200040204     C                   ELSE
017300040204     C*
017400111003     C                   EVAL      VAWDTA  =
017500080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
017600080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
017700080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
017800080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
017900080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
018000080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
018100080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
018200080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
018300080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
018400111003     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV
018500040628     C*
018600040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
018700040628     C                   IF        %subst(prmppt:3:1) = 'D'
018800111003     C                   EVAL      VAWDTA  =
018900131025     C                                              %trim(VAWDTA)    +
019000080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
019100080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
019200080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
019300080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
019400080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
019500080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
019600131025     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV
019700080918     C                   ENDIF
019800080918     C*
019900040204     C                   ENDIF
020000930409     C*
020100100610     C                   WRITE     TIVAW000
020200131025     C                   ENDIF
020300131025     C*
020400050908     C                   DELETE    TIVGD000
020500991027     C*
020600050908     C                   READ      TIVGDTMP
020700030325     C                   ENDDO
020800991027     C*
020900030325     C                   EVAL      wrkesito = '0'
021000991027     C*
021100910830     C                   ENDSR
021200991027
021300991027      /TITLE Operazioni iniziali.
021400991027     C     *inzsr        BEGSR
021500991027     C*
021600991027     C     *ENTRY        PLIST
021700040628     C                   PARM                    prmppt
021800040628     C     wrkesito      PARM      wrkesito      prmesito
021900991027     C*
022000040628     C* DEFINIZIONE CHIAVI
022100040628     C*
022200040628     C* Chiave su TITAS30C - Parziale
022300040628     C     KEYTAS30_1P   KLIST
022400040628     C                   KFLD                    VAGAAS
022500040628     C                   KFLD                    VAGLNP
022600040628     C                   KFLD                    VAGNRS
022700040628     C                   KFLD                    VAGNSP
022800050722     C*
022900050722     C* Chiave su FIAR531C - Parziale
023000050722     C     KEYAR531_1P   KLIST
023100050722     C                   KFLD                    VAGAAS
023200050722     C                   KFLD                    VAGLNP
023300050722     C                   KFLD                    VAGNRS
023400050722     C                   KFLD                    VAGNSP
023500050722     C                   KFLD                    AR5TRD
023600040628     C*
023700991027     C                   ENDSR
