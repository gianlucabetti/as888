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
002500131105     D  tasNAS
002600050722
002700050722     D dsar5           ds
002800050722     D  ar5UNI
002900991027
003000030908     C                   EXSR      chkRECFILMBR
003100991027     C                   EXSR      traduci
003200921023     C                   SETON                                        LR
003300030908
003400030908     C     chkRECFILMBR  BEGSR
003500030908     C*
003600100610     C                   READ      TIVAWWWT                               55
003700030908     C*
003800030908     C                   ENDSR
003900991027
004000991027     C     traduci       BEGSR
004100080918     C*
004200080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004300080923     C                   IF        %subst(prmppt:4:1) = *blanks
004400100610     C                   EVAL      ChCSV = ';'
004500080918     C                   ELSE
004600080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004700080918     C                   ENDIF
004800030325     C*
004900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005000110909     C*
005100110909     C* - NOMI CAMPO
005200030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005300111003     C                   EVAL      VAWDTA  =
005400080918     C                                       '"VAGAGC"'+ChCSV+
005500080918     C                                       '"VAGMGC"'+ChCSV+
005600080918     C                                       '"VAGNGC"'+ChCSV+
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
007100131105     C                                           '"VAGTEL"'+ChCSV+
007200131105     C                                           '"VAGORD"'+ChCSV
007300040628     C                   ENDIF
007400030325     C*
007500100610     C                   WRITE     TIVAW000
007600030325     C                   ENDIF
007700110909     C*
007800110909     C* - DESCRIZIONI CAMPO - ITALIANO
007900110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
008000110909     C                   EVAL      VAWDTA  =
008100110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
008200110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
008300110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
008400110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
008500110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
008600110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
008700110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
008800111003     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV
008900110909     C*
009000110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009100110909     C                   IF        %subst(prmppt:3:1) = 'D'
009200110909     C                   EVAL      VAWDTA  =
009300131025     C                             %trim(VAWDTA)                         +
009400110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
009500110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
009600110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
009700110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
009800110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
009900131105     C                             '"TELEFONO DESTINATARIO"'             +ChCSV+
010000131105     C                             '"ORDER NBR."'                        +ChCSV
010100110909     C                   ENDIF
010200110909     C*
010300110909     C                   WRITE     TIVAW000
010400110909     C                   ENDIF
010500991027     C*
010600050908     C                   READ      TIVGDTMP
010700050908     C                   DOW       not %eof(TIVGDTMP)
010800050908     C                   eval      fnvagds = vgdDTA
010900991027     C*
011000030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
011100060411     C***  '"':''''      XLATE     fnvagds       fnvagds
011200040628     C*
011300111007     C* Chaino la bolla relativa alla giacenza corrente.
011400050722     C                   CLEAR                   dstas
011500050722     C                   CLEAR                   dsar5
011600050722     C                   CLEAR                   dar5gen
011700040628     C     KEYTAS30_1P   CHAIN     TITAS30C
011800040628     C                   IF        %found(TITAS30C)
011900111004     C*
012000111004     C                   z-add     *zeros        wCAS             13 3
012100131025     C*
012200131025     C* Se nei parametri di input richiesto filtro per SOLO GIAC APERTE / SOLO GIAC CHIUSE
012300131025     C* ... gestisco
012400131025     C                   SETOFF                                       40
012500131025     C                   SELECT
012600131025     C                   WHEN      %subst(prmppt:5:1) = 'A' AND                 * solo aperte
012700131025     C                             vagDCG = *zeros
012800131025     C                   SETON                                        40
012900131025     C                   WHEN      %subst(prmppt:5:1) = 'C' AND                 * solo chiuse
013000131025     C                             vagDCG > *zeros
013100131025     C                   SETON                                        40
013200131025     C                   WHEN      %subst(prmppt:5:1) = *blanks                 * tutte
013300131025     C                   SETON                                        40
013400131025     C                   ENDSL
013500150325     C*
013600150325     C* Se nei parametri di input richiesto filtro per NO "DANNI"
013700150325     C* ... gestisco
013800150325     C                   SETOFF                                       41
013900150325     C                   SELECT
014000150325     C                   WHEN      %subst(prmppt:6:1) = 'D' AND                 * solo NO "DANNI"
014100150325     C                             vagNGC = *zeros
014200150325     C                   SETON                                        41
014300150325     C                   ENDSL
014400131025     C*
014500150325     C                   IF        *IN40 and not *IN41
014600111004     C*
014700111004     C* Quindi verifico subito esistenza contrassegno
014800111004     C     KEYcsb03_C    CHAIN     tncsb03l
014900111004     C                   IF        %found(tncsb03l)
015000111004     C                   eval      wCAS = csbCAS
015100111004     C                   ENDIF
015200111004     C*
015300050722     C                   EVAL      AR5TRD = 'GEN'
015400050722     C     KEYAR531_1P   CHAIN     FIAR531C
015500050722     C                   IF        %found(FIAR531C)
015600040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
015700060411     C***  '"':''''      XLATE     dstas         dstas
015800060411     C***  '"':''''      XLATE     dsar5         dsar5
015900050722     C                   EVAL      dar5gen = ar5UNI
016000040628     C                   ENDIF
016100050722     C                   ENDIF
016200030325     C*
016300040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
016400040204     C                   IF        %subst(prmppt:2:1) = 'D'
016500111003     C                   EVAL      VAWDTA  =
016600080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
016700080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
016800080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
016900080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
017000080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
017100080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
017200080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
017300111003     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV
017400080918     C*
017500040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
017600040628     C                   IF        %subst(prmppt:3:1) = 'D'
017700131025     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +
017800080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
017900080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
018000080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
018100080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
018200080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
018300131105     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV+
018400131105     C                                          '"'+%trim(TASNAS)+'"'+ChCSV
018500080918     C                   ENDIF
018600040628     C*
018700040204     C                   ELSE
018800040204     C*
018900111003     C                   EVAL      VAWDTA  =
019000080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
019100080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
019200080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
019300080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
019400080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
019500080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
019600080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
019700111003     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV
019800040628     C*
019900040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
020000040628     C                   IF        %subst(prmppt:3:1) = 'D'
020100111003     C                   EVAL      VAWDTA  =
020200131025     C                                              %trim(VAWDTA)    +
020300080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
020400080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
020500080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
020600080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
020700080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
020800131105     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV+
020900131105     C                                          '"'+%trim(TASNAS)+'"'+ChCSV
021000080918     C                   ENDIF
021100080918     C*
021200040204     C                   ENDIF
021300930409     C*
021400100610     C                   WRITE     TIVAW000
021500131025     C                   ENDIF
021600131025     C*
021700050908     C                   DELETE    TIVGD000
021800991027     C*
021900050908     C                   READ      TIVGDTMP
022000030325     C                   ENDDO
022100991027     C*
022200030325     C                   EVAL      wrkesito = '0'
022300991027     C*
022400910830     C                   ENDSR
022500991027
022600991027      /TITLE Operazioni iniziali.
022700991027     C     *inzsr        BEGSR
022800991027     C*
022900991027     C     *ENTRY        PLIST
023000040628     C                   PARM                    prmppt
023100040628     C     wrkesito      PARM      wrkesito      prmesito
023200991027     C*
023300040628     C* DEFINIZIONE CHIAVI
023400040628     C*
023500040628     C* Chiave su TITAS30C - Parziale
023600040628     C     KEYTAS30_1P   KLIST
023700040628     C                   KFLD                    VAGAAS
023800040628     C                   KFLD                    VAGLNP
023900040628     C                   KFLD                    VAGNRS
024000040628     C                   KFLD                    VAGNSP
024100111004     C*
024200111004     C* Chiave su TNCSB03L - Completa
024300111004     C     KEYcsb03_C    KLIST
024400111004     C                   KFLD                    VAGAAS
024500111004     C                   KFLD                    VAGLNP
024600111004     C                   KFLD                    VAGNRS
024700111004     C                   KFLD                    VAGNSP
024800050722     C*
024900050722     C* Chiave su FIAR531C - Parziale
025000050722     C     KEYAR531_1P   KLIST
025100050722     C                   KFLD                    VAGAAS
025200050722     C                   KFLD                    VAGLNP
025300050722     C                   KFLD                    VAGNRS
025400050722     C                   KFLD                    VAGNSP
025500050722     C                   KFLD                    AR5TRD
025600040628     C*
025700991027     C                   ENDSR
