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
001700160208     D DTA4A         E DS
001800040628
001900040628     D dstas           ds
002000040628     D  tasRSD
002100040628     D  tasIND
002200040628     D  tasCAD
002300040628     D  tasLOD
002400040628     D  tasPRD
002500040628     D  tasNZD
002600131105     D  tasNAS
002700050722
002800050722     D dsar5           ds
002900050722     D  ar5UNI
003000160208
003100160208     D*--------------------
003200160208     D* DEFINIZIONI A PROCEDURE ESTERNE
003300160208     D*--------------------
003400160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
003500160208
003600991027
003700030908     C                   EXSR      chkRECFILMBR
003800991027     C                   EXSR      traduci
003900160208     C*
004000160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
004100160208     C                   CALL      'UBTA400R'
004200160208     C                   PARM                    UBTA4IOPZ
004300160208     C                   PARM      'C'           UBTA4ITLA
004400160208     C                   PARM                    UBTA4IAAS
004500160208     C                   PARM                    UBTA4ILNP
004600160208     C                   PARM                    UBTA4INRS
004700160208     C                   PARM                    UBTA4INSP
004800160208     C                   PARM                    UBTA4ITRC
004900160208     C                   PARM                    UBTA4OERR
005000160208     C                   PARM                    UBTA4ODS
005100160208     C                   PARM                    UBTA4OLEN
005200160208     C                   PARM                    UBTA4ODATI
005300160208     C*
005400921023     C                   SETON                                        LR
005500030908
005600030908     C     chkRECFILMBR  BEGSR
005700030908     C*
005800100610     C                   READ      TIVAWWWT                               55
005900030908     C*
006000030908     C                   ENDSR
006100991027
006200991027     C     traduci       BEGSR
006300080918     C*
006400080918     C* Verifico se richiesto 1 particolare carattere separatore campo
006500080923     C                   IF        %subst(prmppt:4:1) = *blanks
006600100610     C                   EVAL      ChCSV = ';'
006700080918     C                   ELSE
006800080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
006900080918     C                   ENDIF
007000030325     C*
007100030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007200110909     C*
007300110909     C* - NOMI CAMPO
007400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007500111003     C                   EVAL      VAWDTA  =
007600080918     C                                       '"VAGAGC"'+ChCSV+
007700080918     C                                       '"VAGMGC"'+ChCSV+
007800080918     C                                       '"VAGNGC"'+ChCSV+
007900080918     C                                       '"VAGDMC"'+ChCSV+
008000080918     C                                       '"VAGNOT"'+ChCSV+
008100080918     C                                       '"VAGRMN"'+ChCSV+
008200080918     C                                       '"VAGRMA"'+ChCSV+
008300111003     C                                       '"VAGDUR"'+ChCSV
008400040628     C*
008500040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
008600040628     C                   IF        %subst(prmppt:3:1) = 'D'
008700131025     C                   EVAL      VAWDTA  = %trim(VAWDTA)+
008800080918     C                                           '"VAGRSD"'+ChCSV+
008900080918     C                                           '"VAGIND"'+ChCSV+
009000080918     C                                           '"VAGCAD"'+ChCSV+
009100080918     C                                           '"VAGLOD"'+ChCSV+
009200080918     C                                           '"VAGPRD"'+ChCSV+
009300131105     C                                           '"VAGTEL"'+ChCSV+
009400131105     C                                           '"VAGORD"'+ChCSV
009500040628     C                   ENDIF
009600030325     C*
009700100610     C                   WRITE     TIVAW000
009800030325     C                   ENDIF
009900110909     C*
010000110909     C* - DESCRIZIONI CAMPO - ITALIANO
010100110909     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
010200110909     C                   EVAL      VAWDTA  =
010300110909     C                             '"ANNO APERTURA GIACENZA"'            +ChCSV+
010400110909     C                             '"MESE/GIORNO APERTURA GIACENZA"'     +ChCSV+
010500110909     C                             '"NUMERO GIACENZA"'                   +ChCSV+
010600110909     C                             '"DESCRIZIONE CAUSALE GIACENZA"'      +ChCSV+
010700110909     C                             '"ULTERIORE MOTIVO DI GIACENZA"'      +ChCSV+
010800110909     C                             '"RIFERIM.MITTENTE NUMERICO"'         +ChCSV+
010900110909     C                             '"RIFERIM.MITTENTE ALFABETICO"'       +ChCSV+
011000111003     C                             '"ULTIMA DATA RIAPERT.GIACENZA"'      +ChCSV
011100110909     C*
011200110909     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
011300110909     C                   IF        %subst(prmppt:3:1) = 'D'
011400110909     C                   EVAL      VAWDTA  =
011500131025     C                             %trim(VAWDTA)                         +
011600110909     C                             '"RAGIONE SOCIALE DESTINATARIO"'      +ChCSV+
011700110909     C                             '"INDIRIZZO DESTINATARIO"'            +ChCSV+
011800110909     C                             '"CAP DESTINATARIO"'                  +ChCSV+
011900110909     C                             '"LOCALITA DESTINATARIO"'             +ChCSV+
012000110909     C                             '"PROVINCIA DESTINATARIO"'            +ChCSV+
012100131105     C                             '"TELEFONO DESTINATARIO"'             +ChCSV+
012200131105     C                             '"ORDER NBR."'                        +ChCSV
012300110909     C                   ENDIF
012400110909     C*
012500110909     C                   WRITE     TIVAW000
012600110909     C                   ENDIF
012700991027     C*
012800050908     C                   READ      TIVGDTMP
012900050908     C                   DOW       not %eof(TIVGDTMP)
013000050908     C                   eval      fnvagds = vgdDTA
013100991027     C*
013200030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
013300060411     C***  '"':''''      XLATE     fnvagds       fnvagds
013400040628     C*
013500111007     C* Chaino la bolla relativa alla giacenza corrente.
013600050722     C                   CLEAR                   dstas
013700050722     C                   CLEAR                   dsar5
013800050722     C                   CLEAR                   dar5gen
013900040628     C     KEYTAS30_1P   CHAIN     TITAS30C
014000040628     C                   IF        %found(TITAS30C)
014100160208     C*
014200160208     C* Reperisco NATURA MERCE
014300160208     C                   EXSR      RTVNAS
014400111004     C*
014500111004     C                   z-add     *zeros        wCAS             13 3
014600131025     C*
014700131025     C* Se nei parametri di input richiesto filtro per SOLO GIAC APERTE / SOLO GIAC CHIUSE
014800131025     C* ... gestisco
014900131025     C                   SETOFF                                       40
015000131025     C                   SELECT
015100131025     C                   WHEN      %subst(prmppt:5:1) = 'A' AND                 * solo aperte
015200131025     C                             vagDCG = *zeros
015300131025     C                   SETON                                        40
015400131025     C                   WHEN      %subst(prmppt:5:1) = 'C' AND                 * solo chiuse
015500131025     C                             vagDCG > *zeros
015600131025     C                   SETON                                        40
015700131025     C                   WHEN      %subst(prmppt:5:1) = *blanks                 * tutte
015800131025     C                   SETON                                        40
015900131025     C                   ENDSL
016000150325     C*
016100150325     C* Se nei parametri di input richiesto filtro per NO "DANNI"
016200150325     C* ... gestisco
016300150325     C                   SETOFF                                       41
016400150325     C                   SELECT
016500150325     C                   WHEN      %subst(prmppt:6:1) = 'D' AND                 * solo NO "DANNI"
016600150325     C                             vagNGC = *zeros
016700150325     C                   SETON                                        41
016800150325     C                   ENDSL
016900131025     C*
017000150325     C                   IF        *IN40 and not *IN41
017100111004     C*
017200111004     C* Quindi verifico subito esistenza contrassegno
017300111004     C     KEYcsb03_C    CHAIN     tncsb03l
017400111004     C                   IF        %found(tncsb03l)
017500111004     C                   eval      wCAS = csbCAS
017600111004     C                   ENDIF
017700111004     C*
017800050722     C                   EVAL      AR5TRD = 'GEN'
017900050722     C     KEYAR531_1P   CHAIN     FIAR531C
018000050722     C                   IF        %found(FIAR531C)
018100040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
018200060411     C***  '"':''''      XLATE     dstas         dstas
018300060411     C***  '"':''''      XLATE     dsar5         dsar5
018400050722     C                   EVAL      dar5gen = ar5UNI
018500040628     C                   ENDIF
018600050722     C                   ENDIF
018700030325     C*
018800040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
018900040204     C                   IF        %subst(prmppt:2:1) = 'D'
019000111003     C                   EVAL      VAWDTA  =
019100080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
019200080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
019300080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
019400080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
019500080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
019600080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
019700080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
019800111003     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV
019900080918     C*
020000040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
020100040628     C                   IF        %subst(prmppt:3:1) = 'D'
020200131025     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +
020300080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
020400080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
020500080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
020600080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
020700080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
020800131105     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV+
020900160208     C                                          '"'+%trim(§TA4ANAS)+'"'+ChCSV
021000080918     C                   ENDIF
021100040628     C*
021200040204     C                   ELSE
021300040204     C*
021400111003     C                   EVAL      VAWDTA  =
021500080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
021600080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
021800080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
021900080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
022000080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
022100080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
022200111003     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV
022300040628     C*
022400040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
022500040628     C                   IF        %subst(prmppt:3:1) = 'D'
022600111003     C                   EVAL      VAWDTA  =
022700131025     C                                              %trim(VAWDTA)    +
022800080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
022900080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
023000080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
023100080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
023200080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
023300131105     C                                        '"'+%trim(§AR5TELD)+'"'+ChCSV+
023400160208     C                                          '"'+%trim(§TA4ANAS)+'"'+ChCSV
023500080918     C                   ENDIF
023600080918     C*
023700040204     C                   ENDIF
023800930409     C*
023900100610     C                   WRITE     TIVAW000
024000131025     C                   ENDIF
024100131025     C*
024200050908     C                   DELETE    TIVGD000
024300991027     C*
024400050908     C                   READ      TIVGDTMP
024500030325     C                   ENDDO
024600991027     C*
024700030325     C                   EVAL      wrkesito = '0'
024800991027     C*
024900910830     C                   ENDSR
025000160208
025100160208
025200160208
025300160208     C     RTVNAS        BEGSR
025400160208     C*
025500160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
025600160208     C                   CLEAR                   DTA4A
025700160208     C*
025800160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
025900160208     C                   CALL      'UBTA400R'
026000160208     C                   PARM      *blanks       UBTA4IOPZ
026100160208     C                   PARM      *blanks       UBTA4ITLA
026200160208     C                   PARM      tasAAS        UBTA4IAAS
026300160208     C                   PARM      tasLNP        UBTA4ILNP
026400160208     C                   PARM      tasNRS        UBTA4INRS
026500160208     C                   PARM      tasNSP        UBTA4INSP
026600160208     C                   PARM      'A'           UBTA4ITRC
026700160208     C                   PARM                    UBTA4OERR
026800160208     C                   PARM                    UBTA4ODS
026900160208     C                   PARM                    UBTA4OLEN
027000160208     C                   PARM                    UBTA4ODATI
027100160208     C*
027200160208     C                   IF        UBTA4OERR = *zeros
027300160208     C                   SELECT
027400160208     C* Gestione output tipo record 'A'
027500160208     C                   WHEN      UBTA4ODS = 'DTA4A'
027600160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
027700160208     C                   ENDSL
027800160208     C*
027900160208     C                   ENDIF
028000160208     C*
028100160208     C                   ENDSR
028200160208
028300160208
028400991027
028500991027      /TITLE Operazioni iniziali.
028600991027     C     *inzsr        BEGSR
028700991027     C*
028800991027     C     *ENTRY        PLIST
028900040628     C                   PARM                    prmppt
029000040628     C     wrkesito      PARM      wrkesito      prmesito
029100991027     C*
029200040628     C* DEFINIZIONE CHIAVI
029300040628     C*
029400040628     C* Chiave su TITAS30C - Parziale
029500040628     C     KEYTAS30_1P   KLIST
029600040628     C                   KFLD                    VAGAAS
029700040628     C                   KFLD                    VAGLNP
029800040628     C                   KFLD                    VAGNRS
029900040628     C                   KFLD                    VAGNSP
030000111004     C*
030100111004     C* Chiave su TNCSB03L - Completa
030200111004     C     KEYcsb03_C    KLIST
030300111004     C                   KFLD                    VAGAAS
030400111004     C                   KFLD                    VAGLNP
030500111004     C                   KFLD                    VAGNRS
030600111004     C                   KFLD                    VAGNSP
030700050722     C*
030800050722     C* Chiave su FIAR531C - Parziale
030900050722     C     KEYAR531_1P   KLIST
031000050722     C                   KFLD                    VAGAAS
031100050722     C                   KFLD                    VAGLNP
031200050722     C                   KFLD                    VAGNRS
031300050722     C                   KFLD                    VAGNSP
031400050722     C                   KFLD                    AR5TRD
031500040628     C*
031600991027     C                   ENDSR
