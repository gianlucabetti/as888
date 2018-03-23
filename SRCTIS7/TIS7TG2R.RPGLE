000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700090323     FFNVAGXLS  UF A E             DISK    commit
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
003400030908     C                   READ      FNVAGXLS                               55
003500030908     C*
003600030908     C                   ENDSR
003700991027
003800991027     C     traduci       BEGSR
003900080918     C*
004000080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004100080923     C                   IF        %subst(prmppt:4:1) = *blanks
004200080918     C                   EVAL      ChCSV = ','
004300080918     C                   ELSE
004400080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004500080918     C                   ENDIF
004600030325     C*
004700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004900080918     C                   EVAL      VAGXDTA = '"VAGLNP"'+ChCSV+
005000080918     C                                       '"VAGAAS"'+ChCSV+
005100080918     C                                       '"VAGNRS"'+ChCSV+
005200080918     C                                       '"VAGNSP"'+ChCSV+
005300080918     C                                       '"VAGLNA"'+ChCSV+
005400080918     C                                       '"VAGAGC"'+ChCSV+
005500080918     C                                       '"VAGMGC"'+ChCSV+
005600080918     C                                       '"VAGFGC"'+ChCSV+
005700080918     C                                       '"VAGNGC"'+ChCSV+
005800080918     C                                       '"VAGFRG"'+ChCSV+
005900080918     C                                       '"VAGGGA"'+ChCSV+
006000080918     C                                       '"VAGCMC"'+ChCSV+
006100080918     C                                       '"VAGDMC"'+ChCSV+
006200080918     C                                       '"VAGNOT"'+ChCSV+
006300080918     C                                       '"VAGRMN"'+ChCSV+
006400080918     C                                       '"VAGRMA"'+ChCSV+
006500080918     C                                       '"VAGSCM"'+ChCSV+
006600080918     C                                       '"VAGDMM"'+ChCSV+
006700080918     C                                       '"VAGVCS"'+ChCSV+
006800080918     C                                       '"VAGCAS"'+ChCSV+
006900080918     C                                       '"VAGVCA"'+ChCSV+
007000080918     C                                       '"VAGDDM"'+ChCSV+
007100080918     C                                       '"VAGDED"'+ChCSV+
007200080918     C                                       '"VAGDUR"'+ChCSV+
007300080918     C                                       '"VAGDCG"'+ChCSV+
007400080918     C                                       '"VAGCFG"'+ChCSV+
007500080918     C                                       '"VAGSGS"'+ChCSV+
007600080918     C                                       '"VAGSGR"'+ChCSV+
007700080918     C                                       '"VAGSGD"'+ChCSV+
007800080918     C                                       '"VAGDSD"'+ChCSV+
007900080918     C                                       '"VAGDSA"'+ChCSV+
008000080918     C                                       '"VAGGGS"'+ChCSV+
008100080918     C                                       '"VAGSGF"'+ChCSV+
008200080918     C                                       '"VAGSGA"'+ChCSV+
008300080918     C                                       '"VAGTFT"'+ChCSV+
008400080918     C                                       '"VAGDBG"'+ChCSV+
008500080918     C                                       '"VAGLPW"'+ChCSV+
008600080918     C                                       '"VAGASW"'+ChCSV+
008700080918     C                                       '"VAGNSW"'+ChCSV+
008800080918     C                                       '"VAGNBW"'+ChCSV+
008900030331     C                                       '"VAGKSC"'
009000040628     C*
009100040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009200040628     C                   IF        %subst(prmppt:3:1) = 'D'
009300080918     C                   EVAL      VAGXDTA = %trim(VAGXDTA)+ChCSV+
009400080918     C                                           '"VAGRSD"'+ChCSV+
009500080918     C                                           '"VAGIND"'+ChCSV+
009600080918     C                                           '"VAGCAD"'+ChCSV+
009700080918     C                                           '"VAGLOD"'+ChCSV+
009800080918     C                                           '"VAGPRD"'+ChCSV+
009900080918     C                                           '"VAGNZD"'+ChCSV+
010000080918     C                                           '"VAGTEL"'
010100040628     C                   ENDIF
010200030325     C*
010300030331     C                   WRITE     FNVAGX00
010400030325     C                   ENDIF
010500991027     C*
010600050908     C                   READ      TIVGDTMP
010700050908     C                   DOW       not %eof(TIVGDTMP)
010800050908     C                   eval      fnvagds = vgdDTA
010900991027     C*
011000030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
011100060411     C***  '"':''''      XLATE     fnvagds       fnvagds
011200040628     C*
011300040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
011400040628     C* giacenza corrente.
011500040628     C                   IF        %subst(prmppt:3:1) = 'D'
011600050722     C                   CLEAR                   dstas
011700050722     C                   CLEAR                   dsar5
011800050722     C                   CLEAR                   dar5gen
011900040628     C     KEYTAS30_1P   CHAIN     TITAS30C
012000040628     C                   IF        %found(TITAS30C)
012100050722     C                   EVAL      AR5TRD = 'GEN'
012200050722     C     KEYAR531_1P   CHAIN     FIAR531C
012300050722     C                   IF        %found(FIAR531C)
012400040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012500060411     C***  '"':''''      XLATE     dstas         dstas
012600060411     C***  '"':''''      XLATE     dsar5         dsar5
012700050722     C                   EVAL      dar5gen = ar5UNI
012800040628     C                   ENDIF
012900050722     C                   ENDIF
013000040628     C                   ENDIF
013100030325     C*
013200040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
013300040204     C                   IF        %subst(prmppt:2:1) = 'D'
013400080918     C                   EVAL      VAGXDTA = %trim(%editc(VAGLNP:'Q'))+ChCSV+
013500080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
013600080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
013700080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
013800080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
013900080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
014000080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
014100080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
014200080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
014300080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
014400080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
014500080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
014600080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
014700080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
014800080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
014900080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
015000080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
015100080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
015200080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
015300080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
015400080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
015500080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
015600080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
015700080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
015800080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
015900080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
016000080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
016100080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
016200080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
016300080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
016400080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
016500080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
016600080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
016700080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
016800080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
016900080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
017000080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
017100080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
017200080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
017300080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
017400080918     C                                       %trim(%editc(VAGKSC:'Q'))
017500080918     C*
017600040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
017700040628     C                   IF        %subst(prmppt:3:1) = 'D'
017800080918     C                   EVAL      VAGXDTA =           %trim(VAGXDTA)+ChCSV+
017900080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
018000080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
018100080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
018200080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
018300080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
018400080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
018500080918     C                                        '"'+%trim(§AR5TELD)+'"'
018600080918     C                   ENDIF
018700040628     C*
018800040204     C                   ELSE
018900040204     C*
019000080918     C                   EVAL      VAGXDTA = %trim(%editc(VAGLNP:'Q'))+ChCSV+
019100080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
019200080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
019300080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
019400080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
019500080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
019600080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
019700080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
019800080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
019900080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
020000080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
020100080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
020200080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
020300080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
020400080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
020500080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
020600080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
020700080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
020800080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
020900080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
021000080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
021100080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
021200080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
021300080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
021400080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
021500080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
021600080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
021800080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
021900080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
022000080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
022100080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
022200080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
022300080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
022400080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
022500080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
022600080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
022700080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
022800080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
022900080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
023000030331     C                                       %trim(%editc(VAGKSC:'Q'))
023100040628     C*
023200040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
023300040628     C                   IF        %subst(prmppt:3:1) = 'D'
023400040628     C                   EVAL      VAGXDTA = %trim(VAGXDTA) + ',' +
023500080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
023600080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
023700080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
023800080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
023900080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
024000080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
024100080918     C                                        '"'+%trim(§AR5TELD)+'"'
024200080918     C                   ENDIF
024300080918     C*
024400040204     C                   ENDIF
024500930409     C*
024600030331     C                   WRITE     FNVAGX00
024700050908     C                   DELETE    TIVGD000
024800991027     C*
024900050908     C                   READ      TIVGDTMP
025000030325     C                   ENDDO
025100991027     C*
025200030325     C                   EVAL      wrkesito = '0'
025300991027     C*
025400910830     C                   ENDSR
025500991027
025600991027      /TITLE Operazioni iniziali.
025700991027     C     *inzsr        BEGSR
025800991027     C*
025900991027     C     *ENTRY        PLIST
026000040628     C                   PARM                    prmppt
026100040628     C     wrkesito      PARM      wrkesito      prmesito
026200991027     C*
026300040628     C* DEFINIZIONE CHIAVI
026400040628     C*
026500040628     C* Chiave su TITAS30C - Parziale
026600040628     C     KEYTAS30_1P   KLIST
026700040628     C                   KFLD                    VAGAAS
026800040628     C                   KFLD                    VAGLNP
026900040628     C                   KFLD                    VAGNRS
027000040628     C                   KFLD                    VAGNSP
027100050722     C*
027200050722     C* Chiave su FIAR531C - Parziale
027300050722     C     KEYAR531_1P   KLIST
027400050722     C                   KFLD                    VAGAAS
027500050722     C                   KFLD                    VAGLNP
027600050722     C                   KFLD                    VAGNRS
027700050722     C                   KFLD                    VAGNSP
027800050722     C                   KFLD                    AR5TRD
027900040628     C*
028000991027     C                   ENDSR
