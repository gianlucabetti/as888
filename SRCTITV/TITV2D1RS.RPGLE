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
013100110310     C*
013200110310     C* Forzatura x LEVIS => RMN sempre lungo 6 digits
013300110310     C                   MOVE(p)   VAGRMN        wRMN              6
013400030325     C*
013500040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
013600040204     C                   IF        %subst(prmppt:2:1) = 'D'
013700080918     C                   EVAL      VAGXDTA = %trim(%editc(VAGLNP:'Q'))+ChCSV+
013800080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
013900080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
014000080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
014100080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
014200080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
014300080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
014400080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
014500080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
014600080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
014700080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
014800080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
014900080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
015000080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
015100110310     C***                                    %trim(%editc(VAGRMN:'Q'))+ChCSV+
015200110310     C                                         '"'+%trim(wRMN)  +'"'  +ChCSV+
015300080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
015400080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
015500080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
015600080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
015700080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
015800080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
015900080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
016000080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
016100080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
016200080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
016300080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
016400080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
016500080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
016600080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
016700080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
016800080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
016900080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
017000080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
017100080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
017200080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
017300080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
017400080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
017500080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
017600080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
017700080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
017800080918     C                                       %trim(%editc(VAGKSC:'Q'))
017900080918     C*
018000040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
018100040628     C                   IF        %subst(prmppt:3:1) = 'D'
018200080918     C                   EVAL      VAGXDTA =           %trim(VAGXDTA)+ChCSV+
018300080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
018400080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
018500080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
018600080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
018700080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
018800080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
018900080918     C                                        '"'+%trim(§AR5TELD)+'"'
019000080918     C                   ENDIF
019100040628     C*
019200040204     C                   ELSE
019300040204     C*
019400080918     C                   EVAL      VAGXDTA = %trim(%editc(VAGLNP:'Q'))+ChCSV+
019500080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
019600080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
019700080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
019800080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
019900080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
020000080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
020100080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
020200080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
020300080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
020400080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
020500080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
020600080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
020700080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
020800110310     C***                                    %trim(%editc(VAGRMN:'Q'))+ChCSV+
020900110310     C                                         '"'+%trim(wRMN)  +'"'  +ChCSV+
021000080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
021100080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
021200080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
021300080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
021400080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
021500080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
021600080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
021800080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
021900080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
022000080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
022100080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
022200080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
022300080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
022400080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
022500080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
022600080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
022700080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
022800080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
022900080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
023000080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
023100080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
023200080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
023300080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
023400080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
023500030331     C                                       %trim(%editc(VAGKSC:'Q'))
023600040628     C*
023700040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
023800040628     C                   IF        %subst(prmppt:3:1) = 'D'
023900040628     C                   EVAL      VAGXDTA = %trim(VAGXDTA) + ',' +
024000080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
024100080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
024200080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
024300080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
024400080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
024500080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
024600080918     C                                        '"'+%trim(§AR5TELD)+'"'
024700080918     C                   ENDIF
024800080918     C*
024900040204     C                   ENDIF
025000930409     C*
025100030331     C                   WRITE     FNVAGX00
025200050908     C                   DELETE    TIVGD000
025300991027     C*
025400050908     C                   READ      TIVGDTMP
025500030325     C                   ENDDO
025600991027     C*
025700030325     C                   EVAL      wrkesito = '0'
025800991027     C*
025900910830     C                   ENDSR
026000991027
026100991027      /TITLE Operazioni iniziali.
026200991027     C     *inzsr        BEGSR
026300991027     C*
026400991027     C     *ENTRY        PLIST
026500040628     C                   PARM                    prmppt
026600040628     C     wrkesito      PARM      wrkesito      prmesito
026700991027     C*
026800040628     C* DEFINIZIONE CHIAVI
026900040628     C*
027000040628     C* Chiave su TITAS30C - Parziale
027100040628     C     KEYTAS30_1P   KLIST
027200040628     C                   KFLD                    VAGAAS
027300040628     C                   KFLD                    VAGLNP
027400040628     C                   KFLD                    VAGNRS
027500040628     C                   KFLD                    VAGNSP
027600050722     C*
027700050722     C* Chiave su FIAR531C - Parziale
027800050722     C     KEYAR531_1P   KLIST
027900050722     C                   KFLD                    VAGAAS
028000050722     C                   KFLD                    VAGLNP
028100050722     C                   KFLD                    VAGNRS
028200050722     C                   KFLD                    VAGNSP
028300050722     C                   KFLD                    AR5TRD
028400040628     C*
028500991027     C                   ENDSR
