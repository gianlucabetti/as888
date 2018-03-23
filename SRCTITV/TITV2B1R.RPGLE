000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050722     FFIAR531C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700100610     FTIVAWWWT  UF A E             DISK    commit
000800101108     Ftigcp52l  if   e           k disk
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
004900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005000100610     C                   EVAL      VAWDTA  = '"VAGLNP"'+ChCSV+
005100080918     C                                       '"VAGAAS"'+ChCSV+
005200080918     C                                       '"VAGNRS"'+ChCSV+
005300080918     C                                       '"VAGNSP"'+ChCSV+
005400080918     C                                       '"VAGLNA"'+ChCSV+
005500080918     C                                       '"VAGAGC"'+ChCSV+
005600080918     C                                       '"VAGMGC"'+ChCSV+
005700080918     C                                       '"VAGFGC"'+ChCSV+
005800080918     C                                       '"VAGNGC"'+ChCSV+
005900080918     C                                       '"VAGFRG"'+ChCSV+
006000080918     C                                       '"VAGGGA"'+ChCSV+
006100080918     C                                       '"VAGCMC"'+ChCSV+
006200080918     C                                       '"VAGDMC"'+ChCSV+
006300080918     C                                       '"VAGNOT"'+ChCSV+
006400080918     C                                       '"VAGRMN"'+ChCSV+
006500080918     C                                       '"VAGRMA"'+ChCSV+
006600080918     C                                       '"VAGSCM"'+ChCSV+
006700080918     C                                       '"VAGDMM"'+ChCSV+
006800080918     C                                       '"VAGVCS"'+ChCSV+
006900080918     C                                       '"VAGCAS"'+ChCSV+
007000080918     C                                       '"VAGVCA"'+ChCSV+
007100080918     C                                       '"VAGDDM"'+ChCSV+
007200080918     C                                       '"VAGDED"'+ChCSV+
007300080918     C                                       '"VAGDUR"'+ChCSV+
007400080918     C                                       '"VAGDCG"'+ChCSV+
007500080918     C                                       '"VAGCFG"'+ChCSV+
007600080918     C                                       '"VAGSGS"'+ChCSV+
007700080918     C                                       '"VAGSGR"'+ChCSV+
007800080918     C                                       '"VAGSGD"'+ChCSV+
007900080918     C                                       '"VAGDSD"'+ChCSV+
008000080918     C                                       '"VAGDSA"'+ChCSV+
008100080918     C                                       '"VAGGGS"'+ChCSV+
008200080918     C                                       '"VAGSGF"'+ChCSV+
008300080918     C                                       '"VAGSGA"'+ChCSV+
008400080918     C                                       '"VAGTFT"'+ChCSV+
008500080918     C                                       '"VAGDBG"'+ChCSV+
008600080918     C                                       '"VAGLPW"'+ChCSV+
008700080918     C                                       '"VAGASW"'+ChCSV+
008800080918     C                                       '"VAGNSW"'+ChCSV+
008900080918     C                                       '"VAGNBW"'+ChCSV+
009000030331     C                                       '"VAGKSC"'
009100040628     C*
009200040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009300040628     C                   IF        %subst(prmppt:3:1) = 'D'
009400100610     C                   EVAL      VAWDTA  = %trim(VAWDTA) +ChCSV+
009500080918     C                                           '"VAGRSD"'+ChCSV+
009600080918     C                                           '"VAGIND"'+ChCSV+
009700080918     C                                           '"VAGCAD"'+ChCSV+
009800080918     C                                           '"VAGLOD"'+ChCSV+
009900080918     C                                           '"VAGPRD"'+ChCSV+
010000080918     C                                           '"VAGNZD"'+ChCSV+
010100080918     C                                           '"VAGTEL"'
010200040628     C                   ENDIF
010300030325     C*
010400100610     C                   WRITE     TIVAW000
010500030325     C                   ENDIF
010600991027     C*
010700050908     C                   READ      TIVGDTMP
010800050908     C                   DOW       not %eof(TIVGDTMP)
010900050908     C                   eval      fnvagds = vgdDTA
011000991027     C*
011100030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
011200060411     C***  '"':''''      XLATE     fnvagds       fnvagds
011300101108     C
011400101108     C                   SETOFF                                       56
011500101108     C*
011600101108     C* Se richiesto nei parametri => solo giacenze in attesa d disposizioni
011700101108     C                   if        %subst(prmppt:5:1) = 'A'
011800101108     C                   eval      gcpFRG = vagFRG
011900101108     C     KEYGCP52_C    chain     tigcp52l
012000101108     C                   if        %found(tigcp52l)
012100170125     C***                if        gcpfas >= 020 AND
012200170125     C                   if        gcpfas >= 015 AND
012300110316     C                             gcpfas <  025
012400101108     C                   else
012500101108     C                   seton                                        56
012600101108     C                   endif
012700101108     C                   endif
012800101108     C                   endif
012900101108     C*
013000101108     C* Se tutto ok => procedo
013100101108     C                   if        *in56 = *off
013200040628     C*
013300040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
013400040628     C* giacenza corrente.
013500040628     C                   IF        %subst(prmppt:3:1) = 'D'
013600050722     C                   CLEAR                   dstas
013700050722     C                   CLEAR                   dsar5
013800050722     C                   CLEAR                   dar5gen
013900040628     C     KEYTAS30_1P   CHAIN     TITAS30C
014000040628     C                   IF        %found(TITAS30C)
014100050722     C                   EVAL      AR5TRD = 'GEN'
014200050722     C     KEYAR531_1P   CHAIN     FIAR531C
014300050722     C                   IF        %found(FIAR531C)
014400040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
014500060411     C***  '"':''''      XLATE     dstas         dstas
014600060411     C***  '"':''''      XLATE     dsar5         dsar5
014700050722     C                   EVAL      dar5gen = ar5UNI
014800040628     C                   ENDIF
014900050722     C                   ENDIF
015000040628     C                   ENDIF
015100030325     C*
015200040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
015300040204     C                   IF        %subst(prmppt:2:1) = 'D'
015400100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
015500080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
015600080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
015700080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
015800080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
015900080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
016000080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
016100080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
016200080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
016300080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
016400080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
016500080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
016600080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
016700080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
016800080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
016900080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
017000080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
017100080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
017200080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
017300080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
017400080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
017500080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
017600080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
017700080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
017800080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
017900080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
018000080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
018100080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
018200080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
018300080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
018400080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
018500080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
018600080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
018700080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
018800080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
018900080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
019000080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
019100080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
019200080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
019300080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
019400080918     C                                       %trim(%editc(VAGKSC:'Q'))
019500080918     C*
019600040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
019700040628     C                   IF        %subst(prmppt:3:1) = 'D'
019800100610     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
019900080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
020000080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
020100080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
020200080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
020300080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
020400080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
020500080918     C                                        '"'+%trim(§AR5TELD)+'"'
020600080918     C                   ENDIF
020700040628     C*
020800040204     C                   ELSE
020900040204     C*
021000100610     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
021100080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
021200080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
021300080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
021400080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
021500080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
021600080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
021800080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
021900080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
022000080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
022100080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
022200080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
022300080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
022400080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
022500080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
022600080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
022700080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
022800080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
022900080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
023000080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
023100080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
023200080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
023300080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
023400080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
023500080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
023600080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
023700080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
023800080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
023900080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
024000080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
024100080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
024200080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
024300080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
024400080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
024500080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
024600080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
024700080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
024800080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
024900080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
025000030331     C                                       %trim(%editc(VAGKSC:'Q'))
025100040628     C*
025200040628     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
025300040628     C                   IF        %subst(prmppt:3:1) = 'D'
025400100702     C                   EVAL      VAWDTA  =        %trim(VAWDTA)    +ChCSV+
025500080918     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
025600080918     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
025700080918     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
025800080918     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
025900080918     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
026000080918     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
026100080918     C                                        '"'+%trim(§AR5TELD)+'"'
026200080918     C                   ENDIF
026300080918     C*
026400040204     C                   ENDIF
026500930409     C*
026600100610     C                   WRITE     TIVAW000
026700101108     C*
026800101108     C                   endif
026900101108     C*
027000050908     C                   DELETE    TIVGD000
027100991027     C*
027200050908     C                   READ      TIVGDTMP
027300030325     C                   ENDDO
027400991027     C*
027500030325     C                   EVAL      wrkesito = '0'
027600991027     C*
027700910830     C                   ENDSR
027800991027
027900101108
028000991027      /TITLE Operazioni iniziali.
028100991027     C     *inzsr        BEGSR
028200991027     C*
028300991027     C     *ENTRY        PLIST
028400040628     C                   PARM                    prmppt
028500040628     C     wrkesito      PARM      wrkesito      prmesito
028600991027     C*
028700040628     C* DEFINIZIONE CHIAVI
028800040628     C*
028900040628     C* Chiave su TITAS30C - Parziale
029000040628     C     KEYTAS30_1P   KLIST
029100040628     C                   KFLD                    VAGAAS
029200040628     C                   KFLD                    VAGLNP
029300040628     C                   KFLD                    VAGNRS
029400040628     C                   KFLD                    VAGNSP
029500050722     C*
029600050722     C* Chiave su FIAR531C - Parziale
029700050722     C     KEYAR531_1P   KLIST
029800050722     C                   KFLD                    VAGAAS
029900050722     C                   KFLD                    VAGLNP
030000050722     C                   KFLD                    VAGNRS
030100050722     C                   KFLD                    VAGNSP
030200050722     C                   KFLD                    AR5TRD
030300101108     C*
030400101108     C* Chiave su TIGCP52L - Completa
030500101108     C     KEYGCP52_C    klist
030600101108     C                   kfld                    VAGAGC
030700101108     C                   kfld                    VAGFGC
030800101108     C                   kfld                    VAGNGC
030900101108     C                   kfld                    GCPFRG
031000040628     C*
031100991027     C                   ENDSR
