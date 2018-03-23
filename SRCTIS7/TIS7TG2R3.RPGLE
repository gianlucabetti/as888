000100120619     H DECEDIT('0.') DATEDIT(*DMY.)
000200120619     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120619     FTABEL00F  IF   E           K DISK
000500040628     FTITAS30C  IF   E           K DISK
000600050722     FFIAR531C  IF   E           K DISK
000700050908     FTIVGDTMP  UF   E             DISK
000800120619     FTIVAWWWT  UF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300050908     D fnvagds       e ds                  extname(fnvag00t)
001400080918     D ChCSV           s             10A   varying inz(' ')
001500030325
001600050722     D dar5gen       e ds
001700120619     D dsPR          e ds
001800120619     D ds14          e ds
001900040628
002000040628     D dstas           ds
002100040628     D  tasRSD
002200040628     D  tasIND
002300040628     D  tasCAD
002400040628     D  tasLOD
002500040628     D  tasPRD
002600040628     D  tasNZD
002700050722
002800050722     D dsar5           ds
002900050722     D  ar5UNI
003000991027
003100030908     C                   EXSR      chkRECFILMBR
003200991027     C                   EXSR      traduci
003300921023     C                   SETON                                        LR
003400030908
003500030908     C     chkRECFILMBR  BEGSR
003600030908     C*
003700120619     C                   READ      TIVAWWWT                               55
003800030908     C*
003900030908     C                   ENDSR
004000991027
004100991027     C     traduci       BEGSR
004200080918     C*
004300080918     C* Verifico se richiesto 1 particolare carattere separatore campo
004400080923     C                   IF        %subst(prmppt:4:1) = *blanks
004500080918     C                   EVAL      ChCSV = ','
004600080918     C                   ELSE
004700080923     C                   EVAL      ChCSV = %trim(%subst(prmppt:4:1))
004800080918     C                   ENDIF
004900030325     C*
005000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005200120619     C                   EVAL      VAWDTA  = '"VAGLNP"'+ChCSV+
005300080918     C                                       '"VAGAAS"'+ChCSV+
005400080918     C                                       '"VAGNRS"'+ChCSV+
005500080918     C                                       '"VAGNSP"'+ChCSV+
005600080918     C                                       '"VAGLNA"'+ChCSV+
005700080918     C                                       '"VAGAGC"'+ChCSV+
005800080918     C                                       '"VAGMGC"'+ChCSV+
005900080918     C                                       '"VAGFGC"'+ChCSV+
006000080918     C                                       '"VAGNGC"'+ChCSV+
006100080918     C                                       '"VAGFRG"'+ChCSV+
006200080918     C                                       '"VAGGGA"'+ChCSV+
006300080918     C                                       '"VAGCMC"'+ChCSV+
006400080918     C                                       '"VAGDMC"'+ChCSV+
006500080918     C                                       '"VAGNOT"'+ChCSV+
006600080918     C                                       '"VAGRMN"'+ChCSV+
006700080918     C                                       '"VAGRMA"'+ChCSV+
006800080918     C                                       '"VAGSCM"'+ChCSV+
006900080918     C                                       '"VAGDMM"'+ChCSV+
007000080918     C                                       '"VAGVCS"'+ChCSV+
007100080918     C                                       '"VAGCAS"'+ChCSV+
007200080918     C                                       '"VAGVCA"'+ChCSV+
007300080918     C                                       '"VAGDDM"'+ChCSV+
007400080918     C                                       '"VAGDED"'+ChCSV+
007500080918     C                                       '"VAGDUR"'+ChCSV+
007600080918     C                                       '"VAGDCG"'+ChCSV+
007700080918     C                                       '"VAGCFG"'+ChCSV+
007800080918     C                                       '"VAGSGS"'+ChCSV+
007900080918     C                                       '"VAGSGR"'+ChCSV+
008000080918     C                                       '"VAGSGD"'+ChCSV+
008100080918     C                                       '"VAGDSD"'+ChCSV+
008200080918     C                                       '"VAGDSA"'+ChCSV+
008300080918     C                                       '"VAGGGS"'+ChCSV+
008400080918     C                                       '"VAGSGF"'+ChCSV+
008500080918     C                                       '"VAGSGA"'+ChCSV+
008600080918     C                                       '"VAGTFT"'+ChCSV+
008700080918     C                                       '"VAGDBG"'+ChCSV+
008800080918     C                                       '"VAGLPW"'+ChCSV+
008900080918     C                                       '"VAGASW"'+ChCSV+
009000080918     C                                       '"VAGNSW"'+ChCSV+
009100080918     C                                       '"VAGNBW"'+ChCSV+
009200030331     C                                       '"VAGKSC"'
009300040628     C*
009400040628     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche i campi del destinatario
009500040628     C                   IF        %subst(prmppt:3:1) = 'D'
009600120619     C                   EVAL      VAWDTA =  %trim(VAWDTA)+ChCSV+
009700120619     C                                          '"VAGRSD"'+ChCSV+
009800120619     C                                          '"VAGIND"'+ChCSV+
009900120619     C                                          '"VAGCAD"'+ChCSV+
010000120619     C                                          '"VAGLOD"'+ChCSV+
010100120619     C                                          '"VAGPRD"'+ChCSV+
010200120619     C                                          '"VAGNZD"'+ChCSV+
010300120619     C                                          '"VAGTEL"'
010400040628     C                   ENDIF
010500120619     C*
010600120619     C* Se richiesto nei parametri aggingo nelle intestazioni colonne anche il campo della regione
010700120619     C                   IF        %subst(prmppt:5:1) = 'R'
010800120619     C                   EVAL      VAWDTA =  %trim(VAWDTA)+ChCSV+
010900120619     C                                          '"VAGREG"'
011000120619     C                   ENDIF
011100030325     C*
011200120619     C                   WRITE     TIVAW000
011300030325     C                   ENDIF
011400991027     C*
011500050908     C                   READ      TIVGDTMP
011600050908     C                   DOW       not %eof(TIVGDTMP)
011700050908     C                   eval      fnvagds = vgdDTA
011800991027     C*
011900030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012000060411     C***  '"':''''      XLATE     fnvagds       fnvagds
012100040628     C*
012200040628     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
012300040628     C* giacenza corrente.
012400040628     C                   IF        %subst(prmppt:3:1) = 'D'
012500050722     C                   CLEAR                   dstas
012600050722     C                   CLEAR                   dsar5
012700050722     C                   CLEAR                   dar5gen
012800040628     C     KEYTAS30_1P   CHAIN     TITAS30C
012900040628     C                   IF        %found(TITAS30C)
013000050722     C                   EVAL      AR5TRD = 'GEN'
013100050722     C     KEYAR531_1P   CHAIN     FIAR531C
013200050722     C                   IF        %found(FIAR531C)
013300040628     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
013400060411     C***  '"':''''      XLATE     dstas         dstas
013500060411     C***  '"':''''      XLATE     dsar5         dsar5
013600050722     C                   EVAL      dar5gen = ar5UNI
013700040628     C                   ENDIF
013800050722     C                   ENDIF
013900040628     C                   ENDIF
014000120619     C*
014100120619     C* Se richiesto nei parametri anche il campo della regione reperisco i dato dalle tabelle
014200120619     C                   IF        %subst(prmppt:5:1) = 'R'
014300120619     C                   CLEAR                   dsPR
014400120619     C                   CLEAR                   ds14
014500120619     C     KEYTAS30_1P   CHAIN     TITAS30C
014600120619     C                   IF        %found(TITAS30C)
014700120619     C                   EVAL      tblKUT = 1
014800120619     C                   EVAL      tblCOD = 'PR'
014900120619     C                   EVAL      tblKEY = tasPRD
015000120619     C     KEYTAB00_C    CHAIN     TABEL00F                           30
015100120619     C                   IF        %found(TABEL00F)
015200120619     C                   EVAL      dsPR = tblUNI
015300120619     C                   EVAL      tblCOD = '14'
015400120619     C                   EVAL      tblKEY = §PRCRE
015500120619     C     KEYTAB00_C    CHAIN     TABEL00F                           30
015600120619     C                   IF        %found(TABEL00F)
015700120619     C                   EVAL      ds14 = tblUNI
015800120619     C                   ENDIF
015900120619     C                   ENDIF
016000120619     C                   ENDIF
016100120619     C                   ENDIF
016200030325     C*
016300040204     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
016400040204     C                   IF        %subst(prmppt:2:1) = 'D'
016500120619     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
016600080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
016700080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
016800080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
016900080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
017000080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
017100080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
017200080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
017300080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
017400080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
017500080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
017600080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
017700080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
017800080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
017900080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
018000080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
018100080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
018200080918     C                              %trim(%editw(VAGDMM:'    /  /  '))+ChCSV+
018300080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
018400080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
018500080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
018600080918     C                              %trim(%editw(VAGDDM:'    /  /  '))+ChCSV+
018700080918     C                              %trim(%editw(VAGDED:'    /  /  '))+ChCSV+
018800080918     C                              %trim(%editw(VAGDUR:'    /  /  '))+ChCSV+
018900080918     C                              %trim(%editw(VAGDCG:'    /  /  '))+ChCSV+
019000080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
019100080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
019200080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
019300080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
019400080918     C                              %trim(%editw(VAGDSD:'    /  /  '))+ChCSV+
019500080918     C                              %trim(%editw(VAGDSA:'    /  /  '))+ChCSV+
019600080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
019700080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
019800080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
019900080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
020000080918     C                              %trim(%editw(VAGDBG:'    /  /  '))+ChCSV+
020100080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
020200080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
020300080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
020400080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
020500080918     C                                       %trim(%editc(VAGKSC:'Q'))
020600040628     C*
020700040204     C                   ELSE
020800040204     C*
020900120619     C                   EVAL      VAWDTA =  %trim(%editc(VAGLNP:'Q'))+ChCSV+
021000080918     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
021100080918     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
021200080918     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
021300080918     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
021400080918     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
021500080918     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
021600080918     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
021700080918     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
021800080918     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
021900080918     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
022000080918     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
022100080918     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
022200080918     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
022300080918     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
022400080918     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
022500080918     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
022600080918     C                                       %trim(%editc(VAGDMM:'Q'))+ChCSV+
022700080918     C                                           '"'+%trim(VAGVCS)+'"'+ChCSV+
022800080918     C                                       %trim(%editc(VAGCAS:'Q'))+ChCSV+
022900080918     C                                           '"'+%trim(VAGVCA)+'"'+ChCSV+
023000080918     C                                       %trim(%editc(VAGDDM:'Q'))+ChCSV+
023100080918     C                                       %trim(%editc(VAGDED:'Q'))+ChCSV+
023200080918     C                                       %trim(%editc(VAGDUR:'Q'))+ChCSV+
023300080918     C                                       %trim(%editc(VAGDCG:'Q'))+ChCSV+
023400080918     C                                           '"'+%trim(VAGCFG)+'"'+ChCSV+
023500080918     C                                       %trim(%editc(VAGSGS:'Q'))+ChCSV+
023600080918     C                                       %trim(%editc(VAGSGR:'Q'))+ChCSV+
023700080918     C                                       %trim(%editc(VAGSGD:'Q'))+ChCSV+
023800080918     C                                       %trim(%editc(VAGDSD:'Q'))+ChCSV+
023900080918     C                                       %trim(%editc(VAGDSA:'Q'))+ChCSV+
024000080918     C                                       %trim(%editc(VAGGGS:'Q'))+ChCSV+
024100080918     C                                       %trim(%editc(VAGSGF:'Q'))+ChCSV+
024200080918     C                                       %trim(%editc(VAGSGA:'Q'))+ChCSV+
024300080918     C                                       %trim(%editc(VAGTFT:'Q'))+ChCSV+
024400080918     C                                       %trim(%editc(VAGDBG:'Q'))+ChCSV+
024500080918     C                                       %trim(%editc(VAGLPW:'Q'))+ChCSV+
024600080918     C                                       %trim(%editc(VAGASW:'Q'))+ChCSV+
024700080918     C                                       %trim(%editc(VAGNSW:'Q'))+ChCSV+
024800080918     C                                       %trim(%editc(VAGNBW:'Q'))+ChCSV+
024900030331     C                                       %trim(%editc(VAGKSC:'Q'))
025000080918     C*
025100040204     C                   ENDIF
025200120619     C*
025300120619     C* Se richiesto nei parametri aggiungo anche i campi del destinatario
025400120619     C                   IF        %subst(prmppt:3:1) = 'D'
025500120619     C                   EVAL      VAWDTA =             %trim(VAWDTA)+ChCSV+
025600120619     C                                          '"'+%trim(TASRSD)+'"'+ChCSV+
025700120619     C                                          '"'+%trim(TASIND)+'"'+ChCSV+
025800120619     C                                          '"'+%trim(TASCAD)+'"'+ChCSV+
025900120619     C                                          '"'+%trim(TASLOD)+'"'+ChCSV+
026000120619     C                                          '"'+%trim(TASPRD)+'"'+ChCSV+
026100120619     C                                          '"'+%trim(TASNZD)+'"'+ChCSV+
026200120619     C                                        '"'+%trim(§AR5TELD)+'"'
026300120619     C                   ENDIF
026400120619     C*
026500120619     C* Se richiesto nei parametri aggiungo anche i campi della regione
026600120619     C                   IF        %subst(prmppt:5:1) = 'R'
026700120619     C                   EVAL      VAWDTA =             %trim(VAWDTA)+ChCSV+
026800120619     C                                          '"'+%trim(§14DES)+'"'
026900120619     C                   ENDIF
027000930409     C*
027100120619     C                   WRITE     TIVAW000
027200050908     C                   DELETE    TIVGD000
027300991027     C*
027400050908     C                   READ      TIVGDTMP
027500030325     C                   ENDDO
027600991027     C*
027700030325     C                   EVAL      wrkesito = '0'
027800991027     C*
027900910830     C                   ENDSR
028000991027
028100991027      /TITLE Operazioni iniziali.
028200991027     C     *inzsr        BEGSR
028300991027     C*
028400991027     C     *ENTRY        PLIST
028500040628     C                   PARM                    prmppt
028600040628     C     wrkesito      PARM      wrkesito      prmesito
028700991027     C*
028800040628     C* DEFINIZIONE CHIAVI
028900040628     C*
029000040628     C* Chiave su TITAS30C - Parziale
029100040628     C     KEYTAS30_1P   KLIST
029200040628     C                   KFLD                    VAGAAS
029300040628     C                   KFLD                    VAGLNP
029400040628     C                   KFLD                    VAGNRS
029500040628     C                   KFLD                    VAGNSP
029600050722     C*
029700050722     C* Chiave su FIAR531C - Parziale
029800050722     C     KEYAR531_1P   KLIST
029900050722     C                   KFLD                    VAGAAS
030000050722     C                   KFLD                    VAGLNP
030100050722     C                   KFLD                    VAGNRS
030200050722     C                   KFLD                    VAGNSP
030300050722     C                   KFLD                    AR5TRD
030400120619     C*
030500120619     C* Chiave su TABEL00F - Completa
030600120619     C     KEYTAB00_C    KLIST
030700120619     C                   KFLD                    tblKUT
030800120619     C                   KFLD                    tblCOD
030900120619     C                   KFLD                    tblKEY
031000040628     C*
031100991027     C                   ENDSR
