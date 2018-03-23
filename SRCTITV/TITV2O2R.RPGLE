000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400130712     FTITAS30C  IF   E           K DISK
000500050908     FTIVGDTMP  UF   E             DISK
000600101018     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100050908     D fnvagds       e ds                  extname(fnvag00t)
001200080918     D ChCSV           s             10A   varying inz(' ')
001300030325
001400101018
001500101018
001600030908     C                   EXSR      chkRECFILMBR
001700991027     C                   EXSR      traduci
001800921023     C                   SETON                                        LR
001900101018
002000130304
002100101018
002200030908     C     chkRECFILMBR  BEGSR
002300030908     C*
002400101018     C                   READ      TIVAW000                               55
002500030908     C*
002600030908     C                   ENDSR
002700130304
002800101018
002900991027
003000991027     C     traduci       BEGSR
003100080918     C*
003200080918     C* Verifico se richiesto 1 particolare carattere separatore campo
003300101018     C                   IF        %subst(prmppt:2:1) = *blanks
003400080918     C                   EVAL      ChCSV = ','
003500080918     C                   ELSE
003600101018     C                   EVAL      ChCSV = %trim(%subst(prmppt:2:1))
003700080918     C                   ENDIF
003800030325     C*
003900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004100101018     C                   EVAL      VAWDTA  =
004200130304     C                                       '"VAGLNP"'                  +ChCSV+
004300130304     C                                       '"VAGAAS"'                  +ChCSV+
004400130304     C                                       '"VAGNRS"'                  +ChCSV+
004500130304     C                                       '"VAGNSP"'                  +ChCSV+
004600130304     C                                       '"VAGLNA"'                  +ChCSV+
004700130304     C                                       '"VAGAGC"'                  +ChCSV+
004800130304     C                                       '"VAGMGC"'                  +ChCSV+
004900130304     C                                       '"VAGFGC"'                  +ChCSV+
005000130304     C                                       '"VAGNGC"'                  +ChCSV+
005100130304     C                                       '"VAGFRG"'                  +ChCSV+
005200130304     C                                       '"VAGGGA"'                  +ChCSV+
005300130304     C                                       '"VAGCMC"'                  +ChCSV+
005400130304     C                                       '"VAGDMC"'                  +ChCSV+
005500130304     C                                       '"VAGNOT"'                  +ChCSV+
005600130304     C                                       '"VAGRMN"'                  +ChCSV+
005700130304     C                                       '"VAGRMA"'                  +ChCSV+
005800130304     C                                       '"VAGSCM"'                  +ChCSV+
005900130712     C
006000130712     C                                       '"VAGRSD"'                  +ChCSV+
006100130712     C                                       '"VAGIND"'                  +ChCSV+
006200130712     C                                       '"VAGCAD"'                  +ChCSV+
006300130712     C                                       '"VAGLOD"'                  +ChCSV+
006400130712     C                                       '"VAGPRD"'                  +ChCSV+
006500130712     C                                       '"VAGNZD"'                  +ChCSV+
006600130712     C
006700130304     C                                       '        '                  +ChCSV+
006800130304     C                                       '"VARDMC"'                  +ChCSV
006900030325     C*
007000101018     C                   WRITE     TIVAW000
007100030325     C                   ENDIF
007200991027     C*
007300050908     C                   READ      TIVGDTMP
007400050908     C                   DOW       not %eof(TIVGDTMP)
007500050908     C                   eval      fnvagds = vgdDTA
007600130712     C*
007700130712     C     KEYTAS30_1P   CHAIN     TITAS30C
007800130712     C                   IF        %found(TITAS30C)
007900130304     C*
008000130304     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
008100130304     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
008200130304     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
008300130304     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
008400130304     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
008500130304     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
008600130304     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
008700130304     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
008800130304     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
008900130304     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
009000130304     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
009100130304     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
009200130304     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
009300130304     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
009400130304     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
009500130304     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
009600130304     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
009700130712     C                                           '"'+%trim(TASRSD)+'"'+ChCSV+
009800130712     C                                           '"'+%trim(TASIND)+'"'+ChCSV+
009900130712     C                                           '"'+%trim(TASCAD)+'"'+ChCSV+
010000130712     C                                           '"'+%trim(TASLOD)+'"'+ChCSV+
010100130712     C                                           '"'+%trim(TASPRD)+'"'+ChCSV+
010200130712     C                                           '"'+%trim(TASNZD)+'"'+ChCSV+
010300130304     C                                                          ':::>'+ChCSV+
010400130304     C                                                              ''+ChCSV
010500130712     C                   ENDIF
010600930409     C*
010700101018     C                   WRITE     TIVAW000
010800050908     C                   DELETE    TIVGD000
010900991027     C*
011000050908     C                   READ      TIVGDTMP
011100030325     C                   ENDDO
011200991027     C*
011300030325     C                   EVAL      wrkesito = '0'
011400991027     C*
011500910830     C                   ENDSR
011600991027
011700101018
011800991027      /TITLE Operazioni iniziali.
011900991027     C     *inzsr        BEGSR
012000991027     C*
012100991027     C     *ENTRY        PLIST
012200040628     C                   PARM                    prmppt
012300040628     C     wrkesito      PARM      wrkesito      prmesito
012400130712     C*
012500130712     C* DEFINIZIONE CHIAVI
012600130712     C*
012700130712     C* Chiave su TITAS30C - Parziale
012800130712     C     KEYTAS30_1P   KLIST
012900130712     C                   KFLD                    VAGAAS
013000130712     C                   KFLD                    VAGLNP
013100130712     C                   KFLD                    VAGNRS
013200130712     C                   KFLD                    VAGNSP
013300040628     C*
013400991027     C                   ENDSR
