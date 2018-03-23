000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400050908     FTIVGDTMP  UF   E             DISK
000500101018     FTIVAWWWT  UF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000050908     D fnvagds       e ds                  extname(fnvag00t)
001100080918     D ChCSV           s             10A   varying inz(' ')
001200030325
001300101018
001400101018
001500030908     C                   EXSR      chkRECFILMBR
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800101018
001900130304
002000101018
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300101018     C                   READ      TIVAW000                               55
002400030908     C*
002500030908     C                   ENDSR
002600130304
002700101018
002800991027
002900991027     C     traduci       BEGSR
003000080918     C*
003100080918     C* Verifico se richiesto 1 particolare carattere separatore campo
003200101018     C                   IF        %subst(prmppt:2:1) = *blanks
003300080918     C                   EVAL      ChCSV = ','
003400080918     C                   ELSE
003500101018     C                   EVAL      ChCSV = %trim(%subst(prmppt:2:1))
003600080918     C                   ENDIF
003700030325     C*
003800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004000101018     C                   EVAL      VAWDTA  =
004100130304     C                                       '"VAGLNP"'                  +ChCSV+
004200130304     C                                       '"VAGAAS"'                  +ChCSV+
004300130304     C                                       '"VAGNRS"'                  +ChCSV+
004400130304     C                                       '"VAGNSP"'                  +ChCSV+
004500130304     C                                       '"VAGLNA"'                  +ChCSV+
004600130304     C                                       '"VAGAGC"'                  +ChCSV+
004700130304     C                                       '"VAGMGC"'                  +ChCSV+
004800130304     C                                       '"VAGFGC"'                  +ChCSV+
004900130304     C                                       '"VAGNGC"'                  +ChCSV+
005000130304     C                                       '"VAGFRG"'                  +ChCSV+
005100130304     C                                       '"VAGGGA"'                  +ChCSV+
005200130304     C                                       '"VAGCMC"'                  +ChCSV+
005300130304     C                                       '"VAGDMC"'                  +ChCSV+
005400130304     C                                       '"VAGNOT"'                  +ChCSV+
005500130304     C                                       '"VAGRMN"'                  +ChCSV+
005600130304     C                                       '"VAGRMA"'                  +ChCSV+
005700130304     C                                       '"VAGSCM"'                  +ChCSV+
005800130304     C                                       '        '                  +ChCSV+
005900130304     C                                       '"VARDMC"'                  +ChCSV
006000030325     C*
006100101018     C                   WRITE     TIVAW000
006200030325     C                   ENDIF
006300991027     C*
006400050908     C                   READ      TIVGDTMP
006500050908     C                   DOW       not %eof(TIVGDTMP)
006600050908     C                   eval      fnvagds = vgdDTA
006700130304     C*
006800130304     C                   EVAL      VAWDTA  = %trim(%editc(VAGLNP:'Q'))+ChCSV+
006900130304     C                                       %trim(%editc(VAGAAS:'Q'))+ChCSV+
007000130304     C                                       %trim(%editc(VAGNRS:'Q'))+ChCSV+
007100130304     C                                       %trim(%editc(VAGNSP:'Q'))+ChCSV+
007200130304     C                                       %trim(%editc(VAGLNA:'Q'))+ChCSV+
007300130304     C                                       %trim(%editc(VAGAGC:'Q'))+ChCSV+
007400130304     C                                       %trim(%editc(VAGMGC:'Q'))+ChCSV+
007500130304     C                                       %trim(%editc(VAGFGC:'Q'))+ChCSV+
007600130304     C                                       %trim(%editc(VAGNGC:'Q'))+ChCSV+
007700130304     C                                       %trim(%editc(VAGFRG:'Q'))+ChCSV+
007800130304     C                                           '"'+%trim(VAGGGA)+'"'+ChCSV+
007900130304     C                                           '"'+%trim(VAGCMC)+'"'+ChCSV+
008000130304     C                                           '"'+%trim(VAGDMC)+'"'+ChCSV+
008100130304     C                                           '"'+%trim(VAGNOT)+'"'+ChCSV+
008200130304     C                                       %trim(%editc(VAGRMN:'Q'))+ChCSV+
008300130304     C                                           '"'+%trim(VAGRMA)+'"'+ChCSV+
008400130304     C                                       %trim(%editc(VAGSCM:'Q'))+ChCSV+
008500130304     C                                                          ':::>'+ChCSV+
008600130304     C                                                              ''+ChCSV
008700930409     C*
008800101018     C                   WRITE     TIVAW000
008900050908     C                   DELETE    TIVGD000
009000991027     C*
009100050908     C                   READ      TIVGDTMP
009200030325     C                   ENDDO
009300991027     C*
009400030325     C                   EVAL      wrkesito = '0'
009500991027     C*
009600910830     C                   ENDSR
009700991027
009800101018
009900991027      /TITLE Operazioni iniziali.
010000991027     C     *inzsr        BEGSR
010100991027     C*
010200991027     C     *ENTRY        PLIST
010300040628     C                   PARM                    prmppt
010400040628     C     wrkesito      PARM      wrkesito      prmesito
010500040628     C*
010600991027     C                   ENDSR
