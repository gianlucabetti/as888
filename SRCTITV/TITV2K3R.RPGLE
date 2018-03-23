000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400040628     FTITAS30C  IF   E           K DISK
000500050908     FTIVGDTMP  UF   E             DISK
000600101018     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100050908     D fnvagds       e ds                  extname(fnvag00t)
001200080918     D ChCSV           s             10A   varying inz(' ')
001300101018
001400101018     D wRSD            s                   like(tasRSD)
001500030325
001600101018
001700101018
001800030908     C                   EXSR      chkRECFILMBR
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
002100101018
002200101018
002300030908     C     chkRECFILMBR  BEGSR
002400030908     C*
002500101018     C                   READ      TIVAW000                               55
002600030908     C*
002700030908     C                   ENDSR
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
004200120420     C                                       '"DDT"'                     +ChCSV+
004300120420     C                                       '"DESTINATARIO"'            +ChCSV+
004400120420     C                                       '"MOTIVO"'                  +ChCSV+
004500120420     C                                       '"ISTRUZIONI"'              +ChCSV+
004600120420     C                                       '"DATA SVINCOLO"'           +ChCSV
004700030325     C*
004800101018     C                   WRITE     TIVAW000
004900030325     C                   ENDIF
005000991027     C*
005100050908     C                   READ      TIVGDTMP
005200050908     C                   DOW       not %eof(TIVGDTMP)
005300050908     C                   eval      fnvagds = vgdDTA
005400120420     C*
005500120420     C                   CLEAR                   wRSD
005600101018     C*
005700101018     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
005800101018     C* giacenza corrente.
005900101018     C     KEYTAS30_1P   CHAIN     TITAS30C
006000040628     C                   IF        %found(TITAS30C)
006100101018     C                   EVAL      wRSD = tasRSD
006200040628     C                   ENDIF
006300030325     C*
006400101018     C                   EVAL      VAWDTA  =
006500120420     C                                       %trim(%editc(VAGRMN:'X'))   +ChCSV+
006600120420     C                                             '"'+%trim(wRSD)+'"'   +ChCSV+
006700120420     C                             '"'+%trim(VAGDMC)+' '+%trim(VAGNOT)   +ChCSV+
006800120420     C                                                           '" "'   +ChCSV+
006900120420     C                                                           '" "'   +ChCSV
007000930409     C*
007100101018     C                   WRITE     TIVAW000
007200050908     C                   DELETE    TIVGD000
007300991027     C*
007400050908     C                   READ      TIVGDTMP
007500030325     C                   ENDDO
007600991027     C*
007700030325     C                   EVAL      wrkesito = '0'
007800991027     C*
007900910830     C                   ENDSR
008000991027
008100101018
008200991027      /TITLE Operazioni iniziali.
008300991027     C     *inzsr        BEGSR
008400991027     C*
008500991027     C     *ENTRY        PLIST
008600040628     C                   PARM                    prmppt
008700040628     C     wrkesito      PARM      wrkesito      prmesito
008800991027     C*
008900040628     C* DEFINIZIONE CHIAVI
009000040628     C*
009100040628     C* Chiave su TITAS30C - Parziale
009200120420     C     KEYTAS30_1P   KLIST
009300040628     C                   KFLD                    VAGAAS
009400040628     C                   KFLD                    VAGLNP
009500040628     C                   KFLD                    VAGNRS
009600040628     C                   KFLD                    VAGNSP
009700040628     C*
009800991027     C                   ENDSR
