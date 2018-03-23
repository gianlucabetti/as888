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
001400130603     D wRSD            s                   like(tasRSD)
001500130603     D wIND            s                   like(tasIND)
001600130603     D wCAD            s                   like(tasCAD)
001700130603     D wLOD            s                   like(tasLOD)
001800130603     D wPRD            s                   like(tasPRD)
001900030325
002000101018
002100101018
002200030908     C                   EXSR      chkRECFILMBR
002300991027     C                   EXSR      traduci
002400921023     C                   SETON                                        LR
002500101018
002600101018
002700030908     C     chkRECFILMBR  BEGSR
002800030908     C*
002900101018     C                   READ      TIVAW000                               55
003000030908     C*
003100030908     C                   ENDSR
003200101018
003300991027
003400991027     C     traduci       BEGSR
003500080918     C*
003600080918     C* Verifico se richiesto 1 particolare carattere separatore campo
003700101018     C                   IF        %subst(prmppt:2:1) = *blanks
003800080918     C                   EVAL      ChCSV = ','
003900080918     C                   ELSE
004000101018     C                   EVAL      ChCSV = %trim(%subst(prmppt:2:1))
004100080918     C                   ENDIF
004200030325     C*
004300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004500101018     C                   EVAL      VAWDTA  =
004600120420     C                                       '"DDT"'                     +ChCSV+
004700120420     C                                       '"DESTINATARIO"'            +ChCSV+
004800130603     C                                       '"INDIRIZZO DESTINATARIO"'  +ChCSV+
004900130603     C                                       '"CAP  DESTINATARIO"'       +ChCSV+
005000130603     C                                       '"LOCALITA DESTINATARIO"'   +ChCSV+
005100130603     C                                       '"PROV DESTINATARIO"'       +ChCSV+
005200120420     C                                       '"MOTIVO"'                  +ChCSV+
005300120420     C                                       '"ISTRUZIONI"'              +ChCSV+
005400120420     C                                       '"DATA SVINCOLO"'           +ChCSV
005500030325     C*
005600101018     C                   WRITE     TIVAW000
005700030325     C                   ENDIF
005800991027     C*
005900050908     C                   READ      TIVGDTMP
006000050908     C                   DOW       not %eof(TIVGDTMP)
006100050908     C                   eval      fnvagds = vgdDTA
006200120420     C*
006300120420     C                   CLEAR                   wRSD
006400130603     C                   CLEAR                   wIND
006500130603     C                   CLEAR                   wCAD
006600130603     C                   CLEAR                   wLOD
006700130603     C                   CLEAR                   wPRD
006800101018     C*
006900101018     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
007000101018     C* giacenza corrente.
007100101018     C     KEYTAS30_1P   CHAIN     TITAS30C
007200040628     C                   IF        %found(TITAS30C)
007300130621     C                   EVAL      wRSD = tasRSD
007400130621     C                   EVAL      wIND = tasIND
007500130621     C                   EVAL      wCAD = tasCAD
007600130621     C                   EVAL      wLOD = tasLOD
007700130621     C                   EVAL      wPRD = tasPRD
007800130603     C                   ENDIF
007900030325     C*
008000101018     C                   EVAL      VAWDTA  =
008100120420     C                                       %trim(%editc(VAGRMN:'X'))   +ChCSV+
008200130603     C                                             '"'+%trim(wRSD)+'"'   +ChCSV+
008300130603     C                                             '"'+%trim(wIND)+'"'   +ChCSV+
008400130603     C                                             '"'+%trim(wCAD)+'"'   +ChCSV+
008500130603     C                                             '"'+%trim(wLOD)+'"'   +ChCSV+
008600130603     C                                             '"'+%trim(wPRD)+'"'   +ChCSV+
008700120420     C                             '"'+%trim(VAGDMC)+' '+%trim(VAGNOT)   +ChCSV+
008800120420     C                                                           '" "'   +ChCSV+
008900120420     C                                                           '" "'   +ChCSV
009000930409     C*
009100101018     C                   WRITE     TIVAW000
009200050908     C                   DELETE    TIVGD000
009300991027     C*
009400050908     C                   READ      TIVGDTMP
009500030325     C                   ENDDO
009600991027     C*
009700030325     C                   EVAL      wrkesito = '0'
009800991027     C*
009900910830     C                   ENDSR
010000991027
010100101018
010200991027      /TITLE Operazioni iniziali.
010300991027     C     *inzsr        BEGSR
010400991027     C*
010500991027     C     *ENTRY        PLIST
010600040628     C                   PARM                    prmppt
010700040628     C     wrkesito      PARM      wrkesito      prmesito
010800991027     C*
010900040628     C* DEFINIZIONE CHIAVI
011000040628     C*
011100040628     C* Chiave su TITAS30C - Parziale
011200120420     C     KEYTAS30_1P   KLIST
011300040628     C                   KFLD                    VAGAAS
011400040628     C                   KFLD                    VAGLNP
011500040628     C                   KFLD                    VAGNRS
011600040628     C                   KFLD                    VAGNSP
011700040628     C*
011800991027     C                   ENDSR
