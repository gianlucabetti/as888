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
001400101018     D wKSC            s                   like(tasKSC)
001500101018     D wDSP            s              8  0
001600101018     D wDSPa           s              8
001700101018     D wRSD            s                   like(tasRSD)
001800101018     D wLOD            s                   like(tasLOD)
001900101018     D wDGC            s              8  0
002000101018     D wDGCa           s              8
002100030325
002200101018
002300101018
002400030908     C                   EXSR      chkRECFILMBR
002500991027     C                   EXSR      traduci
002600921023     C                   SETON                                        LR
002700101018
002800101018
002900030908     C     chkRECFILMBR  BEGSR
003000030908     C*
003100101018     C                   READ      TIVAW000                               55
003200030908     C*
003300030908     C                   ENDSR
003400101018
003500991027
003600991027     C     traduci       BEGSR
003700080918     C*
003800080918     C* Verifico se richiesto 1 particolare carattere separatore campo
003900101018     C                   IF        %subst(prmppt:2:1) = *blanks
004000080918     C                   EVAL      ChCSV = ','
004100080918     C                   ELSE
004200101018     C                   EVAL      ChCSV = %trim(%subst(prmppt:2:1))
004300080918     C                   ENDIF
004400030325     C*
004500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700101018     C                   EVAL      VAWDTA  =
004800101018     C                                       '"CODICE CLIENTE"'          +ChCSV+
004900101018     C                                       '"DATA SPEDIZIONE"'         +ChCSV+
005000101018     C                                       '"RIF. MITT. NUMERICO"'     +ChCSV+
005100101018     C                                       '"RIF. MITT. ALFABETICO"'   +ChCSV+
005200101018     C                                       '"NUM. SPEDIZIONE"'         +ChCSV+
005300101018     C                                       '"RAG. SOC. DESTINATARIO"'  +ChCSV+
005400101018     C                                       '"LOCALITA DESTINATARIO"'   +ChCSV+
005500101018     C                                       '"DATA APERTURA GIACENZA"'  +ChCSV+
005600101018     C                                       '"MOTIVAZIONE"'             +ChCSV
005700030325     C*
005800101018     C                   WRITE     TIVAW000
005900030325     C                   ENDIF
006000991027     C*
006100050908     C                   READ      TIVGDTMP
006200050908     C                   DOW       not %eof(TIVGDTMP)
006300050908     C                   eval      fnvagds = vgdDTA
006400040628     C*
006500101018     C                   CLEAR                   wKSC
006600101018     C                   CLEAR                   wDSP
006700101018     C                   CLEAR                   wDSPa
006800101018     C                   CLEAR                   wRSD
006900101018     C                   CLEAR                   wLOD
007000101018     C                   CLEAR                   wDGC
007100101018     C                   CLEAR                   wDGCa
007200101018     C*
007300101018     C* Se richiesto nei parametri anche i campi del destinatario "chaino" la bolla relativa alla
007400101018     C* giacenza corrente.
007500101018     C     KEYTAS30_1P   CHAIN     TITAS30C
007600040628     C                   IF        %found(TITAS30C)
007700101018     C                   EVAL      wKSC = tasKSC
007800100504     C                   EVAL      wDSP = tasAAS*10000+tasMGS
007900101018     C                   MOVEL     wDSP          wDSPa
008000101018     C                   EVAL      wRSD = tasRSD
008100101018     C                   EVAL      wLOD = tasLOD
008200101018     C                   EVAL      wDGC = vagagc*10000+vagmgc
008300101018     C                   MOVEL     wDGC          wDGCa
008400040628     C                   ENDIF
008500030325     C*
008600101018     C                   EVAL      VAWDTA  =
008700101018     C                                         %trim(%editc(wKSC:'X'))+ChCSV+
008800101018     C                                       %subst(wDSPa:7:2)+'-'          +
008900101018     C                                       %subst(wDSPa:5:2)+'-'          +
009000101018     C                                       %subst(wDSPa:1:4)        +ChCSV+
009100101018     C                                       %trim(%editc(VAGRMN:'X'))+ChCSV+
009200101018     C                                          '"'+%trim(VAGRMA)+'"' +ChCSV+
009300101018     C                                       %trim(%editc(VAGLNP:'X'))+'-'  +
009400101018     C                                       %trim(%editc(VAGNSP:'X'))+ChCSV+
009500101018     C                                            '"'+%trim(wRSD)+'"' +ChCSV+
009600101018     C                                            '"'+%trim(wLOD)+'"' +ChCSV+
009700101018     C                                       %subst(wDGCa:7:2)+'-'          +
009800101018     C                                       %subst(wDGCa:5:2)+'-'          +
009900101018     C                                       %subst(wDGCa:1:4)        +ChCSV+
010000101018     C                                          '"'+%trim(VAGDMC)+' '       +
010100101018     C                                              %trim(VAGNOT)+'"' +ChCSV
010200930409     C*
010300101018     C                   WRITE     TIVAW000
010400050908     C                   DELETE    TIVGD000
010500991027     C*
010600050908     C                   READ      TIVGDTMP
010700030325     C                   ENDDO
010800991027     C*
010900030325     C                   EVAL      wrkesito = '0'
011000991027     C*
011100910830     C                   ENDSR
011200991027
011300101018
011400991027      /TITLE Operazioni iniziali.
011500991027     C     *inzsr        BEGSR
011600991027     C*
011700991027     C     *ENTRY        PLIST
011800040628     C                   PARM                    prmppt
011900040628     C     wrkesito      PARM      wrkesito      prmesito
012000991027     C*
012100040628     C* DEFINIZIONE CHIAVI
012200040628     C*
012300040628     C* Chiave su TITAS30C - Parziale
012400040628     C     KEYTAS30_1P   KLIST
012500040628     C                   KFLD                    VAGAAS
012600040628     C                   KFLD                    VAGLNP
012700040628     C                   KFLD                    VAGNRS
012800040628     C                   KFLD                    VAGNSP
012900040628     C*
013000991027     C                   ENDSR
