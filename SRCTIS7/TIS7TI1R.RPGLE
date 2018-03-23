000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000090122
001100090122     D fivaids       e ds                  extname(fivai00t) prefix(in_)
001200090122     D fivai_1w        ds
001300090122     D   VAIATB                       1
001400090122     D   VAICCM                       7
001500090122     D   VAIAAS                       4
001600090122     D   VAILNP                       3
001700090122     D   VAINRS                       2
001800090122     D   VAINSP                       7
001900090122     D   VAIRMN                      15
002000090122     D   VAIRMA                      15
002100090122     D   VAIRMX                      20
002200090122     D   VAIDAC                       8
002300090122     D   VAIORC                       6
002400090122     D   VAITDT                       3
002500090122     D   VAIDTI                     256
002600090122     D   VAIXXX                       1
002700991027
002800090122
002900090122
003000991027     C                   EXSR      traduci
003100921023     C                   SETON                                        LR
003200991027
003300090122
003400991027     C     traduci       BEGSR
003500991027     C*
003600050907     C                   READ      TIVGDTMP
003700991027     C                   dow       not %eof
003800050907     C                   eval      fivaids = vgdDTA
003900090122     C                   clear                   fivai_1w
004000991027     C*
004100090122     C                   MOVE      in_vaiATB     vaiATB
004200090122     C                   MOVE      in_vaiCCM     vaiCCM
004300090122     C                   MOVE      in_vaiAAS     vaiAAS
004400090122     C                   MOVE      in_vaiLNP     vaiLNP
004500090122     C                   MOVE      in_vaiNRS     vaiNRS
004600090122     C                   MOVE      in_vaiNSP     vaiNSP
004700090122     C                   MOVE      in_vaiRMN     vaiRMN
004800090122     C                   MOVE      in_vaiRMA     vaiRMA
004900090122     C                   MOVE      in_vaiRMX     vaiRMX
005000090122     C                   MOVE      in_vaiDAC     vaiDAC
005100090122     C                   MOVE      in_vaiORC     vaiORC
005200090122     C                   MOVE      in_vaiTDT     vaiTDT
005300090122     C                   MOVE      in_vaiDTI     vaiDTI
005400090122     C                   MOVE      'X'           vaiXXX
005500930409     C*
005600090122     C                   eval      vawdta = fivai_1w
005700090122     C*
005800090122     C                   WRITE     TIVAW000
005900050907     C                   DELETE    TIVGD000
006000991027     C*
006100050907     C                   READ      TIVGDTMP
006200991027     C                   ENDdo
006300991027     C*
006400991027     C                   eval      wrkesito = '0'
006500991027     C*
006600910830     C                   ENDSR
006700991027
006800090122
006900991027      /TITLE Operazioni iniziali.
007000991027     C     *inzsr        BEGSR
007100991027     C*
007200991027     C     *ENTRY        PLIST
007300991027     C                   parm                    prmppt
007400991027     C     wrkesito      parm      wrkesito      prmesito
007500991027     C*
007600991027     C                   ENDSR
