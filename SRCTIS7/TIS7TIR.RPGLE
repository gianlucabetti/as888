000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400050907     FTIVGDTMP  UF   E             DISK
000500030120     Ffivaiwwt  O    E             DISK    rename(fivai000:fivaiaaa)
000600090323     F                                     prefix(A_) commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100050907     D fivaids       e ds                  extname(fivai00t)
001200991027
001300991027     C                   EXSR      traduci
001400921023     C                   SETON                                        LR
001500991027
001600991027     C     traduci       BEGSR
001700991027     C*
001800050907     C                   READ      TIVGDTMP
001900991027     C                   dow       not %eof
002000050907     C                   eval      fivaids = vgdDTA
002100991027     C*
002200030120     C                   MOVE      vaiATB        A_vaiATB
002300030120     C                   MOVE      vaiCCM        A_vaiCCM
002400030120     C                   MOVE      vaiAAS        A_vaiAAS
002500030120     C                   MOVE      vaiLNP        A_vaiLNP
002600030120     C                   MOVE      vaiNRS        A_vaiNRS
002700030120     C                   MOVE      vaiNSP        A_vaiNSP
002800030120     C                   MOVE      vaiRMN        A_vaiRMN
002900030120     C                   MOVE      vaiRMA        A_vaiRMA
003000030120     C                   MOVE      vaiRMX        A_vaiRMX
003100030120     C                   MOVE      vaiDAC        A_vaiDAC
003200030120     C                   MOVE      vaiORC        A_vaiORC
003300030120     C                   MOVE      vaiTDT        A_vaiTDT
003400030120     C                   MOVE      vaiDTI        A_vaiDTI
003500030120     C                   MOVE      'X'           A_vaiXXX
003600930409     C*
003700030120     C                   WRITE     FIVAIaaa
003800050907     C                   DELETE    TIVGD000
003900991027     C*
004000050907     C                   READ      TIVGDTMP
004100991027     C                   ENDdo
004200991027     C*
004300991027     C                   eval      wrkesito = '0'
004400991027     C*
004500910830     C                   ENDSR
004600991027
004700991027      /TITLE Operazioni iniziali.
004800991027     C     *inzsr        BEGSR
004900991027     C*
005000991027     C     *ENTRY        PLIST
005100991027     C                   parm                    prmppt
005200991027     C     wrkesito      parm      wrkesito      prmesito
005300991027     C*
005400991027     C                   ENDSR
