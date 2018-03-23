000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090323     FTNVASWWT  O    E             DISK    commit
000600991027
000700090122     D tnvasds       e ds                  extname(tnvas00t)
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100991027
001200991027     C                   EXSR      traduci
001300921023     C                   SETON                                        LR
001400991027
001500991027     C     traduci       BEGSR
001600991027     C*
001700090122     C                   READ      tivgdtmp
001800991027     C                   dow       not %eof
001900090122     C                   movel     vgddta        tnvasds
002000991027     C*
002100941209     C                   MOVE      VASLNP        VXSLNP
002200941209     C                   MOVE      VASLNA        VXSLNA
002300941209     C                   MOVE      VASAAS        VXSAAS
002400941209     C                   MOVE      VASNRS        VXSNRS
002500941209     C                   MOVE      VASNSP        VXSNSP
002600941209     C                   MOVE      VASRMN        VXSRMN
002700941209     C                   MOVEL     VASCAS        W11              11            METTO
002800941209     C                   MOVE      ','           W11                            LA ,
002900941209     C                   MOVE      W11           W12              12            NEL C/A
003000941209     C                   MOVE      VASCAS        VXSCAS
003100941209     C                   MOVEL     W12           VXSCAS
003200941209     C                   MOVE      VASVCA        VXSVCA
003300941209     C                   MOVE      VASNRA        VXSNRA
003400941209     C                   MOVE      VASDTE        VXSDTE
003500941209     C                   MOVE      VASTPI        VXSTPI
003600941209     C                   MOVE      VASABI        VXSABI
003700941209     C                   MOVE      VASCAI        VXSCAI
003800941209     C                   MOVE      VASNDP        VXSNDP
003900941209     C                   MOVE      VASDDP        VXSDDP
004000941209     C                   MOVE      VASCCB        VXSCCB
004100941209     C                   MOVE      VASABP        VXSABP
004200941209     C                   MOVE      VASCAP        VXSCAP
004300941209     C                   MOVE      VASFPC        VXSFPC
004400941209     C                   MOVE      VASDVA        VXSDVA
004500941209     C                   MOVE      VASKSC        VXSKSC
004600941209     C                   MOVE      VASRMA        VXSRMA
004700930409     C*
004800941209     C                   WRITE     TNVXS000
004900090122     C                   delete    tivgd000
005000991027     C*
005100090122     C                   READ      tivgdtmp
005200991027     C                   ENDdo
005300991027     C*
005400991027     C                   eval      wrkesito = '0'
005500991027     C*
005600910830     C                   ENDSR
005700991027
005800991027      /TITLE Operazioni iniziali.
005900991027     C     *inzsr        BEGSR
006000991027     C*
006100991027     C     *ENTRY        PLIST
006200991027     C                   parm                    prmppt
006300991027     C     wrkesito      parm      wrkesito      prmesito
006400991027     C*
006500991027     C                   ENDSR
