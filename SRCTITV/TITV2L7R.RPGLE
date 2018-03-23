000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500120604     FTIVAWWWT  O    E             DISK    commit
000600120604     Ftitas30c  IF   E           K DISK
000700991027
000800120604     D tnvas00t      e ds
000900120604     D tnvaswwt      e ds
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300991027
001400991027     C                   EXSR      traduci
001500921023     C                   SETON                                        LR
001600991027
001700991027     C     traduci       BEGSR
001800991027     C*
001900090122     C                   READ      tivgdtmp
002000991027     C                   dow       not %eof
002100120604     C                   movel     vgddta        tnvas00t
002200991027     C*
002300941209     C                   MOVE      VASLNP        VXSLNP
002400941209     C                   MOVE      VASLNA        VXSLNA
002500941209     C                   MOVE      VASAAS        VXSAAS
002600941209     C                   MOVE      VASNRS        VXSNRS
002700941209     C                   MOVE      VASNSP        VXSNSP
002800941209     C                   MOVE      VASRMN        VXSRMN
002900941209     C                   MOVEL     VASCAS        W11              11            METTO
003000941209     C                   MOVE      ','           W11                            LA ,
003100941209     C                   MOVE      W11           W12              12            NEL C/A
003200941209     C                   MOVE      VASCAS        VXSCAS
003300941209     C                   MOVEL     W12           VXSCAS
003400941209     C                   MOVE      VASVCA        VXSVCA
003500941209     C                   MOVE      VASNRA        VXSNRA
003600941209     C                   MOVE      VASDTE        VXSDTE
003700941209     C                   MOVE      VASTPI        VXSTPI
003800941209     C                   MOVE      VASABI        VXSABI
003900941209     C                   MOVE      VASCAI        VXSCAI
004000941209     C                   MOVE      VASNDP        VXSNDP
004100941209     C                   MOVE      VASDDP        VXSDDP
004200941209     C                   MOVE      VASCCB        VXSCCB
004300941209     C                   MOVE      VASABP        VXSABP
004400941209     C                   MOVE      VASCAP        VXSCAP
004500941209     C                   MOVE      VASFPC        VXSFPC
004600941209     C                   MOVE      VASDVA        VXSDVA
004700941209     C                   MOVE      VASKSC        VXSKSC
004800941209     C                   MOVE      VASRMA        VXSRMA
004900120604     C*
005000120604     C                   EVAL      VAWDTA = tnvaswwt
005100120604     C*
005200120604     C* Reperisco i dati NON presenti sul TNVAS00T
005300120604     C     KEYtas30_1P   CHAIN     titas30c
005400120604     C                   IF        %found(titas30c)
005500120604     C                   EVAL      VAWDTA = tnvaswwt + tasRSD + tasIND +
005600120604     C                                                 tasLOD + tasCAD +
005700120604     C                                                 tasPRD
005800120604     C                   ENDIF
005900930409     C*
006000120604     C                   WRITE     TIVAW000
006100090122     C                   delete    tivgd000
006200991027     C*
006300090122     C                   READ      tivgdtmp
006400991027     C                   ENDdo
006500991027     C*
006600991027     C                   eval      wrkesito = '0'
006700991027     C*
006800910830     C                   ENDSR
006900991027
007000991027      /TITLE Operazioni iniziali.
007100991027     C     *inzsr        BEGSR
007200991027     C*
007300991027     C     *ENTRY        PLIST
007400991027     C                   parm                    prmppt
007500991027     C     wrkesito      parm      wrkesito      prmesito
007600120604     C*
007700120604     C* DEFINIZIONE CHIAVI
007800120604     C*
007900120604     C* Chiave su TITAS30C - Parziale
008000120604     C     KEYtas30_1P   KLIST
008100120604     C                   KFLD                    vasAAS
008200120604     C                   KFLD                    vasLNP
008300120604     C                   KFLD                    vasNRS
008400120604     C                   KFLD                    vasNSP
008500991027     C*
008600991027     C                   ENDSR
