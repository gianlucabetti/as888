000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500111024     Ftnvaswwt  O    E             DISK    commit
000600110209     FTIVGD00F  O    E             DISK    prefix(w_) rename(TIVGD000:TIVGDWWW)
000700111024     F                                     commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200111024     D tnvasds       e ds                  extname(tnvas00t)
001300111024
001400991027
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700991027
001800111024
001900111024
002000991027     C     traduci       BEGSR
002100991027     C*
002200050909     C                   READ      TIVGDTMP
002300991027     C                   dow       not %eof
002400111024     C                   eval      tnvasds = vgdDTA
002500991027     C*
002600111024     C                   MOVE      VASLNP        VXSLNP
002700111024     C                   MOVE      VASLNA        VXSLNA
002800111024     C                   MOVE      VASAAS        VXSAAS
002900111024     C                   MOVE      VASNRS        VXSNRS
003000111024     C                   MOVE      VASNSP        VXSNSP
003100111024     C                   MOVE      VASRMN        VXSRMN
003200111024     C                   MOVEL     VASCAS        W11              11            METTO
003300111024     C                   MOVE      ','           W11                            LA ,
003400111024     C                   MOVE      W11           W12              12            NEL C/A
003500111024     C                   MOVE      VASCAS        VXSCAS
003600111024     C                   MOVEL     W12           VXSCAS
003700111024     C                   MOVE      VASVCA        VXSVCA
003800111024     C                   MOVE      VASNRA        VXSNRA
003900111024     C                   MOVE      VASDTE        VXSDTE
004000111024     C                   MOVE      VASTPI        VXSTPI
004100111024     C                   MOVE      VASABI        VXSABI
004200111024     C                   MOVE      VASCAI        VXSCAI
004300111024     C                   MOVE      VASNDP        VXSNDP
004400111024     C                   MOVE      VASDDP        VXSDDP
004500111024     C                   MOVE      VASCCB        VXSCCB
004600111024     C                   MOVE      VASABP        VXSABP
004700111024     C                   MOVE      VASCAP        VXSCAP
004800111024     C                   MOVE      VASFPC        VXSFPC
004900111024     C                   MOVE      VASDVA        VXSDVA
005000111024     C                   MOVE      VASKSC        VXSKSC
005100111024     C                   MOVE      VASRMA        VXSRMA
005200111024     C*
005300111024     C* Smisto i file d output in base al tipo pagamento: S=assegno B=bonifico A=altro
005400111024     C                   SELECT
005500111026     C                   WHEN      VASFPC   = *blanks
005600111024     C                   WRITE     TNVXS000
005700111024     C                   WHEN      VASFPC   = '2'
005800111024     C                   clear                   tivgdwww
005900111024     C                   EVAL      w_vgdTIP = 'SB'
006000111024     C                   EXSR      WRIVGD
006100111024     C                   OTHER
006200111024     C                   clear                   tivgdwww
006300111024     C                   EVAL      w_vgdTIP = 'SA'
006400111024     C                   EXSR      WRIVGD
006500111024     C                   ENDSL
006600930409     C*
006700050909     C                   DELETE    TIVGD000
006800991027     C*
006900050909     C                   READ      TIVGDTMP
007000991027     C                   ENDdo
007100991027     C*
007200991027     C                   eval      wrkesito = '0'
007300991027     C*
007400910830     C                   ENDSR
007500110209
007600110209
007700110209
007800110209      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
007900110209     C     wriVGD        BEGSR
008000110209     C*
008100111024     C                   eval      w_vgdDTA = tnvasds
008200110209     C                   eval      w_vgdKSU = vgdKSU
008300110209     C                   eval      w_vgdTSC = 'WW'
008400110209     C                   eval      w_vgdDAT = datcor
008500111024     C                   eval      w_vgdPGM = 'TITV2I8R'
008600110209     C                   write     tivgdwww
008700110209     C*
008800110209     C                   ENDSR
008900110209     C*------------------------------------------------------------------------*
009000110209
009100110209
009200991027
009300991027      /TITLE Operazioni iniziali.
009400991027     C     *inzsr        BEGSR
009500991027     C*
009600991027     C     *ENTRY        PLIST
009700991027     C                   parm                    prmppt
009800991027     C     wrkesito      parm      wrkesito      prmesito
009900110209     C*
010000110209     C                   z-add     *zeros        datcor            8 0
010100110209     C                   eval      datcor = %dec(%date() : *ISO)
010200991027     C*
010300991027     C                   ENDSR
