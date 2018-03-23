000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300090323
000400050908     FTIVGDTMP  UF   E             DISK
000500090323     Ffnvag1wt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000050908     D fnvagds       e ds                  extname(fnvag00t)
001100991027
001200991027     C                   exsr      traduci
001300991027     C                   seton                                        lr
001400991027
001500991027     C     traduci       BEGSR
001600991027     C*
001700050908     C                   READ      TIVGDTMP
001800991027     C                   dow       not %eof
001900050908     C                   eval      fnvagds = vgdDTA
002000991027     C*
002100941229     C                   MOVE      VAGLNP        VHGLNP
002200941229     C                   MOVE      VAGAAS        VHGAAS
002300941229     C                   MOVE      VAGNRS        VHGNRS
002400941229     C                   MOVE      VAGNSP        VHGNSP
002500941229     C                   MOVE      VAGLNA        VHGLNA
002600941229     C                   MOVE      VAGAGC        VHGAGC
002700941229     C                   MOVE      VAGMGC        VHGMGC
002800941229     C                   MOVE      VAGFGC        VHGFGC
002900941229     C                   MOVE      VAGNGC        VHGNGC
003000941229     C                   MOVE      VAGFRG        VHGFRG
003100941229     C                   MOVE      VAGGGA        VHGGGA
003200941229     C                   MOVE      VAGCMC        VHGCMC
003300941229     C                   MOVE      VAGDMC        VHGDMC
003400941229     C                   MOVE      VAGNOT        VHGNOT
003500941229     C                   MOVE      VAGRMN        VHGRMN
003600941229     C                   MOVE      VAGRMA        VHGRMA
003700941229     C                   MOVE      VAGSCM        VHGSCM
003800941229     C                   MOVE      VAGDMM        VHGDMM
003900941229     C                   MOVE      VAGVCS        VHGVCS
004000020611     C                   MOVE      VAGCAS        VHGCAS
004100941229     C                   MOVE      VAGVCA        VHGVCA
004200941229     C                   MOVE      VAGDDM        VHGDDM
004300941229     C                   MOVE      VAGDED        VHGDED
004400941229     C                   MOVE      VAGDUR        VHGDUR
004500941229     C                   MOVE      VAGDCG        VHGDCG
004600941229     C                   MOVE      VAGCFG        VHGCFG
004700941229     C                   MOVE      VAGSGS        VHGSGS
004800941229     C                   MOVE      VAGSGR        VHGSGR
004900941229     C                   MOVE      VAGSGD        VHGSGD
005000941229     C                   MOVE      VAGDSD        VHGDSD
005100941229     C                   MOVE      VAGDSA        VHGDSA
005200941229     C                   MOVE      VAGGGS        VHGGGS
005300941229     C                   MOVE      VAGSGF        VHGSGF
005400941229     C                   MOVE      VAGSGA        VHGSGA
005500941229     C                   MOVE      VAGTFT        VHGTFT
005600941229     C                   MOVE      VAGDBG        VHGDBG
005700941229     C                   MOVE      VAGLPW        VHGLPW
005800941229     C                   MOVE      VAGASW        VHGASW
005900941229     C                   MOVE      VAGNSW        VHGNSW
006000941229     C                   MOVE      VAGNBW        VHGNBW
006100941229     C                   MOVE      VAGKSC        VHGKSC
006200930409     C*
006300991027     C                   WRITE     FNVHGAA
006400050908     C                   DELETE    TIVGD000
006500991027     C*
006600050908     C                   READ      TIVGDTMP
006700991027     C                   ENDdo
006800991027     C*
006900991027     C                   eval      wrkesito = '0'
007000991027     C*
007100910830     C                   ENDSR
007200991027
007300991027      /TITLE Operazioni iniziali.
007400991027     C     *inzsr        BEGSR
007500991027     C*
007600991027     C     *ENTRY        PLIST
007700991027     C                   parm                    prmppt
007800991027     C     wrkesito      parm      wrkesito      prmesito
007900991027     C*
008000991027     C                   ENDSR
