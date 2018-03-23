000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300090323
000400050908     FTIVGDTMP  UF   E             DISK
000500090323     Ffnvagwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000050908     D fnvagds       e ds                  extname(fnvag00t)
001100941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941221     D* 10+3
001300941221     D                 DS
001400941221     D  WS15                   1      1
001500941221     D  WI15                   2     11
001600991125     D  WV15                  12     12    inz(',')
001700941221     D  WD15                  13     15
001800941221     D  WASC15                 1     15
001900991027
002000991027     C                   exsr      traduci
002100991027     C                   seton                                        lr
002200991027
002300991027     C     traduci       BEGSR
002400991027     C*
002500050908     C                   READ      TIVGDTMP
002600991027     C                   dow       not %eof
002700050908     C                   eval      fnvagds = vgdDTA
002800991027     C*
002900941229     C                   MOVE      VAGLNP        VHGLNP
003000941229     C                   MOVE      VAGAAS        VHGAAS
003100941229     C                   MOVE      VAGNRS        VHGNRS
003200941229     C                   MOVE      VAGNSP        VHGNSP
003300941229     C                   MOVE      VAGLNA        VHGLNA
003400941229     C                   MOVE      VAGAGC        VHGAGC
003500941229     C                   MOVE      VAGMGC        VHGMGC
003600941229     C                   MOVE      VAGFGC        VHGFGC
003700941229     C                   MOVE      VAGNGC        VHGNGC
003800941229     C                   MOVE      VAGFRG        VHGFRG
003900941229     C                   MOVE      VAGGGA        VHGGGA
004000941229     C                   MOVE      VAGCMC        VHGCMC
004100941229     C                   MOVE      VAGDMC        VHGDMC
004200941229     C                   MOVE      VAGNOT        VHGNOT
004300941229     C                   MOVE      VAGRMN        VHGRMN
004400941229     C                   MOVE      VAGRMA        VHGRMA
004500941229     C                   MOVE      VAGSCM        VHGSCM
004600941229     C                   MOVE      VAGDMM        VHGDMM
004700941229     C                   MOVE      VAGVCS        VHGVCS
004800941221     C*
004900941229     C                   MOVE      VAGCAS        WD15
005000941229     C                   MOVEL     VAGCAS        WI15
005100941229     C                   MOVE      WASC15        VHGCAS
005200941221     C*
005300941229     C                   MOVE      VAGVCA        VHGVCA
005400941229     C                   MOVE      VAGDDM        VHGDDM
005500941229     C                   MOVE      VAGDED        VHGDED
005600941229     C                   MOVE      VAGDUR        VHGDUR
005700941229     C                   MOVE      VAGDCG        VHGDCG
005800941229     C                   MOVE      VAGCFG        VHGCFG
005900941229     C                   MOVE      VAGSGS        VHGSGS
006000941229     C                   MOVE      VAGSGR        VHGSGR
006100941229     C                   MOVE      VAGSGD        VHGSGD
006200941229     C                   MOVE      VAGDSD        VHGDSD
006300941229     C                   MOVE      VAGDSA        VHGDSA
006400941229     C                   MOVE      VAGGGS        VHGGGS
006500941229     C                   MOVE      VAGSGF        VHGSGF
006600941229     C                   MOVE      VAGSGA        VHGSGA
006700941229     C                   MOVE      VAGTFT        VHGTFT
006800941229     C                   MOVE      VAGDBG        VHGDBG
006900941229     C                   MOVE      VAGLPW        VHGLPW
007000941229     C                   MOVE      VAGASW        VHGASW
007100941229     C                   MOVE      VAGNSW        VHGNSW
007200941229     C                   MOVE      VAGNBW        VHGNBW
007300941229     C                   MOVE      VAGKSC        VHGKSC
007400121122     C*
007500121122     C* Forzatura => ritorno unicamente i dati relativi al mittente 0503465
007600121122     C                   IF        VAGSCM = 0503465
007700121122     C                   WRITE     FNVHGAA
007800121122     C                   ENDIF
007900050908     C                   DELETE    TIVGD000
008000991027     C*
008100050908     C                   READ      TIVGDTMP
008200991027     C                   ENDdo
008300991027     C*
008400991027     C                   eval      wrkesito = '0'
008500991027     C*
008600910830     C                   ENDSR
008700991027
008800991027      /TITLE Operazioni iniziali.
008900991027     C     *inzsr        BEGSR
009000991027     C*
009100991027     C     *ENTRY        PLIST
009200991027     C                   parm                    prmppt
009300991027     C     wrkesito      parm      wrkesito      prmesito
009400991027     C*
009500991027     C                   ENDSR
