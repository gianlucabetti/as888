000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400010907     FTNCSB03L  IF   E           K DISK
000500011026     FTITAS30C  IF   E           K DISK
000600090122     Ftivgdtmp  uF   E             DISK
000700090122     FTIVAWWWT  O    E             DISK
000800090122
000900090122     D tnvasds       e ds                  extname(tnvas00t)
001000090122     D out_ds          ds
001100090122     D  AMWORD                       11
001200090122     D  AMWIST                       21
001300090122     D  AMWLOC                       18
001400090122     D  AMWIMP                       11
001500090122     D  AMWDRI                        8
001600090122     D  AMWNRI                        6
001700090122     D  AMWDRE                        8
001800090122     D  AMWDIV                        3
001900991027
002000991027     D prmppt          s             50
002100991027     D prmesito        s              1
002200991027     D wrkesito        s                   like(prmesito)
002300010907
002400010907     D WLBDAT          DS                  INZ
002500010907     D  G02DAT                 1      8  0
002600010907     D  G02INV                 9     16  0
002700010907     D  G02ERR                17     17
002800010907     D  G02TGI                18     22  0
002900991027
003000991027     C                   EXSR      traduci
003100921023     C                   SETON                                        LR
003200991027
003300991027     C     traduci       BEGSR
003400991027     C*
003500090122     C                   READ      tivgdtmp
003600991027     C                   dow       not %eof
003700090122     C                   movel     vgddta        tnvasds
003800010907     C*
003900010907     C     keyCSB        chain     TNCSB03L                           54
004000011026     C  N54keyTAS        chain     TITAS30C                           55
004100010907     C                   z-add     vasRMN        W11N             11 0
004200010907     C                   movel     1             W11N
004300010907     C                   move      W11N          amwORD
004400011026     C  N55              movel     tasRSD        amwIST
004500011026     C  N55              movel     tasLOD        amwLOC
004600010907     C                   z-add     vasCAS        W11N2            11 2
004700010907     C                   move      W11N2         amwIMP
004800010910     C                   if        vasVCA = *blanks or
004900010910     C                             vasVCA = 'ITL'
005000010910     C                   movel     *blanks       amwDIV
005100010910     C                   else
005200010910     C                   movel     vasVCA        amwDIV
005300010910     C                   endif
005400010907     C                   move      vasNDP        amwNRI
005500011005     C                   move      vasDDP        amwDRI
005600011005     C*                  move      *zero         amwDRI
005700011005     C*                  if        vasDDP > *zero
005800011005     C*                  z-add     vasDDP        G02INV
005900011005     C*                  movel     '3'           G02ERR
006000011005     C*                  call      'XSRDA8'
006100011005     C*                  parm                    WLBDAT
006200011005     C*                  move      G02DAT        amwDRI
006300011005     C*                  endif
006400010907     C* DATA ORDINE = LORO DATA BOLLA IN RAM OPPURE NS. DATA BOLLA
006500010907     C                   move      *zeros        amwDRE
006600010907     C                   move      vasRMA        W8A               8
006700010907     C                   if        %scan('0':W8A) > 0
006800010907     C                   move      W8A           amwDRE
006900010907     C                   else
007000011026     C  N54              move      csbMGS        amwDRE
007100011026     C  N54              movel     csbAAS        amwDRE
007200010907     C                   endif
007300010910     C***                if        amwDRE >= '19951013'
007400010910     C***                z-add     vasRMN        W11N
007500010910     C***                movel     3             W11N
007600010910     C***                move      W11N          amwORD
007700010910     C***                endif
007800930409     C*
007900090122     C                   eval      vawDTA = out_ds
008000090122     C*
008100090122     C                   WRITE     TIVAW000
008200090122     C                   delete    tivgd000
008300991027     C*
008400090122     C                   READ      tivgdtmp
008500991027     C                   ENDdo
008600991027     C*
008700991027     C                   eval      wrkesito = '0'
008800991027     C*
008900910830     C                   ENDSR
009000991027
009100090122
009200991027      /TITLE Operazioni iniziali.
009300991027     C     *inzsr        BEGSR
009400991027     C*
009500991027     C     *ENTRY        PLIST
009600991027     C                   parm                    prmppt
009700991027     C     wrkesito      parm      wrkesito      prmesito
009800010907     C*
009900010907     C* Definizione chiavi
010000010907     C     keyCSB        klist
010100010907     C                   kfld                    vasAAS
010200010907     C                   kfld                    vasLNP
010300010907     C                   kfld                    vasNRS
010400010907     C                   kfld                    vasNSP
010500011026     C*
010600011026     C* Definizione chiavi
010700011026     C     keyTAS        klist
010800011026     C                   kfld                    csbAAS
010900011026     C                   kfld                    csbLNP
011000011026     C                   kfld                    csbNRS
011100011026     C                   kfld                    csbNSP
011200011026     C                   kfld                    csbTBL
011300991027     C*
011400991027     C                   ENDSR
