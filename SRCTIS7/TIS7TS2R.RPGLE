000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090323     FTNVASXLS  uF A E             DISK    commit
000600991027
000700090122     D tnvasds       e ds                  extname(tnvas00t)
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100060720
001200060720     D wDlmTxt         s              5    varying inz('')
001300060720
001400991027
001500030908     C                   EXSR      chkRECFILMBR
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800030908
001900030908     C     chkRECFILMBR  BEGSR
002000030908     C*
002100030908     C                   READ      TNVASXLS                               55
002200030908     C*
002300030908     C                   ENDSR
002400991027
002500991027     C     traduci       BEGSR
002600040206     C*
002700040206     C* Verifico se richiesto un particolare carattere d separatore campi
002800040206     C                   IF        %subst(prmppt:3:1) <> *blanks
002900040206     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003000040206     C                   ELSE
003100040206     C                   MOVEL     ','           wSepCampi         1
003200040206     C                   ENDIF
003300060720     C*
003400060720     C* Verifico se richiesto nessun delimitatore testo
003500060720     C*   => *blanks = standard = "
003600060720     C*   => N       = nessun delimitatore testo
003700060720     C                   EVAL      wDlmTxt = '"'
003800060720     C                   IF        %subst(prmppt:2:1) = 'N'
003900060720     C                   EVAL      wDlmTxt = *blanks
004000060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
004100060720     C                   ENDIF
004200030325     C*
004300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004500060720     C                   EVAL      VASXDTA =
004600060720     C                                wDlmTxt+'VASLNP'+wDlmTxt+wSepCampi+
004700060720     C                                wDlmTxt+'VASLNA'+wDlmTxt+wSepCampi+
004800060720     C                                wDlmTxt+'VASAAS'+wDlmTxt+wSepCampi+
004900060720     C                                wDlmTxt+'VASNRS'+wDlmTxt+wSepCampi+
005000060720     C                                wDlmTxt+'VASNSP'+wDlmTxt+wSepCampi+
005100060720     C                                wDlmTxt+'VASRMN'+wDlmTxt+wSepCampi+
005200060720     C                                wDlmTxt+'VASCAS'+wDlmTxt+wSepCampi+
005300060720     C                                wDlmTxt+'VASVCA'+wDlmTxt+wSepCampi+
005400060720     C                                wDlmTxt+'VASNRA'+wDlmTxt+wSepCampi+
005500060720     C                                wDlmTxt+'VASDTE'+wDlmTxt+wSepCampi+
005600060720     C                                wDlmTxt+'VASTPI'+wDlmTxt+wSepCampi+
005700060720     C                                wDlmTxt+'VASABI'+wDlmTxt+wSepCampi+
005800060720     C                                wDlmTxt+'VASCAI'+wDlmTxt+wSepCampi+
005900060720     C                                wDlmTxt+'VASNDP'+wDlmTxt+wSepCampi+
006000060720     C                                wDlmTxt+'VASDDP'+wDlmTxt+wSepCampi+
006100060720     C                                wDlmTxt+'VASCCB'+wDlmTxt+wSepCampi+
006200060720     C                                wDlmTxt+'VASABP'+wDlmTxt+wSepCampi+
006300060720     C                                wDlmTxt+'VASCAP'+wDlmTxt+wSepCampi+
006400060720     C                                wDlmTxt+'VASFPC'+wDlmTxt+wSepCampi+
006500060720     C                                wDlmTxt+'VASDVA'+wDlmTxt+wSepCampi+
006600060720     C                                wDlmTxt+'VASKSC'+wDlmTxt+wSepCampi+
006700060720     C                                wDlmTxt+'VASRMA'+wDlmTxt
006800030325     C*
006900030325     C                   WRITE     TNVASX00
007000030325     C                   ENDIF
007100991027     C*
007200090122     C                   READ      tivgdtmp
007300090122     C                   DOW       not %eof(tivgdtmp)
007400090122     C                   movel     vgddta        tnvasds
007500030325     C*
007600060720     C                   EVAL      VASXDTA=
007700060720     C                                   %trim(%editc(VASLNP:'Q'))+wSepCampi+
007800060720     C                                   %trim(%editc(VASLNA:'Q'))+wSepCampi+
007900060720     C                                   %trim(%editc(VASAAS:'Q'))+wSepCampi+
008000060720     C                                   %trim(%editc(VASNRS:'Q'))+wSepCampi+
008100060720     C                                   %trim(%editc(VASNSP:'Q'))+wSepCampi+
008200060720     C                                   %trim(%editc(VASRMN:'Q'))+wSepCampi+
008300060720     C                                   %trim(%editc(VASCAS:'Q'))+wSepCampi+
008400060720     C                                  wDlmTxt+%trim(VASVCA)+wDlmTxt+wSepCampi+
008500060720     C                                  wDlmTxt+%trim(VASNRA)+wDlmTxt+wSepCampi+
008600060720     C                                   %trim(%editc(VASDTE:'Q'))+wSepCampi+
008700060720     C                                  wDlmTxt+%trim(VASTPI)+wDlmTxt+wSepCampi+
008800060720     C                                   %trim(%editc(VASABI:'Q'))+wSepCampi+
008900060720     C                                   %trim(%editc(VASCAI:'Q'))+wSepCampi+
009000060720     C                                   %trim(%editc(VASNDP:'Q'))+wSepCampi+
009100060720     C                                   %trim(%editc(VASDDP:'Q'))+wSepCampi+
009200060720     C                                  wDlmTxt+%trim(VASCCB)+wDlmTxt+wSepCampi+
009300060720     C                                   %trim(%editc(VASABP:'Q'))+wSepCampi+
009400060720     C                                   %trim(%editc(VASCAP:'Q'))+wSepCampi+
009500060720     C                                  wDlmTxt+%trim(VASFPC)+wDlmTxt+wSepCampi+
009600060720     C                                   %trim(%editc(VASDVA:'Q'))+wSepCampi+
009700060720     C                                   %trim(%editc(VASKSC:'Q'))+wSepCampi+
009800060720     C                                  wDlmTxt+%trim(VASRMA)+wDlmTxt
009900930409     C*
010000030325     C                   WRITE     TNVASX00
010100090122     C                   DELETE    tivgd000
010200991027     C*
010300090122     C                   READ      tivgdtmp
010400030325     C                   ENDDO
010500991027     C*
010600030325     C                   EVAL      wrkesito = '0'
010700991027     C*
010800910830     C                   ENDSR
010900991027
011000991027      /TITLE Operazioni iniziali.
011100991027     C     *inzsr        BEGSR
011200991027     C*
011300991027     C     *ENTRY        PLIST
011400991027     C                   parm                    prmppt
011500991027     C     wrkesito      parm      wrkesito      prmesito
011600991027     C*
011700991027     C                   ENDSR
