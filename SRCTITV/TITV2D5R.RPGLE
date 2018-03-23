000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090323     FTNVASXLS  uF A E             DISK    commit
000600110328     FTITAA30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200060720
001300060720     D wDlmTxt         s              5    varying inz('')
001400060720
001500991027
001600030908     C                   EXSR      chkRECFILMBR
001700991027     C                   EXSR      traduci
001800921023     C                   SETON                                        LR
001900030908
002000030908     C     chkRECFILMBR  BEGSR
002100030908     C*
002200030908     C                   READ      TNVASXLS                               55
002300030908     C*
002400030908     C                   ENDSR
002500991027
002600991027     C     traduci       BEGSR
002700040206     C*
002800040206     C* Verifico se richiesto un particolare carattere d separatore campi
002900040206     C                   IF        %subst(prmppt:3:1) <> *blanks
003000040206     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003100040206     C                   ELSE
003200040206     C                   MOVEL     ','           wSepCampi         1
003300040206     C                   ENDIF
003400060720     C*
003500060720     C* Verifico se richiesto nessun delimitatore testo
003600060720     C*   => *blanks = standard = "
003700060720     C*   => N       = nessun delimitatore testo
003800060720     C                   EVAL      wDlmTxt = '"'
003900060720     C                   IF        %subst(prmppt:2:1) = 'N'
004000060720     C                   EVAL      wDlmTxt = *blanks
004100060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
004200060720     C                   ENDIF
004300030325     C*
004400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004600060720     C                   EVAL      VASXDTA =
004700060720     C                                wDlmTxt+'VASLNP'+wDlmTxt+wSepCampi+
004800060720     C                                wDlmTxt+'VASLNA'+wDlmTxt+wSepCampi+
004900060720     C                                wDlmTxt+'VASAAS'+wDlmTxt+wSepCampi+
005000060720     C                                wDlmTxt+'VASNRS'+wDlmTxt+wSepCampi+
005100060720     C                                wDlmTxt+'VASNSP'+wDlmTxt+wSepCampi+
005200060720     C                                wDlmTxt+'VASRMN'+wDlmTxt+wSepCampi+
005300060720     C                                wDlmTxt+'VASCAS'+wDlmTxt+wSepCampi+
005400060720     C                                wDlmTxt+'VASVCA'+wDlmTxt+wSepCampi+
005500060720     C                                wDlmTxt+'VASNRA'+wDlmTxt+wSepCampi+
005600060720     C                                wDlmTxt+'VASDTE'+wDlmTxt+wSepCampi+
005700060720     C                                wDlmTxt+'VASTPI'+wDlmTxt+wSepCampi+
005800060720     C                                wDlmTxt+'VASABI'+wDlmTxt+wSepCampi+
005900060720     C                                wDlmTxt+'VASCAI'+wDlmTxt+wSepCampi+
006000060720     C                                wDlmTxt+'VASNDP'+wDlmTxt+wSepCampi+
006100060720     C                                wDlmTxt+'VASDDP'+wDlmTxt+wSepCampi+
006200060720     C                                wDlmTxt+'VASCCB'+wDlmTxt+wSepCampi+
006300060720     C                                wDlmTxt+'VASABP'+wDlmTxt+wSepCampi+
006400060720     C                                wDlmTxt+'VASCAP'+wDlmTxt+wSepCampi+
006500060720     C                                wDlmTxt+'VASFPC'+wDlmTxt+wSepCampi+
006600060720     C                                wDlmTxt+'VASDVA'+wDlmTxt+wSepCampi+
006700060720     C                                wDlmTxt+'VASKSC'+wDlmTxt+wSepCampi+
006800110328     C                                wDlmTxt+'VASRMA'+wDlmTxt+wSepCampi+
006900110328     C                                wDlmTxt+'VASRMO'+wDlmTxt
007000030325     C*
007100030325     C                   WRITE     TNVASX00
007200030325     C                   ENDIF
007300991027     C*
007400090122     C                   READ      tivgdtmp
007500090122     C                   DOW       not %eof(tivgdtmp)
007600090122     C                   movel     vgddta        tnvasds
007700110328     C*
007800110328     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
007900110328     C                   MOVEL     *blanks       wRMO             35
008000110328     C                   EVAL      taaTRC = 'O'
008100110328     C     KEYtaa30      CHAIN     titaa30c
008200110328     C                   IF        %found(titaa30c)
008300110328     C                   EVAL      wRMO = TAARSC
008400110328     C                   ENDIF
008500030325     C*
008600060720     C                   EVAL      VASXDTA=
008700060720     C                                   %trim(%editc(VASLNP:'Q'))+wSepCampi+
008800060720     C                                   %trim(%editc(VASLNA:'Q'))+wSepCampi+
008900060720     C                                   %trim(%editc(VASAAS:'Q'))+wSepCampi+
009000060720     C                                   %trim(%editc(VASNRS:'Q'))+wSepCampi+
009100060720     C                                   %trim(%editc(VASNSP:'Q'))+wSepCampi+
009200060720     C                                   %trim(%editc(VASRMN:'Q'))+wSepCampi+
009300060720     C                                   %trim(%editc(VASCAS:'Q'))+wSepCampi+
009400060720     C                                  wDlmTxt+%trim(VASVCA)+wDlmTxt+wSepCampi+
009500060720     C                                  wDlmTxt+%trim(VASNRA)+wDlmTxt+wSepCampi+
009600060720     C                                   %trim(%editc(VASDTE:'Q'))+wSepCampi+
009700060720     C                                  wDlmTxt+%trim(VASTPI)+wDlmTxt+wSepCampi+
009800060720     C                                   %trim(%editc(VASABI:'Q'))+wSepCampi+
009900060720     C                                   %trim(%editc(VASCAI:'Q'))+wSepCampi+
010000060720     C                                   %trim(%editc(VASNDP:'Q'))+wSepCampi+
010100060720     C                                   %trim(%editc(VASDDP:'Q'))+wSepCampi+
010200060720     C                                  wDlmTxt+%trim(VASCCB)+wDlmTxt+wSepCampi+
010300060720     C                                   %trim(%editc(VASABP:'Q'))+wSepCampi+
010400060720     C                                   %trim(%editc(VASCAP:'Q'))+wSepCampi+
010500060720     C                                  wDlmTxt+%trim(VASFPC)+wDlmTxt+wSepCampi+
010600060720     C                                   %trim(%editc(VASDVA:'Q'))+wSepCampi+
010700060720     C                                   %trim(%editc(VASKSC:'Q'))+wSepCampi+
010800110328     C                                  wDlmTxt+%trim(VASRMA)+wDlmTxt+wSepCampi+
010900110328     C                                  wDlmTxt+%trim(wRMO)+wDlmTxt
011000930409     C*
011100030325     C                   WRITE     TNVASX00
011200090122     C                   DELETE    tivgd000
011300991027     C*
011400090122     C                   READ      tivgdtmp
011500030325     C                   ENDDO
011600991027     C*
011700030325     C                   EVAL      wrkesito = '0'
011800991027     C*
011900910830     C                   ENDSR
012000991027
012100991027      /TITLE Operazioni iniziali.
012200991027     C     *inzsr        BEGSR
012300991027     C*
012400991027     C     *ENTRY        PLIST
012500991027     C                   parm                    prmppt
012600991027     C     wrkesito      parm      wrkesito      prmesito
012700110328     C*
012800110328     C* Definizione chiavi di lettura
012900110328     C*
013000110328     C* TITAA30C - Completa
013100110328     C     KEYtaa30      KLIST
013200110328     C                   KFLD                    vasAAS
013300110328     C                   KFLD                    vasLNP
013400110328     C                   KFLD                    vasNRS
013500110328     C                   KFLD                    vasNSP
013600110328     C                   KFLD                    taaTRC
013700991027     C*
013800991027     C                   ENDSR
