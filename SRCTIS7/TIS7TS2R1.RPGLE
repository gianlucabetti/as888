000100100610     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500100610     FTIVAWWWT  uF A E             DISK    commit
000600150929     FTITAH30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200150929     D wNNC            s             35
001300060720
001400060720     D wDlmTxt         s              5    varying inz('')
001500060720
001600991027
001700030908     C                   EXSR      chkRECFILMBR
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000150929
002100150929
002200030908
002300030908     C     chkRECFILMBR  BEGSR
002400030908     C*
002500100610     C                   READ      TIVAWWWT                               55
002600030908     C*
002700030908     C                   ENDSR
002800150929
002900150929
003000991027
003100991027     C     traduci       BEGSR
003200040206     C*
003300040206     C* Verifico se richiesto un particolare carattere d separatore campi
003400040206     C                   IF        %subst(prmppt:3:1) <> *blanks
003500040206     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003600040206     C                   ELSE
003700100610     C                   MOVEL     ';'           wSepCampi         1
003800040206     C                   ENDIF
003900060720     C*
004000060720     C* Verifico se richiesto nessun delimitatore testo
004100060720     C*   => *blanks = standard = "
004200060720     C*   => N       = nessun delimitatore testo
004300060720     C                   EVAL      wDlmTxt = '"'
004400060720     C                   IF        %subst(prmppt:2:1) = 'N'
004500060720     C                   EVAL      wDlmTxt = *blanks
004600060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
004700060720     C                   ENDIF
004800030325     C*
004900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005100100610     C                   EVAL      VAWDTA  =
005200060720     C                                wDlmTxt+'VASLNP'+wDlmTxt+wSepCampi+
005300060720     C                                wDlmTxt+'VASLNA'+wDlmTxt+wSepCampi+
005400060720     C                                wDlmTxt+'VASAAS'+wDlmTxt+wSepCampi+
005500060720     C                                wDlmTxt+'VASNRS'+wDlmTxt+wSepCampi+
005600060720     C                                wDlmTxt+'VASNSP'+wDlmTxt+wSepCampi+
005700060720     C                                wDlmTxt+'VASRMN'+wDlmTxt+wSepCampi+
005800060720     C                                wDlmTxt+'VASCAS'+wDlmTxt+wSepCampi+
005900060720     C                                wDlmTxt+'VASVCA'+wDlmTxt+wSepCampi+
006000060720     C                                wDlmTxt+'VASNRA'+wDlmTxt+wSepCampi+
006100060720     C                                wDlmTxt+'VASDTE'+wDlmTxt+wSepCampi+
006200060720     C                                wDlmTxt+'VASTPI'+wDlmTxt+wSepCampi+
006300060720     C                                wDlmTxt+'VASABI'+wDlmTxt+wSepCampi+
006400060720     C                                wDlmTxt+'VASCAI'+wDlmTxt+wSepCampi+
006500060720     C                                wDlmTxt+'VASNDP'+wDlmTxt+wSepCampi+
006600060720     C                                wDlmTxt+'VASDDP'+wDlmTxt+wSepCampi+
006700060720     C                                wDlmTxt+'VASCCB'+wDlmTxt+wSepCampi+
006800060720     C                                wDlmTxt+'VASABP'+wDlmTxt+wSepCampi+
006900060720     C                                wDlmTxt+'VASCAP'+wDlmTxt+wSepCampi+
007000060720     C                                wDlmTxt+'VASFPC'+wDlmTxt+wSepCampi+
007100060720     C                                wDlmTxt+'VASDVA'+wDlmTxt+wSepCampi+
007200060720     C                                wDlmTxt+'VASKSC'+wDlmTxt+wSepCampi+
007300150929     C                                wDlmTxt+'VASRMA'+wDlmTxt
007400150929     C*
007500150929     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
007600150929     C                   IF        %subst(prmppt:4:1) = 'S'
007700150929     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
007800150929     C                                wDlmTxt+'SEGNAC.'+wDlmTxt
007900150929     C                   ENDIF
008000150929     C*
008100100610     C                   WRITE     TIVAW000
008200030325     C                   ENDIF
008300991027     C*
008400090122     C                   READ      tivgdtmp
008500090122     C                   DOW       not %eof(tivgdtmp)
008600090122     C                   movel     vgddta        tnvasds
008700150929     C*
008800100610     C                   EVAL      VAWDTA =
008900060720     C                                   %trim(%editc(VASLNP:'Q'))+wSepCampi+
009000060720     C                                   %trim(%editc(VASLNA:'Q'))+wSepCampi+
009100060720     C                                   %trim(%editc(VASAAS:'Q'))+wSepCampi+
009200060720     C                                   %trim(%editc(VASNRS:'Q'))+wSepCampi+
009300060720     C                                   %trim(%editc(VASNSP:'Q'))+wSepCampi+
009400060720     C                                   %trim(%editc(VASRMN:'Q'))+wSepCampi+
009500060720     C                                   %trim(%editc(VASCAS:'Q'))+wSepCampi+
009600060720     C                                  wDlmTxt+%trim(VASVCA)+wDlmTxt+wSepCampi+
009700060720     C                                  wDlmTxt+%trim(VASNRA)+wDlmTxt+wSepCampi+
009800060720     C                                   %trim(%editc(VASDTE:'Q'))+wSepCampi+
009900060720     C                                  wDlmTxt+%trim(VASTPI)+wDlmTxt+wSepCampi+
010000060720     C                                   %trim(%editc(VASABI:'Q'))+wSepCampi+
010100060720     C                                   %trim(%editc(VASCAI:'Q'))+wSepCampi+
010200060720     C                                   %trim(%editc(VASNDP:'Q'))+wSepCampi+
010300060720     C                                   %trim(%editc(VASDDP:'Q'))+wSepCampi+
010400060720     C                                  wDlmTxt+%trim(VASCCB)+wDlmTxt+wSepCampi+
010500060720     C                                   %trim(%editc(VASABP:'Q'))+wSepCampi+
010600060720     C                                   %trim(%editc(VASCAP:'Q'))+wSepCampi+
010700060720     C                                  wDlmTxt+%trim(VASFPC)+wDlmTxt+wSepCampi+
010800060720     C                                   %trim(%editc(VASDVA:'Q'))+wSepCampi+
010900060720     C                                   %trim(%editc(VASKSC:'Q'))+wSepCampi+
011000060720     C                                  wDlmTxt+%trim(VASRMA)+wDlmTxt
011100150929     C*
011200150929     C* Se richiesto nei parametri l'indicazone anche del primo segnacollo cliente ("chi sono")
011300150929     C                   IF        %subst(prmppt:4:1) = 'S'
011400150929     C*
011500150929     C                   SETOFF                                       60
011600150929     C                   CLEAR                   wNNC
011700150929     C*
011800150929     C* Aggancio il dettaglio colli
011900150929     C                   EVAL      tahTRC = 'C'
012000150929     C     KEYtah30_C    SETLL     titah30c
012100150929     C                   IF        %equal(titah30c)
012200150929     C     KEYtah30_C    READE     titah30c
012300150929     C                   DOW       not %eof(titah30c) AND not *IN60
012400150929     C                   EVAL      wNNC = tahNOT
012500150929     C                   SETON                                        60
012600150929     C     KEYtah30_C    READE     titah30c
012700150929     C                   ENDDO
012800150929     C                   ENDIF
012900150929     C*
013000150929     C                   EVAL      VAWDTA  = %trim(VAWDTA)+wSepCampi+
013100150929     C                                wDlmTxt+wNNC+wDlmTxt
013200150929     C                   ENDIF
013300930409     C*
013400100610     C                   WRITE     TIVAW000
013500090122     C                   DELETE    tivgd000
013600991027     C*
013700090122     C                   READ      tivgdtmp
013800030325     C                   ENDDO
013900991027     C*
014000030325     C                   EVAL      wrkesito = '0'
014100991027     C*
014200910830     C                   ENDSR
014300150929
014400150929
014500991027
014600991027      /TITLE Operazioni iniziali.
014700991027     C     *inzsr        BEGSR
014800991027     C*
014900991027     C     *ENTRY        PLIST
015000991027     C                   parm                    prmppt
015100991027     C     wrkesito      parm      wrkesito      prmesito
015200150929     C*
015300150929     C* Chiave su TITAH30C - completa
015400150929     C     KEYtah30_C    KLIST
015500150929     C                   KFLD                    vasAAS
015600150929     C                   KFLD                    vasLNP
015700150929     C                   KFLD                    vasNRS
015800150929     C                   KFLD                    vasNSP
015900150929     C                   KFLD                    tahTRC
016000991027     C*
016100991027     C                   ENDSR
