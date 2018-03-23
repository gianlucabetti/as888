000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000030325
001100110307     D wfcln10ds     e ds                  extname(wfcln10f)
001200991027
001300030908     C                   EXSR      chkRECFILMBR
001400991027     C                   EXSR      traduci
001500921023     C                   SETON                                        LR
001600030908
001700030908     C     chkRECFILMBR  BEGSR
001800030908     C*
001900060410     C                   READ      TIVAWWWT                               55
002000030908     C*
002100030908     C                   ENDSR
002200991027
002300991027     C     traduci       BEGSR
002400051007     C*
002500051007     C* Verifico se richiesto un particolare carattere d separatore campi
002600051007     C                   IF        %subst(prmppt:3:1) <> *blanks
002700051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
002800051007     C                   ELSE
002900051007     C                   MOVEL     ','           wSepCampi         1
003000051007     C                   ENDIF
003100051007     C*
003200051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003300051007     C                   SETON                                        60
003400051007     C                   IF        %subst(prmppt:4:1) = ','
003500051007     C                   SETOFF                                       60
003600051007     C                   ENDIF
003700030325     C*
003800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004000110307     C                   EVAL      VAWDTA  = '"CCM CLIENTE         "'+wSepCampi+
004100110307     C                                       '"ANNO SPED           "'+wSepCampi+
004200110307     C                                       '"LINEA PART SPED     "'+wSepCampi+
004300110307     C                                       '"SERIE SPED          "'+wSepCampi+
004400110307     C                                       '"NUMERO SPED         "'+wSepCampi+
004500110307     C                                       '"ANNO/MESE           "'+wSepCampi+
004600110307     C                                       '"REGIONE             "'+wSepCampi+
004700110307     C                                       '"PROVINCIA           "'+wSepCampi+
004800110307     C                                       '"CAP                 "'+wSepCampi+
004900110307     C                                       '"I=ISOLA             "'+wSepCampi+
005000110307     C                                       '"S=DIR+GIAC+CCA      "'+wSepCampi+
005100110307     C                                       '"TEMPO RESA TEORICO  "'+wSepCampi+
005200110307     C                                       '"# >= -999 <= -24    "'+wSepCampi+
005300110307     C                                       '"# >   -24 <= -12    "'+wSepCampi+
005400110307     C                                       '"# >   -12 <=   0    "'+wSepCampi+
005500110307     C                                       '"# >     0           "'+wSepCampi+
005600110307     C                                       '"# TOTALI            "'
005700030325     C*
005800060410     C                   WRITE     TIVAW000
005900030325     C                   ENDIF
006000991027     C*
006100060410     C                   READ      TIVGDTMP
006200060410     C                   DOW       not %eof(TIVGDTMP)
006300060411     C*
006400060411     C* Valorizzo il buffer dal campo flat di inout
006500110307     C                   EVAL      wfcln10ds = vgdDTA
006600060411     C*
006700110307     C                   EVAL      VAWDTA =
006800110307     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
006900110307     C                             %trim(%editc(WCLNAAS        :'X'))+wSepCampi+
007000110307     C                             %trim(%editc(WCLNLNP        :'X'))+wSepCampi+
007100110307     C                             %trim(%editc(WCLNNRS        :'X'))+wSepCampi+
007200110307     C                             %trim(%editc(WCLNNSP        :'X'))+wSepCampi+
007300110307     C                             %trim(%editc(WCLNAAAAMM     :'X'))+wSepCampi+
007400110307     C                             %trim(       WCLNREG             )+wSepCampi+
007500110307     C                             %trim(       WCLNPRD             )+wSepCampi+
007600110307     C                             %trim(       WCLNCAD             )+wSepCampi+
007700110307     C                             %trim(       WCLNISO             )+wSepCampi+
007800110307     C                             %trim(       WCLNANOM            )+wSepCampi+
007900110307     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
008000110307     C                             %trim(%editc(WCLNANT        :'Q'))+wSepCampi+
008100110307     C                             %trim(%editc(WCLNM12        :'Q'))+wSepCampi+
008200110307     C                             %trim(%editc(WCLNOKS        :'Q'))+wSepCampi+
008300110307     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
008400110307     C                             %trim(%editc(WCLNTOT        :'Q'))+wSepCampi
008500930409     C*
008600060410     C                   WRITE     TIVAW000
008700060410     C                   DELETE    TIVGD000
008800991027     C*
008900060410     C                   READ      TIVGDTMP
009000030325     C                   ENDDO
009100991027     C*
009200030325     C                   EVAL      wrkesito = '0'
009300991027     C*
009400910830     C                   ENDSR
009500991027
009600110307
009700110307
009800110307
009900991027      /TITLE Operazioni iniziali.
010000991027     C     *inzsr        BEGSR
010100991027     C*
010200991027     C     *ENTRY        PLIST
010300991027     C                   parm                    prmppt
010400991027     C     wrkesito      parm      wrkesito      prmesito
010500991027     C*
010600991027     C                   ENDSR
