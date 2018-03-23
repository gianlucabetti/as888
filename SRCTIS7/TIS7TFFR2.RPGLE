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
001700130305
001800030908     C     chkRECFILMBR  BEGSR
001900030908     C*
002000060410     C                   READ      TIVAWWWT                               55
002100030908     C*
002200030908     C                   ENDSR
002300130305
002400991027
002500991027     C     traduci       BEGSR
002600051007     C*
002700051007     C* Verifico se richiesto un particolare carattere d separatore campi
002800051007     C                   IF        %subst(prmppt:3:1) <> *blanks
002900051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003000051007     C                   ELSE
003100051007     C                   MOVEL     ','           wSepCampi         1
003200051007     C                   ENDIF
003300051007     C*
003400051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003500051007     C                   SETON                                        60
003600051007     C                   IF        %subst(prmppt:4:1) = ','
003700051007     C                   SETOFF                                       60
003800051007     C                   ENDIF
003900030325     C*
004000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004200130305     C                   EVAL      VAWDTA  = '"CLIENTE             "'+wSepCampi+
004300130305     C                                       '"ANNO/MESE CONSEGNA  "'+wSepCampi+
004400130305     C                                       '"REGIONE DEST        "'+wSepCampi+
004500130305     C                                       '"PROVINCIA DEST      "'+wSepCampi+
004600130305     C                                       '"CAP DEST            "'+wSepCampi+
004700130305     C                                       '"INOLTRO             "'+wSepCampi+
004800110307     C                                       '"S=DIR+GIAC+CCA      "'+wSepCampi+
004900110307     C                                       '"TEMPO RESA TEORICO  "'+wSepCampi+
005000110307     C                                       '"# >= -999 <= -24    "'+wSepCampi+
005100110307     C                                       '"# >   -24 <= -12    "'+wSepCampi+
005200110307     C                                       '"# >   -12 <=   0    "'+wSepCampi+
005300110307     C                                       '"# >     0           "'+wSepCampi+
005400130305     C                                       '"# TOTALE SPEDIZIONI "'
005500030325     C*
005600060410     C                   WRITE     TIVAW000
005700030325     C                   ENDIF
005800991027     C*
005900060410     C                   READ      TIVGDTMP
006000060410     C                   DOW       not %eof(TIVGDTMP)
006100060411     C*
006200060411     C* Valorizzo il buffer dal campo flat di inout
006300110307     C                   EVAL      wfcln10ds = vgdDTA
006400060411     C*
006500110307     C                   EVAL      VAWDTA =
006600110307     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
006700110307     C                             %trim(%editc(WCLNAAAAMM     :'X'))+wSepCampi+
006800110307     C                             %trim(       WCLNREG             )+wSepCampi+
006900110307     C                             %trim(       WCLNPRD             )+wSepCampi+
007000110307     C                             %trim(       WCLNCAD             )+wSepCampi+
007100110307     C                             %trim(       WCLNISO             )+wSepCampi+
007200110307     C                             %trim(       WCLNANOM            )+wSepCampi+
007300110307     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
007400110307     C                             %trim(%editc(WCLNANT        :'Q'))+wSepCampi+
007500110307     C                             %trim(%editc(WCLNM12        :'Q'))+wSepCampi+
007600110307     C                             %trim(%editc(WCLNOKS        :'Q'))+wSepCampi+
007700110307     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
007800110307     C                             %trim(%editc(WCLNTOT        :'Q'))+wSepCampi
007900930409     C*
008000060410     C                   WRITE     TIVAW000
008100060410     C                   DELETE    TIVGD000
008200991027     C*
008300060410     C                   READ      TIVGDTMP
008400030325     C                   ENDDO
008500991027     C*
008600030325     C                   EVAL      wrkesito = '0'
008700991027     C*
008800910830     C                   ENDSR
008900991027
009000110307
009100110307
009200110307
009300991027      /TITLE Operazioni iniziali.
009400991027     C     *inzsr        BEGSR
009500991027     C*
009600991027     C     *ENTRY        PLIST
009700991027     C                   parm                    prmppt
009800991027     C     wrkesito      parm      wrkesito      prmesito
009900991027     C*
010000991027     C                   ENDSR
