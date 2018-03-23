000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600160420     FTITAS30C  IF   E           K DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200110307     D wfcln10ds     e ds                  extname(wfcln10f)
001300991027
001400030908     C                   EXSR      chkRECFILMBR
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700030908
001800030908     C     chkRECFILMBR  BEGSR
001900030908     C*
002000060410     C                   READ      TIVAWWWT                               55
002100030908     C*
002200030908     C                   ENDSR
002300991027
002400991027     C     traduci       BEGSR
002500051007     C*
002600051007     C* Verifico se richiesto un particolare carattere d separatore campi
002700051007     C                   IF        %subst(prmppt:3:1) <> *blanks
002800051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
002900051007     C                   ELSE
003000051007     C                   MOVEL     ','           wSepCampi         1
003100051007     C                   ENDIF
003200051007     C*
003300051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003400051007     C                   SETON                                        60
003500051007     C                   IF        %subst(prmppt:4:1) = ','
003600051007     C                   SETOFF                                       60
003700051007     C                   ENDIF
003800030325     C*
003900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004100110307     C                   EVAL      VAWDTA  = '"CCM CLIENTE         "'+wSepCampi+
004200110307     C                                       '"ANNO SPED           "'+wSepCampi+
004300110307     C                                       '"LINEA PART SPED     "'+wSepCampi+
004400160621     C                                       '"LINEA ARR SPED      "'+wSepCampi+
004500110307     C                                       '"SERIE SPED          "'+wSepCampi+
004600110307     C                                       '"NUMERO SPED         "'+wSepCampi+
004700110307     C                                       '"ANNO/MESE           "'+wSepCampi+
004800110307     C                                       '"REGIONE             "'+wSepCampi+
004900110307     C                                       '"PROVINCIA           "'+wSepCampi+
005000110307     C                                       '"CAP                 "'+wSepCampi+
005100160420     C                                       '"DESTINATARIO        "'+wSepCampi+
005200160420     C                                       '"INDIRIZZO           "'+wSepCampi+
005300160420     C                                       '"RIF. NUMERICO       "'+wSepCampi+
005400160420     C                                       '"DATA CONS. RICHIESTA"'+wSepCampi+
005500160420     C                                       '"TIPO CONS. RICHIESTA"'+wSepCampi+
005600160420     C                                       '"I=ISOLA             "'+wSepCampi+
005700160420     C                                       '"S=DIR+GIAC+CCA      "'+wSepCampi+
005800160426     C                                       '"PESO (KG)           "'+wSepCampi+
005900160426     C                                       '"ESISTE PRATICA CA   "'+wSepCampi+
006000160420     C                                       '"TEMPO RESA          "'+wSepCampi+
006100160420     C                                       '"TEMPO RESA TEORICO  "'+wSepCampi+
006200110307     C                                       '"# >= -999 <= -24    "'+wSepCampi+
006300110307     C                                       '"# >   -24 <= -12    "'+wSepCampi+
006400110307     C                                       '"# >   -12 <=   0    "'+wSepCampi+
006500110307     C                                       '"# >     0           "'+wSepCampi+
006600110307     C                                       '"# TOTALI            "'
006700030325     C*
006800060410     C                   WRITE     TIVAW000
006900030325     C                   ENDIF
007000991027     C*
007100060410     C                   READ      TIVGDTMP
007200060410     C                   DOW       not %eof(TIVGDTMP)
007300160420     C*
007400160420     C* Valorizzo il buffer dal campo flat di inout
007500160420     C                   EVAL      wfcln10ds = vgdDTA
007600160420     C*
007700160420     C* Innanzitutto chaino la bolla direttamente su TITAS
007800160420     C     KEYtas30P     CHAIN     titas30c
007900160420     C                   IF        %found(titas30c)
008000060411     C*
008100110307     C                   EVAL      VAWDTA =
008200110307     C                             %trim(%editc(WCLNCCM        :'X'))+wSepCampi+
008300110307     C                             %trim(%editc(WCLNAAS        :'X'))+wSepCampi+
008400110307     C                             %trim(%editc(WCLNLNP        :'X'))+wSepCampi+
008500160621     C                             %trim(%editc(tasLNA         :'X'))+wSepCampi+
008600110307     C                             %trim(%editc(WCLNNRS        :'X'))+wSepCampi+
008700110307     C                             %trim(%editc(WCLNNSP        :'X'))+wSepCampi+
008800110307     C                             %trim(%editc(WCLNAAAAMM     :'X'))+wSepCampi+
008900110307     C                             %trim(       WCLNREG             )+wSepCampi+
009000110307     C                             %trim(       WCLNPRD             )+wSepCampi+
009100110307     C                             %trim(       WCLNCAD             )+wSepCampi+
009200160420     C                             %trim(       tasRSD              )+wSepCampi+
009300160420     C                             %trim(       tasIND              )+wSepCampi+
009400160420     C                             %trim(%editc(tasRMN         :'Q'))+wSepCampi+
009500160420     C                             %trim(%editc(tasDCR         :'X'))+wSepCampi+
009600160420     C                             %trim(       tasTCR              )+wSepCampi+
009700110307     C                             %trim(       WCLNISO             )+wSepCampi+
009800110307     C                             %trim(       WCLNANOM            )+wSepCampi+
009900160426     C                             %trim(%editc(tasPKB         :'Q'))+wSepCampi+
010000160426     C                             %trim(       tasFDA              )+wSepCampi+
010100160420     C                             %trim(%editc(tasNRC         :'Q'))+wSepCampi+
010200110307     C                             %trim(%editc(WCLNDLYTRC     :'Q'))+wSepCampi+
010300110307     C                             %trim(%editc(WCLNANT        :'Q'))+wSepCampi+
010400110307     C                             %trim(%editc(WCLNM12        :'Q'))+wSepCampi+
010500110307     C                             %trim(%editc(WCLNOKS        :'Q'))+wSepCampi+
010600110307     C                             %trim(%editc(WCLNRIT        :'Q'))+wSepCampi+
010700110307     C                             %trim(%editc(WCLNTOT        :'Q'))+wSepCampi
010800930409     C*
010900060410     C                   WRITE     TIVAW000
011000160420     C                   ENDIF
011100160420     C*
011200060410     C                   DELETE    TIVGD000
011300991027     C*
011400060410     C                   READ      TIVGDTMP
011500030325     C                   ENDDO
011600991027     C*
011700030325     C                   EVAL      wrkesito = '0'
011800991027     C*
011900910830     C                   ENDSR
012000991027
012100110307
012200110307
012300110307
012400991027      /TITLE Operazioni iniziali.
012500991027     C     *inzsr        BEGSR
012600991027     C*
012700991027     C     *ENTRY        PLIST
012800991027     C                   parm                    prmppt
012900991027     C     wrkesito      parm      wrkesito      prmesito
013000160420     C*
013100160420     C* Chiave su TITAS30C - Parziale
013200160420     C     KEYtas30P     KLIST
013300160420     C                   KFLD                    WCLNAAS
013400160420     C                   KFLD                    WCLNLNP
013500160420     C                   KFLD                    WCLNNRS
013600160420     C                   KFLD                    WCLNNSP
013700991027     C*
013800991027     C                   ENDSR
