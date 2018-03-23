000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500090415     FTIVAWWWT  uF A E             DISK    commit
000600090415     FTITAS30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200060720
001300060720     D wDlmTxt         s              5    varying inz('')
001400130503     D wApi            s              1    inz('''')
001500130503
001600991027
001700030908     C                   EXSR      chkRECFILMBR
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000030908
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300090415     C                   READ      TIVAWWWT                               55
002400030908     C*
002500030908     C                   ENDSR
002600991027
002700991027     C     traduci       BEGSR
002800040206     C*
002900040206     C* Verifico se richiesto un particolare carattere d separatore campi
003000040206     C                   IF        %subst(prmppt:3:1) <> *blanks
003100130502     C                   EVAL      wSep = %subst(prmppt:3:1)
003200040206     C                   ELSE
003300130502     C                   MOVEL     ','           wSep              1
003400040206     C                   ENDIF
003500060720     C*
003600060720     C* Verifico se richiesto nessun delimitatore testo
003700060720     C*   => *blanks = standard = "
003800060720     C*   => N       = nessun delimitatore testo
003900060720     C                   EVAL      wDlmTxt = '"'
004000060720     C                   IF        %subst(prmppt:2:1) = 'N'
004100060720     C                   EVAL      wDlmTxt = *blanks
004200060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
004300060720     C                   ENDIF
004400030325     C*
004500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700090415     C                   EVAL      VAWDTA =
004800130502     C                                wDlmTxt+'SHIPM.YEAR'        +wDlmTxt+wSep+
004900130502     C                                wDlmTxt+'SHIPM.DEP.DEPOT'   +wDlmTxt+wSep+
005000130502     C                                wDlmTxt+'SHIPM.SERIES'      +wDlmTxt+wSep+
005100130502     C                                wDlmTxt+'SHIPM.NUMBER'      +wDlmTxt+wSep+
005200130502     C                                wDlmTxt+'SHIPM.DATE'        +wDlmTxt+wSep+
005300130502     C                                wDlmTxt+'CONSIGNEE NAME'    +wDlmTxt+wSep+
005400130502     C                                wDlmTxt+'CONSIGNEE PROVINCE'+wDlmTxt+wSep+
005500130502     C                                wDlmTxt+'SENDER REFERENCE 1'+wDlmTxt+wSep+
005600130502     C                                wDlmTxt+'SENDER REFERENCE 2'+wDlmTxt+wSep+
005700130502     C                                wDlmTxt+'COD AMOUNT'        +wDlmTxt+wSep+
005800130502     C                                wDlmTxt+'PARCEL ID'         +wDlmTxt+wSep
005900030325     C*
006000090415     C                   WRITE     TIVAW000
006100030325     C                   ENDIF
006200991027     C*
006300090122     C                   READ      tivgdtmp
006400090122     C                   DOW       not %eof(tivgdtmp)
006500090122     C                   movel     vgddta        tnvasds
006600090415     C*
006700090415     C* "Chaino" la bolla x reperire i dati mancanti
006800090415     C     KEYtas30cP    CHAIN     TITAS30C
006900090415     C                   IF        %found(TITAS30C)
007000030325     C*
007100090415     C                   EVAL      VAWDTA=
007200130502     C                                   %trim(%editc(VASAAS:'X'))+wSep+
007300130502     C                                   %trim(%editc(VASLNP:'X'))+wSep+
007400130502     C                                   %trim(%editc(VASNRS:'X'))+wSep+
007500130502     C                                   %trim(%editc(VASNSP:'X'))+wSep+
007600130503     C                             %trim(%editc(TASAAS*10000+TASMGS:'X'))+wSep+
007700130502     C                                  wDlmTxt+%trim(TASRSD)+wDlmTxt+wSep+
007800130502     C                                  wDlmTxt+%trim(TASPRD)+wDlmTxt+wSep+
007900130502     C                                   %trim(%editc(VASRMN:'X'))+wSep+
008000130502     C                                  wDlmTxt+%trim(VASRMA)+wDlmTxt+wSep+
008100130502     C                                   %trim(%editc(VASCAS:'4'))+wSep+
008200130503     C                             wDlmTxt+wApi+%trim(%editc(TASLNP:'X'))+
008300130502     C                                          %trim(%editc(TASLNA:'X'))+
008400130502     C                                          %trim(%editc(TASNRS:'X'))+
008500130502     C                                          %trim(%editc(TASNCD:'X'))+
008600130502     C                                  wDlmTxt+wSep
008700130502     C*
008800090415     C                   WRITE     TIVAW000
008900090122     C                   DELETE    tivgd000
009000090415     C*
009100090415     C                   ENDIF
009200991027     C*
009300090122     C                   READ      tivgdtmp
009400030325     C                   ENDDO
009500991027     C*
009600030325     C                   EVAL      wrkesito = '0'
009700991027     C*
009800910830     C                   ENDSR
009900991027
010000991027      /TITLE Operazioni iniziali.
010100991027     C     *inzsr        BEGSR
010200991027     C*
010300991027     C     *ENTRY        PLIST
010400991027     C                   parm                    prmppt
010500991027     C     wrkesito      parm      wrkesito      prmesito
010600090415     C****
010700090415     C* Definizione chiavi
010800090415     C****
010900090415     C* Chiave su TITAS30C - parziale
011000090415     C     KEYtas30cP    KLIST
011100090415     C                   KFLD                    vasAAS
011200090415     C                   KFLD                    vasLNP
011300090415     C                   KFLD                    vasNRS
011400090415     C                   KFLD                    vasNSP
011500991027     C*
011600991027     C                   ENDSR
