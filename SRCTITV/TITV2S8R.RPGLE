000100141002     H DECEDIT('0,') DATEDIT(*DMY.)
000200141002     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
001500141002
001600141002     D wDATA           s             10    inz(*blanks)
001700141002     D wCAS            s             13S 2 inz
001800141002
001900141002     D*------------------
002000141002     D* LINKING A DEFINIZIONI ESTERNE
002100141002     D*------------------
002200141002     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002300141002     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
002400141002
002500130503
002600991027
002700030908     C                   EXSR      chkRECFILMBR
002800991027     C                   EXSR      traduci
002900921023     C                   SETON                                        LR
003000141002
003100030908
003200030908     C     chkRECFILMBR  BEGSR
003300030908     C*
003400090415     C                   READ      TIVAWWWT                               55
003500030908     C*
003600030908     C                   ENDSR
003700141002
003800991027
003900991027     C     traduci       BEGSR
004000040206     C*
004100040206     C* Verifico se richiesto un particolare carattere d separatore campi
004200040206     C                   IF        %subst(prmppt:3:1) <> *blanks
004300130502     C                   EVAL      wSep = %subst(prmppt:3:1)
004400040206     C                   ELSE
004500130502     C                   MOVEL     ','           wSep              1
004600040206     C                   ENDIF
004700060720     C*
004800060720     C* Verifico se richiesto nessun delimitatore testo
004900060720     C*   => *blanks = standard = "
005000060720     C*   => N       = nessun delimitatore testo
005100060720     C                   EVAL      wDlmTxt = '"'
005200060720     C                   IF        %subst(prmppt:2:1) = 'N'
005300060720     C                   EVAL      wDlmTxt = *blanks
005400060720     C                   EVAL      wDlmTxt = %trim(wDlmTxt)
005500060720     C                   ENDIF
005600030325     C*
005700030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005900090415     C                   EVAL      VAWDTA =
006000140930     C                             wDlmTxt+'Lettera di Vettura'   +wDlmTxt+wSep+
006100140930     C                             wDlmTxt+'Numero riferimento '  +
006200140930     C                                     'spedizione'           +wDlmTxt+wSep+
006300140930     C                             wDlmTxt+'Codice cliente'       +wDlmTxt+wSep+
006400140930     C                             wDlmTxt+'Data di accredito'    +wDlmTxt+wSep+
006500140930     C                             wDlmTxt+'Destinatario'         +wDlmTxt+wSep+
006600140930     C                             wDlmTxt+'Importo contrassegno' +wDlmTxt+wSep+
006700140930     C                             wDlmTxt+'Modalita di pagamento'+wDlmTxt+wSep+
006800140930     C                             wDlmTxt+'Codice IBAN'          +wDlmTxt+wSep
006900030325     C*
007000090415     C                   WRITE     TIVAW000
007100030325     C                   ENDIF
007200991027     C*
007300090122     C                   READ      tivgdtmp
007400090122     C                   DOW       not %eof(tivgdtmp)
007500090122     C                   movel     vgddta        tnvasds
007600141002     C*
007700141002     C                   clear                   wDATA
007800141002     C                   clear                   wCAS
007900090415     C*
008000090415     C* "Chaino" la bolla x reperire i dati mancanti
008100090415     C     KEYtas30cP    CHAIN     TITAS30C
008200090415     C                   IF        %found(TITAS30C)
008300030325     C*
008400141002     C                   eval      wDATA    =
008500141002     C                              UBFMTDATE_Convert(
008600141002     C                                  %editc(VASDVA:'4') :
008700141002     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
008800141002     C                   eval      wCAS = VASCAS
008900141002     C*
009000090415     C                   EVAL      VAWDTA=
009100140804     C                             wDlmTxt+wApi+%trim(%editc(VASLNP:'X'))+
009200140804     C                                          %trim(%editc(VASNRS:'X'))+
009300140930     C                                          %trim(%editc(VASNSP:'X'))+wSep+
009400140919     C                                          %trim(VASRMA)            +wSep+
009500140804     C                                          %trim(%editc(VASKSC:'X'))+wSep+
009600141002     C                                          %trim(wDATA)             +wSep+
009700140804     C                                      wDlmTxt+%trim(TASRSD)+wDlmTxt+wSep+
009800141002     C                                          %trim(%editc(wCAS:'4'))  +wSep+
009900140804     C                                      wDlmTxt+' '+wDlmTxt          +wSep+
010000140804     C                                      wDlmTxt+%trim(VASCCB)+wDlmTxt
010100130502     C*
010200090415     C                   WRITE     TIVAW000
010300090122     C                   DELETE    tivgd000
010400090415     C*
010500090415     C                   ENDIF
010600991027     C*
010700090122     C                   READ      tivgdtmp
010800030325     C                   ENDDO
010900991027     C*
011000030325     C                   EVAL      wrkesito = '0'
011100991027     C*
011200910830     C                   ENDSR
011300991027
011400141002
011500991027      /TITLE Operazioni iniziali.
011600991027     C     *inzsr        BEGSR
011700991027     C*
011800991027     C     *ENTRY        PLIST
011900991027     C                   parm                    prmppt
012000991027     C     wrkesito      parm      wrkesito      prmesito
012100090415     C****
012200090415     C* Definizione chiavi
012300090415     C****
012400090415     C* Chiave su TITAS30C - parziale
012500090415     C     KEYtas30cP    KLIST
012600090415     C                   KFLD                    vasAAS
012700090415     C                   KFLD                    vasLNP
012800090415     C                   KFLD                    vasNRS
012900090415     C                   KFLD                    vasNSP
013000991027     C*
013100991027     C                   ENDSR
