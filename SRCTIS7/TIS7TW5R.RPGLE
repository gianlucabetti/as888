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
001100080213     D wfs7w52ds     e ds                  extname(wfs7w52f)
001200991027
001300080213
001400070625     C*
001500030908     C                   EXSR      chkRECFILMBR
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800030908
001900070625
002000070625
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300060410     C                   READ      TIVAWWWT                               55
002400030908     C*
002500030908     C                   ENDSR
002600070625
002700070625
002800070625
002900991027     C     traduci       BEGSR
003000051007     C*
003100051007     C* Verifico se richiesto un particolare carattere d separatore campi
003200080213     C                   IF        %subst(prmppt:2:1) <> *blanks
003300080213     C                   EVAL      wSepCampi = %subst(prmppt:2:1)
003400051007     C                   ELSE
003500051007     C                   MOVEL     ','           wSepCampi         1
003600051007     C                   ENDIF
003700030325     C*
003800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004000080213     C                   EVAL      VAWDTA  =
004100080213     C                             '"CODICE CLIENTE"'                +wSepCampi+
004200080213     C                             '"FILIALE ARRIVO"'                +wSepCampi+
004300080213     C                             '"DESCRIZIONE FILIARE ARRIVO"'    +wSepCampi+
004400080213     C                             '"DATA SPEDIZIONE"'               +wSepCampi+
004500080213     C                             '"FILIALE PARTENZA"'              +wSepCampi+
004600080213     C                             '"SERIE SPEDIZIONE"'              +wSepCampi+
004700080213     C                             '"NUMERO SPEDIZIONE"'             +wSepCampi+
004800080213     C                             '"RIFERIMENTO MITTENTE NUM."'     +wSepCampi+
004900080213     C                             '"RIFERIMENTO MITTENTE ALFA"'     +wSepCampi+
005000080213     C                             '"RAG. SOC. DESTINATARIO"'        +wSepCampi+
005100080213     C                             '"INDIRIZZO DESTINATARIO"'        +wSepCampi+
005200080213     C                             '"CAP DESTINATARIO"'              +wSepCampi+
005300080213     C                             '"LOCALITA DESTINATARIO"'         +wSepCampi+
005400080213     C                             '"PROVINCIA DESTINATARIO"'        +wSepCampi+
005500080213     C                             '"DATA CONSEGNA"'                 +wSepCampi+
005600080213     C                             '"STATO SPEDIZIONE"'              +wSepCampi+
005700080213     C                             '"TEMPO CONSEGNA"'
005800030325     C*
005900060410     C                   WRITE     TIVAW000
006000030325     C                   ENDIF
006100991027     C*
006200060410     C                   READ      TIVGDTMP
006300060410     C                   DOW       not %eof(TIVGDTMP)
006400060411     C*
006500060411     C* Valorizzo il buffer dal campo flat di inout
006600080213     C                   EVAL      wfs7w52ds = vgdDTA
006700051007     C*
006800080213     C                   EVAL      VAWDTA =
006900080213     C                                      '"'+%trim(W52KSC)+'"' +wSepCampi+
007000080213     C                                   %trim(%editc(W52LNA:'Q'))+wSepCampi+
007100080213     C                                   '"'+%trim(W52LNADES)+'"' +wSepCampi+
007200080213     C                                   %trim(%editc(W52DSP:'Q'))+wSepCampi+
007300080213     C                                   %trim(%editc(W52LNP:'Q'))+wSepCampi+
007400080213     C                                   %trim(%editc(W52NRS:'Q'))+wSepCampi+
007500080213     C                                   %trim(%editc(W52NSP:'Q'))+wSepCampi+
007600080213     C                                   %trim(%editc(W52RMN:'Q'))+wSepCampi+
007700080213     C                                      '"'+%trim(W52RMA)+'"' +wSepCampi+
007800080213     C                                      '"'+%trim(W52RSD)+'"' +wSepCampi+
007900080213     C                                      '"'+%trim(W52IND)+'"' +wSepCampi+
008000080213     C                                      '"'+%trim(W52CAD)+'"' +wSepCampi+
008100080213     C                                      '"'+%trim(W52LOD)+'"' +wSepCampi+
008200080213     C                                      '"'+%trim(W52PRD)+'"' +wSepCampi+
008300080213     C                                   %trim(%editc(W52DCM:'Q'))+wSepCampi+
008400080213     C                                     '"'+%trim(W52TIPO)+'"' +wSepCampi+
008500080213     C                                     '"'+%trim(W52VALORE)+'"'
008600080213     C*
008700060410     C                   WRITE     TIVAW000
008800060410     C                   DELETE    TIVGD000
008900991027     C*
009000060410     C                   READ      TIVGDTMP
009100030325     C                   ENDDO
009200991027     C*
009300030325     C                   EVAL      wrkesito = '0'
009400991027     C*
009500910830     C                   ENDSR
009600070625
009700070625
009800991027
009900991027      /TITLE Operazioni iniziali.
010000991027     C     *inzsr        BEGSR
010100991027     C*
010200991027     C     *ENTRY        PLIST
010300991027     C                   parm                    prmppt
010400991027     C     wrkesito      parm      wrkesito      prmesito
010500991027     C*
010600991027     C                   ENDSR
