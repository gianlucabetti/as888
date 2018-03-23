000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200140804     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500090416     Ftivgdtmp  UF   E             DISK
000600090309     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800090416     D tivafds       e ds                  extname(tivaf00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200050414
001300050414     D****
001400050414     D* VARIABILI DI WRK
001500050414     D****
001600050414     D parDelimFld     s              1
001700050414     D parDelimTXT     s              1
001800050414     D wDATASPED       s              8  0 inz(*zeros)
001900050414
002000030325
002100991027
002200050414     C* \MAIN
002300991027     C                   EXSR      traduci
002400921023     C                   SETON                                        LR
002500030908
002600991027
002700991027     C     traduci       BEGSR
002800050414     C*
002900050414     C* Inizializzo il buffer del file d output
003000050414     C                   CLEAR                   TIVAW000
003100030325     C*
003200050414     C* Scrivo una prima riga con l'intestazione dei campi
003300050414     C                   EVAL      VAWDTA = 'DATA SPEDIZIONE'+
003400050414     C                             parDelimFld +
003500050414     C                             'RIFERIMENTO NUMERICO'+
003600050414     C                             parDelimFld +
003700050414     C                             parDelimTXT +
003800050414     C                             'RIFERIMENTO ALFANUMERICO'+
003900050414     C                             parDelimTXT +
004000050414     C                             parDelimFld +
004100050414     C                             parDelimTXT +
004200050414     C                             'RAGIONE SOCIALE DESTINATARIO'+
004300050414     C                             parDelimTXT +
004400050414     C                             parDelimFld +
004500050414     C                             parDelimTXT +
004600050414     C                             'PROVINCIA DESTINATARIO'+
004700050414     C                             parDelimTXT +
004800050414     C                             parDelimFld +
004900050414     C                             parDelimTXT +
005000050414     C                             'LOCALITA DESTINATARIO'+
005100050414     C                             parDelimTXT +
005200050414     C                             parDelimFld +
005300050414     C                             parDelimTXT +
005400050414     C                             'COLLI'+
005500050414     C                             parDelimTXT +
005600050414     C                             parDelimFld +
005700050414     C                             parDelimTXT +
005800050414     C                             'PESO KG'+
005900050414     C                             parDelimTXT +
006000050414     C                             parDelimFld +
006100050414     C                             parDelimTXT +
006200050414     C                             'VOLUME M3'+
006300050414     C                             parDelimTXT +
006400050414     C                             parDelimFld +
006500050414     C                             parDelimTXT +
006600050414     C                             'IMPONIBILE' +
006700050414     C                             parDelimTXT
006800030325     C*
006900050414     C                   WRITE     TIVAW000
007000991027     C*
007100090416     C                   READ      tivgdtmp
007200090416     C                   dow       not %eof(tivgdtmp)
007300090416     C                   movel     vgddta        tivafds
007400050414     C*
007500050414     C* Inizializzo il buffer del file d output
007600050414     C                   CLEAR                   TIVAW000
007700030709     C*
007800030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
007900030709     C     KEYtas30C     CHAIN     titas30c
008000030709     C                   IF        %found(titas30c)
008100050414     C*
008200050414     C* Costruisco la data completa da AAS + MGS
008300050414     C                   EVAL      wDATASPED = VAFAAS*10000+VAFMGS
008400030325     C*
008500050414     C* Valorizzo il buffer del file d output
008600050414     C                   EVAL      VAWDTA =
008700050414     C                             %trim(%editc(wDATASPED:'Q')) +
008800050414     C                             parDelimFld +
008900050414     C                             %trim(%editc(VAFRMN:'Z'))+
009000050414     C                             parDelimFld +
009100050414     C                             parDelimTXT +
009200050414     C                             %trim(VAFRMA) +
009300050414     C                             parDelimTXT +
009400050414     C                             parDelimFld +
009500050414     C                             parDelimTXT +
009600050414     C                             %trim(TASRSD) +
009700050414     C                             parDelimTXT +
009800050414     C                             parDelimFld +
009900050414     C                             parDelimTXT +
010000050414     C                             %trim(VAFPRD) +
010100050414     C                             parDelimTXT +
010200050414     C                             parDelimFld +
010300050414     C                             parDelimTXT +
010400050414     C                             %trim(VAFLOD) +
010500050414     C                             parDelimTXT +
010600050414     C                             parDelimFld +
010700050414     C                             %trim(%editc(VAFNCL:'Q'))+
010800050414     C                             parDelimFld +
010900050414     C                             %trim(%editc(VAFPKB:'Q'))+
011000050414     C                             parDelimFld +
011100050414     C                             %trim(%editc(VAFVLF:'Q'))+
011200050414     C                             parDelimFld +
011300050414     C                             %trim(%editc(VAFIMV:'Q'))
011400930409     C*
011500050414     C                   WRITE     TIVAW000
011600050414     C                   ENDIF
011700050414     C*
011800090416     C                   DELETE    tivgd000
011900991027     C*
012000090416     C                   READ      tivgdtmp
012100030325     C                   ENDDO
012200991027     C*
012300030325     C                   EVAL      wrkesito = '0'
012400991027     C*
012500910830     C                   ENDSR
012600050414
012700050414
012800050414
012900991027
013000991027      /TITLE Operazioni iniziali.
013100991027     C     *inzsr        BEGSR
013200991027     C*
013300991027     C     *ENTRY        PLIST
013400991027     C                   parm                    prmppt
013500991027     C     wrkesito      parm      wrkesito      prmesito
013600050414     C*
013700050414     C* Imposto i flag d procedura dai parametri d traduzione ricevuti in input
013800050414     C                   eval      parDelimTXT = %subst(prmppt:1:1)
013900050414     C                   eval      parDelimFld = %subst(prmppt:2:1)
014000030709     C*
014100030709     C* Definizione chiavi
014200030709     C*
014300030709     C* Chiave su TITAS30C - Completa
014400030709     C     KEYtas30C     KLIST
014500030709     C                   KFLD                    vafAAS
014600030709     C                   KFLD                    vafLNP
014700030709     C                   KFLD                    vafNRS
014800030709     C                   KFLD                    vafNSP
014900030709     C                   KFLD                    vafTBL
015000991027     C*
015100991027     C                   ENDSR
