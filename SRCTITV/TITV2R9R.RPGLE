000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130228     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400140218     FTABEL00F  IF   E           K DISK
000500130228     FTISTB07L  UF   E           K DISK    commit
000600090323     FTIVAWWWT  UF A E             DISK    commit
000700030924
000800031103     D prmppi          S             50
000900030924     D prmesito        S              1
001000030924     D wrkesito        S                   like(prmesito)
001100130228     D sepFld          s              1    inz(';')
001200140218     D ds2A          e ds                  inz
001300040216
001400031103
001500031103     D*-----------
001600031103     D* VARIABILI RIFERITE AL DATA-BASE
001700031103     D*-----------
001800031104     D wKstbKSU        S                   like(stbKSU)
001900031103     D wKstbFTR        S                   like(stbFTR)
002000130228     D wKstbPRS        S                   like(stbPRS)
002100031103
002200040503
002300031103
002400031103     D*-----------
002500031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
002600031103     D*-----------
002700031103     D DSPPI           DS
002800031104     D  w§PPIKSU                      7  0
002900030924
003000030325
003100030924     C* MAIN/
003200130228     C                   EXSR      chkRECFILMBR
003300991027     C                   EXSR      traduci
003400921023     C                   SETON                                        LR
003500991027
003600130228
003700130228
003800130228     C     chkRECFILMBR  BEGSR
003900130228     C*
004000130228     C                   READ      TIVAWWWT                               55
004100130228     C*
004200130228     C                   ENDSR
004300030924
004400130228
004500030924
004600991027     C     traduci       BEGSR
004700130228     C*
004800130228     C* Se non presenti inserisco le intestazioni colonne
004900130228     C                   if        *in55
005000130228     C                   clear                   TIVAW000
005100140218     C                   eval      vawDTA = 'COD CLIENTE'     + sepFld +
005200140218     C                                      'DATA SPED.'      + sepFld +
005300140218     C                                      'RIF. MITTENTE 1' + sepFld +
005400140218     C                                      'RIF. MITTENTE 2' + sepFld +
005500140218     C                                      'DATA ESITO'      + sepFld +
005600140218     C                                      'COD. ESITO'      + sepFld +
005700140218     C                                      'DESCR. ESITO'    + sepFld
005800130228     C                   WRITE     TIVAW000
005900130228     C                   endif
006000031103     C*
006100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
006200031103     C* di traduzione (trattasi d "post-traduzione")
006300031104     C                   EVAL      wKstbKSU = w§PPIKSU
006400031103     C                   EVAL      wKstbFTR = *blanks
006500140218     C                   EVAL      wKstbPRS = '2'
006600130228     C     KEYstb07P     SETLL     tistb07l
006700130228     C                   IF        %equal(tistb07l)
006800130228     C     KEYstb07P     READE     tistb07l
006900991027     C*
007000040205     C* Ciclo x tutti i record del cliente da trasmettere
007100130228     C                   DOW       not %eof(tistb07l)
007200031014     C*
007300140218     C                   clear                   ds2A
007400130228     C                   clear                   TIVAW000
007500031103     C*
007600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
007700031103     C* deve restituire al cliente nel file tradotto
007800031103     C                   IF        stbFTR = *blanks                             * ridondante
007900031103     C*
008000140218     C* Gestisco unicamente gli stati "standard" e gli esiti di consegna
008100140218     C                   IF        stbTIS = '1'      AND
008200140218     C                             stbPRS = '2'
008300140218     C*
008400140218     C                   eval      tblKUT = 1
008500140218     C                   eval      tblCOD = '2A'
008600140218     C                   eval      tblKEY = stbCOS
008700140218     C     KEYtab00_C    chain     tabel00f
008800140218     C                   if        %found(tabel00f)
008900140218     C                   movel(p)  tblUNI        ds2A
009000140218     C*
009100040503     C                   EXSR      WRIREC
009200140218     C                   endif
009300031103     C*
009400031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
009500031103     C                   EVAL      stbFTR = 'T'
009600031103     C                   UPDATE    tistb000
009700031103     C                   ENDIF
009800130228     C                   ENDIF
009900991027     C*
010000130228     C     KEYstb07P     READE     tistb07l
010100030325     C                   ENDDO
010200031103     C                   ENDIF
010300991027     C*
010400030325     C                   EVAL      wrkesito = '0'
010500991027     C*
010600910830     C                   ENDSR
010700031103
010800031103
010900031103
011000140218     C     wrirec        BEGSR
011100140218     C*
011200140218     C                   EVAL      vawDTA = %trim(%editc(stbKSC:'X'))+sepFld+
011300140218     C                                      %trim(%editc(stbDSP:'X'))+sepFld+
011400140218     C                                      %trim(%editc(stbRMN:'X'))+sepFld+
011500140218     C                                      %trim(stbRMA)            +sepFld+
011600140218     C                                      %trim(%editc(stbDAS:'X'))+sepFld+
011700140218     C                                      %trim(stbCOS)            +sepFld+
011800140218     C                                      %trim(§2ADES)            +sepFld
011900140218     C*
012000140218     C* Scarico il buffer
012100140218     C                   WRITE     TIVAW000
012200031103     C*
012300031103     C                   ENDSR
012400031103
012500031103
012600991027
012700991027      /TITLE Operazioni iniziali.
012800991027     C     *inzsr        BEGSR
012900991027     C*
013000991027     C     *ENTRY        PLIST
013100031103     C                   parm                    prmppi
013200991027     C     wrkesito      parm      wrkesito      prmesito
013300031103     C*
013400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
013500031103     C                   EVAL      DSPPI = prmppi
013600030709     C*
013700030709     C* Definizione chiavi
013800030924     C*
013900130228     C* Chiave su TISTB07L - Parziale
014000130228     C     KEYstb07P     KLIST
014100031104     C                   KFLD                    wKstbKSU
014200031103     C                   KFLD                    wKstbFTR
014300130228     C                   KFLD                    wKstbPRS
014400140218     C*
014500140218     C* Chiave su TABEL00F - Parziale
014600140218     C     KEYtab00_C    KLIST
014700140218     C                   KFLD                    tblKUT
014800140218     C                   KFLD                    tblCOD
014900140218     C                   KFLD                    tblKEY
015000991027     C*
015100991027     C                   ENDSR
