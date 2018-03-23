000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130228     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400130228     FTISTB07L  UF   E           K DISK    commit
000500090323     FTIVAWWWT  UF A E             DISK    commit
000600030924
000700031103     D prmppi          S             50
000800030924     D prmesito        S              1
000900030924     D wrkesito        S                   like(prmesito)
001000130228     D sepFld          s              1    inz(';')
001100040216
001200031103
001300031103     D*-----------
001400031103     D* VARIABILI RIFERITE AL DATA-BASE
001500031103     D*-----------
001600031104     D wKstbKSU        S                   like(stbKSU)
001700031103     D wKstbFTR        S                   like(stbFTR)
001800130228     D wKstbPRS        S                   like(stbPRS)
001900031103
002000040503
002100031103
002200031103     D*-----------
002300031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
002400031103     D*-----------
002500031103     D DSPPI           DS
002600031104     D  w§PPIKSU                      7  0
002700030924
002800030325
002900030924     C* MAIN/
003000130228     C                   EXSR      chkRECFILMBR
003100991027     C                   EXSR      traduci
003200921023     C                   SETON                                        LR
003300991027
003400130228
003500130228
003600130228     C     chkRECFILMBR  BEGSR
003700130228     C*
003800130228     C                   READ      TIVAWWWT                               55
003900130228     C*
004000130228     C                   ENDSR
004100030924
004200130228
004300030924
004400991027     C     traduci       BEGSR
004500130228     C*
004600130228     C* Se non presenti inserisco le intestazioni colonne
004700130228     C                   if        *in55
004800130228     C                   clear                   TIVAW000
004900130228     C                   eval      vawDTA = 'COD CLIENTE'    + sepFld +
005000130228     C                                      'ID PRATICA'     + sepFld +
005100130228     C                                      'DATA CONSEGNA'  + sepFld
005200130228     C                   WRITE     TIVAW000
005300130228     C                   endif
005400031103     C*
005500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
005600031103     C* di traduzione (trattasi d "post-traduzione")
005700031104     C                   EVAL      wKstbKSU = w§PPIKSU
005800031103     C                   EVAL      wKstbFTR = *blanks
005900130228     C                   EVAL      wKstbPRS = '7'
006000130228     C     KEYstb07P     SETLL     tistb07l
006100130228     C                   IF        %equal(tistb07l)
006200130228     C     KEYstb07P     READE     tistb07l
006300991027     C*
006400040205     C* Ciclo x tutti i record del cliente da trasmettere
006500130228     C                   DOW       not %eof(tistb07l)
006600031014     C*
006700130228     C                   clear                   TIVAW000
006800031103     C*
006900031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
007000031103     C* deve restituire al cliente nel file tradotto
007100031103     C                   IF        stbFTR = *blanks                             * ridondante
007200031103     C*
007300130228     C* Gestisco unicamente gli stati "standard" e gli esiti di consegna OK
007400130228     C                   IF        stbTIS = '1'      AND
007500130228     C                             stbCOS = 'CONSOK'
007600040503     C                   EXSR      WRIREC
007700031103     C*
007800031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
007900031103     C                   EVAL      stbFTR = 'T'
008000031103     C                   UPDATE    tistb000
008100031103     C                   ENDIF
008200130228     C                   ENDIF
008300991027     C*
008400130228     C     KEYstb07P     READE     tistb07l
008500030325     C                   ENDDO
008600031103     C                   ENDIF
008700991027     C*
008800030325     C                   EVAL      wrkesito = '0'
008900991027     C*
009000910830     C                   ENDSR
009100031103
009200031103
009300031103
009400031103     C     wrirec        BEGSR
009500031103     C*
009600130228     C                   EVAL      vawDTA = %trim(%editc(stbKSC:'X'))+sepFld+
009700130228     C                                      %trim(%editc(stbRMN:'X'))+sepFld+
009800130228     C                                      %trim(%editc(stbDAS:'X'))
009900031103     C*
010000130228     C* Scarico il buffer
010100031103     C                   WRITE     TIVAW000
010200031103     C*
010300031103     C                   ENDSR
010400031103
010500031103
010600991027
010700991027      /TITLE Operazioni iniziali.
010800991027     C     *inzsr        BEGSR
010900991027     C*
011000991027     C     *ENTRY        PLIST
011100031103     C                   parm                    prmppi
011200991027     C     wrkesito      parm      wrkesito      prmesito
011300031103     C*
011400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
011500031103     C                   EVAL      DSPPI = prmppi
011600030709     C*
011700030709     C* Definizione chiavi
011800030924     C*
011900130228     C* Chiave su TISTB07L - Parziale
012000130228     C     KEYstb07P     KLIST
012100031104     C                   KFLD                    wKstbKSU
012200031103     C                   KFLD                    wKstbFTR
012300130228     C                   KFLD                    wKstbPRS
012400991027     C*
012500991027     C                   ENDSR
