000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130228     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400141007     FTABEL00F  IF   E           K DISK
000500130228     FTISTB07L  UF   E           K DISK    commit
000600090323     FTIVAWWWT  UF A E             DISK    commit
000700030924
000800031103     D prmppi          S             50
000900030924     D prmesito        S              1
001000030924     D wrkesito        S                   like(prmesito)
001100130228     D sepFld          s              1    inz(';')
001200141007
001300141007     D ds2a          e ds
001400141007     D sk2Agiac        s              3    dim(200)
001500141007     D sk2Alav         s              3    dim(200)
001600040216
001700031103
001800031103     D*-----------
001900031103     D* VARIABILI RIFERITE AL DATA-BASE
002000031103     D*-----------
002100031104     D wKstbKSU        S                   like(stbKSU)
002200031103     D wKstbFTR        S                   like(stbFTR)
002300130228     D wKstbPRS        S                   like(stbPRS)
002400040503
002500141007
002600031103
002700031103     D*-----------
002800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
002900031103     D*-----------
003000031103     D DSPPI           DS
003100031104     D  w§PPIKSU                      7  0
003200030924
003300030325
003400030924     C* MAIN/
003500141007     C                   EXSR      cartab
003600130228     C                   EXSR      chkRECFILMBR
003700991027     C                   EXSR      traduci
003800921023     C                   SETON                                        LR
003900141007
004000141007
004100141007
004200141007     C     CARTAB        BEGSR
004300141007     C*
004400141007     C* Carico i codici eventi d tipo "giacenza" e "lasciato avviso" (ovvero queli significativi
004500141007     C* ai fini del tentativo d consegna).
004600141007     C                   clear                   sk2Agiac
004700141007     C                   clear                   sk2Alav
004800141007     C                   z-add     *zeros        i                 3 0
004900141007     C                   z-add     *zeros        j                 3 0
005000141007     C                   eval      tblKUT = 1
005100141007     C                   eval      tblCOD = '2A'
005200141007     C     KEYtabP       setll     tabel00f
005300141007     C     KEYtabP       reade     tabel00f
005400141007     C                   dow       not %eof(tabel00f)
005500141007     C                   movel(p)  tblUNI        ds2a
005600141007     C                   if        §2aFTC = 'G'
005700141007     C                   add       1             i
005800141007     C                   eval      sk2Agiac(i) = tblKEY
005900141007     C                   endif
006000141007     C                   if        §2aFTC = 'A'
006100141007     C                   add       1             j
006200141007     C                   eval      sk2Alav(j)  = tblKEY
006300141007     C                   endif
006400141007     C     KEYtabP       reade     tabel00f
006500141007     C                   enddo
006600141007     C*
006700141007     C                   ENDSR
006800130228
006900141007
007000130228
007100130228     C     chkRECFILMBR  BEGSR
007200130228     C*
007300130228     C                   READ      TIVAWWWT                               55
007400130228     C*
007500130228     C                   ENDSR
007600030924
007700130228
007800030924
007900991027     C     traduci       BEGSR
008000130228     C*
008100130228     C* Se non presenti inserisco le intestazioni colonne
008200130228     C                   if        *in55
008300130228     C                   clear                   TIVAW000
008400130228     C                   eval      vawDTA = 'COD CLIENTE'    + sepFld +
008500130228     C                                      'ID PRATICA'     + sepFld +
008600141007     C                                      'DATA LAVV'      + sepFld +
008700141007     C                                      'ORA  LAVV'      + sepFld
008800130228     C                   WRITE     TIVAW000
008900130228     C                   endif
009000031103     C*
009100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
009200031103     C* di traduzione (trattasi d "post-traduzione")
009300031104     C                   EVAL      wKstbKSU = w§PPIKSU
009400031103     C                   EVAL      wKstbFTR = *blanks
009500130228     C                   EVAL      wKstbPRS = '7'
009600130228     C     KEYstb07P     SETLL     tistb07l
009700130228     C                   IF        %equal(tistb07l)
009800130228     C     KEYstb07P     READE     tistb07l
009900991027     C*
010000040205     C* Ciclo x tutti i record del cliente da trasmettere
010100130228     C                   DOW       not %eof(tistb07l)
010200031014     C*
010300130228     C                   clear                   TIVAW000
010400031103     C*
010500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
010600031103     C* deve restituire al cliente nel file tradotto
010700031103     C                   IF        stbFTR = *blanks                             * ridondante
010800031103     C*
010900130228     C* Gestisco unicamente gli stati "standard" e gli esiti di consegna OK
011000141007     C                   IF        stbTIS = '1'
011100141007     C*
011200141007     C* Ritorno unicamente gli eventi di tipo Lasciato Avviso
011300141007     C                   MOVEL     stbCOS        wCOS              3
011400141007     C     wCOS          LOOKUP    sk2Alav                                20
011500141007     C                   IF        *IN20 = *ON
011600141007     C*
011700040503     C                   EXSR      WRIREC
011800031103     C*
011900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
012000031103     C                   EVAL      stbFTR = 'T'
012100031103     C                   UPDATE    tistb000
012200141007     C                   ENDIF
012300031103     C                   ENDIF
012400130228     C                   ENDIF
012500991027     C*
012600130228     C     KEYstb07P     READE     tistb07l
012700030325     C                   ENDDO
012800031103     C                   ENDIF
012900991027     C*
013000030325     C                   EVAL      wrkesito = '0'
013100991027     C*
013200910830     C                   ENDSR
013300031103
013400031103
013500031103
013600031103     C     wrirec        BEGSR
013700031103     C*
013800130228     C                   EVAL      vawDTA = %trim(%editc(stbKSC:'X'))+sepFld+
013900130228     C                                      %trim(%editc(stbRMN:'X'))+sepFld+
014000141007     C                                      %trim(%editc(stbDAS:'X'))+sepFld+
014100141007     C                                      %trim(%editc(stbORS:'X'))
014200031103     C*
014300130228     C* Scarico il buffer
014400031103     C                   WRITE     TIVAW000
014500031103     C*
014600031103     C                   ENDSR
014700031103
014800031103
014900991027
015000991027      /TITLE Operazioni iniziali.
015100991027     C     *inzsr        BEGSR
015200991027     C*
015300991027     C     *ENTRY        PLIST
015400031103     C                   parm                    prmppi
015500991027     C     wrkesito      parm      wrkesito      prmesito
015600031103     C*
015700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
015800031103     C                   EVAL      DSPPI = prmppi
015900030709     C*
016000030709     C* Definizione chiavi
016100030924     C*
016200130228     C* Chiave su TISTB07L - Parziale
016300130228     C     KEYstb07P     KLIST
016400031104     C                   KFLD                    wKstbKSU
016500031103     C                   KFLD                    wKstbFTR
016600130228     C                   KFLD                    wKstbPRS
016700141007     C*
016800141007     C* Chiave su TABEL00F - Parziale
016900141007     C     KEYtabP       KLIST
017000141007     C                   KFLD                    tblKUT
017100141007     C                   KFLD                    tblCOD
017200991027     C*
017300991027     C                   ENDSR
