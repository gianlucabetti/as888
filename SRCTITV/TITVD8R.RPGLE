000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700080924     D fnvac00t      E DS
000800030924     D prmppt          S             50
000900030924     D prmesito        S              1
001000030924     D wrkesito        S                   like(prmesito)
001100030924
001200030924     D*-----------
001300040719     D* VARIABILI DI WRK X CONVERSIONE NUMERICI IN TESTO
001400030924     D*-----------
001500040719     D wNum10_3        S             10  3
001600030924
001700030924     D*-----------
001800040719     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO RITORNO ESITI DEL CLIENTE
001900030924     D*-----------
002000040719     D E_DS_Cli        DS
002100040719     D  E_SPPROV                      2    INZ
002200040719     D  E_SPDEPO                      6    INZ
002300040719     D  E_SPCOCL                     10    INZ
002400040719     D  E_SPNRPR                     16    INZ
002500040719     D  E_SPTDOC                      3    INZ
002600040719     D  E_SPDATI                      8    INZ
002700040719     D  E_SPORAI                      6    INZ
002800040719     D  E_SPNRBO                     10    INZ
002900040719     D  E_SPDABO                      8    INZ
003000040719     D  E_SPNRFT                     10    INZ
003100040719     D  E_SPNRIV                     10    INZ
003200040719     D  E_SPDAFT                      8    INZ
003300040719     D  E_SPVAFT                     13    INZ
003400040719     D  E_SPCAFT                      3    INZ
003500040719     D  E_SPCOLL                      5    INZ
003600040719     D  E_SPPESL                     10    INZ
003700040719     D  E_SPVOLU                     10    INZ
003800040719     D  E_SPDAPR                      8    INZ
003900040719     D  E_SPORPR                      6    INZ
004000040719     D  E_SPCDPG                      3    INZ
004100040719     D  E_SPDEPG                     40    INZ
004200040719     D  E_SPCDSP                      3    INZ
004300040719     D  E_SPDESP                     40    INZ
004400040719     D  E_SPNRLE                     25    INZ
004500040719     D  E_SPBENB                     25    INZ
004600040719     D  E_SPNOT1                    120    INZ
004700040719     D  E_SPNOT2                    120    INZ
004800040719     D  E_SPNOT3                    120    INZ
004900040719     D  E_SPDACL                      8    INZ
005000040719     D  E_SPORCL                      6    INZ
005100040719     D  E_SPDAN1                      8    INZ
005200040719     D  E_SPORN1                      6    INZ
005300040719     D  E_SPDAN2                      8    INZ
005400040719     D  E_SPORN2                      6    INZ
005500040719     D  E_SPDAN3                      8    INZ
005600040719     D  E_SPORN3                      6    INZ
005700040719     D  E_SPDAN4                      8    INZ
005800040719     D  E_SPORN4                      6    INZ
005900040719     D  E_SPDAN5                      8    INZ
006000040719     D  E_SPORN5                      6    INZ
006100040719     D  E_SPDAN6                      8    INZ
006200040719     D  E_SPORN6                      6    INZ
006300040719     D  E_SPNOT4                      1    INZ
006400040719
006500030924
006600030325
006700030924     C* MAIN/
006800991027     C                   EXSR      traduci
006900921023     C                   SETON                                        LR
007000991027
007100030924
007200030924
007300991027     C     traduci       BEGSR
007400991027     C*
007500080924     C                   READ      TIVGDTMP
007600080924     C                   DOW       not %eof(TIVGDTMP)
007700080924     C*
007800080924     C                   EVAL      fnvac00t = vgdDTA
007900040719     C*
008000040719     C* Restituisco al cliente solo le bolle con consegna anomala = *blanks (sia consegnate che nn)
008100040719     C* e i resi
008200040719     C                   IF        VACCCA = *blanks OR
008300040719     C                             VACCCA = '2'
008400030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
008500040719     C                   RESET                   E_DS_Cli
008600040719     C*
008700040719     C* Valorizzo i campi del tracciato del cliente richiesti
008800040719     C                   MOVEL     *all'0'       E_SPDEPO
008900040719     C                   IF        VACCCM = 0590376 OR
009000040719     C                             VACCCM = 0590499
009100040719     C                   MOVE      '21'          E_SPDEPO
009200040719     C                   ENDIF
009300040719     C                   IF        VACCCM = 0590422 OR
009400040719     C                             VACCCM = 0590502
009500040719     C                   MOVE      '22'          E_SPDEPO
009600040719     C                   ENDIF
009700040719     C                   IF        VACCCM = 0590098 OR
009800040719     C                             VACCCM = 0590132
009900040719     C                   MOVE      '23'          E_SPDEPO
010000040719     C                   ENDIF
010100040719     C*
010200040719     C                   EVAL      E_SPNRPR = %trim(VACRMA)
010300040719     C                   EVAL      E_SPTDOC = 'DDT'
010400040719     C                   MOVE(P)   VACRMN        E_SPNRBO
010500040719     C                   MOVE(P)   VACNCL        E_SPCOLL
010600040719     C*
010700040719     C                   Z-ADD     VACPKB        wNum10_3
010800040719     C                   MOVE(P)   wNum10_3      E_SPPESL
010900040719     C*
011000040719     C                   Z-ADD     VACVLB        wNum10_3
011100040719     C                   MOVE(P)   wNum10_3      E_SPVOLU
011200040719     C*
011300040719     C                   MOVEL(P)  VACAAS        E_SPDAPR
011400040719     C                   MOVE      VACMGS        E_SPDAPR
011500040719     C*
011600040719     C* Solo se consegna ok valorizzo i campi seguenti
011700040719     C                   IF        VACDCM > *zeros AND
011800040719     C                             VACCCA = *blanks
011900040719     C                   MOVE(P)   VACDCM        E_SPDACL
012000040719     C                   MOVEL(P)  *all'0'       E_SPORCL
012100040719     C                   MOVEL     VACHMC        E_SPORCL
012200040719     C                   EVAL      E_SPNOT4 = 'C'
012300040719     C                   ENDIF
012400040719     C*
012500040719     C* Solo se reso al mittente valorizzo i campi seguenti
012600040719     C                   IF        VACDCM > *zeros AND
012700040719     C                             VACCCA = '2'
012800040719     C                   MOVE(P)   VACDCM        E_SPDACL
012900040719     C                   MOVEL(P)  *all'0'       E_SPORCL
013000040719     C                   MOVEL     VACHMC        E_SPORCL
013100040719     C                   EVAL      E_SPNOT1 = 'RESO AL MITTENTE'
013200040719     C                   ENDIF
013300040719     C*
013400040719     C* Gestisco gli eventi d giacenza o lasciato avviso
013500040719     C                   IF        VACDLA > VACDAG  AND
013600040719     C                             VACDLA > VACDCM
013700040719     C                   MOVE(P)   VACDLA        E_SPDAN1
013800040719     C                   EVAL      E_SPNOT1 = 'LASCIATO AVVISO'
013900040719     C                   ENDIF
014000040719     C                   IF        VACDAG > VACDLA  AND
014100040719     C                             VACDAG > VACDCM
014200040719     C                   MOVE(P)   VACDAG        E_SPDAN1
014300040719     C                   EVAL      E_SPNOT1 = 'APERTURA GIACENZA'
014400040719     C                   ENDIF
014500930409     C*
014600030924     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
014700040719     C                   EVAL      VAWDTA = E_DS_Cli
014800030924     C*
014900040719     C* SCarico il buffer x restituire record corrente al cliente
015000030924     C                   WRITE     TIVAW000
015100040719     C                   ENDIF
015200040719     C*
015300040719     C* In ogni caso storicizzo e cancello l'input sempre
015400080924     C                   DELETE    TIVGD000
015500040719     C*
015600040719     C* Proseguo con la lettura dell'input
015700080924     C                   READ      TIVGDTMP
015800030325     C                   ENDDO
015900991027     C*
016000030325     C                   EVAL      wrkesito = '0'
016100991027     C*
016200910830     C                   ENDSR
016300991027
016400991027      /TITLE Operazioni iniziali.
016500991027     C     *inzsr        BEGSR
016600991027     C*
016700991027     C     *ENTRY        PLIST
016800991027     C                   parm                    prmppt
016900991027     C     wrkesito      parm      wrkesito      prmesito
017000991027     C*
017100991027     C                   ENDSR
