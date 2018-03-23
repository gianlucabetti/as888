000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200150203     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030924     FTNCSB03L  IF   E           K DISK
000500030924     FTABEL00F  IF   E           K DISK
000600030709     FTITAS30C  IF   E           K DISK
000700090416     Ftivgdtmp  UF   E             DISK
000800090309     FTIVAWWWT  UF A E             DISK    commit
000900030924
001000090416     D tivafds       e ds                  extname(tivaf00t)
001100030924     D prmppt          S             50
001200030924     D prmesito        S              1
001300030924     D wrkesito        S                   like(prmesito)
001400030924     D dscc          E DS
001500030924     D i               S              3  0 inz(*zeros)
001600030924
001700030924     D*------------
001800030924     D* SCHIERE SIGLE E DESCRIZIONI VARIE
001900030924     D*------------
002000030924     D SKSVN           S              1    DIM(100)
002100030924     D SKSVNDES        S             20    DIM(100)
002200030924
002300030924     D*-----------
002400030924     D* VARIABILI DI WRK X CONVERSIONE NUMERICI IN TESTO
002500030924     D*-----------
002600030924     D wNum13_2        S             13  2
002700030924     D wNum13_3        S             13  3
002800030924     D wNum13_5        S             13  5
002900030924     D wNum13_6        S             13  6
003000030924
003100030924     D*-----------
003200030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
003300030924     D*-----------
003400030924     D F_DS_Cli        DS
003500030925     D  F_CodVettore                 20    INZ(*blanks)
003600030925     D  F_CodDepPart                 20    INZ(*blanks)
003700030925     D  F_DataFatt                    8    INZ(*zeros)
003800030925     D  F_NumFatt                    13    INZ(*zeros)
003900030925     D  F_RegFatt                     5    INZ(*blanks)
004000030925     D  F_IDSped                     20    INZ(*blanks)
004100030925     D  F_RifSpedVet                 20    INZ(*blanks)
004200030925     D  F_RifMittPri                 20    INZ(*blanks)
004300030925     D  F_DataSped                   12    INZ(*zeros)
004400030925     D  F_DataRitiro                 12    INZ(*zeros)
004500030925     D  F_RifMittRit                 20    INZ(*blanks)
004600030925     D  F_CodCliente                 20    INZ(*blanks)
004700030925     D  F_CodDestino                 20    INZ(*blanks)
004800030925     D  F_RagSocDest                 50    INZ(*blanks)
004900030925     D  F_LocDest                    50    INZ(*blanks)
005000030925     D  F_CodLocDest                  5    INZ(*blanks)
005100030925     D  F_IndDest                    50    INZ(*blanks)
005200030925     D  F_ProvDest                    2    INZ(*blanks)
005300030925     D  F_CAPDest                    10    INZ(*blanks)
005400030925     D  F_DistrDest                   5    INZ(*blanks)
005500030925     D  F_NazDest                     5    INZ(*blanks)
005600030925     D  F_PesoKgBoll                 13    INZ(*zeros)
005700030925     D  F_NumColli                   13    INZ(*zeros)
005800030925     D  F_NumBancali                 13    INZ(*zeros)
005900030925     D  F_Volume                     13    INZ(*zeros)
006000030925     D  F_QtaFatt                    13    INZ(*zeros)
006100030925     D  F_NatMerce                   20    INZ(*blanks)
006200030925     D  F_CodNatMerc                  5    INZ(*blanks)
006300030925     D  F_DivisaFatt                  5    INZ(*blanks)
006400030925     D  F_CambioDiv                  13    INZ(*zeros)
006500030925     D  F_ImpPorto                   13    INZ(*zeros)
006600030925     D  F_Imponib                    13    INZ(*zeros)
006700030925     D  F_AliqIVA                    13    INZ(*zeros)
006800030925     D  F_CodEsIVA                    5    INZ(*blanks)
006900030925     D  F_CodVaria1                   5    INZ(*blanks)
007000030925     D  F_DescVaria1                 35    INZ(*blanks)
007100030925     D  F_ImpVaria1                  13    INZ(*zeros)
007200030925     D  F_CodVaria2                   5    INZ(*blanks)
007300030925     D  F_DescVaria2                 35    INZ(*blanks)
007400030925     D  F_ImpVaria2                  13    INZ(*zeros)
007500030925     D  F_CodVaria3                   5    INZ(*blanks)
007600030925     D  F_DescVaria3                 35    INZ(*blanks)
007700030925     D  F_ImpVaria3                  13    INZ(*zeros)
007800030925     D  F_CodVaria4                   5    INZ(*blanks)
007900030925     D  F_DescVaria4                 35    INZ(*blanks)
008000030925     D  F_ImpVaria4                  13    INZ(*zeros)
008100030925     D  F_CodVaria5                   5    INZ(*blanks)
008200030925     D  F_DescVaria5                 35    INZ(*blanks)
008300030925     D  F_ImpVaria5                  13    INZ(*zeros)
008400030925     D  F_Inoltro                     1    INZ(*blanks)
008500030925     D  F_DivImpAss                   5    INZ(*blanks)
008600030925     D  F_ImpAss                     13    INZ(*zeros)
008700030925     D  F_DivImpCAss                  5    INZ(*blanks)
008800030925     D  F_ImpCAss                    13    INZ(*zeros)
008900030925     D  F_Flag1                       1    INZ(*blanks)
009000030925     D  F_Flag2                       1    INZ(*blanks)
009100030925     D  F_Flag3                       1    INZ(*blanks)
009200030925     D  F_Flag4                       1    INZ(*blanks)
009300030925     D  F_Flag5                       1    INZ(*blanks)
009400030925     D  F_TipPesoRil                  1    INZ(*blanks)
009500030925     D  F_PesoRil                    13    INZ(*zeros)
009600030925     D  F_TipVolRil                   1    INZ(*blanks)
009700030925     D  F_VolRil                     13    INZ(*zeros)
009800030924
009900030924
010000030924     D*-----------
010100030924     D* RIDEFINIZIONE CHIAVE BOLLA IN FORMATO ALFA
010200030924     D*-----------
010300030924     D                 DS
010400090416     D  wVAFAAS                1      4  0
010500090416     D  wVAFLNP                5      7  0
010600090416     D  wVAFNRS                8      9  0
010700090416     D  wVAFNSP               10     16  0
010800030924     D DS_KEYBOLLA             1     16
010900030924     D*-----------
011000030924     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
011100030924     D*-----------
011200030924     D                 DS
011300030924     D  w_AAS                  1      4  0
011400030924     D  w_MGS                  5      8  0
011500030924     D DS_DATABOLLA            1      8
011600030924
011700030325
011800030924     C* MAIN/
011900030924     C                   EXSR      cartab
012000991027     C                   EXSR      traduci
012100921023     C                   SETON                                        LR
012200030908
012300030924
012400030924
012500030924     C     cartab        BEGSR
012600030924     C*
012700030924     C                   clear                   skSVN
012800030924     C                   clear                   skSVNDES
012900030924     C*
013000030924     C* Reperisco le descrizioni delle varie
013100030924     C                   Z-ADD     *zeros        i
013200030924     C                   Z-ADD     1             tblKUT
013300030924     C                   MOVEL(P)  'CC'          tblCOD
013400030924     C     KEYtabel      SETLL     TABEL00F
013500030924     C                   IF        %equal(TABEL00F)
013600030924     C     KEYtabel      READE     TABEL00F
013700030924     C                   DOW       not %eof(TABEL00F)
013800030924     C                   ADD       1             i
013900030924     C                   MOVE(P)   TBLKEY        SKSVN(i)
014000030924     C                   MOVEL(P)  TBLUNI        DSCC
014100030924     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
014200030924     C     KEYtabel      READE     TABEL00F
014300030924     C                   ENDDO
014400030924     C                   ENDIF
014500030924     C*
014600030924     C                   ENDSR
014700991027
014800030924
014900030924
015000991027     C     traduci       BEGSR
015100991027     C*
015200090416     C                   READ      tivgdtmp
015300090416     C                   dow       not %eof(tivgdtmp)
015400090416     C                   movel     vgddta        tivafds
015500030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
015600030925     C                   RESET                   F_DS_Cli
015700030709     C*
015800030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
015900030709     C     KEYtas30C     CHAIN     titas30c
016000030709     C                   IF        %found(titas30c)
016100030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
016200030924     C                   EVAL      F_RagSocDest = TASRSD
016300030924     C                   EVAL      F_IndDest = TASIND
016400030709     C                   ENDIF
016500030924     C*
016600090416     C                   EVAL      wVAFAAS = VAFAAS
016700090416     C                   EVAL      wVAFLNP = VAFLNP
016800090416     C                   EVAL      wVAFNRS = VAFNRS
016900090416     C                   EVAL      wVAFNSP = VAFNSP
017000090416     C*
017100031029     C                   EVAL      F_CodVettore  = 'BA'
017200030924     C                   MOVEL(P)  VAFLNP        F_CodDepPart
017300030925     C                   MOVE      VAFDFT        F_DataFatt
017400030925     C                   MOVE      VAFNFT        F_NumFatt
017500030924     C                   MOVEL(P)  VAFFIV        F_RegFatt
017600030924     C                   MOVEL(P)  VAFRMN        F_IDSped
017700030924     C                   MOVEL(P)  DS_KEYBOLLA   F_RifSpedVet
017800030924     C                   Z-ADD     VAFAAS        w_AAS
017900030924     C                   Z-ADD     VAFMGS        w_MGS
018000030924     C                   MOVEL     DS_DATABOLLA  F_DataSped
018100030924     C                   MOVEL     VAFDRT        F_DataRitiro
018200030924     C                   MOVEL(P)  VAFKSC        F_CodCliente
018300030924     C                   MOVEL(P)  VAFLNA        F_CodDestino
018400030924     C                   MOVEL(P)  VAFLOD        F_LocDest
018500030924     C                   MOVEL(P)  VAFPRD        F_ProvDest
018600030924     C                   MOVEL(P)  VAFCAD        F_CAPDest
018700030924     C                   MOVEL(P)  VAFNZD        F_NazDest
018800030924     C                   Z-ADD     VAFPKB        wNum13_3
018900030925     C                   MOVE      wNum13_3      F_PesoKgBoll
019000030925     C                   MOVE      VAFNCL        F_NumColli
019100030924     C                   Z-ADD     VAFVLF        wNum13_6
019200030925     C                   MOVE      wNum13_6      F_Volume
019300030924     C                   Z-ADD     VAFQFT        wNum13_3
019400030925     C                   MOVE      wNum13_3      F_QtaFatt
019500030924     C                   MOVEL(P)  VAFNAS        F_NatMerce
019600030924     C                   MOVEL(P)  VAFDIV        F_DivisaFatt
019700030924     C                   Z-ADD     VAFPOR        wNum13_5
019800030925     C                   MOVE      wNum13_5      F_ImpPorto
019900030924     C                   Z-ADD     VAFIMV        wNum13_5
020000030925     C                   MOVE      wNum13_5      F_Imponib
020100030925     C                   MOVE      20000         F_AliqIVA                      20% gest. 3 decimali
020200030924     C                   MOVEL(P)  TASFEI        F_CodEsIVA
020300030924     C* Varia 1
020400030925     C                   IF        VAFVA1 <> *zeros
020500030924     C                   MOVEL(P)  VAFSV1        F_CodVaria1
020600030924     C* Reperire la descrizione della varia
020700030924     C                   Z-ADD     1             i
020800030924     C     VAFSV1        LOOKUP    skSVN(i)                               55
020900030924     C                   IF        %found
021000030924     C                   EVAL      F_DescVaria1 = SKSVNDES(i)
021100030924     C                   ELSE
021200030924     C                   EVAL      F_DescVaria1 = 'VAR1'
021300030924     C                   ENDIF
021400030924     C                   Z-ADD     VAFVA1        wNum13_5
021500030925     C                   MOVE      wNum13_5      F_ImpVaria1
021600030925     C                   ENDIF
021700030924     C* Varia 2
021800030925     C                   IF        VAFVA2 <> *zeros
021900030924     C                   MOVEL(P)  VAFSV2        F_CodVaria2
022000030924     C* Reperire la descrizione della varia
022100030924     C                   Z-ADD     1             i
022200030924     C     VAFSV2        LOOKUP    skSVN(i)                               55
022300030924     C                   IF        %found
022400030924     C                   EVAL      F_DescVaria2 = SKSVNDES(i)
022500030924     C                   ELSE
022600030924     C                   EVAL      F_DescVaria2 = 'VAR2'
022700030924     C                   ENDIF
022800030924     C                   Z-ADD     VAFVA2        wNum13_5
022900030925     C                   MOVE      wNum13_5      F_ImpVaria2
023000030925     C                   ENDIF
023100030924     C* Varia 3
023200030925     C                   IF        VAFVA3 <> *zeros
023300030924     C                   MOVEL(P)  VAFSV3        F_CodVaria3
023400030924     C* Reperire la descrizione della varia
023500030924     C                   Z-ADD     1             i
023600030924     C     VAFSV3        LOOKUP    skSVN(i)                               55
023700030924     C                   IF        %found
023800030924     C                   EVAL      F_DescVaria3 = SKSVNDES(i)
023900030924     C                   ELSE
024000030924     C                   EVAL      F_DescVaria3 = 'VAR3'
024100030924     C                   ENDIF
024200030924     C                   Z-ADD     VAFVA3        wNum13_5
024300030925     C                   MOVE      wNum13_5      F_ImpVaria3
024400030925     C                   ENDIF
024500030924     C* Varia 4
024600030925     C                   IF        VAFVA4 <> *zeros
024700030924     C                   MOVEL(P)  VAFSV4        F_CodVaria4
024800030924     C* Reperire la descrizione della varia
024900030924     C                   Z-ADD     1             i
025000030924     C     VAFSV4        LOOKUP    skSVN(i)                               55
025100030924     C                   IF        %found
025200030924     C                   EVAL      F_DescVaria4 = SKSVNDES(i)
025300030924     C                   ELSE
025400030924     C                   EVAL      F_DescVaria4 = 'VAR4'
025500030924     C                   ENDIF
025600030924     C                   Z-ADD     VAFVA4        wNum13_5
025700030925     C                   MOVE      wNum13_5      F_ImpVaria4
025800030925     C                   ENDIF
025900030924     C* Varia 5
026000030924     C                   EVAL      wNum13_5 = VAFVA5 + VAFVA6 + VAFVA7 + VAFVA8
026100030925     C                   IF        wNum13_5 <> *zeros
026200030925     C                   MOVE      wNum13_5      F_ImpVaria5
026300030924     C                   MOVEL(P)  'ALTRE'       F_DescVaria5
026400030925     C                   ENDIF
026500030924     C* FINE VARIE
026600030924     C                   MOVEL(P)  VAFFIN        F_Inoltro
026700030924     C                   MOVEL(P)  VAFVAS        F_DivImpAss
026800030924     C                   Z-ADD     VAFIAS        wNum13_2
026900030925     C                   MOVE      wNum13_2      F_ImpAss
027000030924     C* Reperisco divisa e importo contrassegno dal TNCSB
027100030924     C     KEYcsb03      CHAIN     tncsb03l
027200030924     C                   IF        %found(tncsb03l)
027300030924     C                   MOVEL(P)  CSBVCA        F_DivImpCAss
027400030924     C                   Z-ADD     CSBCAS        wNum13_2
027500030925     C                   MOVE      wNum13_2      F_ImpCAss
027600030924     C                   ENDIF
027700030924     C                   Z-ADD     VAFPKC        wNum13_3
027800030925     C                   MOVE      wNum13_3      F_PesoRil
027900030924     C                   Z-ADD     VAFVLC        wNum13_3
028000030925     C                   MOVE      wNum13_3      F_VolRil
028100930409     C*
028200030924     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
028300030924     C                   EVAL      VAWDTA = F_DS_Cli
028400030924     C*
028500030924     C                   WRITE     TIVAW000
028600090416     C                   DELETE    tivgd000
028700991027     C*
028800090416     C                   READ      tivgdtmp
028900030325     C                   ENDDO
029000991027     C*
029100030325     C                   EVAL      wrkesito = '0'
029200991027     C*
029300910830     C                   ENDSR
029400991027
029500991027      /TITLE Operazioni iniziali.
029600991027     C     *inzsr        BEGSR
029700991027     C*
029800991027     C     *ENTRY        PLIST
029900991027     C                   parm                    prmppt
030000991027     C     wrkesito      parm      wrkesito      prmesito
030100030709     C*
030200030709     C* Definizione chiavi
030300030924     C*
030400030924     C* Chiave su TABEL00F - Completa
030500030924     C     KEYtabel      KLIST
030600030924     C                   KFLD                    tblKUT
030700030924     C                   KFLD                    tblCOD
030800030709     C*
030900030709     C* Chiave su TITAS30C - Completa
031000030709     C     KEYtas30C     KLIST
031100030709     C                   KFLD                    vafAAS
031200030709     C                   KFLD                    vafLNP
031300030709     C                   KFLD                    vafNRS
031400030709     C                   KFLD                    vafNSP
031500030709     C                   KFLD                    vafTBL
031600030924     C*
031700030924     C* Chiave su TNCSB03L - Completa
031800030924     C     KEYcsb03      KLIST
031900030924     C                   KFLD                    vafAAS
032000030924     C                   KFLD                    vafLNP
032100030924     C                   KFLD                    vafNRS
032200030924     C                   KFLD                    vafNSP
032300991027     C*
032400991027     C                   ENDSR
