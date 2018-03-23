000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500030115     FfnvachsewtO    E             DISK
000600030115     FTNCSB03L  IF   E           K DISK
000700050310     FTIGCP51L  IF   E           K DISK
000800050310     FTIGNP01L  IF   E           K DISK
000900991027
001000080924     D fnvac00t      e ds
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400030115     D wrkPKB          s              7  3
001500030115     D wrkVLB          s              7  2
001600030115
001700030115     D*------------------
001800030115     D* DS "XSRDA8" - CONTROLLA DATA (8)
001900030115     D*------------------
002000030115     D WLBDA8          DS                  INZ
002100030115     D  G08DAT                 1      8  0
002200030115     D  G08INV                 9     16  0
002300030115     D  G08ERR                17     17
002400030115     D  G08TGI                18     22  0
002500020729
002600991027
002700991027     C                   EXSR      traduci
002800921023     C                   SETON                                        LR
002900991027
003000991027     C     traduci       BEGSR
003100991027     C*
003200080924     C                   READ      TIVGDTMP
003300080924     C                   DOW       not %eof(TIVGDTMP)
003400080924     C*
003500080924     C                   EVAL      fnvac00t = vgdDTA
003600020508     C*
003700020508     C* Innanzitutto inizializzo il formato record del file tradotto
003800030115     C                   CLEAR                   FNVACHSE1
003900020508     C*
004000020729     C* Quindi effettuo la traduzione vera e propria
004100030115     C                   MOVE      datcor        HSEDAE
004200030121     C                   MOVE      datcor        HSEDVS
004300030115     C* - campi provenienti dal FNVAC
004400030115     C                   MOVEL     vacLNP        HSENLDV
004500030115     C                   MOVE      vacNSP        HSENLDV
004600030115     C                   MOVE      vacCCM        HSECCM
004700030115     C                   MOVEL     vacRMA        HSERMA
004800030121     C                   EVAL      HSENSP = %subst(%editw(vacLNP:'   '):2:2)
004900030121     C                   MOVE      vacNSP        HSENSP
005000030115     C                   MOVEL     vacRSD        HSERSD
005100030115     C                   EVAL      wrkPKB = vacPKB
005200030115     C                   MOVE      wrkPKB        HSEPKB
005300030115     C                   EVAL      wrkVLB = vacVLB
005400030115     C                   MOVE      wrkVLB        HSEVLB
005500030115     C* -- effettuo alcune considerazioni sulle date LASCIATO AVVISO / GIACENZA / CONSEGNA
005600030115     C                   IF        vacDLA > vacDCM AND                          * trattasi di L.AVV.
005700030115     C                             vacDLA > vacDAG
005800030115     C                   MOVE      vacDLA        HSEDCM
005900030115     C                   EVAL      HSESTS = 'KO'
006000030115     C*
006100030115     C                   ELSE
006200030115     C                   IF        vacDAG > vacDCM AND                          * trattasi di GIAC.
006300030115     C                             vacDAG > vacDLA
006400030115     C                   MOVE      vacDAG        HSEDAG
006500050310     C* - campi provenienti dal TIGCP
006600030115     C                   EVAL      gcpFRG = *zeros
006700050310     C     KEYGCP        CHAIN     TIGCP51L
006800050310     C                   IF        %found(TIGCP51L)
006900030115     C                   MOVE      gcpNGC        HSENGC
007000030115     C                   IF        gcpCMC = '001'
007100030115     C                   EVAL      HSESTS = 'EU'
007200030115     C                   ENDIF
007300030115     C                   IF        gcpCMC = '002'
007400030115     C                   EVAL      HSESTS = 'EU'
007500030115     C                   ENDIF
007600030115     C                   IF        gcpCMC = '008'
007700030121     C                   EVAL      HSESTS = 'LN'
007800030115     C                   ENDIF
007900030115     C                   IF        gcpCMC = '012'
008000030115     C                   EVAL      HSESTS = 'EU'
008100030115     C                   ENDIF
008200030115     C                   IF        gcpCMC = '016'
008300030115     C                   EVAL      HSESTS = 'IQ'
008400030115     C                   ENDIF
008500030115     C                   IF        gcpCMC = '021'
008600030115     C                   EVAL      HSESTS = '13'
008700030115     C                   ENDIF
008800030115     C                   IF        gcpCMC = '022'
008900030115     C                   EVAL      HSESTS = 'IA'
009000030115     C                   ENDIF
009100030115     C                   IF        gcpCMC = '023'
009200030115     C                   EVAL      HSESTS = '13'
009300030115     C                   ENDIF
009400030115     C                   IF        gcpCMC = '024'
009500030115     C                   EVAL      HSESTS = '13'
009600030115     C                   ENDIF
009700030115     C                   IF        gcpCMC = '026'
009800030115     C                   EVAL      HSESTS = 'IQ'
009900030115     C                   ENDIF
010000030115     C                   IF        gcpCMC = '028'
010100030115     C                   EVAL      HSESTS = '13'
010200030115     C                   ENDIF
010300030115     C                   IF        gcpCMC = '034'
010400030115     C                   EVAL      HSESTS = 'JH'
010500030115     C                   ENDIF
010600030115     C* -- per ogni altro eventuale codice giacenza utilizzo il codice generico giacenza HSE = 30
010700030115     C                   IF        HSESTS = *blanks
010800030115     C                   EVAL      HSESTS = '30'
010900030115     C                   ENDIF
011000050310     C* - campi provenienti dal TIGNP
011100030115     C                   EVAL      gnpFAS = 10
011200050310     C     KEYGNP        CHAIN     TIGNP01L
011300050310     C                   IF        %found(TIGNP01L)
011400030115     C                   MOVEL     gnpDMC        HSENTG
011500030115     C                   ENDIF
011600030115     C                   ENDIF
011700030115     C*
011800030115     C                   ELSE
011900030115     C                   IF        vacDCM > vacDLA AND                          * trattasi di CONS.
012000030115     C                             vacDCM > vacDAG
012100030115     C                   MOVE      vacDCM        HSEDCM
012200030115     C                   MOVE      vacHMC        HSEHMC
012300030115     C* --- effetuo considerazioni sulla eventuale presenza di anomalie
012400030115     C                   IF        vacCCA = *blanks
012500030115     C                   IF        vacFFD = 'S'
012600030115     C                   EVAL      HSESTS = '20'
012700030115     C                   ELSE
012800030115     C                   EVAL      HSESTS = '90'
012900030115     C                   ENDIF
013000030115     C                   ENDIF
013100030115     C                   IF        vacCCA = 'C'
013200030115     C                   EVAL      HSESTS = '14'
013300030115     C                   ENDIF
013400030115     C                   IF        vacCCA = '1'
013500030115     C                   EVAL      HSESTS = 'IQ'
013600030115     C                   ENDIF
013700030115     C                   IF        vacCCA = '2'
013800030115     C                   EVAL      HSESTS = '99'
013900030115     C                   ENDIF
014000030115     C* --- forzo un valore "civetta" x eventuali spedizioni aventi stato nn qui previsto
014100030115     C                   IF        HSESTS = *blanks
014200030115     C                   EVAL      HSESTS = '??'
014300030115     C                   ENDIF
014400030115     C*
014500030115     C                   ENDIF
014600030115     C                   ENDIF
014700030115     C                   ENDIF
014800030115     C* - campi provenienti dal TNCSB
014900030115     C     KEYCSB        CHAIN     TNCSB03L
015000030115     C                   IF        %found(TNCSB03L)
015100030115     C                   MOVE      csbDTA        HSEDIC
015200030115     C                   ENDIF
015300020508     C*
015400030115     C                   WRITE     FNVACHSE1
015500080924     C                   DELETE    TIVGD000
015600020508     C*
015700080924     C                   READ      TIVGDTMP
015800030115     C                   ENDDO
015900991027     C*
016000030115     C                   EVAL      wrkesito = '0'
016100991027     C*
016200910830     C                   ENDSR
016300020508
016400020508
016500020508
016600991027
016700991027      /TITLE Operazioni iniziali.
016800991027     C     *inzsr        BEGSR
016900991027     C*
017000991027     C     *ENTRY        PLIST
017100991027     C                   parm                    prmppt
017200991027     C     wrkesito      parm      wrkesito      prmesito
017300030115     C*
017400030115     C* CALCOLA LA DATA CORRENTE
017500030115     C                   time                    wn14             14 0
017600030115     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
017700030115     C                   z-add     wn8           g08dat
017800030115     C                   z-add     *zeros        g08inv
017900030115     C                   movel     '0'           g08err
018000030115     C                   call      'XSRDA8'
018100030115     C                   parm                    wlbda8
018200030115     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
018300030115     C*
018400030115     C* Definizione chiavi di lettura
018500030115     C*
018600050310     C* - TIGCP51L: COMPLETA
018700030115     C     KEYGCP        KLIST
018800030115     C                   KFLD                    vacAAS
018900030115     C                   KFLD                    vacLNP
019000030115     C                   KFLD                    vacNRS
019100030115     C                   KFLD                    vacNSP
019200030115     C                   KFLD                    gcpFRG
019300050310     C* - TIGNP01L: PARZIALE
019400030115     C     KEYGNP        KLIST
019500030115     C                   KFLD                    gcpAGC
019600030115     C                   KFLD                    gcpFGC
019700030115     C                   KFLD                    gcpNGC
019800030115     C                   KFLD                    gcpFRG
019900050310     C                   KFLD                    gnpFAS
020000030115     C* - TNCSB03L: COMPLETA
020100030115     C     KEYCSB        KLIST
020200030115     C                   KFLD                    vacAAS
020300030115     C                   KFLD                    vacLNP
020400030115     C                   KFLD                    vacNRS
020500030115     C                   KFLD                    vacNSP
020600991027     C*
020700991027     C                   ENDSR
