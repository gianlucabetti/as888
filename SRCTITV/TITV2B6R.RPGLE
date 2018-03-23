000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400101230     Ftitas30c  IF   E           K DISK
000500120516     Ftitaa30c  IF   E           K DISK
000600101230     Ftita730c  IF   E           K DISK
000700120516     Fcnaco00f  IF   E           K DISK
000800120516     Fcnind00f  IF   E           K DISK
000900090325     Ftivgdtmp  uF   E             DISK
001000091201     FTIVAWWWT  UF A E             DISK    commit
001100991027
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500090325     D tivafds       e ds                  extname(tivaf00t)
001600101230     D wSVN            s                   like(ta7SVN)
001700101230     D wVAN            s                   like(ta7VAN)
001800071205
001900101230
002000101230     D*----------
002100101230     D* DS CLIENTE FATTTURE
002200101230     D*----------
002300101230     D DS_Cli_Fatt     DS
002400101230     D  DS_CF_DataSpe                 8  0 inz
002500101230     D  DS_CF_NumSpe                 15    inz
002600101230     D  DS_CF_DocCli                 15  0 inz
002700101230     D  DS_CF_DataFat                 8  0 inz
002800101230     D  DS_CF_NumFat                 15    inz
002900101230     D  DS_CF_CoPorto                13  3 inz
003000101230     D  DS_CF_CoAssic                13  3 inz
003100101230     D  DS_CF_CoContr                13  3 inz
003200101230     D  DS_CF_CoSprin                13  3 inz
003300101230     D  DS_CF_CoGiac                 13  3 inz
003400101230     D  DS_CF_CoAltri                13  3 inz
003500101230     D  DS_CF_Note                  100    inz
003600120516     D  DS_CF_MittRSC                35    inz
003700120516     D  DS_CF_MittCAP                 9    inz
003800120516     D  DS_CF_MittLOC                35    inz
003900120516     D  DS_CF_MittPRV                 2    inz
004000120516     D  DS_CF_DestRSC                35    inz
004100120516     D  DS_CF_DestCAP                 9    inz
004200120516     D  DS_CF_DestLOC                35    inz
004300120516     D  DS_CF_DestPRV                 2    inz
004400120516     D  DS_CF_PesoTas                 5  0 inz
004500101230
004600101230
004700101230     C*
004800991027     C                   EXSR      traduci
004900921023     C                   SETON                                        LR
005000101230
005100101230
005200991027
005300101230     C     traduci       BEGSR
005400991027     C*
005500090325     C                   READ      tivgdtmp
005600090325     C                   DOW       not %eof
005700101230     C                   movel     vgddta        tivafds
005800101230     C*
005900101230     C                   MOVEL     *blanks       wSVN
006000101230     C                   Z-ADD     *zeros        wVAN
006100101230     C*
006200101230     C                   reset                   DS_Cli_Fatt
006300101230     C                   eval      DS_CF_DataSpe = vafAAS*10000+vafMGS
006400101230     C                   eval      DS_CF_NumSpe  = %editc(vafLNP:'X')+
006500101230     C                                             %editc(vafNRS:'X')+
006600101230     C                                             %editc(vafNSP:'X')+vafTBL
006700101230     C                   eval      DS_CF_DocCli  = vafRMN
006800101230     C                   eval      DS_CF_DataFat = vafDFT
006900101230     C                   eval      DS_CF_NumFat  = %editc(vafNFT:'X')
007000101230     C                   evalr     DS_CF_Note    = '.'
007100030709     C*
007200030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
007300101230     C     KEYtas30C     CHAIN     titas30c
007400030709     C                   IF        %found(titas30c)
007500120516     C                   EVAL      DS_CF_PesoTas = tasPVL
007600120516     C*
007700120516     C* Reperisco i dati del mittente della spedizione
007800120516     C                   CLEAR                   DS_CF_MittRSC
007900120516     C                   CLEAR                   DS_CF_MittCAP
008000120516     C                   CLEAR                   DS_CF_MittLOC
008100120516     C                   CLEAR                   DS_CF_MittPRV
008200120516     C                   MOVE      tasCCM        wMITT             4
008300120516     C                   IF        wMITT = '8888' or wMITT = '9999'
008400120516     C                   EVAL      taaTRC = 'M'
008500120516     C     KEYtaa30C     CHAIN     titaa30c
008600120516     C                   IF        %found(titaa30c)
008700120516     C                   EVAL      DS_CF_MittRSC = taaRSC
008800120516     C                   EVAL      DS_CF_MittCAP = taaCAP
008900120516     C                   EVAL      DS_CF_MittLOC = taaLOC
009000120516     C                   EVAL      DS_CF_MittPRV = taaPRV
009100120516     C                   ENDIF
009200120516     C                   ELSE
009300120516     C                   EVAL      acoKUT = 1
009400120516     C                   EVAL      acoKCC = 151
009500120516     C                   EVAL      acoKSC = tasCCM
009600120516     C     KEYaco00C     CHAIN     cnaco00f
009700120516     C                   IF        %found(cnaco00f)
009800120516     C     KEYaco00C     CHAIN     cnind00f
009900120516     C                   IF        %found(cnind00f)
010000120516     C                   EVAL      DS_CF_MittRSC = acoRAG
010100120516     C                   MOVEL     indCAP        DS_CF_MittCAP
010200120516     C                   EVAL      DS_CF_MittLOC = indCIT
010300120516     C                   EVAL      DS_CF_MittPRV = indPRV
010400120516     C                   ENDIF
010500120516     C                   ENDIF
010600120516     C                   ENDIF
010700120516     C*
010800120516     C* Valorizzo i dati relativi al destinatario
010900120516     C                   EVAL      DS_CF_DestRSC = tasRSD
011000120516     C                   EVAL      DS_CF_DestCAP = tasCAD
011100120516     C                   EVAL      DS_CF_DestLOC = tasLOD
011200120516     C                   EVAL      DS_CF_DestPRV = tasPRD
011300101230     C*
011400101230     C* Verifica ed eventuale scrittura del PORTO su TITAS
011500101230     C                   IF        tasPOR <> *zeros
011600101230     C                   eval      DS_CF_CoPorto = tasPOR
011700101230     C                   ENDIF
011800101230     C*
011900101230     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
012000101230     C                   IF        tasSV1 <> *blanks
012100101230     C                   eval      wSVN = tasSV1
012200101230     C                   eval      wVAN = tasVA1
012300101230     C                   exsr      EXESVN
012400101230     C                   ENDIF
012500101230     C*
012600101230     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
012700101230     C                   IF        tasSV2 <> *blanks
012800101230     C                   eval      wSVN = tasSV2
012900101230     C                   eval      wVAN = tasVA2
013000101230     C                   exsr      EXESVN
013100101230     C                   ENDIF
013200101230     C*
013300101230     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
013400101230     C                   IF        tasSV3 <> *blanks
013500101230     C                   eval      wSVN = tasSV3
013600101230     C                   eval      wVAN = tasVA3
013700101230     C                   exsr      EXESVN
013800101230     C*
013900101230     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
014000120516     C     KEYta730P     SETLL     tita730c
014100101230     C                   IF        %equal(tita730c)
014200120516     C     KEYta730P     READE     tita730c
014300101230     C                   DOW       not %eof(tita730c)
014400101230     C                   if        ta7VAN > *zeros
014500101230     C                   eval      wSVN = ta7SVN
014600101230     C                   eval      wVAN = ta7VAN
014700101230     C                   exsr      EXESVN
014800101230     C                   endif
014900120516     C     KEYta730P     READE     tita730c
015000101230     C                   ENDDO
015100101230     C                   ENDIF
015200101230     C                   ENDIF
015300101230     C*
015400101230     C                   ENDIF
015500101230     C*
015600101230     C* Scarico il buffer in formato cliente
015700101230     C                   eval      vawDTA = DS_Cli_Fatt
015800930409     C*
015900091201     C                   WRITE     TIVAW000
016000090325     C                   delete    tivgd000
016100991027     C*
016200090325     C                   READ      tivgdtmp
016300030325     C                   ENDDO
016400991027     C*
016500030325     C                   EVAL      wrkesito = '0'
016600991027     C*
016700910830     C                   ENDSR
016800101230
016900101230
017000101230     C*------------------------------------------------------------------------*
017100101230     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
017200101230     C*------------------------------------------------------------------------*
017300101230     C     EXESVN        BEGSR
017400101230     C*
017500101230     C                   select
017600101230     C                   when      wSVN = 'd'
017700101230     C                   add       wVAN          DS_CF_CoAssic
017800101230     C                   when      wSVN = 'R'
017900101230     C                   add       wVAN          DS_CF_CoAssic
018000101230     C                   when      wSVN = 'G'
018100101230     C                   add       wVAN          DS_CF_CoContr
018200101230     C                   when      wSVN = 'e'
018300101230     C                   add       wVAN          DS_CF_CoSprin
018400101230     C                   when      wSVN = 'I'
018500101230     C                   add       wVAN          DS_CF_CoGiac
018600101230     C                   other
018700101230     C                   add       wVAN          DS_CF_CoAltri
018800101230     C                   endsl
018900101230     C*
019000101230     C                   ENDSR
019100991027
019200101230
019300101230
019400991027      /TITLE Operazioni iniziali.
019500991027     C     *inzsr        BEGSR
019600991027     C*
019700991027     C     *ENTRY        PLIST
019800991027     C                   parm                    prmppt
019900991027     C     wrkesito      parm      wrkesito      prmesito
020000030709     C*
020100030709     C* Definizione chiavi
020200030709     C*
020300030709     C* Chiave su TITAS30C - Completa
020400120516     C     KEYtas30C     KLIST
020500030709     C                   KFLD                    vafAAS
020600030709     C                   KFLD                    vafLNP
020700030709     C                   KFLD                    vafNRS
020800030709     C                   KFLD                    vafNSP
020900030709     C                   KFLD                    vafTBL
021000101230     C*
021100120516     C* LETTURA TITA730C - PARZIALE
021200120516     C     KEYta730P     KLIST
021300101230     C                   KFLD                    tasAAS
021400101230     C                   KFLD                    tasLNP
021500101230     C                   KFLD                    tasNRS
021600101230     C                   KFLD                    tasNSP
021700101230     C                   KFLD                    tasTBL
021800120516     C*
021900120516     C* Chiave su TITAA30C - Completa
022000120516     C     KEYtaa30C     KLIST
022100120516     C                   KFLD                    vafAAS
022200120516     C                   KFLD                    vafLNP
022300120516     C                   KFLD                    vafNRS
022400120516     C                   KFLD                    vafNSP
022500120516     C                   KFLD                    taaTRC
022600120516     C*
022700120516     C* Chiave su CNACO00F - Completa
022800120516     C     KEYaco00C     KLIST
022900120516     C                   KFLD                    acoKUT
023000120516     C                   KFLD                    acoKCC
023100120516     C                   KFLD                    acoKSC
023200101230     C*
023300991027     C                   ENDSR
