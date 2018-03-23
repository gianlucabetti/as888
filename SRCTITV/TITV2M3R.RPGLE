000100120806     H DECEDIT('0,') DATEDIT(*DMY.)
000200140804     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030924     FTABEL00F  IF   E           K DISK
000500030709     FTITAS30C  IF   E           K DISK
000600120806     Ftita730c  IF   E           K DISK
000700090416     Ftivgdtmp  UF   E             DISK
000800090309     FTIVAWWWT  UF A E             DISK    commit
000900030924
001000090416     D tivafds       e ds                  extname(tivaf00t)
001100030924     D prmppt          S             50
001200030924     D prmesito        S              1
001300030924     D wrkesito        S                   like(prmesito)
001400120806     D DSCC          E DS
001500120806     D sepFld          s              1    inz(';')
001600120806     D iFAT            S              4  0 inz(*zeros)
001700120806     D wFAT            S              4  0 inz(*zeros)
001800120806     D iSVN            S              3  0 inz(*zeros)
001900120806     D wSVNDES         S             20    inz
002000120806     D wSVN            S                   like(vafSV1)
002100120806     D wVAN            S                   like(vafIMV)
002200120806
002300030924
002400030924     D*------------
002500030924     D* SCHIERE SIGLE E DESCRIZIONI VARIE
002600030924     D*------------
002700120806     D skFATSVN        S             15    DIM(9999)
002800120806     D skFATSVNI       S             13  3 DIM(9999)
002900120806     D skSVN           S              1    DIM(999)
003000120806     D skSVND          S             20    DIM(999)
003100120806
003200120806
003300120806     D*-----------
003400120806     D* DEFINISCO LA DS DELLA STRUTTURA DELLA SCHIERA FATTURE/VARIE
003500120806     D*-----------
003600120806     D DS_Fatt_Varie   DS
003700120806     D  Data_Fatt                          like(vafDFT)
003800120806     D  Num_Fatt                           like(vafNFT)
003900120806     D  Sigla_Varia                        like(vafSV1)
004000030924
004100030924
004200030325
004300030924     C* MAIN/
004400030924     C                   EXSR      cartab
004500120806     C                   EXSR      chkRECFILMBR
004600991027     C                   EXSR      traduci
004700120806     C*
004800921023     C                   SETON                                        LR
004900030908
005000030924
005100030924
005200030924     C     cartab        BEGSR
005300030924     C*
005400030924     C                   clear                   skSVN
005500120806     C                   clear                   skSVND
005600030924     C*
005700030924     C* Reperisco le descrizioni delle varie
005800120806     C                   Z-ADD     *zeros        iSVN
005900030924     C                   Z-ADD     1             tblKUT
006000030924     C                   MOVEL(P)  'CC'          tblCOD
006100030924     C     KEYtabel      SETLL     TABEL00F
006200030924     C                   IF        %equal(TABEL00F)
006300030924     C     KEYtabel      READE     TABEL00F
006400030924     C                   DOW       not %eof(TABEL00F)
006500120806     C                   ADD       1             iSVN
006600120806     C                   MOVE(P)   TBLKEY        skSVN(iSVN)
006700030924     C                   MOVEL(P)  TBLUNI        DSCC
006800120806     C                   MOVEL(P)  §CCDES        skSVND(iSVN)
006900030924     C     KEYtabel      READE     TABEL00F
007000030924     C                   ENDDO
007100030924     C                   ENDIF
007200030924     C*
007300030924     C                   ENDSR
007400120806
007500120806
007600120806
007700120806     C     chkRECFILMBR  BEGSR
007800120806     C*
007900120806     C                   READ      TIVAWWWT                               55
008000120806     C*
008100120806     C* Se vuoto inserisco riga di intestazione
008200120806     C                   IF        *IN55
008300120806     C                   EVAL      VAWDTA  =
008400120806     C                                'DATA FATTURA'        +sepFld+
008500120806     C                                'NUMERO FATTURA'      +sepFld+
008600120806     C                                'SIGLA VARIA'         +sepFld+
008700120806     C                                'DESCRIZIONE VARIA'   +sepFld+
008800120806     C                                'TOTALE VARIA'        +sepFld
008900120806     C                   WRITE     TIVAW000
009000120806     C                   ENDIF
009100120806     C*
009200120806     C                   ENDSR
009300991027
009400030924
009500030924
009600991027     C     traduci       BEGSR
009700991027     C*
009800120806     C                   clear                   skFATSVN
009900120806     C                   clear                   skFATSVNI
010000120806     C                   z-add     *zeros        iFAT
010100120806     C*
010200090416     C                   READ      tivgdtmp
010300090416     C                   dow       not %eof(tivgdtmp)
010400090416     C                   movel     vgddta        tivafds
010500030709     C*
010600030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
010700120806     C     KEYtas30_C    CHAIN     titas30c
010800030709     C                   IF        %found(titas30c)
010900120806     C*
011000120806     C* Se trovo la bolla => procedo totalizzando le varie raggruppate x fattura
011100120806     C                   exsr      exeSVN
011200120806     C                   ENDIF
011300120806     C*
011400090416     C                   DELETE    tivgd000
011500991027     C*
011600090416     C                   READ      tivgdtmp
011700030325     C                   ENDDO
011800120806     C*
011900120806     C* Al termine scorro le fatture/varie x l'output
012000120806     C                   z-add     1             iFAT
012100120806     C                   dow       iFAT <= %elem(skFATSVN)
012200120806     C                   if        skFATSVN(iFAT) = *blanks
012300120806     C                   leave
012400120806     C                   else
012500120806     C                   eval      DS_Fatt_Varie = skFATSVN(iFAT)
012600120806     C                   if        Sigla_Varia = *blanks
012700120806     C                   eval      wSVNDES = 'PORTO'
012800120806     C                   else
012900120806     C                   z-add     1             iSVN
013000120806     C     Sigla_Varia   lookup    skSVN(iSVN)                            55
013100120806     C                   if        %found
013200120806     C                   eval      wSVNDES = skSVND(iSVN)
013300120806     C                   else
013400120806     C                   eval      wSVNDES = *all'*'
013500120806     C                   endif
013600120806     C                   endif
013700120806     C                   endif
013800120806     C*
013900120806     C* Emetto l'output
014000120806     C                   EVAL      VAWDTA =
014100120806     C                              %editc(Data_Fatt:'X')              +sepFld+
014200120806     C                              %editc(Num_Fatt:'X')               +sepFld+
014300120806     C                              Sigla_Varia                        +sepFld+
014400120806     C                              %trim(wSVNDES)                     +sepFld+
014500120806     C                              %trim(%editc(skFATSVNI(iFAT):'4')) +sepFld
014600120806     C*
014700120806     C                   WRITE     TIVAW000
014800120806     C*
014900120806     C* Proseguo con lo scorrimento delle totalizzazioni fatture/varie
015000120806     C                   add       1             iFAT
015100120806     C                   enddo
015200991027     C*
015300030325     C                   EVAL      wrkesito = '0'
015400991027     C*
015500910830     C                   ENDSR
015600120806     C*------------------------------------------------------------------------*
015700120806     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
015800120806     C*------------------------------------------------------------------------*
015900120806     C     EXESVN        BEGSR
016000120806     C*
016100120806     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
016200120806     C                   MOVEL     *blanks       wSVN
016300120806     C                   Z-ADD     *zeros        wVAN
016400120806     C*
016500120806     C* Verifica ed eventuale scrittura del PORTO su TITAS
016600120806     C                   IF        tasPOR <> *zeros
016700120806     C                   EVAL      wSVN = *blanks
016800120806     C                   EVAL      wVAN = tasPOR
016900120806     C                   EXSR      WRISVN
017000120806     C                   ENDIF
017100120806     C*
017200120806     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
017300120806     C                   IF        tasSV1 <> *blanks
017400120806     C                   EVAL      wSVN = tasSV1
017500120806     C                   EVAL      wVAN = tasVA1
017600120806     C                   EXSR      WRISVN
017700120806     C                   ENDIF
017800120806     C*
017900120806     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
018000120806     C                   IF        tasSV2 <> *blanks
018100120806     C                   EVAL      wSVN = tasSV2
018200120806     C                   EVAL      wVAN = tasVA2
018300120806     C                   EXSR      WRISVN
018400120806     C                   ENDIF
018500120806     C*
018600120806     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
018700120806     C                   IF        tasSV3 <> *blanks
018800120806     C                   EVAL      wSVN = tasSV3
018900120806     C                   EVAL      wVAN = tasVA3
019000120806     C                   EXSR      WRISVN
019100120806     C*
019200120806     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file estens.
019300120806     C     KEYta730_P    SETLL     tita730c
019400120806     C                   IF        %equal(tita730c)
019500120806     C     KEYta730_P    READE     tita730c
019600120806     C                   DOW       not %eof(tita730c)
019700120806     C                   if        ta7VAN > *zeros
019800120806     C                   EVAL      wSVN = ta7SVN
019900120806     C                   EVAL      wVAN = ta7VAN
020000120806     C                   EXSR      WRISVN
020100120806     C                   endif
020200120806     C     KEYta730_P    READE     tita730c
020300120806     C                   ENDDO
020400120806     C                   ENDIF
020500120806     C                   ENDIF
020600120806     C*
020700120806     C                   ENDSR
020800120806     C*------------------------------------------------------------------------*
020900120806     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
021000120806     C*------------------------------------------------------------------------*
021100120806     C     WRISVN        BEGSR
021200120806     C*
021300120806     C* Elaboro tutte le varie tranne la 8 (IVA)
021400120806     C                   IF        wSVN <> '8'
021500120806     C*
021600120806     C                   eval      Data_Fatt   = vafDFT
021700120806     C                   eval      Num_Fatt    = vafNFT
021800120806     C                   eval      Sigla_Varia = wSVN
021900120806     C*
022000120806     C* Sommo la varia corrente a "se stessa"
022100120806     C                   Z-ADD     1             wFAT
022200120806     C     DS_Fatt_Varie LOOKUP    skFATSVN(wFAT)                         55
022300120806     C                   IF        %found
022400120806     C                   EVAL      skFATSVNI(wFAT) = skFATSVNI(wFAT) + wVAN
022500120806     C                   ELSE
022600120806     C                   ADD       1             iFAT
022700120806     C                   EVAL      skFATSVN(iFAT)  = DS_Fatt_Varie
022800120806     C                   EVAL      skFATSVNI(iFAT) = wVAN
022900120806     C                   ENDIF
023000120806     C*
023100120806     C                   ENDIF
023200120806     C*
023300120806     C                   ENDSR
023400120806
023500120806
023600991027
023700991027      /TITLE Operazioni iniziali.
023800991027     C     *inzsr        BEGSR
023900991027     C*
024000991027     C     *ENTRY        PLIST
024100991027     C                   parm                    prmppt
024200991027     C     wrkesito      parm      wrkesito      prmesito
024300030709     C*
024400030709     C* Definizione chiavi
024500030924     C*
024600030924     C* Chiave su TABEL00F - Completa
024700030924     C     KEYtabel      KLIST
024800030924     C                   KFLD                    tblKUT
024900030924     C                   KFLD                    tblCOD
025000030709     C*
025100030709     C* Chiave su TITAS30C - Completa
025200120806     C     KEYtas30_C    KLIST
025300030709     C                   KFLD                    vafAAS
025400030709     C                   KFLD                    vafLNP
025500030709     C                   KFLD                    vafNRS
025600030709     C                   KFLD                    vafNSP
025700030709     C                   KFLD                    vafTBL
025800120806     C*
025900120806     C* LETTURA tita730C - Parziale
026000120806     C     KEYta730_P    KLIST
026100120806     C                   KFLD                    vafAAS                         *ANNO BOLLA
026200120806     C                   KFLD                    vafLNP                         *LINEA PARTENZA BOLL
026300120806     C                   KFLD                    vafNRS                         *SERIE BOLLA
026400120806     C                   KFLD                    vafNSP                         *NUMERO SPEDIZIONE B
026500120806     C                   KFLD                    vafTBL                         *TIPO BOLLA
026600991027     C*
026700991027     C                   ENDSR
