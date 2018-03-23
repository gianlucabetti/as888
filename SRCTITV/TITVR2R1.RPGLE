000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600080516     FFNLBL02L  IF   E           K DISK
000700080516     FFNANM01L  IF   E           K DISK    usropn extfile(openthisANM)
000800030924
000900080924     D fnvac00t      E DS
001000030924     D prmppt          S             50
001100030924     D prmesito        S              1
001200030924     D wrkesito        S                   like(prmesito)
001300080516
001400080516     D wData           S              8    inz(*blanks)
001500080516     D openthisANM     s             21    inz('FILTRA201/FNANM01L')
001600080516     D
001700060118
001800060118     D*-----------
001900060208     D* RIDEFINIZIONE CAMPI CHIAVE X COSTRUZIONE NUMERO SPEDIZIONE COMPLETO
002000060118     D*-----------
002100060208     D E_DS_NSP        DS
002200080924     D  wVACAAS                            like(vacAAS)
002300080924     D  wVACLNP                            like(vacLNP)
002400080924     D  wVACNRS                            like(vacNRS)
002500080924     D  wVACNSP                            like(vacNSP)
002600030924
002700030924     D*-----------
002800040719     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO RITORNO ESITI DEL CLIENTE
002900030924     D*-----------
003000040719     D E_DS_Cli        DS
003100060208     D  E_CODFORN                    10    INZ('1100006637')
003200060208     D  E_RIFERIM                    10    INZ(*all'0')
003300060208     D  E_NUMSPED                    30    INZ(*all'0')
003400060208     D  E_DATACONS                    8    INZ(*all'0')
003500060208     D  E_ORACONS                     6    INZ(*all'0')
003600060208     D  E_NUMCOLLI                    3    INZ(*all'0')
003700060208     D  E_CODCOLLI1                  20    INZ(*all'0')
003800080516     D  E_CODCOLLI2                  20    INZ(*blanks)
003900080516     D  E_FINALFILLER                10    INZ(*all'0')
004000040719
004100030924
004200030325
004300030924     C* MAIN/
004400080516     C*
004500080516     C* Apro i file
004600080516     C                   IF        not %open(FNANM01L)
004700080516     C                   OPEN      FNANM01L
004800080516     C                   ENDIF
004900080516     C*
005000080516     C* Eseguo traduzione
005100991027     C                   EXSR      traduci
005200080516     C*
005300080516     C* Chiudo i file
005400080516     C                   IF        %open(FNANM01L)
005500080516     C                   CLOSE     FNANM01L
005600080516     C                   ENDIF
005700080516     C*
005800921023     C                   SETON                                        LR
005900991027
006000030924
006100030924
006200991027     C     traduci       BEGSR
006300991027     C*
006400080924     C                   READ      TIVGDTMP
006500060119     C*
006600080924     C                   DOW       not %eof(TIVGDTMP)
006700080924     C*
006800080924     C                   EVAL      fnvac00t = vgdDTA
006900080516     C*
007000080516     C* Inizializzo il flag che pilota il ritorno dell'esito corrente al cliente
007100080516     C                   MOVEL     'N'           wRitorna          1
007200060118     C*
007300030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
007400040719     C                   RESET                   E_DS_Cli
007500080516     C*
007600080516     C* Verifico indicazione x DIROTTAMENTO
007700080516     C                   IF        wRitorna = 'N'
007800080516     C                   IF        VACCCA = '1'
007900080516     C* Verifico se trattasi d bolla "figlia"
008000080516     C     KEYlbl02P     CHAIN     FNLBL02L
008100080516     C                   IF        %found(FNLBL02L)
008200080516     C                   EVAL      E_CODCOLLI1 = 'DIR'
008300080516     C                   MOVEL     lblAAN        wData
008400080516     C                   MOVE      lblMGN        wData
008500080516     C                   EVAL      E_CODCOLLI2 = wData
008600080516     C                   MOVEL     'S'           wRitorna
008700080516     C                   ENDIF
008800080516     C                   ENDIF
008900080516     C                   ENDIF
009000080516     C*
009100080516     C* Verifico indicazione x DISGUIDO
009200080516     C                   IF        wRitorna = 'N'
009300080516     C* 1° tentativo => causale 056
009400080516     C                   EVAL      anmCAA = 056
009500080516     C     KEYanm01C     CHAIN     FNANM01L
009600080516     C                   IF        %found(FNANM01L)
009700080516     C                   EVAL      E_CODCOLLI1 = 'DIS'
009800080516     C                   MOVEL     anmDAO        wData
009900080516     C                   EVAL      E_CODCOLLI2 = wData
010000080516     C                   MOVEL     'S'           wRitorna
010100080516     C                   ELSE
010200080516     C* 2° tentativo => causale 055
010300080516     C                   EVAL      anmCAA = 055
010400080516     C     KEYanm01C     CHAIN     FNANM01L
010500080516     C                   IF        %found(FNANM01L)
010600080516     C                   EVAL      E_CODCOLLI1 = 'DIS'
010700080516     C                   MOVEL     anmDAO        wData
010800080516     C                   EVAL      E_CODCOLLI2 = wData
010900080516     C                   MOVEL     'S'           wRitorna
011000080516     C                   ENDIF
011100080516     C                   ENDIF
011200080516     C                   ENDIF
011300080516     C*
011400080516     C* Restituisco al cliente solo le bolle consegnate OK con consegna anomala *blanks
011500080516     C                   IF        wRitorna = 'N'
011600080516     C                   IF        VACDCM > *zeros AND
011700080516     C                             VACCCA = *blanks
011800080516     C                   MOVEL     'S'           wRitorna
011900080516     C                   ENDIF
012000080516     C                   ENDIF
012100060208     C*
012200060208     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII
012300080516     C                   IF        wRitorna = 'S'
012400080924     C*
012500080924     C                   EVAL      wVACAAS = VACAAS
012600080924     C                   EVAL      wVACLNP = VACLNP
012700080924     C                   EVAL      wVACNRS = VACNRS
012800080924     C                   EVAL      wVACNSP = VACNSP
012900080516     C*
013000080516     C* Valorizzazione buffer out cliente
013100080516     C                   MOVE      VACRMN        E_RIFERIM
013200080516     C                   MOVE      E_DS_NSP      E_NUMSPED
013300080516     C                   MOVE      VACDCM        E_DATACONS
013400080516     C                   MOVEL     VACHMC        E_ORACONS
013500080516     C                   MOVE      VACNCL        E_NUMCOLLI
013600080516     C*
013700080516     C* Verifico eventuali forzature nel passaggio parametri
013800080516     C                   IF        prmppt <> *blanks
013900080516     C                   EVAL      E_CODFORN = %subst(prmppt:1:10)
014000080516     C                   ENDIF
014100080516     C*
014200080516     C* Quindi scarico il buffer
014300060208     C                   EVAL      VAWDTA = E_DS_Cli
014400030924     C                   WRITE     TIVAW000
014500040719     C                   ENDIF
014600040719     C*
014700040719     C* In ogni caso storicizzo e cancello l'input sempre
014800080924     C                   DELETE    TIVGD000
014900040719     C*
015000040719     C* Proseguo con la lettura dell'input
015100080924     C                   READ      TIVGDTMP
015200060119     C*
015300030325     C                   ENDDO
015400991027     C*
015500030325     C                   EVAL      wrkesito = '0'
015600991027     C*
015700910830     C                   ENDSR
015800080516
015900080516
016000080516
016100991027
016200991027      /TITLE Operazioni iniziali.
016300991027     C     *inzsr        BEGSR
016400991027     C*
016500991027     C     *ENTRY        PLIST
016600991027     C                   parm                    prmppt
016700991027     C     wrkesito      parm      wrkesito      prmesito
016800080516     C*
016900080516     C* Chiave su FNLBL02L - parziale
017000080516     C     KEYlbl02P     KLIST
017100080516     C                   KFLD                    vacAAS
017200080516     C                   KFLD                    vacLNP
017300080516     C                   KFLD                    vacNRS
017400080516     C                   KFLD                    vacNSP
017500080516     C*
017600080516     C* Chiave su FNANM01L - completa
017700080516     C     KEYanm01C     KLIST
017800080516     C                   KFLD                    vacAAS
017900080516     C                   KFLD                    vacLNP
018000080516     C                   KFLD                    vacNRS
018100080516     C                   KFLD                    vacNSP
018200080516     C                   KFLD                    anmCAA
018300991027     C*
018400991027     C                   ENDSR
