000100000000     H DECEDIT('0.') DATEDIT(*DMY.)
000200161023     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300000000
000400161023     FTNTBE01L  IF   E           K DISK
000500161023     FAZORG01L  IF   E           K DISK
000600000000     FTITAS30C  IF   E           K DISK
000700000000     FTIVGDTMP  UF   E             DISK
000800000000     Ftivawwwt  uF A E             DISK    commit
000900161023
001000161023     D*------------------
001100161023     D* LINKING A DEFINIZIONI ESTERNE
001200161023     D*------------------
001300161023     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
001400161023     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
001500000000
001600000000     D prmppt          s             50
001700000000     D prmesito        s              1
001800000000     D wrkesito        s                   like(prmesito)
001900000000
002000000000     D wfdscds       e ds                  extname(wfdsc00f)
002100161023     D OG143         e ds
002200161023     D DNTW          e ds
002300000000     D wPKF            s              8
002400000000     D wVLF            s              6
002500000000     D wCAS            s             14
002600000000     D wIFT            s             14
002700000000
002800000000     C                   EXSR      chkRECFILMBR
002900000000     C                   EXSR      traduci
003000000000     C                   SETON                                        LR
003100000000
003200000000     C     chkRECFILMBR  BEGSR
003300000000     C*
003400000000     C                   READ      TIVAWWWT                               55
003500000000     C*
003600000000     C                   ENDSR
003700000000
003800000000     C     traduci       BEGSR
003900000000     C*
004000000000     C* Verifico se richiesto un particolare carattere d separatore campi
004100000000     C                   IF        %subst(prmppt:3:1) <> *blanks
004200000000     C                   EVAL      wSepCSV = %subst(prmppt:3:1)
004300000000     C                   ELSE
004400000000     C                   MOVEL     ','           wSepCSV           1
004500000000     C                   ENDIF
004600000000     C*
004700000000     C* Verifico se richiesto un particolare carattere d separatore decimale
004800000000     C                   SETON                                        60
004900000000     C                   IF        %subst(prmppt:4:1) = ','
005000000000     C                   SETOFF                                       60
005100000000     C                   ENDIF
005200000000     C*
005300000000     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005400000000     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005500000000     C* Riga 1
005600000000     C                   EVAL      VAWDTA  =
005700000000     C                             'SCAC'                              +wSepCSV+
005800000000     C                             'Coll Request date'                 +wSepCSV+
005900000000     C                             'Requested Collection Date'         +wSepCSV+
006000000000     C                             'Collection Date'                   +wSepCSV+
006100000000     C                             'Date shipment reached ' +
006200000000     C                             'carrier last hub'                  +wSepCSV+
006300000000     C                             'Delivery Date'                     +wSepCSV+
006400000000     C                             'Pallets'                           +wSepCSV+
006500000000     C                             'Pieces/parcel'                     +wSepCSV+
006600000000     C                             'Weight'                            +wSepCSV+
006700000000     C                             'Vendor Name'                       +wSepCSV+
006800000000     C                             'Deliver to FC'                     +wSepCSV+
006900000000     C                             'Ship Country (2 letters ' +
007000000000     C                             'country code)'                     +wSepCSV+
007100000000     C                             'References'
007200000000     C                   WRITE     TIVAW000
007300000000     C* Riga 2
007400000000     C                   EVAL      VAWDTA  =
007500000000     C                             'Mandatory'                         +wSepCSV+
007600000000     C                             'Nice to Have'                      +wSepCSV+
007700000000     C                             'Nice to Have'                      +wSepCSV+
007800000000     C                             'Mandatory'                         +wSepCSV+
007900000000     C                             'Mandatory'                         +wSepCSV+
008000000000     C                             'Mandatory'                         +wSepCSV+
008100000000     C                             'Mandatory'                         +wSepCSV+
008200000000     C                             'Nice to Have'                      +wSepCSV+
008300000000     C                             'Non Mandatory'                     +wSepCSV+
008400000000     C                             'Non Mandatory'                     +wSepCSV+
008500000000     C                             'Mandatory'                         +wSepCSV+
008600000000     C                             'One mandatory at least'            +wSepCSV+
008700000000     C                             'Mandatory (UK only )'
008800000000     C                   WRITE     TIVAW000
008900000000     C* Riga 3
009000000000     C                   EVAL      VAWDTA  =
009100000000     C                             'Generic SCAC'                      +wSepCSV+
009200000000     C                             'Format: DD/MM/YYYY'                +wSepCSV+
009300000000     C                             'Format: DD/MM/YYYY'                +wSepCSV+
009400000000     C                             'Format: DD/MM/YYYY'                +wSepCSV+
009500000000     C                             'Format: DD/MM/YYYY'                +wSepCSV+
009600000000     C                             'Format: DD/MM/YYYY'                +wSepCSV+
009700000000     C                             '"Number of pallets: integer ' +
009800000000     C                             'If null then leave the cell empty ' +
009900161023     C                             '(no space, no ""-"", no ""x""' +
010000161023     C                             ', ...)'                            +wSepCSV+
010100000000     C                             'Format: integer'                   +wSepCSV+
010200000000     C                             'Format: Decimal (one ' +
010300000000     C                             'digit after the decimal point)'    +wSepCSV+
010400000000     C                             '"No special characters: no ' +
010500161023     C                             'mutated vowel like ""ò"" or ""à"", ' +
010600000000     C                             'Less than 100 characters"'         +wSepCSV+
010700000000     C                             'Format: 4 characters only ' +
010800000000     C                             '(AMAZON convention)'               +wSepCSV+
010900000000     C                             '"Format: two letters only ' +
011000000000     C                             '(ISO 3166-1 alpha 2 standard) ' +
011100000000     C                             'More details: https://en.wikipedia.org' +
011200000000     C                             '/wiki/ISO_3166-1_alpha-2"'         +wSepCSV+
011300000000     C                             'Less than 20 characters'
011400000000     C                   WRITE     TIVAW000
011500000000     C*
011600000000     C                   ENDIF
011700000000     C*
011800000000     C                   READ      TIVGDTMP
011900000000     C                   DOW       not %eof(TIVGDTMP)
012000000000     C*
012100000000     C* Valorizzo il buffer dal campo flat di inout
012200000000     C                   EVAL      wfdscds = vgdDTA
012300000000     C*
012400000000     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
012500000000     C***  '"':''''      XLATE     wfdscds       wfdscds
012600000000     C*
012700000000     C* Innanzitutto chaino la bolla direttamente su TITAS
012800000000     C                   z-add     *zeros        wDSP              8 0
012900000000     C                   z-add     *zeros        wDTI              8 0
013000000000     C                   movel     *blanks       wFBR              1
013100000000     C     KEYtas30P     chain     titas30c
013200000000     C                   if        %found(titas30c)
013300000000     C                   eval      wFBR = tasFBR
013400000000     C                   eval      wDSP = tasAAS*10000+tasMGS
013500000000     C                   eval      wDTI = tasDTI
013600000000     C                   endif
013700161023     C*
013800161023     C* Reperisco la "regione" ovvero Italia o Europa
013900161023     C                   EXSR      REPNTW
014000000000     C*
014100000000     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
014200000000     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
014300000000     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
014400000000     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
014500000000     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
014600000000     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
014700000000     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
014800000000     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
014900000000     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
015000000000     C*
015100000000     C                   EVAL      VAWDTA = 'BART7'                  +wSepCSV  +
015200161023     C                                                             ''+wSepCSV  +
015300161023     C                                                             ''+wSepCSV  +
015400161023     C                             UBFMTDATE_Convert(%editc(wDSP:'X') :
015500161023     C                                  'YYYYMMDD' : 'DD/MM/YYYY')   +wSepCSV  +
015600161023     C                             UBFMTDATE_Convert(%editc(wDTI:'X') :
015700161023     C                                  'YYYYMMDD' : 'DD/MM/YYYY')   +wSepCSV  +
015800161023     C                             UBFMTDATE_Convert(%editc(DSCDGE:'X') :
015900161023     C                                  'YYYYMMDD' : 'DD/MM/YYYY')   +wSepCSV  +
016000161007     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCSV  +
016100161023     C                                                             ''+wSepCSV  +
016200161007     C                                                    %trim(wPKF)+wSepCSV  +
016300161007     C                                                  %trim(DSCRSM)+wSepCSV  +
016400161007     C                                                         'MXP5'+wSepCSV  +
016500161023     C                                                        wRegion+wSepCSV  +
016600161023     C                                                             ''+wSepCSV
016700000000     C*
016800000000     C                   WRITE     TIVAW000
016900000000     C*
017000000000     C                   DELETE    TIVGD000
017100000000     C*
017200000000     C                   READ      TIVGDTMP
017300000000     C                   ENDDO
017400000000     C*
017500000000     C                   EVAL      wrkesito = '0'
017600000000     C*
017700000000     C                   ENDSR
017800161023
017900161023
018000161023
018100161023     C*------------------------------------------------------------------------*
018200161023     C* REPNTW - Routine di reperimento network cliente
018300161023     C*------------------------------------------------------------------------*
018400161023     C     REPNTW        BEGSR
018500161023     C*
018600161023     C                   MOVEL     'IT'          wRegion           2
018700161023     C*
018800161023     C     DSCLNP        CHAIN     AZORG01L
018900161023     C                   IF        %found(AZORG01L) AND
019000161023     C                             ORGFVA = *blanks
019100161023     C                   EVAL      OG143 = orgDE3
019200161023     C                   EVAL      TBECOD = 'NTW'
019300161023     C                   EVAL      TBEKE1 = §OGNTW
019400161023     C     KEYtbe01P     CHAIN     TNTBE01L
019500161023     C                   IF        %found(TNTBE01L)
019600161023     C                   EVAL      DNTW = tbeUNI
019700161023     C                   IF        §NTWFIE = 'E'
019800161023     C                   EVAL      wRegion = 'EU'
019900161023     C                   ENDIF
020000161023     C                   ENDIF
020100161023     C                   ENDIF
020200161023     C*
020300161023     C                   ENDSR
020400161023     C*---------------------------------------------------------------*
020500000000
020600000000
020700000000
020800000000      /TITLE Operazioni iniziali.
020900000000     C     *inzsr        BEGSR
021000000000     C*
021100000000     C     *ENTRY        PLIST
021200000000     C                   parm                    prmppt
021300000000     C     wrkesito      parm      wrkesito      prmesito
021400000000     C*
021500000000     C* Definizione chiavi
021600000000     C*
021700000000     C* Chiave su TITAS30C - Parziale
021800000000     C     KEYtas30P     KLIST
021900000000     C                   KFLD                    dscAAS
022000000000     C                   KFLD                    dscLNP
022100000000     C                   KFLD                    dscNRS
022200000000     C                   KFLD                    dscNSP
022300161023     C*
022400161023     C* Chiave su TNTBE01L - Parziale
022500161023     C     KEYtbe01P     KLIST
022600161023     C                   KFLD                    tbeCOD
022700161023     C                   KFLD                    tbeKE1
022800000000     C*
022900000000     C                   ENDSR
