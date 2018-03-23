000100001005     H DECEDIT('0,') DATEDIT(*DMY.)
000200990901     F*------------------------------------------------------------------------*
000300990901     F* DATA BASE
000400990901     F*------------------------------------------------------------------------*
000500001005     FTITAS30C  UF   E           K DISK
000600001005     FTITA430C  IF   E           K DISK
000700001005     FTNCSB03L  IF   E           K DISK
000800001005     FFNVAC00F  IF A E           K DISK    USROPN
000900001005     FTISVT00F  O  A E           K DISK
001000001005     F*------------------------------------------------------------------------*
001100001005      * Comandi
001200001005     F*------------------------------------------------------------------------*
001300001005     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
001400001005     D  MSGDS          S             80
001500001005     D  LENGH          S             15  5
001600001005      *---------------------------------------------------------------------*
001700001005     D stringaSQL      s           5000    INZ(*blanks)
001800001005     D SqlDS         E DS                  EXTNAME(FNEVB00F:FNEVB000)
001900001005     C*------------------------------------------------------------------------*
002000001005     D DSBL4J        E DS
002100001005      *------------------------------------------------------------------------*
002200001005      * per riconoscere da quale dei tre del combinato aggiornare
002300001005      *------------------------------------------------------------------------*
002400001005     ITITAS000      01
002500001005     ITITAS010      02
002600001005     ITITASP00      03
002700001005      *------------------------------------------------------------------------*
002800001005      * imposta la parte comune a tutti gli ordinamenti
002900001005      *
003000001005     C                   eval      StringaSQL = 'Select * from FNEVB00F where '+
003100001005     C                             ' EVBDTV >20000910 and EVBLNP = 180 and '+
003200001005     C                             ' EVBCEV = ''021''  '
003300001005      *
003400001005     C/EXEC SQL
003500001005     C+ prepare S1 from :StringaSQL
003600001005     C/END-EXEC
003700001005      *
003800001005     C/EXEC SQL
003900001005     C+ declare C1 cursor for S1
004000001005     C/END-EXEC
004100001005      *
004200001005     C/EXEC SQL
004300001005     C+ open C1
004400001005     C/END-EXEC
004500001005      *
004600001005     C/EXEC SQL
004700001005     C+ Fetch C1 into :SqlDS
004800001005     C/END-EXEC
004900001005      *
005000001005     C                   dow       sqlcod=*zeros
005100001005      *
005200000204     C* ESECUZIONE COMANDI NAVIGAZIONALI
005300000204     C                   EXSR      EXESQL
005400001005      *
005500001005     C/EXEC SQL
005600001005     C+ Fetch C1 into :SqlDS
005700001005     C/END-EXEC
005800001005     C                   ENDDO
005900001005      *
006000001005     C/EXEC SQL
006100001005     C+ close C1
006200001005     C/END-EXEC
006300000204     C*
006400000204     C                   SETON                                        LR
006500000204     C*------------------------------------------------------------------------*
006600000204     C* EXESQL - ESECUZIONE COMANDI NAVIGAZIONALI
006700000204     C*------------------------------------------------------------------------*
006800000204     C     EXESQL        BEGSR
006900000204     C*
007000001005     C                   SETOFF                                       010203
007100001005     C     kEVB          Chain     TITAS30C
007200000203     C*
007300001005     C                   IF        %Found(TITAS30C) and
007400001005     C                             TASCCA='F' and TASDCM>0
007500001005     C*
007600001005     C*  file per pro-memoria
007700001005     C                   eval      SAVAAS = TASAAS
007800001005     C                   eval      SAVLNP = TASLNP
007900001005     C                   eval      SAVNRS = TASNRS
008000001005     C                   eval      SAVNSP = TASNSP
008100001005     C                   eval      SAVP00 = ' '
008200001005     C                   eval      SAV010 = ' '
008300001005     C                   eval      SAV000 = ' '
008400001005     C                   Select
008500001005     C                   When      *in03 = *on
008600001005     C                   eval      SAVP00 = 'X'
008700001005     C                   When      *in02 = *on
008800001005     C                   eval      SAV010 = 'X'
008900001005     C                   When      *in01 = *on
009000001005     C                   eval      SAV000 = 'X'
009100001005     C                   Endsl
009200001005     C                   WRITE     TISAV000
009300001005     C*
009400001005     C* aggiornamento
009500001005     C                   eval      TASCCA='L'
009600001005     C****               goto      salta
009700001005     C   01              UPDATE    TITAS000
009800001005     C   02              UPDATE    TITAS010
009900001005     C   03              UPDATE    TITASP00
010000001005     C**** salta         tag
010100001005     C*
010200001005     C* crea il FNVAC
010300001005     C     kEVB          Chain     FNVAC00F
010400001005     C*
010500001005     C                   IF        not %Found(FNVAC00F)
010600001005     C*
010700001005     C                   eval      VACAAS = TASAAS
010800001005     C                   eval      VACLNP = TASLNP
010900001005     C                   eval      VACNRS = TASNRS
011000001005     C                   eval      VACNSP = TASNSP
011100001005     C                   eval      VACMGS = TASMGS
011200001005     C                   eval      VACCBO = TASCBO
011300001005     C                   eval      VACLNA = TASLNA
011400001005     C                   eval      VACRSD = TASRSD
011500001005     C                   eval      VACPRD = TASPRD
011600001005     C                   eval      VACGC1 = TASGC1
011700001005     C                   eval      VACGC2 = TASGC2
011800001005     C                   eval      VACCTR = TASCTR
011900001005     C                   eval      VACCTS = TASCTS
012000001005     C                   eval      VACFTM = TASFTM
012100001005     C                   eval      VACFIN = TASFIN
012200001005     C                   eval      VACFAP = TASFAP
012300001005     C                   eval      VACTSP = TASTSP
012400001005     C                   eval      VACVAS = TASVAS
012500001005     C                   eval      VACNAS = TASNAS
012600001005     C                   eval      VACNCL = TASNCL
012700001005     C                   eval      VACPKB = TASPKB
012800001005     C                   eval      VACVLB = TASVLB
012900001005     C                   eval      VACQFT = TASQFT
013000001005     C                   eval      VACCCM = TASCCM
013100001005     C                   eval      VACRMN = TASRMN
013200001005     C                   eval      VACFFD = TASFFD
013300001005     C                   eval      VACTCR = TASTCR
013400001005     C                   eval      VACDCR = TASDCR
013500001005     C                   eval      VACHCR = TASHCR
013600001005     C                   eval      VACDCM = TASDCM
013700001005     C                   eval      VACHMC = TASHMC
013800001005     C                   eval      VACTC2 = TASTC2
013900001005     C                   eval      VACCCA = TASCCA
014000001005      **********
014100001005     C*
014200001005     C                   clear                   VACIAS
014300001005      *
014400001005     C     kBL4          Chain     TITA430C
014500001005     C                   IF        %Found(TITA430C)
014600001005     C                   eval      DsBL4J = TA4NOT
014700001005     C                   eval      VACIAS = §B4IAS
014800001005     C                   EndIf
014900001005      *
015000001005     C                   clear                   VACTIC
015100001005     C                   clear                   VACCAS
015200001005     C                   clear                   VACVCA
015300001005     C*
015400001005     C     kEVB          Chain     TNCSB03L
015500001005     C*
015600001005     C                   IF        %Found(TNCSB03L)
015700001005     C                   eval      VACCAS = CSBCAS
015800001005     C                   eval      VACVCA = CSBVCA
015900001005     C                   END
016000001005     C******             eval      VACRMA =    RMA
016100001005     C******             eval      VACRMO =    RMO
016200001005     C******             eval      VACTC1 =    TC1
016300001005     C******             eval      VACDLA =
016400001005     C                   eval      VACDAG = EVBDEV
016500001005     C*
016600001005     C                   WRITE     FNVAC000
016700001005     C                   END
016800001005     C                   ENDif
016900001005     C*
017000000204     C                   ENDSR
017100990901     C*------------------------------------------------------------------------*
017200990901     C* *INZSR - OPERAZIONI INIZIALI
017300990901     C*------------------------------------------------------------------------*
017400990901     C     *INZSR        BEGSR
017500000204     C*
017600001005     C     kEVB          klist
017700001005     C                   kfld                    EVBAAS
017800001005     C                   kfld                    EVBLNP
017900001005     C                   kfld                    EVBNRS
018000001005     C                   kfld                    EVBNSP
018100001005     C*
018200001005     C     kBL4          klist
018300001005     C                   kfld                    EVBAAS
018400001005     C                   kfld                    EVBLNP
018500001005     C                   kfld                    EVBNRS
018600001005     C                   kfld                    EVBNSP
018700001005     C                   kfld                    KTRC
018800001005     C                   MOVEL     'J'           KTRC              1
018900001005     C*
019000001005     C                   MOVEL     CMD(1)        MSGDS
019100001005     C                   Z-ADD     80            LENGH
019200001005     C                   CALL      'QCMDEXC'
019300001005     C                   PARM                    MSGDS
019400001005     C                   PARM                    LENGH
019500001005     C*
019600001005     C                   OPEN      fnvac00f
019700001005     C*
019800990901     C                   ENDSR
019900000203     C*------------------------------------------------------------------------*
020000001005** (CMD)
020100001005OVRDBF FILE(FNVAC00F) TOFILE(EDPAB/FNVAC00F)
