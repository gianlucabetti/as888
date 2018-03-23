000100040220      *------------------------------------------------------------------------*
000200040220      *                CALCOLO DATI DELTA CLIENTE                              *
000300040220      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500040220
000600990609     FSISDC01L  IF   E           K DISK
000700040220
000800040220      *-------------------------------------
000900040220     D KSCKSC          s                   like(sdcksc)
001000040220     d KSCANN          S                   like(SDCANN)
001100040220     d KSCMES          S                   like(SDCMES)
001200040220     d $TSP            S                   like(SDCTSP)
001300040220     d $TPO            S                   like(SDCTPO)
001400040220     d $CTR            S                   like(D60CT1)
001500040220     d $FPE            S                   like(SDCFPE)
001600040220     d $SDX            S              3
001700040220     d $ROK            S              1
001800040220     d $DAT            S              6  0
001900040220     d RICDEL          S             16  3
002000040220     d WRECOK          S              1    INZ('N')
002100040220     d $FINC           S              1    INZ('0')
002200040220     d SELFPE          S              1    INZ('T')
002300040220     d SELSER          S              1    INZ('T')
002400040220     d SELPOR          S              1    INZ('T')
002500040220     d SELPRO          S              1    INZ('T')
002600040220     d SELCTR          S              1    INZ('T')
002700040220      *-------------------
002800040220      * DS DI PROCEDURA
002900040220      *-------------------
003000040220     D TISE68DS      E DS
003100040220      *-------------------
003200040220      * DS DEFINITA per COPY TISE62T
003300040220      *-------------------
003400040219     D TISE60DS      E DS
003500040220     D*------------------
003600040220     D* DS UNIONE DATA SISDC01L
003700040220     D*------------------
003800040220     D                 DS
003900040220     D  SDCANN                 1      4  0
004000040220     D  SDCMES                 5      6  0
004100040220     D  SDCDAT                 1      6  0
004200040220      *------------------
004300040220      * DS SUDDIVISIONE DATA INIZIALE
004400040220      *------------------
004500000000     D                 DS
004600990601     D  ANNINI                 1      4  0
004700990601     D  MESINI                 5      6  0
004800990601     D  DATINI                 1      6  0
004900040220      *------------------
005000040220      * DS SUDDIVISIONE DATA FINALE
005100040220      *------------------
005200000000     D                 DS
005300990601     D  ANNFIN                 1      4  0
005400990601     D  MESFIN                 5      6  0
005500990601     D  DATFIN                 1      6  0
005600040220
005700040220      *------------------------------------------------------------         ---*
005800000000     C*
005900000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
006000000000     C                   EXSR      SETFI1
006100000000     C*
006200000000     C* CICLO FINO A FINE FILE SALDI CLIENTE
006300000000DO  1C     $FINC         DOWEQ     '0'
006400000000     C*
006500000000     C* MEMORIZZA I SALDI CLIENTE
006600000000     C                   EXSR      MEMSDC
006700000000     C*
006800000000     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
006900000000     C                   EXSR      LETFI1
007000000000     C*
007100000000E   1C                   ENDDO
007200000000     C*
007300040219     c                   eval      *inlr = *on
007400000000     C*------------------------------------------------------------------------*
007500000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
007600000000     C*------------------------------------------------------------------------*
007700000000     C     SETFI1        BEGSR
007800000000     C*
007900000000     C                   MOVEL     '0'           $FINC
008000000000     C*
008100040220     C                   Z-ADD     I68KSC        KSCKSC                         *DEPOSITO CLIENTE
008200000000     C                   Z-ADD     ANNINI        KSCANN                         *ANNO INIZIALE
008300000000     C                   Z-ADD     MESINI        KSCMES                         *MESE INIZIALE
008400000000     C*
008500000000     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
008600990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
008700000000     C*
008800000000IF  1C     *IN99         IFEQ      '1'
008900000000     C                   MOVEL     '1'           $FINC
009000000000X   1C                   ELSE
009100000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
009200000000E   1C                   ENDIF
009300000000     C*
009400000000     C                   ENDSR
009500000000     C*------------------------------------------------------------------------*
009600000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI CLIENTE
009700000000     C*------------------------------------------------------------------------*
009800000000     C     LETFI1        BEGSR
009900000000     C*
010000000000     C* LEGGO FINO A:
010100000000     C*     - FINE FILE
010200000000     C*     - TROVATO RECORD VALIDO
010300000000     C                   MOVEL     'N'           WRECOK
010400000000DO  1C     $FINC         DOWEQ     '0'
010500000000     C     WRECOK        ANDEQ     'N'
010600000000     C*
010700990609     C     KSCKSC        READE     SISDC01L                               99
010800000000     C*
010900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
011000000000     C                   MOVEL     '1'           $FINC
011100000000X   2C                   ELSE
011200000000     C                   EXSR      VALSDC                                       *VALORIZZA CON "SDC"
011300000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
011400030520     C*
011500030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
011600030520IF  3C     SDCNSP        IFEQ      *ZEROS
011700030520     C     SDCPKG        ANDEQ     *ZEROS
011800030520     C     SDCPCC        ANDEQ     *ZEROS
011900030520     C     SDCPLS        ANDEQ     *ZEROS
012000030520     C     SDCNCO        ANDEQ     *ZEROS
012100030520     C     SDCVOL        ANDEQ     *ZEROS
012200030520     C     SDCVCC        ANDEQ     *ZEROS
012300030520     C     SDCVLS        ANDEQ     *ZEROS
012400030520     C     SDCIRB        ANDEQ     *ZEROS
012500030520     C     SDCIRR        ANDEQ     *ZEROS
012600030520     C     SDCINC        ANDEQ     *ZEROS
012700030520     C     SDCIRP        ANDEQ     *ZEROS
012800030520     C     SDCIRC        ANDEQ     *ZEROS
012900030520     C     SDCIRD        ANDEQ     *ZEROS
013000030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
013100030520E   3C                   ENDIF
013200000000E   2C                   ENDIF
013300000000     C*
013400000000E   1C                   ENDDO
013500000000     C*
013600000000     C                   ENDSR
013700000000     C*------------------------------------------------------------------------*
013800990609     C*?MEMSDC - MEMORIZZA I SALDI DI SISDC01L
013900000000     C*------------------------------------------------------------------------*
014000000000     C     MEMSDC        BEGSR
014100000000     C*
014200040220     C* PER LA "DELTA"  VANNO BENE TUTTI I TIPI SPEDIZIONE
014300000000     C*
014400000000     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
014500000000     C                   Z-ADD     *ZEROS        RICDEL           16 3
014600040220     C     SDCIRD        ADD       SDCIRR        RICDEL
014700040220     C     SDCINC        ADD       RICDEL        RICDEL                         *RICAVO REALE DELTA
014800040220     C                   Add       RICDEL        O68RIR
014900040220     C* RICAVO PRESUNTO DELTA
015000040220     C                   ADD       SDCIRP        O68RIP
015100000000     C*
015200000000     C                   ENDSR
015300040219     C*------------------------------------------------------------------------*
015400040219     C*?VALSDC - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
015500040219     C*------------------------------------------------------------------------*
015600040219     C     VALSDC        BEGSR
015700040219     C*
015800040220     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
015900040219     C                   Z-ADD     SDCDAT        $DAT                           *DATA (AA/MM)
016000040219     C                   MOVEL     SDCTSP        $TSP                           *TIPO SERVIZIO
016100040219     C                   MOVEL     SDCTPO        $TPO                           *TIPO PORTO
016200040219     C                   MOVEL     SDCCTR        $CTR                           *CODICE TARIFFA
016300040219     C                   MOVEL     SDCFPE        $FPE                           *FASCIA DI PESO
016400040219     C*
016500040219     C                   ENDSR
016600040219     C*------------------------------------------------------------------------*
016700040219     C*?CHKRE1 - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
016800040219     C*------------------------------------------------------------------------*
016900040219     C     CHKRE1        BEGSR
017000040219     C*
017100040219     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
017200040219     C*--------------------
017300040219     C* DATA SALDI
017400040219     C*--------------------
017500040219     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
017600040220IF  1C     $DAT          IFGE      I68DAT
017700040220     C     $DAT          ANDLE     I68DA2
017800040219X   1C                   ELSE
017900040219     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
018000040220IF  2C     $DAT          IFGT      I68DA2
018100040219     C                   MOVEL     '1'           $FINC                          *ESCO DAL CICLO "SDC
018200040219E   2C                   ENDIF
018300040219E   1C                   ENDIF
018400040219     C*
018500040219IF  1C     WRECOK        IFEQ      'S'
018600040219     C*
018700040219     C* CONTROLLO IL RECORD LETTO CON I PARAMETRI GENERALI RICHIESTI A VIDEO
018800040219     C* IMPORTANDO LA ROUTINE DI CONTROLLO ESTERNA ('TISE62T')
018900040219     C                   MOVEL     'S'           $ROK                           *RECORD OK X ROUTINE
019000040219     C*
019100040220    ? /COPY GAITRASRC/SRCTISE,TISE62T
019200040219      *
019300040219     C                   MOVEL     $ROK          WRECOK
019400040219E   1C                   ENDIF
019500040219     C*
019600040219     C                   ENDSR
019700040219     C*------------------------------------------------------------------------*
019800040219     C*?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
019900040219     C*------------------------------------------------------------------------*
020000040219     C     *INZSR        BEGSR
020100040219     C*
020200040219     C* RICEVIMENTO PARAMETRI
020300040219     C     *ENTRY        PLIST
020400040220     C                   PARM                    TISE68DS
020500040220      * pulisco campi output della ds
020600040220     C                   clear                   O68RIR
020700040220     C                   clear                   O68RIP
020800040219     C*
020900040219     C* CHIAVE LETTURA SISDC01L - PARZIALE
021000040219     C     KEYSDC        KLIST
021100040219     C                   KFLD                    KSCKSC                         *CLIENTE
021200040219     C                   KFLD                    KSCANN                         *ANNO
021300040219     C                   KFLD                    KSCMES                         *MESE
021400040219     C*
021500040219     C* SUDDIVISIONE DATE VIDEO IN ANNI E MESI
021600040220     C                   Z-ADD     I68DA2        DATFIN                         *DS DATA FINALE
021700040220     C                   Z-ADD     I68DAT        DATINI                         *DS DATA INIZIALE
021800040220     C*
021900040219     C                   ENDSR
022000040219     C*------------------------------------------------------------------------*
