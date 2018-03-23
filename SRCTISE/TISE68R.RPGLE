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
001900040622     d XX              S              3  0
002000040622     d datcor          S              8  0
002100040622     d w0140           S             14  0
002200040220     d RICDEL          S             16  3
002300040220     d WRECOK          S              1    INZ('N')
002400040220     d $FINC           S              1    INZ('0')
002500040220     d SELFPE          S              1    INZ('T')
002600040220     d SELSER          S              1    INZ('T')
002700040220     d SELPOR          S              1    INZ('T')
002800040220     d SELPRO          S              1    INZ('T')
002900040220     d SELCTR          S              1    INZ('T')
003000040622     D*------------------
003100040622     D* DS CONTROLLO DATA (8)
003200040622     D*------------------
003300040622     D WLBDA8          DS                  INZ
003400040622     D  G08DAT                 1      8  0
003500040622     D  G08INV                 9     16  0
003600040622     D  G08ERR                17     17
003700040622     D  G08TGI                18     22  0
003800040220      *-------------------
003900040220      * DS DI PROCEDURA
004000040220      *-------------------
004100040220     D TISE68DS      E DS
004200040220      *-------------------
004300040220      * DS DEFINITA per COPY TISE62T
004400040220      *-------------------
004500040219     D TISE60DS      E DS
004600040622      *-------------------
004700040622      * DS REPERIMENTI UNIFICANTI
004800040220     D*------------------
004900040622     D TIBS10DS      E DS
005000040622     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
005100040622     D*------------------
005200040220     D* DS UNIONE DATA SISDC01L
005300040220     D*------------------
005400040220     D                 DS
005500040220     D  SDCANN                 1      4  0
005600040220     D  SDCMES                 5      6  0
005700040220     D  SDCDAT                 1      6  0
005800040220      *------------------
005900040220      * DS SUDDIVISIONE DATA INIZIALE
006000040220      *------------------
006100000000     D                 DS
006200990601     D  ANNINI                 1      4  0
006300990601     D  MESINI                 5      6  0
006400990601     D  DATINI                 1      6  0
006500040220      *------------------
006600040220      * DS SUDDIVISIONE DATA FINALE
006700040220      *------------------
006800000000     D                 DS
006900990601     D  ANNFIN                 1      4  0
007000990601     D  MESFIN                 5      6  0
007100990601     D  DATFIN                 1      6  0
007200040220
007300040220      *------------------------------------------------------------         ---*
007400040622
007500040622     C                   Z-ADD     ANNINI        KSCANN                         *ANNO INIZIALE
007600040622     C                   Z-ADD     MESINI        KSCMES                         *MESE INIZIALE
007700040622      *
007800040622    1C     1             DO        500           xx
007900040622      *
008000040622    2C                   IF        SKSC11(XX) = *zeros
008100040622     c                   leave
008200040622   x2c                   ELSE
008300040622     C                   z-add     SKSC11(XX)    kscksc
008400000000     C*
008500000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
008600000000     C                   EXSR      SETFI1
008700000000     C*
008800000000     C* CICLO FINO A FINE FILE SALDI CLIENTE
008900040622    3C     $FINC         DOWEQ     '0'
009000000000     C*
009100000000     C* MEMORIZZA I SALDI CLIENTE
009200000000     C                   EXSR      MEMSDC
009300000000     C*
009400000000     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
009500000000     C                   EXSR      LETFI1
009600000000     C*
009700040622    3C                   ENDDO
009800000000     C*
009900040622    2C                   ENDIF
010000040622     C*
010100040622    1C                   ENDDO
010200040622     C*
010300040219     c                   eval      *inlr = *on
010400000000     C*------------------------------------------------------------------------*
010500000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
010600000000     C*------------------------------------------------------------------------*
010700000000     C     SETFI1        BEGSR
010800000000     C*
010900000000     C                   MOVEL     '0'           $FINC
011000000000     C*
011100000000     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
011200990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
011300000000     C*
011400000000IF  1C     *IN99         IFEQ      '1'
011500000000     C                   MOVEL     '1'           $FINC
011600000000X   1C                   ELSE
011700000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
011800000000E   1C                   ENDIF
011900000000     C*
012000000000     C                   ENDSR
012100000000     C*------------------------------------------------------------------------*
012200000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI CLIENTE
012300000000     C*------------------------------------------------------------------------*
012400000000     C     LETFI1        BEGSR
012500000000     C*
012600000000     C* LEGGO FINO A:
012700000000     C*     - FINE FILE
012800000000     C*     - TROVATO RECORD VALIDO
012900000000     C                   MOVEL     'N'           WRECOK
013000000000DO  1C     $FINC         DOWEQ     '0'
013100000000     C     WRECOK        ANDEQ     'N'
013200000000     C*
013300990609     C     KSCKSC        READE     SISDC01L                               99
013400000000     C*
013500000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
013600000000     C                   MOVEL     '1'           $FINC
013700000000X   2C                   ELSE
013800000000     C                   EXSR      VALSDC                                       *VALORIZZA CON "SDC"
013900000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
014000030520     C*
014100030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
014200030520IF  3C     SDCNSP        IFEQ      *ZEROS
014300030520     C     SDCPKG        ANDEQ     *ZEROS
014400030520     C     SDCPCC        ANDEQ     *ZEROS
014500030520     C     SDCPLS        ANDEQ     *ZEROS
014600030520     C     SDCNCO        ANDEQ     *ZEROS
014700030520     C     SDCVOL        ANDEQ     *ZEROS
014800030520     C     SDCVCC        ANDEQ     *ZEROS
014900030520     C     SDCVLS        ANDEQ     *ZEROS
015000030520     C     SDCIRB        ANDEQ     *ZEROS
015100030520     C     SDCIRR        ANDEQ     *ZEROS
015200030520     C     SDCINC        ANDEQ     *ZEROS
015300030520     C     SDCIRP        ANDEQ     *ZEROS
015400030520     C     SDCIRC        ANDEQ     *ZEROS
015500030520     C     SDCIRD        ANDEQ     *ZEROS
015600030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
015700030520E   3C                   ENDIF
015800000000E   2C                   ENDIF
015900000000     C*
016000000000E   1C                   ENDDO
016100000000     C*
016200000000     C                   ENDSR
016300000000     C*------------------------------------------------------------------------*
016400990609     C*?MEMSDC - MEMORIZZA I SALDI DI SISDC01L
016500000000     C*------------------------------------------------------------------------*
016600000000     C     MEMSDC        BEGSR
016700000000     C*
016800040220     C* PER LA "DELTA"  VANNO BENE TUTTI I TIPI SPEDIZIONE
016900000000     C*
017000000000     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
017100000000     C                   Z-ADD     *ZEROS        RICDEL           16 3
017200040220     C     SDCIRD        ADD       SDCIRR        RICDEL
017300040220     C     SDCINC        ADD       RICDEL        RICDEL                         *RICAVO REALE DELTA
017400040220     C                   Add       RICDEL        O68RIR
017500040220     C* RICAVO PRESUNTO DELTA
017600040220     C                   ADD       SDCIRP        O68RIP
017700000000     C*
017800000000     C                   ENDSR
017900040219     C*------------------------------------------------------------------------*
018000040219     C*?VALSDC - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
018100040219     C*------------------------------------------------------------------------*
018200040219     C     VALSDC        BEGSR
018300040219     C*
018400040220     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
018500040219     C                   Z-ADD     SDCDAT        $DAT                           *DATA (AA/MM)
018600040219     C                   MOVEL     SDCTSP        $TSP                           *TIPO SERVIZIO
018700040219     C                   MOVEL     SDCTPO        $TPO                           *TIPO PORTO
018800040219     C                   MOVEL     SDCCTR        $CTR                           *CODICE TARIFFA
018900040219     C                   MOVEL     SDCFPE        $FPE                           *FASCIA DI PESO
019000040219     C*
019100040219     C                   ENDSR
019200040219     C*------------------------------------------------------------------------*
019300040219     C*?CHKRE1 - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
019400040219     C*------------------------------------------------------------------------*
019500040219     C     CHKRE1        BEGSR
019600040219     C*
019700040219     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
019800040219     C*--------------------
019900040219     C* DATA SALDI
020000040219     C*--------------------
020100040219     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
020200040220IF  1C     $DAT          IFGE      I68DAT
020300040220     C     $DAT          ANDLE     I68DA2
020400040219X   1C                   ELSE
020500040219     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020600040220IF  2C     $DAT          IFGT      I68DA2
020700040219     C                   MOVEL     '1'           $FINC                          *ESCO DAL CICLO "SDC
020800040219E   2C                   ENDIF
020900040219E   1C                   ENDIF
021000040219     C*
021100040219IF  1C     WRECOK        IFEQ      'S'
021200040219     C*
021300040219     C* CONTROLLO IL RECORD LETTO CON I PARAMETRI GENERALI RICHIESTI A VIDEO
021400040219     C* IMPORTANDO LA ROUTINE DI CONTROLLO ESTERNA ('TISE62T')
021500040219     C                   MOVEL     'S'           $ROK                           *RECORD OK X ROUTINE
021600040219     C*
021700040220    ? /COPY GAITRASRC/SRCTISE,TISE62T
021800040219      *
021900040219     C                   MOVEL     $ROK          WRECOK
022000040219E   1C                   ENDIF
022100040219     C*
022200040219     C                   ENDSR
022300040622      *------------------------------------------------------------------------*
022400040622      *?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
022500040622      *------------------------------------------------------------------------*
022600040219     C     *INZSR        BEGSR
022700040622      *
022800040622      * RICEVIMENTO PARAMETRI
022900040219     C     *ENTRY        PLIST
023000040220     C                   PARM                    TISE68DS
023100040220      * pulisco campi output della ds
023200040220     C                   clear                   O68RIR
023300040220     C                   clear                   O68RIP
023400040622      *
023500040622      * CHIAVE LETTURA SISDC01L - PARZIALE
023600040219     C     KEYSDC        KLIST
023700040219     C                   KFLD                    KSCKSC                         *CLIENTE
023800040219     C                   KFLD                    KSCANN                         *ANNO
023900040219     C                   KFLD                    KSCMES                         *MESE
024000040622     C*
024100040622     C* IMNPOSTA DATA / ORA CORRENTE
024200040622     C                   TIME                    w0140                          *ORA (6)+ DATA (8)
024300040622     C                   MOVE      w0140         G08DAT                         *DATA (8) IN G/M/AA
024400040622     C                   Z-ADD     *ZEROS        G08INV
024500040622     C                   MOVEL     '0'           G08ERR
024600040622     C                   CALL      'XSRDA8'
024700040622     C                   PARM                    WLBDA8
024800040622     C                   Z-ADD     G08INV        DATCOR                         *DATA (8) IN AA/M/G
024900040622      *
025000040622      * SUDDIVISIONE DATE VIDEO IN ANNI E MESI
025100040220     C                   Z-ADD     I68DA2        DATFIN                         *DS DATA FINALE
025200040220     C                   Z-ADD     I68DAT        DATINI                         *DS DATA INIZIALE
025300040622      *
025400040622      * IMPOSTA IL CLIENTE/I DA ELABORARE
025500040622     C                   CLEAR                   tibs10ds
025600040622      * reperisco unificati se richiesto
025700040622     C                   IF        I68TLA = 'U'
025800040622     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
025900040622     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
026000040622     C                   MOVEL     'F'           D10PAF                         *CERCA figli
026100040622     C                   z-add     I68KSC        D10cod                         *SCH ALF -> DS ALF
026200040622     C                   CALL      'TIBS10R'
026300040622     C                   PARM                    TIBS10DS
026400040622     C                   ENDIF
026500040622      * Se richiesto singolo codice o non trovato uni imposto da DS
026600040622     C                   IF        I68TLA <> 'U'  or
026700040622     C                             D10ERR <> *BLANKS                            *NO ERRORE
026800040622     C                   Z-ADD     I68KSC        sksc11(1)                      *DS NUM -> SCH NUM
026900040622     C                   ENDIF
027000040622      *
027100040219     C                   ENDSR
027200040622      *------------------------------------------------------------------------*
