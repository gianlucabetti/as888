000100021211      *PARMS CLOSQLCSR(*ENDMOD)
000200021211      *PARMS DYNUSRPRF(*OWNER)
000300021211      *                                                                    *
000400021211      *--TRASFERIMENTO FIVAB46F DA SEDE a 001. STAMPA DATI TRASMESSI
000500001018      *
000600021211      * ------------------------------------------------------------------
000700021211     FFivab46f  uf   e           k disk    usropn
000800021211     FPRTF198   O    F  198        PRINTER OFLIND(*INOG)
000900021211     F                                     usropn
001000021211      *----------------
001100021211      * Schiere e Ds
001200021211      *----------------
001300021211     d Tibs55Ds      e ds
001400021211     d dscmz         e ds                                                       *trasmissione
001500021211     d KPJBA         e ds                                                       *trasmissione
001600030905     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
001700021211     D WrkSqlCmd       S           1024
001800021211     D WrkSqlA1      E DS                  EXTNAME(FIVAB46F)
001900001018      *--------------------------------------------------------------------*
002000001018      *-                       M A I N    L I N E                         -*
002100001018      *--------------------------------------------------------------------*
002200021211      * Operazioni iniziali
002300001018      *
002400021211     C     *ENTRY        PLIST
002500021211     C                   PARM                    KPJBA
002600021211      *
002700021211      * reperisco ora del sistema
002800021211     C                   TIME                    W0140            14 0
002900021211     C                   MOVEL     W0140         TIME              6 0
003000021211     C                   z-add     *zeros        $nrr              5 0
003100021211      *
003200021211      * ovrprtf su file printer
003300021211     C                   MOVEA     CMD           COMMAN           80
003400030905     C                   Z-ADD     80            LUNG             15 5
003500021211      *
003600021211     C                   CALL      'QCMDEXC'                              99
003700021211     C                   PARM                    COMMAN
003800021211     C                   PARM                    LUNG
003900021211      *
004000021211     C                   OPEN      PRTF198
004100021211     C                   OPEN      Fivab46f
004200021211      *
004300021211      * Prepara stringa Sql
004400001018     C                   EXSR      SetSql
004500021211      *
004600021211      * Lettura file ordinato per linea di arrivo e stampa
004700001018     C                   EXSR      ExeSql
004800021211      *
004900021211    1C                   IF        $Nrr > *zeros
005000021211      * Chiudo/Invio
005100021211     C                   CLOSE     FiVab46f
005200021211     C                   EXSR      Invia
005300021211     C                   ELSE
005400021211      * Stampo spedizioni non trovate
005500021211     c   OG
005600021211     cORN77              DO
005700021211     c                   EXCEPT    TESTA
005800021211     c  N77              SETON                                        77
005900021211     c                   ENDDO
006000021211     c                   EXCEPT    DETTA1
006100021211      * Chiudo
006200021211     c                   CLOSE     FiVab46f
006300021211     c                   ENDIF
006400021211     c                   SETON                                        LR
006500021211      *
006600001018      *--------------------------------------------------------------------*
006700001018      *-                   F I N E    P R O G R A M M A                   -*
006800001018      *--------------------------------------------------------------------*
006900021211      *-SubRoutine: Preparazione stringa SQL per ordinamento file
007000001018     C     SetSql        BEGSR
007100001018      *
007200001018     C                   EVAL      WrkSqlCmd
007300001018     C                             =
007400001018     C                             'SELECT'
007500001018     C                             +
007600021211     C                             ' * '
007700021211     C                             +
007800001027     C                             ' FROM'
007900001027     C                             +
008000021211     C                             ' FIVAB46F'
008100001027     C                             +
008200001018     C                             ' ORDER BY'
008300001018     C                             +
008400021211     C                             ' VABLNA, VABAAS, VABLNP, VABNRS, VABNSP'
008500001018      *
008600001018     C                   ENDSR
008700001025      *--------------------------------------------------------------------*
008800021211      *-SubRoutine: Esecuzione  stringa SQL
008900001018     C     ExeSql        BEGSR
009000001018      *
009100001018     C/EXEC SQL
009200001018     C+ PREPARE S1 FROM :WrkSqlCmd
009300001018     C/END-EXEC
009400001018
009500001018     C/EXEC SQL
009600001018     C+ DECLARE A1 CURSOR FOR S1
009700001018     C/END-EXEC
009800001018
009900001018     C/EXEC SQL
010000001018     C+ OPEN A1
010100001018     C/END-EXEC
010200001018
010300021211     C                   DOU       SqlCod <> 0
010400021211      *          lettura cursore
010500001018
010600001018     C/EXEC SQL
010700021211     C+ FETCH NEXT FROM A1 INTO :WrkSqlA1
010800001018     C/END-EXEC
010900001018
011000001018     C                   SELECT
011100021211      * Non ho letto nulla
011200021211     C                   WHEN      SqlCod = 100
011300021211     C                   WHEN      SqlCod >=0
011400021211     C                   EXSR      Stampo
011500021211     C                   add       1             $Nrr
011600001018     C                   ENDSL
011700001018
011800001018     C                   ENDDO
011900001018      *
012000001018     C/EXEC SQL
012100001018     C+ CLOSE A1
012200001018     C/END-EXEC
012300001018      *
012400001018     C                   ENDSR
012500021211      *
012600021211      * STAMPO DATI DA TRASMETTERE -----------------------------------*
012700021211     C     STAMPO        BEGSR
012800021211      *
012900021211     c   OG
013000021211     cORN77              DO
013100021211     c                   EXCEPT    TESTA
013200021211     c                   EXCEPT    TESTA1
013300021211     c                   EXCEPT    TESTA2
013400021211     c                   EXCEPT    TESTA3
013500021211     c  N77              SETON                                        77
013600021211     c                   ENDDO
013700021211      *
013800021211     c                   movel     vabrsd        Dest             20
013900021211     c                   movel     vabind        Ind              20
014000021211     c                   movel     vabcad        Cap               5
014100021211     c                   movel     vablod        Loc              20
014200021211     c                   EXCEPT    DETTA
014300021211     C                   ENDSR
014400021211      *
014500021211      * TRASMETTO I DATI RICHIESTI------------------------------------*
014600021211     C     INVIA         BEGSR
014700021211      *
014800021211     C                   CLEAR                   Tibs55ds
014900021211     C                   MOVEL     KNSIF         I50PSI
015000021211     C                   MOVEL     046           I50PPO
015100021211     C                   MOVE      001           I50APO
015200021211     C                   MOVEL     'L'           I50TLA
015300021211     C                   CALL      'TIBS55R'
015400021211     C                   PARM                    Tibs55Ds
015500021211      *
015600021211      * imposto se i due p.o sono locali
015700021211     C                   CLEAR                   DsCmz
015800021211     C                   MOVEL     O50LCL        CMZLCL            1
015900021211     C                   MOVE      '001'         CMZDST
016000021211     C                   MOVEL     O50PLG        CMZLBD
016100021211     C                   MOVEL     O50ALG        CMZLBA
016200021211     C                   MOVEL     O50ASI        CMZSYF
016300021211     C                   MOVEL     'FIVAB46F'    CMZFLD           10
016400021211     C                   MOVEL     'FIVAB00F'    CMZFLA           10
016500021211     C                   MOVE      '0000'        CMZORI
016600021211     C                   MOVE      '0000000'     CMZDTI
016700021211     C                   MOVE      'A'           CMZADD
016800021211     C                   MOVE      'S'           CMZCLR
016900021211     C                   MOVE      'S'           CMZFTR
017000021211     C                   MOVE      *BLANK        CMZERR
017100021211     C                   MOVE      'N'           CMZURG
017200021211     C                   MOVEL     'FIVAB46F'    CMZMBD
017300021211     C                   MOVEL     'FIVAB00F'    CMZMBA
017400021211     C                   move      $nrr          CMZNRR
017500021211     C                   CLEAR                   W236
017600021211      *
017700021211     C                   CALL      'TIBS53C'
017800021211     C                   PARM                    CMZLCL
017900021211     C                   PARM                    CMZDST
018000021211     C                   PARM                    CMZFLD
018100021211     C                   PARM                    CMZMBD
018200021211     C                   PARM                    CMZLBD
018300021211     C                   PARM                    CMZFLA
018400021211     C                   PARM                    CMZMBA
018500021211     C                   PARM                    CMZLBA
018600021211     C                   PARM                    CMZORI
018700021211     C                   PARM                    CMZDTI
018800021211     C                   PARM                    CMZURG
018900021211     C                   PARM                    CMZADD
019000021211     C                   PARM                    CMZCLR
019100021211     C                   PARM                    CMZFTR
019200021211     C                   PARM                    CMZERR
019300021211     C                   PARM                    CMZAZN
019400021211     C                   PARM                    CMZSYF
019500021211     C                   PARM                    CMZNRR
019600021211     C                   PARM                    CMZPRR
019700021211     C                   PARM                    W236            236
019800021211     C                   ENDSR
019900021211      ********************************************************************
020000021211     oPRTF198   E            TESTA          2 02
020100021211     o                       UDATE             +  2 '  /  /  '
020200021211     o                       TIME              +  2 '  :  :  '
020300021211     o                                         + 35 'Elenco spedizioni inviate '
020400021211     o                                         +  0 'da SEDE a p.o BOLOGNA Fil'
020500021211     o                                         +  0 'iale'
020600021211     o                                         + 10 'TNSD53R'
020700021211     o                                         +  5 'PAG.'
020800021211     o                       PAGE          Z
020900021211     o          E            TESTA1         2
021000021211     o                                         +  0 '  MM. Nr   Num.    Cd Fil '
021100021211     o                                         +  0 'Destinatario         Indir'
021200021211     o                                         +  0 'izzo            C A P Loca'
021300021211     o                                         +  0 'lità            Prv E Coll'
021400021211     o                                         +  0 'li       Peso             '
021500021211     o                                         +  0 '   Riferim. Segnac. Segnac'
021600021211     o                                         +  0 '. Zo F Descrizione'
021700021211     o          E            TESTA2         1
021800021211     o                                         +  0 '  GG. Sr   Sped.   Bl Arr '
021900021211     o                                         +  0 '                          '
022000021211     o                                         +  0 '                          '
022100021211     o                                         +  0 '                    s     '
022200021211     o                                         +  0 '         KG               '
022300021211     o                                         +  0 '   Mittente DAL     AL    '
022400021211     o                                         +  0 '  na D  N O T E'
022500021211     o          E            TESTA3         1
022600021211     o                                         +  0 '                   Pa     '
022700021211     o                                         +  0 '                          '
022800021211     o                                         +  0 '                          '
022900021211     o                                         +  0 '                    p     '
023000021211     o                                         + 14 '               Numerico'
023100021211     o          E            DETTA          2
023200021211     o                       VABMGS            +  0 '  /  '
023300021211     o                       VABNRS            +  1
023400021211     o                       VABNSP        2   +  1
023500021211     o                       VABCBO            +  1
023600021211     o                       VABLNA            +  1
023700021211     o                       DEST              +  1
023800021211     o                       IND               +  1
023900021211     o                       CAP               +  1
024000021211     o                       LOC               +  1
024100021211     o                       VABPRD            +  1
024200021211     o                       VABTSP            +  1
024300021211     o                       VABNCL        2   +  1
024400021211     o                       VABPKB        2   +  1
024500021211     o                       VABRMN        2   +  1
024600021211     o                       VABNCD        2   +  2
024700021211     o                       VABNCA        2   +  1
024800021211     o                       VABZNC        2   +  1
024900021211     o                       VABFFD            +  1
025000021211     o                       VABNOT            +  1
025100021211     o          E            DETTA1           06
025200021211     o                                         + 65 '--- NON TROVATE SPE'
025300021211     o                                         +  0 'DIZIONI ---'
025400021211** Comandi    567890123456789012345678901234567890123456789012345678901234567890
025500030905OVRPRTF FILE(PRTF198)  USRDTA('Bolle 001') HOLD(*YES) SAVE(*YES)
