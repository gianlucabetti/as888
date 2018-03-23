000100881205     H DECEDIT(',') DFTNAME(CN8100) DATEDIT(*DMY/)
000200881205     FCNC810D   CF   E             WORKSTN
000300930901     F                                     INFDS(INFO)
000400881205     FTABEL00F  UF A E           K DISK
000500900419     D ERRO            S             40    DIM(2) CTDATA PERRCD(1)
000600980130     D TRUL12        E DS                  EXTNAME(TRUL12DS)
000700940927     D DSFA          E DS
000800940927     D DSRAT           DS
000900940927     D  RT1                    1      3  0
001000940927     D  RT2                    4      6  0
001100940927     D  RT3                    7      9  0
001200940927     D  RT4                   10     12  0
001300940927     D  RT5                   13     15  0
001400940927     D  RT6                   16     18  0
001500940927     D  RT7                   19     21  0
001600940927     D  RT8                   22     24  0
001700940927     D  RT9                   25     27  0
001800940927     D  RT10                  28     30  0
001900940927     D  RAT                    1     30  0
002000900419     D                                     DIM(10)
002100881205     D KPJBA         E DS
002200881205     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002300881205     D*------- CHIAVE TABELLA
002400881205     D CHIAVE          DS
002500881205     D  COD                    1      3
002600881205     D  CODX                   3      3
002700881205     D  LIN                    4      4  0
002800881205     D  FIL                    5      8
002900881205     D*---DS CHIAV1 PER STAMPA
003000881205     D CHIAV1          DS
003100881205     D  COD1                   1      3
003200881205     D  LIN1                   4      4  0
003300881205     D  FIL1                   5      8
003400881205     D*
003500881205     D*---DS LINGUA X STAMPA
003600881205     D                 DS
003700881205     D  LINGUA                 4      4  0
003800881205     D  TBLKEY                 1      8
003900930901     D*
004000930901     D*---DS PER DECODIFICA TAB 18 "MODALITA' DI PAGAMENTO"
004100930901     D                 DS
004200930901     D  BLANK                  7      7
004300930901     D  $$$KEY                 1      8
004400930901     D INFO            DS
004500930901     D  NRG                  370    370
004600930901     D  NCL                  371    371
004700930901     D  PRIMA                378    379B 0
004800930901     D CURSOR          DS
004900930901     D  RIRI                   1      2B 0
005000930901     D  R$$                    2      2
005100930901     D  COCO                   3      4B 0
005200930901     D  C$$                    4      4
005300881205     C/EJECT
005400881205     C*--------------------------------------------------------------*
005500881205     C     *ENTRY        PLIST
005600881205     C                   PARM                    KPJBA
005700980130     C*----------------------------------------------------*
005800980130     C* Richiamo pgm per controllo modificabilità tabella
005900980130     C                   CLEAR                   TRUL12
006000980130     C                   MOVEL     'L'           I12TLA
006100980130     C                   MOVEL     KNSIF         I12NSI
006200980130     C                   MOVEL     'FA'          I12COD
006300980130     C                   CALL      'TRUL12R'
006400980130     C                   PARM                    TRUL12
006500980130     C     O12ERR        IFEQ      '1'
006600980130     C                   GOTO      FINE
006700980130     C                   END
006800980130     C*----------------------------------------------------*
006900881205     C     KEYTAB        KLIST
007000881205     C                   KFLD                    PARKUT
007100881205     C                   KFLD                    TIPTAB
007200881205     C                   KFLD                    KEYTB
007300930901     C     *LIKE         DEFINE    TBLCOD        $$$COD
007400930901     C     KTBL          KLIST
007500930901     C                   KFLD                    PARKUT
007600930901     C                   KFLD                    $$$COD
007700930901     C                   KFLD                    $$$KEY
007800881205     C     KEYPAR        KLIST
007900881205     C                   KFLD                    PARKUT
008000881205     C                   KFLD                    TIPTAB
008100881205     C*------- INIZIO -------------------------------------*
008200890306     C                   Z-ADD     1             CODUT
008300881205     C                   CALL      'XPARUT'
008400881205     C                   PARM                    PARUT
008500881205     C     CODUT         CABEQ     -1            FINE
008600881205     C                   Z-ADD     CODUT         PARKUT            1 0
008700881205     C                   Z-ADD     CODUT         TBLKUT
008800881205     C                   MOVEL     RAGUT         RSUT
008900930901     C                   MOVEL     KNSIF         VSYS             20            *
009000881205     C                   MOVE      ERRO(1)       ERR              40
009100881205     C*------
009200881205     C                   MOVE      'FA'          TIPTAB            2
009300881205     C                   MOVE      'FA'          TBLCOD
009400890620     C                   MOVEL     *BLANKS       COD
009500890620     C                   MOVEL     '?'           COD
009600890620     C                   Z-ADD     1             LIN
009700890620     C* EMISSIONE FORMATO RICHIESTA CODICE ----------------*
009800890620     C                   WRITE     C810D1
009900881205     C     AAA0          TAG
010000881205     C                   EXFMT     C810D2
010100890620     C*----------------------------------------------------*
010200930901     C   KC              GOTO      FINE
010300930901     C   KH              DO
010400900711     C*STAMPA
010500900419     C                   MOVEL     CHIAVE        KPJBU
010600900419     C                   CALL      'CNC81BR'
010700900419     C                   PARM                    KPJBA
010800900419     C                   GOTO      FINE
010900900419     C                   END
011000890620     C* INTERROGAZIONE CODICI -----------------------------*
011100890620     C     COD           IFEQ      '?'
011200881205     C                   MOVE      *BLANKS       COD
011300890620     C                   CALL      'XCDPAGR'
011400881205     C                   PARM                    PAGCOD            3
011500881205     C                   PARM                    PAGDES           30
011600881205     C                   MOVEL     PAGCOD        COD
011700940927     C                   MOVEL     PAGDES        DES
011800890620     C                   GOTO      AAA0
011900881205     C                   END
012000890620     C*----------------------------------------------------*
012100881205     C                   TESTN                   COD                  06
012200881205     C  N06              SETON                                        18
012300881205     C     CODX          COMP      '0'                                  02
012400881205     C   02              SETON                                        18
012500881205     C  N06
012600881205     COR 02              GOTO      AAA0
012700930901-NT- C*------- CONTROLLO CODICE A 0 O 999
012800930901-NT- C     COD           CABEQ     '999'         AAA0                     19
012900881205     C*--------
013000881205     C                   MOVEA     *ZEROS        *IN(13)
013100881205     C                   SETOFF                                       07
013200881205     C/SPACE 2
013300881205     C*------- CONTROLLO ESISTENZA
013400881205     C     AAA30         TAG
013500930901     C                   MOVEL     *BLANKS       $$$FLG
013600881205     C                   MOVE      CHIAVE        KEYTB             8
013700881205     C     KEYTAB        CHAIN     TABEL                              01
013800940927     C     *IN01         IFEQ      '0'
013900940927     C                   MOVEL     TBLUNI        DSFA
014000940927     C                   SETON                                        51
014100940927     C                   MOVEL     §FADES        DES
014200940927     C                   MOVEL     §FATPG        TIPO
014300940927     C                   MOVEL     §FATTI        IVA
014400940927     C                   MOVE      §FAGGI        GG1
014500940927     C                   MOVE      §FAGGF        GG2
014600940927     C                   MOVEL     §FARAT        DSRAT
014700940927     C                   MOVE      §FASCO        SCONTO
014800940927     C                   MOVE      §FANTR        NRAT
014900971017     C****                 MOVEL§FAFDF    VFDF
015000940927     C     TBLFTT        IFEQ      *BLANK
015100940927     C                   MOVEL     'N'           ABITR
015200940927     C                   ELSE
015300940927     C                   MOVEL     ' '           ABITR
015400940927     C                   ENDIF
015500881205     C*------- IN VARIAZIONE DECODIFICA EFFETTI
015600940927     C     §FATRA        IFEQ      '1'
015700881205     C                   MOVE      'SI'          EFF
015800881205     C                   ELSE
015900881205     C                   MOVE      'NO'          EFF
016000881205     C                   END
016100881205     C*------- IN VARIAZIONE TESTA SE RECORD ANNULLATO
016200940927     C     TBLFLG        IFEQ      '*'
016300881205     C                   SETON                                        07
016400930901     C                   MOVE      '*'           $$$FLG
016500881205     C                   END
016600930901     C*------- IN VARIAZIONE DECODIFICO SE ESISTE LA
016700930901     C*------- MODALITA' DI PAGAMENTO
016800940927     C     §FAMDP        IFNE      *BLANKS
016900930901     C                   MOVE      '18'          $$$COD
017000930901     C                   MOVE      *ZEROS        $$$KEY
017100940927     C                   MOVE      *BLANKS       DUE               2
017200940927     C                   MOVE      §FAMDP        VIDMDP
017300940927     C     §FATPG        IFEQ      '1'                                           RB CL.
017400940927     C     §FATPG        OREQ      '2'                                           RID
017500971009     C***        §FATPG    OREQ '9'                         RB FORN
017600940927     C                   MOVEL     §FAMDP        DUE
017700940927     C                   ELSE
017800940927     C                   MOVE      §FAMDP        DUE
017900940927     C                   ENDIF
018000940927     C                   MOVE      DUE           $$$KEY
018100930901     C     KTBL          CHAIN     TABEL                              91
018200930901     C  N91              MOVEL     TBLUNI        DECMDP
018300930901     C   91              MOVEL     *BLANKS       DECMDP
018400930901     C                   END
018500881205     C*------- INSERIMENTO PULISCI CAMPI E PREPARA CAMPO CHIAVE
018600940927     C                   ELSE
018700940927     C                   CLEAR                   DSFA
018800940927     C                   MOVE      *ZEROS        RAT
018900881205     C                   MOVE      *BLANK        DES
019000881205     C                   MOVE      *BLANK        EFF
019100881205     C                   MOVE      CHIAVE        TBLKEY
019200930901     C                   MOVE      ' '           $$$FLG
019300930901     C                   MOVE      *BLANKS       VIDMDP
019400930901     C                   MOVE      *BLANKS       DECMDP
019500940927     C                   MOVEL     *BLANK        TIPO
019600940927     C                   MOVEL     *BLANK        IVA
019700940927     C                   MOVE      *ZEROS        GG1
019800940927     C                   MOVE      *ZEROS        GG2
019900940927     C                   MOVE      *ZEROS        SCONTO
020000940927     C                   MOVE      *ZEROS        NRAT
020100940927     C                   MOVEL     *BLANK        VFDF
020200940927     C                   MOVEL     'N'           ABITR
020300881205     C                   MOVE      ' '           TBLFLG
020400940927     C                   SETON                                        50
020500940927     C                   ENDIF
020600881205     C/EJECT
020700881205     C*------- LEGGI VIDEO CON CAMPI - ESEGUI CONTROLLI ---*
020800940927     C     BBB0          TAG
020900881205     C                   WRITE     C810D4
021000881205     C                   EXFMT     C810D3
021100930901     C   KL              MOVEL     *BLANKS       COD
021200930901     C   KL              MOVEL     '?'           COD
021300930901     C   KL              GOTO      AAA0                                         * RITORNO
021400930901     C   KC              GOTO      AAA0                                         * RITORNO
021500930901     C   KQ              MOVE      '*'           $$$FLG            1            * ANNULLA
021600930901     C   KE              MOVE      ' '           TBLFLG                         * RIPRISTI
021700930901     C                   MOVEA     *ZEROS        *IN(13)
021800930901     C*----------------------------------------------------*
021900930901     C*RICERCA DELLA MODALITA' DI PAGAMNETO                *
022000940927     C********   TIPO      IFEQ 0
022100930901     C*
022200930901     C     VIDMDP        IFEQ      '?'
022300930901     C                   MOVE      NRG           R$$
022400930901     C                   MOVE      NCL           C$$
022500930901     C                   Z-ADD     RIRI          CURR
022600930901     C                   Z-ADD     COCO          CURC
022700930901     C                   CALL      'CNC018R1'                                   *
022800930901     C                   PARM                    D2TIP             1
022900930901     C                   PARM                    D2COD             1            *
023000930901     C                   PARM                    RSUT                           *
023100930901     C                   PARM                    VSYS                           *
023200930901     C                   PARM                    CURR              2 0          *
023300930901     C                   PARM                    CURC              2 0          *
023400930901     C                   MOVE      D2COD         VIDMDP
023500930901     C                   MOVEL     '1'           *INKD
023600930901     C                   END
023700930901     C*
023800930901     C*CONTROLLO MODALITA' DI PAGAMENTO
023900930901     C     VIDMDP        IFNE      *BLANKS
024000930901     C                   MOVE      '18'          $$$COD
024100930901     C                   MOVE      *ZEROS        $$$KEY
024200940927     C                   MOVE      *BLANKS       DUE
024300940927     C     TIPO          IFEQ      1                                             RB CL.
024400940927     C     TIPO          OREQ      2                                             RID
024500971009     C***        TIPO      OREQ 9                           RB FORN
024600940927     C                   MOVEL     VIDMDP        DUE
024700940927     C                   ELSE
024800940927     C                   MOVE      VIDMDP        DUE
024900940927     C                   ENDIF
025000940927     C                   MOVE      DUE           $$$KEY
025100930901     C     KTBL          CHAIN     TABEL                              91
025200930901     C  N91              MOVEL     TBLUNI        DECMDP
025300930901     C   91              MOVEL     *BLANKS       DECMDP
025400930901     C   91              SETON                                        4115
025500930901     C                   ELSE
025600930901     C                   MOVEL     *BLANKS       DECMDP
025700930901     C                   END
025800930901     C*
025900940927     C******               ELSE
026000940927     C******               MOVEL*BLANKS   VIDMDP
026100940927     C******               MOVEL*BLANKS   DECMDP
026200940927     C******               END
026300930901     C*----------------------------------------------------*
026400881205     C     EFF           CABEQ     'SI'          BBB30
026500881205     C*------- EFF=NO AZZERA TUTTO
026600940927     C                   MOVE      '0'           EFFET             1
026700881205     C                   DO        10            X                 2 0
026800881205     C                   MOVE      *ZEROS        RAT(X)
026900881205     C                   END
027000881205     C                   MOVE      *ZEROS        NRAT
027100881205     C                   MOVE      0             IVA
027200881205     C                   MOVE      *ZEROS        GG1
027300881205     C                   MOVE      *ZEROS        GG2
027400881205     C                   MOVE      *ZEROS        SCONTO
027500881205     C                   GOTO      BBB70
027600881205     C*------- CONTROLLO CONDIZIONI IVA = 0/1/2
027700881205     C     BBB35         TAG
027800881205     C     IVA           IFGT      2
027900881205     C                   SETON                                        2615
028000881205     C                   END
028100881205     C*------- EFF=SI CONTROLLO VARI CAMPI
028200881205     C     BBB30         TAG
028300881205     C                   MOVE      1             EFFET
028400881205     C*------- CONTROLLA GG  =99 FINE MESE
028500881205     C*-------               >31 ERRORE
028600881205     C*-------               GG2 =/ 0 SOLO SE GG1 =/ 0
028700881205     C     GG2           IFNE      0
028800940322     C     GG2           ANDNE     90
028900881205     C     GG1           IFEQ      0
029000881205     C                   SETON                                        2415
029100881205     C                   GOTO      BBB40
029200881205     C                   END
029300881205     C                   END
029400881205     C     GG1           IFNE      99
029500881205     C     GG1           IFGT      31
029600881205     C                   SETON                                        2215
029700881205     C                   END
029800881205     C                   END
029900881205     C     GG2           IFNE      99
030000881205 %%  C     GG2           ANDNE     95
030100940322 %%  C     GG2           ANDNE     90
030200881205     C     GG2           IFGT      31
030300881205     C                   SETON                                        2315
030400881205     C                   END
030500881205     C                   END
030600881205     C*------- CONTROLLO RATE  IVA=/ 2 ALMENO 1 RATA
030700881205     C*-------                 IVA=  2 ALMENO 2 RATE (1 X IVA)
030800881205     C     BBB40         TAG
030900881205     C     RT1           IFEQ      0
031000881205     C                   SETON                                        2015
031100881205     C                   END
031200881205     C     IVA           IFEQ      2
031300881205     C     RT2           IFEQ      0
031400881205     C                   SETON                                        2115
031500881205     C                   END
031600881205     C                   END
031700881205     C*------                  CHE NON ESISTANO BUCHI
031800881205     C                   DO        10            X
031900881205     C     RAT(X)        COMP      0                                      33
032000881205     C  N33              END
032100881205     C     X             DO        10            Y                 2 0          * X PUNTA
032200881205     C     RAT(Y)        IFNE      0
032300881205     C                   SETON                                        2715
032400881205     C                   END
032500881205     C                   END
032600881205     C*------- FORZA NUMERO RATE
032700881205     C     BBB50         TAG
032800881205     C     X             SUB       1             NRAT
032900881205     C*------- FINE CONTROLLI TESTA IN15 SE ON ERRORE
033000881205     C     BBB60         TAG
033100881205     C   15              GOTO      BBB0
033200881205     C/SPACE 5
033300881205     C*------- AGGIORNA RECORD
033400881205     C     BBB70         TAG
033500930901     C*RICHIESTA DECODIFICA
033600930901     C   KD              GOTO      BBB0
033700930901     C*
033800930901     C                   MOVE      CHIAVE        KEYTB             8
033900930901     C     KEYTAB        CHAIN     TABEL                              01
034000930901     C                   MOVEL     $$$FLG        TBLFLG
034100940927     C                   MOVEL     DES           §FADES
034200940927     C                   MOVEL     TIPO          §FATPG
034300940927     C                   MOVEL     EFFET         §FATRA
034400940927     C                   MOVEL     IVA           §FATTI
034500940927     C                   MOVE      GG1           §FAGGI
034600940927     C                   MOVE      GG2           §FAGGF
034700940927     C                   MOVEL     DSRAT         §FARAT
034800940927     C                   MOVE      SCONTO        §FASCO
034900940927     C                   MOVE      NRAT          §FANTR
035000971017     C***                  MOVELVFDF      §FAFDF
035100940927     C                   MOVEL     VIDMDP        §FAMDP
035200940927     C                   MOVEL     DSFA          TBLUNI
035300930901     C                   MOVE      CHIAVE        TBLKEY
035400930901     C                   MOVE      TIPTAB        TBLCOD
035500900419     C*§
035600900419     C     ABITR         IFEQ      *BLANKS
035700900419     C                   MOVE      '1'           TBLFTT                          A TUTTI
035800900419     C                   ELSE
035900900420     C                   MOVE      *BLANK        TBLFTT                          A NESSUNO
036000900419     C                   END
036100900419     C                   MOVE      *ZERO         TBLFLT                          FILIALE
036200900419     C                   MOVE      *BLANK        TBLFTR                          TIPO TRAS
036300900419     C                   MOVE      *ZERO         TBLDTR                          N.TRASM.
036400900419     C*§
036500881205     C   01              WRITE     TABEL
036600881205     C  N01              UPDATE    TABEL
036700881205     C                   GOTO      AAA0
036800900419     C*------- CHIUDI PROGRAMMA
036900900419     C     FINE          TAG
037000900419     C                   SETON                                        LR
037100881205     C/EJECT
037200881205**        ERRO
037300881205ESISTONO DELLE SCADENZE A 0 INTERNE
037400881205NESSUN MOVIMENTO !!!
