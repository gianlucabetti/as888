000100990519     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990519     H*PARMS ACTGRP(QILE)
000300990125     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400870410     F* OCO100R  *---------------------------------------------------*
000500870408     F*----------*                                                   *
000600870410     F*          - IMMISSIONE/REVISIONE ORDINI CLIENTI   -           *
000700870408     F*          -----------------------------------------           *
000800870408     F*                                                    AZ.: N100 *
000900870408     F*                                                              *
001000990125     F*         FILE : OCO100D  - tmmoc00F - tmmoc01L - tmclp01L     *
001100870408     F*                CNACO00F - CNIND00F - ANTAB00F - TABEL00F     *
001200990212     F*                tmlmp01L - tmamp01L - tmsmp01L -              *
001300870408     F*                                                              *
001400870513     F*         PGM. : OCO800R - LANCIO STAMPA ORDINI                *
001500990317     F*                tii007R - INTERROG.PARTICOLARITA'FORNITORI    *
001600870513     F*                OCO8A0R - INTERROG.LISTINI MATERIE PRIME      *
001700990317     F*                TII119R - RICERCA ALFABETICA CLIENTI          *
001800990317     F*                tii104R - RICERCA ALFABETICA MATERIE PRIME    *
001900990317     F*                tii105R - RICERCA PAGAMENTI                   *
002000990317     F*                tii108R - RICERCA MAGAZZINI                   *
002100990317     F*                TII114R - RICERCA GRUPPI MERCEOLOGICI         *
002200990317     F*                TII115R - RICERCA SETTORI MERCEOLOGICI        *
002300990317     F*                TII116R - RICERCA LISTINI MATERIE PRIME       *
002400990317     F*                TII117R - RICERCA TIPOLOGIE ORDINI            *
002500990317     F*                TII202R - RICERCA DISPONIBILITA' M.P.         *
002600870408     F*                RICDATE - INVERSIONE E CONTROLLO DATE         *
002700870408     F*                RIC001C - CALCOLA DATA SOMMANDO XXX GIORNI    *
002800870408     F*                RIC002C - RICHIAMA RISORSE                    *
002900870408     F*                RIC001R - RICERCA DESCRIZIONE DITTA           *
003000870408     F*                                                              *
003100870408     F*--------------------------------------------------------------*
003200870408     F*--------------------------------------------------------------*
003300870408     F* TIPO RIGA :                                                  *
003400870408     F*------------                                                  *
003500870408     F*              A - R I G A    A N N U L L A T A                *
003600870408     F*              1 - RIGA ACQUISTO                               *
003700870408     F*              2 - RIGA CONTO LAVORAZIONE                      *
003800870408     F*              3 - RIGA PRODUZIONE                             *
003900870408     F*------------                                                  *
004000870408     F*              D - RIGA DI ADDEBITO  AL FORNITORE              *
004100870408     F*              C - RIGA DI ACCREDITO AL FORNITORE              *
004200870408     F*              I - RIGA DI I.V.A.                              *
004300870408     F*              S - RIGA DI SPESE                               *
004400870408     F*------------                                                  *
004500870408     F*              6 - RIGA COMMENTO PER FORNITORE                 *
004600870408     F*              7 - RIGA COMMENTO PER USO INTERNO               *
004700870408     F*              8 - RIGA COMMENTO PER MAGAZZINO                 *
004800870408     F*              9 - RIGA COMMENTO GENERICO                      *
004900870408     F*--------------------------------------------------------------*
005000870408     F* FASE RIGA :                                                  *
005100870408     F*------------                                                  *
005200870408     F*             OR - IN ORDINE          (T.R.= 1-2-6-7-8-9)      *
005300870408     F*             IM - IMPEGNATA SU PROD. (T.R.= 1-2)              *
005400870408     F*             CL - IN CONTO/LAVORAZ.  (T.R.= 1-2)              *
005500870408     F*             BB - BOLLETTATA         (T.R.= TUTTE)            *
005600870408     F*             FA - FATTURATA          (T.R.= TUTTE)            *
005700870408     F*--------------------------------------------------------------*
005800990317     FTII300D   CF   E             WORKSTN
005900870408     F                                     SFILE(SFL1:REC1)
006000870408     F                                     SFILE(SFL2:REC2)
006100990125     Ftmmoc00F  UF A E             DISK
006200990125     F                                     RENAME(tmmoc000:ORDFIS)
006300870408     F                                     INFDS(INF2)
006400990125     Ftmmoc01L  UF   E           K DISK
006500990125     F                                     RENAME(tmmoc000:ORDRIG)
006600870408     F                                     INFDS(INF1)
006700990125     Ftmclp01L  IF   E           K DISK
006800870408     FANTAB00F  IF   E           K DISK
006900990125     Ftmlmp01L  IF   E           K DISK
007000990125     Ftmamp01L  IF   E           K DISK
007100990125     Ftmsmp01L  UF A E           K DISK
007200990125     Ftmscm01L  IF   E           K DISK
007300990125RIC  Fancln01l  IF   E           K DISK
007400990125RIC  Fansog01l  IF   E           K DISK
007500990125RIC  Fanind01l  IF   E           K DISK
007600990125RIC  Fanent01l  IF   E           K DISK
007700870408RIC  FTABEL00F  IF   E           K DISK
007800010725     FTNTBE01l  IF   E           k disk
007900970801     FFNSPE01L  IF   E           K DISK
008000970801     F                                     RENAME(FNSPE000:ANSPE1)
008100871219     F*--------- LUOGHI DI DESTINAZIONE -----------------------------*
008200970801     FFNSPE03L  IF   E           K DISK
008300970801     F                                     RENAME(FNSPE000:ANSPE3)
008400870408     D ORD             S             23    DIM(3) CTDATA PERRCD(1)              * SEGNALAZIONI
008500890707     D ERR             S             40    DIM(30) CTDATA PERRCD(1)             * ERRORI
008600870409     D/EJECT
008700970819     D DSC0M6          DS
008800970819     D* Cod.luogo
008900970819     D  DM6SPE                 1      3
009000970819     D* Codice cliente
009100970819     D  DM6COD                 4     10  0
009200970819     D* ric = 'S' -> pgm richiamato
009300970819     D  DM6RIC                11     11
009400871105     D MPHDT1          DS           512
009500871105     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
009600871105     D  NUMDTA                 1      5  0
009700871105     D  FILL1                  6      8
009800871105     D  DATDTA                 9     14  0
009900871105     D  FILL2                 15     17
010000871105     D  ANNDTA                18     19  0
010100871105     D  FIL03                 20     50
010200871105     D  DGIO                  51     56  0
010300871105     D  FIL04                 57    100
010400871105     D  FILL5                101    300
010500871105     D  GIOTOL               301    302  0
010600871105     D  FILL6                303    512
010700870408     D DTAORC          DS
010800870408     D*--- DATA AREA NUMERAZIONE ORDINI :
010900870408     D  ORDNUO                 1      5  0
011000870408     D OCLDT1          DS
011100870408     D*--- DATA AREA ORDINI CLIENTI     :
011200870408     D  F1KKKK                 1     50
011300870408     D  MASTE1                51     51
011400870408     D  MASTE2                52     52
011500870408     D  MASTE3                53     53
011600870408     D  MASTER                51     53
011700870408     D  F2KKKK                54    256
011800870408     D  F3KKKK               257    512
011900870408     D                 DS
012000870408     D*--- SCOMPOSIZIONE MASTER PROCEDURA :
012100870408     D  EDP1                   1      1
012200870408     D  EDP2                   2      2
012300870408     D  EDP3                   3      3
012400940121     D  EDP                    1      6
012500870408     D INF1            DS           512
012600870408     D*--- RELREC LOGICO
012700870408     D  NREL1                397    400B 0
012800870408     D INF2            DS           512
012900870408     D*--- RELREC FISICO
013000870408     D  NREL2                397    400B 0
013100870408RIC  D****************************************************************
013200870408RIC  D PAGAM           DS
013300870408RIC  D*------------- KEY PAGAMENTI PER TABEL00F X PAGAMENTI :
013400870408RIC  D  PAGVID                 1      3
013500870408RIC  D  LINGUA                 4      4
013600870408RIC  D  BLANCO                 5      8
013700870408RIC  D  KEYTBL                 1      8
013800870408RIC  D****************************************************************
013900870921     D PARBOL          DS
014000990125     D  BOLCLI                 1      8
014100990125     D  BOLNOR                 9     13  0
014200990128     D  BOLDEM                14     21  0
014300990128     D  BOLDOR                22     29  0
014400990128     D  socpas                30     32
014500910925     D TB155           DS
014600910925     D*------------- TABELLA CATEGORIA MERCEOLOGICA:
014700910925     D  CAMPO                  1     74
014800910925     D  FLGATT                75     75
014900870408ADB  D****************************************************************
015000870408     D PAR820          DS
015100870408     D*------------- PARAMETRI PER INTERROGAZIONE FORNITORE :
015200990125     D  FOR820                 1      8
015300990125     D  RAG820                 9     38
015400990125     D  FIL820                39    255
015500870408     D  RIC820               256    256
015600870408     D DESAPP        E DS                  EXTNAME(ANTAB157)
015700870408     D*------------- TABELLA CLASSI APPROVVIGIONAMENTO :
015800870408     D DESDEM        E DS                  EXTNAME(ANTAB028)
015900870408     D*------------- TABELLA DESTINAZIONE MERCE :
016000870408     D DSORD         E DS                  EXTNAME(ANTAB162)
016100870408     D*------------- TABELLA TIPOLOGIE ORDINI :
016200870408     D DSMAG         E DS                  EXTNAME(ANTAB153)
016300870408     D*------------- TABELLA MAGAZZINI :
016400870408     D PAR800          DS
016500870408     D*------------- PARAMETRI PER STAMPA ORDINE :
016600990125     D  FOR800                 1      8
016700990125     D  RAG800                 9     38
016800990125     D  ORD800                39     43  0
016900990125     D  RIG800                44     44
017000990125     D  STA800                45     46
017100870408     D PAR8A0          DS
017200870408     D*------------- PARAMETRI PER INTERROGAZIONE LISTINI
017300870408     D  LIS8A0                 1      3
017400870408     D  CLA8A0                 4      6
017500870408     D  SOT8A0                 7     11
017600870408     D  CAT8A0                12     13
017700870408     D  ART8A0                14     20
017800870408     D  FIL8A0                21    255
017900870408     D  RIC8A0               256    256
018000870408     D PH115           DS
018100870408     D*------------- PARAMETRI PER RICERCA DISPONIBILITA' :
018200990125     D  Anno                   1      4
018300990125     D  Aaa115                 1      4  0
018400990125     D  MAG115                 5     11
018500990125     D  MAT115                12     23
018600990125     D  UMI115                24     24
018700990125     D  SCE115                25     25
018800870408     D KPJBA         E DS
018900870408     D*------------- PASSAGGIO PARAMETRI :
019000990125     D  FOR150               247    254
019100990126     D  FIL150               255    393
019200990126     D  nOR150               394    398  0
019300990126     D  dOR150               399    406  0
019400870408     D  RIC150               442    442
019500870408     D  UTE150               443    452
019600870408     D  MSGERR               453    502
019700890707     D STMDS           DS
019800890707     D  STMFLG                41     41
019900990125     D soc001        E DS                  EXTNAME(xsoc001ds)
020000990125     D xsocds          DS          1000
020100010725    >D DGED          E DS
020200990125     d dataiso         s               d   datfmt(*iso)
020300990125     d dataeur         s               d   datfmt(*eur)
020400990310     D* Ricerca divise
020500990310     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
020600990519     D*-------------
020700990519     D Esito           S              1
020800990519     D Datigest        S              1A
020900990519     D NoctrlScos      S              1A
021000990519     D Gesmsg          S              1
021100990519     D*-------------
021200990519     D*  Prototipazione delle procedure
021300990519     D/COPY *LIBL/SRCCPY,PJX002PR
021400870408     C*----------------------------------------------------*
021500870408     C     *ENTRY        PLIST                                                  *
021600870408     C                   PARM                    KPJBA                          *
021700990125     C*---------- RICERCA DITTA :
021800990125     C                   MOVEL     'SOC001'      TIPXSC
021900990125     C                   MOVEL     *blank        SOCXSC
022000990125     C                   EXSR      REPSOC
022100990125     C     RTNXSC        IFNE      '1'
022200990125     C                   MOVEL     XSOCDS        SOC001
022300990125     C                   MOVEL     xscrgs        desdit
022400990125     C                   MOVEL     xscsoc        wsoc              3
022500990128     C                   MOVEL     xscsoc        socpas
022600990310     C                   MOVEL     xscdiv        tesval
022700990125     c                   end
022800990125     c                   time                    como14           14 0
022900990125     c                   move      como14        woggi             8 0
023000990125     c                   move      woggi         dataeur
023100990125     c                   move      dataeur       dataiso
023200990125     c                   move      dataiso       woggig            8 0
023300010725     C*--- Reperisco la divisa corrente ------------------------------*
023400010725     C     ktbe          KLIST
023500010725     C                   KFLD                    kcod
023600010725     C                   KFLD                    kke1
023700010725     C     *like         Define    tbecod        kcod
023800010725     C     *like         Define    tbeke1        kke1
023900010725     C                   move      'GED'         kcod
024000010725     C                   move      *blanks       kke1
024100010725     C                   movel     'MAGAZZINO'   kke1
024200010725     c                   clear                   dged
024300010725     C     ktbe          CHAIN     tntbe01l                           54
024400010725     C  n54              movel     tbeuni        dged
024500010725     C  n54              movel     §GEDCN        xscdiv
024600010725     C  n54              movel     §GEDCN        tesval
024700870410     C*                                                    *
024800870410     C     *DTAARA       DEFINE                  DTAORC                         *
024900870410     C     *DTAARA       DEFINE                  OCLDT1                         *
025000871105     C     *DTAARA       DEFINE                  MPHDT1                         *
025100990125RIC  C     KEYFOR        KLIST                                                  *
025200990125RIC  C                   KFLD                    wsoc                           *
025300990125RIC  C                   KFLD                    clpksc                         *
025400990125     C*--- FILE ORDINI CLIENTI "tmmoc01L"                  *
025500890707     C                   MOVEL     *BLANKS       STMDS
025600870408     C     KEYORD        KLIST                                                  *
025700870408     C                   KFLD                    CODFOR                         *
025800870408     C                   KFLD                    NUMORD                         *
025900870408RIC  C*--- KEY RICERCA C.D.C. E AGENTE SU "CNACO00F"       *
026000870408RIC  C     KEYCDC        KLIST                                                  *
026100990125RIC  C                   KFLD                    wsoc                           *
026200990125RIC  C                   KFLD                    tipocdc           3            *
026300990125RIC  C                   KFLD                    CCOSTO            8            *
026400990125RIC  C                   move      'CDC'         tipocdc                        *
026500870408RIC  C*--- FILE ANAGRAFICO TABELLE "TABEL00F" X COD. PAGAM.*
026600870408RIC  C     KEYPAG        KLIST                                                  *
026700990125RIC  C                   KFLD                    CODUTE            1 0          *
026800870408RIC  C                   KFLD                    TIPTAB            2            *
026900870408RIC  C                   KFLD                    KEYTBL                         *
027000990125     c                   z-add     1             codute
027100870408RIC  C*--- FILE ANAGRAFICO TABELLE "TABEL00F" X CAMBIO VALUTA
027200870408RIC  C     KEYVAL        KLIST                                                  *
027300870408RIC  C                   KFLD                    CODUTE                         *
027400870408RIC  C                   KFLD                    TIPTAB            2            *
027500870408RIC  C                   KFLD                    VALUTA            8            *
027600870408     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
027700870408     C     KEYANT        KLIST                                                  *
027800870408     C                   KFLD                    TABGRU                         *
027900870408     C                   KFLD                    TABCOD                         *
028000990125     C*--- FILE LISTINI "tmlmp01L"                         *
028100870408     C     KEYLIS        KLIST                                                  *
028200870408     C                   KFLD                    SC2LIS                         *
028300870408     C                   KFLD                    SC2MAT                         *
028400990310     C                   KFLD                    tesval                         *
028500990125     C*--- FILE MATERIE PRIME "tmamp01L"                   *
028600870408     C     KEYART        KLIST                                                  *
028700870408     C                   KFLD                    SC2MAT                         *
028800990125     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
028900870408     C     KEYSAL        KLIST                                                  *
029000990125     C                   KFLD                    ANNDTA4                        *
029100870408     C                   KFLD                    MOCMAG                         *
029200870408     C                   KFLD                    SC2MAT                         *
029300870413     C     KSAL          KLIST                                                  *
029400990125     C                   KFLD                    ANNDTA4                        *
029500870413     C                   KFLD                    SC2MAG                         *
029600870413     C                   KFLD                    SC2MAT                         *
029700870408     C     *LIKE         DEFINE    KPJBU         KPJBUW                         *
029800870408     C*---------- RICERCA MASTER --------------------------*
029900870408     C                   MOVEL     KNMUS         EDP                            *
030000890630     C                   MOVEL     KNSIF         SYS               3
030100890630     C** SISTEMA INFORMATIVO
030200870408     C     SUDT1         TAG                                                    *
030300870408     C     *LOCK         IN        OCLDT1                               46      *
030400870408     C   46              GOTO      SUDT1                                        *
030500870408     C                   OUT       OCLDT1                                       *
030600871105     C*----------------
030700871105     C     SUDT2         TAG                                                    *
030800871105     C     *LOCK         IN        MPHDT1                               45      *
030900871105     C   45              GOTO      SUDT2                                        *
031000871105     C                   OUT       MPHDT1                                       *
031100870408RIC  C                   MOVE      '1'           LINGUA                         *
031200080204      *
031300080204     c                   z-add     anndta        anndta4           4 0
031400080204     c                   if        anndta4 > 40
031500080204     c                   add       1900          anndta4
031600080204     c                   else
031700080204     c                   add       2000          anndta4
031800080204     c                   end
031900870408     C*---- RICHIAMO PROGRAMMA ----------------------------*
032000870408     C     RIC150        IFEQ      *BLANKS                                      *
032100870408     C                   Z-ADD     0             DOR150                         *
032200870408     C                   Z-ADD     0             NOR150                         *
032300870408     C                   ELSE                                                   *
032400870408     C                   MOVE      FOR150        CODFOR                         *
032500870408     C                   GOTO      RICFOR                                       *FORNIT.
032600870408     C                   END                                                    *
032700870408     C*----------------------------------------------------*
032800870408     C     INIZIO        TAG                                                    *
032900870408     C*          -------------                             *
033000870408     C                   MOVE      *BLANKS       CODULT                         *
033100870408     C                   MOVE      *ZEROS        ORDULT                         *
033200870408     C                   MOVEA     *ZEROS        *IN(1)                         *
033300871005     C                   MOVEL     '?'           INTERR           12            *
033400870408     C     TAG001        TAG                                                    *
033500870408     C*          -------------                             *
033600870408     C                   SETOFF                                       08        *
033700871106     C                   MOVEL     'S'           TESTRO
033800871106     C                   MOVEL     *BLANKS       TESDTR
033900870408     C*--- EMISSIONE TESTATA                               *
034000870408     C                   WRITE     OCO100T1                                     *
034100870408     C*--- EMISSIONE PROMPT RICHIESTA CLIENTE   :          *
034200870408     C     ORDULT        COMP      0                                  03        *
034300870408     C                   EXFMT     OCO100PT                                     *
034400870408     C                   MOVEA     *ZEROS        *IN(75)                        *
034500990409     C   Kc              GOTO      FINE                                         *
034600870408     C*----------------------------------------------------*
034700870408     C*--- INTERROGAZIONE ANAGRAFICO PARTICOLARITA' -------*
034800870408     C   KB              DO                                                     *
034900870408     C                   MOVEL     KPJBU         KPJBUW                         *
035000870408     C                   MOVE      CODFOR        FOR820                         *
035100870408     C                   MOVE      NOMF30        RAG820                         *
035200870408     C                   MOVE      'R'           RIC820                         *
035300870408     C                   MOVEL     PAR820        KPJBU                          *
035400990317     C                   CALL      'TII007R'                                    *
035500870408     C                   PARM                    KPJBA                          *
035600870408     C                   MOVEL     KPJBU         PAR820                         *
035700870408     C                   MOVE      FOR820        CODFOR                         *
035800870408     C                   MOVE      RAG820        NOMF30                         *
035900870408     C                   MOVEL     KPJBUW        KPJBU                          *
036000870408     C                   GOTO      TAG001                                       *
036100870408     C                   END                                                    *
036200870408     C*--- LANCIO STAMPA ORDINE ---------------------------*
036300990209     C*  KC              DO                                                     *
036400990209     C*                  MOVEL     KPJBU         KPJBUW                         *
036500990209     C*                  MOVE      CODULT        FOR800                         *
036600990209     C*                  MOVE      NOMF30        RAG800                         *
036700990209     C*                  MOVE      ORDULT        ORD800                         *
036800990209     C*                  MOVE      'C'           RIG800                         *
036900990209     C*                  MOVE      'NO'          STA800                         *
037000990209     C*                  MOVE      *BLANKS       KPJBU                          *
037100990209     C*                  MOVEL     PAR800        KPJBU                          *
037200990209     C*                  CALL      'OCO800R'                                    *
037300990209     C*                  PARM                    KPJBA                          *
037400990209     C*                  MOVEL     KPJBUW        KPJBU                          *
037500990209     C*                  MOVE      *BLANKS       CODULT                         *
037600990209     C*                  MOVE      *ZEROS        ORDULT                         *
037700990209     C*                  GOTO      INIZIO                                       *
037800990209     C*                  END                                                    *
037900870408     C*----------------------------------------------------*
038000870408     C*--- RICERCA ALFABETICA PARTICOLARITA' FORNITORE ----*
038100990125     C     CODFOR        IFEQ      '?       '                                   *
038200990317     C                   CALL      'TII119R'                                    *
038300870408     C                   PARM                    CODFOR                         *
038400870408     C                   PARM                    NOMF30                         *
038500870408     C                   GOTO      TAG001                                       *
038600870408     C                   END                                                    *
038700870408RIC  C******************************************************
038800870408RIC  C*--- RICERCA PER CODICE CLIENTE   -------------------*
038900870408RIC  C     RICFOR        TAG                                                    *
039000870408RIC  C                   TESTN                   CODFOR               46        *
039100870408RIC  C     *IN(46)       CABEQ     '0'           TAG001                   91    *
039200990125RIC  C     CODFOR        CHAIN     tmclp01L                           91        *
039300870408RIC  C   91              GOTO      TAG001                                       *
039400990125RIC  C     KEYFOR        CHAIN     ancln01l                           91        *
039500870408RIC  C   91              GOTO      TAG001                                       *
039600990125     C     clnsogg       chain     ansog01l                           91
039700870914     C   91              GOTO      TAG001
039800990125RIC  C                   MOVE      sogdes        NOMFOR                         *
039900990125     C                   MOVEL     sogdes        NOMF30                         *
040000990125     C     clnsogg       CHAIN     aNIND01l                           91        *
040100870408     C   91              GOTO      TAG001                                       *
040200990125     C                   MOVE      CLPKSC        FORNIT            7 0          *
040300990125     C                   MOVEL     CLPSTM        CLISTM            2
040400990125     C                   MOVE      clncondpag    CLICDP            3
040500990125     C                   MOVEL     INDindriz     INDFOR                         *
040600990125     C                   MOVEL     INDlocalit    LOCFOR                         *
040700870408     C                   MOVE      INDCAP        CAPFOR                         *
040800990125     C                   MOVE      INDPRov       PROVFO                         *
040900900216     C** ESENZIONE IVA
041000890707     C                   MOVE      '160'         TABGRU                         *
041100890707     C                   MOVE      *ZEROS        TABCOD                         *
041200890707     C                   MOVE      CLISTM        TABCOD                         *
041300890707     C     KEYANT        CHAIN     ANTAB00F                           46        *
041400890707     C  N46              MOVEL     TABDES        STMDS
041500890718     C   46              MOVEL     *BLANKS       STMFLG
041600870408     C*----------------------------------------------------*
041700870408     C     RIC150        IFEQ      'O'                                          *ORDINE
041800870408     C                   Z-ADD     NOR150        NUMORD                         *
041900870408     C                   Z-ADD     DOR150        TESDOR                         *
042000870408     C                   GOTO      TAG005                                       *
042100870408     C                   END                                                    *
042200870408     C*--- PULIZIA SUBFILE "1" ----------------------------*
042300870408     C                   Z-ADD     0             REC1              3 0    70    *
042400870408     C   70              WRITE     SFL1CT                                       *
042500870408     C                   MOVEA     *ZEROS        *IN(10)                        *
042600870408     C*----------------------------------------------------*
042700870408     C*--- CARICAMENTO SUBFILE "1" ------------------------*
042800990125     C     CODFOR        SETLL     tmmoc01L                                     *
042900870408     C*--- SCRIVO RIGA "ORDINE NUOVO" ---------------------*
043000870408     C     REC1          ADD       1             REC1                           *
043100870408     C                   MOVE      *BLANKS       SF1TOR                         *
043200870408     C                   Z-ADD     0             NUMORD                         *
043300870408     C                   Z-ADD     0             TESDOR                         *
043400870408     C                   MOVEL     ORD(1)        SF1NOT                         *
043500870408     C                   WRITE     SFL1                                         *
043600870408     C*----------------------------------------------------*
043700870408     C*--- RICERCA TESTATE ORDINI -------------------------*
043800870408     C     TAG002        TAG                                                    *
043900870408     C*          -------------                             *
044000990125     C     CODFOR        READE     tmmoc01L                               46    * ORDINE NUOVO
044100870408     C   46              GOTO      TAG003                                       *
044200870408     C     REC1          ADD       1             REC1                           *
044300870408     C                   MOVE      MOCTOR        SF1TOR                         *
044400870408     C                   Z-ADD     MOCNOR        NUMORD                         *
044500990125     c                   if        mocdor > 0
044600990125     C                   move      MOCDOR        DATAIso                        *
044700990125     C                   move      dataiso       dataeur                        *
044800990125     C                   move      DATAeur       TESDOR                         *
044900990125     c                   end
045000870408     C                   MOVE      '162'         TABGRU                         *
045100870408     C                   MOVE      *ZEROS        TABCOD                         *
045200870408     C                   MOVE      MOCTOR        TABCOD                         *
045300870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
045400870408     C   46              MOVEL     'ERRORE'      SF1NOT                         *
045500870408     C  N46              MOVEL     TABDES        SF1NOT                         *
045600870408     C  N46              MOVEL     TABDES        DSORD                          *
045700870408     C*--- SCRIVO TESTATE ORDINI --------------------------*
045800870408     C                   WRITE     SFL1                                         *
045900990125     C     KEYORD        SETGT     tmmoc01L                           46        *
046000870408     C  N46              GOTO      TAG002                                       *
046100870408     C     TAG003        TAG                                                    *
046200870408     C*          -------------                             *
046300870408     C                   SETON                                        1112      *
046400870409     C                   WRITE     OCO100T1                                     *
046500870408     C*--- EMETTO SUBFILE 1 -------------------------------*
046600870408     C                   EXFMT     SFL1CT                                       *
046700870408     C                   SETOFF                                       1112      *
046800990125     C   Kl              DO                                                     *
046900870408     C     RIC150        IFNE      *BLANKS                                      * RICHIAMATO
047000870408     C                   MOVE      'P'           RIC150                         *
047100990409     C                   GOTO      inizio                                       *
047200870408     C                   END                                                    *
047300870408     C                   GOTO      INIZIO                                       *
047400870408     C                   END                                                    *
047500870408     C                   READC     SFL1                                   46    *
047600870408     C   46              GOTO      TAG003                                       *
047700870408     C     SCELTA        CABEQ     *BLANKS       TAG003                         *
047800870408     C*----------------------------------------------------*
047900870408     C     TAG005        TAG                                                    *
048000870408     C*          -------------                             *
048100870408     C     NUMORD        IFEQ      0                                            *
048200870408     C*--- LETTURA NUMERO ORDINE NUOVO DA AREA DATI -------*
048300870408     C     SULOCK        TAG                                                    *
048400870408     C*          -------------                             *
048500870408     C     *LOCK         IN        DTAORC                               46      *
048600870408     C   46              GOTO      SULOCK                                       *
048700870408     C                   Z-ADD     ORDNUO        NUMORD                         *
048800870408     C                   ADD       1             ORDNUO                         *
048900870408     C                   OUT       DTAORC                                       *
049000990125     C                   MOVE      woggi         TESDOR                         *
049100870408     C*--- IMPOSTA DATI TESTATA PER ORDINE NUOVO ----------*
049200870408     C                   MOVE      '? '          TESTOR                         *
049300870408     C                   MOVE      *BLANKS       TESDTO                         *
049400870408     C                   MOVEL     CLPPCL        TESPCL
049500870408     C                   Z-ADD     0             TESDCR
049600870408     C                   Z-ADD     0             TESDCP
049700870408     C*----------------------------------------------------*
049800870408     C                   MOVE      CLPLIS        TESLIS                         *
049900870408     C                   MOVE      CLPVAL        TESVAL                         *
050000990310RIC  C     TESVAL        IFEQ      xscdiv                                       *
050100870408RIC  C                   Z-ADD     1             TESCMB                         *
050200870408RIC  C                   ELSE                                                   *
050300990519     C                   move      tesval        divisa0202
050400990519     C                   z-add     0             Erro              1 0
050500990519     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
050600990519     C                                           ANDVS00F:%size(ANDVS00F) :
050700990519     C                                           ANDVX00F:%size(ANDVX00F))
050800990519     C                   if        Erro<> 1 and Erro<> 3
050900990519     C                   reset                   PJX00202DS
051000990519     C                   movel     tesval        divisa0202
051100990519     C                   movel     xscdiv        divmc0202
051200990519     C                   z-add     1000          impord0202
051300990519     C                   movel     *on           Gesmsg
051400990519     C                   movel     *blanks       Esito
051500990519     C                   movel     *on           Noctrlscos
051600990519     C                   callp     x0202testc(PJX00202DS:
051700990519     C                                  %size(PJX00202DS):
051800990519     C                                  *off : Esito : Noctrlscos :
051900990519     C                                  Datigest :
052000990519     C                                  %size(ANDVS00F):
052100990519     C                                  %size(ANDVX00F):
052200990519     C                                  ANDVS00F : ANDVX00F )
052300990519     C                   If        Esito = '0'
052400990519     c                   z-add     cambio0202    tescmb
052500990310     c                   end
052600990519     c                   end
052700870408RIC  C                   END                                                    *
052800870408     C                   MOVEL     *BLANKS       TESSOS
052900870408     C                   MOVEL     CLPRES        TESRES
053000870408     C                   MOVEL     CLPIMB        TESIMB
053100870408     C                   MOVEL     CLPSPE        TESSPE
053200870520     C                   MOVEL     *BLANKS       TESLUO
053300870408     C                   Z-ADD     0             TESNRF
053400870408     C                   Z-ADD     0             TESDRF
053500870408     C                   Z-ADD     0             TESNPC
053600870408     C                   Z-ADD     0             TESDPC
053700870408     C                   Z-ADD     0             TESNPN
053800870408     C*----------------------------------------------------*
053900990125     C     CODFOR        SETLL     tmscm000                               49
054000870408     C   49              Z-ADD     0             TESSC1
054100870408     C   49              Z-ADD     0             TESSC2
054200870408     C  N49              Z-ADD     CLPSC1        TESSC1                         *
054300870408     C  N49              Z-ADD     CLPSC2        TESSC2                         *
054400870408     C** SE ESISTE GESTIONE SCONTI CLIENTE/CLASSE
054500870408     C** PROPONGO SEMPRE 0 IN TESTATA
054600870408     C** ALTREMENTI PROPONGO GLI SCONTI DEL CLIENTE
054700870408     C                   Z-ADD     0             TESSC3
054800870409     C                   Z-ADD     CLPSC1        TESCL1
054900870409     C                   Z-ADD     CLPSC2        TESCL2
055000870408     C                   Z-ADD     1             TESCLP                         *
055100870408     C                   MOVE      CLICDP        TESCPG                         *
055200870408     C                   MOVE      CLPGPM        TESGPM                         *
055300870408     C                   MOVE      CLPSTM        TESSTM                         *
055400900216     C                   Z-ADD     0             TESAIV
055500870408     C*--- CONTROLLO DATI TESTATA ORDINE NUOVO ------------*
055600870408     C                   EXSR      RUTTES                                       *
055700870408     C*--- PULIZIA PROMPT RIGA DI FONDO -------------------*
055800870408     C                   EXSR      PULFON                                       *
055900870408     C*--- CARICAMENTO PROMPT RIGA DI FONDO ---------------*
056000870408     C                   EXSR      CARFON                                       *
056100870408     C                   END                                                    *
056200870408     C*--- RICERCA ORDINE ---------------------------------*
056300870408     C                   Z-ADD     0             REC2              5 0          *
056400870408     C                   Z-ADD     0             RECT              5 0          *
056500870408     C                   Z-ADD     0             SC1REC                         *
056600870408     C                   Z-ADD     0             SC1VID                         *
056700870408     C*--- PULIZIA SFL2CT                                  *
056800870408     C                   SETON                                            70    *
056900870408     C                   WRITE     SFL2CT                                       *
057000870408     C                   SETOFF                                           70    *
057100870408     C*----------------------------------------------------*
057200870408     C                   MOVEA     *ZEROS        *IN(75)                        *
057300870408     C                   Z-ADD     1             CURSOR                         * POSIZ.CURSORE
057400990125     C     KEYORD        SETLL     tmmoc01L                           41  42    *
057500870408     C   41                                                                     *
057600871005     CORN42              MOVE      *BLANKS       CODSAV           12            *
057700870408     C   41                                                                     *
057800870408     CORN42              GOTO      TAG007                                       * ORDINE NUOVO
057900870408     C*--- LETTURA RIGHE ORDINE ESISTENTE -----------------*
058000870408     C     TAG006        TAG                                                    *
058100870408     C*          -------------                             *
058200990125     C     KEYORD        READE     tmmoc01L                               46    *
058300870408     C   46              Z-ADD     REC2          RECT                           *
058400870408     C   46              GOTO      TAG007                                       *
058500870408     C*--- DATI DA 1° RIGA ORDINE ESISTENTE A TESTATA -----*
058600870408     C  N08              DO                                                     *
058700870408     C                   SETON                                        08        *
058800870408     C*---                                                 *
058900870408     C                   MOVE      '162'         TABGRU                         *
059000870408     C                   MOVE      *ZEROS        TABCOD                         *
059100870408     C                   MOVE      MOCTOR        TABCOD                         *
059200870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
059300870408     C  N46              MOVEL     TABDES        TESDTO                         *
059400870408     C  N46              MOVEL     TABDES        DSORD                          *
059500870408     C   46              MOVE      *BLANKS       TESDTO                         *
059600870408     C*---                                                 *
059700990125     c                   if        mocdor > 0
059800990125     C                   move      MOCDOR        DATAIso                        *
059900990125     C                   move      dataiso       dataeur                        *
060000990125     C                   move      DATAeur       TESDOR                         *
060100990125     c                   end
060200990125     c                   if        mocdcr > 0
060300990125     C                   move      MOCDcR        DATAIso                        *
060400990125     C                   move      dataiso       dataeur                        *
060500990125     C                   move      DATAeur       TESDcR                         *
060600990125     c                   end
060700990125     c                   if        mocdcp > 0
060800990125     C                   move      MOCDcp        DATAIso                        *
060900990125     C                   move      dataiso       dataeur                        *
061000990125     C                   move      DATAeur       TESDcp                         *
061100990125     c                   end
061200870408     C                   MOVE      MOCTOR        TESTOR                         *
061300870408     C                   MOVE      MOCLIS        TESLIS                         *
061400870408     C                   MOVE      MOCVAL        TESVAL                         *
061500990310     C                   z-add     MOCCMB        TESCMB                         *
061600990125     C     CODFOR        SETLL     tmscm000                               49
061700870408     C   49              Z-ADD     0             TESSC1
061800870408     C   49              Z-ADD     0             TESSC2
061900870408     C  N49              Z-ADD     MOCSC1        TESSC1                         *
062000870408     C  N49              Z-ADD     MOCSC2        TESSC2                         *
062100870408     C** SE ESISTE GESTIONE SCONTI CLI/CLASSE
062200870408     C** PROPONGO SEMPRE 0 COLE SCONTI DI TESTATA
062300870408     C** ALTREMENTI PROPONGO QUELLI DELL'ORDINE
062400870408     C                   Z-ADD     MOCSC3        TESSC3
062500870408     C                   Z-ADD     CLPSC1        TESCL1
062600870408     C                   Z-ADD     CLPSC2        TESCL2
062700870408     C                   Z-ADD     MOCCLP        TESCLP                         *
062800870408     C                   MOVEL     MOCCPG        TESCPG                         *
062900870408     C                   MOVE      MOCGPM        TESGPM                         *
063000870408     C                   MOVE      MOCSTM        TESSTM                         *
063100870408     C                   MOVEL     MOCPCL        TESPCL
063200870408     C                   MOVEL     MOCTRO        TESTRO
063300870408     C                   MOVEL     MOCSOS        TESSOS
063400870408     C                   MOVEL     MOCSPE        TESSPE
063500870408     C                   MOVEL     MOCIMB        TESIMB
063600870408     C                   MOVEL     MOCLUO        TESLUO
063700870408     C                   MOVEL     MOCRES        TESRES
063800870408     C                   Z-ADD     MOCNRF        TESNRF
063900990125     c                   if        mocdrf > 0
064000990125     C                   move      MOCDRF        DATAiso
064100990125     C                   move      dataiso       dataeur
064200990125     C                   move      DATAeur       TESDRF
064300990125     c                   end
064400870408     C                   Z-ADD     MOCNPC        TESNPC
064500990125     c                   if        mocdpc > 0
064600990125     C                   move      MOCDpc        DATAiso
064700990125     C                   move      dataiso       dataeur
064800990125     C                   move      DATAeur       TESdpc
064900990125     c                   end
065000870408     C                   Z-ADD     MOCNPN        TESNPN
065100870408     C                   MOVE      '161'         TABGRU
065200870408     C                   MOVE      *ZEROS        TABCOD
065300870408     C                   MOVE      MOCLIS        TABCOD
065400870408     C     KEYANT        CHAIN     ANTAB00F                           81
065500870408     C  N81              MOVEL     TBLUNI        TESDLI
065600870408     C   81              MOVEL     *BLANKS       TESDLI
065700870408     C** LISTINO
065800870408     C                   END
065900870408     C*--- CARICAMENTO SUBFILE 2 --------------------------*
066000870408     C     REC2          ADD       1             REC2                           *
066100870408     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
066200870408     C                   Z-ADD     REC2          SC1VID                         * NRR VIDEO
066300870408     C                   WRITE     SFL2                                         *
066400870408     C                   GOTO      TAG006                                       *
066500870408     C*----------------------------------------------------*
066600870408     C     TAG007        TAG                                                    *
066700870408     C*          -------------                             *
066800870408     C                   SETOFF                                       46  73    *
066900870408     C                   MOVE      MOCMAT        CODSAV                         *
067000870408     C*--- PULIZIA PROMPT RIGA DI FONDO                    *
067100870408     C                   EXSR      PULFON                                       *
067200870408     C*--- CARICAMENTO PROMPT RIGA DI FONDO                *
067300870408     C                   EXSR      CARFON                                       *
067400870408     C*--- EMISSIONE RIGA DI FONDO CON ERRORI -------------*
067500870408     C     TAG008        TAG                                                    *
067600870408     C*          -------------                             *
067700870408     C     CURSOR        DIV       9             CURSOR                         * POSIZ.CURSORE
067800870408     C                   MVR                     RESTO             5 065        *
067900870408     C     CURSOR        MULT      9             CURSOR                         *
068000870408     C   65CURSOR        ADD       1             CURSOR                         *
068100870408     C*--- EMISSIONE PROMPT RIGA --------------------------*
068200870408     C     EMISF2        TAG                                                    *
068300870408     C*          -------------                             *
068400870408     C     RECT          COMP      *ZEROS                             7171      *
068500870408     C                   SETON                                        1113      *
068600870408     C                   WRITE     OCO100T1                                     * TESTATA
068700870408     C  N71              WRITE     PULISCI                                      * PULIZIA
068800870408     C                   EXFMT     SFL2CT                                       * SFL2CT
068900870408     C*--- RICHIESTO HELP ---------------------------------*
069000870408     C   99              DO                                                     *
069100870408     C                   EXFMT     HELP                                         *
069200870408     C                   GOTO      EMISF2                                       *
069300870408     C                   END                                                    *
069400870408     C*----------------------------------------------------*
069500870408     C                   SETOFF                                       111372    *
069600890707     C                   SETOFF                                       69
069700870413     C                   MOVEA     *ZEROS        *IN(74)                        *
069800870408     C*--- FINE LAVORO ------------------------------------*
069900990125     C   Kl              DO                                                     *
070000870408     C     RIC150        IFNE      *BLANKS                                      * RICHIAMATO
070100870408     C                   MOVE      'P'           RIC150                         *
070200990409     C                   GOTO      inizio                                       *
070300870408     C                   END                                                    *
070400870408     C*--- DATI PER STAMPA ORDINE                          *
070500870408     C                   MOVE      CODFOR        CODULT                         *
070600870408     C                   Z-ADD     NUMORD        ORDULT                         *
070700870408     C                   SETOFF                                       02        *
070800870408     C*---                                                 *
070900870408     C                   GOTO      TAG001                                       *
071000870408     C                   END                                                    *
071100870921     C**-- STAMPA BOLLA DI SPEDIZIONE ---------------------*
071200870921     C   KH              DO
071300890707     C     STMFLG        COMP      ' '                                6969
071400890707     C   69              SETON                                        72
071500890707     C   69              MOVEL     ERR(19)       ERRORE
071600890707     C   69              GOTO      EMISF2
071700890707     C** NON E' POSSIBILE STAMPA BOLLA FISCALE SE SETTORE MERC.
071800890707     C** CLIENTE =1
071900870921     C                   MOVEL     MOCCLI        BOLCLI
072000870921     C                   Z-ADD     MOCNOR        BOLNOR
072100990125     c                   if        mocdor > 0
072200990125     C                   move      MOCDOR        DATAiso
072300990125     C                   move      dataiso       dataeur
072400990125     C                   move      DATAeur       BOLDOR
072500990125     c                   end
072600990125     C                   Z-ADD     woggi         BOLDEM
072700870921     C                   MOVEL     PARBOL        KPJBU
072800990317     C                   CALL      'TII311C'
072900870921     C                   PARM                    KPJBA
073000870921     C                   GOTO      EMISF2
073100870921     C                   END
073200890607     C** STAMPA BOLLA USO FISCALE
073300890707     C   KS              DO
073400890707     C     STMFLG        COMP      ' '                                    69
073500890707     C   69              SETON                                        72
073600890707     C   69              MOVEL     ERR(19)       ERRORE
073700890707     C   69              GOTO      EMISF2
073800890707     C** NON E' POSSIBILE STAMPA BOLLA INTERNA SE SETTORE MERC.
073900890707     C** CLIENTE = BLANK
074000890607     C                   MOVEL     MOCCLI        BOLCLI
074100890607     C                   Z-ADD     MOCNOR        BOLNOR
074200990125     c                   if        mocdor > 0
074300990125     C                   move      MOCDOR        DATAiso
074400990125     C                   move      dataiso       dataeur
074500990125     C                   move      DATAeur       BOLDOR
074600990125     C                   Z-ADD     woggi         BOLDEM
074700890607     C                   MOVEL     PARBOL        KPJBU
074800990317     C                   CALL      'TII313C'
074900890607     C                   PARM                    KPJBA
075000890607     C                   GOTO      EMISF2
075100890607     C                   END
075200990125     C                   END
075300890607     C**
075400870408     C*--- RICERCA DATI TESTATA ---------------------------*
075500870408     C   KA              DO                                                     *
075600870408     C                   SETON                                        07        * AGGIOR.RIGHE
075700870408     C                   EXSR      RUTTES                                       * PROMPT TESTA.
075800870408     C                   SETOFF                                       07        *
075900870408     C                   GOTO      TAG005                                       *
076000870408     C                   END                                                    *
076100870408     C*--- VISUALIZZAZIONE DISPONIBILITA' -----------------*
076200870408     C   KD              DO                                                     *
076300870408     C     SC2TRG        IFGE      '1'                                          *
076400870408     C     SC2TRG        IFLE      '3'                                          *
076500870408     C                   EXSR      RUTDIS                                       *
076600870408     C                   GOTO      EMISF2                                       *
076700870408     C                   END                                                    *
076800870408     C                   END                                                    *
076900870408     C                   END                                                    *
077000870408     C*----------------------------------------------------*
077100870408     C*--- LEGGO RIGHE RICHIAMATE -------------------------*
077200870408     C   71              READC     SFL2                                   10    *
077300870408     C   71
077400870408     CANN10              DO                                                     *
077500870408     C     SCELTA        CABEQ     *BLANKS       EMISF2                         *
077600870408     C     SC1REC        CHAIN     ORDFIS                             93        * AIUTO !
077700870408     C   93              MOVEL     ORD(2)        AIUTO                          *
077800870408     C   93              GOTO      TAG001                                       *
077900870408     C                   EXCEPT    RILORD                                       *
078000870408     C*---------- TEST POSSIBILITA AGGIORNAMENTO -BB/FA- --*
078100870408     C     MOCFAS        COMP      'BB'                                   80    *
078200870408     C  N80MOCFAS        COMP      'FA'                                   80    *
078300870408     C   80              DO                                                     *
078400870408     C     SCELTA        CABEQ     'A'           NOMAST                         *
078500870408     C*                                                    *
078600870408     C     MASTE1        IFNE      '*'                                          *
078700870408     C     MASTE1        CABNE     EDP1          NOMAST                         *
078800870408     C                   END                                                    *
078900870408     C*                                                    *
079000870408     C     MASTE2        IFNE      '*'                                          *
079100870408     C     MASTE2        CABNE     EDP2          NOMAST                         *
079200870408     C                   END                                                    *
079300870408     C*                                                    *
079400870408     C     MASTE3        IFNE      '*'                                          *
079500870408     C     MASTE3        CABNE     EDP3          NOMAST                         *
079600870408     C                   END                                                    *
079700870408     C*                                                    *
079800870408     C                   SETOFF                                           80    *
079900870408     C                   SETON                                            73    *
080000870408     C                   END                                                    *
080100870408     C*----------------------------------------------------*
080200870408     C     NOMAST        TAG                                                    *
080300870408     C  N80              SETON                                            14    * HI+BL RIGA
080400870408     C     SC1VID        CHAIN     SFL2                               94        * AIUTO !
080500870408     C   94              MOVEL     ORD(3)        AIUTO                          *
080600870408     C   94              GOTO      TAG001                                       *
080700870408     C                   MOVE      SCELTA        SCELTW                         *
080800870408     C                   MOVE      *BLANKS       SCELTA                         *
080900870408     C                   UPDATE    SFL2                                         *
081000870408     C                   Z-ADD     SC1VID        CURSOR               15  14    * POSIZ.CURSORE
081100870408     C*----------                                          *
081200870408     C   80              GOTO      ERRORI                                       *
081300870408     C                   SETON                                            40    * CODICE PR
081400871221     C                   SETOFF                                       66
081500871221     C** ASS.PROVA DIVERSO DA ZERO NON POSSO AGGIORE NULLA
081600870408     C                   EXSR      CARFON                                       *
081700871221     C     SC2ASS        COMP      0                                  6666
081800870408     C                   GOTO      EMISF2                                       *
081900870408     C                   END                                                    *
082000870408     C*--- CONTROLLO PROMPT RIGA --------------------------*
082100870408     C*----------------------------------------------------*
082200870408     C*--- RIGA RICHIAMATA PER ERRORE ---------------------*
082300870408     C     SC2TRG        IFEQ      *BLANKS                                      *
082400870408     C     SC2VID        IFGT      *ZEROS                                       *
082500870408     C     SC2VID        CHAIN     SFL2                               17        *
082600870408     C                   SETOFF                                       1480      *
082700870408     C  N17              UPDATE    SFL2                                 17      *
082800870408     C                   END                                                    *
082900870408     C                   GOTO      TAG007                                       *
083000870408     C                   END                                                    *
083100870408     C*--- RIGA ANNULLATA ---------------------------------*
083200870408     C     SC2TRG        IFEQ      'A'                                          *
083300870408     C     SCELTW        CABNE     SC2TRG        ERRORI               7575      *
083400870408     C     SC2REC        CABEQ     0             TAG007                         *
083500870408     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
083600870408     C   93              MOVEL     ORD(2)        AIUTO                          *
083700870408     C   93              GOTO      TAG001                                       *
083800880105     C     MOCFAS        CABEQ     'FA'          ERRORI                   75    *
083900880105     C** FASE FA NON SI PUO ANNULLARE
084000880127     C     MOCASS        CABGT     0             ERRORI               75
084100990125     C     KEYART        CHAIN     tmamp01L                           76
084200870413     C     MOCTRG        IFGE      '1'                                          *
084300870408     C     MOCTRG        IFLE      '3'                                          *
084400870408     C     MOCQTA        MULT      -1            MOCQTA                         *
084500991112     C                   EXSR      AGGSAL                                       *
084600870413     C** FASE BB NON SI AGGIORNA SALDI MAGAZZINO
084700870408     C                   END                                                    *
084800870408     C                   END                                                    *
084900910926     C*--- CONTROLLO CODICE PRODOTTO ----------------------*
085000910926     C  N76              DO                                                     *
085100910926     C                   MOVE      '155'         TABGRU                         *
085200910926     C                   MOVE      *ZEROS        TABCOD                         *
085300910926     C                   MOVE      AMPCLM        TABCOD                         *
085400910926     C     KEYANT        CHAIN     ANTAB00F                           78        *
085500910926     C  N78              MOVEL     TABDES        TB155                          *
085600910926     C   78              MOVE      *BLANKS       TB155                          *
085700910926     C                   END                                                    *
085800910926     C*
085900870408     C                   MOVE      'A'           MOCATT                         *
086000870408     C                   EXSR      UPORD                                        *
086100870408     C                   SETOFF                                       7173      *
086200870408     C                   GOTO      TAG005                                       *
086300870408     C                   END                                                    *
086400870408     C*----------------------------------------------------*
086500870408     C*--- CAMBIO TIPO RIGA -------------------------------*
086600870408     C     SC2REC        IFGT      0                                            * RIGA VECCHIA
086700870408     C     MOCTRG        CABNE     SC2TRG        ERRORI               7575      *
086800870408     C                   END                                                    *
086900870408     C*--- RICERCA ALFABETICA ARTICOLI E ------------------*
087000870408     C*--- ACQUISIZIONE DATI DELLA STESSA -----------------*
087100870408     C     SC2TRG        IFGE      '1'                                          *
087200870408     C     SC2TRG        IFLE      '3'                                          *
087300871005     C     SC2MAT        IFEQ      INTERR                                       *
087400870408     C                   MOVE      *BLANKS       SC2MAT                         *
087500990317     C                   CALL      'TII104R'                                    *
087600870408     C                   PARM                    SC2MAT                         *
087700870408     C                   PARM                    SC2DMT                         *
087800870408     C                   END                                                    *
087900870408     C*--- CONTROLLO CODICE PRODOTTO ----------------------*
088000990125     C     KEYART        CHAIN     tmamp01L                           76        *
088100870408     C  N76              DO                                                     *
088200910925     C                   MOVE      '155'         TABGRU                         *
088300910925     C                   MOVE      *ZEROS        TABCOD                         *
088400910925     C                   MOVE      AMPCLM        TABCOD                         *
088500910926     C     KEYANT        CHAIN     ANTAB00F                           78        *
088600910925     C  N78              MOVEL     TABDES        TB155                          *
088700910925     C   78              MOVE      *BLANKS       TB155                          *
088800910925     C     FLGATT        IFEQ      'A'                                          *
088900910925     C                   SETON                                        20        *
089000910925     C                   ELSE                                                   *
089100910925     C                   SETOFF                                       20        *
089200910925     C                   END                                                    *
089300870408     C                   MOVEL     AMPDMT        SC2DMT                         *
089400870408     C                   MOVE      AMPUMQ        SC2UMQ                         *
089500870408     C     SC2MAG        IFEQ      *BLANKS                                      *
089600870408     C                   MOVE      AMPMAG        SC2MAG                         *
089700870408     C                   END                                                    *
089800870408     C     SC2MAG        IFEQ      '9999999'                                    *
089900870408     C                   MOVE      '?      '     SC2MAG                         *
090000870408     C                   END                                                    *
090100870408     C                   END                                                    *
090200870408     C                   END                                                    *
090300870408     C                   END                                                    *
090400870408     C     SC2FAS        COMP      'FA'                                   80
090500870408     C  N80SC2FAS        COMP      'BB'                                   80
090600870408     C*--- CONTROLLO QUANTITA' ----------------------------*
090700870408     C     SC2TRG        IFGE      '6'                                          *
090800870408     C     SC2TRG        IFLE      '9'                                          *
090900870408     C                   Z-ADD     1             SC2QTA                         *
091000870408     C                   END                                                    *
091100870408     C                   END                                                    *
091200870408     C     SC2QTA        COMP      *ZEROS                                 79    *
091300870408     C*--- RICERCA LISTINO --------------------------------*
091400870408     C     SC2LIS        CABEQ     '999'         NOLIST                         *
091500870408     C     SC2LIS        IFEQ      '?  '                                        *
091600870408     C                   MOVE      *BLANKS       SC2LIS                         *
091700870408     C                   MOVE      *BLANKS       DESL75           75            *
091800990317     C                   CALL      'TII116R'                                    *
091900870408     C                   PARM                    SC2LIS                         *
092000870408     C                   PARM                    DESL75                         *
092100870408     C                   END                                                    *
092200870408     C                   MOVE      '161'         TABGRU                         *
092300870408     C                   MOVE      *ZEROS        TABCOD                         *
092400870408     C                   MOVE      SC2LIS        TABCOD                         *
092500870408     C     KEYANT        CHAIN     ANTAB00F                           81        *
092600870408     C     NOLIST        TAG                                                    *
092700870408     C*----------------------------------------------------*
092800870408     C*--- CONTROLLO PREZZO -------------------------------*
092900870408     C     KSFC          KLIST
093000870408     C                   KFLD                    CODFOR
093100870408     C                   KFLD                    AMPCLM
093200870408     C     SC2TRG        IFGE      '1'                                          *
093300870408     C     SC2TRG        IFLE      '3'                                          *
093400870408     C     SC2PRZ        IFEQ      *ZEROS                                       *
093500870408     C     SC2SC1        CABNE     0             POISC
093600870408     C     SC2SC2        CABNE     0             POISC
093700990125     C     KSFC          CHAIN     tmscm000                           49
093800870408     C  N49              Z-ADD     SCCSC1        SC2SC1
093900870408     C  N49              Z-ADD     SCCSC2        SC2SC2
094000871218     C  N49              GOTO      POISC
094100990125     C   49CODFOR        SETLL     tmscm000                               49
094200871218     C  N49              Z-ADD     CLPSC1        SC2SC1
094300871218     C  N49              Z-ADD     CLPSC2        SC2SC2
094400871218     C** SE TROVO SCONTI CLI/CLASSE CONSIDERO LO SCONTO TROVATO
094500871218     C** SE NON TROVO LO SCONTO PER QUELLA CLASSE                      ONT
094600871218     C* VERIFICO SE ESISTE GESTIONE DEGLI SCONTI PER ALTRE CLASSI
094700871218     C** E IN TAL CASO NON APPLICO SCONTI
094800871218     C** SE NON ESISTE NESSUN SCONTO PER NESSUNA CLASSE
094900871218     C* APPLICO GLI SCONTI DEL CLIENTE
095000870408     C     POISC         TAG
095100870408     C*--- INTERR.LISTINI :                                *
095200990125     C     KEYLIS        CHAIN     tmlmp01L                           83        *
095300870408     C  N83              DO                                                     *
095400870408     C     SC2CLP        IFEQ      1                                            *
095500870408     C                   Z-ADD     LMPVP1        SC2PRZ                         *
095600870408     C                   END                                                    *
095700870408     C     SC2CLP        IFEQ      2                                            *
095800870408     C                   Z-ADD     LMPVP2        SC2PRZ                         *
095900870408     C                   END                                                    *
096000870408     C     SC2CLP        IFEQ      3                                            *
096100870408     C                   Z-ADD     LMPVP3        SC2PRZ                         *
096200870408     C                   END                                                    *
096300870408     C     SC2CLP        IFEQ      4                                            *
096400870408     C                   Z-ADD     LMPVP4        SC2PRZ                         *
096500870408     C                   END                                                    *
096600870408     C     SC2CLP        IFEQ      5                                            *
096700870408     C                   Z-ADD     LMPVP5        SC2PRZ                         *
096800870408     C                   END                                                    *
096900870408     C                   END                                                    *
097000870408     C                   END                                                    *
097100870408     C                   END                                                    *
097200870408     C                   END                                                    *
097300870408     C     SC2TRG        IFLE      '3'                                          *
097400870408     C     SC2PRZ        COMP      *ZEROS                                 83    *
097500870408     C                   END                                                    *
097600870408     C*----------------------------------------------------*
097700870408     C*--- CONTROLLO CODICE MAGAZZINO ---------------------*
097800870408     C     SC2TRG        IFGE      '1'                                          *
097900870408     C     SC2TRG        IFLE      '3'                                          *
098000870408     C     SC2MAG        IFEQ      '?      '                                    *
098100870408     C                   MOVE      *BLANKS       SC2MAG                         *
098200870408     C                   MOVE      *BLANKS       DESMAG           75            *
098300990317     C                   CALL      'TII108R'                                    *
098400870408     C                   PARM                    SC2MAG                         *
098500870408     C                   PARM                    DESMAG                         *
098600870408     C                   END                                                    *
098700870408     C                   MOVE      '153'         TABGRU                         *
098800870408     C                   MOVE      SC2MAG        TABCOD                         *
098900870408     C     KEYANT        CHAIN     ANTAB00F                           85        *
099000870408     C  N85              DO                                                     *
099100870408     C                   MOVEL     TABDES        DSMAG                          *
099200870408     C     SC2CDC        IFEQ      *BLANKS                                      *
099300990318     C                   MOVEl     CDC153        SC2CDC                         *
099400870408     C                   END                                                    *
099500870408     C                   END                                                    *
099600870408     C                   END                                                    *
099700870408     C                   END                                                    *
099800870413     C*********************************************
099900870413     C** CONTROLLO FASE AS SE ESISTE DISPONIBILI TA
100000870413     C                   SETOFF                                       82
100100870413     C     SC2FAS        IFEQ      'AS'
100200060628     C                   Z-ADD     0             DIFFAS           10 2
100300870921     C     SC2FAS        IFEQ      MOCFAS
100400870413     C     SC2QTA        IFGT      MOCQTA
100500870413     C     SC2QTA        SUB       MOCQTA        DIFFAS
100600870921     C                   END
100700870921     C                   ELSE
100800870921     C                   Z-ADD     SC2QTA        DIFFAS
100900870921     C                   END
101000870921     C** CALCOLO QUANTITA OCCORRENTE DISPONIBILE
101100990125     C     KSAL          CHAIN     tmsmp01L                           82
101200871118     C   82              GOTO      ERRORI
101300871118     C                   DO
101400060628     C                   Z-ADD     0             DISREA           10 2
101500870413     C     SMPQGF        SUB       SMPQIM        DISREA
101600870518     C                   SUB       SMPQIP        DISREA
101700870413     C     DIFFAS        COMP      DISREA                             82
101800871105     C   82              Z-ADD     DISREA        DISVID            9 0
101900870413     C   82              GOTO      ERRORI
102000870413     C                   END
102100870413     C                   END
102200870413     C** VARIAZIONE FASE  IN AS
102300870413     C** CON TROLLO  LA DISPONIBILITA' REALE
102400870413     C** A MAGAZZINO DELL'ARTICOLO
102500870408RIC  C******************************************************
102600870408RIC  C*--- CONTROLLO C.D.C.--------------------------------*
102700990125RIC  C     SC2CDC        CABEQ     '99999999'    NONCDC                         *
102800870408RIC  C     SC2TRG        IFLT      '6'                                          *
102900870408RIC  C     SC2CDC        CABEQ     *ZEROS        NONCDC                   87    *
103000870408RIC  C                   MOVE      SC2CDC        CCOSTO                         *
103100990125RIC  C     KEYCDC        CHAIN     anent01l                           87        *
103200870408RIC  C                   END                                                    *
103300870408     C     NONCDC        TAG                                                    *
103400870408     C*--- CONTROLLO DATA CONSEGNA RICHIESTA --------------*
103500870410     C     SC2DCP        IFGT      0
103600000621     C     *eur          test(d)                 sc2dcp                 89      *
103700000621     C   89*dmy          test(d)                 tesdor                 89      *
103800000621     C   89              goto      errori                                       *
103900990125     C                   move      sc2dcp        dataeur                        *
104000990125     C                   move      dataeur       dataiso                        *
104100990125     C                   move      dataiso       DATCON            8 0          *
104200870410     C                   END
104300870408     C*--- CONTROLLO PRESENZA ERRORI ----------------------*
104400870408     C     ERRORI        TAG                                                    *
104500880106     C   74              MOVEA     ERR(6)        ERRORE
104600870408     C     75            DO        89            X                 2 0          *
104700870408     C     *IN(X)        COMP      '1'                                    72    *
104800870408     C  N72              END                                                    *
104900870408     C   72              DO                                                     * ERRORI
105000870408     C                   SETOFF                                       15        *
105100870408     C     X             SUB       74            IN                2 0          *
105200870408     C                   MOVEA     ERR(IN)       ERRORE                         *
105300910926     C   88              MOVEA     ERR(20)       ERRORE
105400870413     C   82              MOVEA     ERR(16)       ERRORE
105500871105     C   82              MOVE      DISVID        ERRORE
105600870914     C   84              MOVEA     ERR(17)       ERRORE
105700870914     C   86              MOVEA     ERR(18)       ERRORE
105800000621     C   89              MOVEA     ERR(15)       ERRORE
105900870408     C                   GOTO      TAG008                                       *
106000870408     C                   END                                                    *
106100870408     C*----------------------------------------------------*
106200870408     C*---------- MANUTENZIONE RIGHE ORDINI --ADD----------*
106300870408     C*----------------------------------------------------*
106400870408     C     SC2REC        IFEQ      0                                            * RIGA NUOVA
106500870408     C*--- DATI DA VIDEO A FILE E AGGIORNAMENTO -----------*
106600870408     C                   SETON                                            39    *
106700870408     C                   EXSR      CARFIL                                       *
106800870408     C                   EXSR      UPORD                                        *
106900870408     C                   Z-ADD     NREL2         SC1REC                         *
107000870408     C*--- AGGIUNGO RIGA SFL2 -----------------------------*
107100870408     C     RECT          ADD       1             RECT                           *
107200870408     C                   Z-ADD     RECT          REC2                           *
107300870408     C                   Z-ADD     RECT          SC1VID                         *
107400870408     C                   Z-ADD     RECT          CURSOR                         *
107500870408     C                   WRITE     SFL2                                         *
107600870408     C*--- AGGIORNO CAMPI SU SALDI ------------------------*
107700870408     C     SC2TRG        IFGE      '1'                                          *
107800870408     C     SC2TRG        IFLE      '3'                                          *
107900870408     C                   EXSR      AGGSAL                                       *
108000870408     C                   END                                                    *
108100870408     C                   END                                                    *
108200870408     C                   GOTO      TAG007                                       *
108300870408     C                   END                                                    *
108400870408     C*----------------------------------------------------*
108500870408     C*--- MANUTENZIONE RIGA ORDINI --UDP------------------*
108600870408     C*----------------------------------------------------*
108700870408     C     SC2REC        IFGT      0                                            * RIGA VECCHIA
108800870408     C*--- RICERCA RIGA FILE E RILASCIO RECORD ------------*
108900870408     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
109000870408     C   93              MOVEL     ORD(2)        AIUTO                          *
109100870408     C   93              GOTO      TAG001                                       *
109200870408     C                   EXCEPT    RILORD                                       *
109300870408     C*--- RICARICO VECCHIA DISPONIBILITA' ----------------*
109400880106     C     SC2TRG        IFGE      '1'                                          *
109500880106     C     SC2TRG        IFLE      '3'                                          *
109600870408     C     MOCQTA        MULT      -1            MOCQTA                         *
109700870408     C                   EXSR      AGGSAL                                       *
109800870408     C                   END                                                    *
109900870408     C                   END                                                    *
110000870408     C*--- RICERCA RIGA FILE ------------------------------*
110100870408     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
110200870408     C   93              MOVEL     ORD(2)        AIUTO                          *
110300870408     C   93              GOTO      TAG001                                       *
110400870408     C*--- RICERCA RIGA SFL2 ------------------------------*
110500870408     C     SC2VID        CHAIN     SFL2                               94        * AIUTO !
110600870408     C   94              MOVEL     ORD(3)        AIUTO                          *
110700870408     C   94              GOTO      TAG001                                       *
110800870408     C*--- AGGIORNO RIGA FILE -----------------------------*
110900870408     C     SC2TRG        IFGE      '6'                                          *
111000870408     C                   MOVE      *BLANKS       SC2UMQ                         *
111100870408     C                   END                                                    *
111200870408     C                   SETOFF                                           39    *
111300870408     C                   EXSR      CARFIL                                       *
111400870408     C                   EXSR      UPORD                                        *
111500870408     C*--- AGGIORNO RIGA SFL2 -----------------------------*
111600870408     C                   UPDATE    SFL2                                         *
111700870408     C*--- SCARICO DISPONIBILITA' -------------------------*
111800880106     C     SC2TRG        IFGE      '1'                                          *
111900880106     C     SC2TRG        IFLE      '3'                                          *
112000870408     C                   EXSR      AGGSAL                                       *
112100870408     C                   END                                                    *
112200870408     C                   END                                                    *
112300870408     C                   GOTO      TAG007                                       *
112400870408     C                   END                                                    *
112500870408     C*--- FINE LAVORO ------------------------------------*
112600870408     C     FINE          TAG                                                    *
112700870408     C*--- ERRORE -----------------------------------------*
112800870408     C     FINERR        TAG                                                    *
112900870408     C   90              MOVEL     'KPJB207'     KMSMN                          *
113000870408     C                   SETON                                            LR    *
113100870408     C***------------------------------------------------***
113200870408     C**--- ROUTINE PULIZIA PROMPT RIGA DI FONDO ---------**
113300870408     C***------------------------------------------------***
113400870408     CSR   PULFON        BEGSR                                                  *
113500870408     C                   MOVE      ' '           MOCTRG                         *
113600870408     C                   MOVE      *BLANKS       MOCMAT                         *
113700870408     C                   MOVE      *BLANKS       MOCDMT                         *
113800870408     C                   MOVE      *BLANKS       MOCUMQ                         *
113900870408     C                   Z-ADD     0             MOCQTA                         *
114000870408     C                   MOVE      *BLANKS       MOCFAS                         *
114100870408     C                   MOVE      *BLANKS       MOCLIS                         *
114200870408     C                   Z-ADD     0             MOCPRZ                         *
114300870408     C                   Z-ADD     0             PREZZK           11 2          *
114400870408     C                   Z-ADD     0             MOCSC1                         *
114500870408     C                   Z-ADD     0             MOCSC2                         *
114600870408     C                   MOVE      *BLANKS       MOCCOM                         *
114700870408     C                   MOVE      *BLANKS       MOCMAG                         *
114800870408     C                   MOVE      *BLANKS       MOCCDC                         *
114900870408     C                   Z-ADD     0             MOCAIV                         *
115000870408     C                   Z-ADD     0             MOCDCP                         *
115100870408     C*--                                                  *
115200870408     C                   Z-ADD     0             SC1REC                         * NRR FIS. SFL2
115300870408     C                   Z-ADD     0             SC1VID                   11    * NRR VIDEO
115400870408     C                   Z-ADD     0             SC2REC                         * NRR FIS.FONDO
115500870408     C                   Z-ADD     0             SC2VID                         * NRR VID.FONDO
115600870520     C                   Z-ADD     0             MOCASS
115601110817     C                   Z-ADD     0             SC2dbl                         * NRR VID.FONDO
115602110817     C                   Z-ADD     0             SC2nbl                         * NRR VID.FONDO
115700870408     C                   SETOFF                                           40    *
115800870408     CSR                 ENDSR                                                  *
115900870408     C***------------------------------------------------***
116000870408     C**--- ROUTINE CARICAMENTO DATI PROMPT RIGA FONDO ---**
116100870408     C***------------------------------------------------***
116200870408     CSR   CARFON        BEGSR                                                  *
116300870520     C     MOCASS        IFGT      0
116400870520     C                   Z-ADD     MOCASS        SC2ASS
116500870520     C                   ELSE
116600870520     C                   Z-ADD     0             SC2ASS
116700870520     C                   END
116800870408     C     MOCTRG        IFEQ      *BLANKS                                      *
116900870408     C                   MOVE      '1'           SC2TRG                         *
117000870408     C                   ELSE                                                   *
117100870408     C     SCELTW        IFEQ      'A'                                          *
117200870408     C                   MOVE      'A'           SC2TRG                         *
117300870408     C                   ELSE                                                   *
117400870408     C                   MOVE      MOCTRG        SC2TRG                         *
117500870408     C                   END                                                    *
117600870408     C                   END                                                    *
117700870408     C     MOCMAT        IFEQ      *BLANKS                                      *
117800870408     C                   MOVE      INTERR        SC2MAT                         *
117900870408     C                   ELSE                                                   *
118000870408     C                   MOVE      MOCMAT        SC2MAT                         *
118100870408     C                   END                                                    *
118200870408     C                   MOVEL     MOCDMT        SC2DMT                         *
118300870408     C                   MOVE      MOCUMQ        SC2UMQ                         *
118400870408     C                   Z-ADD     MOCQTA        SC2QTA                         *
118500870408     C     MOCFAS        IFEQ      *BLANKS                                      *
118600870408     C                   MOVE      'OR'          SC2FAS                         *
118700870408     C                   ELSE                                                   *
118800870408     C                   MOVE      MOCFAS        SC2FAS                         *
118900870408     C                   END                                                    *
119000870408     C     MOCLIS        IFEQ      *BLANKS                                      *
119100870408     C                   MOVE      TESLIS        SC2LIS                         *
119200870408     C                   ELSE                                                   *
119300870408     C                   MOVE      MOCLIS        SC2LIS                         *
119400870408     C                   END                                                    *
119500870408     C                   Z-ADD     MOCPRZ        SC2PRZ                         *
119600870408     C                   Z-ADD     SC2PRZ        PREZZK                         *
119601110817     C                   Z-ADD     MOCnbl        SC2nbl                         *
119602110817     c                   clear                   sc2dbl
119603110817      *
119604110817     c                   if        mocdbl > 0
119605110817     c                   move      mocdbl        dataiso
119606110817     c                   move      dataiso       dataeur
119607110817     c                   move      dataeur       sc2dbl
119608110817     c                   endif
119609110817      *
119700870408     C     SC1VID        IFEQ      *ZEROS                                       *
119800870408     C                   Z-ADD     TESSC1        SC2SC1                         *
119900870408     C                   Z-ADD     TESSC2        SC2SC2                         *
120000870408     C                   MOVE      TESCLP        SC2CLP                         *
120100870408     C                   ELSE                                                   *
120200870408     C                   Z-ADD     MOCSC1        SC2SC1                         *
120300870408     C                   Z-ADD     MOCSC2        SC2SC2                         *
120400870408     C                   Z-ADD     MOCCLP        SC2CLP
120500870408     C                   END                                                    *
120600870408     C*----------------------------------------------------*
120700870408     C     MOCCPG        IFEQ      *BLANKS                                      *
120800870408     C                   MOVE      TESCPG        SC2CPG                         *
120900870408     C                   ELSE                                                   *
121000870408     C                   MOVEL     MOCCPG        SC2CPG                         *
121100870408     C                   END                                                    *
121200870408     C                   MOVE      MOCCOM        SC2COM                         *
121300870408     C                   MOVE      MOCMAG        SC2MAG                         *
121400870408     C                   MOVE      MOCCDC        SC2CDC                         *
121500870408     C     MOCAIV        IFEQ      *ZEROS                                       *
121600870408     C                   MOVE      TESAIV        SC2AIV                         *
121700870408     C                   ELSE                                                   *
121800870408     C                   MOVE      MOCAIV        SC2AIV                         *
121900870408     C                   END                                                    *
122000870408     C     MOCDCP        IFEQ      *ZEROS                                       *
122100870408     C                   MOVE      TESDCP        SC2DCP                         *
122200870408     C                   ELSE                                                   *
122300990125     C                   move      MOCDCP        dataiso                        *
122400990125     C                   move      dataiso       dataeur                        *
122500990125     C                   move      DATAeur       SC2DCP                         *
122600870408     C                   END                                                    *
122700870408     C                   Z-ADD     SC1REC        SC2REC                         *NRR FIS.FONDO
122800870408     C                   Z-ADD     SC1VID        SC2VID                         *NRR VID.FONDO
122900870408     CSR                 ENDSR                                                  *
123000870408     C***------------------------------------------------***
123100870408     C**--- AGGIORNAMENTO RIGHE ORDINI -------------------**
123200870408     C***------------------------------------------------***
123300870408     CSR   UPORD         BEGSR                                                  *
123400870408     C  N39              UPDATE    ORDFIS                                       * UPDATE
123500870408     C   39              WRITE     ORDFIS                                       * WRITE
123600870408     C                   SETOFF                                           39    *
123700870408     C                   MOVE      *BLANKS       SCELTW            1            *
123800870408     CSR                 ENDSR                                                  *
123900870408     C***------------------------------------------------***
124000870408     C**--- VISUALIZZAZIONE DISPONIBILITA' ---------------**
124100870408     C***------------------------------------------------***
124200870408     CSR   RUTDIS        BEGSR                                                  *
124300870408     C                   MOVEL     KPJBU         KPJBUW                         *
124400870408     C                   MOVE      TESDOR        AAA115                         *
124500870408     C                   MOVE      SC2MAG        MAG115                         *
124600870408     C                   MOVE      SC2MAT        MAT115                         *
124700870408     C                   MOVE      'Q'           UMI115                         *
124800870408     C                   MOVE      'D'           SCE115                         *
124900870408     C                   MOVEL     PH115         KPJBU                          *
125000990317     C                   CALL      'TII202R'                                    *
125100870408     C                   PARM                    KPJBA                          *
125200870408     C                   MOVEL     KPJBUW        KPJBU                          *
125300870408     CSR                 ENDSR                                                  *
125400870408     C***------------------------------------------------***
125500870408     C**--- ROUTINE DATI DA VIDEO A FILE -----------------**
125600870408     C***------------------------------------------------***
125700870408     CSR   CARFIL        BEGSR                                                  *
125800870409     C                   MOVE      ' '           MOCATT                         *
125900990125     C                   move      xsccut        MOCUTE                         *
126000990125     C                   Z-ADD     woggig        MOCDMN                         *
126100870409     C                   MOVE      CODFOR        MOCCLI                         *
126200990310     C                   MOVEL     TESval        MOCval
126300990310     C                   z-add     TEScmb        MOCcmb
126400990125     c                   if        tesdor > 0
126500990125     C                   move      TESDOR        DATAeur                        *
126600990125     C                   move      dataeur       dataiso                        *
126700990125     C                   move      DATAIso       MOCDOR                         *
126800990125     c                   else
126900990125     c                   z-add     0             mocdor
127000990125     c                   end
127100870409     C                   Z-ADD     NUMORD        MOCNOR                         *
127200870409     C                   MOVE      TESTOR        MOCTOR                         *
127300990125     c                   if        sc2dcp > 0
127400990125     C                   move      sc2dcp        DATAeur                        *
127500990125     C                   move      dataeur       dataiso                        *
127600990125     C                   move      DATAIso       MOCDcp                         *
127700990125     c                   else
127800990125     c                   z-add     0             mocdcp
127900990125     c                   end
128000990125     c                   if        tesdcr > 0
128100990125     C                   move      tesdcr        DATAeur                        *
128200990125     C                   move      dataeur       dataiso                        *
128300990125     C                   move      DATAIso       MOCDcr                         *
128400990125     c                   else
128500990125     c                   z-add     0             mocdcr
128600990125     c                   end
128700870409     C                   MOVE      SC2LIS        MOCLIS                         *
128800870408     C                   Z-ADD     SC2AIV        MOCAIV                         *
128900870520     C   39              MOVE      *BLANKS       MOCFES                         *
129000870409     C                   MOVE      SC2TRG        MOCTRG                         *
129100870408     C     SC2TRG        CABEQ     '1'           NOFASE                         *
129200870408     C     SC2TRG        CABEQ     '2'           NOFASE                         *
129300870408     C     SC2TRG        CABEQ     '3'           NOFASE                         *
129400870409     C                   MOVE      'OR'          SC2FAS                         *
129500870408     C     NOFASE        TAG                                                    *
129600870409     C                   MOVE      SC2FAS        MOCFAS                         *
129700870520     C   39              MOVE      *BLANKS       MOCTSR                         *
129800870409     C                   MOVE      SC2MAT        MOCMAT                         *
129900870408     C     SC2TRG        CABEQ     '1'           NOCODI                         *
130000870408     C     SC2TRG        CABEQ     '2'           NOCODI                         *
130100870408     C     SC2TRG        CABEQ     '3'           NOCODI                         *
130200870408     C                   MOVE      *BLANKS       MOCMAT                         *
130300870408     C     NOCODI        TAG                                                    *
130400870408     C     SC2TRG        IFGE      '6'                                          *
130500870408     C     SC2TRG        IFLE      '8'                                          *
130600870409     C                   MOVE      CODSAV        MOCMAT                         *
130700870408     C                   END                                                    *
130800870408     C                   END                                                    *
130900870408     C*----------------------------------------------------*
131000870408     C                   MOVEL     SC2DMT        MOCDMT                         *
131100870408     C                   MOVE      SC2UMQ        MOCUMQ                         *
131200870409     C     SC2TRG        IFLT      '1'                                          *
131300870408     C                   MOVE      'NR '         MOCUMQ                         *
131400870408     C                   END                                                    *
131500870408     C                   Z-ADD     SC2QTA        MOCQTA                         *
131600870408     C                   Z-ADD     SC2PRZ        MOCPRZ                         *
131700870408     C                   Z-ADD     SC2SC1        MOCSC1                         *
131800870408     C                   Z-ADD     SC2SC2        MOCSC2                         *
131900870408     C                   Z-ADD     TESSC3        MOCSC3
132000870408     C                   Z-ADD     SC2CLP        MOCCLP
132100870408     C                   MOVEL     SC2COM        MOCCOM                         *
132200870520     C   39              Z-ADD     0             MOCDBL                         *
132300870520     C   39              Z-ADD     0             MOCNBL                         *
132400870520     C   39              Z-ADD     0             MOCDFT                         *
132500870520     C   39              Z-ADD     0             MOCNFT                         *
132600870409     C                   MOVE      SC2MAG        MOCMAG                         *
132700870409     C                   MOVE      SC2CDC        MOCCDC                         *
132800870520     C   39              Z-ADD     0             MOCCRG                         *
132900870409     C                   MOVE      TESGPM        MOCGPM                         *
133000870409     C                   MOVE      TESSTM        MOCSTM                         *
133100870409     C   39              MOVE      *BLANKS       MOCFLG                         *
133200870520     C   39              MOVEL     *BLANKS       MOCFLS
133300870520     C   39              Z-ADD     0             MOCASS
133400870408     C                   Z-ADD     TESNPN        MOCNPN
133500990125     c                   if        tesdpc > 0
133600990125     C                   move      TESDPC        DATAeur
133700990125     C                   move      dataeur       dataiso
133800990125     C                   move      DATAIso       MOCDPC
133900990125     c                   else
134000990125     C                   z-add     0             MOCDPC
134100990125     c                   end
134200870410     C                   Z-ADD     TESNPC        MOCNPC
134300870408     C                   MOVEL     TESSOS        MOCSOS
134400870408     C                   MOVEL     TESPCL        MOCPCL
134500990125     c                   if        tesdrf > 0
134600990125     C                   move      TESDrf        DATAeur
134700990125     C                   move      dataeur       dataiso
134800990125     C                   move      DATAIso       MOCdrf
134900990125     c                   else
135000990125     C                   z-add     0             MOCdrf
135100990125     c                   end
135200870408     C                   Z-ADD     TESNRF        MOCNRF
135300870410     C                   MOVEL     TESCPG        MOCCPG
135400870408     C                   MOVEL     TESIMB        MOCIMB
135500870410     C                   MOVEL     TESSPE        MOCSPE
135600870408     C                   MOVEL     TESRES        MOCRES
135700870408     C                   MOVEL     TESLUO        MOCLUO
135800870408     C                   MOVEL     TESTRO        MOCTRO
135900870408     CSR                 ENDSR                                                  *
136000870408     C***------------------------------------------------***
136100870408     C**--- AGGIORNAMENTO CAMPI SU SALDI M.P. ------------**
136200870408     C***------------------------------------------------***
136300870408     CSR   AGGSAL        BEGSR                                                  *
136400870408     C*---                                                 *
136500990929     c                   if        anndta4 = 0
136600990929     c                   z-add     anndta        anndta4           4 0
136700990929     c                   if        anndta4 > 40
136800990929     c                   add       1900          anndta4
136900990929     c                   else
137000990929     c                   add       2000          anndta4
137100990929     c                   end
137200990929     c                   end
137300990125     C     KEYSAL        CHAIN     tmsmp01L                           39        *
137400870408     C   39              DO                                                     *
137500990125     C                   MOVE      ANNDTA4       SMPAAC                         *
137600870408     C                   MOVE      SC2MAG        SMPMAG                         *
137700870408     C                   MOVE      SC2MAT        SMPMAT                         *
137800870408     C                   MOVE      AMPUMA        SMPUMA                         *
137900870408     C                   Z-ADD     0             SMPPLF                         *
138000870408     C                   Z-ADD     0             SMPPLV                         *
138100870408     C                   Z-ADD     0             SMPQGI                         *
138200870408     C                   Z-ADD     0             SMPQOR                         *
138300870408     C                   Z-ADD     0             SMPQPR                         *
138400870408     C                   Z-ADD     0             SMPQIM                         *
138500870519     C                   Z-ADD     0             SMPQIP                         *
138600870408     C                   Z-ADD     0             SMPQGF                         *
138700870408     C                   Z-ADD     0             SMPQE1                         *
138800870408     C                   Z-ADD     0             SMPQE2                         *
138900870408     C                   Z-ADD     0             SMPQE3                         *
139000870408     C                   Z-ADD     0             SMPQE4                         *
139100870408     C                   Z-ADD     0             SMPQE5                         *
139200870408     C                   Z-ADD     0             SMPQU1                         *
139300870408     C                   Z-ADD     0             SMPQU2                         *
139400870408     C                   Z-ADD     0             SMPQU3                         *
139500870408     C                   Z-ADD     0             SMPQU4                         *
139600870408     C                   Z-ADD     0             SMPQU5                         *
139700990125     C                   Z-ADD     woggig        SMPDMS                         *
139800870408     C                   Z-ADD     0             SMPNMS                         *
139900870408     C                   Z-ADD     0             SMPPMC                         *
140000870408     C                   MOVE      *BLANKS       SMPFLG                         *
140100870408     C                   END                                                    *
140200870408     C*---                                                 *
140300870408     C                   Z-ADD     0             QTSALD           11 2          *
140400870408     C     MOCQTA        MULT(H)   AMPFCQ        QTSALD                         *
140500870408     C     MOCFAS        IFEQ      'OR'                                         *
140600870413     C  N39SMPQPR        ADD       QTSALD        SMPQPR                         *
140700870413     C   39              Z-ADD     QTSALD        SMPQPR                         *
140800870408     C                   END                                                    *
140900870413     C     MOCFAS        IFEQ      'AS'                                         *
141000870413     C  N39SMPQIM        ADD       QTSALD        SMPQIM                         *
141100870413     C   39              Z-ADD     QTSALD        SMPQIM                         *
141200870413     C                   END                                                    *
141300870413     C     MOCFAS        IFEQ      'BB'                                         *
141400870413     C                   GOTO      ENDSAL
141500870413     C                   END                                                    *
141600880105     C     MOCFAS        IFEQ      'FA'                                         *
141700880105     C                   GOTO      ENDSAL
141800880105     C                   END                                                    *
141900870408     C**--- AGGIORNO SALDI M.P. --------------------------**
142000870408     C                   MOVE      ' '           SMPATT                         *
142100990125     C                   move      xsccut        SMPUTE                         *
142200990125     C                   Z-ADD     woggig        SMPDMN                         *
142300990125     C  N39              UPDATE    tmsmp000                                     *
142400990125     C   39              WRITE     tmsmp000                                     *
142500870413     CSR   ENDSAL        ENDSR                                                  *
142600870408     C***------------------------------------------------***
142700870408     C**--- ROUTINE RICHIAMO DATI TESTATA ----------------**
142800870408     C***------------------------------------------------***
142900870408     CSR   RUTTES        BEGSR                                                  *
143000870408     C                   MOVE      '162'         TABGRU                         *
143100870408     C                   MOVE      *ZEROS        TABCOD                         *
143200870408     C                   MOVE      TESTOR        TABCOD                         *
143300870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
143400870408     C  N46              MOVEL     TABDES        DSORD                          *
143500870408     C  N46              MOVEL     TABDES        TESDTO                         *
143600870408     C   46              MOVE      *BLANKS       TESDTO                         *
143700870408     C                   MOVE      '161'         TABGRU                         *
143800870408     C                   MOVE      *ZEROS        TABCOD                         *
143900870408     C                   MOVE      TESLIS        TABCOD                         *
144000870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
144100870409     C   46              MOVEL     *BLANKS       TESDLI
144200870410     C  N46              MOVEL     TABDES        TESDLI
144300870408RIC  C******************************************************
144400870408RIC  C                   MOVE      'FA'          TIPTAB                         *
144500870408RIC  C                   MOVE      TESCPG        PAGVID                         *
144600870408RIC  C     KEYPAG        CHAIN     TABEL00F                           46        *
144700870408RIC  C  N46              MOVEL     TBLUNI        TESDPG                         *
144800870408RIC  C   46              MOVE      *BLANKS       TESDPG                         *
144900870408RIC  C******************************************************
145000870408     C                   MOVE      '159'         TABGRU                         *
145100870408     C                   MOVE      *ZEROS        TABCOD                         *
145200870408     C                   MOVE      TESGPM        TABCOD                         *
145300870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
145400870408     C  N46              MOVEL     TABDES        TESDGP                         *
145500870408     C   46              MOVE      *BLANKS       TESDGP                         *
145600870408     C                   MOVE      '160'         TABGRU                         *
145700870408     C                   MOVE      *ZEROS        TABCOD                         *
145800870408     C                   MOVE      TESSTM        TABCOD                         *
145900870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
146000870408     C  N46              MOVEL     TABDES        TESDST                         *
146100870408     C   46              MOVE      *BLANKS       TESDST                         *
146200870408     C*----------------------------------------------------*
146300870408     C     TESTA         TAG                                                    *
146400870408     C*          -------------                             *
146500870408     C*--- EMISSIONE PROMPT TESTATA -----------------------*
146600870410     C                   EXFMT     OCO100TS                                     *
146700990125     C   Kl              DO                                                     *
146800870408     C                   GOTO      INIZIO                                       *
146900870408     C                   END                                                    *
147000870409     C                   SETOFF                                       979695
147100870409     C                   SETOFF                                       933031
147200870409     C                   SETOFF                                       343577
147300870409     C                   SETOFF                                       989190
147400870409     C                   SETOFF                                       92
147500870408     C*--- INTERROGAZIONE ANAGRAFICO PARTICOLARITA' -------*
147600870408     C   KB              DO                                                     *
147700870408     C                   MOVEL     KPJBU         KPJBUW                         *
147800870408     C                   MOVE      CODFOR        FOR820                         *
147900870408     C                   MOVE      NOMF30        RAG820                         *
148000870408     C                   MOVE      'R'           RIC820                         *
148100870408     C                   MOVEL     PAR820        KPJBU                          *
148200990317     C                   CALL      'TII007R'                                    *
148300870408     C                   PARM                    KPJBA                          *
148400870408     C                   MOVEL     KPJBUW        KPJBU                          *
148500870408     C                   GOTO      TESTA                                        *
148600870408     C                   END                                                    *
148700870408     C*--- CONTROLLO DATI TESTATA -------------------------*
148800870408     C                   SETOFF                                       30
148900870408     C                   SETOFF                                       347798
149000870408     C                   SETOFF                                       35
149100990125     C     *eur          test(d)                 tesdor                 90      *
149200990125     C   90*dmy          test(d)                 tesdor                 90      *
149300990125     C   90              goto      testa                                        *
149400870408     C*----------------------------------------------------*
149500870408     C     TESTOR        IFEQ      '? '                                         *
149600870408     C                   MOVE      *BLANKS       TESTOR                         *
149700870408     C                   MOVE      *BLANKS       TESDTO                         *
149800990317     C                   CALL      'TII117R'                                    *
149900870408     C                   PARM                    TESTOR                         *
150000870408     C                   PARM                    TESDTO                         *
150100870408     C     TESDTO        COMP      '99'                                   92    *
150200870408     C                   GOTO      TESTA                                        *
150300870408     C                   ELSE                                                   *
150400870408     C                   MOVE      '162'         TABGRU                         *
150500870408     C                   MOVE      *ZEROS        TABCOD                         *
150600870408     C                   MOVE      TESTOR        TABCOD                         *
150700870408     C     KEYANT        CHAIN     ANTAB00F                           92        *
150800870408     C   92              GOTO      TESTA                                        *
150900870408     C                   MOVEL     TABDES        TESDTO                         *
151000870408     C                   MOVEL     TABDES        DESTOR                         *
151100870408     C                   MOVEL     TABDES        DSORD                          *
151200870408     C                   END                                                    *
151300871104     C*---------
151400871104     C                   MOVE      '707'         TABGRU
151500871104     C                   MOVEL     *BLANKS       TABCOD
151600871104     C                   MOVE      TESTRO        TABCOD
151700871104     C     KEYANT        CHAIN     ANTAB00F                           38
151800871104     C   38              GOTO      TESTA
151900871104     C  N38              MOVEL     TABDES        TESDTR
152000870408     C*---------
152100990125     c                   if        tesdcr > 0
152200990125     C     *eur          test(d)                 tesdcr                 91      *
152300990125     C   91*dmy          test(d)                 tesdcr                 91      *
152400990125     C   91              goto      testa                                        *
152500870408     C** DATA CONSEGNA RICHIESTA ERRATA
152600870410     C                   END
152700870408     C*---------
152800870410     C     TESDCP        IFGT      0
152900990125     C     *eur          test(d)                 tesdcp                 98      *
153000990125     C   98*dmy          test(d)                 tesdcp                 98      *
153100990125     C   98              goto      testa                                        *
153200870408     C** DATA CONSEGNA PREVISTA  ERRATA
153300870410     C                   END
153400870408     C*---------
153500870410     C     TESDRF        IFGT      0
153600990125     C     *eur          test(d)                 tesdrf                 34      *
153700990125     C   34*dmy          test(d)                 tesdrf                 34      *
153800990125     C   34              goto      testa                                        *
153900870408     C** DATA RIF.VS.ORDINE
154000870410     C                   END
154100870408     C*---------
154200870408     C     TESDPC        IFNE      0
154300990125     C     *eur          test(d)                 tesdpc                 35      *
154400990125     C   35*dmy          test(d)                 tesdpc                 35      *
154500990125     C   35              goto      testa                                        *
154600870408     C** DATA PROTOCOLLO CLIENTE
154700870408     C                   END
154800870408     C*---------
154900870408     C     TESRES        IFEQ      '?  '                                         *
155000870408     C                   MOVE      *BLANKS       TESRES                         *
155100870408     C                   MOVE      *BLANKS       DESRES           15            *
155200990317     C                   CALL      'TII106R'                                    *
155300870408     C                   PARM                    TESRES                         *
155400870408     C                   PARM                    DESRES                         *
155500870410     C     TESRES        COMP      '99'                                   30    *
155600870408     C                   GOTO      TESTA                                        *
155700870408     C                   ELSE                                                   *
155800870408     C                   MOVE      '019'         TABGRU                         *
155900870408     C                   MOVE      *ZEROS        TABCOD                         *
156000870410     C                   MOVE      TESRES        TABCOD                         *
156100870408     C     KEYANT        CHAIN     ANTAB00F                           30        *
156200870408     C   30              GOTO      TESTA                                        *
156300870408     C                   END                                                    *
156400870408     C*---------                                           *
156500870408     C     TESIMB        IFEQ      '?  '                                        *IMBALLO
156600870408     C                   MOVE      *BLANKS       TESIMB                         *
156700870408     C                   MOVE      *BLANKS       DESIMB           10            *
156800990317     C                   CALL      'TII101R'                                    *
156900870410     C                   PARM                    TESIMB                         *
157000870408     C                   PARM                    DESIMB                         *
157100870410     C     TESIMB        COMP      '99'                                   31    *
157200870408     C                   GOTO      TESTA                                        *
157300870408     C                   ELSE                                                   *
157400870408     C                   MOVE      '008'         TABGRU                         *
157500870408     C                   MOVE      *ZEROS        TABCOD                         *
157600870410     C                   MOVE      TESIMB        TABCOD                         *
157700870408     C     KEYANT        CHAIN     ANTAB00F                           31        *
157800870408     C   31              GOTO      TESTA                                        *
157900870408     C                   END                                                    *
158000871117     C*---------                                           *SPEDIZIONE
158100871117     C     TESSPE        IFEQ      '?  '                                         *
158200970819     C                   MOVEL     KPJBU         WSVKPJ           25 6
158300970819     C                   CLEAR                   DSC0M6
158400970819     C                   MOVEL     'S'           DM6RIC
158500970819     C                   MOVEL     DSC0M6        KPJBU
158600871117     C                   CALL      'CNC0M6R'                                    *
158700871117     C                   PARM                    KPJBA
158800970819     C                   MOVEL     WSVKPJ        KPJBU
158900871117     C                   GOTO      TESTA                                        *
159000871117     C                   END                                                    *
159100871219     C     KSPE          KLIST
159200970801     C                   KFLD                    FLSSPE
159300871219     C                   KFLD                    CLISPE
159400871219     C                   KFLD                    TESSPE
159500871219     C     TESSPE        IFNE      *BLANKS
159600970801     C                   MOVEL     'S'           FLSSPE            1
159700871219     C                   MOVEL     '9999999'     CLISPE            7 0
159800970801     C     KSPE          CHAIN     FNSPE01L                           50
159900871219     C   50              GOTO      TESTA
160000871219     C                   END
160100871219     C** MANCA SPEDIZIONIERE
160200870408     C**---------
160300870408     C     TESLUO        IFEQ      '?  '                                         *
160400970819     C                   MOVEL     KPJBU         WSVKPJ           25 6
160500970819     C                   CLEAR                   DSC0M6
160600970819     C                   MOVEL     'S'           DM6RIC
160700970819     C                   MOVEL     DSC0M6        KPJBU
160800870408     C                   CALL      'CNC0M6R'                                    *
160900870408     C                   PARM                    KPJBA
161000970819     C                   MOVEL     WSVKPJ        KPJBU
161100870408     C                   GOTO      TESTA                                        *
161200870408     C                   END                                                    *
161300871219     C     KLUO          KLIST
161400970801     C                   KFLD                    FLSSPE
161500871219     C                   KFLD                    CLISPE
161600871219     C                   KFLD                    TESLUO
161700871219     C     TESLUO        IFNE      *BLANKS
161800970801     C                   MOVEL     'L'           FLSSPE
161900990125     C                   MOVE      FORNIT        CLISPE
162000970801     C     KLUO          CHAIN     FNSPE03L                           48
162100871219     C   48              GOTO      TESTA
162200871219     C                   END
162300871219     C** LUOGO ERRATO
162400870408     C*----------LISTINO-----------------------------------*
162500870408     C     TESLIS        IFEQ      '?  '                                        *
162600870408     C                   MOVE      *BLANKS       TESLIS                         *
162700870408     C                   MOVE      *BLANKS       DESL75           75            *
162800990317     C                   CALL      'TII116R'                                    *
162900870408     C                   PARM                    TESLIS                         *
163000870408     C                   PARM                    DESL75                         *
163100870408     C                   GOTO      TESTA                                        *
163200870408     C                   END                                                    *
163300870408     C                   MOVE      '161'         TABGRU                         *
163400870408     C                   MOVE      *ZEROS        TABCOD                         *
163500870408     C                   MOVE      TESLIS        TABCOD                         *
163600870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
163700870408     C     *IN(46)       CABEQ     '1'           TESTA                    93    *
163800870408     C*---------                                           * VALUTA
163900990310     C*----------RICERCA valuta  :                         *
164000990310     c                   if        tesval = '?   '
164100990310     C                   MOVEL     '01'          OPZ015
164200990310     C                   MOVEL     *blank        Div015
164300990310     C                   MOVEL     A015DS        KPJBU
164400990310     C                   CALL      'ANA015R'
164500990310     C                   PARM                    KPJBA
164600990310     C                   MOVEL     KPJBU         A015DS
164700990310     C     Div015        IFNE      *BLANK
164800990310     C                   MOVEL     Div015        tesval
164900990310     C                   GOTO      testa                                        *
165000990310     C                   ENDIF
165100990310     C                   ENDIF
165200990310     C     tesval        CABEQ     *BLANKS       testa                    88    *
165300990519     C                   if        not *in88
165400990519     C                   move      tesval        divisa0202
165500990519     C                   z-add     0             Erro              1 0
165600990519     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
165700990519     C                                           ANDVS00F:%size(ANDVS00F) :
165800990519     C                                           ANDVX00F:%size(ANDVX00F))
165900990519     C                   if        Erro= 1 or  Erro= 3
166000990519     C                   seton                                        88
166100990519     C                   end
166200990519     c                   end
166300990519     C*
166400990519     c   88              goto      testa
166500990519     C     TESVAL        IFEQ      xscdiv                                       *
166600990519     C                   Z-ADD     1             TESCMB                         *
166700990519     C                   else
166800990519     C                   reset                   PJX00202DS
166900990519     C                   movel     tesval        divisa0202
167000990519     C                   movel     xscdiv        divmc0202
167100990519     C                   z-add     1000          impord0202
167200990519     C                   movel     *on           Gesmsg
167300990519     C                   movel     *blanks       Esito
167400990519     C                   movel     *on           Noctrlscos
167500990519     C                   callp     x0202testc(PJX00202DS:
167600990519     C                                  %size(PJX00202DS):
167700990519     C                                  *off : Esito : Noctrlscos :
167800990519     C                                  Datigest :
167900990519     C                                  %size(ANDVS00F):
168000990519     C                                  %size(ANDVX00F):
168100990519     C                                  ANDVS00F : ANDVX00F )
168200990519     C                   If        Esito = '0'
168300990519     c                   z-add     cambio0202    tescmb
168400990519     c                   end
168500990310      *imposta cambio
168600990310RIC  C                   END                                                    *
168700870914     C** SE ESISTE N.PROTOCOLLO
168800870914     C** IL CONTO IVA SE ESISTE DEVE ESSERE ESENTE
168900870914     C**************************************************************
169000870914     C     TESNPC        IFGT      0
169100870914     C     TESDPC        COMP      0                                      32
169200870914     C   32              GOTO      TESTA
169300870914     C     TESNPN        COMP      0                                      32
169400870914     C   32              GOTO      TESTA
169500870914     C                   END
169600870914     C     TESDPC        IFGT      0
169700870914     C     TESNPC        COMP      0                                      32
169800870914     C   32              GOTO      TESTA
169900870914     C     TESNPN        COMP      0                                      32
170000870914     C   32              GOTO      TESTA
170100870914     C                   END
170200870914     C     TESNPN        IFGT      0
170300870914     C     TESNPC        COMP      0                                      32
170400870914     C   32              GOTO      TESTA
170500870914     C     TESDPC        COMP      0                                      32
170600870914     C   32              GOTO      TESTA
170700870914     C                   END
170800870914     C** N.PROTOCOLLO-DATA PROT.-N.PROT.INTERNO
170900870914     C** DEBBONO COESISTERE
171000870408     C*----------PAGAMENTO---------------------------------*
171100870408     C     TESCPG        IFEQ      '?  '                                        *
171200870408     C                   MOVE      *ZEROS        TESCPG                         *
171300870408     C                   MOVE      *BLANKS       TESDPG                         *
171400990317     C                   CALL      'TII105R'                                    *
171500870408     C                   PARM                    TESCPG                         *
171600870408     C                   PARM                    TESDPG                         *
171700870408     C                   GOTO      TESTA                                        *
171800870408     C                   ELSE                                                   *
171900870408     C     TESCPG        CABEQ     '999'         TESTA                    95    *
172000870408     C*----------------------------------------------------*
172100870408RIC  C                   MOVE      'FA'          TIPTAB                         *
172200870408RIC  C                   MOVE      TESCPG        PAGVID                         *
172300870408RIC  C     KEYPAG        CHAIN     TABEL00F                           46        *
172400870408RIC  C  N46              MOVEL     TBLUNI        TESDPG                         *
172500870408RIC  C     *IN(46)       CABEQ     '1'           TESTA                    95    *
172600870408RIC  C                   END                                                    *
172700870408ADB  C******************************************************
172800870408     C     TESGPM        IFEQ      '? '                                         *
172900870408     C                   MOVE      *ZEROS        TESGPM                         *
173000870408     C                   MOVE      *BLANKS       TESDGP                         *
173100990317     C                   CALL      'TII114R'                                    *
173200870408     C                   PARM                    TESGPM                         *
173300870408     C                   PARM                    TESDGP                         *
173400870408     C                   GOTO      TESTA                                        *
173500870408     C                   ELSE                                                   *
173600870408     C                   MOVE      '159'         TABGRU                         *
173700870408     C                   MOVE      *ZEROS        TABCOD                         *
173800870408     C                   MOVE      TESGPM        TABCOD                         *
173900870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
174000870408     C  N46              MOVEL     TABDES        TESDGP                         *
174100870408     C     *IN(46)       CABEQ     '1'           TESTA                    96    *
174200870408     C                   END                                                    *
174300870408     C*---------                                           * SETTORE MERC.
174400870408     C     TESSTM        IFEQ      '? '                                         *
174500870408     C                   MOVE      *ZEROS        TESSTM                         *
174600870408     C                   MOVE      *BLANKS       TESDST                         *
174700990317     C                   CALL      'TII115R'                                    *
174800870408     C                   PARM                    TESSTM                         *
174900870408     C                   PARM                    TESDST                         *
175000870408     C                   GOTO      TESTA                                        *
175100870408     C                   ELSE                                                   *
175200870408     C                   MOVE      '160'         TABGRU                         *
175300870408     C                   MOVE      *ZEROS        TABCOD                         *
175400870408     C                   MOVE      TESSTM        TABCOD                         *
175500870408     C     KEYANT        CHAIN     ANTAB00F                           46        *
175600870408     C  N46              MOVEL     TABDES        TESDST                         *
175700870408     C     *IN(46)       CABEQ     '1'           TESTA                    97    *
175800870408     C                   END                                                    *
175900870408     C*---------                                           *
176000870409     C                   SETOFF                                       979695
176100870409     C                   SETOFF                                       933031
176200870409     C                   SETOFF                                       343577
176300870409     C                   SETOFF                                       989190
176400870409     C                   SETOFF                                       92
176500870408     C*----------------------------------------------------*
176600870408     C*--- AGGIORNAMENTO RIGHE ----------------------------*
176700870408     C   KG              DO                                                     *
176800990125     C     KEYORD        SETLL     tmmoc01L                           47  46    *
176900870408     C   47                                                                     *
177000870408     CORN46              GOTO      NORIGA                                       *
177100870408     C                   DO        *HIVAL                                       *
177200990125     C     KEYORD        READE     tmmoc01L                               46    *
177300870408     C   46              GOTO      NORIGA                                       *
177400870408     C*---------                                           *
177500870408     C     MOCFAS        IFNE      'FA'                                         *
177600990125     c                   if        tesdor > 0
177700990125     C                   move      TESDOR        DATAeur                        *
177800990125     C                   move      dataeur       dataiso                        *
177900990125     C                   move      DATAIso       MOCDOR                         *
178000990125     c                   end
178100870408     C                   MOVE      TESGPM        MOCGPM                         *
178200870408     C                   MOVE      TESSTM        MOCSTM                         *
178300990125     c                   if        tesdcp > 0
178400990125     C                   move      TESDcp        DATAeur                        *
178500990125     C                   move      dataeur       dataiso                        *
178600990125     C                   move      DATAIso       MOCDcp                         *
178700990125     c                   end
178800990125     c                   if        tesdcr > 0
178900990125     C                   move      TESDcr        DATAeur                        *
179000990125     C                   move      dataeur       dataiso                        *
179100990125     C                   move      DATAIso       MOCDcr                         *
179200990125     c                   end
179300870408     C                   MOVE      TESTOR        MOCTOR                         *
179400870408     C                   MOVE      TESLIS        MOCLIS                         *
179500870408     C                   MOVE      TESVAL        MOCVAL                         *
179600870408     C                   Z-ADD     TESCMB        MOCCMB                         *
179700870408     C                   MOVEL     TESCPG        MOCCPG                         *
179800870408     C                   Z-ADD     TESSC3        MOCSC3
179900870408     C                   MOVEL     TESPCL        MOCPCL
180000870408     C                   MOVEL     TESTRO        MOCTRO
180100870408     C                   MOVEL     TESSOS        MOCSOS
180200870408     C                   MOVEL     TESRES        MOCRES
180300870408     C                   MOVEL     TESIMB        MOCIMB
180400870408     C                   MOVEL     TESSPE        MOCSPE
180500870408     C                   MOVEL     TESLUO        MOCLUO
180600870408     C                   Z-ADD     TESNRF        MOCNRF
180700990125     c                   if        tesdrf > 0
180800990125     C                   move      TESDrf        DATAeur                        *
180900990125     C                   move      dataeur       dataiso                        *
181000990125     C                   move      DATAIso       MOCDrf                         *
181100990125     c                   end
181200870408     C                   Z-ADD     TESNPC        MOCNPC
181300870408     C                   Z-ADD     TESNPN        MOCNPN
181400990125     c                   if        tesdpc > 0
181500990125     C                   move      TESDpc        DATAeur                        *
181600990125     C                   move      dataeur       dataiso                        *
181700990125     C                   move      DATAIso       MOCDpc                         *
181800990125     c                   end
181900870408     C                   UPDATE    ORDRIG                                       *
182000870408     C                   END                                                    *
182100870408     C                   END                                                    *
182200870408     C*----------                                          *
182300870408     C     NORIGA        TAG                                                    *
182400870408     C                   SETOFF                                       4647      *
182500870410     C                   END                                                    *
182600870408     CSR                 ENDSR                                                  *
182700990125     C*----------------------------------------------------*
182800990125     C* Reperimento dati società
182900990125     C*----------------------------------------------------*
183000990125     C     REPSOC        BEGSR
183100990125     C*
183200990125     C                   CALLB     'XSOC'
183300990125     C                   PARM                    TIPXSC            6
183400990125     C                   PARM                    SOCXSC            3
183500990125     C                   PARM                    CDSXSC            9 0
183600990125     C                   PARM                    MODXSC            3
183700990125     C                   PARM      *blanks       RTNXSC            1
183800990125     C                   PARM                    XSOCDS
183900990125     C                   PARM                    KPJBA
184000990125     C*
184100990125     C                   ENDSR
184200870408     O*--- RILASCIO RIGA ORDINI ----*
184300870408     OORDFIS    E            RILORD
184400910925**   ORD
184500870408*-ORDINE NUOVO*
184600870408*-MANCA RECORD FISICO-*
184700870408*-MANCA RECORD VIDEO -*
184800870408**     ERR                              * ERRORE
184900870408VARIAZIONE TIPO RIGA NON AMMESSO      -*  ERR,1
185000870408CODICE ARTICOLO ERRATO                -*  ERR,2
185100870408VERSIONE ARTICOLO ERRATA              -*  ERR,3
185200870408UNITA' DI MISURA ERRATA               -*  ERR,4
185300870408DIGITARE QUANTITA'                    -*  ERR,5
185400870408VARIAZIONE NON AMMESSA IN QUESTA FASE -*  ERR,6
185500870408CODICE LISTINO ERRATO                 -*  ERR,7
185600870408CODICE VALUTA ERRATA                  -*  ERR,8
185700870408DIGITARE PREZZO                       -*  ERR,9
185800870408CODICE CONDIZIONI DI PAGAMENTO ERRATO -*  ERR,10
185900870408CODICE MAGAZZINO ERRATO               -*  ERR,11
186000870408CODICE IMBALLO ERRATO                 -*  ERR,12
186100870408CODICE CENTRO DI COSTO ERRATO         -*  ERR,13
186200870408NUMERO LOTTO/COMMESSA OBBLIGATORIO    -*  ERR,14
186300870408DATA DI CONSEGNA PREVISTA ERRATA      -*  ERR,15
186400871105MANCA DISPONIBILITA' MAG.-->          -*  ERR,16
186500870914CONTO IVA NON VALIDO O INESISTENTE    -*  ERR,17
186600870914CONTROPARTITA INESISTENTE             -*  ERR,18
186700910925TIPO BOLLA NON CONGRUENTE CON CLIENTE -*  ERR,19
186800910925MATRICOLA ERRATA PER ATTREZZATURA     -*  ERR,20
