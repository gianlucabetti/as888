000100990205     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990205     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870105     F* OFN80AR *-----------------------------------------------*
000400870105     F*-----------------*                                       *
000500870105     F*           - STAMPA ORDINI FORNITORI -                   *
000600870105     F*           ---------------------------                   *
000700870105     F*                                                         *
000800990205     F*         FILE : tmmof01L - tmfop00F - CNACO00F -         *
000900870105     F*                CNIND00F - ANTAB00F - TABEL00F -         *
001000870131     F*****            BFTAB01L - ANFOR01L                      *
001100870105     F*                                                         *
001200870105     F*         PGM. : RIC001R  - RICERCA DITTA                 *
001300870105     F*                RIC004R  - BLOCCO/SBLOCCO FORNITORE      *
001400870105     F*                                                         *
001500870105     F*---------------------------------------------------------*
001600990205     Ftmmof01L  UF   E           K DISK
001700990205     Ftmfop01L  IF   E           K DISK
001800870105     FANTAB00F  IF   E           K DISK
002000990205RIC  Fanfrn01l  IF   E           K DISK
002100990205RIC  Fansog01l  IF   E           K DISK
002200990205RIC  Fanind01l  IF   E           K DISK
002300870105RIC  FTABEL00F  IF   E           K DISK
002301110503     FTntbe01l  IF   E           K DISK
002302110104     FAZORG01L  IF   E           K DISK
002303110105     Ftii404p   O    e             PRINTER infds(prtds)
002304110104     D*---------------------------------------------------------*
002305110104     D ER1             S             66    DIM(2) CTDATA PERRCD(1)              * NESSUNA RIGA
002306110104     D ERR             S              1    DIM(80) CTDATA PERRCD(80)            * ORDINE BLOCCATO
003700870105     D*---------------------------------------------------------*
003800990205     D PARSTA          DS
003900990205     D*--- PARAMETRI PER STAMPA ORDINE :
004000990205     D  codfor                 1      8
004100990205     D  NUMord                 9     13  0
004200990205     D  Tipsta                14     14
004300990205     D  stfabb                15     16
004400990205     D  socpas                17     19
004500870105     D KPJBA         E DS
004600870105     D*--- DATI DA PROGRAMMA DI LANCIO
004700870105     D  EDPUTE                 1      3
004800870105     D DESDEM        E DS                  EXTNAME(ANTAB028)
004801110503     D dvpobrt       e DS
004900870105     D*--- TABELLA DESTINAZIONE MERCE
005000990205     D soc001        E DS                  EXTNAME(xsoc001ds)
005100990205     D xsocds          DS          1000
005200990205     d dataiso         s               d   datfmt(*iso)
005300990205     d dataeur         s               d   datfmt(*eur)
005301110105     d prtds           ds
005302110105     D ODP_TYPE               81     82                                         * ODP Type
005303110105     D FILE_NAME              83     92                                         * File name
005304110105     D LIBRARY                93    102                                         * Library name
005305110105     D SPOOL_FILE            103    112                                         * Spool file name
005306110105     D SPOOL_LIB             113    122                                         * Spool file lib
005307110105     D SPOOL_NUM             123    124I 0                                      * Spool file num
005308110105     D RCD_LEN               125    126I 0                                      * Max record len
005309110105     D KEY_LEN               127    128I 0                                      * Max key len
005310110105     D MEMBER                129    138                                         * Member name
005311110105     D TYPE                  147    148I 0                                      * File type
005312110105     D ROWS                  152    153I 0                                      * Num PRT/DSP rows
005313110105     D COLUMNS               154    155I 0                                      * Num PRT/DSP cols
005314110105     D NUM_RCDS              156    159I 0                                      * Num of records
005315110105     D ACC_TYPE              160    161                                         * Access type
005316110105     D DUP_KEY               162    162                                         * Duplicate key?
005317110105     D SRC_FILE              163    163                                         * Source file?
005318110105     D VOL_OFF               184    185I 0                                      * Vol label offset
005319110105     D BLK_RCDS              186    187I 0                                      * Max rcds in blk
005320110105      *
005321110105     D OVERFLOW              188    189I 0                                      * Overflow line
005322110105      *
005323110105     D BLK_INCR              190    191I 0                                      * Blk increment
005324110105     D FLAGS1                196    196                                         * Misc flags
005325110105     D REQUESTER             197    206                                         * Requester name
005326110105     D OPEN_COUNT            207    208I 0                                      * Open count
005327110105     D BASED_MBRS            211    212I 0                                      * Num based mbrs
005328110105     D FLAGS2                213    213                                         * Misc flags
005329110105     D OPEN_ID               214    215                                         * Open identifier
005330110105     D RCDFMT_LEN            216    217I 0                                      * Max rcd fmt len
005331110105     D CCSID                 218    219I 0                                      * Database CCSID
005332110105     D FLAGS3                220    220                                         * Misc flags
005333110105     D NUM_DEVS              227    228I 0                                      * Num devs defined
005334110105      *
005335110105     D line                  367    368I 0                                      * Current line num
005336110105     D CUR_PAGE              369    372I 0                                      * Current page cnt
005400870105     C*----------------------------------------------------*
005500870105     C     *ENTRY        PLIST                                                  *
005600870105     C                   PARM                    KPJBA                          *
005700990205     c                   movel     kpjbu         parsta
005701110105     c                   z-add     99            line
005702110503     c                   time                    como14           14 0
005703110503     c                   move      como14        woggi             8 0
005704110503     c                   move      woggi         dataeur
005705110503     c                   move      dataeur       dataiso
005706110503     c                   move      dataiso       woggig            8 0
005707110503     c                   move      'VPO'         tbecod
005708110503     c                   movel     'BRT'         tbeke1
005710110503     c     ktbe          chain     tntbe01l
005711110503     c                   if        %found(tntbe01l)
005712110503     c                   movel     tbeuni        dvpobrt
005713110503     c                   if        woggig <= §VPOBRTDTA
005714110503     c                   seton                                        30
005715110503     c                   endif
005716110503     c                   endif
005717110503     C     ktbe          klist
005718110503     C                   kfld                    tbecod
005719110503     C                   kfld                    tbeke1
005800990205     C*--- FILE ORDINI "tmmof01L" :                        *
005900870105     C     KEYORD        KLIST                                                  *
006000870105     C                   KFLD                    CODFOR                         *
006100870105     C                   KFLD                    NUMORD                         *
006200870105     C*--- KEY PER POSIZIONAMENTO X ERRORE                 *
006300870105     C     KEYGT         KLIST                                                  *
006400870105     C                   KFLD                    MOFFOR                         *
006500870105     C                   KFLD                    MOFNOR                         *
006600870105RIC  C******************************************************
006700870105RIC  C*--- FILES ANAGRAFICI FORNIT. "CNACO00F/CNIND00F"    *
006800870105RIC  C     KEYFOR        KLIST                                                  *
006900990205RIC  C                   KFLD                    wsoc                           *
007000990205RIC  C                   KFLD                    staFOR                         *
007100870105RIC  C*--- KEY RICERCA PAGAMENTI SU "TABEL00F"             *
007200870105RIC  C     KEYPAG        KLIST                                                  *
007300990205RIC  C                   KFLD                    CODUTE            1 0          *
007400870105RIC  C                   KFLD                    TIPTAB            2            *
007500990205RIC  C                   KFLD                    KEYTBL            8            *
007600990205     c                   z-add     1             codute
007700870105     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
007800870105     C     KEYANT        KLIST                                                  *
007900870105     C                   KFLD                    TABGRU                         *
008000870105     C                   KFLD                    TABCOD                         *
008100870105     C*--- RICERCA DESCRIZIONE DITTA ----------------------*
008200990205     C                   MOVEL     'SOC001'      TIPXSC
008300990205     C                   MOVEL     *blank        SOCXSC
008400990205     C                   EXSR      REPSOC
008500990205     C     RTNXSC        IFNE      '1'
008600990205     C                   MOVEL     XSOCDS        SOC001
008700990205     C                   MOVEL     xscrgs        desdit           20
008800990205     C                   MOVEL     xscsoc        wsoc              3
008900990205     c                   end
009100870105     C*--- CONTROLLO SE SCELTA STAMPA SOLO RIGHE AGGIUNTE  *
009200870105     C     TIPSTA        COMP      'A'                                    05    *
009300870105     C*--- RICHIESTO STAMPA TUTTI GLI ORDINI --------------*
009400990205     C     CODFOR        IFEQ      '99999999'                                   *
009500870105     C                   DO        *HIVAL                                       *
009600990205     C                   READ      tmmof000                               01    *
009601110105     c                   if        totlor > 0
009700110104     C   01              write     piede
009701110105     C   01              write     piede2
009702110105     c                   endif
009800870105     C   01              GOTO      FINE                                         *
009900870105     C                   EXSR      ORDINE                                       *
010000870105     C                   END                                                    *
010100870105     C                   END                                                    *
010200870105     C*--- RICHIESTO STAMPA TUTTI GLI ORDINI DI UN FORNIT.-*
010300870105     C     NUMORD        IFEQ      99999                                        *
010400990205     C     CODFOR        SETLL     tmmof000                                     *
010500870105     C                   DO        *HIVAL                                       *
010600990205     C     CODFOR        READE     tmmof000                               01    *
010601110105     c                   if        totlor > 0
010700110104     C   01              write     piede
010701110105     C   01              write     piede2
010702110105     c                   endif
010800870105     C   01              GOTO      FINE                                         *
010900870105     C                   EXSR      ORDINE                                       *
011000870105     C                   END                                                    *
011100870105     C                   ELSE                                                   *
011200870105     C*--- RICHIESTO STAMPA UN SOLO ORDINE : --------------*
011300990205     C     KEYORD        SETLL     tmmof000                                     *
011400870105     C                   DO        *HIVAL                                       *
011500990205     C     KEYORD        READE     tmmof000                               01    *
011501110105     c                   if        totlor > 0
011600110104     C   01              write     piede
011601110105     C   01              write     piede2
011602110105     c                   endif
011700870105     C   01              GOTO      FINE                                         *
011800870105     C                   EXSR      ORDINE                                       *
011900870105     C                   END                                                    *
012000870105     C                   END                                                    *
012100870105     C*--- CHIUSURA ULTIMO ORDINE -------------------------*
012200870105     C     FINE          TAG                                                    *
012300870105     C   10              DO                                                     *
012400110104     C*MM                EXCEPT    RIGFIN                                       *
012500870105     C                   END                                                    *
012600870105     C                   SETON                                            LR    *
012700870105     C*--- ROUTINE EMISSIONE ORDINI -----------------------*
012800870105     C***------------------------------------------------***
012900870105     CSR   ORDINE        BEGSR                                                  *
013000870105     C*--- ACQUISIZIONE DATI ANAGRAFICI FORNITORE :        *
013100870105     C     MOFNOR        IFNE      STANOR                                       *
013102110105     C                   IF        STANOR <> 0 and totlor > 0
013200110104     C                   write     piede
013201110105     C                   write     piede2
013202110104     C                   ENDIF
013400871126     C                   Z-ADD     0             TOTGEN
013500880502     C                   Z-ADD     0             TOTLOR
013600870105     C*---                                                 *
013900870105     C   10              DO                                                     *
014000110104     C*MM                EXCEPT    RIGFIN                                       *
014100870105     C                   END                                                    *
014200870105     C                   SETON                                        10        *
014300870105     C                   SETON                                        07        * NO AGGIUNTE
014400870105     C                   Z-ADD     MOFNOR        STANOR            5 0          *
014500990205     C                   MOVE      MOFFOR        STAFOR            8            *
014600990205     C     MOFFOR        CHAIN     tmfop01L                           01        *
014700990205RIC  C  N01KEYFOR        CHAIN     anfrn01l                           01        *
014800990205     c  n01frnsogg       chain     ansog01l                           01
014900990205RIC  C  N01frnsogg       CHAIN     anind01l                           01        *
015000870105RIC  C  N01              DO                                                     *
015100110104RIC  C                   MOVEL     sogdes        STARAG
015200990205RIC  C                   MOVEL     sogpartiva    STAIVA           16            *
015300990205RIC  C                   MOVEl     INDindriz     STAVIA           30            *
015400990205RIC  C                   MOVEl     INDCAP        STACAP            5            *
015500990205RIC  C                   MOVEl     INDlocalit    STACIT           20            *
015600990205RIC  C                   MOVEl     INDPRov       STAPRV            2            *
015700990205RIC  C                   MOVEl     INDSTAto      STASTA            4            *
015800990205RIC  C                   MOVEl     INDTELefon    STATEL           16            *
015900990205RIC  C                   MOVEl     INDTelex      STATLX           16            *
016000870105RIC  C                   END                                                    *
016100870105RIC  C******************************************************
016200870105     C*----------------------------------------------------*
016300870105     C   01              DO                                                     *
016400870105     C                   MOVE      *ALL'*'       STARAG                         *
016500870105     C                   MOVE      *ALL'*'       STAIVA                         *
016600870105     C                   MOVE      *ALL'*'       STAVIA                         *
016700870105     C                   MOVE      *ALL'*'       STACAP                         *
016800870105     C                   MOVE      *ALL'*'       STACIT                         *
016900870105     C                   MOVE      *ALL'*'       STAPRV                         *
017000870105     C                   MOVE      *ALL'*'       STASTA                         *
017100870105     C                   MOVE      *ALL'*'       STATEL                         *
017200870105     C                   MOVE      *ALL'*'       STATLX                         *
017300870105     C                   END                                                    *
017400870105     C*--- ACQUISIZIONE DATI TESTATA ORDINE :              * DATA ORDINE
017500990205     C                   MOVE      MOFDOR        DATAiso                        *
017600990205     C                   move      dataiso       dataeur                        *
017700990205     C                   MOVE      DATAeur       STADOR            8 0          *
017800870105     C*---                                                 * TIPO ORDINE
017900870105     C                   MOVE      '162'         TABGRU                         *
018000870105     C                   MOVE      *ZEROS        TABCOD                         *
018100870105     C                   MOVE      MOFTOR        TABCOD                         *
018200870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
018300870105     C  N46              MOVEL     TABDES        TESDTO           25            *
018400870105     C   46              MOVE      *BLANKS       TESDTO                         *
018500870105     C                   MOVE      MOFTOR        TESTOR            2            *
018600870105     C*---                                                 * GRUPPO MERC.
018700870105     C                   MOVE      '159'         TABGRU                         *
018800870105     C                   MOVE      *ZEROS        TABCOD                         *
018900870105     C                   MOVE      MOFGPM        TABCOD                         *
019000870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
019100870105     C  N46              MOVEL     TABDES        TESDGP           25            *
019200870105     C   46              MOVE      *BLANKS       TESDGP                         *
019300870105     C                   MOVE      MOFGPM        TESGPM            2            *
019400870105     C*---                                                 * SETTORE MERC.
019500870105     C                   MOVE      '160'         TABGRU                         *
019600870105     C                   MOVE      *ZEROS        TABCOD                         *
019700870105     C                   MOVE      MOFSTM        TABCOD                         *
019800870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
019900870105     C  N46              MOVEL     TABDES        TESDST           25            *
020000870105     C   46              MOVE      *BLANKS       TESDST                         *
020100870105     C                   MOVE      MOFSTM        TESSTM            2            *
020200870105     C*---                                                 * LISTINO
020300870105     C                   MOVE      '161'         TABGRU                         *
020400870105     C                   MOVE      *ZEROS        TABCOD                         *
020500870105     C                   MOVE      MOFLIS        TABCOD                         *
020600870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
020700870105     C  N46              MOVEL     TABDES        TESDLI           25            *
020800870105     C   46              MOVE      *BLANKS       TESDLI                         *
020900870105     C                   MOVE      MOFLIS        TESLIS            3            *
021000870105     C*----------------------------------------------------*
021100870105     C*---                                                 * VALUTA
021200870105     C                   MOVE      MOFVAL        TESVAL            4            *
021300870105     C*---                                                 * CAMBIO
021400870105     C                   MOVE      MOFCMB        TESCMB            7 2          *
021500870105     C*---                                                 * COND.CONSEGNA
021600870105     C                   MOVE      MOFCCO        TESCCO           30            *
021700870105     C*---                                                 * DESTIN.MERCE
021800870105     C                   MOVE      '028'         TABGRU                         *
021900870105     C                   MOVE      *ZEROS        TABCOD                         *
022000110112     C                   MOVE      '102'         TABCOD                         *
022100870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
022200870105     C                   MOVE      MOFDEM        TESDEM            3            *
022300870105     C  N46              DO                                                     *
022400870105     C                   MOVEL     TABDES        DESDEM                         *
022500870105     C                   MOVEL     IND028        TESIND           25            *
022600870105     C                   MOVEL     LOC028        TESLOC           25            *
022700870105     C                   END                                                    *
022800870105     C   46              DO                                                     *
022900870105     C                   MOVE      *BLANKS       TESIND                         *
023000870105     C                   MOVE      *BLANKS       TESLOC                         *
023100870105     C                   END                                                    *
023200870105RIC  C                   MOVE      'FA'          TIPTAB                         *
023300990205RIC  C                   MOVEL     MOFCPG        TESCPG            3            *
023400870105RIC  C     KEYPAG        CHAIN     TABEL00F                           46        *
023500870105RIC  C  N46              MOVEL     TBLUNI        TESDPG                         *
023600870105     C   46              MOVE      *BLANKS       TESDPG           30            *
023700870105     C                   END                                                    *
023800870105     C*---------- ESCLUSIONE FASI BB,FA -------------------*
023900870105     C     STFABB        IFEQ      'NO'                                         *
024000870105     C     MOFFAS        CABEQ     'FA'          SALDET               0606      *
024100870105     C     MOFFAS        CABEQ     'BB'          SALDET               0606      *
024200870105     C                   END                                                    *
024300870105     C*---------- ESCLUSIONE RIGHE GIA STAMPATE -----------*
024400870105     C   05MOFFES        CABEQ     'S'           SALDET               0606      *
024500870105     C*--- PREPARO CAMPI X STAMPA DETTAGLIO : -------------*
024600870105     C                   SETON                                        06        * STAMP.DETT.
024700870105     C                   SETOFF                                       07        * SI AGGIUNTE
024800871007     C                   MOVE      MOFMAT        STAMAT           12            *
024900110104     C                   MOVEL     MOFDMT        STADMT
025000110104     C                   MOVEL     MOFUMQ        STAUMQ
025100870105     C                   Z-ADD     MOFQTA        STAQTA            9 2          *
025200870105     C                   MOVE      MOFFAS        STAFAS            2            *
025300990205     C                   MOVE      MOFDCN        DATAIso                        *
025400990205     C                   move      dataiso       dataeur                        *
025500990205     C                   MOVE      DATAeur       STADCN            8 0          *
025600110104     C                   MOVE      MOFCOM        STACOM
025700870105     C                   MOVE      MOFVAL        STAVAL            4            *
025800990205     C                   Z-ADD     MOFPRZ        STAPRZ           14 5          *
025900870105     C                   Z-ADD     MOFSC1        STASC1            2 0          *
026000870105     C                   Z-ADD     MOFSC2        STASC2            2 0          *
026100870131     C                   Z-ADD     MOFSC3        STASC3            2 0          *
026200870105     C                   MOVEL     MOFCPG        STACPG            3            *
026300870105     C                   MOVE      MOFMAG        STAMAG            7            *
026400990205     C                   MOVE      MOFCDC        STACDC            8            *
026401110104     C                   SETOFF                                       98
026402110118     C                   IF        MOFDEM <> '102'
026404110104     C                   MOVE      '028'         TABGRU                         *
026405110104     C                   MOVE      *ZEROS        TABCOD                         *
026406110104     C                   MOVE      MOFDEM        TABCOD                         *
026407110104     C     KEYANT        CHAIN     ANTAB00F                           46        *
026408110104     C                   IF        %FOUND(ANTAB00F)                             *
026409110104     C                   CLEAR                   WMOFDEM
026410110104     C                   MOVE      MOFDEM        WMOFDEM           3 0
026411110104     C     WMOFDEM       CHAIN     AZORG01L
026412110104     C                   IF        %FOUND(AZORG01L)                             *
026413110104     C                   SETON                                        98
026414110104     C                   ENDIF
026415110104     C                   ENDIF
026416110104     C                   ENDIF
026500871126     C*
026600880502     C                   Z-ADD     0             QTALOR
026700110104     C     STAPRZ        MULT      MOFQTA        QTALOR           14 3
026800110104     C                   ADD       QTALOR        TOTLOR
026900880502     C*
027000990205     C     STAPRZ        MULT      MOFSC1        SCONTO           14 5          *
027100871126     C     SCONTO        DIV       100           SCONTO                         *
027200990205     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
027300871126     C*
027400871126     C     STAPRZ        MULT      MOFSC2        SCONTO
027500871126     C     SCONTO        DIV       100           SCONTO                         *
027600871126     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
027700871126     C*
027800871126     C     STAPRZ        MULT      MOFSC3        SCONTO
027900871126     C     SCONTO        DIV       100           SCONTO                         *
028000871126     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
028100871126     C                   Z-ADD     0             TOTPRZ
028200990205     C     STAPRZ        MULT      MOFQTA        TOTPRZ           14 5          *
028300990205     C                   ADD       TOTPRZ        TOTGEN           14 5
028400870105     C*--- PREPARAZIONE CAMPI STAMPA IN FUNZIONE DEI TIPI  *
028500870105     C*--- RIGA DEGLI ORDINI :                             *
028600870105     C     MOFTRG        CABEQ     '1'           NONCOD                         *
028700870105     C     MOFTRG        CABEQ     '2'           NONCOD                         *
028800870105     C     MOFTRG        CABEQ     '3'           NONCOD                         *
028900870105     C                   MOVE      *ZEROS        STADCN                         *
029000870105     C                   Z-ADD     0             STASC1                         *
029100870105     C                   Z-ADD     0             STASC2                         *
029200870105     C     MOFTRG        IFEQ      'D'                                          *
029300870105     C                   Z-SUB     MOFPRZ        STAPRZ                         *
029400870105     C                   END                                                    *
029500870105     C     MOFTRG        CABLT     '6'           NONCOD                         *
029600870105     C     MOFTRG        CABGT     '9'           NONCOD                         *
029700870105     C                   MOVE      *BLANKS       STAUMQ                         *
029800870105     C                   Z-ADD     0             STAQTA                         *
029900870105     C                   MOVE      *BLANKS       STAFAS                         *
030000870105     C                   MOVE      *BLANKS       STAVAL                         *
030100870105     C                   MOVE      *BLANKS       STACPG                         *
030200870105     C                   MOVE      *BLANKS       STAMAG                         *
030300870105     C                   MOVE      *BLANKS       STACDC                         *
030400870105     C                   Z-ADD     0             STAPRZ                         *
030500870105     C*----------------------------------------------------*
030600870105     C     NONCOD        TAG                                                    *
030700870105     C     STAMAT        COMP      *BLANKS                            8080      *
030800870105     C     STAQTA        COMP      0                                  8181      *
030900870105     C     STAPRZ        COMP      0                                  8282      *
031000870105     C     STADCN        COMP      0                                  8383      *
031100870105     C     STASC2        COMP      0                                  8484      *
031200870131     C     STASC3        COMP      0                                  8585      *
031300870105     C     SALDET        TAG                                                    *
031400870105     C                   EXSR      RUTSTA                                       *
031500870105     C*--- AGGIORNO FLAG DI RIGA STAMPATA SU ORDINI :      *
031600870105     C                   MOVE      'S'           MOFFES                         *
031700870105     C                   EXCEPT    ORDINI                                       *
031800870105     CSR   FINEOR        ENDSR                                                  *
031900870105     C*--- ROUTINE EMISSIONE STAMPA -----------------------*
032000870105     C***------------------------------------------------***
032100870105     CSR   RUTSTA        BEGSR                                                  *
032101110105      *verifico overflow a seconda che debba stampare la destinazione particolare
032102110112     c                   if        mofdem = '102'
032300110110     C                   IF        line > 41 and *in06                          *
032400110104     C                   WRITE     TESTATA                                      *
032401110503     C   30              WRITE     COSTANTI                                     *
032402110503     C  n30              WRITE     COSTANT2                                     *
032500870105     C                   END                                                    *
032501110105     c                   else
032502110105     C                   IF        line > 39 and *in06                          *
032503110105     C                   WRITE     TESTATA                                      *
032504110503     C   30              WRITE     COSTANTI                                     *
032505110503     C  n30              WRITE     COSTANT2                                     *
032506110105     C                   END                                                    *
032507110105     c                   endif
032900110104     C   06              write     dettaglio                                    *
032901110105     c   06
032902110105     Can 98              write     DESPART                                      *
033000870105     CSR                 ENDSR                                                  *
033100990205     C*----------------------------------------------------*
033200990205     C* Reperimento dati società
033300990205     C*----------------------------------------------------*
033400990205     C     REPSOC        BEGSR
033500990205     C*
033600990205     C                   CALLB     'XSOC'
033700990205     C                   PARM                    TIPXSC            6
033800990205     C                   PARM      socpas        SOCXSC            3
033900990205     C                   PARM                    CDSXSC            9 0
034000990205     C                   PARM                    MODXSC            3
034100990205     C                   PARM      *blanks       RTNXSC            1
034200990205     C                   PARM                    XSOCDS
034300990205     C                   PARM                    KPJBA
034400990205     C*
034500990205     C                   ENDSR
034501110104      *----------------------------------------------------------------
050900990205     Otmmof000  E            ORDINI
051000870105     O                       MOFFES
051100870105     O*
053900870105**   ER1
054000870105  *** ATTENZIONE. Non vi sono RIGHE AGGIUNTE da stampare in questo  ER1
054100870105 ORDINE ***                                                         ER1
054200870105**   ERR
054300870105Ordine N° xxxxx del Fornitore xxxxxxx non stampato perche' in uso !
