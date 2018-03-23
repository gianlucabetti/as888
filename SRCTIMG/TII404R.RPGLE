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
001900990205RIC  Fanfrn01l  IF   E           K DISK
002000990205RIC  Fansog01l  IF   E           K DISK
002100990205RIC  Fanind01l  IF   E           K DISK
002200870105RIC  FTABEL00F  IF   E           K DISK
002300110503     FTntbe01l  IF   E           K DISK
002400110104     FAZORG01L  IF   E           K DISK
002500110105     Ftii404p   O    e             PRINTER infds(prtds)
002600110104     D*---------------------------------------------------------*
002700110104     D ER1             S             66    DIM(2) CTDATA PERRCD(1)              * NESSUNA RIGA
002800110104     D ERR             S              1    DIM(80) CTDATA PERRCD(80)            * ORDINE BLOCCATO
002900870105     D*---------------------------------------------------------*
003000990205     D PARSTA          DS
003100990205     D*--- PARAMETRI PER STAMPA ORDINE :
003200990205     D  codfor                 1      8
003300990205     D  NUMord                 9     13  0
003400990205     D  Tipsta                14     14
003500990205     D  stfabb                15     16
003600990205     D  socpas                17     19
003700870105     D KPJBA         E DS
003800870105     D*--- DATI DA PROGRAMMA DI LANCIO
003900870105     D  EDPUTE                 1      3
004000870105     D DESDEM        E DS                  EXTNAME(ANTAB028)
004100110503     D dvpobrt       e DS
004200870105     D*--- TABELLA DESTINAZIONE MERCE
004300990205     D soc001        E DS                  EXTNAME(xsoc001ds)
004400990205     D xsocds          DS          1000
004500990205     d dataiso         s               d   datfmt(*iso)
004600990205     d dataeur         s               d   datfmt(*eur)
004700110105     d prtds           ds
004800110105     D ODP_TYPE               81     82                                         * ODP Type
004900110105     D FILE_NAME              83     92                                         * File name
005000110105     D LIBRARY                93    102                                         * Library name
005100110105     D SPOOL_FILE            103    112                                         * Spool file name
005200110105     D SPOOL_LIB             113    122                                         * Spool file lib
005300110105     D SPOOL_NUM             123    124I 0                                      * Spool file num
005400110105     D RCD_LEN               125    126I 0                                      * Max record len
005500110105     D KEY_LEN               127    128I 0                                      * Max key len
005600110105     D MEMBER                129    138                                         * Member name
005700110105     D TYPE                  147    148I 0                                      * File type
005800110105     D ROWS                  152    153I 0                                      * Num PRT/DSP rows
005900110105     D COLUMNS               154    155I 0                                      * Num PRT/DSP cols
006000110105     D NUM_RCDS              156    159I 0                                      * Num of records
006100110105     D ACC_TYPE              160    161                                         * Access type
006200110105     D DUP_KEY               162    162                                         * Duplicate key?
006300110105     D SRC_FILE              163    163                                         * Source file?
006400110105     D VOL_OFF               184    185I 0                                      * Vol label offset
006500110105     D BLK_RCDS              186    187I 0                                      * Max rcds in blk
006600110105      *
006700110105     D OVERFLOW              188    189I 0                                      * Overflow line
006800110105      *
006900110105     D BLK_INCR              190    191I 0                                      * Blk increment
007000110105     D FLAGS1                196    196                                         * Misc flags
007100110105     D REQUESTER             197    206                                         * Requester name
007200110105     D OPEN_COUNT            207    208I 0                                      * Open count
007300110105     D BASED_MBRS            211    212I 0                                      * Num based mbrs
007400110105     D FLAGS2                213    213                                         * Misc flags
007500110105     D OPEN_ID               214    215                                         * Open identifier
007600110105     D RCDFMT_LEN            216    217I 0                                      * Max rcd fmt len
007700110105     D CCSID                 218    219I 0                                      * Database CCSID
007800110105     D FLAGS3                220    220                                         * Misc flags
007900110105     D NUM_DEVS              227    228I 0                                      * Num devs defined
008000110105      *
008100110105     D line                  367    368I 0                                      * Current line num
008200110105     D CUR_PAGE              369    372I 0                                      * Current page cnt
008300870105     C*----------------------------------------------------*
008400870105     C     *ENTRY        PLIST                                                  *
008500870105     C                   PARM                    KPJBA                          *
008600990205     c                   movel     kpjbu         parsta
008700110105     c                   z-add     99            line
008800110503     c                   time                    como14           14 0
008900110503     c                   move      como14        woggi             8 0
009000110503     c                   move      woggi         dataeur
009100110503     c                   move      dataeur       dataiso
009200110503     c                   move      dataiso       woggig            8 0
009300110503     c                   move      'VPO'         tbecod
009400110503     c                   movel     'BRT'         tbeke1
009500110503     c     ktbe          chain     tntbe01l
009600110503     c                   if        %found(tntbe01l)
009700110503     c                   movel     tbeuni        dvpobrt
009800110503     c                   if        woggig <= §VPOBRTDTA
009900110503     c                   seton                                        30
010000110503     c                   endif
010100110503     c                   endif
010200110503     C     ktbe          klist
010300110503     C                   kfld                    tbecod
010400110503     C                   kfld                    tbeke1
010500990205     C*--- FILE ORDINI "tmmof01L" :                        *
010600870105     C     KEYORD        KLIST                                                  *
010700870105     C                   KFLD                    CODFOR                         *
010800870105     C                   KFLD                    NUMORD                         *
010900870105     C*--- KEY PER POSIZIONAMENTO X ERRORE                 *
011000870105     C     KEYGT         KLIST                                                  *
011100870105     C                   KFLD                    MOFFOR                         *
011200870105     C                   KFLD                    MOFNOR                         *
011300870105RIC  C******************************************************
011400870105RIC  C*--- FILES ANAGRAFICI FORNIT. "CNACO00F/CNIND00F"    *
011500870105RIC  C     KEYFOR        KLIST                                                  *
011600990205RIC  C                   KFLD                    wsoc                           *
011700990205RIC  C                   KFLD                    staFOR                         *
011800870105RIC  C*--- KEY RICERCA PAGAMENTI SU "TABEL00F"             *
011900870105RIC  C     KEYPAG        KLIST                                                  *
012000990205RIC  C                   KFLD                    CODUTE            1 0          *
012100870105RIC  C                   KFLD                    TIPTAB            2            *
012200990205RIC  C                   KFLD                    KEYTBL            8            *
012300990205     c                   z-add     1             codute
012400870105     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
012500870105     C     KEYANT        KLIST                                                  *
012600870105     C                   KFLD                    TABGRU                         *
012700870105     C                   KFLD                    TABCOD                         *
012800870105     C*--- RICERCA DESCRIZIONE DITTA ----------------------*
012900990205     C                   MOVEL     'SOC001'      TIPXSC
013000990205     C                   MOVEL     *blank        SOCXSC
013100990205     C                   EXSR      REPSOC
013200990205     C     RTNXSC        IFNE      '1'
013300990205     C                   MOVEL     XSOCDS        SOC001
013400990205     C                   MOVEL     xscrgs        desdit           20
013500990205     C                   MOVEL     xscsoc        wsoc              3
013600990205     c                   end
013700870105     C*--- CONTROLLO SE SCELTA STAMPA SOLO RIGHE AGGIUNTE  *
013800870105     C     TIPSTA        COMP      'A'                                    05    *
013900870105     C*--- RICHIESTO STAMPA TUTTI GLI ORDINI --------------*
014000990205     C     CODFOR        IFEQ      '99999999'                                   *
014100870105     C                   DO        *HIVAL                                       *
014200990205     C                   READ      tmmof000                               01    *
014300110105     c                   if        totlor > 0
014400110104     C   01              write     piede
014500110105     C   01              write     piede2
014600110105     c                   endif
014700870105     C   01              GOTO      FINE                                         *
014800870105     C                   EXSR      ORDINE                                       *
014900870105     C                   END                                                    *
015000870105     C                   END                                                    *
015100870105     C*--- RICHIESTO STAMPA TUTTI GLI ORDINI DI UN FORNIT.-*
015200870105     C     NUMORD        IFEQ      99999                                        *
015300990205     C     CODFOR        SETLL     tmmof000                                     *
015400870105     C                   DO        *HIVAL                                       *
015500990205     C     CODFOR        READE     tmmof000                               01    *
015600110105     c                   if        totlor > 0
015700110104     C   01              write     piede
015800110105     C   01              write     piede2
015900110105     c                   endif
016000870105     C   01              GOTO      FINE                                         *
016100870105     C                   EXSR      ORDINE                                       *
016200870105     C                   END                                                    *
016300870105     C                   ELSE                                                   *
016400870105     C*--- RICHIESTO STAMPA UN SOLO ORDINE : --------------*
016500990205     C     KEYORD        SETLL     tmmof000                                     *
016600870105     C                   DO        *HIVAL                                       *
016700990205     C     KEYORD        READE     tmmof000                               01    *
016800110105     c                   if        totlor > 0
016900110104     C   01              write     piede
017000110105     C   01              write     piede2
017100110105     c                   endif
017200870105     C   01              GOTO      FINE                                         *
017300870105     C                   EXSR      ORDINE                                       *
017400870105     C                   END                                                    *
017500870105     C                   END                                                    *
017600870105     C*--- CHIUSURA ULTIMO ORDINE -------------------------*
017700870105     C     FINE          TAG                                                    *
017800870105     C   10              DO                                                     *
017900110104     C*MM                EXCEPT    RIGFIN                                       *
018000870105     C                   END                                                    *
018100870105     C                   SETON                                            LR    *
018200870105     C*--- ROUTINE EMISSIONE ORDINI -----------------------*
018300870105     C***------------------------------------------------***
018400870105     CSR   ORDINE        BEGSR                                                  *
018500870105     C*--- ACQUISIZIONE DATI ANAGRAFICI FORNITORE :        *
018600870105     C     MOFNOR        IFNE      STANOR                                       *
018700110105     C                   IF        STANOR <> 0 and totlor > 0
018800110104     C                   write     piede
018900110105     C                   write     piede2
019000110104     C                   ENDIF
019100871126     C                   Z-ADD     0             TOTGEN
019200880502     C                   Z-ADD     0             TOTLOR
019300870105     C*---                                                 *
019400870105     C   10              DO                                                     *
019500110104     C*MM                EXCEPT    RIGFIN                                       *
019600870105     C                   END                                                    *
019700870105     C                   SETON                                        10        *
019800870105     C                   SETON                                        07        * NO AGGIUNTE
019900870105     C                   Z-ADD     MOFNOR        STANOR            5 0          *
020000990205     C                   MOVE      MOFFOR        STAFOR            8            *
020100990205     C     MOFFOR        CHAIN     tmfop01L                           01        *
020200990205RIC  C  N01KEYFOR        CHAIN     anfrn01l                           01        *
020300990205     c  n01frnsogg       chain     ansog01l                           01
020400990205RIC  C  N01frnsogg       CHAIN     anind01l                           01        *
020500870105RIC  C  N01              DO                                                     *
020600110104RIC  C                   MOVEL     sogdes        STARAG
020700990205RIC  C                   MOVEL     sogpartiva    STAIVA           16            *
020800990205RIC  C                   MOVEl     INDindriz     STAVIA           30            *
020900990205RIC  C                   MOVEl     INDCAP        STACAP            5            *
021000990205RIC  C                   MOVEl     INDlocalit    STACIT           20            *
021100990205RIC  C                   MOVEl     INDPRov       STAPRV            2            *
021200990205RIC  C                   MOVEl     INDSTAto      STASTA            4            *
021300990205RIC  C                   MOVEl     INDTELefon    STATEL           16            *
021400990205RIC  C                   MOVEl     INDTelex      STATLX           16            *
021500870105RIC  C                   END                                                    *
021600870105RIC  C******************************************************
021700870105     C*----------------------------------------------------*
021800870105     C   01              DO                                                     *
021900870105     C                   MOVE      *ALL'*'       STARAG                         *
022000870105     C                   MOVE      *ALL'*'       STAIVA                         *
022100870105     C                   MOVE      *ALL'*'       STAVIA                         *
022200870105     C                   MOVE      *ALL'*'       STACAP                         *
022300870105     C                   MOVE      *ALL'*'       STACIT                         *
022400870105     C                   MOVE      *ALL'*'       STAPRV                         *
022500870105     C                   MOVE      *ALL'*'       STASTA                         *
022600870105     C                   MOVE      *ALL'*'       STATEL                         *
022700870105     C                   MOVE      *ALL'*'       STATLX                         *
022800870105     C                   END                                                    *
022900870105     C*--- ACQUISIZIONE DATI TESTATA ORDINE :              * DATA ORDINE
023000990205     C                   MOVE      MOFDOR        DATAiso                        *
023100990205     C                   move      dataiso       dataeur                        *
023200990205     C                   MOVE      DATAeur       STADOR            8 0          *
023300870105     C*---                                                 * TIPO ORDINE
023400870105     C                   MOVE      '162'         TABGRU                         *
023500870105     C                   MOVE      *ZEROS        TABCOD                         *
023600870105     C                   MOVE      MOFTOR        TABCOD                         *
023700870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
023800870105     C  N46              MOVEL     TABDES        TESDTO           25            *
023900870105     C   46              MOVE      *BLANKS       TESDTO                         *
024000870105     C                   MOVE      MOFTOR        TESTOR            2            *
024100870105     C*---                                                 * GRUPPO MERC.
024200870105     C                   MOVE      '159'         TABGRU                         *
024300870105     C                   MOVE      *ZEROS        TABCOD                         *
024400870105     C                   MOVE      MOFGPM        TABCOD                         *
024500870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
024600870105     C  N46              MOVEL     TABDES        TESDGP           25            *
024700870105     C   46              MOVE      *BLANKS       TESDGP                         *
024800870105     C                   MOVE      MOFGPM        TESGPM            2            *
024900870105     C*---                                                 * SETTORE MERC.
025000870105     C                   MOVE      '160'         TABGRU                         *
025100870105     C                   MOVE      *ZEROS        TABCOD                         *
025200870105     C                   MOVE      MOFSTM        TABCOD                         *
025300870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
025400870105     C  N46              MOVEL     TABDES        TESDST           25            *
025500870105     C   46              MOVE      *BLANKS       TESDST                         *
025600870105     C                   MOVE      MOFSTM        TESSTM            2            *
025700870105     C*---                                                 * LISTINO
025800870105     C                   MOVE      '161'         TABGRU                         *
025900870105     C                   MOVE      *ZEROS        TABCOD                         *
026000870105     C                   MOVE      MOFLIS        TABCOD                         *
026100870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
026200870105     C  N46              MOVEL     TABDES        TESDLI           25            *
026300870105     C   46              MOVE      *BLANKS       TESDLI                         *
026400870105     C                   MOVE      MOFLIS        TESLIS            3            *
026500870105     C*----------------------------------------------------*
026600870105     C*---                                                 * VALUTA
026700870105     C                   MOVE      MOFVAL        TESVAL            4            *
026800870105     C*---                                                 * CAMBIO
026900870105     C                   MOVE      MOFCMB        TESCMB            7 2          *
027000870105     C*---                                                 * COND.CONSEGNA
027100870105     C                   MOVE      MOFCCO        TESCCO           30            *
027200870105     C*---                                                 * DESTIN.MERCE
027300870105     C                   MOVE      '028'         TABGRU                         *
027400870105     C                   MOVE      *ZEROS        TABCOD                         *
027500110112     C                   MOVE      '102'         TABCOD                         *
027600870105     C     KEYANT        CHAIN     ANTAB00F                           46        *
027700870105     C                   MOVE      MOFDEM        TESDEM            3            *
027800870105     C  N46              DO                                                     *
027900870105     C                   MOVEL     TABDES        DESDEM                         *
028000870105     C                   MOVEL     IND028        TESIND           25            *
028100870105     C                   MOVEL     LOC028        TESLOC           25            *
028200870105     C                   END                                                    *
028300870105     C   46              DO                                                     *
028400870105     C                   MOVE      *BLANKS       TESIND                         *
028500870105     C                   MOVE      *BLANKS       TESLOC                         *
028600870105     C                   END                                                    *
028700870105RIC  C                   MOVE      'FA'          TIPTAB                         *
028800990205RIC  C                   MOVEL     MOFCPG        TESCPG            3            *
028900870105RIC  C     KEYPAG        CHAIN     TABEL00F                           46        *
029000870105RIC  C  N46              MOVEL     TBLUNI        TESDPG                         *
029100870105     C   46              MOVE      *BLANKS       TESDPG           30            *
029200870105     C                   END                                                    *
029300870105     C*---------- ESCLUSIONE FASI BB,FA -------------------*
029400870105     C     STFABB        IFEQ      'NO'                                         *
029500870105     C     MOFFAS        CABEQ     'FA'          SALDET               0606      *
029600870105     C     MOFFAS        CABEQ     'BB'          SALDET               0606      *
029700870105     C                   END                                                    *
029800870105     C*---------- ESCLUSIONE RIGHE GIA STAMPATE -----------*
029900870105     C   05MOFFES        CABEQ     'S'           SALDET               0606      *
030000870105     C*--- PREPARO CAMPI X STAMPA DETTAGLIO : -------------*
030100870105     C                   SETON                                        06        * STAMP.DETT.
030200870105     C                   SETOFF                                       07        * SI AGGIUNTE
030300871007     C                   MOVE      MOFMAT        STAMAT           12            *
030400110104     C                   MOVEL     MOFDMT        STADMT
030500110104     C                   MOVEL     MOFUMQ        STAUMQ
030600870105     C                   Z-ADD     MOFQTA        STAQTA            9 2          *
030700870105     C                   MOVE      MOFFAS        STAFAS            2            *
030800990205     C                   MOVE      MOFDCN        DATAIso                        *
030900990205     C                   move      dataiso       dataeur                        *
031000990205     C                   MOVE      DATAeur       STADCN            8 0          *
031100110104     C                   MOVE      MOFCOM        STACOM
031200870105     C                   MOVE      MOFVAL        STAVAL            4            *
031300990205     C                   Z-ADD     MOFPRZ        STAPRZ           14 5          *
031400870105     C                   Z-ADD     MOFSC1        STASC1            2 0          *
031500870105     C                   Z-ADD     MOFSC2        STASC2            2 0          *
031600870131     C                   Z-ADD     MOFSC3        STASC3            2 0          *
031700870105     C                   MOVEL     MOFCPG        STACPG            3            *
031800870105     C                   MOVE      MOFMAG        STAMAG            7            *
031900990205     C                   MOVE      MOFCDC        STACDC            8            *
032000110104     C                   SETOFF                                       98
032100110118     C                   IF        MOFDEM <> '102'
032200110104     C                   MOVE      '028'         TABGRU                         *
032300110104     C                   MOVE      *ZEROS        TABCOD                         *
032400110104     C                   MOVE      MOFDEM        TABCOD                         *
032500110104     C     KEYANT        CHAIN     ANTAB00F                           46        *
032600110104     C                   IF        %FOUND(ANTAB00F)                             *
032700110104     C                   CLEAR                   WMOFDEM
032800110104     C                   MOVE      MOFDEM        WMOFDEM           3 0
032900110104     C     WMOFDEM       CHAIN     AZORG01L
033000110104     C                   IF        %FOUND(AZORG01L)                             *
033100110104     C                   SETON                                        98
033200110104     C                   ENDIF
033300110104     C                   ENDIF
033400110104     C                   ENDIF
033500871126     C*
033600880502     C                   Z-ADD     0             QTALOR
033700140418     C     STAPRZ        MULT      MOFQTA        QTALOR
033800110104     C                   ADD       QTALOR        TOTLOR
033900880502     C*
034000990205     C     STAPRZ        MULT      MOFSC1        SCONTO           14 5          *
034100871126     C     SCONTO        DIV       100           SCONTO                         *
034200990205     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
034300871126     C*
034400871126     C     STAPRZ        MULT      MOFSC2        SCONTO
034500871126     C     SCONTO        DIV       100           SCONTO                         *
034600871126     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
034700871126     C*
034800871126     C     STAPRZ        MULT      MOFSC3        SCONTO
034900871126     C     SCONTO        DIV       100           SCONTO                         *
035000871126     C     STAPRZ        SUB       SCONTO        STAPRZ                         *
035100871126     C                   Z-ADD     0             TOTPRZ
035200990205     C     STAPRZ        MULT      MOFQTA        TOTPRZ           14 5          *
035300990205     C                   ADD       TOTPRZ        TOTGEN           14 5
035400870105     C*--- PREPARAZIONE CAMPI STAMPA IN FUNZIONE DEI TIPI  *
035500870105     C*--- RIGA DEGLI ORDINI :                             *
035600870105     C     MOFTRG        CABEQ     '1'           NONCOD                         *
035700870105     C     MOFTRG        CABEQ     '2'           NONCOD                         *
035800870105     C     MOFTRG        CABEQ     '3'           NONCOD                         *
035900870105     C                   MOVE      *ZEROS        STADCN                         *
036000870105     C                   Z-ADD     0             STASC1                         *
036100870105     C                   Z-ADD     0             STASC2                         *
036200870105     C     MOFTRG        IFEQ      'D'                                          *
036300870105     C                   Z-SUB     MOFPRZ        STAPRZ                         *
036400870105     C                   END                                                    *
036500870105     C     MOFTRG        CABLT     '6'           NONCOD                         *
036600870105     C     MOFTRG        CABGT     '9'           NONCOD                         *
036700870105     C                   MOVE      *BLANKS       STAUMQ                         *
036800870105     C                   Z-ADD     0             STAQTA                         *
036900870105     C                   MOVE      *BLANKS       STAFAS                         *
037000870105     C                   MOVE      *BLANKS       STAVAL                         *
037100870105     C                   MOVE      *BLANKS       STACPG                         *
037200870105     C                   MOVE      *BLANKS       STAMAG                         *
037300870105     C                   MOVE      *BLANKS       STACDC                         *
037400870105     C                   Z-ADD     0             STAPRZ                         *
037500870105     C*----------------------------------------------------*
037600870105     C     NONCOD        TAG                                                    *
037700870105     C     STAMAT        COMP      *BLANKS                            8080      *
037800870105     C     STAQTA        COMP      0                                  8181      *
037900870105     C     STAPRZ        COMP      0                                  8282      *
038000870105     C     STADCN        COMP      0                                  8383      *
038100870105     C     STASC2        COMP      0                                  8484      *
038200870131     C     STASC3        COMP      0                                  8585      *
038300870105     C     SALDET        TAG                                                    *
038400870105     C                   EXSR      RUTSTA                                       *
038500870105     C*--- AGGIORNO FLAG DI RIGA STAMPATA SU ORDINI :      *
038600870105     C                   MOVE      'S'           MOFFES                         *
038700870105     C                   EXCEPT    ORDINI                                       *
038800870105     CSR   FINEOR        ENDSR                                                  *
038900870105     C*--- ROUTINE EMISSIONE STAMPA -----------------------*
039000870105     C***------------------------------------------------***
039100870105     CSR   RUTSTA        BEGSR                                                  *
039200110105      *verifico overflow a seconda che debba stampare la destinazione particolare
039300110112     c                   if        mofdem = '102'
039400110110     C                   IF        line > 41 and *in06                          *
039500110104     C                   WRITE     TESTATA                                      *
039600110503     C   30              WRITE     COSTANTI                                     *
039700110503     C  n30              WRITE     COSTANT2                                     *
039800870105     C                   END                                                    *
039900110105     c                   else
040000110105     C                   IF        line > 39 and *in06                          *
040100110105     C                   WRITE     TESTATA                                      *
040200110503     C   30              WRITE     COSTANTI                                     *
040300110503     C  n30              WRITE     COSTANT2                                     *
040400110105     C                   END                                                    *
040500110105     c                   endif
040600110104     C   06              write     dettaglio                                    *
040700110105     c   06
040800110105     Can 98              write     DESPART                                      *
040900870105     CSR                 ENDSR                                                  *
041000990205     C*----------------------------------------------------*
041100990205     C* Reperimento dati società
041200990205     C*----------------------------------------------------*
041300990205     C     REPSOC        BEGSR
041400990205     C*
041500990205     C                   CALLB     'XSOC'
041600990205     C                   PARM                    TIPXSC            6
041700990205     C                   PARM      socpas        SOCXSC            3
041800990205     C                   PARM                    CDSXSC            9 0
041900990205     C                   PARM                    MODXSC            3
042000990205     C                   PARM      *blanks       RTNXSC            1
042100990205     C                   PARM                    XSOCDS
042200990205     C                   PARM                    KPJBA
042300990205     C*
042400990205     C                   ENDSR
042500110104      *----------------------------------------------------------------
042600990205     Otmmof000  E            ORDINI
042700870105     O                       MOFFES
042800870105     O*
042900870105**   ER1
043000870105  *** ATTENZIONE. Non vi sono RIGHE AGGIUNTE da stampare in questo  ER1
043100870105 ORDINE ***                                                         ER1
043200870105**   ERR
043300870105Ordine N° xxxxx del Fornitore xxxxxxx non stampato perche' in uso !
