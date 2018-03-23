000100000000     H*------------------------------------------------------------------------*
000200071228     H* CREAZIONE FILE DATI CLIENTI/FORNITORI PER POSTEL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600110718     FWFPTL00F  IF   E             DISK    USROPN
000700110718     FWFPTL10F  IF   E             DISK    USROPN
000800110718     FTIVGD00F  O    E             DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000000     D*------------------------------------------------------------------------*
001300000000     D* INPUT
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* ARCHITETTURA
001700000000     D*-------------------
001800000000     D KPJBA         E DS
001900000000     D*-------------------
002000000000     D* DS PARAMETRI DI LANCIO PGM
002100000000     D*-------------------
002200060714     D PARAM           DS
002300060714     D  PARDAI                        8  0
002400060714     D  PARDAF                        8  0
002500060714     D  PARFAT                       17  0
002600060714     D  PARKSC                        7
002700060714     D  PARFIL                        3  0
002800060714     D  PARNAZ                        3
002900060714     D  PARCAP                        9
003000060714     D  PARPRV                        2
003100060714     D  PARBLK                        1
003200060714     D  PARCIC                        1
003300060714     D  PARBAR                        1
003400060714     D  PARSDI                        1
003500060714     D  PARPIC                        1
003600060714     D  PARFOA                        1
003700060714     D  PARNTN1                       2
003800060714     D  PARNTE1                       2
003900060714     D  PARSOC                        3
004000060714     D  PARCOF                        1
004100060714     D  PARCPI                        1
004200060714     D  PARIND                        1
004300060714     D  PARNTN2                       2
004400060714     D  PARNTN3                       2
004500060714     D  PARNTN4                       2
004600060714     D  PARNTE2                       2
004700060714     D  PARNTE3                       2
004800060714     D  PARNTE4                       2
004900060714     D  PARNTE5                       2
005000060714     D  PARNTE6                       2
005100060714     D  PARNTE7                       2
005200060714     D  PARNTE8                       2
005300060714     D  PARTBC                        1
005400060714     D  PARRGR                        1
005500060714     D  PAROVR                        1
005600060714     D  PARCHK                        1
005700060714     D  PARSOGN                       1
005800000914     D*-------------------
005900110718     D* DS OUTOUT X DOWNLOAD
006000110718     D*-------------------
006100110718     D DS_OUTDWL       DS
006200110718     D  O_WFPTIP                      1A
006300110718     D  O_WFPCOD                     11S 0
006400110718     D  O_WFPPRO                      2A
006500110718     D  O_WFPCAP                      9A
006600110718     D  O_WFPNAZ                      3A
006700110718     D  O_WFPEML                     60A
006800110718     D  O_WFPABC                      1A
006900110718     D  O_WFPFIL                      3S 0
007000110718     D  O_WFPCOM                      7S 0
007100110718     D  O_WFPDEC                     60A
007200110718     D  O_WFPEMC                     60A
007300110718     D  O_WFPDES                     20A
007400110718     D  O_WFPINF                     20A
007500110718     D  O_WFPCPF                      5S 0
007600110718     D  O_WFPLOF                     20A
007700110718     D  O_WFPPRF                      2A
007800110718     D  O_WFPTEF                     13A
007900110718     D  O_WFPFAF                     13A
008000110909     D  O_WFPARE                      3S 0
008100110718     D*------------------------------------------------------------------------*
008200000918     D* VARIABILI DI WRK
008300000914     D*------------------------------------------------------------------------*
008400000000     C*------------------------------------------------------------------------*
008500000000     C* MAIN LINE
008600000000     C*------------------------------------------------------------------------*
008700000000     C*
008800110718     C* Creazione file estratto clienti
008900110718     C                   exsr      exeCLI
009000110718     C*
009100110718     C* Creazione file estratto potenziali
009200110718     C                   exsr      exePOT
009300110718     C*
009400110718     C* Creazione estratto dati x download
009500110718     C                   exsr      exeDOWN
009600000914     C*
009700000914     C                   SETON                                        LR
009800000922     C*------------------------------------------------------------------------*
009900110718     C* EXECLI - Creazione file estratto clienti
010000000922     C*------------------------------------------------------------------------*
010100110718     C     EXECLI        BEGSR
010200000922     C*
010300110718     C* Reperisco la data corrente (ovvero data "A")
010400110718     C                   z-add     *zeros        datAA             8 0
010500110718     C                   eval      datAA  = %dec(%date() : *ISO)
010600110718     C*
010700110718     C* Reperisco la data corrente - 1 anno (ovvero data "DA")
010800110718     C                   z-add     *zeros        datDA             8 0
010900110718     C                   eval      datDA  = datAA - 10000
011000110718     C*
011100110718     C* Valorizzo i parametri d lancio che pilottano l'estrazinone dei dati
011200110718     C                   clear                   PARAM
011300110718     C*
011400110718     C                   eval      PARDAI  = datDA
011500110718     C                   eval      PARDAF  = datAA
011600110718     C                   eval      PARFAT  = 1
011700110718     C                   eval      PARKSC  = '       '
011800110718     C                   eval      PARFIL  = 000.
011900110718     C                   eval      PARNAZ  = '999'
012000110718     C                   eval      PARCAP  = '         '
012100110718     C                   eval      PARPRV  = '  '
012200120420     C                   eval      PARBLK  = 'N'
012300110718     C                   eval      PARCIC  = ' '
012400110718     C                   eval      PARBAR  = 'S'
012500110718     C                   eval      PARSDI  = 'S'
012600110718     C                   eval      PARPIC  = 'N'
012700110718     C                   eval      PARFOA  = 'S'
012800110718     C                   eval      PARNTN1 = '  '
012900110718     C                   eval      PARNTE1 = '03'
013000110719     C                   eval      PARSOC  = '201'
013100110718     C                   eval      PARCOF  = 'C'
013200110718     C                   eval      PARCPI  = 'N'
013300110718     C                   eval      PARIND  = 'S'
013400110718     C                   eval      PARNTN2 = '  '
013500110718     C                   eval      PARNTN3 = '  '
013600110718     C                   eval      PARNTN4 = '  '
013700110718     C                   eval      PARNTE2 = '06'
013800110718     C                   eval      PARNTE3 = '88'
013900110718     C                   eval      PARNTE4 = '85'
014000110718     C                   eval      PARNTE5 = '84'
014100110718     C                   eval      PARNTE6 = '87'
014200110718     C                   eval      PARNTE7 = '08'
014300110718     C                   eval      PARNTE8 = '  '
014400110718     C                   eval      PARTBC  = 'S'
014500110718     C                   eval      PARRGR  = 'N'
014600110718     C                   eval      PAROVR  = 'N'
014700110718     C                   eval      PARCHK  = 'S'
014800110718     C                   eval      PARSOGN = 'S'
014900110718     C*
015000110718     C                   eval      KPJBU = PARAM
015100110718     C*
015200110718     C* Eseguo il *pgm d estrazione dati clienti potenziali
015300110718     C                   CALL      'TIST43R'
015400110718     C                   PARM                    KPJBA
015500000922     C*
015600000922     C                   ENDSR
015700110718     C*------------------------------------------------------------------------*
015800110718     C* EXEPOT - Creazione file estratto potenziali
015900110718     C*------------------------------------------------------------------------*
016000110718     C     EXEPOT        BEGSR
016100110718     C*
016200110718     C* Eseguo il *pgm d estrazione dati clienti potenziali
016300110718     C                   CALL      'TIST97R'
016400110718     C                   PARM                    KPJBA
016500110718     C*
016600110718     C                   ENDSR
016700110718     C*------------------------------------------------------------------------*
016800110718     C* EXEDOWN - Creazione estratto dati x download
016900110718     C*------------------------------------------------------------------------*
017000110718     C     EXEDOWN       BEGSR
017100110718     C*
017200110718     C* Apro i file d input
017300110718     C                   if        not %open(WFPTL00F)
017400110718     C                   open      WFPTL00F
017500110718     C                   endif
017600110718     C                   if        not %open(WFPTL10F)
017700110718     C                   open      WFPTL10F
017800110718     C                   endif
017900110718     C*
018000110718     C* Leggo tutto il file estratto clienti
018100110718     C     *start        setll     WFPTL00F
018200110718     C                   read      WFPTL00F
018300110718     C                   dow       not %eof(WFPTL00F)
018400110718     C*
018500110718     C                   clear                   DS_OUTDWL
018600110718     C                   eval       O_WFPTIP = WFPTIP
018700110718     C                   eval       O_WFPCOD = WFPKSC
018800110718     C                   eval       O_WFPPRO = WFPPRO
018900110718     C                   eval       O_WFPCAP = WFPCAP
019000110718     C                   eval       O_WFPNAZ = WFPNAZ
019100110718     C                   eval       O_WFPEML = WFPEML
019200110718     C                   eval       O_WFPABC = WFPABC
019300110718     C                   eval       O_WFPFIL = WFPFIL
019400110718     C                   eval       O_WFPCOM = WFPCOM
019500110718     C                   eval       O_WFPDEC = WFPDEC
019600110718     C                   eval       O_WFPEMC = WFPEMC
019700110718     C                   eval       O_WFPDES = WFPDES
019800110718     C                   eval       O_WFPINF = WFPINF
019900110718     C                   eval       O_WFPCPF = WFPCPF
020000110718     C                   eval       O_WFPLOF = WFPLOF
020100110718     C                   eval       O_WFPPRF = WFPPRF
020200110718     C                   eval       O_WFPTEF = WFPTEF
020300110718     C                   eval       O_WFPFAF = WFPFAF
020400110909     C                   eval       O_WFPARE = WFPARE
020500110718     C*
020600110718     C* Scarico il buffer d output
020700110718     C                   exsr      wriVGD
020800110718     C*
020900110718     C                   read      WFPTL00F
021000110718     C                   enddo
021100110718     C*
021200110718     C* Leggo tutto il file estratto potenziali
021300110718     C     *start        setll     WFPTL10F
021400110718     C                   read      WFPTL10F
021500110718     C                   dow       not %eof(WFPTL10F)
021600110718     C*
021700110718     C                   clear                   DS_OUTDWL
021800110718     C                   eval       O_WFPTIP = WFPTIP
021900110905     C                   eval       O_WFPCOD = WFPCPO
022000110718     C                   eval       O_WFPPRO = WFPPRO
022100110718     C                   eval       O_WFPCAP = WFPCAP
022200110718     C                   eval       O_WFPNAZ = WFPNAZ
022300110718     C                   eval       O_WFPEML = WFPEML
022400110718     C                   eval       O_WFPABC = WFPABC
022500110718     C                   eval       O_WFPFIL = WFPFIL
022600110718     C                   eval       O_WFPCOM = WFPCOM
022700110718     C                   eval       O_WFPDEC = WFPDEC
022800110718     C                   eval       O_WFPEMC = WFPEMC
022900110718     C                   eval       O_WFPDES = WFPDES
023000110718     C                   eval       O_WFPINF = WFPINF
023100110718     C                   eval       O_WFPCPF = WFPCPF
023200110718     C                   eval       O_WFPLOF = WFPLOF
023300110718     C                   eval       O_WFPPRF = WFPPRF
023400110718     C                   eval       O_WFPTEF = WFPTEF
023500110718     C                   eval       O_WFPFAF = WFPFAF
023600110909     C                   eval       O_WFPARE = WFPARE
023700110718     C*
023800110718     C* Scarico il buffer d output
023900110718     C                   exsr      wriVGD
024000110718     C*
024100110718     C                   read      WFPTL10F
024200110718     C                   enddo
024300110718     C*
024400110718     C* Chiuso i file d input
024500110718     C                   if        %open(WFPTL00F)
024600110718     C                   close     WFPTL00F
024700110718     C                   endif
024800110718     C                   if        %open(WFPTL10F)
024900110718     C                   close     WFPTL10F
025000110718     C                   endif
025100110718     C*
025200110718     C                   ENDSR
025300110718     C*------------------------------------------------------------------------*
025400110718     C* Scrittura record in TIVGD00F (file VAS generico download)
025500110718     C*------------------------------------------------------------------------*
025600110718     C     wriVGD        BEGSR
025700110718     C*
025800110718     C                   clear                   tivgd000
025900110718     C                   eval      vgdDTA = DS_OUTDWL
026000110718     C                   eval      vgdTIP = 'CC'
026100110718     C                   eval      vgdKSU = '0BART001'
026200110718     C                   eval      vgdTSC = 'WW'
026300110718     C                   eval      vgdDAT = %dec(%date() : *ISO)
026400110718     C                   eval      vgdPGM = 'TIST43R1'
026500110718     C                   write     tivgd000
026600110718     C*
026700110718     C                   ENDSR
026800000000     C*------------------------------------------------------------------------*
026900000000     C* *INZSR - ROUTINE INIZIALE
027000000000     C*------------------------------------------------------------------------*
027100000000     C     *INZSR        BEGSR
027200110718     C*
027300110718     C* PASSAGGIO PARAMETRI
027400000000     C     *ENTRY        PLIST
027500000000     C                   PARM                    KPJBA
027600000000     C*
027700000000     C                   ENDSR
