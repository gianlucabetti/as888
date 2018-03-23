000100000000     H*------------------------------------------------------------------------*
000200071228     H* CREAZIONE FILE DATI CLIENTI/FORNITORI PER POSTEL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500170323     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000600000914     H*------------------------------------------------------------------------*
000700110718     FWFPTL00F  IF   E             DISK    USROPN
000800110718     FWFPTL10F  IF   E             DISK    USROPN
000900170323     FTIVGD00F  O    E             DISK    USROPN COMMIT
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*------------------------------------------------------------------------*
001400000000     D* INPUT
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* ARCHITETTURA
001800000000     D*-------------------
001900000000     D KPJBA         E DS
002000000000     D*-------------------
002100000000     D* DS PARAMETRI DI LANCIO PGM
002200000000     D*-------------------
002300060714     D PARAM           DS
002400060714     D  PARDAI                        8  0
002500060714     D  PARDAF                        8  0
002600060714     D  PARFAT                       17  0
002700060714     D  PARKSC                        7
002800060714     D  PARFIL                        3  0
002900060714     D  PARNAZ                        3
003000060714     D  PARCAP                        9
003100060714     D  PARPRV                        2
003200060714     D  PARBLK                        1
003300060714     D  PARCIC                        1
003400060714     D  PARBAR                        1
003500060714     D  PARSDI                        1
003600060714     D  PARPIC                        1
003700060714     D  PARFOA                        1
003800060714     D  PARNTN1                       2
003900060714     D  PARNTE1                       2
004000060714     D  PARSOC                        3
004100060714     D  PARCOF                        1
004200060714     D  PARCPI                        1
004300060714     D  PARIND                        1
004400060714     D  PARNTN2                       2
004500060714     D  PARNTN3                       2
004600060714     D  PARNTN4                       2
004700060714     D  PARNTE2                       2
004800060714     D  PARNTE3                       2
004900060714     D  PARNTE4                       2
005000060714     D  PARNTE5                       2
005100060714     D  PARNTE6                       2
005200060714     D  PARNTE7                       2
005300060714     D  PARNTE8                       2
005400060714     D  PARTBC                        1
005500060714     D  PARRGR                        1
005600060714     D  PAROVR                        1
005700060714     D  PARCHK                        1
005800060714     D  PARSOGN                       1
005900000914     D*-------------------
006000110718     D* DS OUTOUT X DOWNLOAD
006100110718     D*-------------------
006200110718     D DS_OUTDWL       DS
006300110718     D  O_WFPTIP                      1A
006400110718     D  O_WFPCOD                     11S 0
006500110718     D  O_WFPPRO                      2A
006600110718     D  O_WFPCAP                      9A
006700110718     D  O_WFPNAZ                      3A
006800110718     D  O_WFPEML                     60A
006900110718     D  O_WFPABC                      1A
007000110718     D  O_WFPFIL                      3S 0
007100110718     D  O_WFPCOM                      7S 0
007200110718     D  O_WFPDEC                     60A
007300110718     D  O_WFPEMC                     60A
007400110718     D  O_WFPDES                     20A
007500110718     D  O_WFPINF                     20A
007600110718     D  O_WFPCPF                      5S 0
007700110718     D  O_WFPLOF                     20A
007800110718     D  O_WFPPRF                      2A
007900110718     D  O_WFPTEF                     13A
008000110718     D  O_WFPFAF                     13A
008100110909     D  O_WFPARE                      3S 0
008200110718     D*------------------------------------------------------------------------*
008300000918     D* VARIABILI DI WRK
008400000914     D*------------------------------------------------------------------------*
008500170323     D psds           sds
008600170323     D  procname         *PROC
008700170323     D TIS7VASDS     E DS
008800000000     C*------------------------------------------------------------------------*
008900000000     C* MAIN LINE
009000000000     C*------------------------------------------------------------------------*
009100170323     C*
009200170323     C* Avvio il monitoring del processo
009300170323     C                   monitor
009400170323     C*
009500170323     C* Avvio il controllo sincronia
009600170323     C                   CLEAR                   TIS7VASDS
009700170323     C                   EVAL      i§7VASOPZ = 'SCM'
009800170323     C                   CALL(e)   'TIS7VASR1'
009900170323     C                   PARM                    TIS7VASDS
010000170323     C*
010100170323     C* Verifico esito chiamata
010200170323     C                   if        %error OR o§7VASOK = *off
010300170323     C                   exsr      EXEERR
010400170323     C                   endif
010500170323     C*
010600170323     C* Se tutto OK apro il file download
010700170323     C                   if        not %open(tivgd00f)
010800170323     C                   open      tivgd00f
010900170323     C                   endif
011000000000     C*
011100110718     C* Creazione file estratto clienti
011200110718     C                   exsr      exeCLI
011300110718     C*
011400110718     C* Creazione file estratto potenziali
011500110718     C                   exsr      exePOT
011600110718     C*
011700110718     C* Creazione estratto dati x download
011800110718     C                   exsr      exeDOWN
011900170323     C*
012000170323     C* Sancisco il commit
012100170323     C                   commit(e)
012200170323     C*
012300170323     C* Chiudo il file download
012400170323     C                   if        %open(tivgd00f)
012500170323     C                   close     tivgd00f
012600170323     C                   endif
012700170323     C*
012800170323     C* Fine controllo sincronia
012900170323     C                   EVAL      i§7VASOPZ = 'ECM'
013000170323     C                   CALL(e)   'TIS7VASR1'
013100170323     C                   PARM                    TIS7VASDS
013200170323     C*
013300170323     C* Verifico esito chiamata
013400170323     C                   if        %error OR o§7VASOK = *off
013500170323     C                   exsr      EXEERR
013600170323     C                   endif
013700170323     C*
013800170323     C* Su errore
013900170323     C                   on-error
014000170323     C                   exsr      exeerr
014100170323     C*
014200170323     C* Fine monitoring
014300170323     C                   endmon
014400000914     C*
014500000914     C                   SETON                                        LR
014600000922     C*------------------------------------------------------------------------*
014700110718     C* EXECLI - Creazione file estratto clienti
014800000922     C*------------------------------------------------------------------------*
014900110718     C     EXECLI        BEGSR
015000000922     C*
015100110718     C* Reperisco la data corrente (ovvero data "A")
015200110718     C                   z-add     *zeros        datAA             8 0
015300110718     C                   eval      datAA  = %dec(%date() : *ISO)
015400110718     C*
015500110718     C* Reperisco la data corrente - 1 anno (ovvero data "DA")
015600110718     C                   z-add     *zeros        datDA             8 0
015700110718     C                   eval      datDA  = datAA - 10000
015800110718     C*
015900110718     C* Valorizzo i parametri d lancio che pilottano l'estrazinone dei dati
016000110718     C                   clear                   PARAM
016100110718     C*
016200110718     C                   eval      PARDAI  = datDA
016300110718     C                   eval      PARDAF  = datAA
016400110718     C                   eval      PARFAT  = 1
016500110718     C                   eval      PARKSC  = '       '
016600110718     C                   eval      PARFIL  = 000.
016700110718     C                   eval      PARNAZ  = '999'
016800110718     C                   eval      PARCAP  = '         '
016900110718     C                   eval      PARPRV  = '  '
017000120420     C                   eval      PARBLK  = 'N'
017100110718     C                   eval      PARCIC  = ' '
017200110718     C                   eval      PARBAR  = 'S'
017300110718     C                   eval      PARSDI  = 'S'
017400110718     C                   eval      PARPIC  = 'N'
017500110718     C                   eval      PARFOA  = 'S'
017600110718     C                   eval      PARNTN1 = '  '
017700110718     C                   eval      PARNTE1 = '03'
017800110719     C                   eval      PARSOC  = '201'
017900110718     C                   eval      PARCOF  = 'C'
018000110718     C                   eval      PARCPI  = 'N'
018100110718     C                   eval      PARIND  = 'S'
018200110718     C                   eval      PARNTN2 = '  '
018300110718     C                   eval      PARNTN3 = '  '
018400110718     C                   eval      PARNTN4 = '  '
018500110718     C                   eval      PARNTE2 = '06'
018600110718     C                   eval      PARNTE3 = '88'
018700110718     C                   eval      PARNTE4 = '85'
018800110718     C                   eval      PARNTE5 = '84'
018900110718     C                   eval      PARNTE6 = '87'
019000110718     C                   eval      PARNTE7 = '08'
019100110718     C                   eval      PARNTE8 = '  '
019200110718     C                   eval      PARTBC  = 'S'
019300110718     C                   eval      PARRGR  = 'N'
019400110718     C                   eval      PAROVR  = 'N'
019500110718     C                   eval      PARCHK  = 'S'
019600110718     C                   eval      PARSOGN = 'S'
019700110718     C*
019800110718     C                   eval      KPJBU = PARAM
019900110718     C*
020000110718     C* Eseguo il *pgm d estrazione dati clienti potenziali
020100110718     C                   CALL      'TIST43R'
020200110718     C                   PARM                    KPJBA
020300000922     C*
020400000922     C                   ENDSR
020500110718     C*------------------------------------------------------------------------*
020600110718     C* EXEPOT - Creazione file estratto potenziali
020700110718     C*------------------------------------------------------------------------*
020800110718     C     EXEPOT        BEGSR
020900110718     C*
021000110718     C* Eseguo il *pgm d estrazione dati clienti potenziali
021100110718     C                   CALL      'TIST97R'
021200110718     C                   PARM                    KPJBA
021300110718     C*
021400110718     C                   ENDSR
021500110718     C*------------------------------------------------------------------------*
021600110718     C* EXEDOWN - Creazione estratto dati x download
021700110718     C*------------------------------------------------------------------------*
021800110718     C     EXEDOWN       BEGSR
021900170323     C*
022000170323     C* Inizializzo variabili d wrk
022100170323     C                   movel     'N'           wProcedi          1
022200170323     C*
022300170323     C* Stacco progressivo univoco download
022400170323     C                   CLEAR                   TIS7VASDS
022500170323     C                   EVAL      i§7VASOPZ = 'PRG'
022600170323     C                   CALL(e)   'TIS7VASR1'
022700170323     C                   PARM                    TIS7VASDS
022800170323     C*
022900170323     C* Se OK => proseguo
023000170323     C                   if        not %error AND
023100170323     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
023200170323     C                   movel     'S'           wProcedi
023300170323     C                   endif
023400170323     C*
023500170323     C* Se ok a procedere => elaboro
023600170323     C                   if        wProcedi = 'S'
023700110718     C*
023800110718     C* Apro i file d input
023900110718     C                   if        not %open(WFPTL00F)
024000110718     C                   open      WFPTL00F
024100110718     C                   endif
024200110718     C                   if        not %open(WFPTL10F)
024300110718     C                   open      WFPTL10F
024400110718     C                   endif
024500110718     C*
024600110718     C* Leggo tutto il file estratto clienti
024700110718     C     *start        setll     WFPTL00F
024800110718     C                   read      WFPTL00F
024900110718     C                   dow       not %eof(WFPTL00F)
025000110718     C*
025100110718     C                   clear                   DS_OUTDWL
025200110718     C                   eval       O_WFPTIP = WFPTIP
025300110718     C                   eval       O_WFPCOD = WFPKSC
025400110718     C                   eval       O_WFPPRO = WFPPRO
025500110718     C                   eval       O_WFPCAP = WFPCAP
025600110718     C                   eval       O_WFPNAZ = WFPNAZ
025700110718     C                   eval       O_WFPEML = WFPEML
025800110718     C                   eval       O_WFPABC = WFPABC
025900110718     C                   eval       O_WFPFIL = WFPFIL
026000110718     C                   eval       O_WFPCOM = WFPCOM
026100110718     C                   eval       O_WFPDEC = WFPDEC
026200110718     C                   eval       O_WFPEMC = WFPEMC
026300110718     C                   eval       O_WFPDES = WFPDES
026400110718     C                   eval       O_WFPINF = WFPINF
026500110718     C                   eval       O_WFPCPF = WFPCPF
026600110718     C                   eval       O_WFPLOF = WFPLOF
026700110718     C                   eval       O_WFPPRF = WFPPRF
026800110718     C                   eval       O_WFPTEF = WFPTEF
026900110718     C                   eval       O_WFPFAF = WFPFAF
027000110909     C                   eval       O_WFPARE = WFPARE
027100110718     C*
027200110718     C* Scarico il buffer d output
027300110718     C                   exsr      wriVGD
027400110718     C*
027500110718     C                   read      WFPTL00F
027600110718     C                   enddo
027700110718     C*
027800110718     C* Leggo tutto il file estratto potenziali
027900110718     C     *start        setll     WFPTL10F
028000110718     C                   read      WFPTL10F
028100110718     C                   dow       not %eof(WFPTL10F)
028200110718     C*
028300110718     C                   clear                   DS_OUTDWL
028400110718     C                   eval       O_WFPTIP = WFPTIP
028500110905     C                   eval       O_WFPCOD = WFPCPO
028600110718     C                   eval       O_WFPPRO = WFPPRO
028700110718     C                   eval       O_WFPCAP = WFPCAP
028800110718     C                   eval       O_WFPNAZ = WFPNAZ
028900110718     C                   eval       O_WFPEML = WFPEML
029000110718     C                   eval       O_WFPABC = WFPABC
029100110718     C                   eval       O_WFPFIL = WFPFIL
029200110718     C                   eval       O_WFPCOM = WFPCOM
029300110718     C                   eval       O_WFPDEC = WFPDEC
029400110718     C                   eval       O_WFPEMC = WFPEMC
029500110718     C                   eval       O_WFPDES = WFPDES
029600110718     C                   eval       O_WFPINF = WFPINF
029700110718     C                   eval       O_WFPCPF = WFPCPF
029800110718     C                   eval       O_WFPLOF = WFPLOF
029900110718     C                   eval       O_WFPPRF = WFPPRF
030000110718     C                   eval       O_WFPTEF = WFPTEF
030100110718     C                   eval       O_WFPFAF = WFPFAF
030200110909     C                   eval       O_WFPARE = WFPARE
030300110718     C*
030400110718     C* Scarico il buffer d output
030500110718     C                   exsr      wriVGD
030600110718     C*
030700110718     C                   read      WFPTL10F
030800110718     C                   enddo
030900110718     C*
031000110718     C* Chiuso i file d input
031100110718     C                   if        %open(WFPTL00F)
031200110718     C                   close     WFPTL00F
031300110718     C                   endif
031400110718     C                   if        %open(WFPTL10F)
031500110718     C                   close     WFPTL10F
031600110718     C                   endif
031700170323     C*
031800170323     C* Finalizzo la transazione
031900170323     C                   EVAL      i§7VASOPZ  = 'RLS'
032000170323     C                   EVAL      i§7VASTIP  = 'CC'
032100170323     C                   EVAL      i§7VASKSU  = '0BART001'
032200170323     C                   EVAL      i§7VASTSC  = 'WW'
032300170323     C                   EVAL      i§7VASSTO  = '?'
032400170323     C                   EVAL      i§7VASSTTO = 'G'
032500170323     C                   EVAL      i§7VASPRG  = o§7VASPRG
032600170323     C                   CALL(e)   'TIS7VASR1'
032700170323     C                   PARM                    TIS7VASDS
032800170323     C*
032900170323     C* Verifico esito chiamata
033000170323     C                   if        %error OR o§7VASOK = *off
033100170323     C                   exsr      EXEERR
033200170323     C                   endif
033300170323     C*
033400170323     C                   endif
033500110718     C*
033600110718     C                   ENDSR
033700110718     C*------------------------------------------------------------------------*
033800110718     C* Scrittura record in TIVGD00F (file VAS generico download)
033900110718     C*------------------------------------------------------------------------*
034000110718     C     wriVGD        BEGSR
034100110718     C*
034200110718     C                   clear                   tivgd000
034300110718     C                   eval      vgdDTA = DS_OUTDWL
034400110718     C                   eval      vgdTIP = 'CC'
034500110718     C                   eval      vgdKSU = '0BART001'
034600110718     C                   eval      vgdTSC = 'WW'
034700110718     C                   eval      vgdDAT = %dec(%date() : *ISO)
034800170323     C                   eval      vgdPRG = o§7VASPRG
034900170323     C                   eval      vgdPGM = procname
035000170323     C                   eval      vgdSTO = '?'
035100110718     C                   write     tivgd000
035200110718     C*
035300110718     C                   ENDSR
035400170323     C*------------------------------------------------------------------------*
035500170323     C* EXEERR - Routine di esecuzione azioni su Errore
035600170323     C*------------------------------------------------------------------------*
035700170323     C     EXEERR        BEGSR
035800170323     C*
035900170323     C                   dump(A)
036000170323     C                   rolbk(e)
036100170323     C                   seton                                        lr
036200170323     C                   return
036300170323     C*
036400170323     C                   ENDSR
036500000000     C*------------------------------------------------------------------------*
036600000000     C* *INZSR - ROUTINE INIZIALE
036700000000     C*------------------------------------------------------------------------*
036800000000     C     *INZSR        BEGSR
036900110718     C*
037000110718     C* PASSAGGIO PARAMETRI
037100000000     C     *ENTRY        PLIST
037200000000     C                   PARM                    KPJBA
037300000000     C*
037400000000     C                   ENDSR
