000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300930510      * CNC0V4RT                                                     *
000400930510      *        TRASMISSIONE                                          *
000500911030      *        CHIUSURA PER OPERATORE                                *
000600900606?     *--------------------------------------------------------------*
000700940224      * 01 -ON--> PRIMO LIVELLO/OFF --> SECONDO LIVELLO              *
000800940224      * 02 -ON--> SIMFEL UGUALE A 0                                  *
000900940224?     *--------------------------------------------------------------*
001000930521     FQSYSPRT   O    F  132        PRINTER USROPN
001100971121?     *--------------------------------------------------------------*
001200910708     D CNCR80        E DS
001300911104     D DSCMZ         E DS
001400900514     D KPJBA         E DS
001500900514     D  LIBSYS                92    100
001600900514     D  MSGERR               453    502
001700900516      *
001800900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900020805     d tibs55ds      E DS
002300961014     D                 DS
002400961014     D  WGG                    1      2  0
002500961014     D  WMM                    3      4  0
002600961014     D  WAA                    5      6  0
002700961014     D  WDATA                  1      6  0
002800020805
002900020805     d                 ds
003000020805     d  $emme                  1      1    inz('M')
003100020805     d  $simfel                2      4
003200020805     d  $mbr                   5      7    inz('046')
003300020805     d  $mbr6                  2      7
003400020805     d  $varmbr                1     10
003401020920     d   §membro       ds
003402020920     d  §emme                  1      1    inz('M')
003403020920     d  §fle                   2      4s 0
003404020920     d  §sed                   5      7s 0 inz(046)
003405020920     d  §mbr                   2      7
003500971121?     *--------------------------------------------------------------*
003600930521     C                   OPEN      QSYSPRT
003700930510      *  CONTROLLO SE HO RECORD DA TRASMETTERE
003800930510     C                   EXSR      TSTOBJ
003900971121      *  REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE VARIAZ.
004000971121     C                   EXSR      DATVAR
004100920213      *  TRASMISSIONE VARIAZIONI A SEDE
004200920213     C                   EXSR      X25VRA
004300971121      *  REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE MOV-C/ASS
004400971121     C                   EXSR      DATMOV
004500911104      *  TRASMISSIONE CONTRASSEGNI INCASSATI
004600911104     C     NRARC         IFNE      *ZEROS
004700911104     C                   EXSR      X25ARC
004800911104     C                   END
004900991018     C*  TRASMISSIONE     FNMOV00T
005000911104     C     NRMOV         IFNE      *ZEROS
005100911104     C                   EXSR      X25MOV
005200911104     C                   END
005300911105      * STAMPO SEGNALAZIONE CHE CHIUSURA è OK
005400930513     C     NRARC         IFNE      *ZEROS
005500930513     C     NRMOV         ORNE      *ZEROS
005600911105     C                   EXCEPT    FINNMI
005700931220     C                   ELSE
005800931220     C                   EXCEPT    FINNMN
005900930513     C                   END
006000930510     C     FINE          TAG
006100910521     C                   SETON                                        LR
006200930426      *----------------------------------------------------------------
006300930426      * PULIZIA PARAMETRI X TRUL51C
006400930426      *----------------------------------------------------------------
006500930426     C     TRUL51        BEGSR
006600930426     C                   MOVEL     KNSIF         LIBR             10
006700930426     C                   MOVEL     *BLANK        FILE             10
006800930426     C                   MOVEL     *BLANK        MBR              10
006900930426     C                   Z-ADD     0             NRR              10 0
007000930426     C                   ENDSR
007100971121      *----------------------------------------------------------------
007200971121      * REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE
007300971121      *----------------------------------------------------------------
007400971121     C     DATVAR        BEGSR
007500971121     C*
007600020805     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
007700971121     C* da/a cui spedire
007800020805     C                   CLEAR                   TIBS55ds
007900971121     C                   MOVEL     'L'           I50TLA
008000971121     C                   MOVEL     KNSIF         I50PSI
008100971121     C                   MOVEL     SIMFEL        I50PPO
008200971121     C                   MOVEL     '046'         I50APO
008300020805     C                   CALL      'TIBS55R'
008400020805     C                   PARM                    TIBS55ds
008500971121     C*
008600971121     C                   ENDSR
008700971121      *----------------------------------------------------------------
008800971121      * REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE
008900971121      *----------------------------------------------------------------
009000971121     C     DATMOV        BEGSR
009100971121     C*
009200020805     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
009300971121     C* da/a cui spedire
009400020805     C                   CLEAR                   TIBS55ds
009500971121     C                   MOVEL     'L'           I50TLA
009600971121     C                   MOVEL     KNSIF         I50PSI
009700971121     C                   MOVEL     §FLE          I50PPO
009800971121     C                   MOVEL     '046'         I50APO
009900020805     C                   CALL      'TIBS55R'
010000020805     C                   PARM                    TIBS55ds
010100971121     C*
010200971121     C                   ENDSR
010300930510      *-----------------------------------------------------***********
010400930510      * TEST NR RECORD FILE DA TRASMETTER                   *  TSTOBJ *
010500930510      *-----------------------------------------------------***********
010600930510     C     TSTOBJ        BEGSR
010700930510     C** FILIALE CHE CHIUDE    PER APERTURA MEMBRO
011100020920     C* MEMBRO = §FLE + 046
011400991018     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
011500930510     C                   EXSR      TRUL51
011600991018     C                   MOVEL     'FNMOV00T'    FILE
011700020920     C                   MOVEL(p)  §membro       MBR
011800930510     C                   CALL      'TRUL51C'
011900930510     C                   PARM                    LIBR
012000930510     C                   PARM                    FILE
012100930510     C                   PARM                    MBR
012200930510     C                   PARM                    NRR
012300930512     C     NRR           IFNE      *ALL'9'
012400930510     C                   Z-ADD     NRR           NRMOV            10 0
012500930512     C                   END
012600941018     C* GUARDO SE IL MEMBRO è VUOTO - FNCAS00T
012700930510     C                   EXSR      TRUL51
012800941018     C                   MOVEL     'FNCAS00T'    FILE
012900020920     C                   MOVEL     §membro       MBR
013000930510     C                   CALL      'TRUL51C'
013100930510     C                   PARM                    LIBR
013200930510     C                   PARM                    FILE
013300930510     C                   PARM                    MBR
013400930510     C                   PARM                    NRR
013500930512     C     NRR           IFNE      *ALL'9'
013600930510     C                   Z-ADD     NRR           NRARC            10 0
013700930512     C                   END
013800971125     C* VARIAZIONI BOLLE IL MEMBRO è SEMPRE 046
013900930510      *-----------------------------------------------------------
014200020805     c                   movel     simfel        $simfel
014300020805     C                   MOVE      ')'           VAR7              7
014500020805     c                   movel     $mbr6         var7
014600020805     C                   MOVEL     VAR7          VAR8              8
014700020805     C                   MOVE      ')'           VAR8
014800941018     C* GUARDO SE IL MEMBRO è VUOTO - FNARBD0T
014900930510     C                   EXSR      TRUL51
015000941018     C                   MOVEL     'FNARBD0T'    FILE
015200020805     c                   Movel     $varmbr       Mbr
015300930510     C                   CALL      'TRUL51C'
015400930510     C                   PARM                    LIBR
015500930510     C                   PARM                    FILE
015600930510     C                   PARM                    MBR
015700930510     C                   PARM                    NRR
015800930512     C     NRR           IFNE      *ALL'9'
015900930510     C                   Z-ADD     NRR           RECABI           10 0
016000930512     C                   END
016100941018     C* GUARDO SE IL MEMBRO è VUOTO - FNARBT0T
016200930510     C                   EXSR      TRUL51
016300991012     C                   MOVEL     'FIARBT0T'    FILE
016500020805     c                   Movel     $varmbr       Mbr
016600930510     C                   CALL      'TRUL51C'
016700930510     C                   PARM                    LIBR
016800930510     C                   PARM                    FILE
016900930510     C                   PARM                    MBR
017000930510     C                   PARM                    NRR
017100930512     C     NRR           IFNE      *ALL'9'
017200930510     C                   Z-ADD     NRR           RECABT           10 0
017300930512     C                   END
017400941018     C* GUARDO SE IL MEMBRO è VUOTO - FNARBK0T
017500930510     C                   EXSR      TRUL51
017600941018     C                   MOVEL     'FNARBK0T'    FILE
017800020805     c                   Movel     $varmbr       Mbr
017900930510     C                   CALL      'TRUL51C'
018000930510     C                   PARM                    LIBR
018100930510     C                   PARM                    FILE
018200930510     C                   PARM                    MBR
018300930510     C                   PARM                    NRR
018400930512     C     NRR           IFNE      *ALL'9'
018500930510     C                   Z-ADD     NRR           RECABK           10 0
018600930512     C                   END
018700941018     C* GUARDO SE IL MEMBRO è VUOTO - FNARBM0T
018800930510     C                   EXSR      TRUL51
018900941018     C                   MOVEL     'FNARBM0T'    FILE
019100020805     c                   Movel     $varmbr       Mbr
019200930510     C                   CALL      'TRUL51C'
019300930510     C                   PARM                    LIBR
019400930510     C                   PARM                    FILE
019500930510     C                   PARM                    MBR
019600930510     C                   PARM                    NRR
019700930512     C     NRR           IFNE      *ALL'9'
019800930510     C                   Z-ADD     NRR           RECABM           10 0
019900930512     C                   END
020000941018     C* GUARDO SE IL MEMBRO è VUOTO - FNARBG0T
020100930511     C                   EXSR      TRUL51
020200941018     C                   MOVEL     'FNARBG0T'    FILE
020400020805     c                   Movel     $varmbr       Mbr
020500930511     C                   CALL      'TRUL51C'
020600930511     C                   PARM                    LIBR
020700930511     C                   PARM                    FILE
020800930511     C                   PARM                    MBR
020900930511     C                   PARM                    NRR
021000930512     C     NRR           IFNE      *ALL'9'
021100930511     C                   Z-ADD     NRR           RECABG           10 0
021200930512     C                   END
021300991129     C* GUARDO SE IL MEMBRO è VUOTO - FIARBU0T
021400941018     C                   EXSR      TRUL51
021500991129     C                   MOVEL     'FIARBU0T'    FILE
021700020805     c                   Movel     $varmbr       Mbr
021800941018     C                   CALL      'TRUL51C'
021900941018     C                   PARM                    LIBR
022000941018     C                   PARM                    FILE
022100941018     C                   PARM                    MBR
022200941018     C                   PARM                    NRR
022300941018     C     NRR           IFNE      *ALL'9'
022400941018     C                   Z-ADD     NRR           RECABU           10 0
022500941018     C                   END
022600930510     C                   ENDSR
022700911104     C/SPACE 005
022800911104     C**----------------------------------
022900941018     C** PREPARA SOTTOMISSIONE TRASMISS.FILE VARIAZIONI X LA SEDE
023000920213     C**-------------------------------------
023100920213     C     X25VRA        BEGSR
023200961014     C**
023300961014     C                   TIME                    W0140            14 0
023400961014     C                   MOVE      W0140         UDATE8            8 0          GG/MM/AAAA
023500961014     C** DATA TRASMISSIONE FILE DI VARIAZIONE
023600961014     C                   MOVE      UDATE8        W0020             2 0
023700961014     C                   MOVEL     UDATE8        WDATA
023800961014     C                   MOVE      W0020         WDATA
023900961014     C                   Z-ADD     WGG           WMMGG             4 0
024000961014     C                   MOVEL     WMM           WMMGG
024100961014     C                   Z-ADD     WMMGG         WDT60             6 0          AAAA/MM/GG
024200961014     C                   MOVEL     WAA           WDT60
024300961014     C* ORA DI INVIO
024400961014     C                   TIME                    WOR60             6 0
024500961014     C                   ADD       010000        WOR60
024600961014     C                   MOVE      0000          WOR60
024700920213     C* DESTINATARIO
024800920213     C                   MOVEL     '046'         CMZDST                          PER SEDE
024900991117     C                   MOVEL     'TR39'        COMAZN            4
025000920213     C* ABBLENCO FILE PREREQUISITO
025100920213     C                   MOVEL     *BLANKS       CMZPRR
025400971121     C                   MOVEL     O50PLA        CMZLBD
025500971121     C                   MOVEL     O50ALA        CMZLBA
025600971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
025700961014     C                   MOVEL     WOR60         CMZORI                          ORA
025800930409     C                   MOVE      '0000000'     CMZDTI                          DATA
025900961014     C                   MOVE      WDT60         CMZDTI
026000961014     C                   MOVE      'N'           CMZURG                          NORMALE
026100920213     C                   MOVE      'A'           CMZADD                          IN ADD
026200920213     C                   MOVE      'S'           CMZCLR                          CLEAR
026300920213     C                   MOVE      'S'           CMZFTR                          DA TRANS.
026400920213     C                   MOVE      *BLANK        CMZERR                          1= ERR.
026500920213     C*
026600941018     C* VEDO SE CONTIENE RECORD FNARBD0T
026700920213     C     RECABI        IFGT      0
026800920213     C*
026900990830     C     RECABU        IFEQ      0
027000990830     C     RECABT        ANDEQ     0
027100920213     C     RECABK        ANDEQ     0
027200920213     C     RECABM        ANDEQ     0
027300930511     C     RECABG        ANDEQ     0
027400920213     C                   MOVEL     COMAZN        CMZAZN
027500920213     C                   ELSE
027600920213     C                   MOVEL     *BLANKS       CMZAZN
027700920213     C                   END
027800920213     C*
027900920213     C                   EXSR      X25ABI
028000920213     C                   END
028100990830     C*
028200991129     C* VEDO SE CONTIENE RECORD FIARBU0T
028300990830     C     RECABU        IFGT      0
028400990830     C*
028500990830     C     RECABT        IFEQ      0
028600990830     C     RECABK        ANDEQ     0
028700990830     C     RECABM        ANDEQ     0
028800990830     C     RECABG        ANDEQ     0
028900990830     C                   MOVEL     COMAZN        CMZAZN
029000990830     C                   ELSE
029100990830     C                   MOVEL     *BLANKS       CMZAZN
029200990830     C                   END
029300990830     C*
029400990830     C                   EXSR      X25ABU
029500990830     C                   END
029600920213     C*
029700941018     C* VEDO SE CONTIENE RECORD FNARBT0T
029800920213     C     RECABT        IFGT      0
029900920213     C*
030000920213     C     RECABK        IFEQ      0
030100920213     C     RECABM        ANDEQ     0
030200930511     C     RECABG        ANDEQ     0
030300920213     C                   MOVEL     COMAZN        CMZAZN
030400920213     C                   ELSE
030500920213     C                   MOVEL     *BLANKS       CMZAZN
030600920213     C                   END
030700920213     C*
030800920213     C                   EXSR      X25ABT
030900920213     C                   END
031000920213     C*
031100941018     C* VEDO SE CONTIENE RECORD FNARBM0T
031200920213     C     RECABM        IFGT      0
031300920213     C*
031400920213     C     RECABK        IFEQ      0
031500930511     C     RECABG        ANDEQ     0
031600920213     C                   MOVEL     COMAZN        CMZAZN
031700920213     C                   ELSE
031800920213     C                   MOVEL     *BLANKS       CMZAZN
031900920213     C                   END
032000920213     C*
032100920213     C                   EXSR      X25ABM
032200920213     C                   END
032300920213     C*
032400941018     C* VEDO SE CONTIENE RECORD FNARBK0T
032500920213     C     RECABK        IFGT      0
032600920213     C*
032700991012     C     RECABG        IFEQ      0
032800920213     C                   MOVEL     COMAZN        CMZAZN
032900920213     C                   ELSE
033000920213     C                   MOVEL     *BLANKS       CMZAZN
033100920213     C                   END
033200920213     C*
033300920213     C                   EXSR      X25ABK
033400920213     C                   END
033500920213     C*
033600941018     C* VEDO SE CONTIENE RECORD FNARBG0T
033700930511     C     RECABG        IFGT      0
033800930511     C                   MOVEL     COMAZN        CMZAZN
033900930511     C                   EXSR      X25ABG
034000930511     C                   END
034100941018     C*
034200920213     C                   ENDSR
034300920213     C**----------------------------------
034400941018     C** SOTTOMETTE TRASMISSIONE F N A R B I 0 T
034500920213     C**----------------------------------
034600920213     C     X25ABI        BEGSR
034700941018     C                   MOVEL     'FNARBD0T'    CMZFLD                          DA: >>>
034900020805     c                   clear                   cmzmbd
035000020805     c                   Movel     $varmbr       cmzmbd
035100941018     C                   MOVEL     'FNARBD0R'    CMZFLA                          A: >>>
035200920213     C                   MOVEL     *BLANK        CMZMBA
035400020805     c                   Movel     $varmbr       cmzmba
035500920213     C                   MOVE      RECABI        CMZNRR                          N.REC.
035600920213     C*
035700920213     C                   EXSR      CALCMZ
035800920213     C                   ENDSR
035900920213     C**----------------------------------
036000941018     C** SOTTOMETTE TRASMISSIONE F N A R B T 0 T
036100920213     C**----------------------------------
036200920213     C     X25ABT        BEGSR
036300991012     C                   MOVEL     'FIARBT0T'    CMZFLD                          DA: >>>
036500020805     c                   clear                   cmzmbd
036600020805     c                   Movel     $varmbr       cmzmbd
036700991012     C                   MOVEL     'FIARBT0R'    CMZFLA                          A: >>>
036800920213     C                   MOVEL     *BLANK        CMZMBA
037000020805     c                   Movel     $varmbr       cmzmba
037100920213     C                   MOVE      RECABT        CMZNRR                          N.REC.
037200920213     C*
037300920213     C                   EXSR      CALCMZ
037400920213     C                   ENDSR
037500920213     C**----------------------------------
037600941018     C** SOTTOMETTE TRASMISSIONE F N A R B K 0 T
037700920213     C**----------------------------------
037800920213     C     X25ABK        BEGSR
037900941018     C                   MOVEL     'FNARBK0T'    CMZFLD                          DA: >>>
038100020805     c                   clear                   cmzmbd
038200020805     c                   Movel     $varmbr       cmzmbd
038300941018     C                   MOVEL     'FNARBK0R'    CMZFLA                          A: >>>
038400920213     C                   MOVEL     *BLANK        CMZMBA
038600020805     c                   Movel     $varmbr       cmzmba
038700920213     C                   MOVE      RECABK        CMZNRR                          N.REC.
038800920213     C*
038900920213     C                   EXSR      CALCMZ
039000920213     C                   ENDSR
039100920213     C**----------------------------------
039200941018     C** SOTTOMETTE TRASMISSIONE F N A R B M 0 T
039300920213     C**----------------------------------
039400920213     C     X25ABM        BEGSR
039500941018     C                   MOVEL     'FNARBM0T'    CMZFLD                          DA: >>>
039700020805     c                   clear                   cmzmbd
039800020805     c                   Movel     $varmbr       cmzmbd
039900941018     C                   MOVEL     'FNARBM0R'    CMZFLA                          A: >>>
040000920213     C                   MOVEL     *BLANK        CMZMBA
040200020805     c                   Movel     $varmbr       cmzmba
040300920213     C                   MOVE      RECABM        CMZNRR                          N.REC.
040400920213     C*
040500920213     C                   EXSR      CALCMZ
040600920213     C                   ENDSR
040700930511     C**----------------------------------
040800941018     C** SOTTOMETTE TRASMISSIONE F N A R B G 0 T
040900930511     C**----------------------------------
041000930511     C     X25ABG        BEGSR
041100941018     C                   MOVEL     'FNARBG0T'    CMZFLD                          DA: >>>
041300020805     c                   clear                   cmzmbd
041400020805     c                   Movel     $varmbr       cmzmbd
041500941018     C                   MOVEL     'FNARBG0R'    CMZFLA                          A: >>>
041600930511     C                   MOVEL     *BLANK        CMZMBA
041800020805     c                   Movel     $varmbr       cmzmba
041900930511     C                   MOVE      RECABG        CMZNRR                          N.REC.
042000930511     C*
042100930511     C                   EXSR      CALCMZ
042200930511     C                   ENDSR
042300941018     C**----------------------------------
042400941018     C** SOTTOMETTE TRASMISSIONE F N A R B U 0 T
042500941018     C**----------------------------------
042600941018     C     X25ABU        BEGSR
042700991129     C                   MOVEL     'FIARBU0T'    CMZFLD                          DA: >>>
042900020805     c                   clear                   cmzmbd
043000020805     c                   Movel     $varmbr       cmzmbd
043100991129     C                   MOVEL     'FIARBU0R'    CMZFLA                          A: >>>
043200941018     C                   MOVEL     *BLANK        CMZMBA
043400020805     c                   Movel     $varmbr       cmzmba
043500941018     C                   MOVE      RECABG        CMZNRR                          N.REC.
043600941018     C*
043700941018     C                   EXSR      CALCMZ
043800941018     C                   ENDSR
043900991012     C**----------------------------------
044000911104     C** SOTTOMENTE TRASMISSIONE
044100911104     C**----------------------------------
044200911104     C     X25ARC        BEGSR
044300911104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
044400941018     C                   MOVEL     'FNCAS00T'    CMZFLD                          DA: >>>
044800020920     C                   MOVEL(p)  §membro       CMZMBD
044900971121     C                   MOVEL     O50PLA        CMZLBD
045000941018     C                   MOVEL     'FNCAS00R'    CMZFLA                          A: >>>
045200020920     C                   MOVEL(P)  §membro       CMZMBA
045300971121     C                   MOVEL     O50ALA        CMZLBA
045400010312     C                   MOVEL     'GAITRA'      CMZLBA
045500010312     C                   MOVE      O50ALA        WQUATR            4
045600010312     C     WQUATR        IFEQ      '101 '
045700010312     C                   MOVE      '201 '        CMZLBA
045800010312     C                   ENDIF
045900911104     C                   MOVE      '0000'        CMZORI                          ORA
046000911104     C                   MOVE      '0000000'     CMZDTI                          DATA
046100981210     C                   MOVE      'N'           CMZURG                          NOTTE
046200911104     C                   MOVE      'A'           CMZADD                          IN ADD
046300911104     C                   MOVE      'S'           CMZCLR                          CLEAR
046400911104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
046500911104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
046600941018     C                   MOVE      'TR87'        CMZAZN                          AZIONE
046700971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
046800010312     C                   MOVEL     'GAITRA'      CMZSYF
046900010312     C                   MOVE      O50ASI        WQUATR
047000010312     C     WQUATR        IFEQ      '101 '
047100010312     C                   MOVE      '201 '        CMZSYF
047200010312     C                   ENDIF
047300911104     C                   MOVE      NRARC         CMZNRR                          N.REC.
047400990830     C                   MOVEL     *BLANKS       CMZPRR                          PREREQ
047500920213     C                   EXSR      CALCMZ
047600911104     C                   ENDSR
047700911104     C*
047800911104     C     X25MOV        BEGSR
047900911104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
048000991018     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
048400020920     C                   MOVEL(p)  §membro       CMZMBD
048500971121     C                   MOVEL     O50PLA        CMZLBD
048600991018     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
048800020920     C                   MOVEL(p)  §membro       CMZMBA
048900971121     C                   MOVEL     O50ALA        CMZLBA
049000010312     C                   MOVEL     'GAITRA'      CMZLBA
049100010312     C                   MOVE      O50ALA        WQUATR            4
049200010312     C     WQUATR        IFEQ      '101 '
049300010312     C                   MOVE      '201 '        CMZLBA
049400010312     C                   ENDIF
049500911104     C                   MOVE      '0000'        CMZORI                          ORA
049600911104     C                   MOVE      '0000000'     CMZDTI                          DATA
049700981210     C                   MOVE      'N'           CMZURG                          NOTTE
049800911104     C                   MOVE      'A'           CMZADD                          IN ADD
049900911104     C                   MOVE      'S'           CMZCLR                          CLEAR
050000911104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
050100911104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
050200991117     C                   MOVE      'Y029'        CMZAZN                          AZIONE
050300971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
050400010312     C                   MOVEL     'GAITRA'      CMZSYF
050500010312     C                   MOVE      O50ASI        WQUATR
050600010312     C     WQUATR        IFEQ      '101 '
050700010312     C                   MOVE      '201 '        CMZSYF
050800010312     C                   ENDIF
050900911104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
051000941018     C****** >>>>>>  SE NON CI SONO C/ASSEGNI DA TRASMETTERE   (FNCAS00T)
051100911104     C****** >>>>>>  ABBLENCO FILE PRE-REQUISITO
051200911104     C     NRARC         IFEQ      *ZEROS
051300911104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
051400911104     C                   END
051500920213     C                   EXSR      CALCMZ
051600911104     C                   ENDSR
051700900606      *----------------------------------------------------------------
051800930510     C**----------------------------------
051900930510     C** CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
052000930510     C**----------------------------------
052100930510     C     CALCMZ        BEGSR
052200971121     C*
052300971121     C                   CLEAR                   W236
052400020805     C                   CALL      'TIBS53C'
052500971121     C                   PARM                    O50LCL
052600971121     C                   PARM                    CMZDST
052700930510     C                   PARM                    CMZFLD
052800930510     C                   PARM                    CMZMBD
052900930510     C                   PARM                    CMZLBD
053000930510     C                   PARM                    CMZFLA
053100930510     C                   PARM                    CMZMBA
053200930510     C                   PARM                    CMZLBA
053300930510     C                   PARM                    CMZORI
053400930510     C                   PARM                    CMZDTI
053500930510     C                   PARM                    CMZURG
053600930510     C                   PARM                    CMZADD
053700930510     C                   PARM                    CMZCLR
053800930510     C                   PARM                    CMZFTR
053900930510     C                   PARM                    CMZERR
054000930510     C                   PARM                    CMZAZN
054100930510     C                   PARM                    CMZSYF
054200930510     C                   PARM                    CMZNRR
054300930510     C                   PARM                    CMZPRR
054400971121     C                   PARM                    W236            236
054500930510     C                   ENDSR
054600941018      **
054700941018     C     *INZSR        BEGSR
054800941018     C     *ENTRY        PLIST
054900941018     C                   PARM                    KPJBA
055000941018      *
055100941018     C                   Z-ADD     1             CODUT
055200941018     C                   CALL      'X§PARUT'
055300941018     C                   PARM                    UT§DSE
055400941018     C     CODUT         CABEQ     -1            FINE
055500941018     C                   MOVEL     REC80         CNCR80
056000020508     C     SIMTPP        IFEQ      '2'
056100020508     C     SIMTPP        OREQ      *BLANKS
056200020508     C                   MOVE      SIMPOU        §FLE              3 0
056300020508     C                   ELSE
056400941018     C     SIMFEL        COMP      0                                      02
056500941018     C  N02              MOVE      SIMFEL        §FLE
056600941018     C                   END
056700941018     C                   ENDSR
056800930521     OQSYSPRT   E            FINNMI      3  2
056900911105     O                                         +  1 'CHIUSURA TERMINATA'
057000911105     O                                         +  1 'REGOLARMENTE !!!!!'
057100931220     O          E            FINNMN      3  2
057200931220     O                                         +  1 'CHIUSURA TERMINATA'
057300931220     O                                         +  1 'REGOLARMENTE !!!!!'
057400931220     O                                         +  1 '- SENZA INCASSI -'
