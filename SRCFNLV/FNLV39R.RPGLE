000100041104     H DECEDIT('0,') DATEDIT(*yMd.)
000200990730     H* FNLV39R  *---------------------------------------------------*
000300990730     H*     CALCOLO PESO   DA FATTURARE                             *
000400921021     H*--------------------------------------------------------------*
000500941031     FTABEL00F  IF   E           K DISK
000600041104     FFNagp00f  O    E             DISK    USROPN
000700041104     FFNARBP0T  O    E             DISK    USROPN
000800041104     F                                     RENAME(FNARBP00:FNARBPTT)
000900041104     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
001000041109     D CMDdly          S             48    DIM(1) CTDATA PERRCD(1)
001100940218      *
001200990730     D* DS PER FNLV39R - DS DEI CAMPI PER CALCOLO PESO DA FATTURARE
001300990730     D DSLV39        E DS                  EXTNAME(FNLV39DS)
001400990730     D* DS TIPI PESO
001500990804     D DS7Y          E DS
001600990730     D* DS DATI PESO CML
001700070706     D** DS3I          E DS
001800941031     D KPJBA         E DS
001900041217     d
002000041217     D WLBDAT          DS                  INZ
002100041217     D  G02DAT                 1      8  0
002200041217     D  G02INV                 9     16  0
002300041217     D  G02ERR                17     17
002400041217     D  G02TGI                18     22  0
002500041104     d  MbrFis         s             10    inz('B946046')
002600041104     d  wapri          s              1    inz(' ')
002700921021     C*---------------------------------------------------------------*
002800941031     C* RIEPILOGO INDICATORI
002900990730     C* 01    - PER CML: PESI PARZIALE
003000990730     C* 02    - PER CML: PESI TOTALE
003100041109     C* 06    - ESEGUO LA scrittura file di appoggio
003200990730     C* 26    - ERRORE IN APERTURA FILE FNARBP0T
003300960930     C* 30/34 - DI COMODO
003400941031     C*---------------------------------------------------------------*
003500921021     C     *ENTRY        PLIST
003600941031     C                   PARM                    KPJBA
003700990730     C                   MOVEL     KPJBU         DSLV39
003800041109     c
003900041109     C* SE KNSIF=*BLANKS IMPOSTO *LIBL
004000041109     c                   if        knsif=*blanks
004100041109     c                   eval      knsif='*LIBL'
004200041109     c                   endif
004300000000     C*---------------------------------------------------------------*
004400990730     C* TIPO LANCIO, D39TLA "C" -           CHIUSO CON LR
004500990730     C* TIPO LANCIO, D39TLA "L" - ELABORO E CHIUDO CON LR
004600990730     C* TIPO LANCIO, D39TLA " " - ELABORO E CHIUDO IN RETRN
004700990730    1C     D39TLA        IFNE      'C'
004800990730     C                   CLEAR                   D39ERR
004900041217     c* Calcolo la TIME
005000041217     C                   TIME                    W0140            14 0
005100041217     C* UDATE IN GGMMAAAA
005200041217     C                   MOVE      W0140         WDTGIO            8 0
005300041217     C*
005400041217     C* UDATE IN AAAAMMGG
005500041217     C                   Z-ADD     WDTGIO        G02DAT
005600041217     C                   MOVEL     *BLANK        G02ERR
005700041217     C                   CALL      'XSRDA8'
005800041217     C                   PARM                    WLBDAT
005900041217     C                   MOVEL     G02INV        DATEU             8 0
006000941027     C*
006100990730     C* VEDO QUALE PESO HO MODIFICATO
006200941027     C                   SELECT
006300990730     C     D39TPP        WHENEQ    'B'
006400941027     C                   EXSR      GESVLB
006500941025     C*
006600990730     C     D39TPP        WHENEQ    'F'
006700941027     C                   EXSR      GESVLF
006800941027     C*
006900990730     C     D39TPP        WHENEQ    'C'
007000941027     C                   EXSR      GESVLC
007100941027     C                   ENDSL
007200941031    1C                   ENDIF
007300941031     C*
007400941031     C* CHIUSURA PGM
007500941031     C*
007600990730     C                   MOVEL     DSLV39        KPJBU
007700941031     C*
007800990730    1C     D39TLA        IFEQ      ' '
007900941031     C                   RETURN
008000941031     C                   ELSE
008100041104     c                   if        wapri='S'
008200041104     c                   close     fnarbp0t
008300041104     c                   endif
008400941031     C                   SETON                                        LR
008500941031    1C                   ENDIF
008600941027     C*
008700990730     C* MODIFICATO IL PESO DA FATTURARE ----------------------------*
008800941027     C     GESVLF        BEGSR
008900990730     C* IN PARTENZA VARIO SEMPRE ANCHE IL PESO BOLLETTATO SE
009000990730     C* IL FLAG PESO DA FATTURARE NON E' "Z" O "T"
009100990730     C     D39TBO        IFEQ      'P'
009200941027     C*
009300990730     C     D39FPF        IFNE      'Z'
009400990730     C     D39FPF        ANDNE     'T'
009500990730     C                   MOVEL     D39PLU        D39PLB
009600990730     C                   MOVEL     D39FPL        D39FPB
009700941027     C                   ENDIF
009800941027     C                   ENDIF
009900941027     C*
010000990730     C* IN ARRIVO   VARIO IL PESO BOLLETTATO SOLO SE ERA AUTOMATICO
010100990730     C* ED HO RICALCOLATO UN NUOVO PESO AUTOMATICO
010200990730     C     D39TBO        IFEQ      'A'
010300990730     C     D39TBO        OREQ      ' '
010400941027     C*
010500990730     C     D39FPL        IFEQ      'K'
010600990730     C     D39FPL        OREQ      'C'
010700990730     C                   MOVEL     D39PLU        D39PLB
010800990730     C                   MOVEL     D39FPL        D39FPB
010900941027     C                   ENDIF
011000941027     C                   ENDIF
011100941027     C*
011200990730     C* IMPOSTO IL PESO VARIATO NEL PESO D FATTURARE
011300990730     C                   MOVEL     D39PLU        D39PLF
011400990730     C                   MOVEL     D39FPL        D39FPF
011500941027     C                   ENDSR
011600941027     C*
011700990730     C* MODIFICATO IL PESO BOLLETTATO ------------------------------*
011800941027     C     GESVLB        BEGSR
011900941027     C*
012000941027     C* SE VLF    E' "T" NON LO VARIO
012100941027     C*
012200941027     C* SE VLC NON C'E' --> VLF = VLB
012300990730    1C     D39FPF        IFNE      'T'
012400990730    2C     D39FPF        IFNE      'Z'
012500990730     C                   Z-ADD     D39PLU        D39PLF
012600990730     C                   MOVEL     D39FPL        D39FPF
012700941025    2C                   ENDIF
012800941027     C*
012900941027     C* SE VLF    E' "Z" MAGGIORE DEL NUOVO BOLLETTATO NON LO VARIO
013000941027     C* SE VLF    E' "Z" MINORE IMPOSTO IL BOLLETTATO IN VLF
013100990730    2C     D39FPF        IFEQ      'Z'
013200990730     C     D39PLF        ANDLT     D39PLU
013300990730     C                   Z-ADD     D39PLU        D39PLF
013400990730     C                   MOVEL     D39FPL        D39FPF
013500941027    2C                   ENDIF
013600941031    2C                   ENDIF
013700941027     C*
013800990730     C                   Z-ADD     D39PLU        D39PLB
013900990730     C                   MOVEL     D39FPL        D39FPB
014000941025     C*
014100941027     C                   ENDSR
014200941027     C*
014300990730     C* MODIFICATO IL PESO CML -------------------------------------*
014400941027     C     GESVLC        BEGSR
014500041104     C     WUNO          IFEQ      ' '
014600041104     C                   OPEN      FNAgp00f                             98
014700041104     C                   MOVEL     '1'           WUNO              1
014800041104     C                   ENDIF
014900941025     C*
015000990730     C* DETERMINO SE PESO PARZIALE O TOTALE
015100941025     C                   EXSR      CTRVLC
015200941025     C*
015300941025     C* DETERMINO CHI HA CML E CHI NO
015400941025     C                   EXSR      CTRCML
015500941025     C**
015600041104     C* scrittura file di appoggio e trasmissione a sede
015700941025     C**
015800990730    1C     D39FTR        IFEQ      'S'
015900941025     C                   EXSR      GESTRA
016000970818    1C                   ENDIF
016100941031     C*
016200041109     C                   ENDSR
016300941021     C*
016400941024     C* OPERAZIONI INIZIALI ------------------------------------------*
016500941024     C     *INZSR        BEGSR
016600941025     C     KTAB          KLIST
016700941025     C                   KFLD                    CODUT             1 0
016800941025     C                   KFLD                    COD               2
016900941025     C                   KFLD                    KEY               8
017000041109     C*
017100041109     C                   Z-ADD     1             CODUT             1 0
017200041109     C*
017300041109     C                   Z-ADD     48            LUNG
017400941025     C*
017500941031     C     *LIKE         DEFINE    ARBCVB        WCVB
017600941024     C                   ENDSR
017700941025     C*
017800990730     C* DETERMINO SE PESO TOTALE O PARIZIALE -----------------------*
017900941025     C     CTRVLC        BEGSR
018000941025     C                   SETOFF                                       0102
018100941025     C*
018200990730     C* PESO PARZIALE
018300050111     C     D39ncl        IFGT      D39NCP
018400941031     C                   MOVEL     'Z'           WFVF              1
018500941025     C                   SETON                                        01
018600941025     C                   ELSE
018700990730     C* PESO TOTALE
018800941025     C                   MOVEL     'T'           WFVF
018900941025     C                   SETON                                        02
019000941025     C                   ENDIF
019100941031     C*
019200990730     C                   MOVEL     WFVF          D39FPL
019300941025     C*
019400941025     C                   ENDSR
019500941025     C*
019600041109     C* vedo a chi trasmettere ---------------------------------------*
019700941025     C     CTRCML        BEGSR
019800941025     C                   CLEAR                   WINSE
019900941025     C                   CLEAR                   WINFI
020000941025     C*
020100990730     C* CARICO IN TABELLA LE DS DEI TIPI PESO CML "T" E "Z"
020200941031    1C     WCART         IFEQ      ' '
020300941025     C                   EXSR      CARTAB
020400941031     C                   MOVEL     '1'           WCART             1
020500941031    1C                   ENDIF
020600970826     C**
020700970826     C* CAUSALE VARIAZIONE
020800970826     C**
020900990917     C                   MOVEL     'PV'          WCVB
021000970826     C**
021100041109     C* FLAG DI invio/sistituzione
021200941025     C**
021300970826     C*  PARTENZA
021400941025     C**
021500990730    0C     D39FTR        IFEQ      'S'
021600970902     C**
021700990730    1C     D39TBO        IFEQ      'P'
021800941025     C*
021900941025     C* TRASMISSIONE IN SEDE
022000990804     C   01              MOVEL     Z7YPSF        WINSE             1
022100990804     C   02              MOVEL     T7YPSF        WINSE             1
022200041108     C* TRASMISSIONE IN partenza: sono in partenza non c'e'
022300941031    1C                   ENDIF
022400941025     C**
022500970826     C* ARRIVO
022600941025     C**
022700990730    1C     D39TBO        IFEQ      'A'
022800970826     C* TRASMISSIONE IN SEDE
022900990804     C   01              MOVEL     Z7YASF        WINSE             1
023000990804     C   02              MOVEL     T7YASF        WINSE             1
023100041108     C* TRASMISSIONE IN partenza
023200990804     C   01              MOVEL     Z7YAPF        WINFI             1
023300990804     C   02              MOVEL     T7YAPF        WINFI             1
023400041104     c* Trasmettere alla partenza
023500041104     c                   if        winfi='S'
023600041104     c                   eval      winfi='P'
023700041104     c                   endif
023800941025    1C                   ENDIF
023900941025     C**
024000941025     C* TRANSITO
024100941025     C**
024200990730    1C     D39TBO        IFEQ      'T'
024300941025     C*
024400941025     C* TRASMISSIONE IN SEDE
024500990804     C   01              MOVEL     Z7YTSF        WINSE             1
024600990804     C   02              MOVEL     T7YTSF        WINSE             1
024700041108     C* TRASMISSIONE IN partenza
024800041108     C   01              MOVEL     Z7YTPF        WINFi             1
024900041108     C   02              MOVEL     T7YTPF        WINFi             1
025000041104     c* Trasmettere alla partenza
025100041108     c                   if        winfi='S'
025200041108     c                   eval      winfi='P'
025300041104     c                   endif
025400941025     C*
025500941031    1C                   ENDIF
025600970902    0C                   ENDIF
025700941025     C*
025800941025     C                   ENDSR
025900941025     C*
026000990730     C* CARICO LE 2 DS DEI TIPI PESO ------------------------------*
026100941025     C     CARTAB        BEGSR
026200941025     C*
026300990730     C* CARICO FLAGS TIPO PESO 'T'
026400990804     C                   MOVEL     '7Y'          COD
026500941025     C                   MOVEL(P)  'T'           KEY
026600941025     C     KTAB          CHAIN     TABEL00F                           32
026700941025     C*
026800990804     C  N32              MOVEL     TBLUNI        DS7Y
026900990804     C   32              CLEAR                   DS7Y
027000941025     C*
027100041109     C* ARR.:INVIO PARTEN
027200990804     C                   MOVEL     §7YAPF        T7YAPF            1
027300041109     C* ARR.:INVIO SEDE
027400990804     C                   MOVEL     §7YASF        T7YASF            1
027500941025     C*
027600041109     C* PAR.:INVIO SEDE
027700990804     C                   MOVEL     §7YPSF        T7YPSF            1
027800941025     C*
027900041109     C* TRAN:INVIO PARTEN
028000990804     C                   MOVEL     §7YTPF        T7YTPF            1
028100041109     C* TRAN:INVIO SEDE
028200990804     C                   MOVEL     §7YTSF        T7YTSF            1
028300941025     C*
028400990730     C* CARICO FLAGS TIPO PESO 'Z'
028500990804     C                   MOVEL     '7Y'          COD
028600941025     C                   MOVEL     *BLANKS       KEY
028700941025     C                   MOVEL     'Z'           KEY
028800941025     C     KTAB          CHAIN     TABEL00F                           32
028900941025     C*
029000990804     C  N32              MOVEL     TBLUNI        DS7Y
029100990804     C   32              CLEAR                   DS7Y
029200941025     C*
029300041109     C* ARR.:INVIO PARTEN
029400990804     C                   MOVEL     §7YAPF        Z7YAPF            1
029500041109     C* ARR.:INVIO SEDE
029600990804     C                   MOVEL     §7YASF        Z7YASF            1
029700941025     C*
029800041109     C* PAR.:INVIO SEDE
029900990804     C                   MOVEL     §7YPSF        Z7YPSF            1
030000941025     C*
030100041109     C* TRAN:INVIO PARTEN
030200990804     C                   MOVEL     §7YTPF        Z7YTPF            1
030300041109     C* TRAN:INVIO SEDE
030400990804     C                   MOVEL     §7YTSF        Z7YTSF            1
030500941025     C**
030600970826     C* SE SONO IN SEDE QUESTE ULTIME 3 COSE NON
030700941025     C*  LE FACCIO PERCHE' NON C'E' TRASMISSIONE
030800941025     C**
030900070706     C***  D39TBO        IFNE      ' '
031000941025     C**
031100941025     C* LETTURA PERCENTUALE SCOSTAMENTO
031200941025     C**
031300070706     C***                MOVEL     '3I'          COD
031400070706     C***                MOVEL(P)  '1'           KEY
031500070706     C***  KTAB          CHAIN     TABEL00F                           32
031600070706IF  1C***  *IN32         IFEQ      '0'
031700070706     C***                MOVEL     TBLUNI        DS3I
031800070706E   1C***                ENDIF
031900070706     C***                ENDIF
032000950111     C*
032100941025     C                   ENDSR
032200990730     C* GESTIONE TRASMISSIONE PESO CML -----------------------------*
032300941025     C     GESTRA        BEGSR
032400941025     C*
032500070706     C***                SETOFF                                       06
032600941025     C**
032700041109     C* Aggiornamenti in partenza
032800941025     C**
032900041104IF  1C     WINFI         IFne      ' '
033000941025     C*
033100041104     C* SE PESO TOTALE sempre
033200941025     C*
033300070706IF  2C***  WFVF          IFEQ      'T'
033400070706     C***                SETON                                        06
0335000411091
033600041104     C* se   PARZIALE solo se > tenendo conto anche dello scostamento
033700070706X   2C***                ELSE
033800941025     C*
033900990730     C* PESO NUOVO DEVE ESSERE MAGGIORE DEL VECCHIO
034000070706IF  3C***  D39PLU        IFGT      D39PLF
034100941025     C*
034200941025     C* PULISCO CAMPI DI COMODO
034300070706     C***                Z-ADD     *ZEROS        COMPES
034400070706     C***                Z-ADD     *ZEROS        PESCON
034500941025     C*
034600941025     C* TRASFORMO IN VALORE ASSOLUTO LA PERCENTUALE
034700070706     C***  D39PLF        MULT      §3IPSC        COMPES            5 3
034800070706     C***  COMPES        DIV       100           COMPES            5 3
034900941025     C*
035000990730     C* PESO VECCHIO + LA % DI SCOSTAMENTO
035100070706     C***  D39PLF        ADD       COMPES        PESCON            5 3
035200941025     C*
035300990730     C* NUOVO PESO DEVE ESSERE MAGGIORE DEL VECCHIO + % SCOSTAMENTO
035400070706IF  4C***  D39PLU        IFGT      PESCON
035500070706     C***                SETON                                        06
035600070706E   4C***                ENDIF
035700070706E   3C***                ENDIF
035800070706E   2C***                ENDIF
035900941025     C*
036000041104     C* 06 ON - aggiorno
036100070706IF  2C***  *IN06         IFEQ      *ON
036200041104     C                   Z-ADD     D39AAS        agpAAS
036300041104     C                   Z-ADD     D39LNP        agpLNP
036400041104     C                   Z-ADD     D39NRS        agpNRS
036500041104     C                   Z-ADD     D39NSP        agpNSP
036600041104     C                   MOVEL     D39CBO        agpCBO
036700041104     C                   MOVEL     WFVF          agpFPC
036800041104     C                   Z-ADD     D39PLU        agpPKC
036900041104     C                   Z-ADD     D39NCP        agpNCP
037000041104     C                   TIME                    agpORV
037100041217     C                   MOVEL     dateu         agpDTV
037200041104     C                   MOVEL     WCVB          agpCVB
037300041104     C                   movel     winfi         agptbo
037400041104     c                   write     fnagp000
037500041104     c                   feod      fnagp00f
037600041104     c
037700070706E   2C***                ENDIF
037800941025E   1C                   ENDIF
037900941031     C**
038000941025     C* INVIO A  S E D E
038100941031     C**
038200941025     C* FRANCHI O ASSEGNATI
038300941031IF  1C     WINSE         IFEQ      'S'
038400941025     C*
038500041104     C* Solo la prima volta apro il membro
038600041104     c                   if        wapri=*blanks
038700941031     C                   EXSR      CAEXT
038800041104     c* 26 off - no errori
038900041104     c                   if        *in26=*off
039000041104     c                   eval      wapri='S'
039100041104     c                   else
039200041104     c                   eval      d39err='1'
039300041104     c                   endif
039400041104     c                   endif
039500941025     C*
039600941025     C* SCRIVO SOLO SE HO APERTO IL MEMBRO
039700041104IF  2C                   if        d39err=' '
039800990730     C                   Z-ADD     D39AAS        ARBAAS
039900990730     C                   Z-ADD     D39LNP        ARBLNP
040000990730     C                   Z-ADD     D39NRS        ARBNRS
040100990730     C                   Z-ADD     D39NSP        ARBNSP
040200990730     C                   MOVEL     D39CBO        ARBCBO
040300990730     C                   MOVEL     WFVF          ARBFPC
040400990730     C                   Z-ADD     D39PLU        ARBPKC
040500990730     C                   Z-ADD     D39NCP        ARBNCP
040600941031     C                   TIME                    ARBORV
040700041217     C                   MOVEL     dateu         ARBDTV
040800941031     C                   MOVEL     WCVB          ARBCVB
040900990730     C                   WRITE     FNARBPTT
041000080408     c                   feod      fnarbp0t
041100941031E   2C                   ENDIF
041200941025     C*
041300941025E   1C                   ENDIF
041400941025     C*
041500941025     C                   ENDSR
041600941031     C*---------------------------------------------------------------*
041700941031     C*                    CAEXT                                      *
041800941031     C* ROUTINE DI APERTURA MEMBRO TRASMISSIONE                       *
041900941031     C*---------------------------------------------------------------*
042000941031     C     CAEXT         BEGSR
042100041109     c                   clear                   writenta
042200040510     c* Verifico esistenza membro
042300041104     c                   exsr      ADDmembro
042400041109     c     riopen        tag
042500941031     C* OVRDBF
042600041104     C                   MOVEL     *BLANKS       COMMAN           80
042700941031     C                   MOVEA     CMD4(1)       COMMAN
042800040510     C                   CALL      'QCMDEXC'
042900941031     C                   PARM                    COMMAN
043000041104     C                   PARM                    LUNG             15 5
043100941031     C*
043200041104     C                   OPEN      FNARBP0T                             26
043300041109     c
043400041109     c                   if        writenta<2  and *in26
043500041109     c* se open non riuacita --> ritento con un dly
043600041109     C                   MOVEL     *BLANKS       COMMAN           80
043700041109     C                   MOVEA     CMDdly(1)     COMMAN
043800041109     C                   CALL      'QCMDEXC'
043900041109     C                   PARM                    COMMAN
044000041109     C                   PARM                    LUNG             15 5
044100041109     c
044200041109     c                   add       1             writenta          2 0
044300041109     c                   goto      riopen
044400041109     c                   endif
044500041109     c
044600941031     C*
044700941031     C                   ENDSR
044800041104     c*
044900041104     c* CHK e add del membro che manca ------------------------------------*
045000041104     c     ADDmembro     begsr
045100041104     C                   MOVEL     *BLANKS       FISICO           10
045200041104     C                   MOVEL     'FNARBP0T'    FISICO
045300041104     C                   MOVEL     *BLANKS       LOGICO           10
045400041104     C                   MOVEL     *BLANKS       LOGMBR           10
045500041104     C                   MOVEL     *BLANKS       FLG               1
045600041104     C*
045700041104     C                   CALL      'TRUL50C'
045800041104     C                   PARM                    FISICO
045900041104     C                   PARM                    MbrFis
046000041104     C                   PARM                    KNSIF
046100041104     C                   PARM                    LOGICO
046200041104     C                   PARM                    LOGMBR
046300041104     C                   PARM                    FLG
046400041104     c                   endsr
046500041104**         CMD4
046600041104OVRDBF FILE(FNARBP0T)               MBR(B946046)
046700041109**         CMDdly
046800041109DLYJOB (30)
