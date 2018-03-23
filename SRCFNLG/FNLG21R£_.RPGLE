000100961119     H DECEDIT('0,') DATEDIT(*DMY.)
000200961119      *---------*----------------------------------------------------*
000300961126      * FNLG21R *                                                    *
000400961119      *---------*                                                    *
000500961126      *         - ESECUZIONE DISPOSIZIONI RESO / RICONSEGNA -        *
000600961119      *--------------------------------------------------------------*
000700961126     FFNLG21D   CF   E             WORKSTN
000800961126     F                                     SFILE(LG21S01:NRR)
000900961120     F*---------
001000050222     FTIGNP01L  UF A E           K DISK
001100961202     F*---------
001200050222     FTIGCP21L  IF   E           K DISK
001300961202     F*---------
001400961202     FFNARB01L  IF   E           K DISK
001500961129     F*---------
001600961129     FCNACO00F  IF   E           K DISK
001700961119     F*---------
001800961129     FCNIND00F  IF   E           K DISK
001900961129     F*---------
002000961129     FEDTAB01L  IF   E           K DISK
002100961202     F*---------
002200980521     FAZORG01L  IF   E           K DISK
002300980521     F*---------
002400060213     FFiAR401L  IF   E           K DISK
002500071009     F*---------
002600071009     FTfntc01l  IF   E           K DISK
002700980521     F*---------
002800071009     FLETEST    O    E             PRINTER usropn
002900961129     D*--------------------------------------------------------------*
003000961129     D*  SCHIERE
003100961129     D*--------------------------------------------------------------*
003200961129     D TXT             S             50    DIM(7) CTDATA PERRCD(1)
003300070515     D ERR             S             70    DIM(3) CTDATA PERRCD(1)
003400071009     E*  QCAEXEC
003500071009     D DLT1            S             50    DIM(1) CTDATA PERRCD(1)
003600071009     D DLT2            S             50    DIM(1) CTDATA PERRCD(1)
003700071009     D OVR1            S             50    DIM(1) CTDATA PERRCD(1)
003800071009     D*--------------------------------------------------------------*
003900961129     D KSC             S              7  0 DIM(200)
004000961129     D LNA             S              3  0 DIM(200)
004100961129     D CPU             S             35    DIM(200)
004200961129     D CPT             S             35    DIM(200)
004300961202     D DPT             S             90    DIM(200)
004301120206     D CPZ             S             35    DIM(200)
004302120206     D DPZ             S             90    DIM(200)
004303120206     D exp             S              7    DIM(200)
004400961119     D*--------------------------------------------------------------*
004500961119     D*  DS
004600961119     D*--------------------------------------------------------------*
004700961119     D KPJBA         E DS
004800961219     D WLBDA8          DS
004900961219     D  G02DAT                 1      8  0
005000961219     D  G02INV                 9     16  0
005100961219     D  G02ERR                17     17
005200961219     D  G02TGI                18     22  0
005300060629     D DSBL4i        E DS
005400020717     D OG143         E DS
005500110606     D CNCR42        E DS
005600000314     D FNLG21        E DS                  EXTNAME(FNLG21DS)
005700961127     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005800961129     D  TCU                  398    697
005900961129     D                                     DIM(50)
006000961129     D  KCU                  698    847P 0
006100961129     D                                     DIM(50)
006200961129     D                                     PACKEVEN
006300071009      * invio Mail al posto del FAX
006400071009     D trul44ds      E DS
006500071009     D trtcm1ds      E DS
006600071009      *
006700961129     D TRUL43        E DS                  EXTNAME(TRUL43DS)
006800961129     D TRUL42        E DS                  EXTNAME(TRUL42DS)
006900061107     d trulDEPds     e ds
007000961202     D DSPT          E DS                  EXTNAME(EDIDSPT)
007100961202     D DSPU          E DS                  EXTNAME(EDIDSPU)
007101120206     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
007200961202     D TCUDS           DS
007300961202     D  F3                     3      3
007400961202     D  F4                     4      4
007500961202     D  F56                    5      6
007600961219     D                 DS
007700961219     D  DEDAT                  1      9    INZ('FAX  DEL ')
007800961219     D  GGFAX                 10     11  0
007900961219     D  FILGG                 12     12    INZ('/')
008000961219     D  MMFAX                 13     14  0
008100961219     D  FILMM                 15     15    INZ('/')
008200961219     D  AAFAX                 16     19  0
008300961219     D  TESTA                  1     19
008400050223     D comodo          s                   like(gnpnpr)
008500070515     d digits          c                   '0123456789'
008600961119     C*--------------------------------------------------------------*
008700961119     C*  C I C L O        P R I N C I P A L E
008800961119     C*--------------------------------------------------------------*
008900961127     C*  Caricamento subfile
009000961127     C                   EXSR      CARS01
009100961127     C*
009200961127     C*  Gestisco subfile fino a che non viene premuto F12 o F6
009300961127     C     WFINE         DOWEQ     'N'
009400961127     C     WTPVID        CASEQ     '1'           GESS01
009500961127     C                   END
009600961127     C                   END
009700071010      *
009800961127     C*  Se ho premuto F6 stampo fax
009900961127     C     *INKF         IFEQ      '1'
010000071009     C                   EXSR      CTRMail
010100071009     c                   if        Invio_Mail = 'S'
010200071009      ** solo x E-mail
010300071009     c                   exsr      OVRPRTF_x_Mail
010400071009     c                   end
010500071009      ***
010600961202     C                   EXSR      STAFAX
010700961127     C                   END
010800961127     C*  Fine pgm
010900961127     C     FINE          TAG
011000071009     C*   Prima di tutto chiude il PRTF
011100071009     c                   exsr      Close_PRTF
011200071009      *
011300961127     C                   SETON                                        LR
011400961127     C*--------------------------------------------------------------*
011500961127     C*  CARS01: Eseguo caricamento subfile
011600961127     C*--------------------------------------------------------------*
011700961127     C     CARS01        BEGSR
011800961127     C*
011900970108     C                   SETOFF                                       1011
012000970107     C* Inizializzo subfile
012100970107     C                   EXSR      INISBF
012200970107     C* Se vengo dall'esecuzione disposizioni di giacenza imposto
012300970107     C* subfile dalle note giacenza
012400961202     C     D21TPR        IFEQ      'G'
012500970107     C                   EXSR      REPPRG
012600970108     C     *IN30         IFEQ      '0'
012700970107     C                   EXSR      GIAFAX
012800970108     C                   ELSE
012900970108     C                   EXSR      CARRIG
013000970108     C                   END
013100970107     C* Carico righe vuote
013200970108     C                   ELSE
013300970107     C                   EXSR      CARRIG
013400970108     C                   END
013500961127     C*
013600970108     C                   ENDSR
013700970107     C*--------------------------------------------------------------*
013800970107     C*  INISBF: Inizializzo subfile
013900970107     C*--------------------------------------------------------------*
014000970107     C     INISBF        BEGSR
014100970107     C* Pulisco subfile
014200970107     C                   SETOFF                                       2021
014300970107     C                   WRITE     LG21C01
014400970107     C                   SETON                                        2021
014500970107     C* Carico 5 righe di subfile
014600970107     C                   SETOFF                                       04
014700970108     C                   Z-ADD     0             NRR
014800970107     C*
014900970107     C                   ENDSR
015000970107     C*--------------------------------------------------------------*
015100970107     C*  CARRIG: CAricamento righe vuote
015200970107     C*--------------------------------------------------------------*
015300970107     C     CARRIG        BEGSR
015400970107     C*
015500970107     C* Se sto trattando il reso carico le descrizioni dal testo
015600970107     C* fisso
015700970107     C     D21DIS        IFEQ      'R'
015800970107     C     NRR           IFEQ      0
015900970107     C                   Z-ADD     0             NRR               4 0
016000970107     C     3             DO        7             X
016100970107     C                   ADD       1             NRR
016200970107     C                   MOVEL     TXT(X)        V1CNOT
016300970107     C                   WRITE     LG21S01
016400970107     C                   END
016500970107     C                   END
016600970107     C*
016700970107     C* Se sto esaminando una riconsegna controllo se ho mem.
016800970107     C* nelle note il testo libero
016900970107     C                   ELSE
017000970107     C                   ADD       1             NRR
017100970107     C                   MOVEL     TXT(1)        V1CNOT
017200970107     C                   WRITE     LG21S01
017300970107     C* Carico le righe restanti ad arrivare a 5
017400970107     C     4             SUB       NRR           WDACAR            4 0
017500970107     C                   DO        WDACAR
017600970107     C                   ADD       1             NRR
017700970107     C                   MOVEL     *BLANKS       V1CNOT
017800970107     C                   WRITE     LG21S01
017900970107     C                   END
018000970107     C                   ADD       1             NRR
018100970107     C                   MOVEL     TXT(2)        V1CNOT
018200970107     C                   WRITE     LG21S01
018300970107     C*
018400970107     C                   END
018500970108     C*
018600970108     C                   ENDSR
018700961127     C*--------------------------------------------------------------*
018800961127     C*  GESS01: Gestione subfile
018900961127     C*--------------------------------------------------------------*
019000961127     C     GESS01        BEGSR
019100961127     C*
019200970108     C*  Se è stato inviato più di un fax accendo 10
019300970108     C     WNPROG        COMP      5                                  10
019400961127     C*  Eseguo exfmt
019500961202     C                   WRITE     LG21Z01
019600961202     C                   EXFMT     LG21C01
019700961129     C                   SETOFF                                       28
019800970107     C*  Richiesto F10 ---> fax precedente
019900970107     C     *INKJ         IFEQ      '1'
020000970107     C                   SUB       5             KNPR
020100970108     C                   SETON                                        02
020200970107     C                   EXSR      INISBF
020300970108     C                   EXSR      GIAFAX
020400970108     C                   SETON                                        11
020500970107     C                   GOTO      FINVD1
020600970107     C                   END
020700050223     C*  Richiesto F11 ---> ultimo fax
020800970107     C     *INKK         IFEQ      '1'
020900970107     C                   Z-ADD     WULFAX        KNPR
021000970108     C                   SETOFF                                       02
021100970107     C                   EXSR      INISBF
021200970108     C                   EXSR      GIAFAX
021300970108     C                   SETOFF                                       11
021400970107     C                   GOTO      FINVD1
021500970107     C                   END
021600970107     C*  Controllo se il numero del fax è corretto
021700970107     C                   EXSR      CTRFAX
021800961127     C*  Richiesto F12 o F6 --> fine pgm
021900961127     C     *INKL         IFEQ      '1'
022000961129     C                   MOVEL     'S'           WFINE
022100961129     C                   GOTO      FINVD1
022200961129     C                   END
022300961129     C  N28*INKF         IFEQ      '1'
022400961127     C                   MOVEL     'S'           WFINE
022500961127     C                   END
022600961127     C*
022700961202     C     FINVD1        ENDSR
022800961127     C*--------------------------------------------------------------*
022900961127     C*  Stampa fax e aggiorna note giacenza
023000961127     C*--------------------------------------------------------------*
023100961202     C     STAFAX        BEGSR
023200961127     C*
023300961129     C*  Imposto dati spedizione in stampa
023400961129     C                   EXSR      DATSPE
023500961127     C*  Eseguo stampa fax
023600961203     C                   Z-ADD     1             STPPAG
023700071010     c*
023800071010     c                   if        Invio_Mail <>'S'
023900961203     C                   WRITE     TESFAX
024000071010     c*
024100961203     C                   WRITE     TESGEN
024200980521     C   23              WRITE     TESAWB
024300000320     C     D21DPD        IFEQ      'S'
024400000320     C                   WRITE     RIFDPD
024500000320     C                   END
024600000320     C                   WRITE     TESSPE
024700961203     C                   WRITE     SOGSPE
024800961202     C     D21TPR        IFEQ      'G'
024900961129     C                   WRITE     TESGIA
025000961129     C                   END
025100961202     C*  Stampo testo libero
025200961202     C                   WRITE     INITXT
025300961202     C                   EXSR      STPTXT
025400071010     C*
025500071010     c                   else
025600071010     C* se si manda una Mail
025700071010     C* rispetto al FAX le righe sono posizionate diversamente sulla pagina
025800071010     c*
025900071010     C                   WRITE     TESGENM
026000071010     C   23              WRITE     TESAWBM
026100071010     C     D21DPD        IFEQ      'S'
026200071010     C                   WRITE     RIFDPDM
026300071010     C                   END
026400071010     C                   WRITE     TESSPEM
026500071010     C                   WRITE     SOGSPEM
026600071010     C     D21TPR        IFEQ      'G'
026700071010     C                   WRITE     TESGIAM
026800071010     C                   END
026900071010     C*  Stampo testo libero
027000071010     C                   WRITE     INITXT
027100071010     C                   EXSR      STPTXT
027200071010     c*
027300071010     c                   end
027400071010     C*
027500961202     C*  Se vengo da giacenza e eseguo riconsegna mem. testo
027600961202     C     D21TPR        IFEQ      'G'
027700961202     C                   EXSR      AGGNOT
027800961202     C                   END
027900071010     C*
028000961127     C                   ENDSR
028100961129     C*--------------------------------------------------------------*
028200961129     C*  Controllo se è valido il numero del fax
028300961129     C*--------------------------------------------------------------*
028400961129     C     CTRFAX        BEGSR
028500961129     C*
028600000314     C                   Z-ADD     1             X
028700961129     C*  Eseguo posizionamento sulla bolla
028800961129     C                   Z-ADD     D21AAS        KAAS
028900961129     C                   Z-ADD     D21LNP        KLNP
029000961129     C                   Z-ADD     D21NRS        KNRS
029100961129     C                   Z-ADD     D21NSP        KNSP
029200961202     C     KARB          CHAIN     FNARB01L                           30
029300000314     C*  Se sono una filiale di arrivo estera (no DPD) reperisco
029400000314     C*  il codice dell'anagrafica in base alla tabella Pt
029500000314     C     D21DPD        IFNE      'S'
029600961129     C*  Se vengo da giacenze
029700961129     C*  eseguo posizionamento su giacenza
029800961202     C     D21TPR        IFEQ      'G'
029900970418     C                   Z-ADD     D21FRG        KFRG
030000050222     C     KGCA          CHAIN     TIGCP21L                           31
030100961129     C*  In base alla filiale giac. reperisco il codice partner
030200961129     C     GCPFGC        LOOKUP    LNA(X)                                 32
030300961129     C                   ELSE
030400961129     C*  In base alla lna della bolla reperisco il codice partner
030500961202     C     ARBLNA        LOOKUP    LNA(X)                                 32
030600961129     C                   END
030700961129     C*
030800961129     C*  Reperisco i dati anagrafici del partner
030900961129     C                   Z-ADD     KSC(X)        KKSC
030901120206     c**
030902120206      **  se presente un codice specifico di Export prende questo per
030903120206      **  reperire il FAX o l'indirizzo della mail
030904120206     c                   if        exp(X) <> *all'0'
030905120207     c     digits        check     exp(X)
030906120207     c                   if        not %Found
030907120207     C                   move      exp(X)        KKSC
030908120207     c                   end
030910120206     c                   end
030911120206     c**
031000961202     C                   MOVEL     DPT(X)        DSPT
031100000314     C*  ... altrimenti se lna è DPD compongono il codice in base
031200000314     C*      alla lna + codice di AR4
031300000314     C                   ELSE
031400070515      *?   per DPD    ?
031500000314     C                   Z-ADD     D21AAS        KAAS
031600000314     C                   Z-ADD     D21LNP        KLNP
031700000314     C                   Z-ADD     D21NRS        KNRS
031800000314     C                   Z-ADD     D21NSP        KNSP
031900060629     C                   MOVEL     'I'           KTRC
032000000314     C                   CLEAR                   AR4NOT
032100060213     C     KAR4          CHAIN     FiAR4000                           34
032200060629     C                   MOVEL     AR4NOT        DSBL4I
032300000314     C                   Z-ADD     0             KKSC
032400061107      ***
032500061107      ***   (Prima lo componeva con la linea DPD + il depot di arrivo)
032600061107     C******             MOVEL     ARBLNA        KKSC
032700061107     C******             MOVE      §B4ida        kksc
032800061107      ***     adesso Attenzione:
032900061107      ***  il cod.cliente DPD deve essere reperito utilizzando la tabella
033000061107      ***   dei Depot su cui sono codificati i codici clienti x depot DPD
033100061107     C                   clear                   TRULDEPds
033200061107     c                   move      *zeros        IDEPDPC
033300061107     c                   move      §B4ida        IDEPDPC
033400061107     c                   move      'L'           IDEPTLA
033500061107      ** data della spedizione
033600061107     c                   move      arbMGS        IDEPDRF
033700061107     c                   movel     arbAAS        IDEPDRF
033800061107      **
033900061107     C                   CALL      'TRULDEPR'
034000061107     C                   PARM                    TRULDEPds
034100061107      * tutto OK trovato il cliente
034200061107     c                   if        odepERR <>*blank or OdepKSC =0
034300061107      ** se trovati errori nella decodifica del Depot o manca il Cod.Cliente
034400061107      ** deve avvisare dell'errore del mancato reperimento sul DPPDC00F
034500061107      ** del codice del cliente x poter trovare il nr.FAX
034600061107     C                   SETON                                        28
034700061107     C                   MOVEL     ERR(2)        $MSG
034800061107     c                   else
034900061107     c                   z-add     ODEPKSC       kksc
035000061107     c                   end
035100061107      ***
035200060629     C                   MOVEL     §B4ipn        STPRIF
035300070515      ***
035400070515      * ?-------------------------------------------------------
035500070515      * ?controllo validità
035600070515      * ? campo Deposito  -->  contenente solo digits   OK= not found.
035700070515     c                   clear                   dep_num           4 0
035800070515     c                   clear                   ChronoPost        1
035900070515     c     digits        check     §b4IDA
036000070515     c                   if        not %Found
036100070515     c                   move      §b4IDA        dep_num
036200070515     c                   end
036300070515      * ?------------------------------------------------------
036400070515      * ?x gestire vecchia CRONOPOST prima dell'avvio di EXAPAC dal 21/5/07.
036500070515      * ? [prima era tutto Cronopost (Francia)].
036600070515      * ?------------------------------------------------------
036700070515      * ? Nuovi Depots EXAPAC codice fra 1000 e 1100.
036800070515     c                   if        dep_num >= 1000 and dep_num < 1100 and
036900070524      * ? e se Linea 195 trasmessa con Manifest prima del 28/5/2007
037000070524     c                             arbLNA = 195 and arbDT3 < 20070528
037100070515     c                   move      'S'           ChronoPost
037200070515     c                   end
037300070515      * ?-------------------------------------------------------
037400070525      *  ?   [Chiodo attivato x FAX sempre x 1950010]    ?
037500070525      * ? [Non segnala Cronopost e manda sempre a Exapac].
037600070525     c                   if        arbLNA = 195 and arbDT3 >= 20070528
037700070525     c                   move      ' '           ChronoPost
037800070525     c                   z-add     1950010       kKSC
037900070525     c                   end
038000070515      ***
038100070525      * ?-------------------------------------------------------
038200070525      ***
038300000314     C                   END
038400070515      *
038500000314     c* Aggancio anagrafica
038600961129     C                   MOVEL     *BLANKS       ACORAG
038700000314     C                   Z-ADD     1             KKUT
038800000314     C                   Z-ADD     CLIITA        KKCC
038900961129     C     KACO          CHAIN     CNACO00F                           31
039000961129     C                   MOVEL     ACORAG        STPPAT
039100961129     C*  Controllo se esiste un luogo di invio comunicazione di giac.
039200961129     C                   CLEAR                   TRUL43
039300961129     C                   Z-ADD     KSC(X)        D43KSC
039301120206      **
039302120206      **   se si tratta di un Partner con codice EXPORT impostato
039303120206      **   per un invio ad indirizzo particolare differente da quello x IMPORT.
039304120206     c                   if        exp(X) <> *all'0'
039305120207     c     digits        check     exp(X)
039306120207     c                   if        not %Found
039308120207     C                   move      exp(X)        D43KSC
039309120207     c                   end
039310120206     c                   end
039311120206      **
039400961129     C                   CALL      'TRUL43R'
039500961129     C                   PARM                    TRUL43
039600961129     C     D43TRV        IFNE      'S'
039700961129     C                   MOVEL     *BLANKS       STPFAX
039800961129     C     KIND          CHAIN     CNIND00F                           31
039900961129     C                   MOVEL     INDTLF        STPFAX
040000961129     C                   ELSE
040100961202     C                   MOVEL     D43FAX        STPFAX
040200961129     C                   END
040300961129     C*  Controllo validità numero fax e se non è corretto non consento
040400961129     C*  di confermare
040500961129     C                   CLEAR                   TRUL42
040600961129     C                   MOVEL     STPFAX        D42FAX
040700961129     C                   CALL      'TRUL42R'
040800961129     C                   PARM                    TRUL42
040900961129     C     D42ERR        IFEQ      '1'
041000961129     C                   SETON                                        28
041100961129     C                   MOVEL     ERR(1)        $MSG
041200961129     C                   END
041300070515     C*
041400070515      *?   per DPD e Chronopost prima di EXAPAC              ?
041500070515      *?   Si deve inviare il FAX manualmente a Chronopost   ?
041600070515     C                   If        D21dpd = 'S' and ChronoPost = 'S'
041700070515     C                   SETON                                        28
041800070515     C                   MOVEL     ERR(3)        $MSG
041900070515     c                   end
042000961129     C*
042100961129     C                   ENDSR
042200961129     C*--------------------------------------------------------------*
042300961129     C*  Reperisco dati della spedizione in stampa
042400961129     C*--------------------------------------------------------------*
042500961129     C     DATSPE        BEGSR
042600961129     C*
042700961129     C                   MOVE      WHHDAT        STPOGG
042800080219     C****               Z-ADD     D21AAS        STPAAS
042900080219     C                   move      D21AAS        STPAAS
043000961202     C                   Z-ADD     D21LNP        STPLNP
043100961129     C                   Z-ADD     D21NRS        STPNRS
043200961129     C                   Z-ADD     D21NSP        STPNSP
043300970625     C     ARBDT3        IFEQ      0
043400961129     C                   MOVEL     ARBMGS        WMM               2 0
043500961129     C                   MOVE      ARBMGS        WGG               2 0
043600961129     C                   MOVEL     WGG           WGGMM             4 0
043700961129     C                   MOVE      WMM           WGGMM
043800961129     C                   MOVEL     WGGMM         STPDSP
043900961129     C                   MOVE      ARBAAS        STPDSP
044000970625     C                   ELSE
044100970625     C                   Z-ADD     ARBDT3        G02INV
044200970625     C                   MOVEL     '3'           G02ERR
044300970625     C                   CALL      'XSRDA8'
044400970625     C                   PARM                    WLBDA8
044500970625     C                   Z-ADD     G02DAT        STPDSP            8 0
044600970625     C                   END
044700961129     C*  Imposto dati giacenza se spedizione giacente
044800961129     C     D21TPR        IFEQ      'G'
044900961129     C                   Z-ADD     D21FGC        STPFGC
045000961129     C                   Z-ADD     D21NGC        STPNGC
045100080219     C*****              Z-ADD     D21AAS        STPAA1
045200080219     C                   move      D21AAS        STPAA1
045300961129     C                   Z-ADD     D21LNP        STPLP1
045400961129     C                   Z-ADD     D21NRS        STPNR1
045500961129     C                   Z-ADD     D21NSP        STPNS1
045600961129     C                   MOVE      D21AGC        STPDGC
045700961129     C                   MOVEL     GCPMGC        WMM               2 0
045800961129     C                   MOVE      GCPMGC        WGG               2 0
045900961129     C                   MOVEL     WGG           WGGMM             4 0
046000961129     C                   MOVE      WMM           WGGMM
046100961129     C                   MOVEL     WGGMM         STPDGC
046200961129     C                   END
046300980205     C                   Z-ADD     ARBNCL        STPNCL
046400990730     C                   Z-ADD     ARBPKF        STPPKG
046500961129     C* Reperisco i dati del destinatario dalla bolla
046600961129     C                   MOVEL     ARBRSD        STPRSD
046700961129     C                   MOVEL     ARBIND        STPIND
046800961129     C                   MOVEL     ARBCAD        STPCAD
046900961202     C                   MOVEL     ARBLOD        STPCID
047000961129     C                   MOVEL     ARBPRD        STPPRD
047100961129     C                   MOVEL     ARBNZD        STPNZD
047200961129     C* Preimposto i dati del mittente dalla bolla
047300961129     C                   MOVEL     ARBRSM        STPRSM
047400961129     C                   MOVEL     ARBINM        STPINM
047500961129     C                   MOVEL     ARBCAM        STPCAM
047600961202     C                   MOVEL     ARBLOM        STPCIM
047700961129     C                   MOVEL     ARBPRM        STPPRM
047800961129     C                   MOVEL     ARBNZM        STPNZM
047900000314     C     D21DPD        IFEQ      'S'
047901120117      ** la lunghezza del nostro riferimento bolla deve essere preso dalla
047902120117      ** lunghezza rilevata nel riferimento AGE sul quale si basa tutto l'EDI.
047903120117      **   NON da quanto passato nel CNI.(che comunque coincide)
047904120117      **   quello che fa testo è quanto messo nel RFF+AGE:ns.sped.
047905120117      **  (Ricordo che in EDI il riferimento deve essere max lungo 15)
048000120117     C*****§PTNSP        ORLT      16
048001120117     C     §PTAGE        ORLT      16
048100080219     C****               MOVEL     '00'          STPAAS
048200080219     C****               MOVEL     '00'          STPAA1
048300080219     C                   MOVEL     '  '          STPAAS
048400080219     C                   MOVEL     '  '          STPAA1
048500961202     C                   END
048600980521      * Se SDI ricavo AWB
048700980521     C                   EXSR      RICAWB
048800980521      *
048900961129     C                   ENDSR
049000980521      *--------------------------------------------------------------*
049100980521      *  Stampo testo libero
049200980521      *--------------------------------------------------------------*
049300980521     C     RICAWB        BEGSR
049400980521      *
049500980521     C                   MOVEL     *BLANKS       STPAWB
049600980521      *
049700980521      *  Se vengo da giacenze eseguo posizionamento con fil. giacenza
049800980521     C     D21TPR        IFEQ      'G'
049900980521     C     GCPFGC        CHAIN     AZORG01L                           23
050000980521     C                   ELSE
050100980521     C     ARBLNA        CHAIN     AZORG01L                           23
050200980521     C                   END
050300020717     C**** sostituito con il campo Network la gestione dei vecchi flags
050400020717     C  N23              MOVEL     ORGDE3        OG143
050500980521      *
050600020717     C     *IN23         IFEQ      '0'
050700020717      *  Estero+SDI era Europolitan
050800020717     C     §OgNTW        andeq     'EUP'
050900980521     C                   SETON                                        23
051000980521     C                   ELSE
051100980521     C                   SETOFF                                       23
051200980521     C                   ENDIF
051300980521      *
051400980521     C     *IN23         IFEQ      '1'
051500980521     C                   Z-ADD     D21AAS        KAAS
051600980521     C                   Z-ADD     D21LNP        KLNP
051700980521     C                   Z-ADD     D21NRS        KNRS
051800980521     C                   Z-ADD     D21NSP        KNSP
051900980521     C                   MOVEL     'E'           KTRC
052000060213     C     KAR4          CHAIN     FiAR4000                           34
052100980521     C  N34              MOVEL     AR4NOT        STPAWB
052200980521     C                   ENDIF
052300980521      *
052400980521     C                   ENDSR
052500980521     C*--------------------------------------------------------------*
052600980521     C*  Stampo testo libero
052700980521     C*--------------------------------------------------------------*
052800980521     C     STPTXT        BEGSR
052900980521     C*
053000961202     C*  Stampo testo libero
053100961202     C                   Z-ADD     1             NRR
053200961202     C     NRR           CHAIN     LG21S01                            29
053300961202     C     *IN29         DOWEQ     '0'
053400961202     C     V1CNOT        IFNE      *BLANKS
053500961202     C                   MOVEL     *BLANKS       NOTE
053600961202     C                   MOVEL     V1CNOT        NOTE
053700961202     C                   WRITE     TESLIB
053800961202     C                   END
053900961202     C                   ADD       1             NRR
054000961202     C     NRR           CHAIN     LG21S01                            29
054100961202     C                   END
054200961202     C*
054300961202     C                   ENDSR
054400961202     C*--------------------------------------------------------------*
054500961202     C*  Aggiorno note consegna
054600961202     C*--------------------------------------------------------------*
054700961202     C     AGGNOT        BEGSR
054800961202     C*
054900970108     C*  Reperisco nr. progre. iniziale
055000970108     C     WNRFAX        MULT      5             WNPR
055100961202     C*  5  Righe libere
055200961202     C                   Z-ADD     1             NRR
055300050222     C                   CLEAR                   TIGNP000
055400961202     C     NRR           CHAIN     LG21S01                            29
055500961202     C     *IN29         DOWEQ     '0'
055600961202     C     V1CNOT        IFNE      *BLANKS
055700961202     C                   Z-ADD     D21AGC        GNPAGC
055800961202     C                   Z-ADD     D21FGC        GNPFGC
055900961202     C                   Z-ADD     D21NGC        GNPNGC
056000961202     C                   Z-ADD     0             GNPFRG
056100961219     C                   Z-ADD     35            GNPFAS
056200961202     C                   MOVEL     'I'           GNPTPN
056300050222     C                   MOVEL(p)  V1CNOT        GNPDMC
056400961202     C                   ADD       1             WNPR              5 0
056500961202     C                   Z-ADD     WNPR          GNPNPR
056600050222     C                   Z-ADD     woggi         GNPDIF
056700990628     C                   Z-ADD     0             GNPNRI
056800050222     C                   WRITE     TIGNP000
056900961202     C                   END
057000961202     C                   ADD       1             NRR
057100961202     C     NRR           CHAIN     LG21S01                            29
057200961202     C                   END
057300961202     C*
057400071011      *? imposta la riga per memorizzare l'invio FAX o MAIL  ?
057500071011     C                   MOVEL(p)  '*****'       GNPDMC
057600071011     C                   MOVE      'SENT BY FAX 'GNPDMC
057700071011     c                   if        Invio_Mail = 'S'
057800071011     C                   MOVE      'MAIL'        GNPDMC
057900071011     c                   end
058000071011     C                   WRITE     TIGNP000
058100071011     C*
058200961202     C                   ENDSR
058300970107     C*--------------------------------------------------------------*
058400970107     C*  Caricamento fax da giacenza
058500970107     C*--------------------------------------------------------------*
058600970107     C     GIAFAX        BEGSR
058700970107     C*
058800970107     C                   Z-ADD     D21AGC        KAGC
058900970107     C                   Z-ADD     D21FGC        KFGC
059000970107     C                   Z-ADD     D21NGC        KNGC
059100970418     C                   Z-ADD     D21FRG        KFRG
059200970107     C                   Z-ADD     35            KFAS
059300050222     C     KGNA2         SETLL     TIGNP01L
059400050222     C     KGNA1         READE     TIGNP01L                               30
059500970107     C     *IN30         IFEQ      '0'
059600071011      ****
059700071011      **   non carico se la riga è quella che indica il tipo di invio
059800071011      **   e lo memorizzo per esporlo in testata della finestra.
059900071011      ****
060000071011     c                   movel     gnpDMC        asterischi        5
060100071011     c                   move      gnpDMC        tipo_Invio       12
060200071011     c                   movel     tipo_Invio    sent              4
060300071011     c                   if        asterischi = *all'*' and sent = 'SENT'
060400071011     C     KGNA1         READE     TIGNP01L                               30
060500071011     c                   end
060600071011      ****
060700970107     C* Imposto in testata data fax
060800050222     C                   MOVE      GNPDIF        GGFAX
060900050222     C                   MOVE      GNPDIF        WMMGG             4 0
061000970107     C                   MOVEL     WMMGG         MMFAX
061100050222     C                   MOVEL     GNPDIF        AAFAX
061200970107     C                   MOVEL     TESTA         V1CTES
061300050223     c     gnpnpr        add       5             comodo
061400970107     C*
061500970107     C     *IN30         DOWEQ     '0'
061600970107     C     NRR           ANDLT     5
061700050223     C     gnpnpr        ANDLe     comodo
061800970107     C                   SETON                                        04
061900970108     C                   Z-ADD     GNPNPR        WNPROG            5 0
062000970107     C                   ADD       1             NRR
062100970107     C                   MOVEL     GNPDMC        V1CNOT
062200970107     C                   WRITE     LG21S01
062300071011      ****
062400050222     C     KGNA1         READE     TIGNP01L                               30
062500071011      ****
062600071011      ****  controlla se si tratta di MAIL o FAX
062700071011     c                   if        *in30 = *Off
062800071011     c                   movel     gnpDMC        asterischi        5
062900071011     c                   move      gnpDMC        tipo_Invio       12
063000071011     c                   movel     tipo_Invio    sent              4
063100071011     c                   move      tipo_Invio    FAX_MAIL          4
063200071011     c                   if        asterischi = *all'*' and sent = 'SENT'
063300071011     C                   MOVEL     FAX_MAIL      V1CTES
063400071011     c                   end
063500071011     c                   end
063600071011      ****
063700970107     C                   END
063800970107     C*  Carico righe mancanti
063900970107     C     5             SUB       NRR           WDACAR            4 0
064000970107     C                   DO        WDACAR
064100970107     C                   ADD       1             NRR
064200970107     C                   MOVEL     *BLANKS       V1CNOT
064300970107     C                   WRITE     LG21S01
064400970107     C                   END
064500970107     C                   END
064600970108     C*
064700970108     C                   ENDSR
064800970107     C*--------------------------------------------------------------*
064900970107     C*  Reperisco ultimo numero proggr. nota
065000970107     C*--------------------------------------------------------------*
065100970107     C     REPPRG        BEGSR
065200970107     C*
065300970108     C                   Z-ADD     0             WNRFAX            4 0
065400970108     C                   Z-ADD     0             WULFAX            5 0
065500970107     C                   Z-ADD     D21AGC        KAGC
065600970107     C                   Z-ADD     D21FGC        KFGC
065700970107     C                   Z-ADD     D21NGC        KNGC
065800970418     C                   Z-ADD     D21FRG        KFRG
065900970107     C                   Z-ADD     35            KFAS
066000050222     C     KGNA1         SETGT     TIGNP01L
066100050222     C     KGNA1         READPE    TIGNP01L                               30
066200970107     C     *IN30         IFEQ      '0'
066300970107     C                   Z-ADD     GNPNPR        WNPR
066400071011      *****
066500071011      ***** Ultima riga da non considerare
066600071011     c                   movel     gnpDMC        asterischi        5
066700071011     c                   move      gnpDMC        tipo_Invio       12
066800071011     c                   movel     tipo_Invio    sent              4
066900071011     c                   if        asterischi = *all'*' and sent = 'SENT'
067000071011     c                   sub       1             wNPR
067100071011     c                   end
067200071011      *****
067300071011      *
067400970107     C*  Reperisco nr.progressivo iniziale nota
067500970107     C     WNPR          DIV       5             WNRFAX
067600970108     C                   MVR                     WRESTO            2 0
067700970108     C     WRESTO        IFEQ      0
067800970108     C                   Z-ADD     5             WRESTO
067900050223     c* tolgo 1 al numero di fax (WNRFAX) se il resto è = 0 (progressivo=
068000050223     c* 5 o suoi multipli) perchè alla fine aggiunge 1 al numero di fax
068100050223     C                   sub       1             WNRFAX
068200970108     C                   END
068300970108     C     WRESTO        SUB       1             WSOTTR            2 0
068400970108     C     WNPR          SUB       WSOTTR        WULFAX            5 0
068500970107     C*  Imposto proggr. nr. di fax dall'ultimo
068600970107     C                   Z-ADD     WULFAX        KNPR
068700970108     C                   ADD       1             WNRFAX
068800970108     C                   END
068900970107     C*
069000970107     C                   ENDSR
069100961127     C*--------------------------------------------------------------*
069200961127     C*  Operazioni iniziali
069300961127     C*--------------------------------------------------------------*
069400961127     C     *INZSR        BEGSR
069500961127     C*
069600961119     C     *ENTRY        PLIST
069700961119     C                   PARM                    KPJBA
069800961127     C                   MOVEL     KPJBU         FNLG21
069900071009     c                   movel     D21PRF        Outq_Fax         10
070000071009      ***
070100071010     C*  Comunque chiude eventuale OVRPRTF precedente
070200071010     C*   esegue la DLTOVR
070300071010     c                   exsr      DLTOVR_LETEST
070400071009     C*
070500071009     C* quindi esegue la
070600071009     C*  OVRPRTF x FAX
070700071009     c                   z-add     50            lung
070800071009     C                   MOVEL     *BLANKS       COMMAN
070900071009     C                   MOVEA     OVR1(1)       COMMAN
071000071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
071100071009
071200071009     C                   CALL      'QCMDEXC'                            99
071300071009     C                   PARM                    COMMAN           80
071400071009     C                   PARM                    LUNG             15 5
071500961127     C*
071600071009     c                   open      LETEST
071700071009     C*
071800961127     C                   Z-ADD     1             CODUT
071900961127     C                   CALL      'X§PARUT'
072000961127     C                   PARM                    UTEDSE
072100961127     C     CODUT         CABEQ     -1            FINE
072200110606     C                   MOVEL     REC42         CNCR42
072300110606     C**                 MOVEL     RAGUT         RSUT             20
072400110606     C**                 MOVEL     RAGUT         STPRUT
072500110606     C                   MOVEL     simRGS        RSUT             20
072600110606     C                   MOVEL     simRGS        STPRUT
072700961129     C*--- RICERCA CAPOCONTI------------------------------------------*
072800130304     C                   DO        50            X                 3 0
072900961202     C                   MOVE      TCU(X)        TCUDS
073000961129     C     F56           IFEQ      'CG'
073100961129     C     F4            ANDEQ     '1'
073200961129     C     F3            ANDEQ     '0'
073300961129     C                   Z-ADD     KCU(X)        CLIITA            4 0
073400961129     C                   END
073500961129     C                   END
073600961129     C*
073700961129     C* Definisco chiave di accesso
073800961129     C     KACO          KLIST
073900961129     C                   KFLD                    KKUT
074000961129     C                   KFLD                    KKCC
074100961129     C                   KFLD                    KKSC
074200961129     C     KIND          KLIST
074300961129     C                   KFLD                    KKUT
074400961129     C                   KFLD                    KKCC
074500961129     C                   KFLD                    KKSC
074600961129     C     KGCA          KLIST
074700961129     C                   KFLD                    KAAS
074800961129     C                   KFLD                    KLNP
074900961129     C                   KFLD                    KNRS
075000961129     C                   KFLD                    KNSP
075100961129     C     KARB          KLIST
075200961129     C                   KFLD                    KAAS
075300961129     C                   KFLD                    KLNP
075400961129     C                   KFLD                    KNRS
075500961129     C                   KFLD                    KNSP
075600970107     C     KGNA1         KLIST
075700970107     C                   KFLD                    KAGC
075800970107     C                   KFLD                    KFGC
075900970107     C                   KFLD                    KNGC
076000970107     C                   KFLD                    KFRG
076100970107     C                   KFLD                    KFAS
076200970107     C     KGNA2         KLIST
076300961127     C                   KFLD                    KAGC
076400961127     C                   KFLD                    KFGC
076500961127     C                   KFLD                    KNGC
076600961127     C                   KFLD                    KFRG
076700961127     C                   KFLD                    KFAS
076800970107     C                   KFLD                    KNPR
076900980521     C     KAR4          KLIST
077000980521     C                   KFLD                    KAAS
077100980521     C                   KFLD                    KLNP
077200980521     C                   KFLD                    KNRS
077300980521     C                   KFLD                    KNSP
077400980521     C                   KFLD                    KTRC
077500961119     C* Definisco variabili
077600980521     C     *LIKE         DEFINE    AR4TRC        KTRC
077700961127     C     *LIKE         DEFINE    GNPAGC        KAGC
077800961127     C     *LIKE         DEFINE    GNPFGC        KFGC
077900961127     C     *LIKE         DEFINE    GNPNGC        KNGC
078000961127     C     *LIKE         DEFINE    GNPFRG        KFRG
078100961127     C     *LIKE         DEFINE    GNPFAS        KFAS
078200970107     C     *LIKE         DEFINE    GNPNPR        KNPR
078300961129     C     *LIKE         DEFINE    TABCOD        KCOD
078400961129     C     *LIKE         DEFINE    ACOKUT        KKUT
078500961129     C     *LIKE         DEFINE    ACOKCC        KKCC
078600961129     C     *LIKE         DEFINE    ACOKSC        KKSC
078700961129     C     *LIKE         DEFINE    ARBAAS        KAAS
078800961129     C     *LIKE         DEFINE    ARBLNP        KLNP
078900961129     C     *LIKE         DEFINE    ARBNRS        KNRS
079000961129     C     *LIKE         DEFINE    ARBNSP        KNSP
079100071009     C     *LIKE         DEFINE    NTCAPL        kcapl
079200071009     C     *LIKE         DEFINE    NTCNK1        kcnk1
079300071009     C     *LIKE         DEFINE    NTCNK2        kcnk2
079400071009     C     *LIKE         DEFINE    NTCTNT        kctnt
079500961129     C*
079600961127     C* Imposto variabili
079700961127     C                   MOVEL     'N'           WFINE             1
079800961202     C                   MOVEL     '1'           WTPVID            1
079900961129     C*
080000000314     C* Carico dati DS partner in TXT solo se no DPD
080100000314     C     D21DPD        IFNE      'S'
080200961129     C                   Z-ADD     1             X
080300961129     C                   MOVEL     'PT'          KCOD
080400961202     C     KCOD          CHAIN     EDTAB01L                           30
080500961129     C     *IN30         DOWEQ     '0'
080600961129     C     TABFLG        IFEQ      ' '
080700961129     C                   ADD       1             X
080800961202     C                   MOVEL     TABUNI        DPT(X)
080900961202     C                   MOVEL     TABUNI        DSPT
081000961129     C                   MOVEL     TABKEY        CPU(X)
081100961129     C                   MOVEL     TABKEY        CPT(X)
081101120207     C                   MOVEL     TABKEY        CPZ(X)
081200961129     C                   MOVE      '    '        CPT(X)
081300961129     C                   MOVEL     §PTKSC        KSC(X)
081400961129     C                   MOVEL     §PTLNP        LNA(X)
081500961129     C                   END
081600961202     C     KCOD          READE     EDTAB01L                               30
081700961129     C                   END
081800961129     C*
081900961129     C                   MOVEL     'PU'          KCOD
082000961202     C     KCOD          CHAIN     EDTAB01L                           30
082100961129     C     *IN30         DOWEQ     '0'
082200961129     C     TABFLG        IFEQ      ' '
082300961129     C                   Z-ADD     1             X
082400961129     C     TABKEY        LOOKUP    CPU(X)                                 32
082500961129     C     *IN32         IFEQ      '1'
082600961129     C                   MOVEL     TABUNI        DSPU
082700961129     C* Se no partner metto zero in LNA
082800961129     C     §PUTPC        IFEQ      'C'
082900961129     C                   Z-ADD     0             LNA(X)
083000961129     C                   END
083100961129     C                   END
083200961129     C                   END
083300961202     C     KCOD          READE     EDTAB01L                               30
083400961129     C                   END
083500000314     C*
083501120206     C*
083502120206     C                   MOVEL     'PZ'          KCOD
083503120206     C     KCOD          CHAIN     EDTAB01L                           30
083504120206     C     *IN30         DOWEQ     '0'
083505120206     C     TABFLG        IFEQ      ' '
083506120206     C                   Z-ADD     1             X
083507120206     C     TABKEY        LOOKUP    CPZ(X)                                 32
083508120206     C     *IN32         IFEQ      '1'
083509120206     C                   MOVEL     TABUNI        DSPZ
083510120206     C                   MOVEL     TABUNI        DPZ(X)
083511120206     C* imposta il valore del codice di Export
083512120206     C     §PzKSCexp     IFne      *blank
083513120206     C                   move      §PzKSCexp     exp(X)
083514120206     c                   else
083515120206     C                   move      *zeros        exp(X)
083516120206     C                   END
083517120206     C                   END
083518120206     C                   END
083519120206     C     KCOD          READE     EDTAB01L                               30
083520120206     C                   END
083521120206     C*
083600000314     C                   END
083700961129     C* Reperisco data del giorno
083800961202     C                   TIME                    WHHDAT           14 0
083900961219     C                   MOVE      WHHDAT        G02DAT
084000961219     C                   MOVE      *ZEROS        G02INV
084100961219     C                   MOVEL     *BLANK        G02ERR
084200961219     C                   CALL      'XSRDA8'
084300961219     C                   PARM                    WLBDA8
084400961219     C                   Z-ADD     G02INV        WOGGI             8 0
084500980521     C*  Modalità visualizzazione
084600980521     C     D21VIS        COMP      'V'                                    12
084700980521      *
084800961127     C                   ENDSR
084900071009     C*----------------------------------------------------------------*
085000071009     C*  Controllo dati x invio mail
085100071009     C*----------------------------------------------------------------*
085200071009     C     CTRMail       BEGSR
085300071009      *
085400071009     C     Kntc          KLIST
085500071009     C                   KFLD                    kcapl
085600071009     C                   KFLD                    kcnk1
085700071009     C                   KFLD                    kcnk2
085800071009     C                   KFLD                    kctnt
085900071009      *
086000071009     c                   move      'C'           kcapl
086100071009     c                   movel     '0151'        kcnk1
086200071009     c                   move      kKSC          kcnk1
086300071009     c                   move      '88'          kctnt
086400071009     C*
086500071009     c                   move      'N'           invio_Mail        1
086600071009      * se esiste  e-mail su TFNTC nota '88'
086700071009     c     kntc          chain     tfntc01l
086800071009     c                   if        %found(tfntc01l)
086900071009     c                   move      'S'           invio_Mail
087000071009     c                   end
087100071009     C*
087200071009     C                   ENDSR
087300071009     C*----------------------------------------------------------------*
087400071009     C*  Esegue OVRPRTF x lancio E-mail
087500071009     C*----------------------------------------------------------------*
087600071009     C     OVRPRTF_x_mailBEGSR
087700071009     C*
087800071009     C*   Prima di tutto chiude il PRTF
087900071009     c                   exsr      Close_PRTF
088000071009     C*
088100071009     C* se è riuscita la DLTOVR
088200071009     c                   IF        *in99 = *off
088300071009     C*
088400071009     C*  Quindi riapre eseguendo la giusta OVRPRTF x invio coda E-mail
088500071009     C*   con tutti i parametri tramite il TRUL44R
088600071010     C*
088700071009     c                   clear                   trul44ds
088800071009     c                   clear                   trtcm1ds
088900071009     c                   eval      d44pgm = 'FNLG21R'
089000071009     c                   eval      d44cl  = '005'
089100071009     c                   move      Kksc          d44ksc
089200071009     c                   eval      d44apl = 'C'
089300071009     c                   eval      d44nk1 = '0151' + %editc(Kksc:'X')
089400071009     c                   eval      d44tnt = '88'
089500071010     c                   eval      d44scope='*JOB'
089600071009     c                   eval      d44ovr = 'S'
089700140226     c                   eval      d44obj = '*OBJM*Further Instruction for -
089800140226     c                             exceptions Shipment Nr.: '
089801140226     c                              + %editc(d21AAS:'Z') +
089900071026     c                             ' ' + %char(d21LNP) + ' ' + %char(d21NRS) +
090000071026     c                             ' ' + %char(d21NSP)
090100071009     c                   eval      d44prtf = 'LETEST'
090200071009     c                   call      'TRUL44R'
090300071009     c                   parm                    trul44ds
090400071009     c                   parm                    trtcm1ds
090500071009      *
090600071009      **  Se c'è stato qualche problema
090700071009     c     d44err        ifne      *blank
090800071009     C**
090900071009      * se non è riuscita x qualche motivo lo rimanda come FAX
091000071009     c                   close     LETEST                               99
091100071009     C*  OVRPRTF x FAX
091200071009     c                   z-add     50            lung
091300071009     C                   MOVEL     *BLANKS       COMMAN
091400071009     C                   MOVEA     OVR1(1)       COMMAN
091500071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
091600071009
091700071009     C                   CALL      'QCMDEXC'                            99
091800071009     C                   PARM                    COMMAN           80
091900071009     C                   PARM                    LUNG             15 5
092000071009     C*
092100071009     c                   end
092200071009     C**
092300071009     c                   endIF
092400071009     C**
092500071009     C*   Apre il file dopo OVRPRTF
092600071009     c                   open      LETEST
092700071009     C*
092800071009     C                   ENDSR
092900071009     C*----------------------------------------------------------------*
093000071010     C*  Esegue la DLTOVR sul file
093100071009     C*----------------------------------------------------------------*
093200071010     C     DLTOVR_LETEST BEGSR
093300071009     C*
093400071010     C*  DLTOVR
093500071010     c                   z-add     50            lung
093600071010     C                   MOVEL     *BLANKS       COMMAN
093700071010     C                   MOVEA     dlt1(1)       COMMAN
093800071010     C                   CALL      'QCMDEXC'                            99
093900071010     C                   PARM                    COMMAN           80
094000071010     C                   PARM                    LUNG             15 5
094100071010     C*
094200071010     c                   if        *in99
094300071010     C                   MOVEA     dlt2(1)       COMMAN
094400071010     C                   CALL      'QCMDEXC'                            99
094500071010     C                   PARM                    COMMAN           80
094600071010     C                   PARM                    LUNG             15 5
094700071010     c                   end
094800071010      *
094900071010     C     end_dlt       ENDSR
095000071010     C*----------------------------------------------------------------*
095100071010     C*  Chiusura PRTF  x mail
095200071010     C*----------------------------------------------------------------*
095300071010     C     Close_PRTF    BEGSR
095400071010     C*
095500071009     C                   close     LETEST
095600071010     c                   exsr      DLTOVR_LETEST
095700071009     C*
095800071009     C                   ENDSR
095900980521     C*--------------------------------------------------------------*
096000961202**  Schiera con testi da visualizz.nella finestra dell'esec.disposizioni
096100961129REDELIVER TO:
096200961202AND PROVIDE P.O.D. AFTER DELIVERY
096300961129SEND SHIPMENT BACK TO ITALY
096400961129IMPORTANT !
096500961202AS PER OPS MANUAL THE "SHIPMENT REFERENCE" MUST
096600961129BE OUR SHIPMENT NR. IN "FREE TEXT NOTE", PLEASE
096700961129SPECIFY IT IS A RETURN SHIPMENT.
096800961202**
096900961202Numero fax errato impossibile procedere oltre
097000061107Nr.fax errato. Manca anagr.Depot in tabella Depot DPD->Avvisare CED!
097100070515Attenzione: Spedizione Chronopost - Inviare Manualmente il FAX
097200071009**         dlt1
097300071009DLTOVR FILE(LETEST)
097400071009**         dlt2
097500071009DLTOVR FILE(LETEST) LVL(*JOB)
097600071009**         Ovr1
097700071009OVRPRTF FILE(LETEST) OUTQ(
