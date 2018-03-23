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
004400120206     D CPZ             S             35    DIM(200)
004500120206     D DPZ             S             90    DIM(200)
004600120206     D exp             S              7    DIM(200)
004700170511     D CPS             S             35    DIM(200)
004800170511     D DPS             S             90    DIM(200)
004900961119     D*--------------------------------------------------------------*
005000961119     D*  DS
005100961119     D*--------------------------------------------------------------*
005200961119     D KPJBA         E DS
005300961219     D WLBDA8          DS
005400961219     D  G02DAT                 1      8  0
005500961219     D  G02INV                 9     16  0
005600961219     D  G02ERR                17     17
005700961219     D  G02TGI                18     22  0
005800060629     D DSBL4i        E DS
005900020717     D OG143         E DS
006000110606     D CNCR42        E DS
006100000314     D FNLG21        E DS                  EXTNAME(FNLG21DS)
006200961127     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006300961129     D  TCU                  398    697
006400961129     D                                     DIM(50)
006500961129     D  KCU                  698    847P 0
006600961129     D                                     DIM(50)
006700961129     D                                     PACKEVEN
006800071009      * invio Mail al posto del FAX
006900071009     D trul44ds      E DS
007000071009     D trtcm1ds      E DS
007100071009      *
007200961129     D TRUL43        E DS                  EXTNAME(TRUL43DS)
007300961129     D TRUL42        E DS                  EXTNAME(TRUL42DS)
007400061107     d trulDEPds     e ds
007500961202     D DSPT          E DS                  EXTNAME(EDIDSPT)
007600961202     D DSPU          E DS                  EXTNAME(EDIDSPU)
007700120206     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
007800170511     D DSPS          E DS                  EXTNAME(EDIDSPS)
007900961202     D TCUDS           DS
008000961202     D  F3                     3      3
008100961202     D  F4                     4      4
008200961202     D  F56                    5      6
008300961219     D                 DS
008400961219     D  DEDAT                  1      9    INZ('FAX  DEL ')
008500961219     D  GGFAX                 10     11  0
008600961219     D  FILGG                 12     12    INZ('/')
008700961219     D  MMFAX                 13     14  0
008800961219     D  FILMM                 15     15    INZ('/')
008900961219     D  AAFAX                 16     19  0
009000961219     D  TESTA                  1     19
009100050223     D comodo          s                   like(gnpnpr)
009200070515     d digits          c                   '0123456789'
009300961119     C*--------------------------------------------------------------*
009400961119     C*  C I C L O        P R I N C I P A L E
009500961119     C*--------------------------------------------------------------*
009600961127     C*  Caricamento subfile
009700961127     C                   EXSR      CARS01
009800961127     C*
009900961127     C*  Gestisco subfile fino a che non viene premuto F12 o F6
010000961127     C     WFINE         DOWEQ     'N'
010100961127     C     WTPVID        CASEQ     '1'           GESS01
010200961127     C                   END
010300961127     C                   END
010400071010      *
010500961127     C*  Se ho premuto F6 stampo fax
010600961127     C     *INKF         IFEQ      '1'
010700071009     C                   EXSR      CTRMail
010800071009     c                   if        Invio_Mail = 'S'
010900071009      ** solo x E-mail
011000071009     c                   exsr      OVRPRTF_x_Mail
011100071009     c                   end
011200071009      ***
011300961202     C                   EXSR      STAFAX
011400961127     C                   END
011500961127     C*  Fine pgm
011600961127     C     FINE          TAG
011700071009     C*   Prima di tutto chiude il PRTF
011800071009     c                   exsr      Close_PRTF
011900071009      *
012000961127     C                   SETON                                        LR
012100961127     C*--------------------------------------------------------------*
012200961127     C*  CARS01: Eseguo caricamento subfile
012300961127     C*--------------------------------------------------------------*
012400961127     C     CARS01        BEGSR
012500961127     C*
012600970108     C                   SETOFF                                       1011
012700970107     C* Inizializzo subfile
012800970107     C                   EXSR      INISBF
012900970107     C* Se vengo dall'esecuzione disposizioni di giacenza imposto
013000970107     C* subfile dalle note giacenza
013100961202     C     D21TPR        IFEQ      'G'
013200970107     C                   EXSR      REPPRG
013300970108     C     *IN30         IFEQ      '0'
013400970107     C                   EXSR      GIAFAX
013500970108     C                   ELSE
013600970108     C                   EXSR      CARRIG
013700970108     C                   END
013800970107     C* Carico righe vuote
013900970108     C                   ELSE
014000970107     C                   EXSR      CARRIG
014100970108     C                   END
014200961127     C*
014300970108     C                   ENDSR
014400970107     C*--------------------------------------------------------------*
014500970107     C*  INISBF: Inizializzo subfile
014600970107     C*--------------------------------------------------------------*
014700970107     C     INISBF        BEGSR
014800970107     C* Pulisco subfile
014900970107     C                   SETOFF                                       2021
015000970107     C                   WRITE     LG21C01
015100970107     C                   SETON                                        2021
015200970107     C* Carico 5 righe di subfile
015300970107     C                   SETOFF                                       04
015400970108     C                   Z-ADD     0             NRR
015500970107     C*
015600970107     C                   ENDSR
015700970107     C*--------------------------------------------------------------*
015800970107     C*  CARRIG: CAricamento righe vuote
015900970107     C*--------------------------------------------------------------*
016000970107     C     CARRIG        BEGSR
016100970107     C*
016200970107     C* Se sto trattando il reso carico le descrizioni dal testo
016300970107     C* fisso
016400970107     C     D21DIS        IFEQ      'R'
016500970107     C     NRR           IFEQ      0
016600970107     C                   Z-ADD     0             NRR               4 0
016700970107     C     3             DO        7             X
016800970107     C                   ADD       1             NRR
016900970107     C                   MOVEL     TXT(X)        V1CNOT
017000970107     C                   WRITE     LG21S01
017100970107     C                   END
017200970107     C                   END
017300970107     C*
017400970107     C* Se sto esaminando una riconsegna controllo se ho mem.
017500970107     C* nelle note il testo libero
017600970107     C                   ELSE
017700970107     C                   ADD       1             NRR
017800970107     C                   MOVEL     TXT(1)        V1CNOT
017900970107     C                   WRITE     LG21S01
018000970107     C* Carico le righe restanti ad arrivare a 5
018100970107     C     4             SUB       NRR           WDACAR            4 0
018200970107     C                   DO        WDACAR
018300970107     C                   ADD       1             NRR
018400970107     C                   MOVEL     *BLANKS       V1CNOT
018500970107     C                   WRITE     LG21S01
018600970107     C                   END
018700970107     C                   ADD       1             NRR
018800970107     C                   MOVEL     TXT(2)        V1CNOT
018900970107     C                   WRITE     LG21S01
019000970107     C*
019100970107     C                   END
019200970108     C*
019300970108     C                   ENDSR
019400961127     C*--------------------------------------------------------------*
019500961127     C*  GESS01: Gestione subfile
019600961127     C*--------------------------------------------------------------*
019700961127     C     GESS01        BEGSR
019800961127     C*
019900970108     C*  Se è stato inviato più di un fax accendo 10
020000970108     C     WNPROG        COMP      5                                  10
020100961127     C*  Eseguo exfmt
020200961202     C                   WRITE     LG21Z01
020300961202     C                   EXFMT     LG21C01
020400961129     C                   SETOFF                                       28
020500970107     C*  Richiesto F10 ---> fax precedente
020600970107     C     *INKJ         IFEQ      '1'
020700970107     C                   SUB       5             KNPR
020800970108     C                   SETON                                        02
020900970107     C                   EXSR      INISBF
021000970108     C                   EXSR      GIAFAX
021100970108     C                   SETON                                        11
021200970107     C                   GOTO      FINVD1
021300970107     C                   END
021400050223     C*  Richiesto F11 ---> ultimo fax
021500970107     C     *INKK         IFEQ      '1'
021600970107     C                   Z-ADD     WULFAX        KNPR
021700970108     C                   SETOFF                                       02
021800970107     C                   EXSR      INISBF
021900970108     C                   EXSR      GIAFAX
022000970108     C                   SETOFF                                       11
022100970107     C                   GOTO      FINVD1
022200970107     C                   END
022300170511      **
022400970107     C*  Controllo se il numero del fax è corretto
022500970107     C                   EXSR      CTRFAX
022600170511      **
022700961127     C*  Richiesto F12 o F6 --> fine pgm
022800961127     C     *INKL         IFEQ      '1'
022900961129     C                   MOVEL     'S'           WFINE
023000961129     C                   GOTO      FINVD1
023100961129     C                   END
023200961129     C  N28*INKF         IFEQ      '1'
023300961127     C                   MOVEL     'S'           WFINE
023400961127     C                   END
023500961127     C*
023600961202     C     FINVD1        ENDSR
023700961127     C*--------------------------------------------------------------*
023800961127     C*  Stampa fax e aggiorna note giacenza
023900961127     C*--------------------------------------------------------------*
024000961202     C     STAFAX        BEGSR
024100961127     C*
024200961129     C*  Imposto dati spedizione in stampa
024300961129     C                   EXSR      DATSPE
024400961127     C*  Eseguo stampa fax
024500961203     C                   Z-ADD     1             STPPAG
024600170511     c*
024700170511     c*  da tab.PT/PS il controllo se il PARTNER NON deve avere
024800170511     c*    la comunicazione (MAIL o FAX)
024900170511     c                   if        ESEGUIRE_invio <> 'N'
025000170511     c*-------
025100071010     c                   if        Invio_Mail <>'S'
025200961203     C                   WRITE     TESFAX
025300071010     c*
025400961203     C                   WRITE     TESGEN
025500980521     C   23              WRITE     TESAWB
025600000320     C     D21DPD        IFEQ      'S'
025700000320     C                   WRITE     RIFDPD
025800000320     C                   END
025900000320     C                   WRITE     TESSPE
026000961203     C                   WRITE     SOGSPE
026100961202     C     D21TPR        IFEQ      'G'
026200961129     C                   WRITE     TESGIA
026300961129     C                   END
026400961202     C*  Stampo testo libero
026500961202     C                   WRITE     INITXT
026600961202     C                   EXSR      STPTXT
026700071010     C*
026800071010     c                   else
026900071010     C* se si manda una Mail
027000071010     C* rispetto al FAX le righe sono posizionate diversamente sulla pagina
027100071010     c*
027200071010     C                   WRITE     TESGENM
027300071010     C   23              WRITE     TESAWBM
027400071010     C     D21DPD        IFEQ      'S'
027500071010     C                   WRITE     RIFDPDM
027600071010     C                   END
027700071010     C                   WRITE     TESSPEM
027800071010     C                   WRITE     SOGSPEM
027900071010     C     D21TPR        IFEQ      'G'
028000071010     C                   WRITE     TESGIAM
028100071010     C                   END
028200071010     C*  Stampo testo libero
028300071010     C                   WRITE     INITXT
028400071010     C                   EXSR      STPTXT
028500071010     c*
028600071010     c                   end
028700071010     C*
028800170511     c                   ENDIF
028900170511     c*-------
029000170511     C*
029100961202     C*  Se vengo da giacenza e eseguo riconsegna mem. testo
029200961202     C     D21TPR        IFEQ      'G'
029300961202     C                   EXSR      AGGNOT
029400961202     C                   END
029500071010     C*
029600961127     C                   ENDSR
029700961129     C*--------------------------------------------------------------*
029800961129     C*  Controllo se è valido il numero del fax
029900961129     C*--------------------------------------------------------------*
030000961129     C     CTRFAX        BEGSR
030100961129     C*
030200170511     C*  Default *blank= INVIO "SI"
030300170511     c                   clear                   eseguire_invio    1
030400170511     C*
030500000314     C                   Z-ADD     1             X
030600961129     C*  Eseguo posizionamento sulla bolla
030700961129     C                   Z-ADD     D21AAS        KAAS
030800961129     C                   Z-ADD     D21LNP        KLNP
030900961129     C                   Z-ADD     D21NRS        KNRS
031000961129     C                   Z-ADD     D21NSP        KNSP
031100961202     C     KARB          CHAIN     FNARB01L                           30
031200000314     C*  Se sono una filiale di arrivo estera (no DPD) reperisco
031300000314     C*  il codice dell'anagrafica in base alla tabella Pt
031400170511     C*
031500000314     C     D21DPD        IFNE      'S'
031600170511     C*
031700961129     C*  Se vengo da giacenze
031800961129     C*  eseguo posizionamento su giacenza
031900961202     C     D21TPR        IFEQ      'G'
032000170511     C*
032100970418     C                   Z-ADD     D21FRG        KFRG
032200050222     C     KGCA          CHAIN     TIGCP21L                           31
032300961129     C*  In base alla filiale giac. reperisco il codice partner
032400961129     C     GCPFGC        LOOKUP    LNA(X)                                 32
032500961129     C                   ELSE
032600961129     C*  In base alla lna della bolla reperisco il codice partner
032700961202     C     ARBLNA        LOOKUP    LNA(X)                                 32
032800961129     C                   END
032900961129     C*
033000961129     C*  Reperisco i dati anagrafici del partner
033100961129     C                   Z-ADD     KSC(X)        KKSC
033200120206     c**
033300120206      **  se presente un codice specifico di Export prende questo per
033400120206      **  reperire il FAX o l'indirizzo della mail
033500120206     c                   if        exp(X) <> *all'0'
033600120207     c     digits        check     exp(X)
033700120207     c                   if        not %Found
033800120207     C                   move      exp(X)        KKSC
033900120207     c                   end
034000120206     c                   end
034100120206     c**
034200961202     C                   MOVEL     DPT(X)        DSPT
034300170511     c**
034400170511     c**  Sulla tab.EDIDSPS c'è  (§PSnoDISPO)
034500170511     c**   l'informazione di NON mandare la comunicazione al PARTNER
034600170511     c**    e quindi NON deve generare la LETEST.
034700170511     C                   MOVEL     DPS(X)        DSPS
034800170511     c                   movel     §PSnoDISPO    eseguire_invio
034900170511     C*   Può avere "N" per non far inviare nè mail nè fax poichè il PARTNER
035000170511     c**   come AZKAR vuole che siano immesse direttamente sul suo SITO WEB
035100170511     c**    le disposizioni e NON vuole più ricevere COMUNICAZIONI.
035200170511     c**
035300170511      * ?-------------------------------------------------------
035400000314     C*  ... altrimenti se lna è DPD compongono il codice in base
035500000314     C*      alla lna + codice di AR4
035600000314     C                   ELSE
035700070515      *?   per DPD    ?
035800000314     C                   Z-ADD     D21AAS        KAAS
035900000314     C                   Z-ADD     D21LNP        KLNP
036000000314     C                   Z-ADD     D21NRS        KNRS
036100000314     C                   Z-ADD     D21NSP        KNSP
036200060629     C                   MOVEL     'I'           KTRC
036300000314     C                   CLEAR                   AR4NOT
036400060213     C     KAR4          CHAIN     FiAR4000                           34
036500060629     C                   MOVEL     AR4NOT        DSBL4I
036600000314     C                   Z-ADD     0             KKSC
036700061107      ***
036800061107      ***   (Prima lo componeva con la linea DPD + il depot di arrivo)
036900061107     C******             MOVEL     ARBLNA        KKSC
037000061107     C******             MOVE      §B4ida        kksc
037100061107      ***     adesso Attenzione:
037200061107      ***  il cod.cliente DPD deve essere reperito utilizzando la tabella
037300061107      ***   dei Depot su cui sono codificati i codici clienti x depot DPD
037400061107     C                   clear                   TRULDEPds
037500061107     c                   move      *zeros        IDEPDPC
037600061107     c                   move      §B4ida        IDEPDPC
037700061107     c                   move      'L'           IDEPTLA
037800061107      ** data della spedizione
037900061107     c                   move      arbMGS        IDEPDRF
038000061107     c                   movel     arbAAS        IDEPDRF
038100061107      **
038200061107     C                   CALL      'TRULDEPR'
038300061107     C                   PARM                    TRULDEPds
038400061107      * tutto OK trovato il cliente
038500061107     c                   if        odepERR <>*blank or OdepKSC =0
038600061107      ** se trovati errori nella decodifica del Depot o manca il Cod.Cliente
038700061107      ** deve avvisare dell'errore del mancato reperimento sul DPPDC00F
038800061107      ** del codice del cliente x poter trovare il nr.FAX
038900061107     C                   SETON                                        28
039000061107     C                   MOVEL     ERR(2)        $MSG
039100061107     c                   else
039200061107     c                   z-add     ODEPKSC       kksc
039300061107     c                   end
039400061107      ***
039500060629     C                   MOVEL     §B4ipn        STPRIF
039600070515      ***
039700070515      * ?-------------------------------------------------------
039800070515      * ?controllo validità
039900070515      * ? campo Deposito  -->  contenente solo digits   OK= not found.
040000070515     c                   clear                   dep_num           4 0
040100070515     c                   clear                   ChronoPost        1
040200070515     c     digits        check     §b4IDA
040300070515     c                   if        not %Found
040400070515     c                   move      §b4IDA        dep_num
040500070515     c                   end
040600070515      * ?------------------------------------------------------
040700070515      * ?x gestire vecchia CRONOPOST prima dell'avvio di EXAPAC dal 21/5/07.
040800070515      * ? [prima era tutto Cronopost (Francia)].
040900070515      * ?------------------------------------------------------
041000070515      * ? Nuovi Depots EXAPAC codice fra 1000 e 1100.
041100070515     c                   if        dep_num >= 1000 and dep_num < 1100 and
041200070524      * ? e se Linea 195 trasmessa con Manifest prima del 28/5/2007
041300070524     c                             arbLNA = 195 and arbDT3 < 20070528
041400070515     c                   move      'S'           ChronoPost
041500070515     c                   end
041600070515      * ?-------------------------------------------------------
041700070525      *  ?   [Chiodo attivato x FAX sempre x 1950010]    ?
041800070525      * ? [Non segnala Cronopost e manda sempre a Exapac].
041900070525     c                   if        arbLNA = 195 and arbDT3 >= 20070528
042000070525     c                   move      ' '           ChronoPost
042100070525     c                   z-add     1950010       kKSC
042200070525     c                   end
042300070515      ***
042400070525      * ?-------------------------------------------------------
042500070525      ***
042600000314     C                   END
042700070515      *
042800000314     c* Aggancio anagrafica
042900961129     C                   MOVEL     *BLANKS       ACORAG
043000000314     C                   Z-ADD     1             KKUT
043100000314     C                   Z-ADD     CLIITA        KKCC
043200961129     C     KACO          CHAIN     CNACO00F                           31
043300961129     C                   MOVEL     ACORAG        STPPAT
043400170511      **
043500961129     C*  Controllo se esiste un luogo di invio comunicazione di giac.
043600961129     C                   CLEAR                   TRUL43
043700961129     C                   Z-ADD     KSC(X)        D43KSC
043800120206      **
043900120206      **   se si tratta di un Partner con codice EXPORT impostato
044000120206      **   per un invio ad indirizzo particolare differente da quello x IMPORT.
044100120206     c                   if        exp(X) <> *all'0'
044200120207     c     digits        check     exp(X)
044300120207     c                   if        not %Found
044400120207     C                   move      exp(X)        D43KSC
044500120207     c                   end
044600120206     c                   end
044700120206      **
044800961129     C                   CALL      'TRUL43R'
044900961129     C                   PARM                    TRUL43
045000170511      **
045100961129     C     D43TRV        IFNE      'S'
045200961129     C                   MOVEL     *BLANKS       STPFAX
045300961129     C     KIND          CHAIN     CNIND00F                           31
045400961129     C                   MOVEL     INDTLF        STPFAX
045500961129     C                   ELSE
045600961202     C                   MOVEL     D43FAX        STPFAX
045700961129     C                   END
045800170511      **
045900961129     C*  Controllo validità numero fax e se non è corretto non consento
046000961129     C*  di confermare
046100961129     C                   CLEAR                   TRUL42
046200961129     C                   MOVEL     STPFAX        D42FAX
046300961129     C                   CALL      'TRUL42R'
046400961129     C                   PARM                    TRUL42
046500961129     C     D42ERR        IFEQ      '1'
046600961129     C                   SETON                                        28
046700961129     C                   MOVEL     ERR(1)        $MSG
046800961129     C                   END
046900070515     C*
047000070515      *?   per DPD e Chronopost prima di EXAPAC              ?
047100070515      *?   Si deve inviare il FAX manualmente a Chronopost   ?
047200070515     C                   If        D21dpd = 'S' and ChronoPost = 'S'
047300070515     C                   SETON                                        28
047400070515     C                   MOVEL     ERR(3)        $MSG
047500070515     c                   end
047600961129     C*
047700961129     C                   ENDSR
047800961129     C*--------------------------------------------------------------*
047900961129     C*  Reperisco dati della spedizione in stampa
048000961129     C*--------------------------------------------------------------*
048100961129     C     DATSPE        BEGSR
048200961129     C*
048300961129     C                   MOVE      WHHDAT        STPOGG
048400080219     C****               Z-ADD     D21AAS        STPAAS
048500080219     C                   move      D21AAS        STPAAS
048600961202     C                   Z-ADD     D21LNP        STPLNP
048700961129     C                   Z-ADD     D21NRS        STPNRS
048800961129     C                   Z-ADD     D21NSP        STPNSP
048900970625     C     ARBDT3        IFEQ      0
049000961129     C                   MOVEL     ARBMGS        WMM               2 0
049100961129     C                   MOVE      ARBMGS        WGG               2 0
049200961129     C                   MOVEL     WGG           WGGMM             4 0
049300961129     C                   MOVE      WMM           WGGMM
049400961129     C                   MOVEL     WGGMM         STPDSP
049500961129     C                   MOVE      ARBAAS        STPDSP
049600970625     C                   ELSE
049700970625     C                   Z-ADD     ARBDT3        G02INV
049800970625     C                   MOVEL     '3'           G02ERR
049900970625     C                   CALL      'XSRDA8'
050000970625     C                   PARM                    WLBDA8
050100970625     C                   Z-ADD     G02DAT        STPDSP            8 0
050200970625     C                   END
050300961129     C*  Imposto dati giacenza se spedizione giacente
050400961129     C     D21TPR        IFEQ      'G'
050500961129     C                   Z-ADD     D21FGC        STPFGC
050600961129     C                   Z-ADD     D21NGC        STPNGC
050700080219     C*****              Z-ADD     D21AAS        STPAA1
050800080219     C                   move      D21AAS        STPAA1
050900961129     C                   Z-ADD     D21LNP        STPLP1
051000961129     C                   Z-ADD     D21NRS        STPNR1
051100961129     C                   Z-ADD     D21NSP        STPNS1
051200961129     C                   MOVE      D21AGC        STPDGC
051300961129     C                   MOVEL     GCPMGC        WMM               2 0
051400961129     C                   MOVE      GCPMGC        WGG               2 0
051500961129     C                   MOVEL     WGG           WGGMM             4 0
051600961129     C                   MOVE      WMM           WGGMM
051700961129     C                   MOVEL     WGGMM         STPDGC
051800961129     C                   END
051900980205     C                   Z-ADD     ARBNCL        STPNCL
052000990730     C                   Z-ADD     ARBPKF        STPPKG
052100961129     C* Reperisco i dati del destinatario dalla bolla
052200961129     C                   MOVEL     ARBRSD        STPRSD
052300961129     C                   MOVEL     ARBIND        STPIND
052400961129     C                   MOVEL     ARBCAD        STPCAD
052500961202     C                   MOVEL     ARBLOD        STPCID
052600961129     C                   MOVEL     ARBPRD        STPPRD
052700961129     C                   MOVEL     ARBNZD        STPNZD
052800961129     C* Preimposto i dati del mittente dalla bolla
052900961129     C                   MOVEL     ARBRSM        STPRSM
053000961129     C                   MOVEL     ARBINM        STPINM
053100961129     C                   MOVEL     ARBCAM        STPCAM
053200961202     C                   MOVEL     ARBLOM        STPCIM
053300961129     C                   MOVEL     ARBPRM        STPPRM
053400961129     C                   MOVEL     ARBNZM        STPNZM
053500000314     C     D21DPD        IFEQ      'S'
053600120117      ** la lunghezza del nostro riferimento bolla deve essere preso dalla
053700120117      ** lunghezza rilevata nel riferimento AGE sul quale si basa tutto l'EDI.
053800120117      **   NON da quanto passato nel CNI.(che comunque coincide)
053900120117      **   quello che fa testo è quanto messo nel RFF+AGE:ns.sped.
054000120117      **  (Ricordo che in EDI il riferimento deve essere max lungo 15)
054100120117     C*****§PTNSP        ORLT      16
054200120117     C     §PTAGE        ORLT      16
054300080219     C****               MOVEL     '00'          STPAAS
054400080219     C****               MOVEL     '00'          STPAA1
054500080219     C                   MOVEL     '  '          STPAAS
054600080219     C                   MOVEL     '  '          STPAA1
054700961202     C                   END
054800980521      * Se SDI ricavo AWB
054900980521     C                   EXSR      RICAWB
055000980521      *
055100961129     C                   ENDSR
055200980521      *--------------------------------------------------------------*
055300980521      *  Stampo testo libero
055400980521      *--------------------------------------------------------------*
055500980521     C     RICAWB        BEGSR
055600980521      *
055700980521     C                   MOVEL     *BLANKS       STPAWB
055800980521      *
055900980521      *  Se vengo da giacenze eseguo posizionamento con fil. giacenza
056000980521     C     D21TPR        IFEQ      'G'
056100980521     C     GCPFGC        CHAIN     AZORG01L                           23
056200980521     C                   ELSE
056300980521     C     ARBLNA        CHAIN     AZORG01L                           23
056400980521     C                   END
056500020717     C**** sostituito con il campo Network la gestione dei vecchi flags
056600020717     C  N23              MOVEL     ORGDE3        OG143
056700980521      *
056800020717     C     *IN23         IFEQ      '0'
056900020717      *  Estero+SDI era Europolitan
057000020717     C     §OgNTW        andeq     'EUP'
057100980521     C                   SETON                                        23
057200980521     C                   ELSE
057300980521     C                   SETOFF                                       23
057400980521     C                   ENDIF
057500980521      *
057600980521     C     *IN23         IFEQ      '1'
057700980521     C                   Z-ADD     D21AAS        KAAS
057800980521     C                   Z-ADD     D21LNP        KLNP
057900980521     C                   Z-ADD     D21NRS        KNRS
058000980521     C                   Z-ADD     D21NSP        KNSP
058100980521     C                   MOVEL     'E'           KTRC
058200060213     C     KAR4          CHAIN     FiAR4000                           34
058300980521     C  N34              MOVEL     AR4NOT        STPAWB
058400980521     C                   ENDIF
058500980521      *
058600980521     C                   ENDSR
058700980521     C*--------------------------------------------------------------*
058800980521     C*  Stampo testo libero
058900980521     C*--------------------------------------------------------------*
059000980521     C     STPTXT        BEGSR
059100980521     C*
059200961202     C*  Stampo testo libero
059300961202     C                   Z-ADD     1             NRR
059400961202     C     NRR           CHAIN     LG21S01                            29
059500961202     C     *IN29         DOWEQ     '0'
059600961202     C     V1CNOT        IFNE      *BLANKS
059700961202     C                   MOVEL     *BLANKS       NOTE
059800961202     C                   MOVEL     V1CNOT        NOTE
059900961202     C                   WRITE     TESLIB
060000961202     C                   END
060100961202     C                   ADD       1             NRR
060200961202     C     NRR           CHAIN     LG21S01                            29
060300961202     C                   END
060400961202     C*
060500961202     C                   ENDSR
060600961202     C*--------------------------------------------------------------*
060700961202     C*  Aggiorno note consegna
060800961202     C*--------------------------------------------------------------*
060900961202     C     AGGNOT        BEGSR
061000961202     C*
061100970108     C*  Reperisco nr. progre. iniziale
061200970108     C     WNRFAX        MULT      5             WNPR
061300961202     C*  5  Righe libere
061400961202     C                   Z-ADD     1             NRR
061500050222     C                   CLEAR                   TIGNP000
061600961202     C     NRR           CHAIN     LG21S01                            29
061700961202     C     *IN29         DOWEQ     '0'
061800961202     C     V1CNOT        IFNE      *BLANKS
061900961202     C                   Z-ADD     D21AGC        GNPAGC
062000961202     C                   Z-ADD     D21FGC        GNPFGC
062100961202     C                   Z-ADD     D21NGC        GNPNGC
062200961202     C                   Z-ADD     0             GNPFRG
062300961219     C                   Z-ADD     35            GNPFAS
062400961202     C                   MOVEL     'I'           GNPTPN
062500050222     C                   MOVEL(p)  V1CNOT        GNPDMC
062600961202     C                   ADD       1             WNPR              5 0
062700961202     C                   Z-ADD     WNPR          GNPNPR
062800050222     C                   Z-ADD     woggi         GNPDIF
062900990628     C                   Z-ADD     0             GNPNRI
063000050222     C                   WRITE     TIGNP000
063100961202     C                   END
063200961202     C                   ADD       1             NRR
063300961202     C     NRR           CHAIN     LG21S01                            29
063400961202     C                   END
063500961202     C*
063600071011      *? imposta la riga per memorizzare l'invio FAX o MAIL  ?
063700071011     C                   MOVEL(p)  '*****'       GNPDMC
063800170511      ***
063900170511      *  se ha inviato l'informazione oppure NO
064000170511     c                   clear                   campo16          16
064100170511     C                   MOVE      'SENT BY FAX 'campo16
064200170511      *
064300170511      ** se NON deve mandare, comunque lo memorizza
064400170511     c                   if        eseguire_INVIO ='N'
064500170511     C                   MOVEL     'NOT'         campo16
064600170511     c                   end
064700170511     C                   MOVE      campo16       GNPDMC
064800170511      **
064900170511      *  se invece del FAX era con MAIL
065000071011     c                   if        Invio_Mail = 'S'
065100071011     C                   MOVE      'MAIL'        GNPDMC
065200071011     c                   end
065300170511      **
065400071011     C                   WRITE     TIGNP000
065500071011     C*
065600961202     C                   ENDSR
065700970107     C*--------------------------------------------------------------*
065800970107     C*  Caricamento fax da giacenza
065900970107     C*--------------------------------------------------------------*
066000970107     C     GIAFAX        BEGSR
066100970107     C*
066200970107     C                   Z-ADD     D21AGC        KAGC
066300970107     C                   Z-ADD     D21FGC        KFGC
066400970107     C                   Z-ADD     D21NGC        KNGC
066500970418     C                   Z-ADD     D21FRG        KFRG
066600970107     C                   Z-ADD     35            KFAS
066700050222     C     KGNA2         SETLL     TIGNP01L
066800050222     C     KGNA1         READE     TIGNP01L                               30
066900970107     C     *IN30         IFEQ      '0'
067000071011      ****
067100071011      **   non carico se la riga è quella che indica il tipo di invio
067200071011      **   e lo memorizzo per esporlo in testata della finestra.
067300071011      ****
067400071011     c                   movel     gnpDMC        asterischi        5
067500071011     c                   move      gnpDMC        tipo_Invio       12
067600071011     c                   movel     tipo_Invio    sent              4
067700071011     c                   if        asterischi = *all'*' and sent = 'SENT'
067800071011     C     KGNA1         READE     TIGNP01L                               30
067900071011     c                   end
068000071011      ****
068100970107     C* Imposto in testata data fax
068200050222     C                   MOVE      GNPDIF        GGFAX
068300050222     C                   MOVE      GNPDIF        WMMGG             4 0
068400970107     C                   MOVEL     WMMGG         MMFAX
068500050222     C                   MOVEL     GNPDIF        AAFAX
068600970107     C                   MOVEL     TESTA         V1CTES
068700050223     c     gnpnpr        add       5             comodo
068800970107     C*
068900970107     C     *IN30         DOWEQ     '0'
069000970107     C     NRR           ANDLT     5
069100050223     C     gnpnpr        ANDLe     comodo
069200970107     C                   SETON                                        04
069300970108     C                   Z-ADD     GNPNPR        WNPROG            5 0
069400970107     C                   ADD       1             NRR
069500970107     C                   MOVEL     GNPDMC        V1CNOT
069600970107     C                   WRITE     LG21S01
069700071011      ****
069800050222     C     KGNA1         READE     TIGNP01L                               30
069900071011      ****
070000071011      ****  controlla se si tratta di MAIL o FAX
070100071011     c                   if        *in30 = *Off
070200071011     c                   movel     gnpDMC        asterischi        5
070300071011     c                   move      gnpDMC        tipo_Invio       12
070400071011     c                   movel     tipo_Invio    sent              4
070500071011     c                   move      tipo_Invio    FAX_MAIL          4
070600071011     c                   if        asterischi = *all'*' and sent = 'SENT'
070700071011     C                   MOVEL     FAX_MAIL      V1CTES
070800071011     c                   end
070900071011     c                   end
071000071011      ****
071100970107     C                   END
071200970107     C*  Carico righe mancanti
071300970107     C     5             SUB       NRR           WDACAR            4 0
071400970107     C                   DO        WDACAR
071500970107     C                   ADD       1             NRR
071600970107     C                   MOVEL     *BLANKS       V1CNOT
071700970107     C                   WRITE     LG21S01
071800970107     C                   END
071900970107     C                   END
072000970108     C*
072100970108     C                   ENDSR
072200970107     C*--------------------------------------------------------------*
072300970107     C*  Reperisco ultimo numero proggr. nota
072400970107     C*--------------------------------------------------------------*
072500970107     C     REPPRG        BEGSR
072600970107     C*
072700970108     C                   Z-ADD     0             WNRFAX            4 0
072800970108     C                   Z-ADD     0             WULFAX            5 0
072900970107     C                   Z-ADD     D21AGC        KAGC
073000970107     C                   Z-ADD     D21FGC        KFGC
073100970107     C                   Z-ADD     D21NGC        KNGC
073200970418     C                   Z-ADD     D21FRG        KFRG
073300970107     C                   Z-ADD     35            KFAS
073400050222     C     KGNA1         SETGT     TIGNP01L
073500050222     C     KGNA1         READPE    TIGNP01L                               30
073600970107     C     *IN30         IFEQ      '0'
073700970107     C                   Z-ADD     GNPNPR        WNPR
073800071011      *****
073900071011      ***** Ultima riga da non considerare
074000071011     c                   movel     gnpDMC        asterischi        5
074100071011     c                   move      gnpDMC        tipo_Invio       12
074200071011     c                   movel     tipo_Invio    sent              4
074300071011     c                   if        asterischi = *all'*' and sent = 'SENT'
074400071011     c                   sub       1             wNPR
074500071011     c                   end
074600071011      *****
074700071011      *
074800970107     C*  Reperisco nr.progressivo iniziale nota
074900970107     C     WNPR          DIV       5             WNRFAX
075000970108     C                   MVR                     WRESTO            2 0
075100970108     C     WRESTO        IFEQ      0
075200970108     C                   Z-ADD     5             WRESTO
075300050223     c* tolgo 1 al numero di fax (WNRFAX) se il resto è = 0 (progressivo=
075400050223     c* 5 o suoi multipli) perchè alla fine aggiunge 1 al numero di fax
075500050223     C                   sub       1             WNRFAX
075600970108     C                   END
075700970108     C     WRESTO        SUB       1             WSOTTR            2 0
075800970108     C     WNPR          SUB       WSOTTR        WULFAX            5 0
075900970107     C*  Imposto proggr. nr. di fax dall'ultimo
076000970107     C                   Z-ADD     WULFAX        KNPR
076100970108     C                   ADD       1             WNRFAX
076200970108     C                   END
076300970107     C*
076400970107     C                   ENDSR
076500961127     C*--------------------------------------------------------------*
076600961127     C*  Operazioni iniziali
076700961127     C*--------------------------------------------------------------*
076800961127     C     *INZSR        BEGSR
076900961127     C*
077000961119     C     *ENTRY        PLIST
077100961119     C                   PARM                    KPJBA
077200961127     C                   MOVEL     KPJBU         FNLG21
077300071009     c                   movel     D21PRF        Outq_Fax         10
077400071009      ***
077500071010     C*  Comunque chiude eventuale OVRPRTF precedente
077600071010     C*   esegue la DLTOVR
077700071010     c                   exsr      DLTOVR_LETEST
077800071009     C*
077900071009     C* quindi esegue la
078000071009     C*  OVRPRTF x FAX
078100071009     c                   z-add     50            lung
078200071009     C                   MOVEL     *BLANKS       COMMAN
078300071009     C                   MOVEA     OVR1(1)       COMMAN
078400071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
078500071009
078600071009     C                   CALL      'QCMDEXC'                            99
078700071009     C                   PARM                    COMMAN           80
078800071009     C                   PARM                    LUNG             15 5
078900961127     C*
079000071009     c                   open      LETEST
079100071009     C*
079200961127     C                   Z-ADD     1             CODUT
079300961127     C                   CALL      'X§PARUT'
079400961127     C                   PARM                    UTEDSE
079500961127     C     CODUT         CABEQ     -1            FINE
079600110606     C                   MOVEL     REC42         CNCR42
079700110606     C**                 MOVEL     RAGUT         RSUT             20
079800110606     C**                 MOVEL     RAGUT         STPRUT
079900110606     C                   MOVEL     simRGS        RSUT             20
080000110606     C                   MOVEL     simRGS        STPRUT
080100961129     C*--- RICERCA CAPOCONTI------------------------------------------*
080200130304     C                   DO        50            X                 3 0
080300961202     C                   MOVE      TCU(X)        TCUDS
080400961129     C     F56           IFEQ      'CG'
080500961129     C     F4            ANDEQ     '1'
080600961129     C     F3            ANDEQ     '0'
080700961129     C                   Z-ADD     KCU(X)        CLIITA            4 0
080800961129     C                   END
080900961129     C                   END
081000961129     C*
081100961129     C* Definisco chiave di accesso
081200961129     C     KACO          KLIST
081300961129     C                   KFLD                    KKUT
081400961129     C                   KFLD                    KKCC
081500961129     C                   KFLD                    KKSC
081600961129     C     KIND          KLIST
081700961129     C                   KFLD                    KKUT
081800961129     C                   KFLD                    KKCC
081900961129     C                   KFLD                    KKSC
082000961129     C     KGCA          KLIST
082100961129     C                   KFLD                    KAAS
082200961129     C                   KFLD                    KLNP
082300961129     C                   KFLD                    KNRS
082400961129     C                   KFLD                    KNSP
082500961129     C     KARB          KLIST
082600961129     C                   KFLD                    KAAS
082700961129     C                   KFLD                    KLNP
082800961129     C                   KFLD                    KNRS
082900961129     C                   KFLD                    KNSP
083000970107     C     KGNA1         KLIST
083100970107     C                   KFLD                    KAGC
083200970107     C                   KFLD                    KFGC
083300970107     C                   KFLD                    KNGC
083400970107     C                   KFLD                    KFRG
083500970107     C                   KFLD                    KFAS
083600970107     C     KGNA2         KLIST
083700961127     C                   KFLD                    KAGC
083800961127     C                   KFLD                    KFGC
083900961127     C                   KFLD                    KNGC
084000961127     C                   KFLD                    KFRG
084100961127     C                   KFLD                    KFAS
084200970107     C                   KFLD                    KNPR
084300980521     C     KAR4          KLIST
084400980521     C                   KFLD                    KAAS
084500980521     C                   KFLD                    KLNP
084600980521     C                   KFLD                    KNRS
084700980521     C                   KFLD                    KNSP
084800980521     C                   KFLD                    KTRC
084900961119     C* Definisco variabili
085000980521     C     *LIKE         DEFINE    AR4TRC        KTRC
085100961127     C     *LIKE         DEFINE    GNPAGC        KAGC
085200961127     C     *LIKE         DEFINE    GNPFGC        KFGC
085300961127     C     *LIKE         DEFINE    GNPNGC        KNGC
085400961127     C     *LIKE         DEFINE    GNPFRG        KFRG
085500961127     C     *LIKE         DEFINE    GNPFAS        KFAS
085600970107     C     *LIKE         DEFINE    GNPNPR        KNPR
085700961129     C     *LIKE         DEFINE    TABCOD        KCOD
085800961129     C     *LIKE         DEFINE    ACOKUT        KKUT
085900961129     C     *LIKE         DEFINE    ACOKCC        KKCC
086000961129     C     *LIKE         DEFINE    ACOKSC        KKSC
086100961129     C     *LIKE         DEFINE    ARBAAS        KAAS
086200961129     C     *LIKE         DEFINE    ARBLNP        KLNP
086300961129     C     *LIKE         DEFINE    ARBNRS        KNRS
086400961129     C     *LIKE         DEFINE    ARBNSP        KNSP
086500071009     C     *LIKE         DEFINE    NTCAPL        kcapl
086600071009     C     *LIKE         DEFINE    NTCNK1        kcnk1
086700071009     C     *LIKE         DEFINE    NTCNK2        kcnk2
086800071009     C     *LIKE         DEFINE    NTCTNT        kctnt
086900961129     C*
087000961127     C* Imposto variabili
087100961127     C                   MOVEL     'N'           WFINE             1
087200961202     C                   MOVEL     '1'           WTPVID            1
087300961129     C*
087400000314     C* Carico dati DS partner in TXT solo se no DPD
087500000314     C     D21DPD        IFNE      'S'
087600961129     C                   Z-ADD     1             X
087700961129     C                   MOVEL     'PT'          KCOD
087800961202     C     KCOD          CHAIN     EDTAB01L                           30
087900961129     C     *IN30         DOWEQ     '0'
088000961129     C     TABFLG        IFEQ      ' '
088100961129     C                   ADD       1             X
088200961202     C                   MOVEL     TABUNI        DPT(X)
088300961202     C                   MOVEL     TABUNI        DSPT
088400961129     C                   MOVEL     TABKEY        CPU(X)
088500961129     C                   MOVEL     TABKEY        CPT(X)
088600120207     C                   MOVEL     TABKEY        CPZ(X)
088700170511     C                   MOVEL     TABKEY        CPS(X)
088800961129     C                   MOVE      '    '        CPT(X)
088900961129     C                   MOVEL     §PTKSC        KSC(X)
089000961129     C                   MOVEL     §PTLNP        LNA(X)
089100961129     C                   END
089200961202     C     KCOD          READE     EDTAB01L                               30
089300961129     C                   END
089400961129     C*
089500961129     C                   MOVEL     'PU'          KCOD
089600961202     C     KCOD          CHAIN     EDTAB01L                           30
089700961129     C     *IN30         DOWEQ     '0'
089800961129     C     TABFLG        IFEQ      ' '
089900961129     C                   Z-ADD     1             X
090000961129     C     TABKEY        LOOKUP    CPU(X)                                 32
090100961129     C     *IN32         IFEQ      '1'
090200961129     C                   MOVEL     TABUNI        DSPU
090300961129     C* Se no partner metto zero in LNA
090400961129     C     §PUTPC        IFEQ      'C'
090500961129     C                   Z-ADD     0             LNA(X)
090600961129     C                   END
090700961129     C                   END
090800961129     C                   END
090900961202     C     KCOD          READE     EDTAB01L                               30
091000961129     C                   END
091100000314     C*
091200120206     C*
091300120206     C                   MOVEL     'PZ'          KCOD
091400120206     C     KCOD          CHAIN     EDTAB01L                           30
091500120206     C     *IN30         DOWEQ     '0'
091600120206     C     TABFLG        IFEQ      ' '
091700120206     C                   Z-ADD     1             X
091800120206     C     TABKEY        LOOKUP    CPZ(X)                                 32
091900120206     C     *IN32         IFEQ      '1'
092000120206     C                   MOVEL     TABUNI        DSPZ
092100120206     C                   MOVEL     TABUNI        DPZ(X)
092200120206     C* imposta il valore del codice di Export
092300120206     C     §PzKSCexp     IFne      *blank
092400120206     C                   move      §PzKSCexp     exp(X)
092500120206     c                   else
092600120206     C                   move      *zeros        exp(X)
092700120206     C                   END
092800120206     C                   END
092900120206     C                   END
093000120206     C     KCOD          READE     EDTAB01L                               30
093100120206     C                   END
093200120206     C*
093300170511     C* aggiunto la tab.PS dove c'è la NON volontà di inviare
093400170511     C*  la LETEST al Partner. AZKAR vuole che mettiamo direttamente
093500170511     C* sul loro sito le disposizioni di GIACENZA perchè NON riescono
093600170511     C*  a stare nei tempi se ricevono le nostre MAIL come facevamo
093700170511     C* di solito.
093800170511     C                   MOVEL     'PS'          KCOD
093900170511     C     KCOD          setll     EDTAB01L
094000170511     C     KCOD          Reade     EDTAB01L
094100170511     C                   DOW       not %EoF(EDTAB01L)
094200170511     C                   IF        TABFLG =' '
094300170511     C                   Z-ADD     1             X
094400170511     C     TABKEY        LOOKUP    CPS(X)                                 32
094500170511     C   32              MOVEL     TABUNI        DPS(X)
094600170511     C                   END
094700170511     C     KCOD          Reade     EDTAB01L
094800170511     C                   END
094900170511     C*
095000000314     C                   END
095100170511     C*
095200961129     C* Reperisco data del giorno
095300961202     C                   TIME                    WHHDAT           14 0
095400961219     C                   MOVE      WHHDAT        G02DAT
095500961219     C                   MOVE      *ZEROS        G02INV
095600961219     C                   MOVEL     *BLANK        G02ERR
095700961219     C                   CALL      'XSRDA8'
095800961219     C                   PARM                    WLBDA8
095900961219     C                   Z-ADD     G02INV        WOGGI             8 0
096000980521     C*  Modalità visualizzazione
096100980521     C     D21VIS        COMP      'V'                                    12
096200980521      *
096300961127     C                   ENDSR
096400071009     C*----------------------------------------------------------------*
096500071009     C*  Controllo dati x invio mail
096600071009     C*----------------------------------------------------------------*
096700071009     C     CTRMail       BEGSR
096800071009      *
096900071009     C     Kntc          KLIST
097000071009     C                   KFLD                    kcapl
097100071009     C                   KFLD                    kcnk1
097200071009     C                   KFLD                    kcnk2
097300071009     C                   KFLD                    kctnt
097400071009      *
097500071009     c                   move      'C'           kcapl
097600071009     c                   movel     '0151'        kcnk1
097700071009     c                   move      kKSC          kcnk1
097800071009     c                   move      '88'          kctnt
097900071009     C*
098000071009     c                   move      'N'           invio_Mail        1
098100071009      * se esiste  e-mail su TFNTC nota '88'
098200071009     c     kntc          chain     tfntc01l
098300071009     c                   if        %found(tfntc01l)
098400071009     c                   move      'S'           invio_Mail
098500071009     c                   end
098600071009     C*
098700071009     C                   ENDSR
098800071009     C*----------------------------------------------------------------*
098900071009     C*  Esegue OVRPRTF x lancio E-mail
099000071009     C*----------------------------------------------------------------*
099100071009     C     OVRPRTF_x_mailBEGSR
099200071009     C*
099300071009     C*   Prima di tutto chiude il PRTF
099400071009     c                   exsr      Close_PRTF
099500071009     C*
099600071009     C* se è riuscita la DLTOVR
099700071009     c                   IF        *in99 = *off
099800071009     C*
099900071009     C*  Quindi riapre eseguendo la giusta OVRPRTF x invio coda E-mail
100000071009     C*   con tutti i parametri tramite il TRUL44R
100100071010     C*
100200071009     c                   clear                   trul44ds
100300071009     c                   clear                   trtcm1ds
100400071009     c                   eval      d44pgm = 'FNLG21R'
100500071009     c                   eval      d44cl  = '005'
100600071009     c                   move      Kksc          d44ksc
100700071009     c                   eval      d44apl = 'C'
100800071009     c                   eval      d44nk1 = '0151' + %editc(Kksc:'X')
100900071009     c                   eval      d44tnt = '88'
101000071010     c                   eval      d44scope='*JOB'
101100071009     c                   eval      d44ovr = 'S'
101200140226     c                   eval      d44obj = '*OBJM*Further Instruction for -
101300140226     c                             exceptions Shipment Nr.: '
101400140226     c                              + %editc(d21AAS:'Z') +
101500071026     c                             ' ' + %char(d21LNP) + ' ' + %char(d21NRS) +
101600071026     c                             ' ' + %char(d21NSP)
101700071009     c                   eval      d44prtf = 'LETEST'
101800071009     c                   call      'TRUL44R'
101900071009     c                   parm                    trul44ds
102000071009     c                   parm                    trtcm1ds
102100071009      *
102200071009      **  Se c'è stato qualche problema
102300071009     c     d44err        ifne      *blank
102400071009     C**
102500071009      * se non è riuscita x qualche motivo lo rimanda come FAX
102600071009     c                   close     LETEST                               99
102700071009     C*  OVRPRTF x FAX
102800071009     c                   z-add     50            lung
102900071009     C                   MOVEL     *BLANKS       COMMAN
103000071009     C                   MOVEA     OVR1(1)       COMMAN
103100071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
103200071009
103300071009     C                   CALL      'QCMDEXC'                            99
103400071009     C                   PARM                    COMMAN           80
103500071009     C                   PARM                    LUNG             15 5
103600071009     C*
103700071009     c                   end
103800071009     C**
103900071009     c                   endIF
104000071009     C**
104100071009     C*   Apre il file dopo OVRPRTF
104200071009     c                   open      LETEST
104300071009     C*
104400071009     C                   ENDSR
104500071009     C*----------------------------------------------------------------*
104600071010     C*  Esegue la DLTOVR sul file
104700071009     C*----------------------------------------------------------------*
104800071010     C     DLTOVR_LETEST BEGSR
104900071009     C*
105000071010     C*  DLTOVR
105100071010     c                   z-add     50            lung
105200071010     C                   MOVEL     *BLANKS       COMMAN
105300071010     C                   MOVEA     dlt1(1)       COMMAN
105400071010     C                   CALL      'QCMDEXC'                            99
105500071010     C                   PARM                    COMMAN           80
105600071010     C                   PARM                    LUNG             15 5
105700071010     C*
105800071010     c                   if        *in99
105900071010     C                   MOVEA     dlt2(1)       COMMAN
106000071010     C                   CALL      'QCMDEXC'                            99
106100071010     C                   PARM                    COMMAN           80
106200071010     C                   PARM                    LUNG             15 5
106300071010     c                   end
106400071010      *
106500071010     C     end_dlt       ENDSR
106600071010     C*----------------------------------------------------------------*
106700071010     C*  Chiusura PRTF  x mail
106800071010     C*----------------------------------------------------------------*
106900071010     C     Close_PRTF    BEGSR
107000071010     C*
107100071009     C                   close     LETEST
107200071010     c                   exsr      DLTOVR_LETEST
107300071009     C*
107400071009     C                   ENDSR
107500980521     C*--------------------------------------------------------------*
107600961202**  Schiera con testi da visualizz.nella finestra dell'esec.disposizioni
107700961129REDELIVER TO:
107800961202AND PROVIDE P.O.D. AFTER DELIVERY
107900961129SEND SHIPMENT BACK TO ITALY
108000961129IMPORTANT !
108100961202AS PER OPS MANUAL THE "SHIPMENT REFERENCE" MUST
108200961129BE OUR SHIPMENT NR. IN "FREE TEXT NOTE", PLEASE
108300961129SPECIFY IT IS A RETURN SHIPMENT.
108400961202**
108500961202Numero fax errato impossibile procedere oltre
108600061107Nr.fax errato. Manca anagr.Depot in tabella Depot DPD->Avvisare CED!
108700070515Attenzione: Spedizione Chronopost - Inviare Manualmente il FAX
108800071009**         dlt1
108900071009DLTOVR FILE(LETEST)
109000071009**         dlt2
109100071009DLTOVR FILE(LETEST) LVL(*JOB)
109200071009**         Ovr1
109300071009OVRPRTF FILE(LETEST) OUTQ(
