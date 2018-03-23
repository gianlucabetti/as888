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
002500171018     FFiPND01L  IF   E           K DISK
002600071009     F*---------
002700071009     FTfntc01l  IF   E           K DISK
002800980521     F*---------
002900071009     FLETEST    O    E             PRINTER usropn
003000961129     D*--------------------------------------------------------------*
003100961129     D*  SCHIERE
003200961129     D*--------------------------------------------------------------*
003300961129     D TXT             S             50    DIM(7) CTDATA PERRCD(1)
003400171018     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
003500071009     E*  QCAEXEC
003600071009     D DLT1            S             50    DIM(1) CTDATA PERRCD(1)
003700071009     D DLT2            S             50    DIM(1) CTDATA PERRCD(1)
003800071009     D OVR1            S             50    DIM(1) CTDATA PERRCD(1)
003900071009     D*--------------------------------------------------------------*
004000961129     D KSC             S              7  0 DIM(200)
004100961129     D LNA             S              3  0 DIM(200)
004200961129     D CPU             S             35    DIM(200)
004300961129     D CPT             S             35    DIM(200)
004400961202     D DPT             S             90    DIM(200)
004500120206     D CPZ             S             35    DIM(200)
004600120206     D DPZ             S             90    DIM(200)
004700120206     D exp             S              7    DIM(200)
004800170511     D CPS             S             35    DIM(200)
004900170511     D DPS             S             90    DIM(200)
005000961119     D*--------------------------------------------------------------*
005100961119     D*  DS
005200961119     D*--------------------------------------------------------------*
005300961119     D KPJBA         E DS
005400961219     D WLBDA8          DS
005500961219     D  G02DAT                 1      8  0
005600961219     D  G02INV                 9     16  0
005700961219     D  G02ERR                17     17
005800961219     D  G02TGI                18     22  0
005900171018     D**** DSBL4i        E DS
006000171018      *-------------------
006100020717     D OG143         E DS
006200110606     D CNCR42        E DS
006300000314     D FNLG21        E DS                  EXTNAME(FNLG21DS)
006400961127     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006500961129     D  TCU                  398    697
006600961129     D                                     DIM(50)
006700961129     D  KCU                  698    847P 0
006800961129     D                                     DIM(50)
006900961129     D                                     PACKEVEN
007000071009      * invio Mail al posto del FAX
007100071009     D trul44ds      E DS
007200071009     D trtcm1ds      E DS
007300071009      *
007400961129     D TRUL43        E DS                  EXTNAME(TRUL43DS)
007500961129     D TRUL42        E DS                  EXTNAME(TRUL42DS)
007600171019     d trulDEPds     e ds
007700171019     d trulDEPds1    e ds
007800171019     d DpndDP4       e ds
007900171019     d DpndDP5       e ds
008000961202     D DSPT          E DS                  EXTNAME(EDIDSPT)
008100961202     D DSPU          E DS                  EXTNAME(EDIDSPU)
008200120206     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
008300170511     D DSPS          E DS                  EXTNAME(EDIDSPS)
008400961202     D TCUDS           DS
008500961202     D  F3                     3      3
008600961202     D  F4                     4      4
008700961202     D  F56                    5      6
008800961219     D                 DS
008900961219     D  DEDAT                  1      9    INZ('FAX  DEL ')
009000961219     D  GGFAX                 10     11  0
009100961219     D  FILGG                 12     12    INZ('/')
009200961219     D  MMFAX                 13     14  0
009300961219     D  FILMM                 15     15    INZ('/')
009400961219     D  AAFAX                 16     19  0
009500961219     D  TESTA                  1     19
009600050223     D comodo          s                   like(gnpnpr)
009700070515     d digits          c                   '0123456789'
009800961119     C*--------------------------------------------------------------*
009900961119     C*  C I C L O        P R I N C I P A L E
010000961119     C*--------------------------------------------------------------*
010100961127     C*  Caricamento subfile
010200961127     C                   EXSR      CARS01
010300961127     C*
010400961127     C*  Gestisco subfile fino a che non viene premuto F12 o F6
010500961127     C     WFINE         DOWEQ     'N'
010600961127     C     WTPVID        CASEQ     '1'           GESS01
010700961127     C                   END
010800961127     C                   END
010900071010      *
011000961127     C*  Se ho premuto F6 stampo fax
011100961127     C     *INKF         IFEQ      '1'
011200071009     C                   EXSR      CTRMail
011300071009     c                   if        Invio_Mail = 'S'
011400071009      ** solo x E-mail
011500071009     c                   exsr      OVRPRTF_x_Mail
011600071009     c                   end
011700071009      ***
011800961202     C                   EXSR      STAFAX
011900961127     C                   END
012000961127     C*  Fine pgm
012100961127     C     FINE          TAG
012200071009     C*   Prima di tutto chiude il PRTF
012300071009     c                   exsr      Close_PRTF
012400071009      *
012500961127     C                   SETON                                        LR
012600961127     C*--------------------------------------------------------------*
012700961127     C*  CARS01: Eseguo caricamento subfile
012800961127     C*--------------------------------------------------------------*
012900961127     C     CARS01        BEGSR
013000961127     C*
013100970108     C                   SETOFF                                       1011
013200970107     C* Inizializzo subfile
013300970107     C                   EXSR      INISBF
013400970107     C* Se vengo dall'esecuzione disposizioni di giacenza imposto
013500970107     C* subfile dalle note giacenza
013600961202     C     D21TPR        IFEQ      'G'
013700970107     C                   EXSR      REPPRG
013800970108     C     *IN30         IFEQ      '0'
013900970107     C                   EXSR      GIAFAX
014000970108     C                   ELSE
014100970108     C                   EXSR      CARRIG
014200970108     C                   END
014300970107     C* Carico righe vuote
014400970108     C                   ELSE
014500970107     C                   EXSR      CARRIG
014600970108     C                   END
014700961127     C*
014800970108     C                   ENDSR
014900970107     C*--------------------------------------------------------------*
015000970107     C*  INISBF: Inizializzo subfile
015100970107     C*--------------------------------------------------------------*
015200970107     C     INISBF        BEGSR
015300970107     C* Pulisco subfile
015400970107     C                   SETOFF                                       2021
015500970107     C                   WRITE     LG21C01
015600970107     C                   SETON                                        2021
015700970107     C* Carico 5 righe di subfile
015800970107     C                   SETOFF                                       04
015900970108     C                   Z-ADD     0             NRR
016000970107     C*
016100970107     C                   ENDSR
016200970107     C*--------------------------------------------------------------*
016300970107     C*  CARRIG: CAricamento righe vuote
016400970107     C*--------------------------------------------------------------*
016500970107     C     CARRIG        BEGSR
016600970107     C*
016700970107     C* Se sto trattando il reso carico le descrizioni dal testo
016800970107     C* fisso
016900970107     C     D21DIS        IFEQ      'R'
017000970107     C     NRR           IFEQ      0
017100970107     C                   Z-ADD     0             NRR               4 0
017200970107     C     3             DO        7             X
017300970107     C                   ADD       1             NRR
017400970107     C                   MOVEL     TXT(X)        V1CNOT
017500970107     C                   WRITE     LG21S01
017600970107     C                   END
017700970107     C                   END
017800970107     C*
017900970107     C* Se sto esaminando una riconsegna controllo se ho mem.
018000970107     C* nelle note il testo libero
018100970107     C                   ELSE
018200970107     C                   ADD       1             NRR
018300970107     C                   MOVEL     TXT(1)        V1CNOT
018400970107     C                   WRITE     LG21S01
018500970107     C* Carico le righe restanti ad arrivare a 5
018600970107     C     4             SUB       NRR           WDACAR            4 0
018700970107     C                   DO        WDACAR
018800970107     C                   ADD       1             NRR
018900970107     C                   MOVEL     *BLANKS       V1CNOT
019000970107     C                   WRITE     LG21S01
019100970107     C                   END
019200970107     C                   ADD       1             NRR
019300970107     C                   MOVEL     TXT(2)        V1CNOT
019400970107     C                   WRITE     LG21S01
019500970107     C*
019600970107     C                   END
019700970108     C*
019800970108     C                   ENDSR
019900961127     C*--------------------------------------------------------------*
020000961127     C*  GESS01: Gestione subfile
020100961127     C*--------------------------------------------------------------*
020200961127     C     GESS01        BEGSR
020300961127     C*
020400970108     C*  Se è stato inviato più di un fax accendo 10
020500970108     C     WNPROG        COMP      5                                  10
020600961127     C*  Eseguo exfmt
020700961202     C                   WRITE     LG21Z01
020800961202     C                   EXFMT     LG21C01
020900961129     C                   SETOFF                                       28
021000970107     C*  Richiesto F10 ---> fax precedente
021100970107     C     *INKJ         IFEQ      '1'
021200970107     C                   SUB       5             KNPR
021300970108     C                   SETON                                        02
021400970107     C                   EXSR      INISBF
021500970108     C                   EXSR      GIAFAX
021600970108     C                   SETON                                        11
021700970107     C                   GOTO      FINVD1
021800970107     C                   END
021900050223     C*  Richiesto F11 ---> ultimo fax
022000970107     C     *INKK         IFEQ      '1'
022100970107     C                   Z-ADD     WULFAX        KNPR
022200970108     C                   SETOFF                                       02
022300970107     C                   EXSR      INISBF
022400970108     C                   EXSR      GIAFAX
022500970108     C                   SETOFF                                       11
022600970107     C                   GOTO      FINVD1
022700970107     C                   END
022800170511      **
022900970107     C*  Controllo se il numero del fax è corretto
023000970107     C                   EXSR      CTRFAX
023100170511      **
023200961127     C*  Richiesto F12 o F6 --> fine pgm
023300961127     C     *INKL         IFEQ      '1'
023400961129     C                   MOVEL     'S'           WFINE
023500961129     C                   GOTO      FINVD1
023600961129     C                   END
023700961129     C  N28*INKF         IFEQ      '1'
023800961127     C                   MOVEL     'S'           WFINE
023900961127     C                   END
024000961127     C*
024100961202     C     FINVD1        ENDSR
024200961127     C*--------------------------------------------------------------*
024300961127     C*  Stampa fax e aggiorna note giacenza
024400961127     C*--------------------------------------------------------------*
024500961202     C     STAFAX        BEGSR
024600961127     C*
024700961129     C*  Imposto dati spedizione in stampa
024800961129     C                   EXSR      DATSPE
024900961127     C*  Eseguo stampa fax
025000961203     C                   Z-ADD     1             STPPAG
025100170511     c*
025200170511     c*  da tab.PT/PS il controllo se il PARTNER NON deve avere
025300170511     c*    la comunicazione (MAIL o FAX)
025400170511     c                   if        ESEGUIRE_invio <> 'N'
025500170511     c*-------
025600071010     c                   if        Invio_Mail <>'S'
025700961203     C                   WRITE     TESFAX
025800071010     c*
025900961203     C                   WRITE     TESGEN
026000980521     C   23              WRITE     TESAWB
026100000320     C     D21DPD        IFEQ      'S'
026200000320     C                   WRITE     RIFDPD
026300000320     C                   END
026400000320     C                   WRITE     TESSPE
026500961203     C                   WRITE     SOGSPE
026600961202     C     D21TPR        IFEQ      'G'
026700961129     C                   WRITE     TESGIA
026800961129     C                   END
026900961202     C*  Stampo testo libero
027000961202     C                   WRITE     INITXT
027100961202     C                   EXSR      STPTXT
027200071010     C*
027300071010     c                   else
027400071010     C* se si manda una Mail
027500071010     C* rispetto al FAX le righe sono posizionate diversamente sulla pagina
027600071010     c*
027700071010     C                   WRITE     TESGENM
027800071010     C   23              WRITE     TESAWBM
027900071010     C     D21DPD        IFEQ      'S'
028000071010     C                   WRITE     RIFDPDM
028100071010     C                   END
028200071010     C                   WRITE     TESSPEM
028300071010     C                   WRITE     SOGSPEM
028400071010     C     D21TPR        IFEQ      'G'
028500071010     C                   WRITE     TESGIAM
028600071010     C                   END
028700071010     C*  Stampo testo libero
028800071010     C                   WRITE     INITXT
028900071010     C                   EXSR      STPTXT
029000071010     c*
029100071010     c                   end
029200071010     C*
029300170511     c                   ENDIF
029400170511     c*-------
029500170511     C*
029600961202     C*  Se vengo da giacenza e eseguo riconsegna mem. testo
029700961202     C     D21TPR        IFEQ      'G'
029800961202     C                   EXSR      AGGNOT
029900961202     C                   END
030000071010     C*
030100961127     C                   ENDSR
030200961129     C*--------------------------------------------------------------*
030300961129     C*  Controllo se è valido il numero del fax
030400961129     C*--------------------------------------------------------------*
030500961129     C     CTRFAX        BEGSR
030600961129     C*
030700170511     C*  Default *blank= INVIO "SI"
030800170511     c                   clear                   eseguire_invio    1
030900170511     C*
031000000314     C                   Z-ADD     1             X
031100961129     C*  Eseguo posizionamento sulla bolla
031200961129     C                   Z-ADD     D21AAS        KAAS
031300961129     C                   Z-ADD     D21LNP        KLNP
031400961129     C                   Z-ADD     D21NRS        KNRS
031500961129     C                   Z-ADD     D21NSP        KNSP
031600961202     C     KARB          CHAIN     FNARB01L                           30
031700000314     C*  Se sono una filiale di arrivo estera (no DPD) reperisco
031800000314     C*  il codice dell'anagrafica in base alla tabella Pt
031900170511     C*
032000000314     C     D21DPD        IFNE      'S'
032100170511     C*
032200961129     C*  Se vengo da giacenze
032300961129     C*  eseguo posizionamento su giacenza
032400961202     C     D21TPR        IFEQ      'G'
032500170511     C*
032600970418     C                   Z-ADD     D21FRG        KFRG
032700050222     C     KGCA          CHAIN     TIGCP21L                           31
032800961129     C*  In base alla filiale giac. reperisco il codice partner
032900961129     C     GCPFGC        LOOKUP    LNA(X)                                 32
033000961129     C                   ELSE
033100961129     C*  In base alla lna della bolla reperisco il codice partner
033200961202     C     ARBLNA        LOOKUP    LNA(X)                                 32
033300961129     C                   END
033400961129     C*
033500961129     C*  Reperisco i dati anagrafici del partner
033600961129     C                   Z-ADD     KSC(X)        KKSC
033700120206     c**
033800120206      **  se presente un codice specifico di Export prende questo per
033900120206      **  reperire il FAX o l'indirizzo della mail
034000120206     c                   if        exp(X) <> *all'0'
034100120207     c     digits        check     exp(X)
034200120207     c                   if        not %Found
034300120207     C                   move      exp(X)        KKSC
034400120207     c                   end
034500120206     c                   end
034600120206     c**
034700961202     C                   MOVEL     DPT(X)        DSPT
034800170511     c**
034900170511     c**  Sulla tab.EDIDSPS c'è  (§PSnoDISPO)
035000170511     c**   l'informazione di NON mandare la comunicazione al PARTNER
035100170511     c**    e quindi NON deve generare la LETEST.
035200170511     C                   MOVEL     DPS(X)        DSPS
035300170511     c                   movel     §PSnoDISPO    eseguire_invio
035400170511     C*   Può avere "N" per non far inviare nè mail nè fax poichè il PARTNER
035500170511     c**   come AZKAR vuole che siano immesse direttamente sul suo SITO WEB
035600170511     c**    le disposizioni e NON vuole più ricevere COMUNICAZIONI.
035700170511     c**
035800171018      *  -------------------------------------------------------
035900000314     C                   ELSE
036000171018      *    per DPD    ?
036100000314     C                   Z-ADD     D21AAS        KAAS
036200000314     C                   Z-ADD     D21LNP        KLNP
036300000314     C                   Z-ADD     D21NRS        KNRS
036400000314     C                   Z-ADD     D21NSP        KNSP
036500171018      *
036600000314     C                   Z-ADD     0             KKSC
036700061107      ***
036800171018     C*******            MOVEL     'I'           KTRC
036900171018     C*******            CLEAR                   AR4NOT
037000171018     C**** KAR4          CHAIN     FiAR4000                           34
037100171018     C*******            MOVEL     AR4NOT        DSBL4I
037200061107      ***   (Prima lo componeva con la linea DPD + il depot di arrivo)
037300061107     C******             MOVEL     ARBLNA        KKSC
037400061107     C******             MOVE      §B4ida        kksc
037500171018      *
037600171018      *--- Aggancia il nuovo Archivio che sostituisce il FIAR4 "I"
037700171018     C     Kpnd          CHAIN     Fipnd01l
037800171019      *
037900171018      *  Se non aggancia la spedizione sul nuovo archivio FIPND che
038000171018      *   sostituisce il FIAR4 "I".
038100171018     c                   if        not %Found(Fipnd01l)
038200171018     C                   SETON                                        28
038300171018     C                   MOVEL     ERR(4)        $MSG
038400171018     c                   else
038500171018      *
038600061107      ***     adesso Attenzione:
038700061107      ***  il cod.cliente DPD deve essere reperito utilizzando la tabella
038800061107      ***   dei Depot su cui sono codificati i codici clienti x depot DPD
038900171019     C                   clear                   TRULDEPds
039000171019     C                   clear                   TRULDEPds1
039100171019     c                   move      'L'           IDEPTLA
039200061107      ** data della spedizione
039300171019     c                   move      arbMGS        IDEPDRF
039400171019     c                   movel     arbAAS        IDEPDRF
039500171019      *--
039600171019      *** se ho il depot a 7
039700171019     c                   if        pndIDA7 <> ' '
039800171019     c                   movel     pndIDA7       IDEPDEP7
039900171019     c                   else
040000171019      *--
040100171019      *** se il depot ancora a 4
040200171019     c                   if        pndDPE = 'DP4'
040300171019     c                   movel     pndETIU       DPNDDP4
040400171019     c                   move      *zeros        IDEPDPC
040500171019     c                   move      §B4ida        IDEPDPC
040600171019     c                   eLSEIF    pndDPE = 'DP5'
040700171019     c                   movel     pndETIU       DPNDDP5
040800171019     c                   move      *zeros        IDEPDPC
040900171019     c                   move      §PNDIDAST     IDEPDPC
041000171019     c                   end
041100171019      *--
041200171019     c                   end
041300171019      *--
041400171019     C                   CALL      'TRULDEPR'
041500171019     C                   PARM                    TRULDEPds
041600171019     C                   PARM                    TRULDEPds1
041700061107      * tutto OK trovato il cliente
041800171019     c                   if        odepERR <>*blank or OdepKSC =0
041900061107      ** se trovati errori nella decodifica del Depot o manca il Cod.Cliente
042000061107      ** deve avvisare dell'errore del mancato reperimento sul DPPDC00F
042100061107      ** del codice del cliente x poter trovare il nr.FAX
042200171019     C                   SETON                                        28
042300171019     C                   MOVEL     ERR(2)        $MSG
042400171019     c                   else
042500171019     c                   z-add     ODEPKSC       kksc
042600171019     c                   end
042700171019      ***
042800171019     C**********         MOVEL     §B4ipn        STPRIF
042900171019      ***
043000171018      *  -------------------------------------------------------
043100171018     C                   MOVEL     PNDipn        STPRIF
043200171018     c                   endif
043300171018      *  -------------------------------------------------------
043400171018      *  controllo validità
043500171018      *   campo Deposito  -->  contenente solo digits   OK= not found.
043600171018      ***
043700171018     c*********          clear                   dep_num           4 0
043800171018     c**** digits        check     §b4IDA
043900171018     c********           if        not %Found
044000171018     c*********          move      §b4IDA        dep_num
044100171018     c**********         end
044200171018      ***
044300171018      *  ------------------------------------------------------
044400171018      *  x gestire vecchia CRONOPOST prima dell'avvio di EXAPAC dal 21/5/07.
044500171018      *   [prima era tutto Cronopost (Francia)].
044600171018      *  ------------------------------------------------------
044700171018      ***   VECCHISSIMO CONTROLLO --> ELIMINATO poichè INATTIVO dal 28/5/2007
044800171018     c                   clear                   ChronoPost        1
044900171018      **
045000171018      *   Nuovi Depots EXAPAC codice fra 1000 e 1100.
045100171018      *   e se Linea 195 trasmessa con Manifest prima del 28/5/2007
045200171018     c**********         if        dep_num >= 1000 and dep_num < 1100 and
045300171018     c**********                   arbLNA = 195 and arbDT3 < 20070528
045400171018     c**********         move      'S'           ChronoPost
045500171018     c**********         end
045600171018      *  -------------------------------------------------------
045700171018      *      [Chiodo attivato x FAX sempre x 1950010]    ?
045800171018      *   [Non segnala Cronopost e manda sempre a Exapac].
045900070525     c                   if        arbLNA = 195 and arbDT3 >= 20070528
046000070525     c                   move      ' '           ChronoPost
046100070525     c                   z-add     1950010       kKSC
046200070525     c                   end
046300070515      ***
046400171018      *  -------------------------------------------------------
046500000314     C                   END
046600070515      *
046700000314     c* Aggancio anagrafica
046800961129     C                   MOVEL     *BLANKS       ACORAG
046900000314     C                   Z-ADD     1             KKUT
047000000314     C                   Z-ADD     CLIITA        KKCC
047100961129     C     KACO          CHAIN     CNACO00F                           31
047200961129     C                   MOVEL     ACORAG        STPPAT
047300170511      **
047400961129     C*  Controllo se esiste un luogo di invio comunicazione di giac.
047500961129     C                   CLEAR                   TRUL43
047600961129     C                   Z-ADD     KSC(X)        D43KSC
047700120206      **
047800120206      **   se si tratta di un Partner con codice EXPORT impostato
047900120206      **   per un invio ad indirizzo particolare differente da quello x IMPORT.
048000120206     c                   if        exp(X) <> *all'0'
048100120207     c     digits        check     exp(X)
048200120207     c                   if        not %Found
048300120207     C                   move      exp(X)        D43KSC
048400120207     c                   end
048500120206     c                   end
048600120206      **
048700961129     C                   CALL      'TRUL43R'
048800961129     C                   PARM                    TRUL43
048900170511      **
049000961129     C     D43TRV        IFNE      'S'
049100961129     C                   MOVEL     *BLANKS       STPFAX
049200961129     C     KIND          CHAIN     CNIND00F                           31
049300961129     C                   MOVEL     INDTLF        STPFAX
049400961129     C                   ELSE
049500961202     C                   MOVEL     D43FAX        STPFAX
049600961129     C                   END
049700170511      **
049800961129     C*  Controllo validità numero fax e se non è corretto non consento
049900961129     C*  di confermare
050000961129     C                   CLEAR                   TRUL42
050100961129     C                   MOVEL     STPFAX        D42FAX
050200961129     C                   CALL      'TRUL42R'
050300961129     C                   PARM                    TRUL42
050400961129     C     D42ERR        IFEQ      '1'
050500961129     C                   SETON                                        28
050600961129     C                   MOVEL     ERR(1)        $MSG
050700961129     C                   END
050800070515     C*
050900171018      *    per DPD e Chronopost prima di EXAPAC              ?
051000171018      *    Si deve inviare il FAX manualmente a Chronopost   ?
051100070515     C                   If        D21dpd = 'S' and ChronoPost = 'S'
051200070515     C                   SETON                                        28
051300070515     C                   MOVEL     ERR(3)        $MSG
051400070515     c                   end
051500961129     C*
051600961129     C                   ENDSR
051700961129     C*--------------------------------------------------------------*
051800961129     C*  Reperisco dati della spedizione in stampa
051900961129     C*--------------------------------------------------------------*
052000961129     C     DATSPE        BEGSR
052100961129     C*
052200961129     C                   MOVE      WHHDAT        STPOGG
052300080219     C****               Z-ADD     D21AAS        STPAAS
052400080219     C                   move      D21AAS        STPAAS
052500961202     C                   Z-ADD     D21LNP        STPLNP
052600961129     C                   Z-ADD     D21NRS        STPNRS
052700961129     C                   Z-ADD     D21NSP        STPNSP
052800970625     C     ARBDT3        IFEQ      0
052900961129     C                   MOVEL     ARBMGS        WMM               2 0
053000961129     C                   MOVE      ARBMGS        WGG               2 0
053100961129     C                   MOVEL     WGG           WGGMM             4 0
053200961129     C                   MOVE      WMM           WGGMM
053300961129     C                   MOVEL     WGGMM         STPDSP
053400961129     C                   MOVE      ARBAAS        STPDSP
053500970625     C                   ELSE
053600970625     C                   Z-ADD     ARBDT3        G02INV
053700970625     C                   MOVEL     '3'           G02ERR
053800970625     C                   CALL      'XSRDA8'
053900970625     C                   PARM                    WLBDA8
054000970625     C                   Z-ADD     G02DAT        STPDSP            8 0
054100970625     C                   END
054200961129     C*  Imposto dati giacenza se spedizione giacente
054300961129     C     D21TPR        IFEQ      'G'
054400961129     C                   Z-ADD     D21FGC        STPFGC
054500961129     C                   Z-ADD     D21NGC        STPNGC
054600080219     C*****              Z-ADD     D21AAS        STPAA1
054700080219     C                   move      D21AAS        STPAA1
054800961129     C                   Z-ADD     D21LNP        STPLP1
054900961129     C                   Z-ADD     D21NRS        STPNR1
055000961129     C                   Z-ADD     D21NSP        STPNS1
055100961129     C                   MOVE      D21AGC        STPDGC
055200961129     C                   MOVEL     GCPMGC        WMM               2 0
055300961129     C                   MOVE      GCPMGC        WGG               2 0
055400961129     C                   MOVEL     WGG           WGGMM             4 0
055500961129     C                   MOVE      WMM           WGGMM
055600961129     C                   MOVEL     WGGMM         STPDGC
055700961129     C                   END
055800980205     C                   Z-ADD     ARBNCL        STPNCL
055900990730     C                   Z-ADD     ARBPKF        STPPKG
056000961129     C* Reperisco i dati del destinatario dalla bolla
056100961129     C                   MOVEL     ARBRSD        STPRSD
056200961129     C                   MOVEL     ARBIND        STPIND
056300961129     C                   MOVEL     ARBCAD        STPCAD
056400961202     C                   MOVEL     ARBLOD        STPCID
056500961129     C                   MOVEL     ARBPRD        STPPRD
056600961129     C                   MOVEL     ARBNZD        STPNZD
056700961129     C* Preimposto i dati del mittente dalla bolla
056800961129     C                   MOVEL     ARBRSM        STPRSM
056900961129     C                   MOVEL     ARBINM        STPINM
057000961129     C                   MOVEL     ARBCAM        STPCAM
057100961202     C                   MOVEL     ARBLOM        STPCIM
057200961129     C                   MOVEL     ARBPRM        STPPRM
057300961129     C                   MOVEL     ARBNZM        STPNZM
057400000314     C     D21DPD        IFEQ      'S'
057500120117      ** la lunghezza del nostro riferimento bolla deve essere preso dalla
057600120117      ** lunghezza rilevata nel riferimento AGE sul quale si basa tutto l'EDI.
057700120117      **   NON da quanto passato nel CNI.(che comunque coincide)
057800120117      **   quello che fa testo è quanto messo nel RFF+AGE:ns.sped.
057900120117      **  (Ricordo che in EDI il riferimento deve essere max lungo 15)
058000120117     C*****§PTNSP        ORLT      16
058100120117     C     §PTAGE        ORLT      16
058200080219     C****               MOVEL     '00'          STPAAS
058300080219     C****               MOVEL     '00'          STPAA1
058400080219     C                   MOVEL     '  '          STPAAS
058500080219     C                   MOVEL     '  '          STPAA1
058600961202     C                   END
058700980521      * Se SDI ricavo AWB
058800980521     C                   EXSR      RICAWB
058900980521      *
059000961129     C                   ENDSR
059100980521      *--------------------------------------------------------------*
059200980521      *  Stampo testo libero
059300980521      *--------------------------------------------------------------*
059400980521     C     RICAWB        BEGSR
059500980521      *
059600980521     C                   MOVEL     *BLANKS       STPAWB
059700980521      *
059800980521      *  Se vengo da giacenze eseguo posizionamento con fil. giacenza
059900980521     C     D21TPR        IFEQ      'G'
060000980521     C     GCPFGC        CHAIN     AZORG01L                           23
060100980521     C                   ELSE
060200980521     C     ARBLNA        CHAIN     AZORG01L                           23
060300980521     C                   END
060400020717     C**** sostituito con il campo Network la gestione dei vecchi flags
060500020717     C  N23              MOVEL     ORGDE3        OG143
060600980521      *
060700020717     C     *IN23         IFEQ      '0'
060800020717      *  Estero+SDI era Europolitan
060900020717     C     §OgNTW        andeq     'EUP'
061000980521     C                   SETON                                        23
061100980521     C                   ELSE
061200980521     C                   SETOFF                                       23
061300980521     C                   ENDIF
061400980521      *
061500980521     C     *IN23         IFEQ      '1'
061600980521     C                   Z-ADD     D21AAS        KAAS
061700980521     C                   Z-ADD     D21LNP        KLNP
061800980521     C                   Z-ADD     D21NRS        KNRS
061900980521     C                   Z-ADD     D21NSP        KNSP
062000980521     C                   MOVEL     'E'           KTRC
062100060213     C     KAR4          CHAIN     FiAR4000                           34
062200980521     C  N34              MOVEL     AR4NOT        STPAWB
062300980521     C                   ENDIF
062400980521      *
062500980521     C                   ENDSR
062600980521     C*--------------------------------------------------------------*
062700980521     C*  Stampo testo libero
062800980521     C*--------------------------------------------------------------*
062900980521     C     STPTXT        BEGSR
063000980521     C*
063100961202     C*  Stampo testo libero
063200961202     C                   Z-ADD     1             NRR
063300961202     C     NRR           CHAIN     LG21S01                            29
063400961202     C     *IN29         DOWEQ     '0'
063500961202     C     V1CNOT        IFNE      *BLANKS
063600961202     C                   MOVEL     *BLANKS       NOTE
063700961202     C                   MOVEL     V1CNOT        NOTE
063800961202     C                   WRITE     TESLIB
063900961202     C                   END
064000961202     C                   ADD       1             NRR
064100961202     C     NRR           CHAIN     LG21S01                            29
064200961202     C                   END
064300961202     C*
064400961202     C                   ENDSR
064500961202     C*--------------------------------------------------------------*
064600961202     C*  Aggiorno note consegna
064700961202     C*--------------------------------------------------------------*
064800961202     C     AGGNOT        BEGSR
064900961202     C*
065000970108     C*  Reperisco nr. progre. iniziale
065100970108     C     WNRFAX        MULT      5             WNPR
065200961202     C*  5  Righe libere
065300961202     C                   Z-ADD     1             NRR
065400050222     C                   CLEAR                   TIGNP000
065500961202     C     NRR           CHAIN     LG21S01                            29
065600961202     C     *IN29         DOWEQ     '0'
065700961202     C     V1CNOT        IFNE      *BLANKS
065800961202     C                   Z-ADD     D21AGC        GNPAGC
065900961202     C                   Z-ADD     D21FGC        GNPFGC
066000961202     C                   Z-ADD     D21NGC        GNPNGC
066100961202     C                   Z-ADD     0             GNPFRG
066200961219     C                   Z-ADD     35            GNPFAS
066300961202     C                   MOVEL     'I'           GNPTPN
066400050222     C                   MOVEL(p)  V1CNOT        GNPDMC
066500961202     C                   ADD       1             WNPR              5 0
066600961202     C                   Z-ADD     WNPR          GNPNPR
066700050222     C                   Z-ADD     woggi         GNPDIF
066800990628     C                   Z-ADD     0             GNPNRI
066900050222     C                   WRITE     TIGNP000
067000961202     C                   END
067100961202     C                   ADD       1             NRR
067200961202     C     NRR           CHAIN     LG21S01                            29
067300961202     C                   END
067400961202     C*
067500171018      *  imposta la riga per memorizzare l'invio FAX o MAIL  ?
067600071011     C                   MOVEL(p)  '*****'       GNPDMC
067700170511      ***
067800170511      *  se ha inviato l'informazione oppure NO
067900170511     c                   clear                   campo16          16
068000170511     C                   MOVE      'SENT BY FAX 'campo16
068100170511      *
068200170511      ** se NON deve mandare, comunque lo memorizza
068300170511     c                   if        eseguire_INVIO ='N'
068400170511     C                   MOVEL     'NOT'         campo16
068500170511     c                   end
068600170511     C                   MOVE      campo16       GNPDMC
068700170511      **
068800170511      *  se invece del FAX era con MAIL
068900071011     c                   if        Invio_Mail = 'S'
069000071011     C                   MOVE      'MAIL'        GNPDMC
069100071011     c                   end
069200170511      **
069300071011     C                   WRITE     TIGNP000
069400071011     C*
069500961202     C                   ENDSR
069600970107     C*--------------------------------------------------------------*
069700970107     C*  Caricamento fax da giacenza
069800970107     C*--------------------------------------------------------------*
069900970107     C     GIAFAX        BEGSR
070000970107     C*
070100970107     C                   Z-ADD     D21AGC        KAGC
070200970107     C                   Z-ADD     D21FGC        KFGC
070300970107     C                   Z-ADD     D21NGC        KNGC
070400970418     C                   Z-ADD     D21FRG        KFRG
070500970107     C                   Z-ADD     35            KFAS
070600050222     C     KGNA2         SETLL     TIGNP01L
070700050222     C     KGNA1         READE     TIGNP01L                               30
070800970107     C     *IN30         IFEQ      '0'
070900071011      ****
071000071011      **   non carico se la riga è quella che indica il tipo di invio
071100071011      **   e lo memorizzo per esporlo in testata della finestra.
071200071011      ****
071300071011     c                   movel     gnpDMC        asterischi        5
071400071011     c                   move      gnpDMC        tipo_Invio       12
071500071011     c                   movel     tipo_Invio    sent              4
071600071011     c                   if        asterischi = *all'*' and sent = 'SENT'
071700071011     C     KGNA1         READE     TIGNP01L                               30
071800071011     c                   end
071900071011      ****
072000970107     C* Imposto in testata data fax
072100050222     C                   MOVE      GNPDIF        GGFAX
072200050222     C                   MOVE      GNPDIF        WMMGG             4 0
072300970107     C                   MOVEL     WMMGG         MMFAX
072400050222     C                   MOVEL     GNPDIF        AAFAX
072500970107     C                   MOVEL     TESTA         V1CTES
072600050223     c     gnpnpr        add       5             comodo
072700970107     C*
072800970107     C     *IN30         DOWEQ     '0'
072900970107     C     NRR           ANDLT     5
073000050223     C     gnpnpr        ANDLe     comodo
073100970107     C                   SETON                                        04
073200970108     C                   Z-ADD     GNPNPR        WNPROG            5 0
073300970107     C                   ADD       1             NRR
073400970107     C                   MOVEL     GNPDMC        V1CNOT
073500970107     C                   WRITE     LG21S01
073600071011      ****
073700050222     C     KGNA1         READE     TIGNP01L                               30
073800071011      ****
073900071011      ****  controlla se si tratta di MAIL o FAX
074000071011     c                   if        *in30 = *Off
074100071011     c                   movel     gnpDMC        asterischi        5
074200071011     c                   move      gnpDMC        tipo_Invio       12
074300071011     c                   movel     tipo_Invio    sent              4
074400071011     c                   move      tipo_Invio    FAX_MAIL          4
074500071011     c                   if        asterischi = *all'*' and sent = 'SENT'
074600071011     C                   MOVEL     FAX_MAIL      V1CTES
074700071011     c                   end
074800071011     c                   end
074900071011      ****
075000970107     C                   END
075100970107     C*  Carico righe mancanti
075200970107     C     5             SUB       NRR           WDACAR            4 0
075300970107     C                   DO        WDACAR
075400970107     C                   ADD       1             NRR
075500970107     C                   MOVEL     *BLANKS       V1CNOT
075600970107     C                   WRITE     LG21S01
075700970107     C                   END
075800970107     C                   END
075900970108     C*
076000970108     C                   ENDSR
076100970107     C*--------------------------------------------------------------*
076200970107     C*  Reperisco ultimo numero proggr. nota
076300970107     C*--------------------------------------------------------------*
076400970107     C     REPPRG        BEGSR
076500970107     C*
076600970108     C                   Z-ADD     0             WNRFAX            4 0
076700970108     C                   Z-ADD     0             WULFAX            5 0
076800970107     C                   Z-ADD     D21AGC        KAGC
076900970107     C                   Z-ADD     D21FGC        KFGC
077000970107     C                   Z-ADD     D21NGC        KNGC
077100970418     C                   Z-ADD     D21FRG        KFRG
077200970107     C                   Z-ADD     35            KFAS
077300050222     C     KGNA1         SETGT     TIGNP01L
077400050222     C     KGNA1         READPE    TIGNP01L                               30
077500970107     C     *IN30         IFEQ      '0'
077600970107     C                   Z-ADD     GNPNPR        WNPR
077700071011      *****
077800071011      ***** Ultima riga da non considerare
077900071011     c                   movel     gnpDMC        asterischi        5
078000071011     c                   move      gnpDMC        tipo_Invio       12
078100071011     c                   movel     tipo_Invio    sent              4
078200071011     c                   if        asterischi = *all'*' and sent = 'SENT'
078300071011     c                   sub       1             wNPR
078400071011     c                   end
078500071011      *****
078600071011      *
078700970107     C*  Reperisco nr.progressivo iniziale nota
078800970107     C     WNPR          DIV       5             WNRFAX
078900970108     C                   MVR                     WRESTO            2 0
079000970108     C     WRESTO        IFEQ      0
079100970108     C                   Z-ADD     5             WRESTO
079200050223     c* tolgo 1 al numero di fax (WNRFAX) se il resto è = 0 (progressivo=
079300050223     c* 5 o suoi multipli) perchè alla fine aggiunge 1 al numero di fax
079400050223     C                   sub       1             WNRFAX
079500970108     C                   END
079600970108     C     WRESTO        SUB       1             WSOTTR            2 0
079700970108     C     WNPR          SUB       WSOTTR        WULFAX            5 0
079800970107     C*  Imposto proggr. nr. di fax dall'ultimo
079900970107     C                   Z-ADD     WULFAX        KNPR
080000970108     C                   ADD       1             WNRFAX
080100970108     C                   END
080200970107     C*
080300970107     C                   ENDSR
080400961127     C*--------------------------------------------------------------*
080500961127     C*  Operazioni iniziali
080600961127     C*--------------------------------------------------------------*
080700961127     C     *INZSR        BEGSR
080800961127     C*
080900961119     C     *ENTRY        PLIST
081000961119     C                   PARM                    KPJBA
081100961127     C                   MOVEL     KPJBU         FNLG21
081200071009     c                   movel     D21PRF        Outq_Fax         10
081300071009      ***
081400071010     C*  Comunque chiude eventuale OVRPRTF precedente
081500071010     C*   esegue la DLTOVR
081600071010     c                   exsr      DLTOVR_LETEST
081700071009     C*
081800071009     C* quindi esegue la
081900071009     C*  OVRPRTF x FAX
082000071009     c                   z-add     50            lung
082100071009     C                   MOVEL     *BLANKS       COMMAN
082200071009     C                   MOVEA     OVR1(1)       COMMAN
082300071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
082400071009
082500071009     C                   CALL      'QCMDEXC'                            99
082600071009     C                   PARM                    COMMAN           80
082700071009     C                   PARM                    LUNG             15 5
082800961127     C*
082900071009     c                   open      LETEST
083000071009     C*
083100961127     C                   Z-ADD     1             CODUT
083200961127     C                   CALL      'X§PARUT'
083300961127     C                   PARM                    UTEDSE
083400961127     C     CODUT         CABEQ     -1            FINE
083500110606     C                   MOVEL     REC42         CNCR42
083600110606     C**                 MOVEL     RAGUT         RSUT             20
083700110606     C**                 MOVEL     RAGUT         STPRUT
083800110606     C                   MOVEL     simRGS        RSUT             20
083900110606     C                   MOVEL     simRGS        STPRUT
084000961129     C*--- RICERCA CAPOCONTI------------------------------------------*
084100130304     C                   DO        50            X                 3 0
084200961202     C                   MOVE      TCU(X)        TCUDS
084300961129     C     F56           IFEQ      'CG'
084400961129     C     F4            ANDEQ     '1'
084500961129     C     F3            ANDEQ     '0'
084600961129     C                   Z-ADD     KCU(X)        CLIITA            4 0
084700961129     C                   END
084800961129     C                   END
084900961129     C*
085000961129     C* Definisco chiave di accesso
085100961129     C     KACO          KLIST
085200961129     C                   KFLD                    KKUT
085300961129     C                   KFLD                    KKCC
085400961129     C                   KFLD                    KKSC
085500961129     C     KIND          KLIST
085600961129     C                   KFLD                    KKUT
085700961129     C                   KFLD                    KKCC
085800961129     C                   KFLD                    KKSC
085900961129     C     KGCA          KLIST
086000961129     C                   KFLD                    KAAS
086100961129     C                   KFLD                    KLNP
086200961129     C                   KFLD                    KNRS
086300961129     C                   KFLD                    KNSP
086400961129     C     KARB          KLIST
086500961129     C                   KFLD                    KAAS
086600961129     C                   KFLD                    KLNP
086700961129     C                   KFLD                    KNRS
086800961129     C                   KFLD                    KNSP
086900970107     C     KGNA1         KLIST
087000970107     C                   KFLD                    KAGC
087100970107     C                   KFLD                    KFGC
087200970107     C                   KFLD                    KNGC
087300970107     C                   KFLD                    KFRG
087400970107     C                   KFLD                    KFAS
087500970107     C     KGNA2         KLIST
087600961127     C                   KFLD                    KAGC
087700961127     C                   KFLD                    KFGC
087800961127     C                   KFLD                    KNGC
087900961127     C                   KFLD                    KFRG
088000961127     C                   KFLD                    KFAS
088100970107     C                   KFLD                    KNPR
088200980521     C     KAR4          KLIST
088300980521     C                   KFLD                    KAAS
088400980521     C                   KFLD                    KLNP
088500980521     C                   KFLD                    KNRS
088600980521     C                   KFLD                    KNSP
088700980521     C                   KFLD                    KTRC
088800171018      *--
088900171018     C     Kpnd          KLIST
089000171018     C                   KFLD                    KAAS
089100171018     C                   KFLD                    KLNP
089200171018     C                   KFLD                    KNRS
089300171018     C                   KFLD                    KNSP
089400171018      *--
089500961119     C* Definisco variabili
089600980521     C     *LIKE         DEFINE    AR4TRC        KTRC
089700961127     C     *LIKE         DEFINE    GNPAGC        KAGC
089800961127     C     *LIKE         DEFINE    GNPFGC        KFGC
089900961127     C     *LIKE         DEFINE    GNPNGC        KNGC
090000961127     C     *LIKE         DEFINE    GNPFRG        KFRG
090100961127     C     *LIKE         DEFINE    GNPFAS        KFAS
090200970107     C     *LIKE         DEFINE    GNPNPR        KNPR
090300961129     C     *LIKE         DEFINE    TABCOD        KCOD
090400961129     C     *LIKE         DEFINE    ACOKUT        KKUT
090500961129     C     *LIKE         DEFINE    ACOKCC        KKCC
090600961129     C     *LIKE         DEFINE    ACOKSC        KKSC
090700961129     C     *LIKE         DEFINE    ARBAAS        KAAS
090800961129     C     *LIKE         DEFINE    ARBLNP        KLNP
090900961129     C     *LIKE         DEFINE    ARBNRS        KNRS
091000961129     C     *LIKE         DEFINE    ARBNSP        KNSP
091100071009     C     *LIKE         DEFINE    NTCAPL        kcapl
091200071009     C     *LIKE         DEFINE    NTCNK1        kcnk1
091300071009     C     *LIKE         DEFINE    NTCNK2        kcnk2
091400071009     C     *LIKE         DEFINE    NTCTNT        kctnt
091500961129     C*
091600961127     C* Imposto variabili
091700961127     C                   MOVEL     'N'           WFINE             1
091800961202     C                   MOVEL     '1'           WTPVID            1
091900961129     C*
092000000314     C* Carico dati DS partner in TXT solo se no DPD
092100000314     C     D21DPD        IFNE      'S'
092200961129     C                   Z-ADD     1             X
092300961129     C                   MOVEL     'PT'          KCOD
092400961202     C     KCOD          CHAIN     EDTAB01L                           30
092500961129     C     *IN30         DOWEQ     '0'
092600961129     C     TABFLG        IFEQ      ' '
092700961129     C                   ADD       1             X
092800961202     C                   MOVEL     TABUNI        DPT(X)
092900961202     C                   MOVEL     TABUNI        DSPT
093000961129     C                   MOVEL     TABKEY        CPU(X)
093100961129     C                   MOVEL     TABKEY        CPT(X)
093200120207     C                   MOVEL     TABKEY        CPZ(X)
093300170511     C                   MOVEL     TABKEY        CPS(X)
093400961129     C                   MOVE      '    '        CPT(X)
093500961129     C                   MOVEL     §PTKSC        KSC(X)
093600961129     C                   MOVEL     §PTLNP        LNA(X)
093700961129     C                   END
093800961202     C     KCOD          READE     EDTAB01L                               30
093900961129     C                   END
094000961129     C*
094100961129     C                   MOVEL     'PU'          KCOD
094200961202     C     KCOD          CHAIN     EDTAB01L                           30
094300961129     C     *IN30         DOWEQ     '0'
094400961129     C     TABFLG        IFEQ      ' '
094500961129     C                   Z-ADD     1             X
094600961129     C     TABKEY        LOOKUP    CPU(X)                                 32
094700961129     C     *IN32         IFEQ      '1'
094800961129     C                   MOVEL     TABUNI        DSPU
094900961129     C* Se no partner metto zero in LNA
095000961129     C     §PUTPC        IFEQ      'C'
095100961129     C                   Z-ADD     0             LNA(X)
095200961129     C                   END
095300961129     C                   END
095400961129     C                   END
095500961202     C     KCOD          READE     EDTAB01L                               30
095600961129     C                   END
095700000314     C*
095800120206     C*
095900120206     C                   MOVEL     'PZ'          KCOD
096000120206     C     KCOD          CHAIN     EDTAB01L                           30
096100120206     C     *IN30         DOWEQ     '0'
096200120206     C     TABFLG        IFEQ      ' '
096300120206     C                   Z-ADD     1             X
096400120206     C     TABKEY        LOOKUP    CPZ(X)                                 32
096500120206     C     *IN32         IFEQ      '1'
096600120206     C                   MOVEL     TABUNI        DSPZ
096700120206     C                   MOVEL     TABUNI        DPZ(X)
096800120206     C* imposta il valore del codice di Export
096900120206     C     §PzKSCexp     IFne      *blank
097000120206     C                   move      §PzKSCexp     exp(X)
097100120206     c                   else
097200120206     C                   move      *zeros        exp(X)
097300120206     C                   END
097400120206     C                   END
097500120206     C                   END
097600120206     C     KCOD          READE     EDTAB01L                               30
097700120206     C                   END
097800120206     C*
097900170511     C* aggiunto la tab.PS dove c'è la NON volontà di inviare
098000170511     C*  la LETEST al Partner. AZKAR vuole che mettiamo direttamente
098100170511     C* sul loro sito le disposizioni di GIACENZA perchè NON riescono
098200170511     C*  a stare nei tempi se ricevono le nostre MAIL come facevamo
098300170511     C* di solito.
098400170511     C                   MOVEL     'PS'          KCOD
098500170511     C     KCOD          setll     EDTAB01L
098600170511     C     KCOD          Reade     EDTAB01L
098700170511     C                   DOW       not %EoF(EDTAB01L)
098800170511     C                   IF        TABFLG =' '
098900170511     C                   Z-ADD     1             X
099000170511     C     TABKEY        LOOKUP    CPS(X)                                 32
099100170511     C   32              MOVEL     TABUNI        DPS(X)
099200170511     C                   END
099300170511     C     KCOD          Reade     EDTAB01L
099400170511     C                   END
099500170511     C*
099600000314     C                   END
099700170511     C*
099800961129     C* Reperisco data del giorno
099900961202     C                   TIME                    WHHDAT           14 0
100000961219     C                   MOVE      WHHDAT        G02DAT
100100961219     C                   MOVE      *ZEROS        G02INV
100200961219     C                   MOVEL     *BLANK        G02ERR
100300961219     C                   CALL      'XSRDA8'
100400961219     C                   PARM                    WLBDA8
100500961219     C                   Z-ADD     G02INV        WOGGI             8 0
100600980521     C*  Modalità visualizzazione
100700980521     C     D21VIS        COMP      'V'                                    12
100800980521      *
100900961127     C                   ENDSR
101000071009     C*----------------------------------------------------------------*
101100071009     C*  Controllo dati x invio mail
101200071009     C*----------------------------------------------------------------*
101300071009     C     CTRMail       BEGSR
101400071009      *
101500071009     C     Kntc          KLIST
101600071009     C                   KFLD                    kcapl
101700071009     C                   KFLD                    kcnk1
101800071009     C                   KFLD                    kcnk2
101900071009     C                   KFLD                    kctnt
102000071009      *
102100071009     c                   move      'C'           kcapl
102200071009     c                   movel     '0151'        kcnk1
102300071009     c                   move      kKSC          kcnk1
102400071009     c                   move      '88'          kctnt
102500071009     C*
102600071009     c                   move      'N'           invio_Mail        1
102700071009      * se esiste  e-mail su TFNTC nota '88'
102800071009     c     kntc          chain     tfntc01l
102900071009     c                   if        %found(tfntc01l)
103000071009     c                   move      'S'           invio_Mail
103100071009     c                   end
103200071009     C*
103300071009     C                   ENDSR
103400071009     C*----------------------------------------------------------------*
103500071009     C*  Esegue OVRPRTF x lancio E-mail
103600071009     C*----------------------------------------------------------------*
103700071009     C     OVRPRTF_x_mailBEGSR
103800071009     C*
103900071009     C*   Prima di tutto chiude il PRTF
104000071009     c                   exsr      Close_PRTF
104100071009     C*
104200071009     C* se è riuscita la DLTOVR
104300071009     c                   IF        *in99 = *off
104400071009     C*
104500071009     C*  Quindi riapre eseguendo la giusta OVRPRTF x invio coda E-mail
104600071009     C*   con tutti i parametri tramite il TRUL44R
104700071010     C*
104800071009     c                   clear                   trul44ds
104900071009     c                   clear                   trtcm1ds
105000071009     c                   eval      d44pgm = 'FNLG21R'
105100071009     c                   eval      d44cl  = '005'
105200071009     c                   move      Kksc          d44ksc
105300071009     c                   eval      d44apl = 'C'
105400071009     c                   eval      d44nk1 = '0151' + %editc(Kksc:'X')
105500071009     c                   eval      d44tnt = '88'
105600071010     c                   eval      d44scope='*JOB'
105700071009     c                   eval      d44ovr = 'S'
105800140226     c                   eval      d44obj = '*OBJM*Further Instruction for -
105900140226     c                             exceptions Shipment Nr.: '
106000140226     c                              + %editc(d21AAS:'Z') +
106100071026     c                             ' ' + %char(d21LNP) + ' ' + %char(d21NRS) +
106200071026     c                             ' ' + %char(d21NSP)
106300071009     c                   eval      d44prtf = 'LETEST'
106400071009     c                   call      'TRUL44R'
106500071009     c                   parm                    trul44ds
106600071009     c                   parm                    trtcm1ds
106700071009      *
106800071009      **  Se c'è stato qualche problema
106900071009     c     d44err        ifne      *blank
107000071009     C**
107100071009      * se non è riuscita x qualche motivo lo rimanda come FAX
107200071009     c                   close     LETEST                               99
107300071009     C*  OVRPRTF x FAX
107400071009     c                   z-add     50            lung
107500071009     C                   MOVEL     *BLANKS       COMMAN
107600071009     C                   MOVEA     OVR1(1)       COMMAN
107700071009     c                   eval      comman = %Trim(Comman)+ Outq_FAX + ')'
107800071009
107900071009     C                   CALL      'QCMDEXC'                            99
108000071009     C                   PARM                    COMMAN           80
108100071009     C                   PARM                    LUNG             15 5
108200071009     C*
108300071009     c                   end
108400071009     C**
108500071009     c                   endIF
108600071009     C**
108700071009     C*   Apre il file dopo OVRPRTF
108800071009     c                   open      LETEST
108900071009     C*
109000071009     C                   ENDSR
109100071009     C*----------------------------------------------------------------*
109200071010     C*  Esegue la DLTOVR sul file
109300071009     C*----------------------------------------------------------------*
109400071010     C     DLTOVR_LETEST BEGSR
109500071009     C*
109600071010     C*  DLTOVR
109700071010     c                   z-add     50            lung
109800071010     C                   MOVEL     *BLANKS       COMMAN
109900071010     C                   MOVEA     dlt1(1)       COMMAN
110000071010     C                   CALL      'QCMDEXC'                            99
110100071010     C                   PARM                    COMMAN           80
110200071010     C                   PARM                    LUNG             15 5
110300071010     C*
110400071010     c                   if        *in99
110500071010     C                   MOVEA     dlt2(1)       COMMAN
110600071010     C                   CALL      'QCMDEXC'                            99
110700071010     C                   PARM                    COMMAN           80
110800071010     C                   PARM                    LUNG             15 5
110900071010     c                   end
111000071010      *
111100071010     C     end_dlt       ENDSR
111200071010     C*----------------------------------------------------------------*
111300071010     C*  Chiusura PRTF  x mail
111400071010     C*----------------------------------------------------------------*
111500071010     C     Close_PRTF    BEGSR
111600071010     C*
111700071009     C                   close     LETEST
111800071010     c                   exsr      DLTOVR_LETEST
111900071009     C*
112000071009     C                   ENDSR
112100980521     C*--------------------------------------------------------------*
112200961202**  Schiera con testi da visualizz.nella finestra dell'esec.disposizioni
112300961129REDELIVER TO:
112400961202AND PROVIDE P.O.D. AFTER DELIVERY
112500961129SEND SHIPMENT BACK TO ITALY
112600961129IMPORTANT !
112700961202AS PER OPS MANUAL THE "SHIPMENT REFERENCE" MUST
112800961129BE OUR SHIPMENT NR. IN "FREE TEXT NOTE", PLEASE
112900961129SPECIFY IT IS A RETURN SHIPMENT.
113000961202**
113100961202Numero fax errato impossibile procedere oltre
113200061107Nr.fax errato. Manca anagr.Depot in tabella Depot DPD->Avvisare CED!
113300070515Attenzione: Spedizione Chronopost - Inviare Manualmente il FAX
113400171018Attenzione: Non trovato il PARCEL DPD per poter inviare il FAX
113500071009**         dlt1
113600071009DLTOVR FILE(LETEST)
113700071009**         dlt2
113800071009DLTOVR FILE(LETEST) LVL(*JOB)
113900071009**         Ovr1
114000071009OVRPRTF FILE(LETEST) OUTQ(
