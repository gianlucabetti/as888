000100931018     H DECEDIT('0,') DATEDIT(*DMY.)
000200931018?     *--------------------------------------------------------------*
000300941104      * TNSC26R                                                      *
000400931018      *       *-----------------------------------------*            *
000500931018      *       *  STAMPE CONTRASSEGNI: STAMPA DATI W.F.  *            *
000600931018      *       *-----------------------------------------*            *
000700931018      *                                                              *
000800931018?     *--------------------------------------------------------------*
000900940218      * 01 -ORDINATO PER LINEA DI PARTENZA                           *
001000940218      * 02 -ORDINATO PER LINEA DI ARRIVO                             *
001100940218      * 03 -SCOPERTI IN PARTENZA                                     *
001200940218      * 04 -SCOPERTI IN ARRIVO                                       *
001300940218      * 05 -STAMPA RIEPILOGO FINALE                                  *
001400940218      * 21 -CHAIN SU TABEL                                           *
001500980520      * 23 -ON STAMPO SCRITTA AWB                                    *
001600940218      * 40 -LOOKUP SU LINEA IN GESTIONE                              *
001700940218      * 78 -CHAIN SU AZORG                                           *
001800940218      * 98 -ALMENO UN RECORD                                         *
001900940218      * 99 -OVERFLOW                                                 *
002000940218?     *--------------------------------------------------------------*
002100941104     FFNCSBWRK  IP   E           K DISK
002200931018?     *---------
002300931018     FTABEL00F  IF   E           K DISK    USROPN
002400990205?     *---------
002500990205     FFNDCT02L  IF   E           K DISK
002600931018?     *---------
002700941104     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002800931018?     *---------
002900941104     FTNSC26P   O    E             PRINTER OFLIND(*IN99)
003000931018?     *--------------------------------------------------------------*
003100931018?     *  Schiere x stampa dati totali filiale                        *
003200931018?     *--------------------------------------------------------------*
003300020507     D*FIL1            S              3    DIM(30)
003400931018     D LIN             S              3  0 DIM(30)
003500931018     D NMR             S              6  0 DIM(30)
003600000105     D NSC             S              6  0 DIM(30)
003700941104     D IPC             S             15  3 DIM(30)
003800941104     D IPV             S             15  3 DIM(30)
003900000105     D IPE             S             15  3 DIM(30)
004000961216      *  Schiere per caricamento dati riepilogo x filiali/valute
004100961216     D LIV             S              6    DIM(500)
004200941104      *  Schiere per caricamento dati valute
004300941104     D CCV             S              3    DIM(100)
004400000105     D DVA             S              1    DIM(100)
004500011001     D******              CAM       100  9 3             CAMBIO DIVISA ITA.
004600950117      *  Schiere per caricamento dati consegna anomala
004700950117     D C7O             S              1    DIM(100)
004800950117     D D7O             S             20    DIM(100)
004900980113      *  Schiere per caricamento Filiali estero
005000980113     D EST             S              3  0 DIM(200)
005100980113     D DES             S             15    DIM(100)
005200931018?     *--------------------------------------------------------------*
005300931018?     *  DS                                                          *
005400931018?     *--------------------------------------------------------------*
005500000105     D DSBS02        E DS                  EXTNAME(TIBS02DS)
005600020729     D OG143         E DS
005700000105     D DGED          E DS
005800961216     D KPJBA         E DS
005900961216     D MULT            DS                  OCCURS(500)
006000961216     D  NMV                    1      6  0
006100961216     D  IMV                    7     21  3
006200931018      *
006300010615     D YEURDS        E DS                  EXTNAME(YEURCODS)
006400941104     D TNSC24        E DS                  EXTNAME(TNSC24DS)
006500931018      *
006600941104     D DSCV          E DS
006700941104     D*
006800931018     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006900941104     D WLBDA8          DS
007000941104     D  G02DAT                 1      8  0
007100941104     D  G02INV                 9     16  0
007200941104     D  G02ERR                17     17
007300941104     D  G02TGI                18     22  0
007400950117     D                 DS
007500950117     D  WSTRG                  1     11
007600950117     D  WSTR                   1     20
007700020507     d trul06ds      e ds
007800020507     d  lint                   1     90  0 dim(30)
007900931018     D CNCR80        E DS
008000931018      *
008100931018     D                SDS
008200931018     D  NOMPG                  1     10
008300931018     D*--------------------------------------------------------------*
008400931018     D*  COSTANTI                                                    *
008500931018     D*--------------------------------------------------------------*
008600931018     D ORD1            C                   CONST('LIN.DI PARTENZA')
008700931018     D ORD2            C                   CONST('LIN.DI ARRIVO  ')
008800950117     D NOT1            C                   CONST('DA INCA.')
008900950117     D NOT2            C                   CONST('GIACENTE ')
009000990205     D NOT3            C                   CONST('C.A. APE.')
009100931112?     *--------------------------------------------------------------*
009200931112?     *  ROTTURE di livello                                          *
009300931112?     *--------------------------------------------------------------*
009400941104     IFNCSWRK
009500931112     I                                          CSBKE1        L1
009600941104?     *--------------------------------------------------------------*
009700941104?     *  Ridefinizione interna campi azorg                           *
009800941104?     *--------------------------------------------------------------*
009900941104     IAZORG01L  AA
010000941104     I                             P    4    5 0ORGFIL
010100980113     I                                 11   11  ORGFL1
010200941104     I                                 14   33  ORGDES
010300020729     I                               3776 3800  ORGDE3
010400931018      *---------------------------------------------------------------*
010500931018      *   C I C L O      P R I N C I P A L E                          *
010600931018      *---------------------------------------------------------------*
010700931018     C*  Accendo indicatore se esiste almeno un record
010800931018     C  N98              SETON                                        98
010900931018     C*
011000931018     C*  Scrittura riga di Printer file
011100931018     C                   MOVE      CSBLNA        LNA
011200931018     C                   MOVE      CSBLNP        LNP
011300931112     C*  Controllo se devo stampare il dettaglio
011400941104     C     D24DET        IFEQ      'SI'
011500931112     C                   EXSR      WRTDET
011600931116     C                   END
011700980113     C*  Controllo se filiale estera
011800980113     C     CSBLNA        LOOKUP    EST                                    33
011900931018     C*  Calcolo totale x le filiali in gestione
012000961216     C     D24SCP        IFEQ      '1'
012100961216     C     D24ORD        OREQ      'P'
012200980113     C     *IN33         OREQ      '0'
012300941104     C                   EXSR      CALTOT
012400961216     C*  Se sto esaminando filiale estera calcolo totali valuta
012500961216     C                   ELSE
012600961216     C                   EXSR      CALTVA
012700961216     C                   END
012800931112     C*
012900931112     C*  Controllo se devo gestire stampa totali linea
013000961216     CL1 98D24TPI        IFEQ      'I'
013100961216     CL1 05              EXSR      TOTFIL
013200961216     CL1                 ELSE
013300961216     CL1                 EXSR      TOTFVA
013400961216     CL1                 END
013500931018     C*
013600931018     C*  Scrivo singole righe di dettaglio
013700961216     CLR 98D24TPI        IFEQ      'I'
013800961216     CLR                 EXSR      TOTGEN
013900961216     CLR                 END
014000931112      *---------------------------------------------------------------*
014100931112      *  Wrtdet: Scritture righe di dettaglio                         *
014200931112      *---------------------------------------------------------------*
014300931112     C     WRTDET        BEGSR
014400990205     C*
014500990205     C* GESTISCO FLAG PRATICA CA APERTA
014600990205     C                   MOVEL     *BLANKS       WCAAPE            1
014700990205     C*
014800931112     C                   MOVE      CSBAAS        AA
014900931112     C                   MOVE      CSBNRS        NRS
015000931112     C                   MOVE      CSBNSP        NSP
015100970625     C     D24TPI        IFEQ      'I'
015200970625     C     CSBDDP        OREQ      0
015300941104     C                   MOVEL     CSBAAS        G02INV
015400941104     C                   MOVE      CSBMGS        G02INV
015500970625     C                   ELSE
015600970625     C                   Z-ADD     CSBDDP        G02INV
015700970625     C                   END
015800941104     C                   Z-ADD     0             G02DAT
015900941104     C                   MOVEL     '3'           G02ERR
016000941104     C                   CALL      'XSRDA8'
016100941104     C                   PARM                    WLBDA8
016200941104     C                   MOVE      G02DAT        DSP
016300941104     C                   MOVEL     CSBRSD        RGD
016400941104     C                   MOVEL     CSBRSM        RGM
016500931112     C                   MOVE      CSBRMN        RMN
016600941104     C                   MOVE      CSBRMA        RMA
016700961217     C                   MOVE      CSBCAS        COMO3             3 0
016800950117     C     COMO3         IFEQ      *ZEROS
016900950117     C                   Z-ADD     CSBCAS        ASS1
017000950117     C                   SETON                                        06
017100950117     C                   ELSE
017200950117     C                   Z-ADD     CSBCAS        ASS2
017300950117     C                   SETOFF                                       06
017400950117     C                   END
017500941104     C                   MOVE      CSBVCA        VCA
017600961216     C     D24TPI        IFEQ      'I'
017700941104     C                   MOVE      CSBCMB        CMB
017800961216     C                   END
017900941104     C                   MOVE      CSBDGC        G02INV
018000941104     C                   Z-ADD     0             G02DAT
018100941104     C                   MOVEL     '3'           G02ERR
018200941104     C                   CALL      'XSRDA8'
018300941104     C                   PARM                    WLBDA8
018400941104     C                   MOVE      G02DAT        DGC
018500941104     C                   MOVE      CSBDCM        G02INV
018600941104     C                   Z-ADD     0             G02DAT
018700941104     C                   MOVEL     '3'           G02ERR
018800941104     C                   CALL      'XSRDA8'
018900941104     C                   PARM                    WLBDA8
019000941104     C                   MOVE      G02DAT        DCS
019100950117     C*  Controllo se devo eseguire decodifica consegna anomala
019200950117     C                   MOVEL     *BLANKS       WSTR
019300950117     C     CSBCCA        IFNE      *BLANKS
019400950117     C                   Z-ADD     1             XX
019500950117     C     CSBCCA        LOOKUP    C7O(XX)                                31
019600950117     C     *IN31         IFEQ      '1'
019700961216     C     D24TPI        ANDEQ     'I'
019800950117     C                   MOVEL     D7O(XX)       WDCA             18
019900950117     C                   MOVEL     C7O(XX)       WCCA              1
020000950117     C                   MOVEL     WCCA          WSTR             20
020100950117     C                   MOVE      WDCA          WSTR
020200950117     C                   END
020300950117     C                   END
020400931112     C*
020500990205     C*  Controllo se contrassegno solo da incassare, se non è ancora stata
020600990205     C*  effettuata la consegna, se esiste pratica C.A. aperta
020700961216     C     D24TPI        IFEQ      'I'
020800020507     C                   MOVE      *BLANKS       NOTa
020900931112     C     CSBDCM        IFNE      0
021000931112     C     *IN04         ANDEQ     '1'
021100020507     C                   MOVEL     NOT1          NOTa
021200931112     C                   END
021300931112     C*
021400931112     C     CSBDGC        IFNE      0
021500931222     C     CSBDCM        ANDEQ     0
021600020507     C                   MOVEL     NOT2          NOTa
021700931112     C                   END
021800990205     C* Eseguo chain su File dei danni per verificare che non esista
021900990205     C* una pratica C.A. aperta su quella bolla
022000990205     C                   MOVE      CSBAAS        KAAS
022100990205     C                   MOVE      CSBLNP        KLNP
022200990205     C                   MOVE      CSBNRS        KNRS
022300990205     C                   MOVE      CSBNSP        KNSP
022400990205     C     KDCT          CHAIN     FNDCT02L                           31
022500990205     C     *IN31         DOWEQ     '0'
022600990205     C     DCTATB        ANDEQ     'A'
022700990205     C     *IN31         OREQ      '0'
022800990205     C     DCTDCH        ANDNE     0
022900990205     C     KDCT          READE     FNDCT02L                               31
023000990205     C                   END
023100990205     C*
023200990205     C     *IN31         IFEQ      '0'
023300990205     C     DCTATB        ANDNE     'A'
023400990205     C     DCTDCH        ANDEQ     0
023500990205     C                   MOVEL     'S'           WCAAPE            1
023600020507     C                   MOVEL     NOT3          NOTa
023700990205     C                   MOVEL     DCTAAC        G02INV
023800990205     C                   MOVE      DCTMGC        G02INV
023900990205     C                   Z-ADD     0             G02DAT
024000990205     C                   MOVEL     '3'           G02ERR
024100990205     C                   CALL      'XSRDA8'
024200990205     C                   PARM                    WLBDA8
024300990205     C                   MOVE      G02DAT        DGC
024400990205     C                   END
024500950117     C*
024600950117     C     WSTR          IFNE      *BLANKS
024700020507     C     NOTa          IFEQ      *BLANKS
024800020507     C                   MOVEL     WSTR          NOTa
024900950117     C                   ELSE
025000020507     C                   MOVE      WSTRG         NOTa
025100950117     C                   END
025200961216     C                   END
025300961216     C                   END
025400931112     C*  Test Overflow
025500961216     C     D24TPI        IFEQ      'I'
025600980217     C     *IN99         IFEQ      '1'
025700980217     C                   WRITE     TES00
025800980217     C                   SETOFF                                       99
025900980217     C                   END
026000980217     C                   WRITE     DET00
026100980217     C                   ELSE
026200980217     C     *IN99         IFEQ      '1'
026300980217     C                   WRITE     TESES
026400980217     C                   SETOFF                                       99
026500980217     C                   END
026600980217     C                   WRITE     DETES
026700931112     C                   END
026800931112      *
026900931112     C                   ENDSR
027000941104      *---------------------------------------------------------------*
027100941104      *  CALTOT: Calcolo totale  di dettaglio                         *
027200941104      *---------------------------------------------------------------*
027300941104     C     CALTOT        BEGSR
027400941104     C*
027500991018     C                   Z-ADD     1             X
027600000105     C     D24LIN        IFEQ      0
027700000105     C   03LNP           LOOKUP    LIN(X)                                 40
027800000105     C   04LNA           LOOKUP    LIN(X)                                 40
027900000105     C                   END
028000000105     C*  Sommo totali
028100000105     C                   ADD       1             NSC(X)
028200990205     C     WCAAPE        IFNE      'S'
028300941104     C*  Sommo totali
028400941104     C                   ADD       1             NMR(X)
028500941104     C* Controllo se il C/Assegno è in lire o in divisa
028600941104     C     VCA           IFNE      *BLANKS
028700941104     C                   Z-ADD     1             XX
028800941104     C     VCA           LOOKUP    CCV(XX)                                32
028900941104     C                   END
029000941104     C* Somma nel totale corretto
029100941104     C     VCA           IFEQ      *BLANKS
029200011001     C     VCA           OREQ      §GEDCN
029300941104     C                   ADD       CSBCAS        IPC(X)
029400000105     C                   ELSE
029500000105     C     DVA(XX)       IFEQ      'S'
029600000105     C     *IN32         ANDEQ     '1'
029700000105     C                   ADD       CSBCAS        IPE(X)
029800941104     C                   ELSE
029900010615     C***        CMB       IFEQ 0
030000010615     C***                  Z-ADDCAM,XX    CAMBIO  94
030100010615     C***                  ELSE
030200010615     C***                  Z-ADDCMB       CAMBIO
030300010615     C***                  END
030400010615     C***                  SELEC
030500010615     C***        NRDEC     WHEQ 2
030600010615     C***        CAMBIO    MULT CSBCAS    IMPV2  142H
030700010615     C***                  Z-ADDIMPV2     WIMPV
030800010615     C***        NRDEC     WHEQ 1
030900010615     C***        CAMBIO    MULT CSBCAS    IMPV1  131H
031000010615     C***                  Z-ADDIMPV1     WIMPV
031100010615     C***        NRDEC     WHEQ 0
031200010615     C***        CAMBIO    MULT CSBCAS    IMPV0  120H
031300010615     C***                  Z-ADDIMPV0     WIMPV
031400010615     C***                  OTHER
031500010615     C***        CAMBIO    MULT CSBCAS    WIMPV  153
031600010615     C***                  ENDSL
031700010615     C*
031800010615     C* Per convertire gli importi in valute estere utilizzo
031900010615     C* la routine YEURCO in quanto il cambio indicato sul
032000010615     C* file dei C/assegni di filiale non è affidabile
032100010615     C                   CLEAR                   YEURDS
032200010615     C                   MOVEL     VCA           YECDVI
032300010615     C                   Z-ADD     CSBCAS        YECIMI
032400011001     C                   MOVEL     §GEDCN        YECDVO
032500010615     C* passo al pgm io il nr. dei decimali della divisa corrente
032600010615     C* in base a quanto memorizzato sulla tabella CV
032700010615     C                   MOVE      NRDEC         YECDCO
032800010615     C                   MOVEL     'H'           YECTAR
032900010615     C                   CALL      'YEURCO'
033000010615     C                   PARM                    YEURDS
033100010615     C                   Z-ADD     YECIMO        WIMPV            15 3
033200010615     C                   ADD       WIMPV         IPV(X)
033300941104     C                   END
033400990205     C                   END
033500000105     C                   END
033600941104     C*
033700941104     C                   ENDSR
033800961216      *---------------------------------------------------------------*
033900961216      *  CALTVA: Calcolo totali per filiali/valuta                    *
034000961216      *---------------------------------------------------------------*
034100961216     C     CALTVA        BEGSR
034200961216     C*
034300990205     C     WCAAPE        IFNE      'S'
034400961216     C* Controllo se il C/Assegno è in lire o in divisa
034500961216     C     VCA           IFNE      *BLANKS
034600961216     C                   Z-ADD     1             XX
034700961216     C     VCA           LOOKUP    CCV(XX)                                32
034800961216     C                   END
034900961216     C*  Se no ho un c/ass. in valuta abblenco VCA
035000000105     C     VCA           IFNE      *BLANKS
035100011001     C     §GEDCN        ANDEQ     VCA
035200961216     C                   MOVE      *BLANKS       VCA
035300961216     C                   END
035400961216     C*
035500991018     C                   Z-ADD     1             XW                3 0
035600961216     C                   MOVEL     LNA           LINVAL
035700961216     C                   MOVE      VCA           LINVAL            6
035800991018     C     LINVAL        LOOKUP    LIV(XW)                                40
035900961216     C*  Sommo totali
036000961217     C     *IN40         IFEQ      '0'
036100961216     C                   ADD       1             YY
036200961216     C                   MOVEL     LNA           LIV(YY)
036300961216     C                   MOVE      VCA           LIV(YY)
036400991018     C                   Z-ADD     YY            XW
036500961216     C                   END
036600991018     C     XW            OCCUR     MULT
036700961216     C                   ADD       1             NMV
036800961216     C                   ADD       CSBCAS        IMV
036900990205     C                   END
037000961216     C*
037100961216     C                   ENDSR
037200931018      *---------------------------------------------------------------*
037300931018      *  Totgen: Stampa totale genrale + totali filiali in gestione   *
037400931018      *---------------------------------------------------------------*
037500931018     C     TOTGEN        BEGSR
037600931018     C*
037700931116     C                   WRITE     TES01
037800931116     C                   SETOFF                                       0599
037900931116     C*
038000020507     C                   DO        30            X                 2 0
038100931112     C                   EXSR      TOTFIL
038200931112     C                   END
038300931018     C*
038400931018     C*  Test Overflow
038500931018     C     *IN99         IFEQ      '1'
038600931018     C                   WRITE     TES00
038700931018     C                   SETOFF                                       99
038800931018     C                   END
038900931116     C*
039000931116     C                   XFOOT     NMR           TOT
039100931116     C                   XFOOT     IPC           TOTIMP
039200941104     C                   XFOOT     IPV           TOTIMV
039300000105     C                   XFOOT     IPE           TOTIME
039400011002     C                   MOVEL     §GEDCN        TOTVAV
039500931116     C*
039600931018     C                   WRITE     TOTGN
039700931112     C*
039800931018     C                   ENDSR
039900931112      *---------------------------------------------------------------*
040000931112      *  Totfil: Stampa totale filiale                                *
040100931112      *---------------------------------------------------------------*
040200931112     C     TOTFIL        BEGSR
040300931112     C*
040400931112     C     NMR(X)        IFNE      0
040500000105     C     NSC(X)        ORNE      0
040600931112     C                   MOVEL     *BLANKS       ORGDES
040700931112     C     LIN(X)        CHAIN     AZORG01L                           78
040800931112     C                   MOVE      LIN(X)        FIL
040900931112     C                   MOVE      ORGDES        DEC
041000931112     C                   MOVE      NMR(X)        NUM
041100931112     C                   MOVE      IPC(X)        IMP
041200941104     C                   MOVE      IPV(X)        IMPV
041300000105     C                   MOVE      IPE(X)        IMPE
041400011002     C                   MOVEL     §GEDCN        VALV
041500931112     C*  Test Overflow
041600931112     C     *IN99         IFEQ      '1'
041700931112     C                   WRITE     TES00
041800931112     C                   SETOFF                                       99
041900931112     C                   END
042000931112     C*
042100931112     C                   WRITE     TOTFL
042200931112     C                   END
042300931112     C*
042400931112     C                   ENDSR
042500961216      *---------------------------------------------------------------*
042600961216      *  Totfva: Stampa totale filiale/valuta                         *
042700961216      *---------------------------------------------------------------*
042800961216     C     TOTFVA        BEGSR
042900961216     C*
043000991018     C                   DO        YY            XW
043100991018     C     XW            OCCUR     MULT
043200961216     C     NMV           IFNE      0
043300991018     C                   MOVEL     LIV(XW)       WLIV              3 0
043400980113     C     WLIV          IFLT      0
043500980113     C                   MULT      -1            WLIV
043600980113     C                   END
043700961216     C                   Z-ADD     1             W                 3 0
043800980113     C     WLIV          LOOKUP    EST(W)                                 40
043900980113     C   40              MOVE      DES(W)        DEC
044000961216     C  N40              MOVE      *ALL'?'       DEC
044100961216     C                   MOVE      WLIV          FIL
044200961216     C                   MOVE      NMV           NUM
044300991018     C                   MOVE      LIV(XW)       VALES
044400961217     C                   MOVE      IMV           COMO3             3 0
044500961217     C     COMO3         IFEQ      *ZEROS
044600961217     C                   Z-ADD     IMV           IMPVE0
044700961217     C                   SETON                                        06
044800961217     C                   ELSE
044900961217     C                   Z-ADD     IMV           IMPVE
045000961217     C                   SETOFF                                       06
045100961217     C                   END
045200961216     C*  Test Overflow
045300961216     C     *IN99         IFEQ      '1'
045400961216     C                   WRITE     TESES
045500961216     C                   SETOFF                                       99
045600961216     C                   END
045700961216     C*
045800961216     C                   WRITE     TOTFE
045900961216     C                   END
046000961217     C                   END
046100961217     C*
046200970616     C*  Salto pagina per ogni partner
046300991018     C     *IN99         IFEQ      '0'
046400970618     C     *INLR         ANDEQ     '0'
046500970616     C                   WRITE     TESES
046600970616     C                   END
046700970616     C*
046800961217     C                   MOVEA     *BLANKS       LIV
046900961217     C                   DO        500           YY
047000961217     C     YY            OCCUR     MULT
047100961217     C                   CLEAR                   MULT
047200961217     C                   END
047300961217     C                   Z-ADD     0             YY                4 0
047400961216     C*
047500961216     C                   ENDSR
047600931018      *---------------------------------------------------------------*
047700931018      *  *INZSR: Operazioni iniziali                                  *
047800931018      *---------------------------------------------------------------*
047900931018     C     *INZSR        BEGSR
048000931018      *---------------------------------------------------------------*
048100931018     C     *ENTRY        PLIST
048200931018     C                   PARM                    KPJBA
048300941104     C                   MOVEL     KPJBU         TNSC24
048400011001     C*
048500011001     C     *LIKE         DEFINE    CSBCMB        CMB
048600931018     C*
048700931018     C                   Z-ADD     1             CODUT
048800931018     C                   CALL      'X§PARUT'
048900931018     C                   PARM                    UT§DSE
049000931018     C*          CODUT     CABEQ-1        FINE
049100931018     C                   MOVEL     RAGUT         RSUT             20
049200931018     C                   MOVEL     REC80         CNCR80
049300931018     C                   MOVEL     KNMUS         §SEI              6
049400931018     C*----------------------------------------------------*
049500931019     C*  Chiave x reperimento dati '£3'                    *
049600931018     C     KEYTAB        KLIST
049700931018     C                   KFLD                    CODUT
049800931018     C                   KFLD                    TBLCOD
049900931018     C                   KFLD                    TBLKEY
050000941104     C*
050100941104     C     KTAB1         KLIST
050200941104     C                   KFLD                    CODUT
050300941104     C                   KFLD                    TBLCOD
050400990205     C*
050500990205     C     KDCT          KLIST
050600990205     C                   KFLD                    KAAS
050700990205     C                   KFLD                    KLNP
050800990205     C                   KFLD                    KNRS
050900990205     C                   KFLD                    KNSP
051000990205     C*----------------------------------------------------*
051100990205     C     *LIKE         DEFINE    CSBAAS        KAAS
051200990205     C     *LIKE         DEFINE    CSBLNP        KLNP
051300990205     C     *LIKE         DEFINE    CSBNRS        KNRS
051400990205     C     *LIKE         DEFINE    CSBNSP        KNSP
051500931018     C*----------------------------------------------------*
051600931018     C*  Reperimanto codici filiali in gestione sull'AS
051700020507     C                   OPEN      TABEL00F
051800020507     c                   clear                   trul06ds
051900020507     c                   eval      d06cod = '£1'
052000020507     c                   movel     simfel        d06key
052100020507     c                   movel     trul06ds      kpjbu
052200020507     c                   call      'TRUL06R'
052300020507     c                   parm                    kpjba
052400020507     c                   movel     kpjbu         trul06ds
052500020507     c                   movea     lint          lin
052600020507     C*    D24LIN        IFEQ      0
052700020507     C*                  MOVEL     '£3'          TBLCOD
052800020507     C*                  MOVEL     SIMFEL        TBLKEY
052900020507     C*    KEYTAB        CHAIN     TABEL                              21
053000020507     C*    *IN21         IFEQ      '0'
053100020507     C*                  MOVEA     TBLUNI        FIL1
053200020507     C*                  DO        30            X                 2 0
053300020507     C*    FIL1(X)       IFGT      *ZEROS
053400020507     C*                  ADD       1             Y
053500020507     C*                  MOVE      FIL1(X)       LIN(Y)
053600020507     C*                  END
053700020507     C*                  END
053800931018     C*
053900020507     C*                  ELSE
054000020507     C*                  MOVE      SIMFEL        LIN(1)
054100020507     C*                  END
054200931018     C*
054300020507     C*                  ELSE
054400020507     C*                  Z-ADD     1             Y
054500020507     C*                  MOVE      D24LIN        LIN(1)
054600020507     C*                  END
054700000105     C*----------------------------------------------------*
054800000105     C*  RECUPERO DIVISA MONETA CORRENTE
054900000105     C                   CLEAR                   DGED
055000000105     C                   CLEAR                   DSBS02
055100000105     C                   MOVEL     'C'           T02MOD
055200000105     C                   MOVEL     KNSIF         T02SIF
055300000105     C                   MOVEL     'GED'         T02COD
055400000105     C                   MOVEL     '1'           T02KE1
055500000105     C                   CALL      'TIBS02R'
055600000105     C                   PARM                    KPJBA
055700000105     C                   PARM                    DSBS02
055800000105     C*
055900000105    3C     T02ERR        IFEQ      ' '
056000000105     C                   MOVEL     T02UNI        DGED
056100011001     C                   MOVEL     §GEDCN        DIVCOR
056200000105    3C                   ENDIF
056300000105     C*----------------------------------------------------*
056400941104     C*  Carico la schiera delle valute
056500950117     C                   MOVEL     'CV'          TBLCOD
056600941104     C                   Z-ADD     0             XX                3 0
056700941104     C     KTAB1         CHAIN     TABEL                              31
056800941104     C     *IN31         DOWEQ     '0'
056900941104     C     XX            ANDLT     100
057000941104     C     TBLFLG        IFEQ      ' '
057100941104     C                   ADD       1             XX
057200941104     C                   MOVEL     TBLKEY        CCV(XX)
057300941104     C                   MOVEL     TBLUNI        DSCV
057400000105     C                   MOVEL     §CVDVA        DVA(XX)
057500011001     C******               MOVEL§CVCMB    CAM,XX
057600000105     C     §CVDVA        IFEQ      'S'
057700011001     C     CCV(XX)       ANDNE     §GEDCN
057800000105     C                   MOVEL     CCV(XX)       DIVALT
057900000105     C                   END
058000000105     C* Memorizzo nr. decimali della divisa corrente
058100011001     C     CCV(XX)       IFEQ      §GEDCN
058200000105     C                   Z-ADD     §CVDEC        NRDEC             2 0
058300000105     C                   END
058400941104     C                   END
058500941104     C     KTAB1         READE     TABEL                                  31
058600941104     C                   END
058700950117     C*
058800950117     C*  Carico la schiera delle consegne anomale
058900950117     C                   MOVEL     '7O'          TBLCOD
059000950117     C                   Z-ADD     0             XX                3 0
059100950117     C     KTAB1         CHAIN     TABEL                              31
059200950117     C     *IN31         DOWEQ     '0'
059300950117     C     XX            ANDLT     100
059400950117     C     TBLFLG        IFEQ      ' '
059500950117     C                   ADD       1             XX
059600950117     C                   MOVEL     TBLKEY        C7O(XX)
059700950117     C                   MOVEL     TBLUNI        D7O(XX)
059800950117     C                   END
059900950117     C     KTAB1         READE     TABEL                                  31
060000950117     C                   END
060100931018     C*----------------------------------------------------*
060200931018     C                   MOVEL     'N'           £FINE             1
060300941104     C                   Z-ADD     SIMFEL        ORGFIL
060400941104     C     ORGFIL        CHAIN     AZORG01L                           78
060500931018     C  N78              MOVEL     ORGDES        FILIAL
060600980113     C*----------------------------------------------------*
060700980113     C* Loop AZORG x caricamento delle sole filiali estere
060800980113     C     *LOVAL        SETLL     AZORG01L
060900980113     C                   READ      AZORG01L                               32
061000980113     C                   Z-ADD     0             XY                3 0
061100980113     C     *IN32         DOWEQ     '0'
061200020729     C                   MOVEL     ORGDE3        OG143
061300020729     C                   MOVEL     *blank        flgest            1
061400020729     C     §OgNTW        ifeq      'FED'
061500020729     C     §OgNTW        oreq      'EEX'
061600020729     C     §OgNTW        oreq      'EUP'
061700020729     C                   MOVEL     'E'           flgest
061800020729     c                   end
061900020729     C***  ORGFL1        IFEQ      'E'
062000020729     C     flgest        IFEQ      'E'
062100980113     C                   ADD       1             XY
062200980113     C                   Z-ADD     ORGFIL        EST(XY)
062300980113     C                   MOVEL     ORGDES        DES(XY)
062400980113     C                   END
062500980113     C                   READ      AZORG01L                               32
062600980113     C                   END
062700980113     C*----------------------------------------------------*
062800980113     C*  Azzero schiera totali valuta se estero
062900980113     C     D24TPI        IFEQ      'E'
063000980113     C                   MOVEA     *BLANKS       LIV
063100980113     C                   DO        500           YY
063200980113     C     YY            OCCUR     MULT
063300980113     C                   CLEAR                   MULT
063400980113     C                   END
063500980113     C                   Z-ADD     0             YY
063600980113     C                   END
063700931018     C*----------------------------------------------------*
063800980520     C     D24AWB        COMP      '1'                                    23
063900941104     C     D24ORD        COMP      'P'                                020201
064000941104     C     D24SCP        COMP      '1'                                040403
064100931112     C*----------------------------------------------------*
064200931112     C*  Controllo se stampo totali filiali a rottura di livello
064300931112     C     *IN01         IFEQ      '1'
064400931112     C     *IN03         ANDEQ     '1'
064500941104     C     D24DET        ANDEQ     'SI'
064600931112     C     *IN02         OREQ      '1'
064700931112     C     *IN04         ANDEQ     '1'
064800941104     C     D24DET        ANDEQ     'SI'
064900931112     C                   SETON                                        05
065000931112     C                   END
065100931018     C*----------------------------------------------------*
065200931018     C*  Scrittura Prima pagina
065300941104     C                   CLEAR                   WLBDA8
065400931018     C                   MOVEL     NOMPG         NOMPGM
065500961217     C                   MOVE      D24TPI        S26TPI
065600961217     C                   MOVE      D24SCP        S26SL1
065700941104     C                   MOVE      D24SCA        S26SL2
065800941104     C                   MOVE      D24DET        S26DET
065900941104     C                   MOVE      D24ORD        S26ORD
066000941104     C     D24ORD        IFEQ      'P'
066100931018     C                   MOVEL     ORD1          S26DOR
066200931018     C                   ELSE
066300931018     C                   MOVEL     ORD2          S26DOR
066400931018     C                   END
066500941104     C                   MOVE      D24SDD        G02INV
066600941104     C                   Z-ADD     0             G02DAT
066700941104     C                   MOVEL     '3'           G02ERR
066800941104     C                   CALL      'XSRDA8'
066900941104     C                   PARM                    WLBDA8
067000941104     C                   MOVE      G02DAT        S26SDD
067100941104     C                   MOVE      D24SDA        G02INV
067200941104     C                   Z-ADD     0             G02DAT
067300941104     C                   MOVEL     '3'           G02ERR
067400941104     C                   CALL      'XSRDA8'
067500941104     C                   PARM                    WLBDA8
067600941104     C                   MOVE      G02DAT        S26SDA
067700941104     C                   MOVE      D24CDI        S26COD
067800941104     C                   MOVEL     D24RAG        S26RAG
067900941104     C                   MOVE      D24LIN        S26LIN
068000931018     C*
068100931018     C                   MOVEL     *ALL'-'       CMP198
068200931018     C                   MOVEL     *ALL'-'       CM1198
068300931115     C                   MOVEL     *ALL'-'       CM198
068400931018     C                   WRITE     PAG00
068500931018     C*
068600931018     C                   SETON                                        99
068700931018     C*----------------------------------------------------*
068800931018     C                   ENDSR
