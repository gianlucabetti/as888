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
002500040715     F****FNDCT02L  IF   E           K DISK
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
008000040715
008100040715     d Fidn12ds      e ds
008200040715     d  skKey                 26    305    dim(20)
008300040715     d  skAnn                306    325    dim(20)
008400040715     d  skDca                326    485    dim(20)
008500040715     d  skFca                486    545  0 dim(20)
008600040715     d  skDch                546    705  0 dim(20)
008700040715     d  skCch                706    745    dim(20)
008800040715
008900931018     D                SDS
009000931018     D  NOMPG                  1     10
009100931018     D*--------------------------------------------------------------*
009200931018     D*  COSTANTI                                                    *
009300931018     D*--------------------------------------------------------------*
009400931018     D ORD1            C                   CONST('LIN.DI PARTENZA')
009500931018     D ORD2            C                   CONST('LIN.DI ARRIVO  ')
009600950117     D NOT1            C                   CONST('DA INCA.')
009700950117     D NOT2            C                   CONST('GIACENTE ')
009800990205     D NOT3            C                   CONST('C.A. APE.')
009900931112?     *--------------------------------------------------------------*
010000931112?     *  ROTTURE di livello                                          *
010100931112?     *--------------------------------------------------------------*
010200941104     IFNCSWRK
010300931112     I                                          CSBKE1        L1
010400941104?     *--------------------------------------------------------------*
010500941104?     *  Ridefinizione interna campi azorg                           *
010600941104?     *--------------------------------------------------------------*
010700941104     IAZORG01L  AA
010800941104     I                             P    4    5 0ORGFIL
010900980113     I                                 11   11  ORGFL1
011000941104     I                                 14   33  ORGDES
011100020729     I                               3776 3800  ORGDE3
011200931018      *---------------------------------------------------------------*
011300931018      *   C I C L O      P R I N C I P A L E                          *
011400931018      *---------------------------------------------------------------*
011500931018     C*  Accendo indicatore se esiste almeno un record
011600931018     C  N98              SETON                                        98
011700931018     C*
011800931018     C*  Scrittura riga di Printer file
011900931018     C                   MOVE      CSBLNA        LNA
012000931018     C                   MOVE      CSBLNP        LNP
012100931112     C*  Controllo se devo stampare il dettaglio
012200941104     C     D24DET        IFEQ      'SI'
012300931112     C                   EXSR      WRTDET
012400931116     C                   END
012500980113     C*  Controllo se filiale estera
012600980113     C     CSBLNA        LOOKUP    EST                                    33
012700931018     C*  Calcolo totale x le filiali in gestione
012800961216     C     D24SCP        IFEQ      '1'
012900961216     C     D24ORD        OREQ      'P'
013000980113     C     *IN33         OREQ      '0'
013100941104     C                   EXSR      CALTOT
013200961216     C*  Se sto esaminando filiale estera calcolo totali valuta
013300961216     C                   ELSE
013400961216     C                   EXSR      CALTVA
013500961216     C                   END
013600931112     C*
013700931112     C*  Controllo se devo gestire stampa totali linea
013800961216     CL1 98D24TPI        IFEQ      'I'
013900961216     CL1 05              EXSR      TOTFIL
014000961216     CL1                 ELSE
014100961216     CL1                 EXSR      TOTFVA
014200961216     CL1                 END
014300931018     C*
014400931018     C*  Scrivo singole righe di dettaglio
014500961216     CLR 98D24TPI        IFEQ      'I'
014600961216     CLR                 EXSR      TOTGEN
014700961216     CLR                 END
014800931112      *---------------------------------------------------------------*
014900931112      *  Wrtdet: Scritture righe di dettaglio                         *
015000931112      *---------------------------------------------------------------*
015100931112     C     WRTDET        BEGSR
015200990205     C*
015300990205     C* GESTISCO FLAG PRATICA CA APERTA
015400990205     C                   MOVEL     *BLANKS       WCAAPE            1
015500990205     C*
015600931112     C                   MOVE      CSBAAS        AA
015700931112     C                   MOVE      CSBNRS        NRS
015800931112     C                   MOVE      CSBNSP        NSP
015900970625     C     D24TPI        IFEQ      'I'
016000970625     C     CSBDDP        OREQ      0
016100941104     C                   MOVEL     CSBAAS        G02INV
016200941104     C                   MOVE      CSBMGS        G02INV
016300970625     C                   ELSE
016400970625     C                   Z-ADD     CSBDDP        G02INV
016500970625     C                   END
016600941104     C                   Z-ADD     0             G02DAT
016700941104     C                   MOVEL     '3'           G02ERR
016800941104     C                   CALL      'XSRDA8'
016900941104     C                   PARM                    WLBDA8
017000941104     C                   MOVE      G02DAT        DSP
017100941104     C                   MOVEL     CSBRSD        RGD
017200941104     C                   MOVEL     CSBRSM        RGM
017300931112     C                   MOVE      CSBRMN        RMN
017400941104     C                   MOVE      CSBRMA        RMA
017500961217     C                   MOVE      CSBCAS        COMO3             3 0
017600950117     C     COMO3         IFEQ      *ZEROS
017700950117     C                   Z-ADD     CSBCAS        ASS1
017800950117     C                   SETON                                        06
017900950117     C                   ELSE
018000950117     C                   Z-ADD     CSBCAS        ASS2
018100950117     C                   SETOFF                                       06
018200950117     C                   END
018300941104     C                   MOVE      CSBVCA        VCA
018400961216     C     D24TPI        IFEQ      'I'
018500941104     C                   MOVE      CSBCMB        CMB
018600961216     C                   END
018700941104     C                   MOVE      CSBDGC        G02INV
018800941104     C                   Z-ADD     0             G02DAT
018900941104     C                   MOVEL     '3'           G02ERR
019000941104     C                   CALL      'XSRDA8'
019100941104     C                   PARM                    WLBDA8
019200941104     C                   MOVE      G02DAT        DGC
019300941104     C                   MOVE      CSBDCM        G02INV
019400941104     C                   Z-ADD     0             G02DAT
019500941104     C                   MOVEL     '3'           G02ERR
019600941104     C                   CALL      'XSRDA8'
019700941104     C                   PARM                    WLBDA8
019800941104     C                   MOVE      G02DAT        DCS
019900950117     C*  Controllo se devo eseguire decodifica consegna anomala
020000950117     C                   MOVEL     *BLANKS       WSTR
020100950117     C     CSBCCA        IFNE      *BLANKS
020200950117     C                   Z-ADD     1             XX
020300950117     C     CSBCCA        LOOKUP    C7O(XX)                                31
020400950117     C     *IN31         IFEQ      '1'
020500961216     C     D24TPI        ANDEQ     'I'
020600950117     C                   MOVEL     D7O(XX)       WDCA             18
020700950117     C                   MOVEL     C7O(XX)       WCCA              1
020800950117     C                   MOVEL     WCCA          WSTR             20
020900950117     C                   MOVE      WDCA          WSTR
021000950117     C                   END
021100950117     C                   END
021200931112     C*
021300990205     C*  Controllo se contrassegno solo da incassare, se non è ancora stata
021400990205     C*  effettuata la consegna, se esiste pratica C.A. aperta
021500961216     C     D24TPI        IFEQ      'I'
021600020507     C                   MOVE      *BLANKS       NOTa
021700931112     C     CSBDCM        IFNE      0
021800931112     C     *IN04         ANDEQ     '1'
021900020507     C                   MOVEL     NOT1          NOTa
022000931112     C                   END
022100931112     C*
022200931112     C     CSBDGC        IFNE      0
022300931222     C     CSBDCM        ANDEQ     0
022400020507     C                   MOVEL     NOT2          NOTa
022500931112     C                   END
022600990205     C* Eseguo chain su File dei danni per verificare che non esista
022700990205     C* una pratica C.A. aperta su quella bolla
022800040715      **
022900040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
023000040715      **   sui danni fra bolla madre e figlia
023100040715     C**                 MOVE      CSBAAS        KAAS
023200040715     C**                 MOVE      CSBLNP        KLNP
023300040715     C**                 MOVE      CSBNRS        KNRS
023400040715     C**                 MOVE      CSBNSP        KNSP
023500040715     C**   KDCT          CHAIN     FNDCT02L                           31
023600040715     C**   *IN31         DOWEQ     '0'
023700040715     C**   DCTATB        ANDEQ     'A'
023800040715     C**   *IN31         OREQ      '0'
023900040715     C**   DCTDCH        ANDNE     0
024000040715     C**   KDCT          READE     FNDCT02L                               31
024100040715     C**                 END
024200040715      *
024300040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
024400040715      * SIA COME MAMMA CHE COME FIGLIA
024500040715     c                   clear                   fidn12ds
024600040715     c                   eval      i12aas = CSBAAS
024700040715     c                   eval      i12lnp = CSBLNP
024800040715     c                   eval      i12nrs = CSBNRS
024900040715     c                   eval      i12nsp = CSBNSP
025000040715     c                   eval      i12fel = 'E'
025100040715     c                   eval      i12fan = 'E'
025200040715     c                   eval      i12fch = 'E'
025300040715     c                   eval      i12nca = 1
025400040715     c                   call      'FIDN12R'
025500040715     c                   parm                    fidn12ds
025600040715      **
025700040715      * se c'è almeno una C.A. aperta
025800040715      **
025900040715     c     o12nca        ifgt      0
026000040715     C**   *IN31         IFEQ      '0'
026100040715     C**   DCTATB        ANDNE     'A'
026200040715     C**   DCTDCH        ANDEQ     0
026300040715      **
026400990205     C                   MOVEL     'S'           WCAAPE            1
026500020507     C                   MOVEL     NOT3          NOTa
026600040715      **
026700040715     C                   MOVEL     skDca(1)      G02INV
026800040715     C*****              MOVEL     DCTAAC        G02INV
026900040715     C*****              MOVE      DCTMGC        G02INV
027000040715      **
027100990205     C                   Z-ADD     0             G02DAT
027200990205     C                   MOVEL     '3'           G02ERR
027300990205     C                   CALL      'XSRDA8'
027400990205     C                   PARM                    WLBDA8
027500990205     C                   MOVE      G02DAT        DGC
027600990205     C                   END
027700950117     C*
027800950117     C     WSTR          IFNE      *BLANKS
027900020507     C     NOTa          IFEQ      *BLANKS
028000020507     C                   MOVEL     WSTR          NOTa
028100950117     C                   ELSE
028200020507     C                   MOVE      WSTRG         NOTa
028300950117     C                   END
028400961216     C                   END
028500961216     C                   END
028600931112     C*  Test Overflow
028700961216     C     D24TPI        IFEQ      'I'
028800980217     C     *IN99         IFEQ      '1'
028900980217     C                   WRITE     TES00
029000980217     C                   SETOFF                                       99
029100980217     C                   END
029200980217     C                   WRITE     DET00
029300980217     C                   ELSE
029400980217     C     *IN99         IFEQ      '1'
029500980217     C                   WRITE     TESES
029600980217     C                   SETOFF                                       99
029700980217     C                   END
029800980217     C                   WRITE     DETES
029900931112     C                   END
030000931112      *
030100931112     C                   ENDSR
030200941104      *---------------------------------------------------------------*
030300941104      *  CALTOT: Calcolo totale  di dettaglio                         *
030400941104      *---------------------------------------------------------------*
030500941104     C     CALTOT        BEGSR
030600941104     C*
030700991018     C                   Z-ADD     1             X
030800000105     C     D24LIN        IFEQ      0
030900000105     C   03LNP           LOOKUP    LIN(X)                                 40
031000000105     C   04LNA           LOOKUP    LIN(X)                                 40
031100000105     C                   END
031200000105     C*  Sommo totali
031300000105     C                   ADD       1             NSC(X)
031400990205     C     WCAAPE        IFNE      'S'
031500941104     C*  Sommo totali
031600941104     C                   ADD       1             NMR(X)
031700941104     C* Controllo se il C/Assegno è in lire o in divisa
031800941104     C     VCA           IFNE      *BLANKS
031900941104     C                   Z-ADD     1             XX
032000941104     C     VCA           LOOKUP    CCV(XX)                                32
032100941104     C                   END
032200941104     C* Somma nel totale corretto
032300941104     C     VCA           IFEQ      *BLANKS
032400011001     C     VCA           OREQ      §GEDCN
032500941104     C                   ADD       CSBCAS        IPC(X)
032600000105     C                   ELSE
032700000105     C     DVA(XX)       IFEQ      'S'
032800000105     C     *IN32         ANDEQ     '1'
032900000105     C                   ADD       CSBCAS        IPE(X)
033000941104     C                   ELSE
033100010615     C***        CMB       IFEQ 0
033200010615     C***                  Z-ADDCAM,XX    CAMBIO  94
033300010615     C***                  ELSE
033400010615     C***                  Z-ADDCMB       CAMBIO
033500010615     C***                  END
033600010615     C***                  SELEC
033700010615     C***        NRDEC     WHEQ 2
033800010615     C***        CAMBIO    MULT CSBCAS    IMPV2  142H
033900010615     C***                  Z-ADDIMPV2     WIMPV
034000010615     C***        NRDEC     WHEQ 1
034100010615     C***        CAMBIO    MULT CSBCAS    IMPV1  131H
034200010615     C***                  Z-ADDIMPV1     WIMPV
034300010615     C***        NRDEC     WHEQ 0
034400010615     C***        CAMBIO    MULT CSBCAS    IMPV0  120H
034500010615     C***                  Z-ADDIMPV0     WIMPV
034600010615     C***                  OTHER
034700010615     C***        CAMBIO    MULT CSBCAS    WIMPV  153
034800010615     C***                  ENDSL
034900010615     C*
035000010615     C* Per convertire gli importi in valute estere utilizzo
035100010615     C* la routine YEURCO in quanto il cambio indicato sul
035200010615     C* file dei C/assegni di filiale non è affidabile
035300010615     C                   CLEAR                   YEURDS
035400010615     C                   MOVEL     VCA           YECDVI
035500010615     C                   Z-ADD     CSBCAS        YECIMI
035600011001     C                   MOVEL     §GEDCN        YECDVO
035700010615     C* passo al pgm io il nr. dei decimali della divisa corrente
035800010615     C* in base a quanto memorizzato sulla tabella CV
035900010615     C                   MOVE      NRDEC         YECDCO
036000010615     C                   MOVEL     'H'           YECTAR
036100010615     C                   CALL      'YEURCO'
036200010615     C                   PARM                    YEURDS
036300010615     C                   Z-ADD     YECIMO        WIMPV            15 3
036400010615     C                   ADD       WIMPV         IPV(X)
036500941104     C                   END
036600990205     C                   END
036700000105     C                   END
036800941104     C*
036900941104     C                   ENDSR
037000961216      *---------------------------------------------------------------*
037100961216      *  CALTVA: Calcolo totali per filiali/valuta                    *
037200961216      *---------------------------------------------------------------*
037300961216     C     CALTVA        BEGSR
037400961216     C*
037500990205     C     WCAAPE        IFNE      'S'
037600961216     C* Controllo se il C/Assegno è in lire o in divisa
037700961216     C     VCA           IFNE      *BLANKS
037800961216     C                   Z-ADD     1             XX
037900961216     C     VCA           LOOKUP    CCV(XX)                                32
038000961216     C                   END
038100961216     C*  Se no ho un c/ass. in valuta abblenco VCA
038200000105     C     VCA           IFNE      *BLANKS
038300011001     C     §GEDCN        ANDEQ     VCA
038400961216     C                   MOVE      *BLANKS       VCA
038500961216     C                   END
038600961216     C*
038700991018     C                   Z-ADD     1             XW                3 0
038800961216     C                   MOVEL     LNA           LINVAL
038900961216     C                   MOVE      VCA           LINVAL            6
039000991018     C     LINVAL        LOOKUP    LIV(XW)                                40
039100961216     C*  Sommo totali
039200961217     C     *IN40         IFEQ      '0'
039300961216     C                   ADD       1             YY
039400961216     C                   MOVEL     LNA           LIV(YY)
039500961216     C                   MOVE      VCA           LIV(YY)
039600991018     C                   Z-ADD     YY            XW
039700961216     C                   END
039800991018     C     XW            OCCUR     MULT
039900961216     C                   ADD       1             NMV
040000961216     C                   ADD       CSBCAS        IMV
040100990205     C                   END
040200961216     C*
040300961216     C                   ENDSR
040400931018      *---------------------------------------------------------------*
040500931018      *  Totgen: Stampa totale genrale + totali filiali in gestione   *
040600931018      *---------------------------------------------------------------*
040700931018     C     TOTGEN        BEGSR
040800931018     C*
040900931116     C                   WRITE     TES01
041000931116     C                   SETOFF                                       0599
041100931116     C*
041200020507     C                   DO        30            X                 2 0
041300931112     C                   EXSR      TOTFIL
041400931112     C                   END
041500931018     C*
041600931018     C*  Test Overflow
041700931018     C     *IN99         IFEQ      '1'
041800931018     C                   WRITE     TES00
041900931018     C                   SETOFF                                       99
042000931018     C                   END
042100931116     C*
042200931116     C                   XFOOT     NMR           TOT
042300931116     C                   XFOOT     IPC           TOTIMP
042400941104     C                   XFOOT     IPV           TOTIMV
042500000105     C                   XFOOT     IPE           TOTIME
042600011002     C                   MOVEL     §GEDCN        TOTVAV
042700931116     C*
042800931018     C                   WRITE     TOTGN
042900931112     C*
043000931018     C                   ENDSR
043100931112      *---------------------------------------------------------------*
043200931112      *  Totfil: Stampa totale filiale                                *
043300931112      *---------------------------------------------------------------*
043400931112     C     TOTFIL        BEGSR
043500931112     C*
043600931112     C     NMR(X)        IFNE      0
043700000105     C     NSC(X)        ORNE      0
043800931112     C                   MOVEL     *BLANKS       ORGDES
043900931112     C     LIN(X)        CHAIN     AZORG01L                           78
044000931112     C                   MOVE      LIN(X)        FIL
044100931112     C                   MOVE      ORGDES        DEC
044200931112     C                   MOVE      NMR(X)        NUM
044300931112     C                   MOVE      IPC(X)        IMP
044400941104     C                   MOVE      IPV(X)        IMPV
044500000105     C                   MOVE      IPE(X)        IMPE
044600011002     C                   MOVEL     §GEDCN        VALV
044700931112     C*  Test Overflow
044800931112     C     *IN99         IFEQ      '1'
044900931112     C                   WRITE     TES00
045000931112     C                   SETOFF                                       99
045100931112     C                   END
045200931112     C*
045300931112     C                   WRITE     TOTFL
045400931112     C                   END
045500931112     C*
045600931112     C                   ENDSR
045700961216      *---------------------------------------------------------------*
045800961216      *  Totfva: Stampa totale filiale/valuta                         *
045900961216      *---------------------------------------------------------------*
046000961216     C     TOTFVA        BEGSR
046100961216     C*
046200991018     C                   DO        YY            XW
046300991018     C     XW            OCCUR     MULT
046400961216     C     NMV           IFNE      0
046500991018     C                   MOVEL     LIV(XW)       WLIV              3 0
046600980113     C     WLIV          IFLT      0
046700980113     C                   MULT      -1            WLIV
046800980113     C                   END
046900961216     C                   Z-ADD     1             W                 3 0
047000980113     C     WLIV          LOOKUP    EST(W)                                 40
047100980113     C   40              MOVE      DES(W)        DEC
047200961216     C  N40              MOVE      *ALL'?'       DEC
047300961216     C                   MOVE      WLIV          FIL
047400961216     C                   MOVE      NMV           NUM
047500991018     C                   MOVE      LIV(XW)       VALES
047600961217     C                   MOVE      IMV           COMO3             3 0
047700961217     C     COMO3         IFEQ      *ZEROS
047800961217     C                   Z-ADD     IMV           IMPVE0
047900961217     C                   SETON                                        06
048000961217     C                   ELSE
048100961217     C                   Z-ADD     IMV           IMPVE
048200961217     C                   SETOFF                                       06
048300961217     C                   END
048400961216     C*  Test Overflow
048500961216     C     *IN99         IFEQ      '1'
048600961216     C                   WRITE     TESES
048700961216     C                   SETOFF                                       99
048800961216     C                   END
048900961216     C*
049000961216     C                   WRITE     TOTFE
049100961216     C                   END
049200961217     C                   END
049300961217     C*
049400970616     C*  Salto pagina per ogni partner
049500991018     C     *IN99         IFEQ      '0'
049600970618     C     *INLR         ANDEQ     '0'
049700970616     C                   WRITE     TESES
049800970616     C                   END
049900970616     C*
050000961217     C                   MOVEA     *BLANKS       LIV
050100961217     C                   DO        500           YY
050200961217     C     YY            OCCUR     MULT
050300961217     C                   CLEAR                   MULT
050400961217     C                   END
050500961217     C                   Z-ADD     0             YY                4 0
050600961216     C*
050700961216     C                   ENDSR
050800931018      *---------------------------------------------------------------*
050900931018      *  *INZSR: Operazioni iniziali                                  *
051000931018      *---------------------------------------------------------------*
051100931018     C     *INZSR        BEGSR
051200931018      *---------------------------------------------------------------*
051300931018     C     *ENTRY        PLIST
051400931018     C                   PARM                    KPJBA
051500941104     C                   MOVEL     KPJBU         TNSC24
051600011001     C*
051700011001     C     *LIKE         DEFINE    CSBCMB        CMB
051800931018     C*
051900931018     C                   Z-ADD     1             CODUT
052000931018     C                   CALL      'X§PARUT'
052100931018     C                   PARM                    UT§DSE
052200931018     C*          CODUT     CABEQ-1        FINE
052300931018     C                   MOVEL     RAGUT         RSUT             20
052400931018     C                   MOVEL     REC80         CNCR80
052500931018     C                   MOVEL     KNMUS         §SEI              6
052600931018     C*----------------------------------------------------*
052700931019     C*  Chiave x reperimento dati '£3'                    *
052800931018     C     KEYTAB        KLIST
052900931018     C                   KFLD                    CODUT
053000931018     C                   KFLD                    TBLCOD
053100931018     C                   KFLD                    TBLKEY
053200941104     C*
053300941104     C     KTAB1         KLIST
053400941104     C                   KFLD                    CODUT
053500941104     C                   KFLD                    TBLCOD
053600990205     C*
053700040715     C**   KDCT          KLIST
053800040715     C**                 KFLD                    KAAS
053900040715     C**                 KFLD                    KLNP
054000040715     C**                 KFLD                    KNRS
054100040715     C**                 KFLD                    KNSP
054200990205     C*----------------------------------------------------*
054300990205     C     *LIKE         DEFINE    CSBAAS        KAAS
054400990205     C     *LIKE         DEFINE    CSBLNP        KLNP
054500990205     C     *LIKE         DEFINE    CSBNRS        KNRS
054600990205     C     *LIKE         DEFINE    CSBNSP        KNSP
054700931018     C*----------------------------------------------------*
054800931018     C*  Reperimanto codici filiali in gestione sull'AS
054900020507     C                   OPEN      TABEL00F
055000020507     c                   clear                   trul06ds
055100020507     c                   eval      d06cod = '£1'
055200020507     c                   movel     simfel        d06key
055300020507     c                   movel     trul06ds      kpjbu
055400020507     c                   call      'TRUL06R'
055500020507     c                   parm                    kpjba
055600020507     c                   movel     kpjbu         trul06ds
055700020507     c                   movea     lint          lin
055800020507     C*    D24LIN        IFEQ      0
055900020507     C*                  MOVEL     '£3'          TBLCOD
056000020507     C*                  MOVEL     SIMFEL        TBLKEY
056100020507     C*    KEYTAB        CHAIN     TABEL                              21
056200020507     C*    *IN21         IFEQ      '0'
056300020507     C*                  MOVEA     TBLUNI        FIL1
056400020507     C*                  DO        30            X                 2 0
056500020507     C*    FIL1(X)       IFGT      *ZEROS
056600020507     C*                  ADD       1             Y
056700020507     C*                  MOVE      FIL1(X)       LIN(Y)
056800020507     C*                  END
056900020507     C*                  END
057000931018     C*
057100020507     C*                  ELSE
057200020507     C*                  MOVE      SIMFEL        LIN(1)
057300020507     C*                  END
057400931018     C*
057500020507     C*                  ELSE
057600020507     C*                  Z-ADD     1             Y
057700020507     C*                  MOVE      D24LIN        LIN(1)
057800020507     C*                  END
057900000105     C*----------------------------------------------------*
058000000105     C*  RECUPERO DIVISA MONETA CORRENTE
058100000105     C                   CLEAR                   DGED
058200000105     C                   CLEAR                   DSBS02
058300000105     C                   MOVEL     'C'           T02MOD
058400000105     C                   MOVEL     KNSIF         T02SIF
058500000105     C                   MOVEL     'GED'         T02COD
058600000105     C                   MOVEL     '1'           T02KE1
058700000105     C                   CALL      'TIBS02R'
058800000105     C                   PARM                    KPJBA
058900000105     C                   PARM                    DSBS02
059000000105     C*
059100000105    3C     T02ERR        IFEQ      ' '
059200000105     C                   MOVEL     T02UNI        DGED
059300011001     C                   MOVEL     §GEDCN        DIVCOR
059400000105    3C                   ENDIF
059500000105     C*----------------------------------------------------*
059600941104     C*  Carico la schiera delle valute
059700950117     C                   MOVEL     'CV'          TBLCOD
059800941104     C                   Z-ADD     0             XX                3 0
059900941104     C     KTAB1         CHAIN     TABEL                              31
060000941104     C     *IN31         DOWEQ     '0'
060100941104     C     XX            ANDLT     100
060200941104     C     TBLFLG        IFEQ      ' '
060300941104     C                   ADD       1             XX
060400941104     C                   MOVEL     TBLKEY        CCV(XX)
060500941104     C                   MOVEL     TBLUNI        DSCV
060600000105     C                   MOVEL     §CVDVA        DVA(XX)
060700011001     C******               MOVEL§CVCMB    CAM,XX
060800000105     C     §CVDVA        IFEQ      'S'
060900011001     C     CCV(XX)       ANDNE     §GEDCN
061000000105     C                   MOVEL     CCV(XX)       DIVALT
061100000105     C                   END
061200000105     C* Memorizzo nr. decimali della divisa corrente
061300011001     C     CCV(XX)       IFEQ      §GEDCN
061400000105     C                   Z-ADD     §CVDEC        NRDEC             2 0
061500000105     C                   END
061600941104     C                   END
061700941104     C     KTAB1         READE     TABEL                                  31
061800941104     C                   END
061900950117     C*
062000950117     C*  Carico la schiera delle consegne anomale
062100950117     C                   MOVEL     '7O'          TBLCOD
062200950117     C                   Z-ADD     0             XX                3 0
062300950117     C     KTAB1         CHAIN     TABEL                              31
062400950117     C     *IN31         DOWEQ     '0'
062500950117     C     XX            ANDLT     100
062600950117     C     TBLFLG        IFEQ      ' '
062700950117     C                   ADD       1             XX
062800950117     C                   MOVEL     TBLKEY        C7O(XX)
062900950117     C                   MOVEL     TBLUNI        D7O(XX)
063000950117     C                   END
063100950117     C     KTAB1         READE     TABEL                                  31
063200950117     C                   END
063300931018     C*----------------------------------------------------*
063400931018     C                   MOVEL     'N'           £FINE             1
063500941104     C                   Z-ADD     SIMFEL        ORGFIL
063600941104     C     ORGFIL        CHAIN     AZORG01L                           78
063700931018     C  N78              MOVEL     ORGDES        FILIAL
063800980113     C*----------------------------------------------------*
063900980113     C* Loop AZORG x caricamento delle sole filiali estere
064000980113     C     *LOVAL        SETLL     AZORG01L
064100980113     C                   READ      AZORG01L                               32
064200980113     C                   Z-ADD     0             XY                3 0
064300980113     C     *IN32         DOWEQ     '0'
064400020729     C                   MOVEL     ORGDE3        OG143
064500020729     C                   MOVEL     *blank        flgest            1
064600020729     C     §OgNTW        ifeq      'FED'
064700020729     C     §OgNTW        oreq      'EEX'
064800020729     C     §OgNTW        oreq      'EUP'
064900020729     C                   MOVEL     'E'           flgest
065000020729     c                   end
065100020729     C***  ORGFL1        IFEQ      'E'
065200020729     C     flgest        IFEQ      'E'
065300980113     C                   ADD       1             XY
065400980113     C                   Z-ADD     ORGFIL        EST(XY)
065500980113     C                   MOVEL     ORGDES        DES(XY)
065600980113     C                   END
065700980113     C                   READ      AZORG01L                               32
065800980113     C                   END
065900980113     C*----------------------------------------------------*
066000980113     C*  Azzero schiera totali valuta se estero
066100980113     C     D24TPI        IFEQ      'E'
066200980113     C                   MOVEA     *BLANKS       LIV
066300980113     C                   DO        500           YY
066400980113     C     YY            OCCUR     MULT
066500980113     C                   CLEAR                   MULT
066600980113     C                   END
066700980113     C                   Z-ADD     0             YY
066800980113     C                   END
066900931018     C*----------------------------------------------------*
067000980520     C     D24AWB        COMP      '1'                                    23
067100941104     C     D24ORD        COMP      'P'                                020201
067200941104     C     D24SCP        COMP      '1'                                040403
067300931112     C*----------------------------------------------------*
067400931112     C*  Controllo se stampo totali filiali a rottura di livello
067500931112     C     *IN01         IFEQ      '1'
067600931112     C     *IN03         ANDEQ     '1'
067700941104     C     D24DET        ANDEQ     'SI'
067800931112     C     *IN02         OREQ      '1'
067900931112     C     *IN04         ANDEQ     '1'
068000941104     C     D24DET        ANDEQ     'SI'
068100931112     C                   SETON                                        05
068200931112     C                   END
068300931018     C*----------------------------------------------------*
068400931018     C*  Scrittura Prima pagina
068500941104     C                   CLEAR                   WLBDA8
068600931018     C                   MOVEL     NOMPG         NOMPGM
068700961217     C                   MOVE      D24TPI        S26TPI
068701070702     C                   move      D24xCa        S26XCA
068800961217     C                   MOVE      D24SCP        S26SL1
068900941104     C                   MOVE      D24SCA        S26SL2
069000941104     C                   MOVE      D24DET        S26DET
069100941104     C                   MOVE      D24ORD        S26ORD
069200941104     C     D24ORD        IFEQ      'P'
069300931018     C                   MOVEL     ORD1          S26DOR
069400931018     C                   ELSE
069500931018     C                   MOVEL     ORD2          S26DOR
069600931018     C                   END
069700941104     C                   MOVE      D24SDD        G02INV
069800941104     C                   Z-ADD     0             G02DAT
069900941104     C                   MOVEL     '3'           G02ERR
070000941104     C                   CALL      'XSRDA8'
070100941104     C                   PARM                    WLBDA8
070200941104     C                   MOVE      G02DAT        S26SDD
070300941104     C                   MOVE      D24SDA        G02INV
070400941104     C                   Z-ADD     0             G02DAT
070500941104     C                   MOVEL     '3'           G02ERR
070600941104     C                   CALL      'XSRDA8'
070700941104     C                   PARM                    WLBDA8
070800941104     C                   MOVE      G02DAT        S26SDA
070900941104     C                   MOVE      D24CDI        S26COD
071000941104     C                   MOVEL     D24RAG        S26RAG
071100941104     C                   MOVE      D24LIN        S26LIN
071200931018     C*
071300931018     C                   MOVEL     *ALL'-'       CMP198
071400931018     C                   MOVEL     *ALL'-'       CM1198
071500931115     C                   MOVEL     *ALL'-'       CM198
071600931018     C                   WRITE     PAG00
071700931018     C*
071800931018     C                   SETON                                        99
071900931018     C*----------------------------------------------------*
072000931018     C                   ENDSR
