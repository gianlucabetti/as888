000100940307     H DECEDIT('0,') DATEDIT(*DMY.)
000200940307?     *--------------------------------------------------------------*
000300950203      * TNSC62R                                                      *
000400940307      *       *-----------------------------------------*            *
000500940307      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940307      *                  ESEGUE STAMPA DATI C/A                      *
000700940307      *       *-----------------------------------------*            *
000800940307      *                                                              *
000900940307?     *--------------------------------------------------------------*
001000120326     FTNcpm03l  IF   E           K DISK
001001120326?     *----------
001002120326     FTNCPB01L  IF   E           K DISK
001100940307?     *----------
001200950203     FTNCPV01L  IF   E           K DISK
001300940307?     *----------
001400940307     FTABEL00F  IF   E           K DISK    USROPN
001500940307?     *----------
001600950203     FTNSC62P   O    E             PRINTER OFLIND(*IN01)
001700940307?     *--------------------------------------------------------------*
001800940307?     *  SCHIERE                                                     *
001900940307?     *--------------------------------------------------------------*
002000940307     D CAU             S              4    DIM(20)
002100950215     D DES             S             18    DIM(20)
002200940307     D TPV             S              1    DIM(20)
002300940307     D STP             S              1    DIM(20)
002400940307?     *--------------------------------------------------------------*
002500940307?     *  DS                                                          *
002600940307?     *--------------------------------------------------------------*
002700940307     D KPJBA         E DS
002800940307      *
002900950203     D TNSC60        E DS                  EXTNAME(TNSC60DS)
003000940307      *
003100940307     D PARUT         E DS                  EXTNAME(UT§DSE0F)
003200940307     D CNCR80        E DS
003300940307      *
003400940307     D DS4V          E DS
003500940308     D*
003600950215     D WLBDAT          DS
003700950216     D  G02DAT                 1      6  0
003800950216     D  G02INV                 7     12  0
003900950216     D  G02ERR                13     13
004000950216     D  G02TGI                14     18  0
004100940308     D                SDS
004200940308     D  NOMPGM                 1     10
004300940307      *---------------------------------------------------------------*
004400940307      *  Costanti                                                     *
004500940307      *---------------------------------------------------------------*
004600940307     D COST1           C                   CONST('CONTANTE')
004700940310     D COST2           C                   CONST('ANNULLATO')
004800940307      *---------------------------------------------------------------*
004900940307      *   C I C L O      P R I N C I P A L E                          *
005000940307      *---------------------------------------------------------------*
005100940307      *  Posizionamento iniziale per data limite spedizione inferiore
005200950215     C     KEYCPB        SETLL     TNCPB01L
005300950215     C                   READ      TNCPB01L                               80
005400940307     C*
005500950215     C     *IN80         IFEQ      '0'
005600950215     C                   Z-ADD     CPBMGS        WDSP              8 0
005700950215     C                   MOVEL     CPBAAS        WDSP              8 0
005800000421     C                   Z-ADD     0             WAAMM
005900000421     C                   Z-ADD     0             WAAMMS
006000000822     C                   MOVEL     WDSP          WAAMM             8 0
006100000822     C                   MOVEL     WDSP          WAAMMS            8 0
006200940307     C* Loop di letture fino a fine file
006300950215     C     WDSP          DOWLE     D60SDA
006400940307     C     *IN80         ANDEQ     '0'
006500940307     C                   EXSR      STPBOL
006600950215     C                   READ      TNCPB01L                               80
006700950215     C     *IN80         IFEQ      '0'
006800950215     C                   Z-ADD     CPBMGS        WDSP              8 0
006900950215     C                   MOVEL     CPBAAS        WDSP              8 0
007000000421     C                   MOVEL     WDSP          WAAMM
007100000822     C* A rottura di GIORNO/mese/anno spedizione gestisco salto pagina
007200000421     C     WAAMM         IFNE      WAAMMS
007300000421     C                   SETON                                        01
007400000822     C                   Z-ADD     0             PAGE
007500000421     C                   MOVE      WAAMM         WAAMMS
007600940307     C                   END
007700000421     C                   END
007800950215     C*
007900950215     C                   END
008000950215     C                   END
008100940307     C* Fine pgm.
008200940307     C                   SETON                                        LR
008300940307      *---------------------------------------------------------------*
008400940307      *   STPBOL: Stampa dati bolla                                   *
008500940307      *---------------------------------------------------------------*
008600940307     C     STPBOL        BEGSR
008700940307     C*
008800950215     C                   Z-ADD     WDSP          G02INV
008900950215     C                   MOVEL     '3'           G02ERR
009000950215     C                   CALL      'XSRDAT'
009100950215     C                   PARM                    WLBDAT
009200950215     C                   MOVEL     G02DAT        DSP
009300940307     C                   Z-ADD     CPBLNP        LNP
009400940307     C                   Z-ADD     CPBNRS        NRS
009500940307     C                   Z-ADD     CPBNSP        NSP
009600940307     C                   Z-ADD     CPBLNA        LNA
009700940308     C                   Z-ADD     CPBRMN        RNM
009800940308     C                   MOVEL     CPBRSD        RGD
009900940310     C                   MOVEL     CPBRSM        RGM
010000940308     C                   MOVEL     CPBPAR        LOC
010100940308     C                   MOVEL     CPBPRO        PRV
010200950216     C                   MOVE      CPBCAS        COM               3 0
010300950215     C     COM           IFEQ      0
010400950215     C                   SETON                                        02
010500950215     C                   Z-ADD     CPBCAS        IMP1
010600950215     C                   ELSE
010700950215     C                   SETOFF                                       02
010800950215     C                   Z-ADD     CPBCAS        IMP2
010900950215     C                   END
011000950215     C                   MOVEL     CPBVCA        VCA
011600950215     C                   Z-ADD     CPBDDC        G02INV
011700950215     C                   MOVEL     '3'           G02ERR
011800950215     C                   CALL      'XSRDAT'
011900950215     C                   PARM                    WLBDAT
012000950215     C                   MOVEL     G02DAT        DDT
012100940307     C                   Z-ADD     CPBNDT        NDT
012300940516     C     CPBTPA        IFEQ      ' '
012400940310     C     DDT           ANDNE     *ZERO
012500940516     C                   MOVEL     *BLANKS       NDC
012600940516     C                   MOVEL     COST1         NDC
012700940307     C                   END
013000950215     C                   Z-ADD     CPBDDP        G02INV
013100950215     C                   MOVEL     '3'           G02ERR
013200950215     C                   CALL      'XSRDAT'
013300950215     C                   PARM                    WLBDAT
013400950215     C                   MOVEL     G02DAT        DDP
013500940308     C                   Z-ADD     CPBNDP        NDP
013600950215     C                   Z-ADD     CPBLAA        AASL
013700950215     C                   Z-ADD     CPBLLP        LNPL
013800950215     C                   Z-ADD     CPBLNS        NRSL
013900950215     C                   Z-ADD     CPBLSP        NSPL
014000940516     C                   Z-ADD     0             BAN
014100940516     C     CPBBNA        IFNE      9999999
014200940516     C     CPBBNA        ANDNE     *ZEROS
014300940516     C                   Z-ADD     CPBBNA        BAN
014400940516     C                   END
014500940516     C                   MOVEL     '   '         TPP
014600940307     C*  Tipo Documento
014700940307     C     CPBFPC        IFEQ      '2'
014800940307     C                   MOVE      'BON'         TPP
014900940307     C                   ELSE
015000940516     C     CPBBNA        IFEQ      9999999
015100940307     C                   MOVE      'LET'         TPP
015200940307     C                   ELSE
015300940307     C                   MOVE      'ASS'         TPP
015400940307     C                   END
015500940307     C                   END
015600940307     C*
015700940307     C* Controllo se è cambiato l'anno se si accendo indicatore
015800000824     C* di Salto pagina
015900940307     C     CPBAAS        IFNE      ANNSPE
016000000822     C                   Z-ADD     CPBAAS        ANNSPE            4 0
016100940307     C                   SETON                                        01
016200000824     C                   Z-ADD     0             PAGE
016300940307     C                   END
016400950215     C*
016500950215     C* Controllo se ci sono variazioni
016600950215     C                   Z-ADD     CPBAAS        CPWAAS
016700950215     C                   Z-ADD     CPBLNP        CPWLNP
016800950215     C                   Z-ADD     CPBNRS        CPWNRS
016900950215     C                   Z-ADD     CPBNSP        CPWNSP
017000950215     C     KEYCPV        CHAIN     TNCPV01L                           70
017100940307     C* Test Overflow
017200940307     C     *IN01         IFEQ      '1'
017300000822      *
017400000822      * INVERTO DATA SPEDIZIONE DA STAMPARE IN TESTATA
017500000822     C                   MOVE      CPBAAS        WDUE              2 0
017600000822     C                   MOVEL     WDUE          G02INV
017700000822     C                   MOVE      CPBMGS        G02INV
017800000822     C                   MOVEL     '3'           G02ERR
017900000822     C                   CALL      'XSRDAT'
018000000822     C                   PARM                    WLBDAT
018100000822     C                   MOVEL     G02DAT        DATSPE            6 0
018200940307     C                   WRITE     TES00
018300940307     C                   SETOFF                                       01
018400940307     C                   END
018500000822      *
018600950215     C     CPBSTA        COMP      9                                      99
018700950215     C                   WRITE     DET00
018800950215     C     CPBSTA        IFEQ      9
018900950215     C     NSPL          ANDNE     0
019000950215     C                   WRITE     LEGAN
019100950215     C                   END
019200940310     C* Scrivo i riferimenti di incassp se esistono
019201120326     C     DDT           IFNE      *ZERO
019202120326     c                   if        %subst(cpbnra: 10: 1)<> ' '
019203120326     c                             and cpbabi = 0
019204120326     c                   exsr      srmulti
019205120326     c                   else
019206120326     C                   Z-ADD     CPBDTE        G02INV
019207120326     C                   MOVEL     '3'           G02ERR
019208120326     C                   CALL      'XSRDAT'
019209120326     C                   PARM                    WLBDAT
019210120326     C                   MOVEL     G02DAT        DDC
019211120326     C                   MOVEL     CPBNRA        NDC
019212120326     C                   Z-ADD     CPBABI        ABI
019213120326     C                   Z-ADD     CPBCAB        CAB
019214120326     C                   WRITE     DETIN
019215120326     c                   end
019216120326     C     NSPL          IFNE      0
019217120326     C                   WRITE     LEGIN
019218120326     C                   END
019219120326     C                   END
019900940307     C*
020000940307     C* Controllo se ci sono variazioni da stampare
020100940307     C                   EXSR      STPVAR
020200940307     C*
020300940307     C                   ENDSR
020400940307      *---------------------------------------------------------------*
020500120326      *   Srmulti Stampa multiassegni                                 *
020600940307      *---------------------------------------------------------------*
020700120326     C     Srmulti       BEGSR
020701120326     c     cpbnra        setll     tncpm03l
020702120326     c                   do        *hival
020703120326     c     cpbnra        reade     tncpm03l
020704120326     c                   if        %eof(tncpm03l)
020705120326     c                   leave
020706120326     c                   end
020707120326     C     *IN01         IFEQ      '1'
020710120326     C                   MOVE      CPBAAS        WDUE              2 0
020711120326     C                   MOVEL     WDUE          G02INV
020712120326     C                   MOVE      CPBMGS        G02INV
020713120326     C                   MOVEL     '3'           G02ERR
020714120326     C                   CALL      'XSRDAT'
020715120326     C                   PARM                    WLBDAT
020716120326     C                   MOVEL     G02DAT        DATSPE            6 0
020717120326     C                   WRITE     TES00
020718120326     C                   SETOFF                                       01
020719120326     C                   END
020720120326     C                   Z-ADD     CpmDTE        G02INV
020721120326     C                   MOVEL     '3'           G02ERR
020722120326     C                   CALL      'XSRDAT'
020723120326     C                   PARM                    WLBDAT
020724120326     C                   MOVEL     G02DAT        DDC
020725120326     C                   MOVEL     CpmNRA        NDC
020726120326     C                   Z-ADD     CpmABI        ABI
020727120326     C                   Z-ADD     CpmCAB        CAB
020729120326     C                   WRITE     DETIN
020730120326     c                   enddo
020731120326     C                   ENDSR
020732120326      *---------------------------------------------------------------*
020733120326      *   STPVAR: Stampa dati variazione                              *
020734120326      *---------------------------------------------------------------*
020735120326     C     STPVAR        BEGSR
020800940307     C*
020900950215     C     *IN70         IFEQ      '0'
021000940307     C* Posizionamento iniziale
021100940310     C     *IN70         DOWEQ     '0'
021200940307     C                   SETOFF                                       070809
021300940310     C                   SETON                                        10
021400940310     C                   SETOFF                                       4011
021500940307     C* Cerco decodifica da stampare
021600940516     C                   Z-ADD     1             X                 3 0
021700940308     C     CPVCAV        LOOKUP    CAU(X)                                 40
021800940310     C     *IN40         IFEQ      '1'
021900940307     C     STP(X)        ANDEQ     'S'
022000940307     C                   MOVEL     DES(X)        DESV
022100950216     C                   Z-ADD     CPVDTV        G02INV
022200950215     C                   MOVEL     '3'           G02ERR
022300950215     C                   CALL      'XSRDAT'
022400950215     C                   PARM                    WLBDAT
022500950215     C                   MOVEL     G02DAT        DTV
022600940307     C                   Z-ADD     CPVFEV        LNV
022700940307     C*  Annullamento !!!
022800940516     C*
022900940308     C     TPV(X)        IFEQ      'A'
023000940516     C     TPV(X)        OREQ      'V'
023100950215     C     CPVCAS        ANDEQ     CPBCAS
023200950215     C     CPVVCA        ANDEQ     CPBVCA
023300940516     C     CPBSTA        ANDEQ     9
023400940516     C     TPV(X)        IFEQ      'V'
023500940516     C                   MOVEL     *BLANKS       DESV
023600940516     C                   MOVEL     'ANNULLAM'    DES16            16
023700940516     C                   MOVE      'ENTO C/A'    DES16            16
023800940516     C                   MOVEL     DES16         DESV
023900940516     C                   END
024000940516     C*
024100940310     C     AASL          IFNE      0
024200940310     C     LNPL          ORNE      0
024300940310     C     NRSL          ORNE      0
024400940310     C     NSPL          ORNE      0
024500940310     C                   SETON                                        11
024600940310     C                   END
024700940310     C                   SETON                                        09
024800940307     C                   ELSE
024900940307     C*  Apertura giacenza !!!
025000940308     C     TPV(X)        IFEQ      'G'
025100940307     C                   SETON                                        08
025200940314     C                   Z-ADD     CPBNGC        NGC
025300940307     C*  Variazione C/A
025400940307     C                   ELSE
025500940308     C     TPV(X)        IFEQ      'V'
025600940307     C                   SETON                                        07
025700950216     C                   MOVE      CPVCAS        COM
025800950215     C     COM           IFEQ      0
025900950215     C                   SETON                                        02
026000950215     C                   Z-ADD     CPVCAS        PIMP1
026100950215     C                   ELSE
026200950215     C                   SETOFF                                       02
026300950215     C                   Z-ADD     CPVCAS        PIMP2
026400950215     C                   END
026500940307     C                   END
026600940307     C                   END
026700940307     C                   END
026800940307     C*
026900940516     C*  se leggo varizione importo C/A e l'importo è uguale
027000940516     C*  a quello della bolla non stampo nulla
027100940516     C     TPV(X)        IFNE      'V'
027200950215     C     CPVCAS        ORNE      CPBCAS
027300950215     C     CPVVCA        ORNE      CPBVCA
027400940516     C     CPBSTA        OREQ      9
027500940516     C*
027600940516     C     *IN01         IFEQ      '1'
027700940307     C                   WRITE     TES00
027800940307     C                   SETOFF                                       01
027900940307     C                   END
028000940307     C                   WRITE     DETEV
028100940307     C                   END
028200940516     C*
028300940516     C                   END
028400940307     C*  Lettura successivo
028500950215     C     KEYCPV        READE     TNCPV01L                               70
028600940307     C                   END
028700940310     C*  Se contrassegno annullato stampo dati annullam.
028800940310     C                   ELSE
028900940310     C                   SETOFF                                       1011
029000940310     C                   SETOFF                                       070809
029100940310     C     CPBSTA        IFEQ      9
029200940310     C                   MOVEL     COST2         DESV
029300940310     C     *IN01         IFEQ      '1'
029400940310     C                   WRITE     TES00
029500940310     C                   SETOFF                                       01
029600940310     C                   END
029700940310     C                   WRITE     DETEV
029800940310     C                   END
029900940310     C                   END
030000940307     C*
030100940307     C                   ENDSR
030200940307      *---------------------------------------------------------------*
030300940307      *   *INZSR: Operazioni iniziali                                 *
030400940307      *---------------------------------------------------------------*
030500940307     C     *INZSR        BEGSR
030600940307     C*
030700940307     C     *ENTRY        PLIST
030800940307     C                   PARM                    KPJBA
030900950215     C                   MOVEL     KPJBU         TNSC60
031000940307     C*----------------------------------------------------*
031100940307     C* Call a XPARUT
031200940307     C                   Z-ADD     1             CODUT
031300940307     C                   CALL      'X§PARUT'
031400940307     C                   PARM                    PARUT
031500940307     C*
031600940307     C                   MOVEL     RAGUT         RSUT             20
031700940307     C                   MOVEL     REC80         CNCR80
031800940307     C                   MOVEL     KNMUS         §SEI              6
031900940307     C*----------------------------------------------------*
032000940307     C* Definizione chiavi
032100940307     C     KEYCPB        KLIST
032200950215     C                   KFLD                    CPWAAS
032300950215     C                   KFLD                    CPWMGS
032400940307     C*
032500940307     C     KEYCPV        KLIST
032600940307     C                   KFLD                    CPWLNP
032700940307     C                   KFLD                    CPWAAS
032800940307     C                   KFLD                    CPWNRS
032900940307     C                   KFLD                    CPWNSP
033000940307     C*
033100940307     C     KEYTAB        KLIST
033200940307     C                   KFLD                    TBWKUT
033300940308     C                   KFLD                    TBWCOD
033400940307     C*----------------------------------------------------*
033500940307     C* Definizione chiavi
033600950215     C     *LIKE         DEFINE    CPBMGS        CPWMGS
033700940307     C     *LIKE         DEFINE    CPBAAS        CPWAAS
033800940307     C     *LIKE         DEFINE    CPBLNP        CPWLNP
033900940307     C     *LIKE         DEFINE    CPBNRS        CPWNRS
034000940307     C     *LIKE         DEFINE    CPBNSP        CPWNSP
034100940307     C     *LIKE         DEFINE    TBLKUT        TBWKUT
034200940308     C     *LIKE         DEFINE    TBLCOD        TBWCOD
034300940307     C*----------------------------------------------------*
034400940307     C* Inizializzazione variabili
034500950215     C                   Z-ADD     D60SDD        CPWMGS
034600950215     C                   MOVEL     D60SDD        CPWAAS
034700940307     C                   Z-ADD     1             TBWKUT
034800940307     C*----------------------------------------------------*
034900940307     C* Caricamento dati tabella 4V
035000940308     C                   MOVE      '4V'          TBWCOD
035100940307     C                   Z-ADD     0             X
035200940307     C                   OPEN      TABEL00F
035300940307     C     KEYTAB        CHAIN     TABEL00F                           81
035400940307     C     *IN81         DOWEQ     '0'
035500940307     C     X             ANDLT     20
035600940307     C                   MOVEL     TBLUNI        DS4V
035700940307     C                   ADD       1             X
035800940308     C                   MOVEL     TBLKEY        CAU(X)
035900940307     C                   MOVEL     §4VDES        DES(X)
036000940307     C                   MOVEL     §4VTIP        TPV(X)
036100940307     C                   MOVEL     §4VSTP        STP(X)
036200940307     C     KEYTAB        READE     TABEL00F                               81
036300940307     C                   END
036400940307     C                   CLOSE     TABEL00F
036500940307     C*----------------------------------------------------*
036600940308     C* Stampo date limiti spedizioni
036700950215     C                   Z-ADD     D60SDD        G02INV
036800950215     C                   MOVEL     '3'           G02ERR
036900950215     C                   CALL      'XSRDAT'
037000950215     C                   PARM                    WLBDAT
037100950215     C                   MOVEL     G02DAT        DATSP1
037200950215     C                   Z-ADD     D60SDA        G02INV
037300950215     C                   MOVEL     '3'           G02ERR
037400950215     C                   CALL      'XSRDAT'
037500950215     C                   PARM                    WLBDAT
037600950215     C                   MOVEL     G02DAT        DATSP2
037700940308     C*
037800950215     C                   Z-ADD     DATSP1        S62DT1
037900950215     C                   Z-ADD     DATSP2        S62DT2
038000950215     C                   MOVEL     D60PUL        S62PUL
038100950215     C                   MOVEL     D60STA        S62STA
038200000822     C  N01
038300000822     CAN 01              WRITE     PAG00
038400940308     C*----------------------------------------------------*
038500940308     C                   ENDSR
038600940307     C*----------------------------------------------------*
