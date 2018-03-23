000100950927     H DECEDIT('0,') DATEDIT(*DMY.)
000200950927?     *--------------------------------------------------------------*
000300130315      * FNLG52R                                                      *
000400950927      *     *----------------------------------------------*         *
000500950927      *         STATISTICA GIACENZE APERTE PER CLIENTE               *
000600950927      *                      UFFICIO GIACENZE                        *
000700950927      *     *---------------------------------------------*          *
000800950927      *                                                              *
000900950927?     *--------------------------------------------------------------*
001000950927      * KC - FINE LAVORO                                             *
001100950927      * KF - CONFERMA                                                *
001200950927?     *--------------------------------------------------------------*
001300950928      * 28 - Visualizzazione messaggio di errore                     *
001400950928      * 30 - Indicatore per chain generiche                          *
001500070703      * 31 - Call TRUL53C                                            *
001600950928      * 40 - Reverse Immage su data spedizione da                    *
001700950928      * 41 - Reverse Immage su data spedizione a                     *
001800950928      * 42 - Reverse Immage su codice cliente                        *
001900950928?     *--------------------------------------------------------------*
002000950927     FCNACO00F  IF   E           K DISK
002100950927     F*--------
002200070706     FTABEL00F  IF   E           K DISK
002300960209     F*--------
002400960209     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002500950927     F*--------
002600070703     FFNLG52D   CF   E             WORKSTN
002700070703     F                                     SFILE(LG52S01:NRR)
002800950927     D*---------------------------------------------------------------*
002900950927     D* SCHIERA
003000950927     D*---------------------------------------------------------------*
003100020502     D*UNI             S              3    DIM(29)
003200020502     D £1              S              3  0 DIM(30)
003300960209     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
003400950928     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
003500950927?     *--------------------------------------------------------------*
003600950927?     *  DS                                                          *
003700950927?     *--------------------------------------------------------------*
003800950927     D KPJBA         E DS
003900950927     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004000950927     D  TCU                  398    697
004100950928     D                                     DIM(50)
004200950927     D  KCU                  698    847P 0
004300950928     D                                     DIM(50)
004400950928     D                                     PACKEVEN
004500950927     D TCUDS           DS
004600950927     D  F34                    3      4
004700950927     D  F56                    5      6
004800950927     D CNCR80        E DS
004900950927      *
005000070703     D FNLG52ds      E DS
005100951002     D  KSC                   17    156  0
005200951002     D                                     DIM(20)
005300020502     d trul06ds      e ds
005400020502     d  lin                    1     90  0 dim(30)
005500010726     D*?*DS5A2     E DS
005600010726     D DS5A          E DS
005700070706     D DS$3          E DS
005800950928     D WLBDA8          DS
005900950928     D  G02DAT                 1      8  0
006000950928     D  G02INV                 9     16  0
006100950928     D  G02ERR                17     17
006200950928     D  G02TGI                18     22  0
006300950929     D WGIDAT          DS
006400950929     D  GIODAT                 1      8  0
006500950929     D  GIOINV                 9     16  0
006600950929     D  GIOTGI                17     21  0
006700950927     D                SDS
006800950927     D  NOMPGM                 1     10
006900960209?     *--------------------------------------------------------------*
007000960209?     *  Organigramma di filiale                                     *
007100960209?     *--------------------------------------------------------------*
007200960209     IAZORG01L  AA
007300960209     I                             P    4    5 0ORGFIL
007400960209     I                                 14   33  ORGDES
007500950927?     *--------------------------------------------------------------*
007600950927?     *  CICLO PRINCIPALE                                            *
007700950927?     *--------------------------------------------------------------*
007800950927      *  Impostazione parametri prima videata
007900950927     C                   EXSR      INZ01
008000950927      *  Loop gestione videata
008100950927     C     WFINE         DOUEQ     'S'
008200950927     C                   EXSR      GESD01
008300950927     C                   END
008400950927      *  Fine Lavoro
008500950927     C     FINE          TAG
008600950927     C                   SETON                                        LR
008700950927?     *--------------------------------------------------------------*
008800950927?     *  INZ01: Inizializzazione prima videata                       *
008900950927?     *--------------------------------------------------------------*
009000950927     C     INZ01         BEGSR
009100950927      *
009200951005     C                   Z-ADD     0             NRR
009300950928     C                   SETOFF                                       404142
009400951002     C                   SETOFF                                       28
009500070703     C                   CLEAR                   FNLG52ds
009600950928     C                   MOVEL     '1'           WTPVID            1
009700950928     C                   Z-ADD     0             V1CDSD
009800950928     C                   Z-ADD     0             V1CDSA
009900951002      *  Pulisco subfile
010000951002     C                   Z-ADD     1             DSPNR1
010100951002     C                   SETOFF                                       2021
010200070703     C                   WRITE     LG52C01
010300951002     C                   SETON                                        2021
010400951002      *  Carico una pagina di subfile
010500951002     C                   EXSR      ROLLUP
010600950928     C*
010700950928     C                   ENDSR
010800951002?     *--------------------------------------------------------------*
010900951002?     *  ROLLUP: Carico una nuova pagina di subfile                  *
011000951002?     *--------------------------------------------------------------*
011100951002     C     ROLLUP        BEGSR
011200951002      *
011300951002     C                   DO        16
011400951002     C                   SETOFF                                       42
011500951002     C                   ADD       1             NRR               4 0
011600951002     C                   MOVEL     *BLANKS       V1DKSC
011700951002     C                   Z-ADD     0             V1CKSC
011800070703     C                   WRITE     LG52S01
011900951002     C                   END
012000951002     C                   Z-ADD     NRR           WSVNRR            4 0
012100951002     C   25              Z-ADD     NRR           DSPNR1            4 0
012200951002      *
012300951002     C                   ENDSR
012400950928?     *--------------------------------------------------------------*
012500950928?     *  GESD01: Gestione prima videata                              *
012600950928?     *--------------------------------------------------------------*
012700950928     C     GESD01        BEGSR
012800950928      *
012900070703     C                   WRITE     LG52Z01
013000070703     C                   EXFMT     LG52C01
013100951002     C                   Z-ADD     1             DSPNR1            4 0
013200950928     C                   SETOFF                                       404142
013300950928     C                   SETOFF                                       4328
013400950928      *  Fine Lavoro
013500950928     C     *INKC         IFEQ      '1'
013600950928     C                   MOVEL     'S'           WFINE
013700950928     C                   GOTO      FINVD1
013800950928     C                   END
013900951002      *  ROLLUP
014000951002     C     *IN25         IFEQ      '1'
014100951002     C                   Z-ADD     WSVNRR        NRR
014200951002     C                   EXSR      ROLLUP
014300951002     C                   GOTO      FINVD1
014400951002     C                   END
014500950928      *  Controlli
014600950928     C                   EXSR      CTR01
014700950928     C   28              GOTO      FINVD1
014800070703      *  Conferma: Sbm. FNLG53R
014900951002     C     *INKF         IFEQ      '1'
015000070703     C                   EXSR      SBMP53
015100950928     C                   EXSR      INZ01
015200950928     C                   END
015300950928      *
015400950928     C     FINVD1        ENDSR
015500950928?     *--------------------------------------------------------------*
015600950928?     *  CTR01: Controlli prima videata                              *
015700950928?     *--------------------------------------------------------------*
015800950928     C     CTR01         BEGSR
015900950928      *
016000950928      *  Controllo data spedizione da
016100950928     C     V1CDSD        IFEQ      0
016200950928     C                   SETON                                        4028
016300950928     C                   MOVEL     ERR(1)        $MSG
016400950928     C                   GOTO      FINCT1
016500950928     C                   ELSE
016600950928     C                   Z-ADD     V1CDSD        G02DAT
016700950928     C                   Z-ADD     0             G02INV
016800950928     C                   MOVEL     *BLANKS       G02ERR
016900950928     C                   CALL      'XSRDA8'
017000950928     C                   PARM                    WLBDA8
017100950928     C     G02ERR        IFEQ      '1'
017200950928     C                   SETON                                        4028
017300950928     C                   MOVEL     ERR(2)        $MSG
017400950928     C                   GOTO      FINCT1
017500950928     C                   ELSE
017600950928     C                   Z-ADD     G02INV        WDATDA
017700950928     C                   Z-ADD     G02TGI        WGIODA            5 0
017800950928     C                   Z-ADD     G02DAT        V1CDSD
017900950928     C                   END
018000950928     C                   END
018100950928     C     WDATDA        IFGT      WOGGI
018200950928     C                   SETON                                        4028
018300950928     C                   MOVEL     ERR(3)        $MSG
018400950928     C                   GOTO      FINCT1
018500950928     C                   END
018600950928      *  Controllo data spedizione a
018700950928     C     V1CDSA        IFEQ      0
018800950928     C                   SETON                                        4128
018900950928     C                   MOVEL     ERR(1)        $MSG
019000950928     C                   GOTO      FINCT1
019100950928     C                   ELSE
019200950928     C                   Z-ADD     V1CDSA        G02DAT
019300950928     C                   Z-ADD     0             G02INV
019400950928     C                   MOVEL     *BLANKS       G02ERR
019500950928     C                   CALL      'XSRDA8'
019600950928     C                   PARM                    WLBDA8
019700950928     C     G02ERR        IFEQ      '1'
019800950928     C                   SETON                                        4128
019900950928     C                   MOVEL     ERR(2)        $MSG
020000950928     C                   GOTO      FINCT1
020100950928     C                   ELSE
020200950928     C                   Z-ADD     G02INV        WDATAA
020300950928     C                   Z-ADD     G02TGI        WGIOA             5 0
020400950928     C                   Z-ADD     G02DAT        V1CDSA
020500950928     C                   END
020600950928     C                   END
020700950928     C     WDATAA        IFGT      WOGGI
020800950928     C                   SETON                                        4128
020900950928     C                   MOVEL     ERR(3)        $MSG
021000950928     C                   GOTO      FINCT1
021100950928     C                   END
021200950928      *  Controllo limiti fra due date
021300950928     C     WDATDA        IFGT      WDATAA
021400950928     C                   SETON                                        404128
021500950928     C                   MOVEL     ERR(4)        $MSG
021600950928     C                   GOTO      FINCT1
021700950928     C                   END
021800950928      *  Controllo differenza gg. <= 1 mese
021801110623     c*mm                if        %subst(knmus:1:3)<> 'EDP'
021900110622     C                   MOVEL     WDATDA        WANNO             4 0
022000110622     C                   MOVE      WDATDA        WMMGG             4 0
022100110622     C                   MOVEL     WMMGG         WM                2 0
022200110622     C     WANNO         DIV       4             WCOMO             4 0
022300110622     C                   MVR                     WRESTO            2 0
022400110622     C                   Z-ADD     28            XDTF(2)
022500110622     C     WRESTO        IFEQ      0
022600110622     C                   Z-ADD     29            XDTF(2)
022700110622     C                   END
022800110622     C     WGIOA         SUB       WGIODA        WGGDIF            5 0
022900110622     C                   ADD       1             WGGDIF
022901110623     C     WGGDIF        IFGT      190
023000110623     C*mm  WGGDIF        IFGT      XDTF(WM)
023100110622     C                   SETON                                        404128
023200110622     C                   MOVEL     ERR(5)        $MSG
023300110622     C                   GOTO      FINCT1
023400110622     C                   END
023401110623     c*mm                endif
023500110622     C                   MOVEL     '$3'          KCOD
023600070706     C                   MOVEL     '1       '    KKEY
023700070706     c     ktab          chain     tabel00f
023800070706     c                   if        %found(tabel00f)
023900070706     c                   movel     tbluni        ds$3
024000070706     C     WDATDA        IFLT      §$3dp0
024100070706     C                   SETON                                        4028
024200070706     C                   MOVEL     ERR(7)        $MSG
024300070706     C                   GOTO      FINCT1
024400070706     C                   END
024500070706     c                   else
024600070706     C                   SETON                                        4028
024700070706     C                   MOVEL     ERR(8)        $MSG
024800070706     C                   GOTO      FINCT1
024900070706     c                   endif
025000951002      *  Lettura subfile
025100070703     C                   READC     LG52S01                                29
025200951002     C                   Z-ADD     0             WNRCLI            3 0
025300960209     C                   Z-ADD     0             WNRLNP            3 0
025400951002     C                   Z-ADD     0             KSC
025500951002     C                   Z-ADD     0             X                 3 0
025600951002     C     *IN29         DOWEQ     '0'
025700951002     C                   SETOFF                                       42
025800960209      *  Controllo se immesso cliente o lnp
025900960209     C                   MOVEL     V1CKSC        WTEST             4 0
026000960209     C     WTEST         IFNE      0
026100960209     C     V1DKSC        ORNE      *BLANKS
026200960209     C     V1CKSC        ANDEQ     0
026300960209     C                   EXSR      CTRCLI
026400960209     C                   ELSE
026500960209     C                   EXSR      CTRLNP
026600960209     C                   END
026700951002     C                   SETON                                        22
026800070703     C                   UPDATE    LG52S01
026900951002     C                   SETOFF                                       22
027000070703     C                   READC     LG52S01                                29
027100951002     C                   END
027200951002     C   28              GOTO      FINCT1
027300951002      *  Controllo che sia stato selez. almeno un cliente
027400951002     C     WNRCLI        IFEQ      0
027500960209     C     WNRLNP        ANDEQ     0
027600951002     C                   SETON                                        28
027700951002     C                   MOVEL     ERR(9)        $MSG
027800951002     C                   END
027900951002      *  Do errore se sono stati selez. più di 20 codici
028000951002     C     WNRCLI        IFGT      20
028100951002     C                   SETON                                        28
028200951002     C                   MOVEL     ERR(10)       $MSG
028300951002     C                   END
028400960209      *  Do errore se sono stati selez. più di 20 LNP
028500960209     C     WNRLNP        IFGT      20
028600960209     C                   SETON                                        28
028700960209     C                   MOVEL     ERR(14)       $MSG
028800960209     C                   END
028900950928     C*
029000950928     C     FINCT1        ENDSR
029100960209?     *--------------------------------------------------------------*
029200960209?     *  CTRCLI: Controllo dati cliente                              *
029300960209?     *--------------------------------------------------------------*
029400960209     C     CTRCLI        BEGSR
029500960209      *
029600960209      *  Controllo se ho già immesso delle lnp.
029700960209     C     WNRLNP        IFNE      0
029800960209     C  N28              Z-ADD     NRR           DSPNR1
029900960209     C  N28              MOVEL     ERR(13)       $MSG
030000960209     C                   SETON                                        4228
030100960209     C                   END
030200960209      *  Controllo se richiesta ricerca alfabetica cliente
030300960209     C     V1CKSC        IFEQ      0
030400960209     C     V1DKSC        ANDNE     *BLANKS
030500960209     C                   MOVEL     V1DKSC        ACORAG
030600960209     C                   Z-ADD     CLIITA        ACOKCC
030700960209     C                   Z-ADD     0             ACOKSC
030800960209     C                   CALL      'XALFA1'
030900960209     C                   PARM                    PARDUT
031000960209     C                   PARM                    PARKUT
031100960209     C                   PARM                    ACORAG
031200960209     C                   PARM                    ACOKCC
031300960209     C                   PARM                    ACOKSC
031400960209     C                   PARM                    PARSTA            1 0
031500961016     C                   PARM                    PARFLR           90
031600960209     C     PARSTA        IFEQ      0
031700960209     C                   Z-ADD     ACOKSC        V1CKSC
031800960209     C                   END
031900960209     C                   END
032000960209      *  Controllo validità conto cliente
032100960209     C                   MOVEL     *BLANKS       V1DKSC
032200960209     C     V1CKSC        IFNE      0
032300960209     C     V1CKSC        LOOKUP    KSC                                    30
032400960209     C     *IN30         IFEQ      '1'
032500960209     C  N28              Z-ADD     NRR           DSPNR1
032600960209     C  N28              MOVEL     ERR(11)       $MSG
032700960209     C                   SETON                                        4228
032800960209     C                   ELSE
032900960209     C                   ADD       1             WNRCLI
033000960209     C     WNRCLI        IFLE      20
033100960209     C                   ADD       1             X
033200960209     C                   Z-ADD     V1CKSC        KSC(X)
033300960209     C                   END
033400960209     C                   Z-ADD     1             KKUT
033500960209     C                   Z-ADD     CLIITA        KKCC
033600960209     C                   Z-ADD     V1CKSC        KKSC
033700960209     C     KACO          CHAIN     CNACO00F                           30
033800960209     C     *IN30         IFNE      '0'
033900960209     C     ACOFLG        ORNE      *BLANKS
034200960209     C  N28              Z-ADD     NRR           DSPNR1
034300960209     C  N28              MOVEL     ERR(6)        $MSG
034400960209     C                   SETON                                        4228
034500960209     C                   END
034600960209     C                   MOVEL     ACORAG        V1DKSC
034700960209     C                   END
034800960209     C                   END
034900960209     C*
035000960209     C                   ENDSR
035100960209?     *--------------------------------------------------------------*
035200960209?     *  CTRLNP: Controllo dati linea di partenza                    *
035300960209?     *--------------------------------------------------------------*
035400960209     C     CTRLNP        BEGSR
035500960209      *
035600960209      *  Controllo se ho già immesso delle lnp.
035700960209     C     WNRCLI        IFNE      0
035800960209     C  N28              Z-ADD     NRR           DSPNR1
035900960209     C  N28              MOVEL     ERR(12)       $MSG
036000960209     C                   SETON                                        4228
036100960209     C                   END
036200960209      *  Controllo validità liea di partenza
036300960209     C                   MOVEL     *BLANKS       V1DKSC
036400960209     C     V1CKSC        IFNE      0
036500960209     C     V1CKSC        LOOKUP    KSC                                    30
036600960209     C     *IN30         IFEQ      '1'
036700960209     C  N28              Z-ADD     NRR           DSPNR1
036800960209     C  N28              MOVEL     ERR(15)       $MSG
036900960209     C                   SETON                                        4228
037000960209     C                   ELSE
037100960209     C                   ADD       1             WNRLNP
037200960209     C     WNRLNP        IFLE      20
037300960209     C                   ADD       1             X
037400960209     C                   Z-ADD     V1CKSC        KSC(X)
037500960209     C                   END
037600960209     C                   Z-ADD     V1CKSC        ORGFIL
037700960209     C     ORGFIL        CHAIN     AZORG01L                           30
037800960209     C     *IN30         IFNE      '0'
037900960209     C  N28              Z-ADD     NRR           DSPNR1
038000960209     C  N28              MOVEL     ERR(16)       $MSG
038100960209     C                   SETON                                        4228
038200960209     C                   END
038300960209     C                   MOVEL     ORGDES        V1DKSC
038400960212     C     ORGFIL        LOOKUP    £1                                     30
038500960212     C     *IN30         IFEQ      '0'
038600960209     C  N28              Z-ADD     NRR           DSPNR1
038700960209     C  N28              MOVEL     ERR(17)       $MSG
038800960209     C                   SETON                                        4228
038900960209     C                   END
039000960209     C                   END
039100960209     C                   END
039200960209     C*
039300960209     C                   ENDSR
039400950928?     *--------------------------------------------------------------*
039500070703?     *  SBMP53: Sottometto stampa statistica                        *
039600950928?     *--------------------------------------------------------------*
039700070703     C     SBMP53        BEGSR
039800950928      *
039900960209     C     WNRCLI        IFEQ      0
040000070703     C                   MOVEL     'L'           D52MOD
040100960209     C                   ELSE
040200070703     C                   MOVEL     'C'           D52MOD
040300960209     C                   END
040400070703     C                   Z-ADD     WDATDA        D52DSD
040500070703     C                   Z-ADD     WDATAA        D52DSA
040600070703     C                   MOVEL     FNLG52ds      KPJBU
040700070703     C                   MOVEL     'LG53'        KCOAZ
040800070703     C*    KNMUS         IFNE      *ALL'1'
040900070703     C*                  CALL      'BCH10'
041000070703     C*                  PARM                    KPJBA
041100070703     C*                  ELSE
041200070703     C                   CALL      'FNLG53R1'
041300950928     C                   PARM                    KPJBA
041400070703     C*                  END
041500950928     C*
041600950928     C                   ENDSR
041700950928?     *--------------------------------------------------------------*
041800950928?     *  *INZSR: Operazioni iniziali                                 *
041900950928?     *--------------------------------------------------------------*
042000950928     C     *INZSR        BEGSR
042100950928     C*
042200950928     C     *ENTRY        PLIST
042300950928     C                   PARM                    KPJBA
042400950928     C* Richiamo XPARUT
042500950928     C                   Z-ADD     1             CODUT
042600950928     C                   CALL      'X§PARUT'
042700950928     C                   PARM                    UTEDSE
042800960209     C                   MOVEL     REC80         CNCR80
042900950928     C*--- Ricerca Capoconti -----------------------------------------*
043000950928     C                   DO        50            X
043100950928     C                   MOVE      TCU(X)        TCUDS
043200950928     C     F56           IFEQ      'CG'
043300950928     C     F34           ANDEQ     '01'
043400950928     C                   Z-ADD     KCU(X)        CLIITA            4 0
043500950928     C                   END
043600950928     C                   END
043700950928     C*  KLIST
043800960209     C     KTAB          KLIST
043900950928     C                   KFLD                    KKUT
044000950928     C                   KFLD                    KCOD
044100950928     C                   KFLD                    KKEY
044200950928     C*
044300950928     C     KACO          KLIST
044400950928     C                   KFLD                    KKUT
044500950928     C                   KFLD                    KKCC
044600950928     C                   KFLD                    KKSC
044700950928     C*  Definizione delle variabili
044800950928     C     *LIKE         DEFINE    ACOKUT        KKUT
044900950928     C     *LIKE         DEFINE    ACOKCC        KKCC
045000950928     C     *LIKE         DEFINE    ACOKSC        KKSC
045100950928     C     *LIKE         DEFINE    TBLCOD        KCOD
045200950928     C     *LIKE         DEFINE    TBLKEY        KKEY
045300950928     C     *LIKE         DEFINE    V1CDSD        WDATDA
045400950928     C     *LIKE         DEFINE    V1CDSA        WDATAA
045500950928     C*  Inizializzazione
045600950928     C                   MOVEL     'N'           WFINE             1
045700950928     C                   MOVEL     REC80         CNCR80
045800950928     C                   MOVEL     RAGUT         PARDUT           30
045900950928     C                   MOVEL     RAGUT         RSUT
046000950928     C                   Z-ADD     CODUT         PARKUT            1 0
046100950928     C                   TIME                    WHHDAT           14 0
046200950928     C                   MOVE      WHHDAT        G02DAT
046300950928     C                   Z-ADD     0             G02INV
046400950928     C                   MOVEL     *BLANKS       G02ERR
046500950928     C                   CALL      'XSRDA8'
046600950928     C                   PARM                    WLBDA8
046700950928     C                   Z-ADD     G02INV        WOGGI             8 0
046800950929     C                   Z-ADD     G02TGI        WTODAY            5 0
046900960209      * Reperisco filiali su AS
047000020730     C     SimTpP        IFNE      '2'
047100960209     C                   Z-ADD     1             KKUT
047200020502     c                   clear                   trul06ds
047300020502     c                   eval      d06cod = '£1'
047400020730     c                   movel     SimPOU        d06key
047500020502     c                   movel     trul06ds      kpjbu
047600020502     c                   call      'TRUL06R'
047700020502     c                   parm                    kpjba
047800020502     c                   movel     kpjbu         trul06ds
047900020502     c                   movea     lin           £1
048000960212     C                   ELSE
048100020730     C                   MOVE      SimPOU        £1(1)
048200960212     C                   END
048300960209      * Lettura giorni di pulizia
048400080403     C                   Z-ADD     1             KKUT
048500970718     C                   MOVEL     '5A'          KCOD
048600040213     C*********          MOVEL     '2       '    KKEY
048700040213     C                   MOVEL     '1       '    KKEY
048800960209     C     KTAB          CHAIN     TABEL00F                           30
048900010726     C  N30              MOVEL     TBLUNI        DS5A
049000010726     C   30              CLEAR                   DS5A
049100950928      *
049200950928     C                   ENDSR
049300950928**
049400950928Data spedizione obbligatoria                                          01
049500951002Data spedizione formalmente errata                                    02
049600950928Data spedizione maggiore data del giorno                              03
049700950928Limiti incompatibili: data spedizione iniziale maggiore finale        04
049800110623RANGE di date superiore a sei mesi                                    05
049900950928Codice cliente inesistente o annullato                                06
050000951002Attenzione: data spedizione precedente all'ultima data pulizia bolle  07
050100951002Data pulizia bolle non reperibile i dati potrebbero essere imprecisi  08
050200960209Indicare almeno un codice cliente o una linea di partenza             09
050300951002Impossibile selezionare più di 20 codici cliente                      10
050400951002Codice cliente già immesso                                            11
050500960209Sono già stati immessi codici clienti impossibile lavorare per lnp    12
050600960209Sono già stati immesse lnp impossibile lavorare per codici clienti    13
050700960209Impossibile selezionare più di 20 linee di partenza                   14
050800960209Linea di partenza già immessa                                         15
050900960209Linea di partenza inesitente                                          16
051000960212Linea di partenza non gestibile                                       17
051100950928**
051200950928312831303130313130313031
