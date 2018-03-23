000100930203     H DECEDIT('0,') DATEDIT(*DMY.)
000200941018     H* FNLG74R *----------------------------------------------------*
000300930203     H*                                                              *
000400930203     H*   - STAMPA ELENCO GIACENZE CHIUSE PER CLIENTE -   -FILTRO-   *
000500930203     H*                                                              *
000600930203     H*--------------------------------------------------------------*
000700941018     FFNLG74D   CF   E             WORKSTN
000800930203     FCNACO00F  IF   E           K DISK
000900970109     FTABEL00F  IF   E           K DISK
001000010528     FAZORG01L  IF   E           K DISK
001100941018     D*--------------------------------------------------------------*
001200970109     D FL£7            S              3    DIM(29)
001300020502     D*FL£1            S              3    DIM(29)
001400020502     D £1              S              3  0 DIM(30)
001500970109     D £7              S              3  0 DIM(29)
001600970109     D ERR             S             70    DIM(6) CTDATA PERRCD(1)
001700941018     D*--------------------------------------------------------------*
001800941018     D FNLG74        E DS                  EXTNAME(FNLG74DS)
001900930203     D TCUDS           DS
002000930203     D  F3                     3      3
002100930203     D  F4                     4      4
002200930203     D  F56                    5      6
002300930203     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002400930203     D  TCU                  398    697
002500930203     D                                     DIM(50)
002600930203     D  KCU                  698    847P 0
002700930203     D                                     DIM(50)
002800930203     D                                     PACKEVEN
002900930203     D CNCR80        E DS
003000930203     D CNCR42        E DS
003100930203     D KPJBA         E DS
003200020930     d savkpjbu        s                   like(kpjbu)
003300020502     d trul06ds      e ds
003400020502     d  lin                    1     90  0 dim(30)
003500941018     D WLBDA8          DS
003600941018     D  G02DAT                 1      8  0
003700941018     D  G02INV                 9     16  0
003800941018     D  G02ERR                17     17
003900941018     D  G02TGI                18     22  0
004000020927      *parametri per reperimento coda
004100020927     d finep           s              1
004200020927     d codpo           s              3s 0
004300020927     d tpcoda          s              1
004400020927     d nomcoda         s             10
004500020927     d libcoda         s             10
004600020927     d esito           s              1
004700930203     C*---------------------------------------------------------------*
004800970109     C*  Inizializzazione videata
004900970109     C                   EXSR      INZ01
005000970109     C*  Loop videate
005100920522     C     *INKC         DOWEQ     '0'
005200970109     C                   EXSR      GESD01
005300970109     C                   END
005400970109     C*
005500920522     C                   SETON                                        LR
005600970109     C*------------------------------------------------------------------*
005700970109     C*  Gestione Videata
005800970109     C*------------------------------------------------------------------*
005900970109     C     GESD01        BEGSR
006000970109     C*
006100970109     C                   EXFMT     LG74D01
006200970109     C*  Azzero campi e indicatori di errore
006300970109     C                   MOVEL     *BLANKS       $MSG
006400970109     C                   SETOFF                                       404142
006500970109     C                   SETOFF                                       432802
006600970109     C*
006700970109     C   KC              GOTO      FINVD1
006800970109     C*
006900970109     C*   SE RICHIESTO CAMBIO FILIALE VADO A FINE VIDEATA
007000970109     C     *INKS         IFEQ      '1'
007100970109     C                   SETON                                        1802
007200970109     C                   GOTO      FINVD1
007300970109     C                   END
007400970109     C*   Se ho modificatao la filiale controllo nuovo valore
007500020730     C     SimPOU        IFNE      V1CFIL
007600970109     C                   EXSR      CTRFIL
007700970109     C                   END
007800970109     C* Controlli
007900970109     C                   EXSR      CTR01
008000970109     C*  SE NON CI SONO ERRORI SOTTOMETTO LA STAMPA
008100970109     C     *IN28         IFEQ      '0'
008200970109     C     *IN99         ANDEQ     '0'
008300970109     C     *INKF         ANDEQ     '1'
008400970109     C                   Z-ADD     V1CFIL        D74FIL
008500970109     C                   MOVE      VCLI          D74COD
008600970109     C                   MOVEL     VRIS          D74RIS
008700020927     C                   MOVE      nomcoda       D74PRF
008800050321     c                   move      'S'           d74comm
008900041029     C                   MOVEL     FNLG74        KPJBU
009000080219     C*mm  VTIP          IFEQ      'R'
009100080219     C*mm                MOVEL     'LG75'        KCOAZ
009200080219     C*mm                ELSE
009300080219     C*mm                MOVEL     'LG17'        KCOAZ
009400080219     C*mm                END
009500080219     c*mm                if        knmus = *all'1'
009600041029     C     VTIP          ifeq      'R'
009700041029     C                   call      'FNLG75C'
009800041029     C                   parm                    kpjba
009900041029     c                   else
010000041029     C                   call      'FNLG17C'
010100041029     C                   parm                    kpjba
010200041029     c                   end
010300080219     c*mm                else
010400080219     C*mm                CALL      'BCH10'
010500080219     C*mm                PARM                    KPJBA
010600080219     c*mm                end
010700041029     C                   MOVEL     *BLANKS       VCLI
010800041029     C                   MOVEL     *BLANKS       DESCLI
010900060112     C                   clear                   fnlg74
011000970109     C                   END
011100970109     C*
011200970109     C     FINVD1        ENDSR
011300970109     C*------------------------------------------------------------------*
011400970109     C*  Controlli
011500970109     C*------------------------------------------------------------------*
011600970109     C     CTR01         BEGSR
011700970109     C*
011800970109     C* CONTROLLO CODICE CLIENTE
011900970109     C     '?'           SCAN      VCLI                                   21
012000970109     C   21              DO
012100970109     C                   SETON                                        99
012200970109     C                   Z-ADD     CODUT         CODUT§            1 0
012300970109     C                   Z-ADD     KCI           KCI§              4 0
012400970109     C                   MOVEL     *BLANKS       RICRAG           48
012500970109     C                   MOVEL     DESCLI        RICRAG
012600970109     C                   Z-ADD     0             RICKSC            7 0
012700970109     C                   Z-ADD     0             PARSTA            1 0
012800970109     C                   CALL      'XALFA1'
012900970109     C                   PARM                    PARDUT
013000970109     C                   PARM                    CODUT§
013100970109     C                   PARM                    RICRAG
013200970109     C                   PARM                    KCI§
013300970109     C                   PARM                    RICKSC
013400970109     C                   PARM                    PARSTA
013500970109     C                   PARM                    PARFLR           90
013600970109     C     PARSTA        IFNE      -1
013700970109     C                   MOVE      RICKSC        VCLI
013800970109     C                   MOVEL     RICRAG        DESCLI
013900970109     C                   ELSE
014000970109     C                   MOVE      *ZEROS        VCLI
014100970109     C                   MOVE      *BLANKS       DESCLI
014200970109     C                   END
014300970109     C                   END
014400970109     C                   MOVE      VCLI          CODCLI
014500970109     C     KACO          CHAIN     CNACO000                           11
014600970109     C     *IN11         IFEQ      '1'
014700970109     C                   SETON                                        2843
014800970109     C                   MOVEL     ERR(6)        $MSG
014900970109     C                   MOVEL     *BLANKS       DESCLI
015000970109     C                   ELSE
015100970109     C                   MOVEL     ACORAG        DESCLI
015200970109     C                   END
015300970109     C* CONTROLLO DATA DAL
015400970109     C  N28VTIP          IFNE      'D'
015500970109     C     VTIP          OREQ      'D'
015600970109     C     VRIS          ANDEQ     'R'
015700970109     C                   Z-ADD     VDAL          G02DAT
015800970109     C                   Z-ADD     0             G02INV
015900970109     C                   MOVEL     *BLANKS       G02ERR
016000970109     C                   CALL      'XSRDA8'
016100970109     C                   PARM                    WLBDA8
016200970109     C     G02ERR        IFEQ      '1'
016300970109     C                   SETON                                        2841
016400970109     C                   MOVEL     ERR(2)        $MSG
016500970109     C                   ELSE
016600970109     C                   Z-ADD     G02INV        D74DT1
016700970109     C                   Z-ADD     G02DAT        VDAL
016800970109     C                   END
016900970109     C                   END
017000970109     C* CONTROLLO DATA AL
017100970109     C  N28VTIP          IFNE      'D'
017200970109     C     VTIP          OREQ      'D'
017300970109     C     VRIS          ANDEQ     'R'
017400970109     C                   Z-ADD     VAL           G02DAT
017500970109     C                   Z-ADD     0             G02INV
017600970109     C                   MOVEL     *BLANKS       G02ERR
017700970109     C                   CALL      'XSRDA8'
017800970109     C                   PARM                    WLBDA8
017900970109     C     G02ERR        IFEQ      '1'
018000970109     C                   SETON                                        2842
018100970109     C                   MOVEL     ERR(2)        $MSG
018200970109     C                   ELSE
018300970109     C                   Z-ADD     G02INV        D74DT2
018400970109     C                   Z-ADD     G02DAT        VAL
018500970109     C                   END
018600970109     C                   END
018700970109     C* CONTROLLO CHE DAL SIA MINORE DI AL
018800970109     C  N28VTIP          IFNE      'D'
018900970109     C     VTIP          OREQ      'D'
019000970109     C     VRIS          ANDEQ     'R'
019100970109     C     D74DT1        IFGT      D74DT2
019200970109     C                   SETON                                        284142
019300970109     C                   MOVEL     ERR(3)        $MSG
019400970109     C                   END
019500970109     C                   END
019600970109     C* CONTROLLO CHE AL NON SIA MAGGIORE DELLA DATA DEL GIORNO
019700970109     C  N28VTIP          IFNE      'D'
019800970109     C     VTIP          OREQ      'D'
019900970109     C     VRIS          ANDEQ     'R'
020000970109     C     D74DT2        IFGT      DATEU
020100970109     C                   SETON                                        4228
020200970109     C                   MOVEL     ERR(5)        $MSG
020300970109     C                   END
020400970109     C                   END
020500970109     C* CONTROLLO CHE DAL/AL SIA A 0 SE RICHIESTA LA STAMPA A
020600970109     C* DETTAGLIO
020700970109     C  N28VTIP          IFEQ      'D'
020800970109     C     VRIS          ANDNE     'R'
020900970109     C     VDAL          IFNE      0
021000970109     C     VAL           ORNE      0
021100970109     C                   SETON                                        284142
021200970109     C                   MOVEL     ERR(4)        $MSG
021300970109     C                   END
021400970109     C                   END
021500970109     C*
021600970109     C  N28VTIP          IFNE      'D'
021700970109     C     VTIP          OREQ      'D'
021800970109     C     VRIS          ANDEQ     'R'
021900970109     C     D74DT1        IFGT      D74DT2
022000970109     C                   SETON                                        284142
022100970109     C                   MOVEL     ERR(3)        $MSG
022200970109     C                   END
022300970109     C                   END
022400970109     C*
022500970109     C                   ENDSR
022600970109     C*------------------------------------------------------------------*
022700970109     C*  INIZIALIZZAZIONE VIDEATA
022800970109     C*------------------------------------------------------------------*
022900970109     C     INZ01         BEGSR
023000970109     C*
023100970109     C                   Z-ADD     0             VDAL
023200970109     C                   Z-ADD     0             VAL
023300970109     C                   MOVEL     *BLANKS       VCLI
023400970109     C                   MOVEL     'D'           VTIP
023500970109     C                   MOVEL     *BLANKS       DESCLI
023600970109     C                   MOVEL     *BLANKS       VRIS
023700970109     C* Reperisco fil da elaborare
023800020730     C                   MOVEL     SimPOU        V1CFIL
023900970109     C*
024000970109     C* INVERTO DATA DEL GIORNO
024100970109     C                   CLEAR                   WLBDA8
024200970109     C                   TIME                    WHHDAT           14 0
024300970109     C                   MOVE      WHHDAT        G02DAT
024400970109     C                   CALL      'XSRDA8'
024500970109     C                   PARM                    WLBDA8
024600970109     C                   MOVE      G02INV        DATEU             8 0
024700970109     C*  Azzero campi e indicatori di errore
024800970109     C                   MOVEL     *BLANKS       $MSG
024900970109     C                   SETOFF                                       404142
025000970109     C                   SETOFF                                       432802
025100970109     C*  Caricamento tabella £7
025200970109     C                   EXSR      CAR£7
025300970109     C*
025400970109     C                   ENDSR
025500970109     C*----------------------------------------------------------*
025600970109     C* Controllo filiale
025700970109     C*----------------------------------------------------------*
025800970109     C     CTRFIL        BEGSR
025900970109     C*
026000970109     C     V1CFIL        LOOKUP    £1                                     32
026100970109     C     *IN32         IFEQ      '0'
026200970109     C                   SETON                                        2840
026300970109     C                   MOVEL     ERR(1)        $MSG
026400970109     C                   ELSE
026500970109     C                   EXSR      CAR£7
026600970109     C                   END
026700970109     C*
026800970109     C                   ENDSR
026900970109     C*----------------------------------------------------------*
027000970109     C* Caricamento tabella £7
027100970109     C*----------------------------------------------------------*
027200970109     C     CAR£7         BEGSR
027300970109     C* Caricamento £7
027400970109     C                   MOVEA     *ZEROS        FL£7
027500970109     C                   MOVEL     *BLANKS       TBLKEY
027600970109     C                   MOVEL     V1CFIL        TBLKEY
027700970109     C                   MOVEL     '£7'          TBLCOD
027800970109     C     KTAB          CHAIN     TABEL                              31
027900970109     C     *IN31         IFEQ      '0'
028000970109     C     TBLFLG        ANDEQ     ' '
028100970109     C                   MOVEA     TBLUNI        FL£7
028200970109     C                   ELSE
028300970109     C                   MOVE      V1CFIL        FL£7(1)
028400970109     C                   END
028500970109     C*
028600970109     C                   Z-ADD     0             £7
028700970109     C                   Z-ADD     0             X£7               2 0
028800970109     C                   DO        29            X                 3 0
028900970109     C     FL£7(X)       IFNE      *BLANKS
029000970109     C                   ADD       1             X£7               2 0
029100970109     C                   MOVE      FL£7(X)       £7(X£7)
029200970109     C                   END
029300970109     C                   END
029400970109     C*
029500970109     C                   ENDSR
029600941018     C*------------------------------------------------------------------*
029700941018     C*  OPERAZIONI INIZIALI
029800941018     C*------------------------------------------------------------------*
029900941018     C     *INZSR        BEGSR
030000941018     C*
030100941018     C     *ENTRY        PLIST
030200941018     C                   PARM                    KPJBA
030300941018     C                   Z-ADD     1             CODUT
030400941018     C                   CALL      'X§PARUT'
030500941018     C                   PARM                    UTEDSE
030600941018     C*          CODUT     CABEQ-1        FINE
030700941018     C                   MOVEL     RAGUT         RSUT             20
030800941018     C*--- RICERCA CAPOCONTI------------------------------------------*
030900970109     C                   DO        50            X
031000941018     C                   MOVE      TCU(X)        TCUDS
031100941018     C     F56           IFEQ      'CG'
031200941018     C     F4            ANDEQ     '1'
031300941018     C     F3            ANDEQ     '0'
031400941018     C                   Z-ADD     KCU(X)        KCI               4 0
031500941018     C                   END
031600941018     C                   END
031700941018     C*---------------------------------------------------------------*
031800941018     C                   MOVEL     REC80         CNCR80
031900941018     C                   MOVEL     REC42         CNCR42
032000941018     C                   MOVEL     *BLANKS       RAGSOC
032100941018     C                   MOVEL     SIMRGS        RAGSOC           30
032200941018     C                   MOVEL     RAGUT         PARDUT           30
032300941018     C* CNACO
032400941018     C     KACO          KLIST
032500941018     C                   KFLD                    CODUT
032600941018     C                   KFLD                    KCI
032700941018     C                   KFLD                    CODCLI            7 0
032800970109     C     KTAB          KLIST
032900970109     C                   KFLD                    CODUT
033000970109     C                   KFLD                    KCOD
033100970109     C                   KFLD                    KKEY
033200970109     C*  Definisco variabili
033300970109     C     *LIKE         DEFINE    TBLCOD        KCOD
033400970109     C     *LIKE         DEFINE    TBLKEY        KKEY
033500970109     C*
033600060327     C                   clear                   fnlg74
033700970109     C*  Controllo qual'è la filiale con cui sto lavorando
033800020730     C     SimTpP        COMP      '2'                                0101
033900970109     C*
034000020927      *richiamo programma per impostare coda Fax
034100020927     c                   move      'L'           finep
034200020927     c                   z-add     simpou        codpo
034300020927     c                   move      '2'           tpcoda
034400020927     c                   clear                   nomcoda
034500020927     c                   clear                   libcoda
034600020927     c                   clear                   esito
034700020927     c                   call      'TRULOUTQ'
034800020927     c                   parm                    finep
034900020927     c                   parm                    codpo
035000020927     c                   parm                    tpcoda
035100020927     c                   parm                    nomcoda
035200020927     c                   parm                    libcoda
035300020927     c                   parm                    esito
035400020927     c                   if        esito <> '0' or nomcoda = *blank
035500020927     c                   eval      nomcoda = 'QDKT'
035600020927     c                   endif
035700020927     c
035800970109     C*  Carico £1
035900970109     C                   Z-ADD     0             Y                 2 0
036000020502     c                   clear                   trul06ds
036100020502     c                   eval      d06cod = '£1'
036200020730     c                   movel     SimPOU        d06key
036300020930     c                   movel     kpjbu         savkpjbu
036400020502     c                   movel     trul06ds      kpjbu
036500020502     c                   call      'TRUL06R'
036600020502     c                   parm                    kpjba
036700020502     c                   movel     kpjbu         trul06ds
036800020502     c                   movea     lin           £1
036900020930     c                   movel     savkpjbu      kpjbu
037000020502     C*                  MOVEL     *BLANKS       KKEY
037100020502     C*                  MOVEL     '£1'          KCOD
037200020502     C*                  MOVEL     WFIL          KKEY
037300020502     C*    KTAB          CHAIN     TABEL                              31
037400020502     C*    *IN31         IFEQ      '0'
037500020502     C*    TBLFLG        ANDEQ     ' '
037600020502     C*                  MOVEA     TBLUNI        FL£1
037700020502     C*                  DO        29            X
037800020502     C*    FL£1(X)       IFNE      *BLANKS
037900020502     C*                  ADD       1             Y
038000020502     C*                  MOVE      FL£1(X)       £1(Y)
038100020502     C*                  END
038200020502     C*                  END
038300020502     C*                  ELSE
038400020502     C*                  MOVE      WFIL          £1(1)
038500020502     C*                  END
038600970109     C*
038700941018     C*----------------------------------------------------*
038800970109     C                   ENDSR
038900970109**
039000970109Punto operativo non gestito su questo AS                              01
039100970109Data errata                                                           02
039200970109Data iniziale maggiore data finale                                    03
039300970109Immettere dal al solo se richiesta la ristampa o la stp per riepilogo 04
039400970109La data finale non può essere superiore alla data del giorno          05
039500970109Codice cleinte errato                                                 06
