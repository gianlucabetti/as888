000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200000908
000300000523      *****************************************************************
000400000908      *                                                               *
000500000908      *    Utility   CANCELLAZIONE BOLLE PARTENZA                     *
000600000908      *                                                               *
000700000908      *****************************************************************
000800000908
000900000908     FTABEL00F  IF   E           K DISK
001000000919     FAZORG01L  IF   E           K DISK
001100000908     FCNACO00F  IF   E           K DISK
001200030220     FFNBLP70L  IF   E           K DISK
001300000920     FFNBLP01L  IF   E           K DISK    RENAME(FNBLP000:FNBLP1)
001400090304     FFIART01L  IF   E           K DISK
001500090304     FFIART27L  IF   E           K DISK    RENAME(FIART000:FIART27)
001600070116     FFNBRV07L  IF   E           K DISK
001700030220     FFiVAB01L  UF   E           K DISK
001800030220     FFiVAD01L  UF   E           K DISK
001900030220     FFiVAT01L  UF   E           K DISK
002000040604     FFiVAX01L  UF   E           K DISK
002100000908     FFNLSCBD   CF   E             WORKSTN
002200000908
002300000919     D L1              S              3  0 DIM(30)
002400030220     d Lnp             s              3  0 Dim(30)
002500000908     D paxccc          s                   like(acokcc)
002600000908     D paxdmt          s                   like(acorag)
002700000908     D keyaas          s                   like(blpaas)
002800000908     D keymgs          s                   like(blpmgs)
002900000908     D keycod          s                   like(tblcod)
003000000908     D keykey          s                   like(tblkey)
003100030220     d KeyLnp          s                   Like(BlpLnp)
003200030220     d w0030           s              3  0
003300030220     d xx              s              2  0
003400000908
003500900514     D KPJBA         E DS
003600000908     D DS3A          E DS
003700000920     D OG143         E DS
003800000908     D DSLS63        E DS                  EXTNAME(FNLS63DS)
003900030220     d Fnlse5ds      e ds
004000000908
004100000908      * Parametri azienda e tabelle ricerca conti
004200000908
004300000908     D CNCR80        E DS
004400000919
004500000919     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004600000919     D  LIN                    1     90  0
004700000919     D                                     DIM(30)
004800000908
004900950523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000000908     D  TCU                  398    697
005100000908     D                                     DIM(50)
005200000908     D  KCU                  698    847P 0
005300000908     D                                     DIM(50)
005400000908     D                                     PACKEVEN
005500000908
005600000908     D TCUDS           DS
005700000908     D  F1                     1      1
005800000908     D  F3                     3      3
005900000908     D  F2                     2      2
006000000908     D  F4                     4      4
006100000908     D  F56                    5      6
006200000908
006300000908      * Parametri per controllo data e transform in giorni - XSRDA8 -
006400000908
006500000523     D WLBDAT          DS                  INZ
006600000523     D  G02DAT                 1      8  0
006700000523     D  G02INV                 9     16  0
006800000523     D  G02ERR                17     17
006900000523     D  G02TGI                18     22  0
007000000908
007100000523      *****************************************************************
007200000908      *                                                               *
007300000908      *    OPERAZIONI INIZIALI                                        *
007400000908      *                                                               *
007500000523      *****************************************************************
007600000908
007700000523     C     *ENTRY        PLIST
007800000523     C                   PARM                    KPJBA
007900000908
008000000908      *    Recupero ragione sociale ditta da emettere a video
008100000908
008200000523     C                   Z-ADD     1             CODUT
008300000523     C                   CALL      'X§PARUT'
008400000523     C                   PARM                    UT§DSE
008500000523     C                   MOVEL     RAGUT         RSUT
008600000908     C                   MOVEL     REC80         CNCR80
008700000919     C                   CLEAR                   DSUL06
008800000919     C                   MOVE      '£1'          D06COD
008900000919     C                   MOVEL     SIMFEL        D06KEY
009000000919     C                   MOVEL     DSUL06        KPJBU
009100000919     C                   CALL      'TRUL06R'
009200000919     C                   PARM                    KPJBA
009300000919     C                   MOVEL     KPJBU         DSUL06
009400000919     C                   MOVEA     LIN           L1
009500000908
009600000908      *    Ricerca Capoconti
009700000908
009800000908     C                   DO        50            X                 2 0
009900000908     C                   MOVE      TCU(X)        TCUDS
010000000908     C     F56           CABNE     'CG'          END1
010100000908     C     F4            COMP      '1'                                    21
010200000908     C     F4            COMP      '2'                                    22
010300000908     C     F4            COMP      '3'                                    23
010400000908     C     F4            COMP      '6'                                    27
010500000908      ** 1 CLIENTI   21
010600000908      ** 2 FORNITORI 22
010700000908      ** 3 AGENTI    23
010800000908     C     F3            COMP      '0'                                242425
010900000908     C     F3            COMP      'I'                                    26
011000000908      ** 0 ITALIA   25
011100000908      ** 1 ESTERO   24
011200000908      ** I CAPO CONTO IVA
011300000908     C   21
011400000908     CAN 24              Z-ADD     KCU(X)        KCE               4 0
011500000908     C   21
011600000908     CAN 25              Z-ADD     KCU(X)        KCI               4 0
011700000908     C   22
011800000908     CAN 24              Z-ADD     KCU(X)        KFE               4 0
011900000908     C   22
012000000908     CAN 25              Z-ADD     KCU(X)        KFI               4 0
012100000908     C   23
012200000908     CAN 24              Z-ADD     KCU(X)        KAE               4 0
012300000908     C   23
012400000908     CAN 25              Z-ADD     KCU(X)        KAI               4 0
012500000908     C   26              Z-ADD     KCU(X)        KIVA              4 0
012600000908     C   27              Z-ADD     KCU(X)        KBNA              4 0
012700000908     C     END1          TAG
012800000908     C                   END
012900000908     C                   SETOFF                                       212223
013000000908     C                   SETOFF                                       242526
013100000908     C                   SETOFF                                       27
013200000908
013300000908      *    Accesso a CNACO00F
013400000908
013500000908     C     KACO          KLIST
013600000908     C                   KFLD                    CODUT
013700000908     C                   KFLD                    KCI
013800000908     C                   KFLD                    VIDCLI
013900000908
014000000908      *    Accesso a TABEL00F
014100000908
014200000908     C     KTAB          KLIST
014300000908     C                   KFLD                    CODUT
014400000908     C                   KFLD                    KEYCOD
014500000908     C                   KFLD                    KEYKEY
014600000908     C                   move      '3A'          KEYCOD
014700000908
014800000908      *    Accesso a FNBLP40L
014900000908
015000000908     C     KBLP          KLIST
015100030220     c                   Kfld                    KeyLnp
015200000908     C                   KFLD                    KEYAAS
015300000908     C                   KFLD                    KEYMGS
015400000908
015500090304      *    Accesso a FIART01L
015600000908
015700090304     C     KART          KLIST
015800000908     C                   KFLD                    BLPAAS
015900000908     C                   KFLD                    BLPLNP
016000000908     C                   KFLD                    BLPNRS
016100000908     C                   KFLD                    BLPNSP
016200000908
016300070116      *    Accesso a FNBRV07L
016400000908
016500000908     C     KBRV          KLIST
016600090304     C                   KFLD                    ARTFLS
016700090304     C                   KFLD                    ARTLNA
016800090304     C                   KFLD                    ARTNRS
016900090304     C                   KFLD                    ARTNSC
017000000920
017100000920      *    Accesso a FNBLP01L
017200000920
017300000920     C     KBLP1         KLIST
017400090304     C                   KFLD                    ARTAAS
017500090304     C                   KFLD                    ARTLNP
017600090304     C                   KFLD                    ARTNRS
017700090304     C                   KFLD                    ARTNSP
017800000920
017900090304      *    Accesso a FIART27L
018000000920
018100090304     C     KART27        KLIST
018200000920     C                   KFLD                    VABLNP
018300000920     C                   KFLD                    VABLNA
018400000920     C                   KFLD                    VABNRS
018500000920     C                   KFLD                    VABNCD
018600000920
018700030220      *    Accesso a FiVAD01L e FiVAT01L
018800000920
018900000920     C     KVADET        KLIST
019000030220     c                   Kfld                    VabFgs
019100000920     C                   KFLD                    VABCCM
019200000920     C                   KFLD                    VABAAS
019300000920     C                   KFLD                    VABLNP
019400000920     C                   KFLD                    VABNRS
019500000920     C                   KFLD                    VABNSP
019600000908
019700000523      *****************************************************************
019800000908      *                                                               *
019900000908      *    ROUTINE PRINCIPALE                                         *
020000000908      *                                                               *
020100000523      *****************************************************************
020200000908
020300000908      *    Imposto valori iniziali file video
020400000908
020500000908     C                   z-add     0             vidcli
020600000908     C                   clear                   viddes
020700000919     C                   z-add     0             vidlnp
020800000919     C                   clear                   viddea
020900000919     C                   z-add     0             vidnrs
021000000919     C                   move      'N'           vidcon
021100000908     C                   move      *DATE         viddts
021200000908     C                   move      '2'           vidspu
021300000919     C                   move      'N'           vidnco
021400000919     C                   move      '2'           viddop
021500000908     C                   move      'S'           vidsim
021600000919     C                   clear                   vidsc1
021700000919     C                   clear                   vidsc2
021800000908
021900000908      *    ===============
022000000908      *    EMISSIONE VIDEO
022100000908      *    ===============
022200000908
022300000523     C     EMET0         TAG
022400000908     C                   EXFMT     fnlscb1
022500000908
022600000908      *    F3=Fine
022700000908
022800000908     C   KC              goto      FINE
022900000908
023000000908      *    ===========================
023100000908      *    CONTROLLO PARAMETRI IMMESSI
023200000908      *    ===========================
023300000908
023400000908     C                   setoff                                       0304
023500000919     C                   clear                   vidsc1
023600000919     C                   clear                   vidsc2
023700000908
023800000919      *    1) Ricerca Cliente Mittente
023900000908
024000000908     C     viddes        ifne      *blanks
024100000908     C     vidcli        andeq     0
024200000908
024300000908     C                   movel     rsut          PAXDUT           30
024400000908     C                   z-add     9             PAXSTA
024500000908     C                   z-add     KCI           PAXCCC
024600000908     C                   movel(P)  viddes        PAXDMT
024700000908     C                   z-add     1             PAXNUM
024800000908
024900000908     C                   CALL      'XALFA3BR'
025000000908     c                   PARM                    PAXDUT
025100000908     C                   PARM                    CODUT
025200000908     C                   PARM                    PAXDMT
025300000908     C                   PARM                    PAXCCC
025400000908     C                   PARM                    PAXSTA            1 0
025500000908     C                   PARM                    PAXFLR           90
025600000908     C                   PARM                    PAXDIT            3
025700000908     C                   PARM                    PAXNUM            2 0
025800000908     C                   PARM                    PAXKCM           80
025900000908     C                   PARM                    PAXKSM          140
026000000908     C                   PARM                    PAXKDM           60
026100000908
026200000908    2C     PAXSTA        IFGT      -1
026300000908     C                   movel     paxksm        vidcli
026400000908     C                   movel(P)  paxdmt        viddes
026500000908     C                   else
026600000908     C                   clear                   vidcli
026700000908     C                   endif
026800000908     C                   goto      EMET0
026900000908      *                  ---------------
027000000908
027100000908     C                   endif
027200000908
027300000919      *    2) O Cliente O P.O. Partenza
027400000919
027500000919     C     vidcli        ifeq      0
027600000919     C     vidlnp        andeq     0
027700000919     C     vidnrs        andeq     0
027800000919     C                   clear                   viddes
027900000919     C                   clear                   viddea
028000000919     C                   seton                                        21
028100000919     C                   goto      EMET0
028200000919      *                  ---------------
028300000919     C                   endif
028400000919
028500000919     C     vidcli        ifne      0
028600000919     C     vidlnp        andne     0
028700000919     C                   clear                   viddes
028800000919     C                   clear                   viddea
028900000919     C                   seton                                        21
029000000919     C                   goto      EMET0
029100000919      *                  ---------------
029200000919     C                   endif
029300000919
029400000919      *    3) Se immessa Serie Immettere anche P.O. Partenza
029500000919
029600000919     C     vidnrs        ifne      0
029700000919     C     vidlnp        andeq     0
029800000919     C                   clear                   viddes
029900000919     C                   clear                   viddea
030000000919     C                   seton                                        25
030100000919     C                   goto      EMET0
030200000919      *                  ---------------
030300000919     C                   endif
030400000919
030500000919      *    4) Controllo Cliente in Anagrafica
030600000919
030700000919     C     vidcli        ifne      0
030800000908     C     kaco          chain     CNACO00F
030900000908     C                   IF        %FOUND
031000000908     C     acoflg        ifeq      '*'
031100000908     C                   clear                   viddes
031200000908     C                   seton                                        01
031300000908     C                   goto      EMET0
031400000908      *                  ---------------
031500000908     C                   else
031600000908     C                   movel     acorag        viddes
031700000908     C                   endif
031800000908     C                   ELSE
031900000908     C                   clear                   viddes
032000000908     C                   seton                                        01
032100000908     C                   goto      EMET0
032200000908      *                  ---------------
032300000908     C                   ENDIF
032400030220      *   non può essere un cliente Poste
032500030220     c                   Clear                   Og143
032600030220     c                   Movel     Vidcli        w0030
032700030220     c     w0030         Chain     Azorg01l
032800030220     c                   If        %Found(Azorg01l)
032900030220     c                   Movel     Orgde3        Og143
033000030220     c                   EndIf
033100030220     c                   If        §OgNtw = 'PPT'
033200030220     c                   Eval      *In26 = *On
033300030220     c                   GoTo      Emet0
033400030220      *                  ---------------
033500030220     c                   EndIf
033600000919     C                   else
033700000919     C                   clear                   viddes
033800000919     C                   clear                   vidcli
033900000919     C                   endif
034000000919
034100000919      *    5) Controllo P.O. Partenza
034200000919
034300000919     C     vidlnp        ifne      0
034400000919     C     vidnrs        andeq     0
034500000919     C                   clear                   viddes
034600000919     C                   clear                   viddea
034700000919     C                   seton                                        24
034800000919     C                   goto      EMET0
034900000919      *                  ---------------
035000000919     C                   endif
035100000908
035200000919     C     vidlnp        ifne      0
035300030220     c                   Clear                   Og143
035400000919     C     vidlnp        chain     azorg01l
035500000919     C                   if        %found
035600000919     C                   movel     orgdes        viddea
035700030220     c                   Movel     Orgde3        Og143
035800000919     C     orgfva        ifne      ' '
035900000919     C                   seton                                        22
036000000919     C                   goto      EMET0
036100000919      *                  ---------------
036200000919     C                   endif
036300000919     C     orgfag        ifne      'A'
036400000919     C     orgfag        andne     'F'
036500000919     C                   seton                                        22
036600000919     C                   goto      EMET0
036700000919      *                  ---------------
036800000919     C                   endif
036900030220      *   non può essere un p.o. Poste
037000030220     c                   If        §OgNtw = 'PPT'
037100030220     c                   Eval      *In28 = *On
037200030220     c                   GoTo      Emet0
037300030220      *                  ---------------
037400030220     c                   EndIf
037500000919     C     vidcon        ifeq      'S'
037600000919     C     vidlnp        lookup    L1                                     27
037700000919     C     *in27         ifeq      *off
037800000919     C                   seton                                        23
037900000919     C                   goto      EMET0
038000000919      *                  ---------------
038100000919     C                   endif
038200000919     C                   endif
038300000919     C                   else
038400000919     C                   clear                   viddea
038500000919     C                   seton                                        22
038600000919     C                   goto      EMET0
038700000919      *                  ---------------
038800000919     C                   endif
038900000919     C                   else
039000000919     C                   clear                   viddea
039100000919     C                   clear                   vidlnp
039200000919     C                   endif
039300000919
039400000919      *    6) Cancellazione Confermate o NON Confermate
039500000919
039600000919     C     vidcon        ifeq      'S'
039700000919     C     vidnco        andeq     'S'
039800000919     C                   seton                                        41
039900000919     C                   goto      EMET0
040000000919      *                  ---------------
040100000919     C                   endif
040200000919
040300000919     C     vidcon        ifeq      'N'
040400000919     C     vidnco        andeq     'N'
040500000919     C                   seton                                        41
040600000919     C                   goto      EMET0
040700000919      *                  ---------------
040800000919     C                   endif
040900000919
041000000919      *    7) Data
041100000908
041200000919     C     vidcon        ifeq      'S'
041300000908     C                   Z-ADD     viddts        G02DAT
041400000523     C                   MOVEL     *BLANK        G02ERR
041500000523     C                   CALL      'XSRDA8'
041600000523     C                   PARM                    WLBDAT
041700000523     C                   MOVEL     G02INV        DATA$$            8 0
041800000908
041900000523     C     G02ERR        IFEQ      '1'
042000000908     C     viddts        OREQ      0
042100000908     C                   SETON                                        02
042200000908     C                   goto      EMET0
042300000908      *                  ---------------
042400000523     C                   ENDIF
042500000908     C                   Z-ADD     G02DAT        viddts
042600000919     C                   endif
042700000908
042800000919      *    8) Non premuto F6
042900000908
043000000908     C  NKF              goto      EMET0
043100000908      *                  ---------------
043200000908
043300000908      *    ====================================
043400000908      *    CONTROLLI PASSATI: CANCELLO LE BOLLE
043500000908      *    ====================================
043600000919
043700000919     C                   WRITE     fnlscb2
043800000919     C                   seton                                        0304
043900000919     C     vidsim        ifeq      'A'
044000000919     C                   seton                                        10
044100000919     C                   else
044200000919     C                   setoff                                       10
044300000919     C                   endif
044400000919     C                   z-add     0             VIDNRB
044500000908
044600000919     C     vidcon        ifeq      'S'
044700000919     C                   move      '<----'       vidsc1
044800000908     C                   exsr      DLTBLP
044900000919      *                  ================
045000000919     C                   endif
045100000919
045200000919     C     vidnco        ifeq      'S'
045300000919     C                   move      '<----'       vidsc2
045400000919     C                   exsr      DLTVAB
045500000919      *                  ================
045600000919     C                   endif
045700000919
045800000908     C                   goto      EMET0
045900000908
046000000523      *    FINE PROGRAMMA
046100000908
046200000523     C     FINE          TAG
046300000925     c                   clear                   dsls63
046400000925     c                   movel     'C'           i63tla
046500000925     c                   movel     dsls63        kpjbu
046600000925     c                   call      'FNLS63R'
046700000925     c                   parm                    kpjba
046800030220
046900030220      *    Prima di fine richiamo il pgm per pulizia bolle obsolete
047000030220      *    in modalità 'C' se il richiamo precedente era andato a
047100030220      *    buon fine
047200030220     c                   If        Oe5Err <> 'E'
047300030220     c                   Clear                   Fnlse5ds
047400030220     c                   Eval      Ie5Tla = 'C'
047500030220     c                   Call      'FNLSE5R'
047600030220     c                   Parm                    Fnlse5ds
047700030220     c                   EndIf
047800000925     c*
047900000523     C                   SETON                                        LR
048000000908
048100000523      *****************************************************************
048200000908      *                                                               *
048300000908      *    SUBROUTINES                                                *
048400000908      *                                                               *
048500000523      *****************************************************************
048600000908
048700000525      *---------------------------------------------------------------*
048800000919      *    Subroutine di cancellazione bolle CONFERMATE               *
048900000525      *---------------------------------------------------------------*
049000000908
049100000908     C     DLTBLP        begsr
049200030220
049300030220     c                   If        VidLnp > 0
049400030220     c                   Eval      Lnp(1) = VidLnp
049500030220     c                   Else
049600030220     c                   Movea     Lin           Lnp
049700030220     c                   EndIf
049800000908     C                   movel     data$$        keyaas
049900000908     C                   move      data$$        keymgs
050000000908
050100030220     c                   Do        30            xx
050200030220     c                   If        Lnp(xx) = *Zeros
050300030220     c                   Leave
050400030220     c                   EndIf
050500030220     c                   Movel     Lnp(xx)       KeyLnp
050600030220      *    =============================
050700030220      *    LEGGO LE BOLLE PER LNP E DATA
050800030220      *    =============================
050900000908
051000030220     c     Kblp          Setll     Fnblp70l
051100000908===  C                   DO        *hival
051200000908
051300030220     C     kblp          reade     fnblp70l                               30
051400000908     C   30              leave
051500000908
051600000908      *    Escludo quelle già trasmesse
051700000908
051800000908     C     blpft1        ifne      ' '
051900000908     C                   iter
052000000908     C                   endif
052100000908
052200000919      *    SE ELABORAZIONE PER MITTENTE
052300000919      *    Escludo se il mittente non è quello digitato a video
052400000908      *    NB: - per le bolle franco il cli.mittente è BLPKSC
052500000908      *        - per le bolle assegn il cli.mittente è BLPCCM
052600000908
052700000919+++  C     VIDCLI        ifne      0
052800000908     C                   movel(P)  blpcbo        keykey
052900000908     C     ktab          chain     tabel00f
053000000908---  C                   if        %found
053100000908     C                   movel     tbluni        ds3a
053200000908     C                   movel     §3atb1        Wfrass            1
053300000908     C     Wfrass        ifne      'A'
053400000908     C     Wfrass        andne     'F'
053500000908     C                   iter
053600000908     C                   endif
053700000908      *              Se Assegnato
053800000908     C     Wfrass        ifeq      'A'
053900000908     C     VIDCLI        ifne      blpccm
054000000908     C                   iter
054100000908     C                   endif
054200000908      *              Se Franco
054300000908     C                   else
054400000908     C     VIDCLI        ifne      blpksc
054500000908     C                   iter
054600000908     C                   endif
054700000908     C                   endif
054800000908---  C                   else
054900000908     C                   iter
055000000919---  C                   endif
055100000919+++  C                   endif
055200000919
055300000919      *    SE ELABORAZIONE PER P.O. PARTENZA
055400000919      *    Escludo se LNP o NRS non corrispondono al video
055500000908
055600000919+++  C     VIDLNP        ifne      0
055700030220     C     VIDNRS        andne     blpnrs
055800000919     C                   iter
055900000919+++  C                   endif
056000000919
056100000908      *    Escludo se vi sono spunte e non voglio cancellare
056200000908
056300000908     C     VIDSPU        ifeq      '2'
056400000908     C                   setoff                                       33
056500090304     C     kART          setll     FIART01l
056600000908     C                   do        *hival
056700090304     C     kART          reade     FIART01l                               34
056800000908     C   34              leave
056900070116     C     kbrv          chain     fnbrv07l
057000000908     C                   if        %found
057100000908     C                   seton                                        33
057200000908     C                   leave
057300000908     C                   endif
057400000908     C                   enddo
057500000908     C   33              iter
057600000908     C                   endif
057700000908
057800000908      *    OK CANCELLO
057900000908      *    ===========
058000000908
058100000908     C                   add       1             vidnrb
058200000908     C                   setoff                                       03
058300000908
058400000908      *    Lancio il programma di cancellazione bolla singola
058500000908      *    Se scelto "AGGIORNAMENTO" a video
058600000908
058700000908     C     vidsim        ifeq      'A'
058800000908     C                   CLEAR                   DSLS63
058900000908     C                   MOVE      BLPAAS        I63AAS
059000000908     C                   MOVE      BLPLNP        I63LNP
059100000908     C                   MOVE      BLPNRS        I63NRS
059200000908     C                   MOVE      BLPNSP        I63NSP
059300000908     C                   MOVEL     DSLS63        KPJBU
059400000908     C                   CALL      'FNLS63R'
059500000908     C                   PARM                    KPJBA
059600000908     C                   endif
059700000908
059800000908===  C                   ENDDO
059900030220
060000030220     c                   add       1             xx
060100030220     c                   EndDo
060200000908
060300000908     C                   endsr
060400000919
060500000919      *---------------------------------------------------------------*
060600000919      *    Subroutine di cancellazione bolle NON CONFERMATE           *
060700000919      *---------------------------------------------------------------*
060800000919
060900000919     C     DLTVAB        begsr
061000030220
061100030220      *    Per prima cosa richiamo il pgm oer pulizia bolle obsolete
061200030220     c                   Clear                   Fnlse5ds
061300030220     c                   Eval      Ie5Pou = Simfel
061400030220     c                   Call      'FNLSE5R'
061500030220     c                   Parm                    Fnlse5ds
061600000919
061700000920      *    Verifico quale file devo cancellare e per quale chiave:
061800000920      *    *IN46 = *ON .. Posso posizionarmi per codice cliente
061900000920
062000000920     C     VIDCLI        ifne      0
062100000920     C                   seton                                        46
062200000920     C                   else
062300000920     C                   setoff                                       46
062400000920     C                   endif
062500000920
062600000920      *    ===========================
062700000920      *    Mi posiziono per la lettura
062800000920      *    ===========================
062900000920
063000030220     C   46VIDCLI        setll     fivab01l
063100030220     C  N46*loval        setll     fivab01l
063200000920
063300000920      *    ==============
063400000920      *    LEGGO LE BOLLE
063500000920      *    ==============
063600000920
063700000920===  C                   do        *hival
063800000920
063900030220     C   46VIDCLI        reade     fivab01l
064000030220     C  N46              read      fivab01l
064100000920
064200000920     C                   if        %eof
064300000920     C                   leave
064400000920     C                   endif
064500030220
064600030220      *  Controllo il p.o. gestione --> deve essere in L1
064700030220     c     VabFgs        Lookup    L1                                     30
064800030220     c                   If        Not *In30
064900030220     c                   Iter
065000030220     c                   EndIf
065100000920
065200000920      *    Controllo le linee partenza e le serie
065300000920      *    (se non sto leggendo per cliente)
065400000920
065500000920     C     *in46         ifeq      *off
065600000920     C     VIDLNP        ifne      vablnp
065700000920     C                   iter
065800000920     C                   endif
065900000920     C     VIDNRS        ifne      vabnrs
066000000920     C                   iter
066100000920     C                   endif
066200000920     C                   endif
066300000920
066400000920      *    CONTROLLO DETTAGLIO SEGNACOLLI:
066500000920
066600090304     C     kART27        chain     FIART27l                           27
066700000920
066800000920      *    Se ho richiesto solo le bolle con segnacolli doppi
066900090304      *    non considero se non ho trovato record di ART
067000000920
067100000920     C   27VIDDOP        ifeq      '2'
067200000920     C                   iter
067300000920     C                   endif
067400000920
067500000919      *    OK CANCELLO
067600000919      *    ===========
067700000919
067800000919     C                   add       1             vidnrb
067900000919     C                   setoff                                       03
068000000920
068100000920      *    Cancello solo se è stato scelto "AGGIORNAMENTO"
068200000919
068300000920---  C     VIDSIM        ifeq      'A'
068400000920
068500040604      *    Cancellazione bolla normale (FiVAD01L + FiVAT01L + FIVAX01L e
068600030220      *    per ultimo FiVAB01L)
068700000920
068800000920...  C     vabnsp        ifgt      0
068900000920
069000030220     C     kvadet        setll     fivad01l
069100000920     C                   do        *hival
069200030220     C     kvadet        reade     fivad01l
069300000920     C                   if        %eof
069400000920     C                   leave
069500000920     C                   endif
069600000920
069700000920      *                  ------------------
069800030220     C                   delete    fivad000
069900000920      *                  ------------------
070000000920
070100000920     C                   enddo
070200000920
070300030220     C     kvadet        setll     fivat01l
070400000920     C                   do        *hival
070500030220     C     kvadet        reade     fivat01l
070600000920     C                   if        %eof
070700000920     C                   leave
070800000920     C                   endif
070900000920
071000000920      *                  ------------------
071100030220     C                   delete    fivat000
071200000920      *                  ------------------
071300000920
071400000920     C                   enddo
071500040604
071600040604     C     kvadet        setll     fivax01l
071700040604     C                   do        *hival
071800040604     C     kvadet        reade     fivax01l
071900040604     C                   if        %eof
072000040604     C                   leave
072100040604     C                   endif
072200040604
072300040604      *                  ------------------
072400040604     C                   delete    fivax000
072500040604      *                  ------------------
072600040604
072700040604     C                   enddo
072800000920
072900000920...  C                   endif
073000000920
073100000920      *                  ------------------
073200030220     C                   delete    fivab000
073300000920      *                  ------------------
073400000920
073500000920---  C                   endif
073600000920
073700000920===  C                   enddo
073800000920
073900000919     C                   endsr
