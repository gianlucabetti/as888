000100001003     H DECEDIT('0,') DATEDIT(*YMD.)
000200000927
000300000927      *****************************************************************
000400000927      *                                                               *
000500001018      *         STAMPA ANAGRAFICA CLIENTI RITIRO - filtro             *
000600000927      *                                                               *
000700000927      *****************************************************************
000800000927
000900001018     FCNACO00F  IF   E           K DISK
001000010118     FTABEL00F  IF   E           K DISK
001100001017     FAZORG01L  IF   E           K DISK
001200001017     FFIOR10D   CF   E             WORKSTN
001300000927
001400001018     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MSG VIDEO
001500000927     D L1              S              3  0 DIM(30)                              P.O. Gestiti
001600001018
001700010118     D kcod            S                   LIKE(TBLcod)
001800010118     D kkey            S                   LIKE(TBLkey)
001900010118     D kdes            S             30
002000001018
002100001017     D KPJBA         E DS
002200001018
002300001018     D PARAM           DS                                                       Passaggio Parametri
002400001018     D  vidse1                 1      1
002500001018     D  vidse2                 2      2
002600001018     D  vidcro                 4     13  0
002700001018     D  vidrsr                14     43
002800001018     D  vidpoa                58     60  0
002900001018     D  vidtac                61     61
003000001018     D  vidtcr                62     62
003100001018     D  vidznr                63     64
003200001018     D  vidksc                66     72  0
003300001018     D  vidcro2               73     82  0
003400010206     D  vidsol                83     83
003500000927
003600000927      * Parametri Azienda e Tabelle Ricerca Conti
003700000927
003800000927     D CNCR80        E DS
003900000927
004000000927     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004100000927     D  LIN                    1     90  0
004200000927     D                                     DIM(30)
004300000927
004400000927     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004500000927     D  TCU                  398    697
004600000927     D                                     DIM(50)
004700000927     D  KCU                  698    847P 0
004800000927     D                                     DIM(50)
004900000927     D                                     PACKEVEN
005000000927
005100000927     D TCUDS           DS
005200000927     D  F1                     1      1
005300000927     D  F3                     3      3
005400000927     D  F2                     2      2
005500000927     D  F4                     4      4
005600000927     D  F56                    5      6
005700010118
005800010118     D  ds_kkey        DS
005900010118     D  ds_vipoa                      3  0
006000010118     D  ds_viznr                      2
006100000927
006200000927      *---------------------------------------------------------------*
006300000927      *                                                               *
006400000927      *    UTILIZZO DEGLI INDICATORI                                  *
006500000927      *                                                               *
006600000927      *---------------------------------------------------------------*
006700000927      *                                                               *
006800001018      *    01 - Posiz. Cursore su Selez. per Codice                   *
006900001018      *    02 - Posiz. Cursore su Selez. per Ragione Sociale          *
007000001018      *    03 - Posiz. Cursore su Codice Cliente Ritiro               *
007100001018      *    04 - Posiz. Cursore su Ragione Sociale                     *
007200001018      *    07 - Posiz. Cursore su P.O. Gestione/Appartenenza          *
007300001018      *    08 - Posiz. Cursore su Zona di Ritiro                      *
007400001018      *    09 - Posiz. Cursore su Codice PdC                          *
007500001018      *    28 - Visualizzo msg Errore                                 *
007600001018      *    39 - Comodo                                                *
007700001018      *    90 - Riemetto videata senza Errore                         *
007800000927      *                                                               *
007900000927      *---------------------------------------------------------------*
008000000927
008100000927      *****************************************************************
008200000927      *                                                               *
008300000927      *    OPERAZIONI INIZIALI                                        *
008400000927      *                                                               *
008500000927      *****************************************************************
008600000927
008700000927     C     *ENTRY        PLIST
008800000927     C                   PARM                    KPJBA
008900001018
009000010118      *    Accesso a TABEL
009100010118
009200010118     C     Ktab          klist
009300010118     C                   kfld                    codut
009400010118     C                   kfld                    kcod
009500010118     C                   kfld                    kkey
009600010119     C                   movel     '03'          kcod
009700001018
009800001018      *    Accesso a CNACO
009900001018
010000001018     C     KCNACO        KLIST
010100001018     C                   kfld                    CODUT
010200001018     C                   kfld                    KCI
010300001018     C                   kfld                    vidksc
010400000928
010500000927      *    Recupero Ragione Sociale Ditta da Emettere a Video
010600000927
010700000927     C                   Z-ADD     1             CODUT
010800000927     C                   CALL      'X§PARUT'
010900000927     C                   PARM                    UT§DSE
011000000927     C                   MOVEL     RAGUT         RSUT
011100000927     C                   MOVEL     REC80         CNCR80
011200000927     C                   CLEAR                   DSUL06
011300000927     C                   MOVE      '£1'          D06COD
011400000927     C                   MOVEL     SIMFEL        D06KEY
011500000927     C                   MOVEL     DSUL06        KPJBU
011600000927     C                   CALL      'TRUL06R'
011700000927     C                   PARM                    KPJBA
011800000927     C                   MOVEL     KPJBU         DSUL06
011900000927     C                   MOVEA     LIN           L1
012000000927
012100000927      *    Ricerca Capoconti
012200000927
012300000927     C                   DO        50            X                 2 0
012400000927     C                   MOVE      TCU(X)        TCUDS
012500000927     C     F56           CABNE     'CG'          END1
012600000927     C     F4            COMP      '1'                                    21
012700000927     C     F4            COMP      '2'                                    22
012800000927     C     F4            COMP      '3'                                    23
012900000927     C     F4            COMP      '6'                                    27
013000000927      ** 1 CLIENTI   21
013100000927      ** 2 FORNITORI 22
013200000927      ** 3 AGENTI    23
013300000927     C     F3            COMP      '0'                                242425
013400000927     C     F3            COMP      'I'                                    26
013500000927      ** 0 ITALIA   25
013600000927      ** 1 ESTERO   24
013700000927      ** I CAPO CONTO IVA
013800000927     C   21
013900000927     CAN 24              Z-ADD     KCU(X)        KCE               4 0
014000000927     C   21
014100000927     CAN 25              Z-ADD     KCU(X)        KCI               4 0
014200000927     C   22
014300000927     CAN 24              Z-ADD     KCU(X)        KFE               4 0
014400000927     C   22
014500000927     CAN 25              Z-ADD     KCU(X)        KFI               4 0
014600000927     C   23
014700000927     CAN 24              Z-ADD     KCU(X)        KAE               4 0
014800000927     C   23
014900000927     CAN 25              Z-ADD     KCU(X)        KAI               4 0
015000000927     C   26              Z-ADD     KCU(X)        KIVA              4 0
015100000927     C   27              Z-ADD     KCU(X)        KBNA              4 0
015200000927     C     END1          TAG
015300000927     C                   END
015400000927     C                   SETOFF                                       212223
015500000927     C                   SETOFF                                       242526
015600000927     C                   SETOFF                                       27
015700000927
015800000927      *    Inizializzo Campi Video
015900000927
016000001017     C                   clear                   vidse1
016100001017     C                   clear                   vidse2
016200001017     C                   clear                   vidcro
016300001018     C                   clear                   vidcro2
016400001017     C                   clear                   vidrsr
016500001017     C                   clear                   vidtac
016600001017     C                   clear                   vidtcr
016700010206     C                   clear                   vidsol
016800010206     C                   clear                   vidznr
016900001018     C                   movel(p)  '_=Tutte'     viddez
017000001017     C                   clear                   vidksc
017100020502     C*                  movel     knmus         wrem              3
017200020502     C*    wrem          ifeq      'REM'
017300020502     C*                  movel     knmus         w6alfa            6
017400020502     C*                  move      w6alfa        vidpoa
017500020502     c     simtpp        ifeq      '2'
017600020502     c     simtpp        oreq      *blanks
017700020502     c                   move      simpou        vidpoa
017800001017     C                   else
017900001017     C                   move      simfel        vidpoa
018000001017     C                   endif
018100001017     C     vidpoa        chain     azorg01l
018200001017     C                   if        %found(azorg01l)
018300001017     C                   movel     orgdes        viddep
018400001017     C                   else
018500001017     C                   movel(p)  '0=Tutti'     viddep
018600001017     C                   endif
018700000927
018800000927      *****************************************************************
018900000927      *                                                               *
019000000927      *    ROUTINE PRINCIPALE                                         *
019100000927      *                                                               *
019200000927      *****************************************************************
019300000927
019400000927     C                   DO        *hival
019500000927
019600000928      *    Emissione 1° Videata
019700000927
019800001017     C                   EXFMT     FIOR101
019900001018     C                   eval      *in28 = *off
020000001018     C                   eval      *in90 = *off
020100001018     C   KC              LEAVE
020200001002
020300001018      *    Conrollo parametri immessi
020400001018
020500001018     C                   EXSR      SR_CONTROL1
020600001018
020700001018      *    Se Vi Sono ERRORI Riemetto
020800001018
020900001018     C     *in28         ifeq      *on
021000001018     C     *in90         oreq      *on
021100001018     C                   ITER
021200001018     C                   endif
021300001018
021400001018      *    F6=Conferma ---> Lancio Batch
021500001018
021600001018     C     *inkf         ifeq      *on
021700001019     C                   move      'OR11'        KCOAZ
021800001018     C                   movel     PARAM         KPJBU
021900001019     C                   call      'BCH10'
022000001018     C                   parm                    KPJBA
022100001018     C                   LEAVE
022200001018     C                   endif
022300001018
022400000927     C                   ENDDO
022500000927
022600000927     C                   eval      *inlr = *on
022700000927
022800000927      *****************************************************************
022900000927      *                                                               *
023000000927      *    SUBROUTINES                                                *
023100000927      *                                                               *
023200000927      *****************************************************************
023300000927
023400000927      *---------------------------------------------------------------*
023500001018      *    Controllo Parametri Immessi                                *
023600000927      *---------------------------------------------------------------*
023700001018
023800001018     C     SR_CONTROL1   begsr
023900001018
024000001018      *    Reset indicatori
024100001018
024200001018     C                   setoff                                       010203
024300040629     C                   setoff                                       04
024400001018     C                   setoff                                       070809
024500001018
024600001018      *    Controllo P.O. Gestione
024700001018
024800001018     C     vidpoa        ifne      0
024900001018     C     vidpoa        chain     azorg01l
025000001018     C                   if        %found(azorg01l)
025100001018     C                   movel     orgdes        viddep
025200001018     C                   else
025300001018     C                   movel(p)  '0=Tutti'     viddep
025400001018     C                   eval      *in07 = *on
025500001018     C                   eval      *in28 = *on
025600001018     C                   movel     msg(1)        vidmsg
025700001018     C                   goto      endsr1
025800001018     C                   endif
025900001018     C                   else
026000001018     C                   movel(p)  '0=Tutti'     viddep
026100001018     C                   endif
026200001018
026300001018      *    Ricerca Zona Ritiro con "?"
026400001018
026500001018     C     '?'           scan      vidznr                                 39
026600001018     C     *in39         ifeq      *on
026700001018     C                   eval      *in08 = *on
026800001018     C                   movel(p)  '_=Tutte'     viddez
026900001018     C                   eval      *in90 = *on
027000010208     C                   clear                   ds_kkey
027100010118     C                   eval      ds_vipoa = vidpoa
027200010118     C                   movel     ds_kkey       kkey
027300010118     C                   CALL      'X§TABER'
027400010118     C                   PARM                    codut
027500010118     C                   PARM                    kcod
027600010118     C                   PARM                    kkey
027700010118     C                   PARM                    kdes
027800010215     C                   if        kkey = *blanks
027900010215     C                   clear                   vidznr
028000010215     C                   goto      endsr1
028100010215     C                   endif
028200010118     C                   movel     kkey          ds_kkey
028300010118     C                   eval      vidznr = ds_viznr
028400010118     C                   movel     kdes          viddez
028500010208     C                   if        ds_vipoa <> vidpoa
028600010208     C                   movel     msg(9)        vidmsg
028700010208     C                   seton                                        08  28
028800010215     C                   clear                   vidznr
028900010215     C                   clear                   ds_kkey
029000010208     C                   endif
029100001018     C                   goto      endsr1
029200001018     C                   endif
029300001018
029400001018      *    E' necessario immettere una e solo una Selezione
029500001018
029600001018     C     vidse1        ifeq      ' '
029700001018     C     vidse2        andeq     ' '
029800001018     C                   eval      *in01 = *on
029900001018     C                   eval      *in28 = *on
030000001018     C                   movel     msg(2)        vidmsg
030100001018     C                   goto      endsr1
030200001018     C                   endif
030300001018
030400001018     C     vidse1        ifeq      '1'
030500001018     C     vidse2        ifeq      '1'
030600001018     C                   eval      *in01 = *on
030700001018     C                   eval      *in28 = *on
030800001018     C                   movel     msg(3)        vidmsg
030900001018     C                   goto      endsr1
031000001018     C                   endif
031100001018     C                   endif
031200001018
031300001018      *    Non è possibile Valorizzare i Campi Senza Selezione
031400001018
031500001018     C     vidse1        ifeq      ' '
031600001018     C     vidcro        andne     0
031700001018     C                   eval      *in03 = *on
031800001018     C                   eval      *in28 = *on
031900001018     C                   movel     msg(4)        vidmsg
032000001018     C                   goto      endsr1
032100001018     C                   endif
032200001018
032300001018     C     vidse1        ifeq      ' '
032400001018     C     vidcro2       andne     0
032500001018     C                   eval      *in10 = *on
032600001018     C                   eval      *in28 = *on
032700001018     C                   movel     msg(4)        vidmsg
032800001018     C                   goto      endsr1
032900001018     C                   endif
033000001018
033100001018     C     vidse2        ifeq      ' '
033200001018     C     vidrsr        andne     *blanks
033300001018     C                   eval      *in04 = *on
033400001018     C                   eval      *in28 = *on
033500001018     C                   movel     msg(4)        vidmsg
033600001018     C                   goto      endsr1
033700001018     C                   endif
033800001018
033900001018      *    Codice Cliente DA > A
034000001018
034100001018     C     vidcro        ifne      0
034200001018     C     vidcro2       orne      0
034300001018     C     vidcro        ifgt      vidcro2
034400001018     C                   eval      *in03 = *on
034500001018     C                   eval      *in28 = *on
034600001018     C                   movel     msg(5)        vidmsg
034700001018     C                   goto      endsr1
034800001018     C                   endif
034900001018     C                   endif
035000001018
035100010119      *    La Zona di Ritiro se immessa deve esistere su TABEL
035200010208
035300001018     C     vidznr        ifne      *blanks
035400010208     C                   clear                   ds_kkey
035500010118     C                   eval      ds_vipoa = vidpoa
035600010118     C                   eval      ds_viznr = vidznr
035700010118     C                   movel     ds_kkey       kkey
035800010118     C     ktab          chain     tabel00f
035900010215     C                   if        not%found(tabel00f)
036000010215     C                   movel     msg(6)        vidmsg
036100010215     C                   seton                                        08  28
036200010215     C                   goto      endsr1
036300010215     C                   endif
036400010215     C     tblflg        ifne      ' '
036500010215     C                   movel     msg(6)        vidmsg
036600010215     C                   seton                                        08  28
036700010215     C                   goto      endsr1
036800010215     C                   endif
036900010118     C                   movel     tbluni        viddez
037000001018     C                   else
037100001018     C                   movel(p)  '_=Tutte'     viddez
037200001018     C                   endif
037300001018
037400001018      *    Controllo Codice PdC
037500001018
037600001018     C     vidksc        ifne      0
037700001018     C     KCNACO        chain     CNACO00F
037800010223     C                   if        %found(cnaco00f)
037900001018     C     acoflg        ifne      ' '
038000001018     C                   movel     msg(8)        vidmsg
038100001018     C                   seton                                        09  28
038200001018     C                   goto      endsr1
038300001018     C                   endif
038400001018     C                   else
038500001018     C                   movel     msg(8)        vidmsg
038600001018     C                   seton                                        09  28
038700001018     C                   goto      endsr1
038800001018     C                   endif
038900001018     C                   endif
039000001018
039100001018     C     ENDSR1        endsr
039200001018
039300001018** MSG
039400070418Filiale errata                                                                      1
039500001018Immettere una Selezione                                                             2
039600001018Immessa Più di una Selezione, Immetterne Solamente una                              3
039700001018Non è Possibile Immettere una Valore se non è stata Immessa una Selezione           4
039800001018Codice Cliente Ritiro Iniziale Maggiore di quello Finale                            5
039900001018Zona di Ritiro Errata                                                               6
040000040629                                                                                    7
040100001018Codice Cliente Piano dei Conti Errato                                               8
040200070418Scelta zona relativa ad altra filiale                                               9
040300001018..............  Libero                                                             10
