000100020726     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020726     H*PARMS ACTGRP(QILE)
000300020726     H decedit(',') datedit(*dmy)
000400891030     F*--------------------------------------------------------------*
000500020726     F* GESTIONE TABELLA Y4O
000600891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000700891030     F*  01        INSERIMENTO                                       *
000800891030     F*  02        VARIAZIONE                                        *
000900020726     F*  03        annullamento                                      *
001000891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001100891030     F*--------------------------------------------------------------*
001200020726     FTntb27D   CF   E             WORKSTN
001300020726     Fanuni01l  IF   E           K DISK
001400020726     Fansif01l  IF   E           K DISK
001500020726     Fanope01l  IF   E           K DISK
001600020726     FTntbe01l  UF A E           K DISK
001700891030     D*--------------------------------------------------------------*
001800891030     D*                        I N P U T                             *
001900891030     D*--------------------------------------------------------------*
002000020726     D TIBS02DS      E DS
002100060519     D Dy4O          E DS                  inz
002200891030     D KPJBA         E DS
002300020726     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400020726     D                SDS
002500020726     D  VTCPGM                 1     10
002600020726     D* Dati di ambiente ottenuti da XSOC
002700020726    >D xsoc001ds     E DS
002800020726     D* DS Interna per dati di output di XSOC
002900020726     D XSOCDS          DS          1000
003000020726     D* Descrizione conti
003100020726     D ND002DS       E DS
003200020726     D*-------------
003300020726     D* Definizioni parametri standard chiamate moduli
003400020726     D $Esito          S              1
003500020726     D $gesmsg         S              1
003600020726     D $lenout         S              9B 0
003700020726     D $struttura      S             10
003800020726     D $kcc            S              6
003900020726     D $ksc            S              8
004000020726     D Wdate           S               D
004100971210     C******************************************************
004200020726     C                   DO        *HIVAL
004300891107     C                   MOVEL     *BLANKS       VCOD
004400891107     C                   MOVEL     '?'           VCOD
004500891030     C     INIZIO        TAG
004600911115     C                   EXFMT     TB31D1
004700020726     C   KC              LEAVE
004800020726     C* CONTROLLI 1° VIDEO
004900020726     C                   EXSR      CTR01
005000020726     C   99              GOTO      INIZIO
005100020726     C*
005200020726     C                   SETOFF                                       010203
005300020726     C                   EXSR      PULVID
005400020726     C*
005500020726     C     KTAB          CHAIN     Tntbe01L
005600020726     C* INSERIMENTO
005700020726     C                   IF        NOT %FOUND(TNTBE01L)
005800020726     C                   SETON                                        01        *IMMISSIONE
005900020726     C                   ELSE
006000020726     C*
006001060519     c                   if        %subst(tbeuni: 100: 1)=' '
006002060519     c                   eval      %subst(tbeuni: 100: 1)='0'
006003060519     c                   end
006100020726     C                   MOVEL     TBEUNI        DY4O
006200020726     C* RECORD ANNULLATO
006300020726     C     TBEATB        IFNE      ' '
006400020726     C                   SETON                                        03
006500020726     C                   ELSE
006600020726     C* MANUTENZIONE
006700020726     C                   SETON                                        02        *VARIAZIONE
006800020726     C                   END
006900020726     C                   END
007000020726     C* 2° VIDEO
007100891030     C     SU            TAG
007200911115     C                   EXFMT     TB31D2
007300911115     C   KC              GOTO      FINE
007400911213     C   KL              GOTO      INIZIO
007500891030     C**--- ANNULLAMENTO
007600911115     C   KQ              DO
007700020726     C                   MOVEL     'A'           TBEATB
007800020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
007900020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
008000020726     C                   UPDATE    Tntbe000
008100891030     C                   GOTO      INIZIO
008200891030     C                   END
008300020726     C** RIPRISTINO
008400020726     C   KE              DO
008500020726     C                   MOVE      *BLANKS       TBEATB
008600020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
008700020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
008800020726     C                   UPDATE    Tntbe000
008900020726     C                   GOTO      INIZIO
009000020726     C                   END
009100900608      *
009200891102     C                   EXSR      CTRFMT
009300900608     C   99
009400020726     CORNKF              GOTO      SU
009500891030     C                   EXSR      SCRIVI
009600911119     C**
009700020726     C                   ENDDO
009800891030     C     FINE          TAG
009900891030     C                   SETON                                        LR
010000891030     C**------------------------
010100020726     C** CONTROLLI 1° VIDEO
010200891030     C**------------------------
010300020726     C     CTR01         BEGSR
010400020726     C*
010500020726     C                   SETOFF                                       99
010600020726     C*§  RICERCA CODICE
010700020726     C     VCOD          IFEQ      '?'
010800020726     c                   clear                   TIBS02DS
010900020726     c                   movel     'R'           t02mod
011000020726     c                   movel     knsif         t02sif
011100020726     c                   movel     xscsoc        t02ke1
011200020726     C                   movel     'Y4O'         t02cod
011300020726     C                   CALL      'TIBS02R'
011400020726     c                   parm                    KPJBA
011500020726     c                   parm                    TIBS02DS
011600020726      *
011700020726    2C                   IF        T02err = *blanks and
011800020726     c                             t02ke1 = xscsoc
011900020726     C                   movel     T02ke2        VCOD
012000020726     C                   ENDIF
012100020726     C                   ENDIF
012200020726     C* CAMPI OBBLIGATORI
012300020726     C     VCOD          IFEQ      *BLANKS
012400020726     C     VCOD          OREQ      *ZEROS
012500020726     C                   SETON                                        2999
012600020726     C                   END
012700020726     C** IL CODICE società + filiale deve esistere
012800020726     C                   MOVEL(p)  VCOD          uniunita
012900020726     C                   MOVEL     *BLANKS       DESFIL
013000020726     c     kuni          chain     anuni01l                           29
013100020726     C  N29              MOVEL     UNIDESBREV    DESFIL
013200020726     C   29              SETON                                        99
013300020726     C*
013400020726     C                   ENDSR
013500020726     C**------------------------
013600020726     C** PULISCI CAMPI VIDEO
013700020726     C**------------------------
013800020726     C     PULVID        BEGSR
013900900608      *
014000911115     C                   MOVEL     *BLANKS       §4OCO1
014100911115     C                   MOVEL     *BLANKS       §4OCO2
014200911115     C                   MOVEL     *BLANKS       §4OCO3
014300911115     C                   MOVEL     *BLANKS       §4OCO4
014301021126     C                   MOVEL     *BLANKS       §4OCO5
014302060519     C                   MOVEL     *BLANKS       §4OCO6
014303060519     C                   MOVEL     *BLANKS       §4OCO7
014304060519     C                   MOVEL     *BLANKS       §4OCO8
014305060519     C                   MOVEL     *BLANKS       §4OCO9
014400020726     C                   MOVEL     *BLANKS       §4OCC1
014500020726     C                   MOVEL     *BLANKS       §4OSC1
014600020726     C                   MOVEL     *BLANKS       §4OCC2
014700020726     C                   MOVEL     *BLANKS       §4OSC2
014800020726     C                   MOVEL     *BLANKS       §4OCC3
014900020726     C                   MOVEL     *BLANKS       §4OSC3
015000020726     C                   MOVEL     *BLANKS       §4OCC4
015100020726     C                   MOVEL     *BLANKS       §4OSC4
015101060519     C                   MOVEL     *BLANKS       §4OCC7
015102060519     C                   MOVEL     *BLANKS       §4OSC7
015103060519     C                   MOVEL     *BLANKS       §4OCC8
015104060519     C                   MOVEL     *BLANKS       §4OSC8
015105060519     C                   MOVEL     *BLANKS       §4Odom
015106060519     C                   MOVEL     *zeros        §4Odog
015200900608     C                   MOVEL     *BLANKS       VIDDS1
015300900608     C                   MOVEL     *BLANKS       VIDDS2
015400900608     C                   MOVEL     *BLANKS       VIDDS3
015500911115     C                   MOVEL     *BLANKS       VIDDS4
015600020726     C                   MOVEL     *BLANKS       VIDDS7
015700020726     C                   MOVEL     *BLANKS       VIDDS8
015800020726     C                   MOVEL     *BLANKS       VIDDS9
015900911115     C                   MOVEL     *BLANKS       VIDS10
015901021126     C                   MOVEL     *BLANKS       VIDS11
015902060519     C                   MOVEL     *BLANKS       VIDS12
015903060519     C                   MOVEL     *BLANKS       VIDS13
015904060519     C                   MOVEL     *BLANKS       VIDS14
015905060519     C                   MOVEL     *BLANKS       VIDS15
015906060519     C                   MOVEL     *BLANKS       VIDS16
015907060519     C                   MOVEL     *BLANKS       VIDS17
016000020726     C                   MOVEL(p)  xscsoc        §ke1
016100020726     C                   MOVEL(p)  vCOD          §ke2
016200900608      *
016300891030     C                   ENDSR
016400891030     C**------------------------
016500891030     C** CONTROLLI 2 VIDEATA
016600891030     C**------------------------
016700891030     C     CTRFMT        BEGSR
016800900608      *
016900020726     C                   SETOFF                                       99
017000020726     C                   MOVEL     '0000000000'  *IN(51)
017001060519     C                   MOVEL     '0000000'     *IN(61)
017100900608      *
017200020726     C                   move      §4OCC1        $KCC
017300020726     C                   move      §4OSC1        $KSC
017400020726     C* Richiamo modulo controllo conto x descrizione
017500020726     C                   EXSR      CALLMVC002
017600020726     C*
017700020726     C                   If        $Esito =*off
017800020726     C                   eval      vidds1         = Descr002
017900020726     c                   else
018000020726     c                   seton                                        5199
018100020726     C                   MOVEL     *ALL'?'       VIDDS1
018200020726     C                   Endif
018300900608      *
018400020726     C                   move      §4OCC2        $KCC
018500020726     C                   move      §4OSC2        $KSC
018600020726     C* Richiamo modulo controllo conto x descrizione
018700020726     C                   EXSR      CALLMVC002
018800020726     C*
018900020726     C                   If        $Esito =*off
019000020726     C                   eval      vidds2         = Descr002
019100020726     c                   else
019200020726     c                   seton                                        5299
019300020726     C                   MOVEL     *ALL'?'       VIDDS2
019400020726     C                   Endif
019500900608      *
019600020726     C                   move      §4OCC3        $KCC
019700020726     C                   move      §4OSC3        $KSC
019800020726     C* Richiamo modulo controllo conto x descrizione
019900020726     C                   EXSR      CALLMVC002
020000020726     C*
020100020726     C                   If        $Esito =*off
020200020726     C                   eval      vidds3         = Descr002
020300020726     c                   else
020400020726     c                   seton                                        5399
020500020726     C                   MOVEL     *ALL'?'       VIDDS3
020600020726     C                   Endif
020700911115      *
020800020726     C                   move      §4OCC4        $KCC
020900020726     C                   move      §4OSC4        $KSC
021000020726     C* Richiamo modulo controllo conto x descrizione
021100020726     C                   EXSR      CALLMVC002
021200020726     C*
021300020726     C                   If        $Esito =*off
021400020726     C                   eval      vidds4         = Descr002
021500020726     c                   else
021600020726     c                   seton                                        5499
021700020726     C                   MOVEL     *ALL'?'       VIDDS4
021800060519     c                   Endif
021801060519     c*
021802060519     c                   if        §4occ7 <> ' ' or §4osc7 <> ' '
021803060519     C                   move      §4OCC7        $KCC
021804060519     C                   move      §4OSC7        $KSC
021805060519     C* Richiamo modulo controllo conto x descrizione
021806060519     C                   EXSR      CALLMVC002
021807060519     C*
021808060519     C                   If        $Esito =*off
021809060519     C                   eval      vids13         = Descr002
021810060519     c                   else
021811060519     c                   seton                                        6399
021812060519     C                   MOVEL     *ALL'?'       VIDs13
021813060519     C                   Endif
021814060519     c                   else
021815060519     c                   if        §4oco7 <> ' '
021816060519     c                   seton                                        6399
021817060519     c                   endif
021818060519     C                   Endif
021819060519     c*
021820060519     C                   move      §4OCC8        $KCC
021821060519     C                   move      §4OSC8        $KSC
021822060519     C* Richiamo modulo controllo conto x descrizione
021823060519     C                   EXSR      CALLMVC002
021824060519     C*
021825060519     C                   If        $Esito =*off
021826060519     C                   eval      vids15         = Descr002
021827060519     c                   else
021828060519     c                   seton                                        6599
021829060519     C                   MOVEL     *ALL'?'       VIDs15
021830060519     C                   Endif
021900900608      *
022000911115     C                   MOVEL     §4OCO1        COD001                                     ER.
022100020726     C     KEY1          CHAIN     ANOPE01L                           57
022200020726     C  N57              MOVEL     OPEDESBREV    VIDDS7
022300911115     C   57              SETON                                        99
022400900608      *
022500911115     C                   MOVEL     §4OCO2        COD001                                     ER.
022600020726     C     KEY1          CHAIN     ANOPE01L                           58
022700020726     C  N58              MOVEL     OPEDESBREV    VIDDS8
022800911115     C   58              SETON                                        99
022900911115      *
023000911115     C                   MOVEL     §4OCO3        COD001                                     ER.
023100020726     C     KEY1          CHAIN     ANOPE01L                           59
023200020726     C  N59              MOVEL     OPEDESBREV    VIDDS9
023300911115     C   59              SETON                                        99
023400911115      *
023500911115     C                   MOVEL     §4OCO4        COD001                                     ER.
023600020726     C     KEY1          CHAIN     ANOPE01L                           60
023700020726     C  N60              MOVEL     OPEDESBREV    VIDS10
023800911115     C   60              SETON                                        99
023801021126     C*
023802021126     C                   MOVEL     §4OCO5        COD001                                     ER.
023803021126     C     KEY1          CHAIN     ANOPE01L                           61
023804021126     C  N61              MOVEL     OPEDESBREV    VIDS11
023805021126     C   61              SETON                                        99
023806060519     C*
023807060519     C                   MOVEL     §4OCO6        COD001                                     ER.
023808060519     C     KEY1          CHAIN     ANOPE01L                           62
023809060519     C  N62              MOVEL     OPEDESBREV    VIDS12
023810060519     C   62              SETON                                        99
023811060519     C*
023812060519     c                   if        §4oco7 <> ' '
023813060519     C                   MOVEL     §4OCO7        COD001                                     ER.
023814060519     C     KEY1          CHAIN     ANOPE01L                           64
023815060519     C  N64              MOVEL     OPEDESBREV    VIDS14
023816060519     C   64              SETON                                        99
023817060519     c                   else
023818060519     c                   if        §4occ7 <> ' ' or §4osc7 <> ' '
023819060519     c                   seton                                        6499
023820060519     c                   end
023821060519     c                   end
023822060519     C*
023823060519     C                   MOVEL     §4OCO8        COD001                                     ER.
023824060519     C     KEY1          CHAIN     ANOPE01L                           66
023825060519     C  N66              MOVEL     OPEDESBREV    VIDS16
023826060519     C   66              SETON                                        99
023827060519     C*
023828060519     C                   MOVEL     §4OCO9        COD001                                     ER.
023829060519     C     KEY1          CHAIN     ANOPE01L                           67
023830060519     C  N67              MOVEL     OPEDESBREV    VIDS17
023831060519     C   67              SETON                                        99
023900020726     C*
024000020726     C                   ENDSR
024100891030     C**----------------------------
024200891030     C** AGGIORNAMENTO / INSERIMENTO
024300891030     C**----------------------------
024400891030     C     SCRIVI        BEGSR
024500020726     C     KTAB          CHAIN     TNTBE01L                           80
024600020726     C                   MOVEL     §COD          TBECOD
024700020726     C                   MOVEL     §KE1          TBEKE1
024800020726     C                   MOVEL     §KE2          TBEKE2
024900020726     C                   MOVEL     DY4O          TBEUNI
025000020726     C* SPEDISCE SOLO SE SOCIETÀ 201
025100020726     C                   IF        xscsoc = '201'
025200020726     C                   MOVE      'S'           TBEFTT                          A TUTTI
025300020726     C                   MOVEL     TBEKE2        TBEFLT                          FILIALE
025400020726     C                   END
025500020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
025600020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
025700900420     C*§
025800020726     C   80              WRITE     TNTBE000                             98
025900020726     C  N80              UPDATE    TNTBE000                             98
026000020726     C*
026100891030     C                   ENDSR
026200020726     C******************************************************
026300020726     C     *INZSR        BEGSR
026400020726     C******************************************************
026500020726     C     *ENTRY        PLIST
026600020726     C                   PARM                    KPJBA
026700020726     C                   CALL      'X§PARUT'
026800020726     C                   PARM                    UT§DSE
026900020726     C                   MOVEL     RAGUT         RSUT
027000020726     C                   clear                   SocXsc
027100020726     C                   eval      tipxsc = 'SOC001'
027200020726     C                   exsr      rtvsocieta
027300020726     C                   if        rtnxsc = '1'
027400020726     C                   exsr      uscita
027500020726     C                   else
027600020726     C                   MOVEL     XSOCDS        xsoc001ds
027700020726     C                   endif
027800020726     C******************************************************
027900020726     C* POSIZIONAMENTO TABELLE                             *
028000020726     C     KTAB          KLIST                                                  *
028100020726     C                   KFLD                    §COD                           *
028200020726     C                   KFLD                    §KE1                           *
028300020726     C                   KFLD                    §KE2                           *
028400020726     C*                                                    *
028500020726     C     KEY1          KLIST                                                  *           STA
028600020726     C                   KFLD                    xscsoc                         *
028700020726     C                   KFLD                    COD001            4            *
028800020726     C*                                                    *
028900020726     C     Kuni          KLIST
029000020726     C                   KFLD                    xscsoc
029100020726     C                   KFLD                    UNIUNITA
029200020726     C******************************************************
029300020726     C     *LIKE         DEFINE    TBECOD        §COD
029400020726     C     *LIKE         DEFINE    TBEKE1        §KE1
029500020726     C     *LIKE         DEFINE    TBEKE2        §KE2
029600020726     C******************************************************
029700020726     C                   SETOFF                                       79
029800020726     C                   MOVEL     KNMUS         PROF              3
029900020726     C     PROF          IFNE      'EDP'
030000020726     C                   SETON                                        79
030100020726     C                   END
030200020726     C                   MOVEL     'Y4O'         §COD
030300020726     C                   ENDSR
030400020726     C*---------------------------------------------------------------------
030500020726      /TITLE Reperisco società.
030600020726     C     rtvsocieta    begsr
030700020726     C*
030800020726     C                   CALLb     'XSOC'
030900020726   >>C                   Parm                    TipXsc            6
031000020726   >>C                   Parm                    SocXsc            3
031100020726   >>C                   Parm                    CdsXsc            9 0
031200020726   >>C                   Parm                    ModXsc            3
031300020726   >>C                   Parm      *BLANK        RtnXsc            1
031400020726     C                   Parm                    XsocDS
031500020726   >>C                   Parm                    Kpjba
031600020726     C*
031700020726     C                   endsr
031800020726      *-------------------------------------------------------------------
031900020726      /TITLE Uscita.
032000020726     C     uscita        begsr
032100020726     C*
032200020726     C                   seton                                        lr
032300020726     C*
032400020726     C                   return
032500020726     C*
032600020726     C                   endsr
032700020726     C*----------------------------------------------------------
032800020726     C* Chiamata NDMVC002
032900020726     C*----------------------------------------------------------
033000020726     C     CallMvc002    BEGSR
033100020726     C*
033200020726     C* Controllo conto
033300020726     C                   Clear                   ND002DS
033400020726     C                   EVAL      $LenOut = %Size(ND002DS)
033500020726     C                   CallB     'NDMVC002'
033600020726     C                   PARM                    xscsoc
033700020726     C                   PARM                    $kcc
033800020726     C                   PARM                    $ksc
033900020726     C                   PARM                    Wdate
034000020726     C                   PARM      *OFF          $GesMsg
034100020726     C                   PARM      *OFF          $Esito
034200020726     C                   PARM      'ND002DS'     $Struttura
034300020726     C                   PARM                    ND002DS
034400020726     C                   PARM                    $LenOut
034500020726     C*
034600020726     C                   ENDSR
