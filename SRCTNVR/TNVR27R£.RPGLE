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
006100060519     c                   if        %subst(tbeuni: 100: 1)=' '
006200060519     c                   eval      %subst(tbeuni: 100: 1)='0'
006300060519     c                   end
006400020726     C                   MOVEL     TBEUNI        DY4O
006500020726     C* RECORD ANNULLATO
006600020726     C     TBEATB        IFNE      ' '
006700020726     C                   SETON                                        03
006800020726     C                   ELSE
006900020726     C* MANUTENZIONE
007000020726     C                   SETON                                        02        *VARIAZIONE
007100020726     C                   END
007200020726     C                   END
007300020726     C* 2° VIDEO
007400891030     C     SU            TAG
007500911115     C                   EXFMT     TB31D2
007600911115     C   KC              GOTO      FINE
007700911213     C   KL              GOTO      INIZIO
007800891030     C**--- ANNULLAMENTO
007900911115     C   KQ              DO
008000020726     C                   MOVEL     'A'           TBEATB
008100020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
008200020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
008300020726     C                   UPDATE    Tntbe000
008400891030     C                   GOTO      INIZIO
008500891030     C                   END
008600020726     C** RIPRISTINO
008700020726     C   KE              DO
008800020726     C                   MOVE      *BLANKS       TBEATB
008900020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
009000020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
009100020726     C                   UPDATE    Tntbe000
009200020726     C                   GOTO      INIZIO
009300020726     C                   END
009400900608      *
009500891102     C                   EXSR      CTRFMT
009600900608     C   99
009700020726     CORNKF              GOTO      SU
009800891030     C                   EXSR      SCRIVI
009900911119     C**
010000020726     C                   ENDDO
010100891030     C     FINE          TAG
010200891030     C                   SETON                                        LR
010300891030     C**------------------------
010400020726     C** CONTROLLI 1° VIDEO
010500891030     C**------------------------
010600020726     C     CTR01         BEGSR
010700020726     C*
010800020726     C                   SETOFF                                       99
010900020726     C*§  RICERCA CODICE
011000020726     C     VCOD          IFEQ      '?'
011100020726     c                   clear                   TIBS02DS
011200020726     c                   movel     'R'           t02mod
011300020726     c                   movel     knsif         t02sif
011400020726     c                   movel     xscsoc        t02ke1
011500020726     C                   movel     'Y4O'         t02cod
011600020726     C                   CALL      'TIBS02R'
011700020726     c                   parm                    KPJBA
011800020726     c                   parm                    TIBS02DS
011900020726      *
012000020726    2C                   IF        T02err = *blanks and
012100020726     c                             t02ke1 = xscsoc
012200020726     C                   movel     T02ke2        VCOD
012300020726     C                   ENDIF
012400020726     C                   ENDIF
012500020726     C* CAMPI OBBLIGATORI
012600020726     C     VCOD          IFEQ      *BLANKS
012700020726     C     VCOD          OREQ      *ZEROS
012800020726     C                   SETON                                        2999
012900020726     C                   END
013000020726     C** IL CODICE società + filiale deve esistere
013100020726     C                   MOVEL(p)  VCOD          uniunita
013200020726     C                   MOVEL     *BLANKS       DESFIL
013300020726     c     kuni          chain     anuni01l                           29
013400020726     C  N29              MOVEL     UNIDESBREV    DESFIL
013500020726     C   29              SETON                                        99
013600020726     C*
013700020726     C                   ENDSR
013800020726     C**------------------------
013900020726     C** PULISCI CAMPI VIDEO
014000020726     C**------------------------
014100020726     C     PULVID        BEGSR
014200900608      *
014300911115     C                   MOVEL     *BLANKS       §4OCO1
014400911115     C                   MOVEL     *BLANKS       §4OCO2
014500911115     C                   MOVEL     *BLANKS       §4OCO3
014600911115     C                   MOVEL     *BLANKS       §4OCO4
014700021126     C                   MOVEL     *BLANKS       §4OCO5
014800060519     C                   MOVEL     *BLANKS       §4OCO6
014900060519     C                   MOVEL     *BLANKS       §4OCO7
015000060519     C                   MOVEL     *BLANKS       §4OCO8
015100060519     C                   MOVEL     *BLANKS       §4OCO9
015200020726     C                   MOVEL     *BLANKS       §4OCC1
015300020726     C                   MOVEL     *BLANKS       §4OSC1
015400020726     C                   MOVEL     *BLANKS       §4OCC2
015500020726     C                   MOVEL     *BLANKS       §4OSC2
015600020726     C                   MOVEL     *BLANKS       §4OCC3
015700020726     C                   MOVEL     *BLANKS       §4OSC3
015800020726     C                   MOVEL     *BLANKS       §4OCC4
015900020726     C                   MOVEL     *BLANKS       §4OSC4
016000060519     C                   MOVEL     *BLANKS       §4OCC7
016100060519     C                   MOVEL     *BLANKS       §4OSC7
016200150313     C                   MOVEL     *BLANKS       §4OCCB
016300150313     C                   MOVEL     *BLANKS       §4OSCB
016400060519     C                   MOVEL     *BLANKS       §4OCC8
016500060519     C                   MOVEL     *BLANKS       §4OSC8
016600060519     C                   MOVEL     *BLANKS       §4Odom
016700060519     C                   MOVEL     *zeros        §4Odog
016800900608     C                   MOVEL     *BLANKS       VIDDS1
016900900608     C                   MOVEL     *BLANKS       VIDDS2
017000900608     C                   MOVEL     *BLANKS       VIDDS3
017100911115     C                   MOVEL     *BLANKS       VIDDS4
017200020726     C                   MOVEL     *BLANKS       VIDDS7
017300020726     C                   MOVEL     *BLANKS       VIDDS8
017400020726     C                   MOVEL     *BLANKS       VIDDS9
017500911115     C                   MOVEL     *BLANKS       VIDS10
017600021126     C                   MOVEL     *BLANKS       VIDS11
017700060519     C                   MOVEL     *BLANKS       VIDS12
017800060519     C                   MOVEL     *BLANKS       VIDS13
017900060519     C                   MOVEL     *BLANKS       VIDS14
018000060519     C                   MOVEL     *BLANKS       VIDS15
018100060519     C                   MOVEL     *BLANKS       VIDS16
018200060519     C                   MOVEL     *BLANKS       VIDS17
018300020726     C                   MOVEL(p)  xscsoc        §ke1
018400020726     C                   MOVEL(p)  vCOD          §ke2
018500900608      *
018600891030     C                   ENDSR
018700891030     C**------------------------
018800891030     C** CONTROLLI 2 VIDEATA
018900891030     C**------------------------
019000891030     C     CTRFMT        BEGSR
019100900608      *
019200020726     C                   SETOFF                                       99
019300020726     C                   MOVEL     '0000000000'  *IN(51)
019400060519     C                   MOVEL     '0000000'     *IN(61)
019500900608      *
019600020726     C                   move      §4OCC1        $KCC
019700020726     C                   move      §4OSC1        $KSC
019800020726     C* Richiamo modulo controllo conto x descrizione
019900020726     C                   EXSR      CALLMVC002
020000020726     C*
020100020726     C                   If        $Esito =*off
020200020726     C                   eval      vidds1         = Descr002
020300020726     c                   else
020400020726     c                   seton                                        5199
020500020726     C                   MOVEL     *ALL'?'       VIDDS1
020600020726     C                   Endif
020700900608      *
020800020726     C                   move      §4OCC2        $KCC
020900020726     C                   move      §4OSC2        $KSC
021000020726     C* Richiamo modulo controllo conto x descrizione
021100020726     C                   EXSR      CALLMVC002
021200020726     C*
021300020726     C                   If        $Esito =*off
021400020726     C                   eval      vidds2         = Descr002
021500020726     c                   else
021600020726     c                   seton                                        5299
021700020726     C                   MOVEL     *ALL'?'       VIDDS2
021800020726     C                   Endif
021900900608      *
022000020726     C                   move      §4OCC3        $KCC
022100020726     C                   move      §4OSC3        $KSC
022200020726     C* Richiamo modulo controllo conto x descrizione
022300020726     C                   EXSR      CALLMVC002
022400020726     C*
022500020726     C                   If        $Esito =*off
022600020726     C                   eval      vidds3         = Descr002
022700020726     c                   else
022800020726     c                   seton                                        5399
022900020726     C                   MOVEL     *ALL'?'       VIDDS3
023000020726     C                   Endif
023100911115      *
023200020726     C                   move      §4OCC4        $KCC
023300020726     C                   move      §4OSC4        $KSC
023400020726     C* Richiamo modulo controllo conto x descrizione
023500020726     C                   EXSR      CALLMVC002
023600020726     C*
023700020726     C                   If        $Esito =*off
023800020726     C                   eval      vidds4         = Descr002
023900020726     c                   else
024000020726     c                   seton                                        5499
024100020726     C                   MOVEL     *ALL'?'       VIDDS4
024200060519     c                   Endif
024300060519     c*
024400060519     c                   if        §4occ7 <> ' ' or §4osc7 <> ' '
024500060519     C                   move      §4OCC7        $KCC
024600060519     C                   move      §4OSC7        $KSC
024700060519     C* Richiamo modulo controllo conto x descrizione
024800060519     C                   EXSR      CALLMVC002
024900060519     C*
025000060519     C                   If        $Esito =*off
025100060519     C                   eval      vids13         = Descr002
025200060519     c                   else
025300060519     c                   seton                                        6399
025400060519     C                   MOVEL     *ALL'?'       VIDs13
025500060519     C                   Endif
025600060519     c                   else
025700060519     c                   if        §4oco7 <> ' '
025800060519     c                   seton                                        6399
025900060519     c                   endif
026000060519     C                   Endif
026100060519     c*
026200060519     C                   move      §4OCC8        $KCC
026300060519     C                   move      §4OSC8        $KSC
026400060519     C* Richiamo modulo controllo conto x descrizione
026500060519     C                   EXSR      CALLMVC002
026600060519     C*
026700060519     C                   If        $Esito =*off
026800060519     C                   eval      vids15         = Descr002
026900060519     c                   else
027000060519     c                   seton                                        6599
027100060519     C                   MOVEL     *ALL'?'       VIDs15
027200060519     C                   Endif
027300900608      *
027400911115     C                   MOVEL     §4OCO1        COD001                                     ER.
027500020726     C     KEY1          CHAIN     ANOPE01L                           57
027600020726     C  N57              MOVEL     OPEDESBREV    VIDDS7
027700911115     C   57              SETON                                        99
027800900608      *
027900911115     C                   MOVEL     §4OCO2        COD001                                     ER.
028000020726     C     KEY1          CHAIN     ANOPE01L                           58
028100020726     C  N58              MOVEL     OPEDESBREV    VIDDS8
028200911115     C   58              SETON                                        99
028300150313     c*
028400150313     c                   if        §4occb <> ' ' or §4oscb <> ' '
028500150313     C                   move      §4OCCb        $KCC
028600150313     C                   move      §4OSCb        $KSC
028700150313     C* Richiamo modulo controllo conto x descrizione
028800150313     C                   EXSR      CALLMVC002
028900150313     C*
029000150313     C                   If        $Esito =*off
029100150313     C                   eval      vids18         = Descr002
029200150313     c                   else
029300150313     c                   seton                                        6899
029400150313     C                   MOVEL     *ALL'?'       VIDs18
029500150313     C                   Endif
029600150313     c                   else
029700150313     c                   if        §4occb <> ' '
029800150313     c                   seton                                        6899
029900150313     c                   endif
030000150313     C                   Endif
030100150313     c*
030200911115      *
030300911115     C                   MOVEL     §4OCO3        COD001                                     ER.
030400020726     C     KEY1          CHAIN     ANOPE01L                           59
030500020726     C  N59              MOVEL     OPEDESBREV    VIDDS9
030600911115     C   59              SETON                                        99
030700911115      *
030800911115     C                   MOVEL     §4OCO4        COD001                                     ER.
030900020726     C     KEY1          CHAIN     ANOPE01L                           60
031000020726     C  N60              MOVEL     OPEDESBREV    VIDS10
031100911115     C   60              SETON                                        99
031200021126     C*
031300021126     C                   MOVEL     §4OCO5        COD001                                     ER.
031400021126     C     KEY1          CHAIN     ANOPE01L                           61
031500021126     C  N61              MOVEL     OPEDESBREV    VIDS11
031600021126     C   61              SETON                                        99
031700060519     C*
031800060519     C                   MOVEL     §4OCO6        COD001                                     ER.
031900060519     C     KEY1          CHAIN     ANOPE01L                           62
032000060519     C  N62              MOVEL     OPEDESBREV    VIDS12
032100060519     C   62              SETON                                        99
032200060519     C*
032300060519     c                   if        §4oco7 <> ' '
032400060519     C                   MOVEL     §4OCO7        COD001                                     ER.
032500060519     C     KEY1          CHAIN     ANOPE01L                           64
032600060519     C  N64              MOVEL     OPEDESBREV    VIDS14
032700060519     C   64              SETON                                        99
032800060519     c                   else
032900060519     c                   if        §4occ7 <> ' ' or §4osc7 <> ' '
033000060519     c                   seton                                        6499
033100060519     c                   end
033200060519     c                   end
033300060519     C*
033400060519     C                   MOVEL     §4OCO8        COD001                                     ER.
033500060519     C     KEY1          CHAIN     ANOPE01L                           66
033600060519     C  N66              MOVEL     OPEDESBREV    VIDS16
033700060519     C   66              SETON                                        99
033800060519     C*
033900060519     C                   MOVEL     §4OCO9        COD001                                     ER.
034000060519     C     KEY1          CHAIN     ANOPE01L                           67
034100060519     C  N67              MOVEL     OPEDESBREV    VIDS17
034200060519     C   67              SETON                                        99
034300020726     C*
034400020726     C                   ENDSR
034500891030     C**----------------------------
034600891030     C** AGGIORNAMENTO / INSERIMENTO
034700891030     C**----------------------------
034800891030     C     SCRIVI        BEGSR
034900020726     C     KTAB          CHAIN     TNTBE01L                           80
035000020726     C                   MOVEL     §COD          TBECOD
035100020726     C                   MOVEL     §KE1          TBEKE1
035200020726     C                   MOVEL     §KE2          TBEKE2
035300020726     C                   MOVEL     DY4O          TBEUNI
035400020726     C* SPEDISCE SOLO SE SOCIETÀ 201
035500020726     C                   IF        xscsoc = '201'
035600020726     C                   MOVE      'S'           TBEFTT                          A TUTTI
035700020726     C                   MOVEL     TBEKE2        TBEFLT                          FILIALE
035800020726     C                   END
035900020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
036000020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
036100900420     C*§
036200020726     C   80              WRITE     TNTBE000                             98
036300020726     C  N80              UPDATE    TNTBE000                             98
036400020726     C*
036500891030     C                   ENDSR
036600020726     C******************************************************
036700020726     C     *INZSR        BEGSR
036800020726     C******************************************************
036900020726     C     *ENTRY        PLIST
037000020726     C                   PARM                    KPJBA
037100020726     C                   CALL      'X§PARUT'
037200020726     C                   PARM                    UT§DSE
037300020726     C                   MOVEL     RAGUT         RSUT
037400020726     C                   clear                   SocXsc
037500020726     C                   eval      tipxsc = 'SOC001'
037600020726     C                   exsr      rtvsocieta
037700020726     C                   if        rtnxsc = '1'
037800020726     C                   exsr      uscita
037900020726     C                   else
038000020726     C                   MOVEL     XSOCDS        xsoc001ds
038100020726     C                   endif
038200020726     C******************************************************
038300020726     C* POSIZIONAMENTO TABELLE                             *
038400020726     C     KTAB          KLIST                                                  *
038500020726     C                   KFLD                    §COD                           *
038600020726     C                   KFLD                    §KE1                           *
038700020726     C                   KFLD                    §KE2                           *
038800020726     C*                                                    *
038900020726     C     KEY1          KLIST                                                  *           STA
039000020726     C                   KFLD                    xscsoc                         *
039100020726     C                   KFLD                    COD001            4            *
039200020726     C*                                                    *
039300020726     C     Kuni          KLIST
039400020726     C                   KFLD                    xscsoc
039500020726     C                   KFLD                    UNIUNITA
039600020726     C******************************************************
039700020726     C     *LIKE         DEFINE    TBECOD        §COD
039800020726     C     *LIKE         DEFINE    TBEKE1        §KE1
039900020726     C     *LIKE         DEFINE    TBEKE2        §KE2
040000020726     C******************************************************
040100020726     C                   SETOFF                                       79
040200020726     C                   MOVEL     KNMUS         PROF              3
040300020726     C     PROF          IFNE      'EDP'
040400020726     C                   SETON                                        79
040500020726     C                   END
040600020726     C                   MOVEL     'Y4O'         §COD
040700020726     C                   ENDSR
040800020726     C*---------------------------------------------------------------------
040900020726      /TITLE Reperisco società.
041000020726     C     rtvsocieta    begsr
041100020726     C*
041200020726     C                   CALLb     'XSOC'
041300020726   >>C                   Parm                    TipXsc            6
041400020726   >>C                   Parm                    SocXsc            3
041500020726   >>C                   Parm                    CdsXsc            9 0
041600020726   >>C                   Parm                    ModXsc            3
041700020726   >>C                   Parm      *BLANK        RtnXsc            1
041800020726     C                   Parm                    XsocDS
041900020726   >>C                   Parm                    Kpjba
042000020726     C*
042100020726     C                   endsr
042200020726      *-------------------------------------------------------------------
042300020726      /TITLE Uscita.
042400020726     C     uscita        begsr
042500020726     C*
042600020726     C                   seton                                        lr
042700020726     C*
042800020726     C                   return
042900020726     C*
043000020726     C                   endsr
043100020726     C*----------------------------------------------------------
043200020726     C* Chiamata NDMVC002
043300020726     C*----------------------------------------------------------
043400020726     C     CallMvc002    BEGSR
043500020726     C*
043600020726     C* Controllo conto
043700020726     C                   Clear                   ND002DS
043800020726     C                   EVAL      $LenOut = %Size(ND002DS)
043900020726     C                   CallB     'NDMVC002'
044000020726     C                   PARM                    xscsoc
044100020726     C                   PARM                    $kcc
044200020726     C                   PARM                    $ksc
044300020726     C                   PARM                    Wdate
044400020726     C                   PARM      *OFF          $GesMsg
044500020726     C                   PARM      *OFF          $Esito
044600020726     C                   PARM      'ND002DS'     $Struttura
044700020726     C                   PARM                    ND002DS
044800020726     C                   PARM                    $LenOut
044900020726     C*
045000020726     C                   ENDSR
