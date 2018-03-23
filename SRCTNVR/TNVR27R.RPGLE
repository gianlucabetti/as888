000100150525     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND')
000200150525     H ACTGRP('QILE')
000300020726     H decedit(',') datedit(*dmy)
000400891030     F*--------------------------------------------------------------*
000500020726     F* GESTIONE TABELLA Y4O
000600891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000700891030     F*  01        INSERIMENTO                                       *
000800891030     F*  02        VARIAZIONE                                        *
000900020726     F*  03        annullamento                                      *
001000891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001100891030     F*--------------------------------------------------------------*
001200150525     FTnvr27D   CF   E             WORKSTN
001300020726     Fanuni01l  IF   E           K DISK
001400150525     FTntbe01l  UF   E           K DISK    usropn
001500891030     D*--------------------------------------------------------------*
001600891030     D*                        I N P U T                             *
001700891030     D*--------------------------------------------------------------*
001800020726     D TIBS02DS      E DS
001900060519     D Dy4O          E DS                  inz
002000891030     D KPJBA         E DS
002100020726     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200020726     D                SDS
002300020726     D  VTCPGM                 1     10
002400020726     D* Dati di ambiente ottenuti da XSOC
002500020726    >D xsoc001ds     E DS
002600020726     D* DS Interna per dati di output di XSOC
002700020726     D XSOCDS          DS          1000
002800020726     D* Descrizione conti
002900020726     D ND002DS       E DS
003000020726     D*-------------
003100020726     D* Definizioni parametri standard chiamate moduli
003200020726     D $Esito          S              1
003300020726     D $gesmsg         S              1
003400020726     D $lenout         S              9B 0
003500020726     D $struttura      S             10
003600020726     D $kcc            S              6
003700020726     D $ksc            S              8
003800020726     D Wdate           S               D
003900971210     C******************************************************
004000020726     C                   DO        *HIVAL
004100150525      *
004200891107     C                   MOVEL     *BLANKS       VCOD
004300891107     C                   MOVEL     '?'           VCOD
004400150525      *
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
005900150525     c                   goto      INIZIO
006000020726     C                   ELSE
006100020726     C*
006200060519     c                   if        %subst(tbeuni: 100: 1)=' '
006300060519     c                   eval      %subst(tbeuni: 100: 1)='0'
006400060519     c                   end
006500150525     C*
006600020726     C                   MOVEL     TBEUNI        DY4O
006700020726     C* RECORD ANNULLATO
006800020726     C     TBEATB        IFNE      ' '
006900020726     C                   SETON                                        03
007000150525     c                   goto      INIZIO
007100020726     C                   ELSE
007200020726     C* MANUTENZIONE
007300020726     C                   SETON                                        02        *VARIAZIONE
007400020726     C                   END
007500020726     C                   END
007600020726     C* 2° VIDEO
007700891030     C     SU            TAG
007800911115     C                   EXFMT     TB31D2
007900911115     C   KC              GOTO      FINE
008000911213     C   KL              GOTO      INIZIO
008100900608      *
008200891102     C                   EXSR      CTRFMT
008300900608     C   99
008400020726     CORNKF              GOTO      SU
008500150525      *
008600150525     C                   EXSR      aggiorna_sede
008700150525      *
008800150525     C                   EXSR      aggiorna_fil
008900911119     C**
009000020726     C                   ENDDO
009100891030     C     FINE          TAG
009200891030     C                   SETON                                        LR
009300891030     C**------------------------
009400020726     C** CONTROLLI 1° VIDEO
009500891030     C**------------------------
009600020726     C     CTR01         BEGSR
009700020726     C*
009800020726     C                   SETOFF                                       99
009900020726     C*§  RICERCA CODICE
010000020726     C     VCOD          IFEQ      '?'
010100020726     c                   clear                   TIBS02DS
010200020726     c                   movel     'R'           t02mod
010300020726     c                   movel     knsif         t02sif
010400020726     c                   movel     xscsoc        t02ke1
010500020726     C                   movel     'Y4O'         t02cod
010600020726     C                   CALL      'TIBS02R'
010700020726     c                   parm                    KPJBA
010800020726     c                   parm                    TIBS02DS
010900020726      *
011000020726    2C                   IF        T02err = *blanks and
011100020726     c                             t02ke1 = xscsoc
011200020726     C                   movel     T02ke2        VCOD
011300020726     C                   ENDIF
011400020726     C                   ENDIF
011500020726     C* CAMPI OBBLIGATORI
011600020726     C     VCOD          IFEQ      *BLANKS
011700020726     C     VCOD          OREQ      *ZEROS
011800020726     C                   SETON                                        2999
011900020726     C                   END
012000020726     C** IL CODICE società + filiale deve esistere
012100020726     C                   MOVEL(p)  VCOD          uniunita
012200020726     C                   MOVEL     *BLANKS       DESFIL
012300020726     c     kuni          chain     anuni01l                           29
012400020726     C  N29              MOVEL     UNIDESBREV    DESFIL
012500020726     C   29              SETON                                        99
012600020726     C*
012700020726     C                   ENDSR
012800020726     C**------------------------
012900020726     C** PULISCI CAMPI VIDEO
013000020726     C**------------------------
013100020726     C     PULVID        BEGSR
013200900608      *
013300150313     C                   MOVEL     *BLANKS       §4OCCB
013400150313     C                   MOVEL     *BLANKS       §4OSCB
013500150525     C                   MOVEL     *BLANKS       VIDS18
013600020726     C                   MOVEL(p)  xscsoc        §ke1
013700020726     C                   MOVEL(p)  vCOD          §ke2
013800900608      *
013900891030     C                   ENDSR
014000891030     C**------------------------
014100891030     C** CONTROLLI 2 VIDEATA
014200891030     C**------------------------
014300891030     C     CTRFMT        BEGSR
014400900608      *
014500020726     C                   SETOFF                                       99
014600020726     C                   MOVEL     '0000000000'  *IN(51)
014700060519     C                   MOVEL     '0000000'     *IN(61)
014800900608      *
014900150313     c                   if        §4occb <> ' ' or §4oscb <> ' '
015000150313     C                   move      §4OCCb        $KCC
015100150313     C                   move      §4OSCb        $KSC
015200150313     C* Richiamo modulo controllo conto x descrizione
015300150313     C                   EXSR      CALLMVC002
015400150313     C*
015500150313     C                   If        $Esito =*off
015600150313     C                   eval      vids18         = Descr002
015700150313     c                   else
015800150313     c                   seton                                        6899
015900150313     C                   MOVEL     *ALL'?'       VIDs18
016000150313     C                   Endif
016100150313     c                   else
016200150313     c                   if        §4occb <> ' '
016300150313     c                   seton                                        6899
016400150313     c                   endif
016500150313     C                   Endif
016600150525      *
016700020726     C                   ENDSR
016800891030     C**----------------------------
016900150525     C** AGGIORNA
017000891030     C**----------------------------
017100150525     C     aggiorna_Sede BEGSR
017200150525      *
017300150525     C     KTAB          CHAIN     TNTBE01L
017400150525     c                   if        %Found(TNTBE01L)
017500020726     C                   MOVEL     DY4O          TBEUNI
017600150525     C                   UPDATE    TNTBE000                             98
017700150525     c                   end
017800020726     C*
017900150525     c                   close     TNTBE01L
018000150525     c                   eval      cMD = 'DLTOVR FILE(*ALL)'
018100150525     c                   call      'QCMDEXC'
018200150525     c                   parm                    cMD              80
018300150525     c                   parm      18            lung             15 5
018400150525      *
018500891030     C                   ENDSR
018600150525     C*-------------------------------------------------
018700150525     C     aggiorna_Fil  BEGSR
018800150525      *
018900150525      * prima punta a sede tabelle di gruppo di
019000150525     c                   eval      cMD = 'OVRDBF FILE(TNTBE01L) -
019100150525     c                             TOFILE(FILTRAGRPF/TNTBE01L)'
019200150525     c                   call      'QCMDEXC'
019300150525     c                   parm                    cMD              80
019400150525     c                   parm      50            lung             15 5
019500150525      *
019600150525     c                   open      TNTBE01L
019700150525      *
019800150525     C     KTAB          CHAIN     TNTBE01L
019900150525     c                   if        %Found(TNTBE01L)
020000150525     C                   MOVEL     DY4O          TBEUNI
020100150525     C                   UPDATE    TNTBE000                             98
020200150525     c                   end
020300150525     C*
020400150525     c                   close     TNTBE01L
020500150525     c                   eval      cMD = 'DLTOVR FILE(*ALL)'
020600150525     c                   call      'QCMDEXC'
020700150525     c                   parm                    cMD              80
020800150525     c                   parm      18            lung             15 5
020900150525      *
021000150525     C                   ENDSR
021100020726     C******************************************************
021200020726     C     *INZSR        BEGSR
021300020726     C******************************************************
021400020726     C     *ENTRY        PLIST
021500020726     C                   PARM                    KPJBA
021600150525      *
021700150525      * prima punta a sede tabelle di gruppo di
021800150525     c                   eval      cMD = 'OVRDBF FILE(TNTBE01L) -
021900150525     c                             TOFILE(GAITRAGRPS/TNTBE01L)'
022000150525     c                   call      'QCMDEXC'
022100150525     c                   parm                    cMD              80
022200150525     c                   parm      50            lung             15 5
022300150525      *
022400150525     c                   open      TNTBE01L
022500150525      *
022600020726     C                   CALL      'X§PARUT'
022700020726     C                   PARM                    UT§DSE
022800020726     C                   MOVEL     RAGUT         RSUT
022900150525      *
023000020726     C                   clear                   SocXsc
023100020726     C                   eval      tipxsc = 'SOC001'
023200020726     C                   exsr      rtvsocieta
023300020726     C                   if        rtnxsc = '1'
023400020726     C                   exsr      uscita
023500020726     C                   else
023600020726     C                   MOVEL     XSOCDS        xsoc001ds
023700020726     C                   endif
023800150525      *
023900020726     C******************************************************
024000020726     C* POSIZIONAMENTO TABELLE                             *
024100020726     C     KTAB          KLIST                                                  *
024200020726     C                   KFLD                    §COD                           *
024300020726     C                   KFLD                    §KE1                           *
024400020726     C                   KFLD                    §KE2                           *
024500020726     C*                                                    *
024600020726     C     KEY1          KLIST                                                  *           STA
024700020726     C                   KFLD                    xscsoc                         *
024800020726     C                   KFLD                    COD001            4            *
024900020726     C*                                                    *
025000020726     C     Kuni          KLIST
025100020726     C                   KFLD                    xscsoc
025200020726     C                   KFLD                    UNIUNITA
025300020726     C******************************************************
025400020726     C     *LIKE         DEFINE    TBECOD        §COD
025500020726     C     *LIKE         DEFINE    TBEKE1        §KE1
025600020726     C     *LIKE         DEFINE    TBEKE2        §KE2
025700020726     C******************************************************
025800020726     C                   SETOFF                                       79
025900020726     C                   MOVEL     KNMUS         PROF              3
026000150525      *
026100020726     C     PROF          IFNE      'EDP'
026200020726     C                   SETON                                        79
026300020726     C                   END
026400020726     C                   MOVEL     'Y4O'         §COD
026500150525      *
026600020726     C                   ENDSR
026700020726     C*---------------------------------------------------------------------
026800020726      /TITLE Reperisco società.
026900020726     C     rtvsocieta    begsr
027000020726     C*
027100020726     C                   CALLb     'XSOC'
027200020726   >>C                   Parm                    TipXsc            6
027300020726   >>C                   Parm                    SocXsc            3
027400020726   >>C                   Parm                    CdsXsc            9 0
027500020726   >>C                   Parm                    ModXsc            3
027600020726   >>C                   Parm      *BLANK        RtnXsc            1
027700020726     C                   Parm                    XsocDS
027800020726   >>C                   Parm                    Kpjba
027900020726     C*
028000020726     C                   endsr
028100020726      *-------------------------------------------------------------------
028200020726      /TITLE Uscita.
028300020726     C     uscita        begsr
028400020726     C*
028500020726     C                   seton                                        lr
028600020726     C                   return
028700020726     C*
028800020726     C                   endsr
028900020726     C*----------------------------------------------------------
029000020726     C* Chiamata NDMVC002
029100020726     C*----------------------------------------------------------
029200020726     C     CallMvc002    BEGSR
029300020726     C*
029400020726     C* Controllo conto
029500020726     C                   Clear                   ND002DS
029600020726     C                   EVAL      $LenOut = %Size(ND002DS)
029700020726     C                   CallB     'NDMVC002'
029800020726     C                   PARM                    xscsoc
029900020726     C                   PARM                    $kcc
030000020726     C                   PARM                    $ksc
030100020726     C                   PARM                    Wdate
030200020726     C                   PARM      *OFF          $GesMsg
030300020726     C                   PARM      *OFF          $Esito
030400020726     C                   PARM      'ND002DS'     $Struttura
030500020726     C                   PARM                    ND002DS
030600020726     C                   PARM                    $LenOut
030700020726     C*
030800020726     C                   ENDSR
030900150525     C*----------------------------------------------------------
