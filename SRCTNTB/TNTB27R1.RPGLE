000100150525     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND')
000200150525     H ACTGRP('QILE')
000300020726     H decedit(',') datedit(*dmy)
000400891030     F*--------------------------------------------------------------*
000500150525     F* aggiorna della tab.Y4O  solo la BANCA
000600891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000700891030     F*  01        INSERIMENTO                                       *
000800891030     F*  02        VARIAZIONE                                        *
000900020726     F*  03        annullamento                                      *
001000891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001100891030     F*--------------------------------------------------------------*
001200150525     FTNTB27D1  CF   E             WORKSTN
001300020726     Fanuni01l  IF   E           K DISK
001400150528     Fazorg01l  IF   E           K DISK
001500150525     FTntbe01l  UF   E           K DISK    usropn
001600891030     D*--------------------------------------------------------------*
001700891030     D*                        I N P U T                             *
001800891030     D*--------------------------------------------------------------*
001900020726     D TIBS02DS      E DS
002000150525     D TIBS55DS      E DS
002100060519     D Dy4O          E DS                  inz
002200891030     D KPJBA         E DS
002300150526     D UT§DSE0F      E DS
002400150525     D*--------------------------------------------------------------*
002500020726     D                SDS
002600020726     D  VTCPGM                 1     10
002700020726     D* Dati di ambiente ottenuti da XSOC
002800020726    >D xsoc001ds     E DS
002900020726     D* DS Interna per dati di output di XSOC
003000020726     D XSOCDS          DS          1000
003100020726     D* Descrizione conti
003200020726     D ND002DS       E DS
003300150525     D*--------------------------------------------------------------*
003400020726     D* Definizioni parametri standard chiamate moduli
003500020726     D $Esito          S              1
003600020726     D $gesmsg         S              1
003700020726     D $lenout         S              9B 0
003800020726     D $struttura      S             10
003900020726     D $kcc            S              6
004000020726     D $ksc            S              8
004100020726     D Wdate           S               D
004200150528     D digits          C                   CONST('0123456789')
004300971210     C******************************************************
004400020726     C                   DO        *HIVAL
004500150525      *
004600891107     C                   MOVEL     *BLANKS       VCOD
004700891107     C                   MOVEL     '?'           VCOD
004800150525      *
004900891030     C     INIZIO        TAG
005000911115     C                   EXFMT     TB31D1
005100020726     C   KC              LEAVE
005200020726     C* CONTROLLI 1° VIDEO
005300020726     C                   EXSR      CTR01
005400020726     C   99              GOTO      INIZIO
005500020726     C*
005600020726     C                   SETOFF                                       010203
005700020726     C                   EXSR      PULVID
005800020726     C*
005900020726     C     KTAB          CHAIN     Tntbe01L
006000020726     C* INSERIMENTO
006100020726     C                   IF        NOT %FOUND(TNTBE01L)
006200020726     C                   SETON                                        01        *IMMISSIONE
006300150525     c                   goto      INIZIO
006400020726     C                   ELSE
006500020726     C*
006600060519     c                   if        %subst(tbeuni: 100: 1)=' '
006700060519     c                   eval      %subst(tbeuni: 100: 1)='0'
006800060519     c                   end
006900150525     C*
007000020726     C                   MOVEL     TBEUNI        DY4O
007100020726     C* RECORD ANNULLATO
007200020726     C     TBEATB        IFNE      ' '
007300020726     C                   SETON                                        03
007400150525     c                   goto      INIZIO
007500020726     C                   ELSE
007600020726     C* MANUTENZIONE
007700020726     C                   SETON                                        02        *VARIAZIONE
007800020726     C                   END
007900020726     C                   END
008000020726     C* 2° VIDEO
008100891030     C     SU            TAG
008200911115     C                   EXFMT     TB31D2
008300911115     C   KC              GOTO      FINE
008400911213     C   KL              GOTO      INIZIO
008500900608      *
008600891102     C                   EXSR      CTRFMT
008700900608     C   99
008800020726     CORNKF              GOTO      SU
008900150525      *
009000150525     C                   EXSR      aggiorna_sede
009100150525      *
009200150525     C                   EXSR      aggiorna_fil
009300911119     C**
009400020726     C                   ENDDO
009500891030     C     FINE          TAG
009600891030     C                   SETON                                        LR
009700891030     C**------------------------
009800020726     C** CONTROLLI 1° VIDEO
009900891030     C**------------------------
010000020726     C     CTR01         BEGSR
010100020726     C*
010200020726     C                   SETOFF                                       99
010300020726     C*§  RICERCA CODICE
010400020726     C     VCOD          IFEQ      '?'
010500150526     c                   call      'TNSD24R'
010600150526     c                   parm      *blanks       §COD3             3
010700150526     c                   parm      *blanks       §TIP              1
010800150526     c                   parm      *blanks       §DES25           25
010900150526     c                   parm      *blanks       §PRA              3
011000150526     c                   movel     §COD3         VCOD
011100020726     C                   ENDIF
011200150528      ***
011300150528      * CAMPI OBBLIGATORI
011400020726     C     VCOD          IFEQ      *BLANKS
011500020726     C     VCOD          OREQ      *ZEROS
011600020726     C                   SETON                                        2999
011700150528      *
011800150528     C                   else
011900150528      *
012000150528     c                   clear                   dove              3 0
012100150528     c     digits        check     vCOD:1        dove
012200150528     c                   if        dove > 0
012300150528     C                   SETON                                        9929
012400150528     c                   goto      err_sr
012500150528     c                   else
012600150528      *
012700150528      * controlla la filiale che sia una filiale vera.
012800150528     c                   move      vcod          fil_03            3 0
012900150528     c     fil_03        chain     azorg01l
013000150528     c                   if        %Found(azorg01l) and orgfag = 'F'
013100150528     C                   MOVEL     orgdes        DESFIL
013200150528     c                   else
013300150528     C                   SETON                                        9929
013400150528     c   99              goto      err_sr
013500150528     c                   end
013600150528     c                   end
013700150528     c
013800150528     c
013900150528     c
014000150528     C                   END
014100150528      ***
014200020726     C** IL CODICE società + filiale deve esistere
014300020726     C                   MOVEL(p)  VCOD          uniunita
014400020726     C                   MOVEL     *BLANKS       DESFIL
014500150526     C                   MOVEL     '201'         xscsoc201
014600020726     c     kuni          chain     anuni01l                           29
014700020726     C  N29              MOVEL     UNIDESBREV    DESFIL
014800020726     C   29              SETON                                        99
014900150528     c   99              goto      err_sr
015000020726     C*
015100150528     C     err_sr        ENDSR
015200020726     C**------------------------
015300020726     C** PULISCI CAMPI VIDEO
015400020726     C**------------------------
015500020726     C     PULVID        BEGSR
015600150525     C*
015700150525     C                   CLEAR                   TIBS55ds
015800150525     C                   MOVEL     'L'           I50TLA
015900150528     C                   MOVEL     '001'         I50APO
016000150525     C                   MOVEL     '046'         I50PPO
016100150525     C                   CALL      'TIBS55R'
016200150525     C                   PARM                    TIBS55ds
016300150525      *
016400150525      * prima punta a sede tabelle di gruppo di
016500150525     c                   eval      cMD = 'OVRDBF FILE(TNTBE01L) TOFILE(' +
016600150525     c                             %trim(O50PLG) + '/TNTBE01L)'
016700150525     c                   call      'QCMDEXC'
016800150525     c                   parm                    cMD              80
016900150525     c                   parm      50            lung             15 5
017000150525      *
017100150525     c                   open      TNTBE01L
017200150525      *
017300900608      *
017400150313     C                   MOVEL     *BLANKS       §4OCCB
017500150313     C                   MOVEL     *BLANKS       §4OSCB
017600150525     C                   MOVEL     *BLANKS       VIDS18
017700150526     C                   MOVEL(p)  '201'         §ke1
017800020726     C                   MOVEL(p)  vCOD          §ke2
017900900608      *
018000891030     C                   ENDSR
018100891030     C**------------------------
018200891030     C** CONTROLLI 2 VIDEATA
018300891030     C**------------------------
018400891030     C     CTRFMT        BEGSR
018500900608      *
018600150526     C                   SETOFF                                       9968
018700150526      *
018800150526     c                   if        §4oscb = *all'0'
018900150526     C                   move      *blank        §4OCCb
019000150526     C                   move      *blank        §4OSCb
019100150526     C                   move      *blank        vids18
019200150526     c                   end
019300900608      *
019400150526     c                   if        §4oscb <> *blank
019500150526      *
019600150526      * capoconto Banche
019700150526     C                   move      '000201'      $KCC
019800150313     C                   move      §4OSCb        $KSC
019900150313     C* Richiamo modulo controllo conto x descrizione
020000150313     C                   EXSR      CALLMVC002
020100150313     C*
020200150313     C                   If        $Esito =*off
020300150313     C                   eval      vids18         = Descr002
020400150526     C                   move      '000201'      §4OCCb
020500150313     c                   else
020600150313     c                   seton                                        6899
020700150313     C                   MOVEL     *ALL'?'       VIDs18
020800150313     C                   Endif
020900150526      *
021000150313     c                   else
021100150526     C                   move      *blank        §4OCCb
021200150313     C                   Endif
021300150525      *
021400020726     C                   ENDSR
021500891030     C**----------------------------
021600150525     C** AGGIORNA
021700891030     C**----------------------------
021800150525     C     aggiorna_Sede BEGSR
021900150525      *
022000150526     c                   exsr      aggiorna_TBE
022100020726     C*
022200150525     c                   close     TNTBE01L
022300150525     c                   eval      cMD = 'DLTOVR FILE(*ALL)'
022400150525     c                   call      'QCMDEXC'
022500150525     c                   parm                    cMD              80
022600150525     c                   parm      18            lung             15 5
022700150525      *
022800891030     C                   ENDSR
022900150525     C*-------------------------------------------------
023000150525     C     aggiorna_Fil  BEGSR
023100150525      *
023200150525      * poi punta a tabelle di gruppo di filiale
023300150525     c                   eval      cMD = 'OVRDBF FILE(TNTBE01L) TOFILE(' +
023400150525     c                             %trim(O50ALG) + '/TNTBE01L)'
023500150525     c                   call      'QCMDEXC'
023600150525     c                   parm                    cMD              80
023700150525     c                   parm      50            lung             15 5
023800150525      *
023900150525     c                   open      TNTBE01L
024000150525      *
024100150526     c                   exsr      aggiorna_TBE
024200150525     C*
024300150525     c                   close     TNTBE01L
024400150525     c                   eval      cMD = 'DLTOVR FILE(*ALL)'
024500150525     c                   call      'QCMDEXC'
024600150525     c                   parm                    cMD              80
024700150525     c                   parm      18            lung             15 5
024800150525      *
024900150525     C                   ENDSR
025000020726     C******************************************************
025100150526     C     aggiorna_TBE  BEGSR
025200150526      *
025300150526     C     KTAB          CHAIN     TNTBE01L
025400150526     c                   if        %Found(TNTBE01L)
025500150526     C                   if          §4OSCB = *blank
025600150526     C                   MOVEL     *BLANKS       §4OCCB
025700150526     c                   else
025800150526     C                   MOVEL     '000201'      §4OCCB
025900150526     c                   end
026000150526     C                   MOVEL     DY4O          TBEUNI
026100150526     C                   UPDATE    TNTBE000                             98
026200150526     c                   end
026300150526      *
026400150526     C                   ENDSR
026500150526     C******************************************************
026600150526     C     *INZSR        BEGSR
026700020726     C******************************************************
026800020726     C     *ENTRY        PLIST
026900020726     C                   PARM                    KPJBA
027000150525      *
027100020726     C                   CALL      'X§PARUT'
027200150526     C                   PARM                    UT§DSE0F
027300020726     C                   MOVEL     RAGUT         RSUT
027400150525      *
027500020726     C                   clear                   SocXsc
027600020726     C                   eval      tipxsc = 'SOC001'
027700020726     C                   exsr      rtvsocieta
027800020726     C                   if        rtnxsc = '1'
027900020726     C                   exsr      uscita
028000020726     C                   else
028100020726     C                   MOVEL     XSOCDS        xsoc001ds
028200020726     C                   endif
028300150525      *
028400020726     C******************************************************
028500020726     C* POSIZIONAMENTO TABELLE                             *
028600020726     C     KTAB          KLIST                                                  *
028700020726     C                   KFLD                    §COD                           *
028800020726     C                   KFLD                    §KE1                           *
028900020726     C                   KFLD                    §KE2                           *
029000020726     C*                                                    *
029100020726     C     Kuni          KLIST
029200150526     C                   KFLD                    xscsoc201         3
029300020726     C                   KFLD                    UNIUNITA
029400020726     C******************************************************
029500020726     C     *LIKE         DEFINE    TBECOD        §COD
029600020726     C     *LIKE         DEFINE    TBEKE1        §KE1
029700020726     C     *LIKE         DEFINE    TBEKE2        §KE2
029800020726     C******************************************************
029900020726     C                   SETOFF                                       79
030000020726     C                   MOVEL     KNMUS         PROF              3
030100150525      *
030200020726     C     PROF          IFNE      'EDP'
030300020726     C                   SETON                                        79
030400020726     C                   END
030500020726     C                   MOVEL     'Y4O'         §COD
030600150525      *
030700020726     C                   ENDSR
030800020726     C*---------------------------------------------------------------------
030900020726      /TITLE Reperisco società.
031000020726     C     rtvsocieta    begsr
031100020726     C*
031200020726     C                   CALLb     'XSOC'
031300020726   >>C                   Parm                    TipXsc            6
031400020726   >>C                   Parm                    SocXsc            3
031500020726   >>C                   Parm                    CdsXsc            9 0
031600020726   >>C                   Parm                    ModXsc            3
031700020726   >>C                   Parm      *BLANK        RtnXsc            1
031800020726     C                   Parm                    XsocDS
031900020726   >>C                   Parm                    Kpjba
032000020726     C*
032100020726     C                   endsr
032200020726      *-------------------------------------------------------------------
032300020726      /TITLE Uscita.
032400020726     C     uscita        begsr
032500020726     C*
032600020726     C                   seton                                        lr
032700020726     C                   return
032800020726     C*
032900020726     C                   endsr
033000020726     C*----------------------------------------------------------
033100020726     C* Chiamata NDMVC002
033200020726     C*----------------------------------------------------------
033300020726     C     CallMvc002    BEGSR
033400020726     C*
033500020726     C* Controllo conto
033600020726     C                   Clear                   ND002DS
033700020726     C                   EVAL      $LenOut = %Size(ND002DS)
033800150526     C                   move      '201'         xscsoc201
033900020726     C                   CallB     'NDMVC002'
034000150526     C                   PARM                    xscsoc201
034100020726     C                   PARM                    $kcc
034200020726     C                   PARM                    $ksc
034300020726     C                   PARM                    Wdate
034400020726     C                   PARM      *OFF          $GesMsg
034500020726     C                   PARM      *OFF          $Esito
034600020726     C                   PARM      'ND002DS'     $Struttura
034700020726     C                   PARM                    ND002DS
034800020726     C                   PARM                    $LenOut
034900020726     C*
035000020726     C                   ENDSR
035100150525     C*----------------------------------------------------------
