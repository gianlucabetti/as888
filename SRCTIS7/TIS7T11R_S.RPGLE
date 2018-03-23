000100010122      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200130115     H DFTACTGRP(*no) ACTGRP(*caller)
000300990908
000400030624     Ffnacr01l  if   e           k disk
000500070926     Ftivss02l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800010122     FFNVAOwwr  O    E             DISK    usropn
000900011112     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100990908
001200000512     D*------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600030624     D*------------
001700941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001800030624     D*------------
001900941229     D*  6+1
002000941229     D                 DS
002100941229     D  WS09                   1      1
002200941229     D  WI09                   2      7
002300941229     D  WV09                   8      8
002400941229     D  WD09                   9      9
002500941229     D  WASC09                 1      9
002600941229     D*  2+3
002700941229     D                 DS
002800941229     D  WS07                   1      1
002900941229     D  WI07                   2      3
003000941229     D  WV07                   4      4
003100941229     D  WD07                   5      7
003200941229     D  WASC07                 1      7
003300010122     D*
003400990920     D tivlrds       e ds                  extname(tivlr00f)
003500050826     D fnvapds       e ds                  extname(fnvap00f)
003600010613     D fnvaoaer      e ds                  inz
003700010122     D                                     prefix(a_)
003800141209     D fnvaoeaer     e ds                  inz
003900141209     D                                     prefix(w_)
004000031014     D tisi95ds      e ds
004100011112     D dorm01        e ds
004200011119     D dfar          e ds
004300090617     d trul33ds      e ds
004400090617     d kpjba         e ds
004500990910     D esito           s              1
004600000724     D prmlit          s             10
004700000710     D prmfir          s             10
004800990921     D wrkesito        s                   like(esito)
004900000613     D rrnum           s              6  0 INZ(*zeros)
005000060306     D Cli_Abil        s              1    inz(*off)
005100060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005200061031     D ConstINO        s             15    inz('ORM DA INTERNET')
005300090617     d §numori         s              7  0 inz
005400011112     D*-------------------
005500011112     D* DS "XSRDA8" - controllo data (8)
005600011112     D*-------------------
005700011112     D wlbda8          DS                  INZ
005800011112     D  g08dat                 1      8  0
005900011112     D  g08inv                 9     16  0
006000011112     D  g08err                17     17
006100011112     D  g08tgi                18     22  0
006200011122     D*-------------------
006300010731     D* COSTANTI
006400011122     D*-------------------
006500010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006600010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006700990908
006800091026
006900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
007100091026
007200010528
007300010528
007400010528
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000724     C*
007900010528     C                   exsr      opeini
008000010528     C                   exsr      rwvao
008100010528     C*
008200010528     C                   seton                                        lr
008300010528
008400010528
008500010528
008600010528
008700010528     C*--------------------------------------------------------
008800010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008900010528     C*--------------------------------------------------------
009000010528     C     PREELA        BEGSR
009100010528     C*
009200000724     C* SE OCCORRE SPEDIRE IN FILIALE
009300010528     C                   if        invfil <> *zeros and
009400010528     C                             flgGiro = '0'
009500010528     C*
009600010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009700010528     C                   eval      flgGiro = '1'
009800010529     C*
009900010529     C                   endif
010000010528     C*
010100010528     C                   ENDSR
010200010528     C***
010300010528
010400010528
010500010528
010600010528
010700010528
010800010528
010900010528     C*--------------------------------------------------------
011000010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010528     C*--------------------------------------------------------
011200010528     C     ENDELA        BEGSR
011300000613     C*
011400010528     C                   ENDSR
011500010528     C***
011600990908
011700010528
011800010528
011900010528
012000010528
012100910830     C*--------------------------------------------------------
012200010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
012300910830     C*--------------------------------------------------------
012400010122     C     RWVAO         BEGSR
012500010528     C*
012600990914     C                   if        not %open(tivin00r)
012700990908     C                   open      tivin00r
012800990914     C                   endif
012900010122     C                   if        not %open(fnvaowwr)
013000010122     C                   open      fnvaowwr
013100990914     C                   endif
013200141209     C*
013300141209     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
013400141209     C                   call      'TIS7T7ER2'
013500141209     C                   parm      'O'           pIn_Opz           1
013600141209     C                   parm                    tivlrds
013700141209     C                   parm                    FNVAOEAER
013800141209     C                   parm                    oriPRG
013900141209     C                   parm      *zeros        pIn_Dst           3
014000141209     C                   parm      *blanks       pOut_Esito        1
014100141209     C                   if        pOut_Esito = '2'
014200141209     C                   goto      FINE
014300141209     C                   endif
014400990910     C*
014500990910     C                   clear                   §CTROK
014600990910     C                   clear                   §CTRMO
014700990910     C                   clear                   §CTRNO
014800990910     C*
014900921023     C                   DO        *HIVAL
015000990913     C*
015100990915     C                   READ      tivin00r                               70
015200010731     C*
015300010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015400010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
015500010618     C*
015600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015700010618     C                   if        vinflg = '1'
015800010618     C                   eval      flgOk = '1'
015900010618     C                   endif
016000010618     C*
016100000905     C                   if        vindta > *blanks
016200000613     C                   add       1             rrnum
016300990913     C*
016400010711     C                   if        *in70 = *off and
016500010711     C                             (vinflg = *blanks
016600010711     C                              or vinflg = '0'
016700010711     C                              or vinflg = '2')
016800990913     C*
016900010528     C                   clear                   FNVAO000
017000011112     C                   clear                   TIORI000
017100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017200010711     C                   if        vinflg = *blanks or vinflg = '0'
017300010711     C                   clear                   vinmsg
017400010711     C                   endif
017500010711     C*
017600010613     C                   eval      fnvaoaer = vindta
017700141209     C*
017800141209     C* Verifico se trattasi di formato ORM "esteso"
017900141209     C                   setoff                                       53
018000141209     C                   if        %subst(vindta:717+1:%size(fnvaoeaer)-717+1)
018100141209     C                             > *blanks
018200141209     C                   eval      fnvaoeaer = vindta
018300141209     C                   seton                                        53
018400141209     C                   exsr      EXEFORZ
018500141209     C                   endif
018600991022     C*
018700010122     C                   testb     x'3F'         a_vaoatb                 02
018800991022     C                   if        not *in02
018900991022     C*
019000971021     C                   SETOFF                                       303132
019100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019300971023     C*    OPPURE TUTTI BLANKS
019400010122     C*-------------------
019500010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
019600010122     C* IN32 => Campo facoltativo, muovo tutti zero e passo cimunque il record marcandolo come OK
019700010122     C*-------------------
019800010122     C*
019900010417 xxx C*                  TESTN                   a_vaopoe             30
020000010417  "  C* N30              TESTN                   a_vaopoe               30
020100010417  "  C* N30              TESTN                   a_vaopoe                 30
020200010417     C*                  MOVE      a_vaopoe      W001A             1
020300010417     C*    *IN30         IFEQ      *OFF
020400010417     C*    W001A         ORLT      '0'
020500010417     C*                  SETON                                        32
020600010417     C*                  MOVE      *ZEROS        VAOPOE
020700010417     C*                  EVAL      vinmsg = 'VAOPOE'
020800010417     C*                  ELSE
020900010417     C*                  MOVE      a_vaopoe      VAOPOE
021000010417     C*                  ENDIF
021100010417  "  C*
021200010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
021300010604 xxx C                   IF        vlrpoi <> 999
021400010528     C                   EVAL      VAOPOE = vlrpoi
021500010528     C                   ELSE
021600010528     C                   IF        a_vaopoe = *blanks or
021700011010     C                             a_vaopoe = *zeros  or
021800011010     C                             a_vaopoe = ' 000'
021900010528     C                   IF        a_vaopor <> *blanks and
022000011010     C                             a_vaopor <> *zeros  and
022100011010     C                             a_vaopor <> ' 000'
022200010528     C                   MOVE      a_vaopor      VAOPOE
022300010528     C                   ELSE
022400011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
022500010528     C                   MOVE      a_vaopoe      VAOPOE
022600010528     C                   ENDIF
022700010529     C                   ELSE
022800010529     C                   MOVE      a_vaopoe      VAOPOE
022900010528     C                   ENDIF
023000010528     C                   ENDIF
023100010122     C*
023200010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023300010528     C                   eval      depfil = VAOPOE
023400010528     C                   exsr      repfil
023500010528     C                   if        depfil = invfil and
023600010528     C                             (vinflg = *blanks
023700010528     C                              or vinflg = '0'
023800010528     C                              or vinflg = '2')
023900010528     C*
024000010528     C                   exsr      PREELA
024100010528     C*
024200010719     C*                  TESTN                   a_vaonsr             30
024300010719     C* N30              TESTN                   a_vaonsr               30
024400010719     C* N30              TESTN                   a_vaonsr                 30
024500010719     C*                  MOVE      a_vaonsr      W001A             1
024600010719     C*    *IN30         IFEQ      *OFF
024700010719     C*    W001A         ORLT      '0'
024800010719     C*                  SETON                                        32
024900010719     C*                  MOVE      *ZEROS        VAONSR
025000010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
025100010719     C*                  ELSE
025200010719     C*                  MOVE      a_vaonsr      VAONSR
025300010719     C*                  ENDIF
025400010122     C*
025500010719     C*                  TESTN                   a_vaonor             30
025600010719     C* N30              TESTN                   a_vaonor               30
025700010719     C* N30              TESTN                   a_vaonor                 30
025800010719     C*                  MOVE      a_vaonor      W001A             1
025900010719     C*    *IN30         IFEQ      *OFF
026000010719     C*    W001A         ORLT      '0'
026100010719     C*                  SETON                                        32
026200010719     C*                  MOVE      *ZEROS        VAONOR
026300010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
026400010719     C*                  ELSE
026500010719     C*                  MOVE      a_vaonor      VAONOR
026600010719     C*                  ENDIF
026700010122     C*
026800010719     C*                  TESTN                   a_vaonrv             30
026900010719     C* N30              TESTN                   a_vaonrv               30
027000010719     C* N30              TESTN                   a_vaonrv                 30
027100010719     C*                  MOVE      a_vaonrv      W001A             1
027200010719     C*    *IN30         IFEQ      *OFF
027300010719     C*    W001A         ORLT      '0'
027400010719     C*                  SETON                                        32
027500010719     C*                  MOVE      *ZEROS        VAONRV
027600010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
027700010719     C*                  ELSE
027800010719     C*                  MOVE      a_vaonrv      VAONRV
027900010719     C*                  ENDIF
028000010122     C*
028100010122     C                   TESTN                   a_vaodao             30
028200010122     C  N30              TESTN                   a_vaodao               30
028300010122     C  N30              TESTN                   a_vaodao                 30
028400010122     C                   MOVE      a_vaodao      W001A             1
028500010122     C     *IN30         IFEQ      *OFF
028600010122     C     W001A         ORLT      '0'
028700010122     C                   SETON                                        32
028800010122     C                   MOVE      *ZEROS        VAODAO
028900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
029000010122     C                   ELSE
029100010122     C                   MOVE      a_vaodao      VAODAO
029200010122     C                   ENDIF
029300010122     C*
029400010122     C                   TESTN                   a_vaooao             30
029500010122     C  N30              TESTN                   a_vaooao               30
029600010122     C  N30              TESTN                   a_vaooao                 30
029700010122     C                   MOVE      a_vaooao      W001A             1
029800010122     C     *IN30         IFEQ      *OFF
029900010122     C     W001A         ORLT      '0'
030000010122     C                   SETON                                        32
030100010122     C                   MOVE      *ZEROS        VAOOAO
030200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
030300010122     C                   ELSE
030400010122     C                   MOVE      a_vaooao      VAOOAO
030500010122     C                   ENDIF
030600010122     C*
030700010122     C                   TESTN                   a_vaocor             30
030800010122     C  N30              TESTN                   a_vaocor               30
030900010122     C  N30              TESTN                   a_vaocor                 30
031000010122     C                   MOVE      a_vaocor      W001A             1
031100010122     C     *IN30         IFEQ      *OFF
031200010122     C     W001A         ORLT      '0'
031300010122     C                   SETON                                        32
031400010122     C                   MOVE      *ZEROS        VAOCOR
031500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
031600010122     C                   ELSE
031700010122     C                   MOVE      a_vaocor      VAOCOR
031800010122     C                   ENDIF
031900010122     C*
032000010122     C                   TESTN                   a_vaocra             30
032100010122     C  N30              TESTN                   a_vaocra               30
032200010122     C  N30              TESTN                   a_vaocra                 30
032300010122     C                   MOVE      a_vaocra      W001A             1
032400010122     C     *IN30         IFEQ      *OFF
032500010122     C     W001A         ORLT      '0'
032600010122     C                   SETON                                        32
032700010122     C                   MOVE      *ZEROS        VAOCRA
032800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
032900010122     C                   ELSE
033000010122     C                   MOVE      a_vaocra      VAOCRA
033100010122     C                   ENDIF
033200010122     C*
033300010719     C                   IF        a_vaocrc = *blanks or
033400010719     C                             a_vaocrc = *zeros
033500010719     C                   ELSE
033600010122     C                   TESTN                   a_vaocrc             30
033700010122     C  N30              TESTN                   a_vaocrc               30
033800010122     C  N30              TESTN                   a_vaocrc                 30
033900010122     C                   MOVE      a_vaocrc      W001A             1
034000010122     C     *IN30         IFEQ      *OFF
034100010122     C     W001A         ORLT      '0'
034200010122     C                   SETON                                        32
034300010122     C                   MOVE      *ZEROS        VAOCRC
034400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
034500010122     C                   ELSE
034600010122     C                   MOVE      a_vaocrc      VAOCRC
034700010122     C                   ENDIF
034800010719     C                   ENDIF
034900010122     C*
035000010122     C                   TESTN                   a_vaodar             30
035100010122     C  N30              TESTN                   a_vaodar               30
035200010122     C  N30              TESTN                   a_vaodar                 30
035300010122     C                   MOVE      a_vaodar      W001A             1
035400010122     C     *IN30         IFEQ      *OFF
035500010122     C     W001A         ORLT      '0'
035600010122     C                   SETON                                        32
035700010122     C                   MOVE      *ZEROS        VAODAR
035800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
035900010122     C                   ELSE
036000010122     C                   MOVE      a_vaodar      VAODAR
036100010122     C                   ENDIF
036200010122     C*
036300010122     C                   TESTN                   a_vaoorr             30
036400010122     C  N30              TESTN                   a_vaoorr               30
036500010122     C  N30              TESTN                   a_vaoorr                 30
036600010122     C                   MOVE      a_vaoorr      W001A             1
036700010122     C     *IN30         IFEQ      *OFF
036800010122     C     W001A         ORLT      '0'
036900010122     C                   SETON                                        32
037000010122     C                   MOVE      *ZEROS        VAOORR
037100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
037200010122     C                   ELSE
037300010122     C                   MOVE      a_vaoorr      VAOORR
037400010122     C                   ENDIF
037500010122     C*
037600010122     C                   TESTN                   a_vaoncl             30
037700010122     C  N30              TESTN                   a_vaoncl               30
037800010122     C  N30              TESTN                   a_vaoncl                 30
037900010122     C                   MOVE      a_vaoncl      W001A             1
038000010122     C     *IN30         IFEQ      *OFF
038100010122     C     W001A         ORLT      '0'
038200010122     C                   SETON                                        32
038300010122     C                   MOVE      *ZEROS        VAONCL
038400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
038500010122     C                   ELSE
038600010122     C                   MOVE      a_vaoncl      VAONCL
038700010122     C                   ENDIF
038800010122     C*
038900010122     C                   MOVE      *ZERO         VAOPKG
039000010122     C                   MOVE      a_vaopkg      WASC09
039100010122     C                   TESTN                   WI09                 30
039200010122     C  N30              TESTN                   WI09                   30
039300010122     C  N30              TESTN                   WI09                     30
039400010122     C                   MOVE      WI09          W001A             1
039500010122     C     *IN30         IFEQ      *OFF
039600010122     C     W001A         ORLT      '0'
039700010122     C     W001A         ANDNE     *BLANK
039800010122     C                   SETON                                        32
039900010122     C                   MOVE      *ZERO         VAOPKG
040000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
040100010122     C                   ELSE
040200010122     C                   MOVEL     WI09          VAOPKG
040300010122     C                   TESTN                   WD09                 30
040400010122     C  N30              TESTN                   WD09                     30
040500010122     C                   MOVE      WD09          W001A             1
040600010122     C     *IN30         IFEQ      *OFF
040700010122     C     W001A         ORLT      '0'
040800010122     C     W001A         ANDNE     *BLANK
040900010122     C                   SETON                                        32
041000010122     C                   MOVE      *ZERO         VAOPKG
041100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
041200010122     C                   ELSE
041300010122     C                   MOVE      WD09          VAOPKG
041400010122     C                   ENDIF
041500050118     C                   ENDIF
041600010122     C*
041700010122     C                   MOVE      *ZERO         VAOVLM
041800010122     C                   MOVE      a_vaovlm      WASC07
041900010122     C                   TESTN                   WI07                 30
042000010122     C  N30              TESTN                   WI07                   30
042100010122     C  N30              TESTN                   WI07                     30
042200010122     C                   MOVE      WI07          W001A             1
042300010122     C     *IN30         IFEQ      *OFF
042400010122     C     W001A         ORLT      '0'
042500010122     C     W001A         ANDNE     *BLANK
042600010122     C                   SETON                                        32
042700010122     C                   MOVE      *ZERO         VAOVLM
042800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
042900010122     C                   ELSE
043000010122     C                   MOVEL     WI07          VAOVLM
043100010122     C                   TESTN                   WD07                 30
043200010122     C  N30              TESTN                   WD07                   30
043300010122     C  N30              TESTN                   WD07                     30
043400010122     C                   MOVE      WD07          W001A             1
043500010122     C     *IN30         IFEQ      *OFF
043600010122     C     W001A         ORLT      '0'
043700010122     C     W001A         ANDNE     *BLANK
043800010122     C                   SETON                                        32
043900010122     C                   MOVE      *ZERO         VAOVLM
044000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
044100010122     C                   ELSE
044200010122     C                   MOVE      WD07          VAOVLM
044300010122     C                   ENDIF
044400050118     C                   ENDIF
044500010122     C*
044600010122     C                   TESTN                   a_vaobnc             30
044700010122     C  N30              TESTN                   a_vaobnc               30
044800010122     C  N30              TESTN                   a_vaobnc                 30
044900010122     C                   MOVE      a_vaobnc      W001A             1
045000010122     C     *IN30         IFEQ      *OFF
045100010122     C     W001A         ORLT      '0'
045200010122     C                   SETON                                        32
045300010122     C                   MOVE      *ZEROS        VAOBNC
045400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
045500010122     C                   ELSE
045600010122     C                   MOVE      a_vaobnc      VAOBNC
045700010122     C                   ENDIF
045800010122     C*
045900010122     C                   TESTN                   a_vaoblc             30
046000010122     C  N30              TESTN                   a_vaoblc               30
046100010122     C  N30              TESTN                   a_vaoblc                 30
046200010122     C                   MOVE      a_vaoblc      W001A             1
046300010122     C     *IN30         IFEQ      *OFF
046400010122     C     W001A         ORLT      '0'
046500010122     C                   SETON                                        32
046600010122     C                   MOVE      *ZEROS        VAOBLC
046700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
046800010122     C                   ELSE
046900010122     C                   MOVE      a_vaoblc      VAOBLC
047000010122     C                   ENDIF
047100010122     C*
047200010122     C                   TESTN                   a_vaoatt             30
047300010122     C  N30              TESTN                   a_vaoatt               30
047400010122     C  N30              TESTN                   a_vaoatt                 30
047500010122     C                   MOVE      a_vaoatt      W001A             1
047600010122     C     *IN30         IFEQ      *OFF
047700010122     C     W001A         ORLT      '0'
047800010122     C                   SETON                                        32
047900010122     C                   MOVE      *ZEROS        VAOATT
048000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
048100010122     C                   ELSE
048200010122     C                   MOVE      a_vaoatt      VAOATT
048300010122     C                   ENDIF
048400010122     C*
048500010122     C                   TESTN                   a_vaomtc             30
048600010122     C  N30              TESTN                   a_vaomtc               30
048700010122     C  N30              TESTN                   a_vaomtc                 30
048800010122     C                   MOVE      a_vaomtc      W001A             1
048900010122     C     *IN30         IFEQ      *OFF
049000010122     C     W001A         ORLT      '0'
049100010122     C                   SETON                                        32
049200010122     C                   MOVE      *ZEROS        VAOMTC
049300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
049400010122     C                   ELSE
049500010122     C                   MOVE      a_vaomtc      VAOMTC
049600010122     C                   ENDIF
049700010122     C*
049800010719     C                   IF        a_vaoksc = *blanks or
049900010719     C                             a_vaoksc = *zeros
050000010719     C                   ELSE
050100010122     C                   TESTN                   a_vaoksc             30
050200010122     C  N30              TESTN                   a_vaoksc               30
050300010122     C  N30              TESTN                   a_vaoksc                 30
050400010122     C                   MOVE      a_vaoksc      W001A             1
050500010122     C     *IN30         IFEQ      *OFF
050600010122     C     W001A         ORLT      '0'
050700010122     C                   SETON                                        32
050800010122     C                   MOVE      *ZEROS        VAOKSC
050900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
051000010122     C                   ELSE
051100010122     C                   MOVE      a_vaoksc      VAOKSC
051200010122     C                   ENDIF
051300010719     C                   ENDIF
051400010122     C*
051500010710     C                   IF        a_vaoctr > *blanks
051600010122     C                   TESTN                   a_vaoctr             30
051700010122     C  N30              TESTN                   a_vaoctr               30
051800010122     C  N30              TESTN                   a_vaoctr                 30
051900010122     C                   MOVE      a_vaoctr      W001A             1
052000010122     C     *IN30         IFEQ      *OFF
052100010122     C     W001A         ORLT      '0'
052200010122     C                   SETON                                        32
052300010627     C                   MOVE      *blanks       VAOCTR
052400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
052500010122     C                   ELSE
052600010122     C                   MOVE      a_vaoctr      VAOCTR
052700010122     C                   ENDIF
052800010710     C                   ENDIF
052900010122     C*
053000010122     C                   TESTN                   a_vaopor             30
053100010122     C  N30              TESTN                   a_vaopor               30
053200010122     C  N30              TESTN                   a_vaopor                 30
053300010122     C                   MOVE      a_vaopor      W001A             1
053400010122     C     *IN30         IFEQ      *OFF
053500010122     C     W001A         ORLT      '0'
053600010122     C                   SETON                                        32
053700010122     C                   MOVE      *ZEROS        VAOPOR
053800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
053900010122     C                   ELSE
054000010122     C                   MOVE      a_vaopor      VAOPOR
054100010122     C                   ENDIF
054200010122     C*
054300010719     C                   IF        a_vaopoc = *blanks or
054400010719     C                             a_vaopoc = *zeros
054500010719     C                   ELSE
054600010122     C                   TESTN                   a_vaopoc             30
054700010122     C  N30              TESTN                   a_vaopoc               30
054800010122     C  N30              TESTN                   a_vaopoc                 30
054900010122     C                   MOVE      a_vaopoc      W001A             1
055000010122     C     *IN30         IFEQ      *OFF
055100010122     C     W001A         ORLT      '0'
055200010122     C                   SETON                                        32
055300010122     C                   MOVE      *ZEROS        VAOPOC
055400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
055500010122     C                   ELSE
055600010122     C                   MOVE      a_vaopoc      VAOPOC
055700010122     C                   ENDIF
055800010719     C                   ENDIF
055900010122     C*
056000010719     C                   IF        a_vaosto = *blanks or
056100010719     C                             a_vaosto = *zeros
056200010719     C                   ELSE
056300010122     C                   TESTN                   a_vaosto             30
056400010122     C  N30              TESTN                   a_vaosto               30
056500010122     C  N30              TESTN                   a_vaosto                 30
056600010122     C                   MOVE      a_vaosto      W001A             1
056700010122     C     *IN30         IFEQ      *OFF
056800010122     C     W001A         ORLT      '0'
056900010122     C                   SETON                                        32
057000010122     C                   MOVE      *ZEROS        VAOSTO
057100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
057200010122     C                   ELSE
057300010122     C                   MOVE      a_vaosto      VAOSTO
057400010122     C                   ENDIF
057500010719     C                   ENDIF
057600971021     C*
057700971021     C* CAMPI ALFABETICI:
057800010122     C                   MOVE      a_vaoatb      VAOATB
057900010122     C                   MOVE      a_vaotor      VAOTOR
058000010531     C*
058100010531     C                   IF        a_vaotco = *blanks
058200010531     C                   MOVE      'F'           VAOTCO
058300010531     C                   ELSE
058400010531     C                   MOVE      a_vaotco      VAOTCO
058500010531     C                   ENDIF
058600010531     C*
058700010122     C                   MOVE      a_vaorso      VAORSO
058800010122     C                   MOVE      a_vaoino      VAOINO
058900010122     C                   MOVE      a_vaocao      VAOCAO
059000010122     C                   MOVE      a_vaoloo      VAOLOO
059100010122     C                   MOVE      a_vaopro      VAOPRO
059200090129     C                   MOVE      a_vaonao      VAONAO
059300031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
059400031014     C                   IF        VAOPRO  = *blanks AND
059500050907     C                             VAOCAO <> *blanks AND
059600050907     C                             VAONAO  = *blanks                            * solo x Italia
059700031014     C                   CLEAR                   TISI95DS
059800031014     C                   EVAL      I95TCN = '3'
059900031014     C                   Z-ADD     datcor        I95DAT
060000031014     C                   EVAL      I95CAP = VAOCAO
060100031014     C                   EVAL      I95LOC = VAOLOO
060200031014     C                   CALL      'TISI95R'
060300031014     C                   PARM                    TISI95DS
060400031014     C                   EVAL      VAOPRO = O95PRV
060500031014     C                   ENDIF
060600031014     C*
060700010122     C                   MOVE      a_vaorsr      VAORSR
060800010122     C                   MOVE      a_vaoinr      VAOINR
060900010122     C                   MOVE      a_vaocar      VAOCAR
061000010122     C                   MOVE      a_vaolor      VAOLOR
061100010122     C                   MOVE      a_vaoprr      VAOPRR
061200090129     C                   MOVE      a_vaonar      VAONAR
061300031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
061400031014     C                   IF        VAOPRR  = *blanks AND
061500050907     C                             VAOCAR <> *blanks AND
061600050907     C                             VAONAR  = *blanks                            * solo x Italia
061700031014     C                   CLEAR                   TISI95DS
061800031014     C                   EVAL      I95TCN = '3'
061900031014     C                   Z-ADD     datcor        I95DAT
062000031014     C                   EVAL      I95CAP = VAOCAR
062100031014     C                   EVAL      I95LOC = VAOLOR
062200031014     C                   CALL      'TISI95R'
062300031014     C                   PARM                    TISI95DS
062400031014     C                   EVAL      VAOPRR = O95PRV
062500031014     C                   ENDIF
062600031014     C*
062700010122     C                   MOVE      a_vaorer      VAORER
062800010122     C                   MOVE      a_vaoter      VAOTER
062900010122     C                   MOVE      a_vaorsc      VAORSC
063000010122     C                   MOVE      a_vaoinc      VAOINC
063100010122     C                   MOVE      a_vaocac      VAOCAC
063200010122     C                   MOVE      a_vaoloc      VAOLOC
063300010122     C                   MOVE      a_vaoprc      VAOPRC
063400090129     C                   MOVE      a_vaonac      VAONAC
063500031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
063600031014     C                   IF        VAOPRC  = *blanks AND
063700050907     C                             VAOCAC <> *blanks AND
063800050907     C                             VAONAC  = *blanks                            * solo x Italia
063900031014     C                   CLEAR                   TISI95DS
064000031014     C                   EVAL      I95TCN = '3'
064100031014     C                   Z-ADD     datcor        I95DAT
064200031014     C                   EVAL      I95CAP = VAOCAC
064300031014     C                   EVAL      I95LOC = VAOLOC
064400031014     C                   CALL      'TISI95R'
064500031014     C                   PARM                    TISI95DS
064600031014     C                   EVAL      VAOPRC = O95PRV
064700031014     C                   ENDIF
064800031014     C*
064900010122     C                   MOVE      a_vaoffd      VAOFFD
065000010122     C                   MOVE      a_vaormp      VAORMP
065100010122     C                   MOVE      a_vaonam      VAONAM
065200010122     C                   MOVE      a_vaopag      VAOPAG
065300010122     C                   MOVE      a_vaozor      VAOZOR
065400010122     C                   MOVE      a_vaono1      VAONO1
065500010122     C                   MOVE      a_vaono2      VAONO2
065600010122     C                   MOVE      a_vaoddt      VAODDT
065700010122     C                   MOVE      a_vaorfa      VAORFA
065800010122     C                   MOVE      a_vaospi      VAOSPI
065900010122     C                   MOVE      a_vaoflo      VAOFLO
066000050304     C*
066100050304     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
066200050304     C*** VAONAO
066300050304     C                   IF        %trim(VAONAO) = 'I' OR
066400050304     C                             %trim(VAONAO) = 'IT'
066500050304     C                   EVAL      VAONAO = *blanks
066600050304     C                   ENDIF
066700050304     C*** VAONAR
066800050304     C                   IF        %trim(VAONAR) = 'I' OR
066900050304     C                             %trim(VAONAR) = 'IT'
067000050304     C                   EVAL      VAONAR = *blanks
067100050304     C                   ENDIF
067200050304     C*** VAONAC
067300050304     C                   IF        %trim(VAONAC) = 'I' OR
067400050304     C                             %trim(VAONAC) = 'IT'
067500050304     C                   EVAL      VAONAC = *blanks
067600050304     C                   ENDIF
067700050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
067800050304     C                   IF        VAOPKG =  *zeros AND
067900050304     C                             VAOVLM =  *zeros AND
068000050304     C                             VAOBNC =  *zeros AND
068100050304     C                             VAONCL <> *zeros
068200050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
068300050304     C                   ENDIF
068400030603     C*
068500030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
068600030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
068700030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
068800050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
068900930409     C*
069000010604     C  N31              ADD       1             §CTROK            7 0
069100010604     C   32              ADD       1             §CTRMO            7 0
069200010604     C   31              ADD       1             §CTRNO            7 0
069300030624     C  N31              EXSR      CHKCOR
069400011112     C  N31              EXSR      WRIORI
069500050826     C  N31              EXSR      WRIVGD
069600050421     C*
069700050421     C***
069800050421     C* FORZATURA SU DATA/ORA => NN CONSIDERO QUELLE PASSATE SUL FILE DAL CLIENTE MA SEMPRE
069900050421     C* DATA/ORA DI TRADUZIONE (OVVERO VERA IMMISSIONE DEL DATO SEPPUR DA CONFERMARE SUL
070000050421     C* NS. GESTIONALE). QUESTO SOLO X LA SCRITTURA DEL VAO!!!
070100050421     C                   eval      VAODAO = datcor
070200050421     C                   eval      VAOOAO = wn6
070300050421     C***
070400010122     C  N31              WRITE     FNVAO000
070500141209     C*
070600141209     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
070700141209     C                   if        *in53 and not *in31
070800141209     C                   move      VAOPOE        w_vaopoe                       * forzatura necessar
070900141209     C                   move      VAOCOR        w_vaocor                       * forzatura necessar
071000141209     C                   move      VAONOR        w_vaonor                       * forzatura necessar
071100141209     C                   call      'TIS7T7ER2'
071200141209     C                   parm      'W'           pIn_Opz           1
071300141209     C                   parm                    tivlrds
071400141209     C                   parm                    FNVAOEAER
071500141209     C                   parm                    oriPRG
071600141209     C                   parm      *zeros        pIn_Dst           3
071700141209     C                   parm      *blanks       pOut_Esito        1
071800141209     C                   endif
071900141209     C                   if        pOut_Esito = '2'
072000141209     C                   seton                                        32
072100141209     C                   endif
072200990910     C*
072300010604     C                   if        *in31 = *off and
072400010604     C                             *in32 = *off
072500990910     C                   eval      vinflg = '1'
072600990910     C                   else
072700990910     C                   eval      vinflg = '2'
072800990910     C                   endif
072900991022     C                   endif
073000990910     C                   endif
073100010604     C*
073200010604     C                   endif
073300000905     C*
073400000905     C                   else
073500000905     C                   eval      vinflg = '1'
073600000905     C                   endif
073700000905     C*
073800000905     C  N70              update    tivin000
073900991022     C*
074000991022     C  N70              ENDdo
074100141209     C*
074200141209     C     FINE          tag
074300141209     C*
074400141209     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
074500141209     C                   call      'TIS7T7ER2'
074600141209     C                   parm      'C'           pIn_Opz           1
074700141209     C                   parm                    tivlrds
074800141209     C                   parm                    FNVAOEAER
074900141209     C                   parm                    oriPRG
075000141209     C                   parm      *zeros        pIn_Dst           3
075100141209     C                   parm      *blanks       pOut_Esito        1
075200010528     C*
075300010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075400010528     C                   if        cntNonEl = *zeros or
075500010528     C                             flgMulti = '0'
075600010528     C* Se non ci sono record con errori ...
075700010528     C                   if        §ctrno = 0 and
075800010604     C                             §ctrmo = 0 and
075900010528     C                             flgStato <> '2'
076000990910     C* ... restituisco esito OK.
076100990921     C                   eval      wrkesito = '0'
076200990910     C                   else
076300000710     C                   if        §ctrok > 0
076400990921     C                   eval      wrkesito = '1'
076500000710     C                   else
076600010615     C                   if        flgOk = '0'
076700010615     C                   eval      wrkesito = '2'
076800010615     C                   else
076900010615     C                   eval      wrkesito = '6'
077000010615     C                   endif
077100990910     C                   endif
077200010528     C                   endif
077300010529     C                   else
077400010529     C                   eval      wrkesito = '9'
077500000710     C                   endif
077600990910     C*
077700990914     C                   if        %open(tivin00r)
077800990908     C                   close     tivin00r
077900990914     C                   endif
078000010122     C                   if        %open(fnvaowwr)
078100010122     C                   close     fnvaowwr
078200990914     C                   endif
078300990910     C*
078400010528     C                   if        vlrpoi <> 999
078500010528     C                   eval      invfil = vlrpoi
078600010528     C                   endif
078700010528     C*
078800990920     C                   if        §ctrok > 0
078900010528     C                             and invfil > *zeros
079000000613     C                   exsr      invio
079100141209     C*
079200141209     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
079300141209     C                   move      invfil        cmzdst
079400141209     C                   call      'TIS7T7ER2'
079500141209     C                   parm      'S'           pIn_Opz           1
079600141209     C                   parm                    tivlrds
079700141209     C                   parm                    FNVAOEAER
079800141209     C                   parm                    oriPRG
079900141209     C                   parm      cmzdst        pIn_Dst           3
080000141209     C                   parm      *blanks       pOut_Esito        1
080100990920     C                   endif
080200010612     C*
080300010612     C                   if        flgGiro = '1'
080400010612     C                   exsr      endela
080500010612     C                   endif
080600010528     C*
080700910830     C                   ENDSR
080800000613     C***
080900141209
081000141209
081100141209     C*------------------------------------------------------------------------*
081200141209     C* EXEFORZ - Effettuo forzature
081300141209     C*------------------------------------------------------------------------*
081400141209     C     EXEFORZ       BEGSR
081500141209     C*
081600141209     C* Se necessario e VAONOR non è valorizzato stacco un numeratore da AZNUM
081700141209     C                   if        vaoNOR = *zeros
081800141209     C                   clear                   trul33ds
081900141209     C                   eval      I33OPE = *zeros
082000141209     C                   eval      I33CNU = 600
082100141209     C                   eval      I33NUM = 1
082200141209     C                   movel     TRUL33DS      KPJBU
082300141209     C                   call      'TRUL33R'
082400141209     C                   parm                    KPJBA
082500141209     C                   movel     KPJBU         TRUL33DS
082600141209     C                   if        O33ERR = *zeros
082700141209     C                   z-add     O33NRF        §numori
082800141209     c                   else
082900141209     c                   eval      §numori = *all'9'
083000141209     C                   seton                                        31        * errore bloccante
083100141209     C                   endif
083200141209     C*
083300141209     C                   eval      vaoNOR = §numori
083400141209     C                   endif
083500141209     C*
083600141209     C                   ENDSR
083700141209
083800050323
083900050323
084000050323     C*------------------------------------------------------------------------*
084100050323     C* CHKCOR - Considerazioni su VAOCOR
084200050323     C*------------------------------------------------------------------------*
084300050323     C     CHKCOR        BEGSR
084400050323     C*
084500050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
084600050323     C                   if        vaoTCO = 'F'
084700050323     C* Quindi verifico se il codice ordinante non è valorizzato
084800050323     C                   if        vaoCOR = *zeros
084900050323     C*                  if        vaoCOR = *zeros  AND
085000050323     C*                            vaoRSO = *blanks AND
085100050323     C*                            vaoINO = *blanks AND
085200050323     C*                            vaoCAO = *blanks AND
085300050323     C*                            vaoLOO = *blanks AND
085400050323     C*                            vaoPRO = *blanks AND
085500050323     C*                            vaoNAO = *blanks
085600050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
085700050323     C                   move      vlrKSC        wCli              7 0
085800050323     C                   movel(p)  wCli          acrCRO
085900050323     C     acrCRO        setll     fnacr01l
086000050323     C                   if        %found(fnacr01l)
086100050323     C                   read      fnacr01l
086200050323     C                   dow       not %eof(fnacr01l)
086300050323     C                   movel     acrCRO        wCliAcr           7 0
086400050323     C                   if        wCliAcr = wCli
086500050323     C                   eval      vaoCOR = acrCRO
086600050323     C                   leave
086700050323     C                   else
086800050323     C                   leave
086900050323     C                   endif
087000050323     C                   read      fnacr01l
087100050323     C                   enddo
087200050323     C                   endif
087300050323     C                   endif
087400050323     C                   endif
087500050323     C*
087600050323     C                   if        vaoTCO = 'I'
087700050323     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
087800050323     C                   if        vlrKSC = '0ORM0000'
087900050323     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
088000061031     C***                eval      vaoINO = 'ORM DA INTERNET'
088100061031     C                   movel     ConstINO      vaoINO
088200050323     C                   eval      vaoCAO = '40138'
088300050323     C                   eval      vaoLOO = 'BOLOGNA'
088400050323     C                   eval      vaoPRO = 'BO'
088500050323     C                   endif
088600050323     C                   endif
088700050323     C*
088800050323     C                   ENDSR
088900990920
089000010528
089100011112
089200011112     C*------------------------------------------------------------------------*
089300011112     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
089400011112     C*------------------------------------------------------------------------*
089500011112     C     WRIORI        BEGSR
089600011112     C*
089700090617     C* Come prima cosa stacco un numeratore da AZNUM
089800090617     C                   clear                   trul33ds
089900090617     C                   eval      I33OPE = *zeros
090000090617     C                   eval      I33CNU = 600
090100090617     C                   eval      I33NUM = 1
090200090617     C                   movel     TRUL33DS      KPJBU
090300090617     C                   call      'TRUL33R'
090400090617     C                   parm                    KPJBA
090500090617     C                   movel     KPJBU         TRUL33DS
090600090617     C                   if        O33ERR = *zeros
090700090617     C                   z-add     O33NRF        §numori
090800090618     c                   else
090900090618     c                   eval      §numori = *all'9'
091000011112     C                   endif
091100070925     C*
091200070925     C* Giro "standard" (ovvero x cliente d invio dati)
091300070925     C                   if        *in51 = *off
091400011112     C*
091500011122     C* Qindi imposto i campi "extra-VAO"
091600011122     C                   if        vlrKSC = '0ORM0000'
091700011122     C*
091800011122     C                   if        vaoCOR > *zeros
091900011122     C                   movel     vaoCOR        wrkCOR7           7 0
092000011122     C*
092100011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
092200011122     C* codice padre. Se è un padre assegno già il codice ordinante.
092300011122     C                   clear                   tibs10ds
092400011122     C                   eval      d10drf = datcor                              *data riferimento
092500011122     C                   eval      d10paf = 'P'                                 *cerca il padre
092600011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
092700091026     C*
092800091026     C* Preparazione chiamata al TIBS10R1
092900091026     C                   clear                   tibs10r1ds
093000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
093100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
093200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
093300091026     C*
093400091026     C                   call      'TIBS10R1'
093500091026     C                   parm                    tibs10r1ds
093600091026     C*
093700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
093800011122     C*
093900011122     C                   if        d10err <> *blanks or                         *No unif. o padre
094000011122     C                             d10cop =  *zeros  or
094100011122     C                             d10cop =  d10cod
094200011122     C                   move      d10cod        oriIDC
094300011122     C                   else                                                   *Figlio
094400011122     C                   move      d10cop        oriIDC
094500011122     C                   endif
094600011122     C*
094700011122     C                   else
094800011122     C                   eval      oriIDC = *zeros
094900011122     C                   endif
095000011122     C*
095100011122     C                   else
095200011122     C                   eval      oriIDC = vlrKSC
095300011122     C                   endif
095400070925     C*
095500070925     C* Giro "forzato" (ovvero x codice ordinante)
095600070925     C                   else
095700070925     C*
095800070925     C                   if        vaoCOR > *zeros
095900070925     C                   movel     vaoCOR        wrkCOR7           7 0
096000070925     C*
096100070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
096200070925     C* codice padre. Se è un padre assegno già il codice ordinante.
096300070925     C                   clear                   tibs10ds
096400070925     C                   eval      d10drf = datcor                              *data riferimento
096500070925     C                   eval      d10paf = 'P'                                 *cerca il padre
096600070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
096700091026     C*
096800091026     C* Preparazione chiamata al TIBS10R1
096900091026     C                   clear                   tibs10r1ds
097000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
097100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
097200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
097300091026     C*
097400091026     C                   call      'TIBS10R1'
097500091026     C                   parm                    tibs10r1ds
097600091026     C*
097700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
097800070925     C*
097900070925     C                   if        d10err <> *blanks or                         *No unif. o padre
098000070925     C                             d10cop =  *zeros  or
098100070925     C                             d10cop =  d10cod
098200070925     C                   move      d10cod        oriIDC
098300070925     C                   else                                                   *Figlio
098400070925     C                   move      d10cop        oriIDC
098500070925     C                   endif
098600070926     C*
098700070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
098800070926     C* al servizio d rintraccia ritiri on-line
098900070926     C                   eval      vssKSU = oriIDC
099000070926     C                   eval      vssISV = 'T3'
099100070926     C     keyvss02_p    chain     tivss02l
099200070926     C                   if        %found(tivss02l)
099300070926     C                   if        vssdde <= datcor AND
099400070926     C                             datcor <= vssdsc
099500070926     C                   else
099600070926     C                   eval      oriIDC = vlrKSC
099700070926     C                   endif
099800070926     C                   else
099900070926     C                   eval      oriIDC = vlrKSC
100000070926     C                   endif
100100070926     C*
100200070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
100300070926     C* anagrafica clienti ritiro
100400070926     C                   setoff                                       52
100500070926     C                   eval      acrCRO = wrkCOR7 * 1000
100600070926     C     acrCRO        setll     fnacr01l
100700070926     C                   if        %found(fnacr01l)
100800070926     C                   read      fnacr01l
100900070926     C                   dow       not %eof(fnacr01l)
101000070926     C                   movel     acrCRO        wCliAcr           7 0
101100070926     C                   if        wCliAcr = wrkCOR7
101200070926     C                   seton                                        52
101300070926     C                   leave
101400070926     C                   else
101500070926     C                   leave
101600070926     C                   endif
101700070926     C                   read      fnacr01l
101800070926     C                   enddo
101900070926     C                   endif
102000070926     C*
102100070926     C                   if        *in52  = *off
102200070926     C                   eval      oriIDC = vlrKSC
102300070926     C                   endif
102400070925     C*
102500070925     C                   else
102600070925     C                   if        vlrKSC = '0ORM0000'
102700070925     C                   eval      oriIDC = *zeros
102800070925     C                   else
102900070925     C                   eval      oriIDC = vlrKSC
103000070925     C                   endif
103100070925     C                   endif
103200070925     C                   endif
103300011122     C*
103400011112     C                   eval      oriPRG = §numori
103500011112     C                   eval      oriDER = datcor
103600011112     C                   eval      oriOER = wn6
103700011112     C                   eval      oriPOE = vaoPOE
103800011112     C                   eval      oriNSR = vaoNSR
103900011112     C                   eval      oriNOR = vaoNOR
104000011112     C                   eval      oriNRV = vaoNRV
104100011112     C                   eval      oriFLO = *blanks
104200011112     C*
104300011112     C* Infine valorizzo la chieve esterna sul file FNVAO
104400011112     C                   eval      dorm01 = vaoFLO
104500011112     C                   movel     §numori       §ormpg
104600011112     C                   eval      vaoFLO = dorm01
104700011112     C*
104800011112     C* ...quindi scrivo il file TIORI00F
104900011112     C                   WRITE     TIORI000
105000011112     C*
105100011112     C                   ENDSR
105200050826
105300050826
105400050826
105500050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
105600050826     C     wriVGD        BEGSR
105700050826     C*
105800050826     C* Reperisco la descrizione della fase da tabella
105900050826     C                   movel(p)  'FAR'         KteCOD
106000050826     C                   movel(p)  '000'         KteKE1
106100050826     C     KEYtbe        chain     tntbe01l
106200050826     C                   if        %found(tntbe01l)
106300050826     C                   movel(P)  tbeuni        DFAR
106400050826     C                   else
106500050826     C                   clear                   DFAR
106600050826     C                   endif
106700050826     C*
106800050826     C* Valorizzo buffer record
106900050826     C                   clear                   FNVAPDS
107000050826     C                   eval      vapIDC = oriIDC
107100050826     C                   eval      vapPOE = oriPOE
107200050826     C                   eval      vapRFA = vaoRFA
107300050826     C                   eval      vapPOG = oriPOE
107400050826     C                   eval      vapDAE = oriDER
107500050826     C                   eval      vapORE = oriOER
107600050826     C                   if        d§fardva <> *blanks
107700050826     C                   eval      vapDFA = d§fardva
107800050826     C                   else
107900050826     C                   eval      vapDFA = d§fardes
108000050826     C                   endif
108100050826     C*
108200050826     C                   clear                   tivgd000
108300080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
108400050826     C                   eval      vgdTIP = 'VP'
108500050826     C                   eval      vgdKSU = vapIDC
108600050826     C                   eval      vgdTSC = 'WW'
108700050826     C                   eval      vgdDAT = datcor
108800050826     C                   eval      vgdPGM = 'TIS7T11R'
108900060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
109000060306     C                   exsr      sr_Control
109100060306     C                   if        Cli_Abil = *on
109200060306     C                   eval      vgdKSU = woriIDC
109300060306     C                   write     tivgd000
109400060306     C                   endif
109500050826     C*
109600050826     C                   ENDSR
109700050826     C*------------------------------------------------------------------------*
109800011119
109900010528
110000010528
110100010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
110200010528     C     repfil        BEGSR
110300010528     C*
110400010528     C                   if        invfil = *zeros and
110500010528     C                             depfil > *zeros and
110600010613     C                             (vinflg = *blanks or
110700010613     C                              vinflg = *zeros)
110800010528     C
110900010528     C                   eval      invfil = depfil
111000010528     C                   endif
111100010528     C*
111200010529     C                   if        depfil <> invfil and
111300010529     C                             invfil > *zeros
111400010528     C                   eval      flgMulti = '1'
111500010528     C                   if        vinflg = *blanks
111600010528     C                   add       1             cntNonEl
111700010528     C                   endif
111800010528     C                   endif
111900010528     C*
112000010528     C                   if        vinflg = '2'
112100010528     C                   eval      flgStato = '2'
112200010528     C                   endif
112300010528     C*
112400010528     C                   ENDSR
112500010528     C***
112600010528
112700010528
112800010528
112900010528
113000990920      /TITLE Invio dei dati al punto operativo.
113100000613     C     invio         BEGSR
113200990920     C*
113300990920     C                   reset                   dscmz
113400010528     C                   move      invfil        cmzdst
113500990920     C                   eval      cmzfld = vlrfou
113600990920     C                   eval      cmzmbd = vlrhdl
113700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
113800000710     C                   if        prmfir = *blanks
113900010122     C                   eval      cmzfla = 'FNVAO00F'
114000010122     C                   eval      cmzmba = 'FNVAO00F'
114100000710     C                   else
114200000710     C                   eval      cmzfla = prmfir
114300000710     C                   eval      cmzmba = prmfir
114400000710     C                   endif
114500990920     C                   eval      cmznrr = *zeros
114600990920     C                   move      §ctrok        cmznrr
114700010123     C                   eval      cmzlba = vlrfl1
114800990920     C                   call(e)   'TIS711C'
114900990920     C                   parm                    dscmz
115000990921     C                   parm      *blanks       esito
115100990920     C                   if        %error
115200990920     C                             or cmzerr = '1'
115300990921     C                             or esito  = '1'
115400000710     C                   eval      wrkesito = '3'
115500990920     C                   endif
115600990920     C*
115700000613     C                   ENDSR
115800000613     C***
115900990910
116000010528
116100010528
116200010528
116300010528      /TITLE Invio dei dati al punto operativo.
116400010528     C     opeini        BEGSR
116500010528     C*
116600010528     C* Inizializzo flag e contatori operativi
116700010528     C                   movel     '0'           flgGiro           1
116800010528     C                   movel     '0'           flgMulti          1
116900010528     C                   movel     '1'           flgStato          1
117000010615     C                   movel     '0'           flgOk             1
117100010528     C                   z-add     *zeros        cntNonEl         10 0
117200010528     C                   z-add     *zeros        depfil            3 0
117300010528     C                   z-add     *zeros        invfil            3 0
117400030407     C*
117500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
117600030407     C                   if        %subst(vlrppt:1:1) = 'S'
117700030407     C                   seton                                        50
117800030407     C                   else
117900030407     C                   setoff                                       50
118000030407     C                   endif
118100070925     C*
118200070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
118300070925     C                   if        %subst(vlrppt:2:1) = 'S'
118400070925     C                   seton                                        51
118500070925     C                   else
118600070925     C                   setoff                                       51
118700070925     C                   endif
118800010528     C*
118900010528     C                   ENDSR
119000010528     C***
119100060306
119200060306
119300060306     C*-------------------------------------------------------------------------*
119400060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
119500060306     C*-------------------------------------------------------------------------*
119600060306     C     sr_Control    begsr
119700060306     C*
119800060306     C* inizializzo a ok il flag d controllo abilitazione
119900060306     C                   eval      Cli_Abil = *on
120000060306     C
120100060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
120200060306     C                   clear                   woriIDC
120300060306     C                   eval      kteCOD = 'VAP'
120400060306     C                   eval      kteKE1 = oriIDC
120500060306     C     keytbe        chain     TNTBE01L
120600060306     C                   if        %found(Tntbe01l)
120700060306     C                   eval      woriIDC = TbeKe2
120800060306     C                   else
120900060306     C                   eval      Cli_Abil = *off
121000060306     C                   endif
121100060306     C*
121200060306     C                   endsr
121300010528
121400010528
121500010528
121600010528
121700000613     C     *inzsr        BEGSR
121800990910     C*
121900990910     C     *entry        plist
122000990920     C                   parm                    tivlrds
122100990921     C                   parm      wrkesito      esito
122200000724     C                   parm                    prmlit
122300000710     C                   parm                    prmfir
122400011112     C*
122500011112     C* Campi riferito al database
122600011112     C     *like         define    tbecod        kteCOD                         *TNTBE01L
122700011112     C     *like         define    tbeke1        kteKE1
122800011112     C*
122900011112     C* Definizione chiavi di lettura
123000011112     C* TNTBE01L - parziale
123100011112     C     keytbe        klist
123200011112     C                   kfld                    kteCOD                         *UTENTE
123300011112     C                   kfld                    kteKE1                         *CODICE
123400070926     C* TIVSS02L - parziale
123500070926     C     keyvss02_p    klist
123600070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
123700070926     C                   kfld                    vssISV                         *SERVIZIO
123800011112     C*
123900011112     C* Reperimento data e ora corrente
124000011112     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
124100011112     C                   movel     wn14          wn6               6 0          *ORA (6)
124200011112     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
124300011112     C                   z-add     wn8           g08dat
124400011112     C                   z-add     *zeros        g08inv
124500011112     C                   movel     '0'           g08err
124600011112     C                   call      'XSRDA8'
124700011112     C                   parm                    wlbda8
124800011112     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
124900010529     C*
125000000613     C                   ENDSR
125100000613     C***
