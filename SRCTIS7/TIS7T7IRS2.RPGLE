000100010122      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400030624     Ffnacr01l  if   e           k disk
000500070926     Ftivss02l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800010122     FFNVAOwwr  O    E             DISK    usropn
000900011105     Ftiori00f  O    E             DISK
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
003600151106     D fnvaoeiir     e ds                  inz
003700151106     D                                     prefix(a_)
003800160601     D UBORM_pIn_OPZ   s              1    inz
003900160601     D UBORM_pOut_CNT  s              3S 0 inz
004000160601     D UBORM_pOut_ESI  s              1    inz
004100160601     D*
004200031014     D tisi95ds      e ds
004300030624     D dorm01        e ds
004400011119     D dfar          e ds
004500090617     d trul33ds      e ds
004600090617     d kpjba         e ds
004700990910     D esito           s              1
004800000724     D prmlit          s             10
004900000710     D prmfir          s             10
005000990921     D wrkesito        s                   like(esito)
005100000613     D rrnum           s              6  0 INZ(*zeros)
005200060306     D Cli_Abil        s              1    inz(*off)
005300060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005400061031     D ConstINO        s             15    inz('ORM DA INTERNET')
005500090617     d §numori         s              7  0 inz
005600011122     D*-------------------
005700010731     D* COSTANTI
005800011122     D*-------------------
005900010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100990908
006200091026
006300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006500091026
006600010528
006700010528
006800010528
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000724     C*
007300010528     C                   exsr      opeini
007400010528     C                   exsr      rwvao
007500010528     C*
007600010528     C                   seton                                        lr
007700010528
007800010528
007900010528
008000010528
008100010528     C*--------------------------------------------------------
008200010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010528     C*--------------------------------------------------------
008400010528     C     PREELA        BEGSR
008500010528     C*
008600000724     C* SE OCCORRE SPEDIRE IN FILIALE
008700010528     C                   if        invfil <> *zeros and
008800010528     C                             flgGiro = '0'
008900010528     C*
009000010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010528     C                   eval      flgGiro = '1'
009200010529     C*
009300010529     C                   endif
009400010528     C*
009500010528     C                   ENDSR
009600010528     C***
009700010528
009800010528
009900010528
010000010528     C*--------------------------------------------------------
010100010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010528     C*--------------------------------------------------------
010300010528     C     ENDELA        BEGSR
010400020917     C*
010500010528     C                   ENDSR
010600010528     C***
010700010528
010800010528
010900010528
011000910830     C*--------------------------------------------------------
011100010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
011200910830     C*--------------------------------------------------------
011300010122     C     RWVAO         BEGSR
011400010528     C*
011500990914     C                   if        not %open(tivin00r)
011600990908     C                   open      tivin00r
011700990914     C                   endif
011800010122     C                   if        not %open(fnvaowwr)
011900010122     C                   open      fnvaowwr
012000990914     C                   endif
012100140416     C*
012200140416     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
012300151106     C                   call      'TIS7T7IIR'
012400140416     C                   parm      'O'           pIn_Opz           1
012500140416     C                   parm                    tivlrds
012600151106     C                   parm                    FNVAOEIIR
012700140416     C                   parm                    oriPRG
012800140528     C                   parm      *zeros        pIn_Dst           3
012900140416     C                   parm      *blanks       pOut_Esito        1
013000140416     C                   if        pOut_Esito = '2'
013100140416     C                   goto      FINE
013200140416     C                   endif
013300990910     C*
013400990910     C                   clear                   §CTROK
013500990910     C                   clear                   §CTRMO
013600990910     C                   clear                   §CTRNO
013700990910     C*
013800921023     C                   DO        *HIVAL
013900990913     C*
014000990915     C                   READ      tivin00r                               70
014100010731     C*
014200010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
014300010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014400010618     C*
014500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014600010618     C                   if        vinflg = '1'
014700010618     C                   eval      flgOk = '1'
014800010618     C                   endif
014900010618     C*
015000000905     C                   if        vindta > *blanks
015100000613     C                   add       1             rrnum
015200990913     C*
015300010711     C                   if        *in70 = *off and
015400010711     C                             (vinflg = *blanks
015500010711     C                              or vinflg = '0'
015600010711     C                              or vinflg = '2')
015700990913     C*
015800010528     C                   clear                   FNVAO000
015900011105     C                   clear                   TIORI000
016000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016100010711     C                   if        vinflg = *blanks or vinflg = '0'
016200010711     C                   clear                   vinmsg
016300010711     C                   endif
016400010711     C*
016500151106     C                   eval      fnvaoeiir = vindta
016600160601     C*
016700160601     C* Verifico univocità "fisica" ORM corrente
016800160601     C                   call      'UBORMCNTR'
016900160601     C                   parm      '1'           UBORM_pIn_OPZ
017000160601     C                   parm                    fnvaoeiir
017100160601     C                   parm      *zeros        UBORM_pOut_CNT
017200160601     C                   parm      *blanks       UBORM_pOut_ESI
017300160601     C*
017400160601     C* Se ORM "fisicamente" duplicato lo marco a warning e non lo acquisisco
017500160601     C                   if         UBORM_pOut_ESI = 'E' OR
017600160601     C                             (UBORM_pOut_ESI = '1' AND
017700160601     C                              UBORM_pOut_CNT > 0)
017800160601     C                   seton                                        31
017900160601     C                   else
018000151019     C*
018100151019     C* Stacco subito un numeratore univoco "ORM"
018200151106     C                   exsr      RTVNUM
018300080521     C*
018400080521     C* Verifico se richiesto nei parametri anagrafica cliente/tipo-file d particolari
018500080521     C* impostazioni dei flag operativi
018600080521     C                   if        %subst(vlrppt:3:1) = 'S'
018700080521     C                   eval      dorm01 = a_vaoFLO
018800080521     C                   eval      §ormks = 'S'
018900080521     C                   eval      a_vaoFLO = dorm01
019000080521     C                   endif
019100991022     C*
019200010122     C                   testb     x'3F'         a_vaoatb                 02
019300991022     C                   if        not *in02
019400991022     C*
019500971021     C                   SETOFF                                       303132
019600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019800971023     C*    OPPURE TUTTI BLANKS
019900010122     C*-------------------
020000010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
020100010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
020200010122     C*-------------------
020300010122     C*
020400011119 xxx C*                  TESTN                   a_vaopoe             30
020500010417  "  C* N30              TESTN                   a_vaopoe               30
020600010417  "  C* N30              TESTN                   a_vaopoe                 30
020700010417     C*                  MOVE      a_vaopoe      W001A             1
020800010417     C*    *IN30         IFEQ      *OFF
020900010417     C*    W001A         ORLT      '0'
021000010417     C*                  SETON                                        32
021100010417     C*                  MOVE      *ZEROS        VAOPOE
021200010417     C*                  EVAL      vinmsg = 'VAOPOE'
021300010417     C*                  ELSE
021400010417     C*                  MOVE      a_vaopoe      VAOPOE
021500010417     C*                  ENDIF
021600010417  "  C*
021700010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
021800010604 xxx C                   IF        vlrpoi <> 999
021900010528     C                   EVAL      VAOPOE = vlrpoi
022000150331     C                   ELSE
022100150331     C*
022200150331     C                   IF        VAOPOE <> 999
022300150331     C*
022400150331     C* se i char dal 5° al 7° dei parametri è <> blank ed è numerico, allora è VAOPOE
022500150331     C                   IF        %subst(vlrppt:5:3) <> *blank
022600150331     C                   EVAL      PiStr=%trim(%subst(vlrppt:5:3))
022700150331     C                   EXSR      CHKNUM
022800150331     C                   IF        PiInt=*on
022900150331     C                   Z-ADD     PiVal         VAOPOE
023000150331     C                   ENDIF
023100150331     C                   ELSE
023200150331     C*
023300010528     C                   IF        a_vaopoe = *blanks or
023400011010     C                             a_vaopoe = *zeros  or
023500011010     C                             a_vaopoe = ' 000'
023600010528     C                   IF        a_vaopor <> *blanks and
023700011010     C                             a_vaopor <> *zeros  and
023800011010     C                             a_vaopor <> ' 000'
023900010528     C                   MOVE      a_vaopor      VAOPOE
024000010528     C                   ELSE
024100011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
024200010528     C                   MOVE      a_vaopoe      VAOPOE
024300010528     C                   ENDIF
024400010529     C                   ELSE
024500010529     C                   MOVE      a_vaopoe      VAOPOE
024600010528     C                   ENDIF
024700010528     C                   ENDIF
024800150331     C                   ENDIF
024900150331     C                   ENDIF
025000010122     C*
025100010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025200010528     C                   eval      depfil = VAOPOE
025300010528     C                   exsr      repfil
025400010528     C                   if        depfil = invfil and
025500010528     C                             (vinflg = *blanks
025600010528     C                              or vinflg = '0'
025700010528     C                              or vinflg = '2')
025800010528     C*
025900010528     C                   exsr      PREELA
026000010710     C*
026100010719     C*                  TESTN                   a_vaonsr             30
026200010719     C* N30              TESTN                   a_vaonsr               30
026300010719     C* N30              TESTN                   a_vaonsr                 30
026400010719     C*                  MOVE      a_vaonsr      W001A             1
026500010719     C*    *IN30         IFEQ      *OFF
026600010719     C*    W001A         ORLT      '0'
026700010719     C*                  SETON                                        32
026800010719     C*                  MOVE      *ZEROS        VAONSR
026900010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
027000010719     C*                  ELSE
027100010719     C*                  MOVE      a_vaonsr      VAONSR
027200010719     C*                  ENDIF
027300010122     C*
027400010719     C*                  TESTN                   a_vaonor             30
027500010719     C* N30              TESTN                   a_vaonor               30
027600010719     C* N30              TESTN                   a_vaonor                 30
027700010719     C*                  MOVE      a_vaonor      W001A             1
027800010719     C*    *IN30         IFEQ      *OFF
027900010719     C*    W001A         ORLT      '0'
028000010719     C*                  SETON                                        32
028100010719     C*                  MOVE      *ZEROS        VAONOR
028200010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
028300010719     C*                  ELSE
028400010719     C*                  MOVE      a_vaonor      VAONOR
028500010719     C*                  ENDIF
028600010122     C*
028700010719     C*                  TESTN                   a_vaonrv             30
028800010719     C* N30              TESTN                   a_vaonrv               30
028900010719     C* N30              TESTN                   a_vaonrv                 30
029000010719     C*                  MOVE      a_vaonrv      W001A             1
029100010719     C*    *IN30         IFEQ      *OFF
029200010719     C*    W001A         ORLT      '0'
029300010719     C*                  SETON                                        32
029400010719     C*                  MOVE      *ZEROS        VAONRV
029500010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
029600010719     C*                  ELSE
029700010719     C*                  MOVE      a_vaonrv      VAONRV
029800010719     C*                  ENDIF
029900010122     C*
030000010122     C                   TESTN                   a_vaodao             30
030100010122     C  N30              TESTN                   a_vaodao               30
030200010122     C  N30              TESTN                   a_vaodao                 30
030300010122     C                   MOVE      a_vaodao      W001A             1
030400010122     C     *IN30         IFEQ      *OFF
030500010122     C     W001A         ORLT      '0'
030600010122     C                   SETON                                        32
030700050421     C                   MOVE      *ZEROS        VAODAO
030800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
030900010122     C                   ELSE
031000050421     C                   MOVE      a_vaodao      VAODAO
031100010122     C                   ENDIF
031200010122     C*
031300010122     C                   TESTN                   a_vaooao             30
031400010122     C  N30              TESTN                   a_vaooao               30
031500010122     C  N30              TESTN                   a_vaooao                 30
031600010122     C                   MOVE      a_vaooao      W001A             1
031700010122     C     *IN30         IFEQ      *OFF
031800010122     C     W001A         ORLT      '0'
031900010122     C                   SETON                                        32
032000050421     C                   MOVE      *ZEROS        VAOOAO
032100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
032200010122     C                   ELSE
032300050421     C                   MOVE      a_vaooao      VAOOAO
032400010122     C                   ENDIF
032500010122     C*
032600010122     C                   TESTN                   a_vaocor             30
032700010122     C  N30              TESTN                   a_vaocor               30
032800010122     C  N30              TESTN                   a_vaocor                 30
032900010122     C                   MOVE      a_vaocor      W001A             1
033000010122     C     *IN30         IFEQ      *OFF
033100010122     C     W001A         ORLT      '0'
033200010122     C                   SETON                                        32
033300010122     C                   MOVE      *ZEROS        VAOCOR
033400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
033500010122     C                   ELSE
033600050421     C                   MOVE      a_vaocor      VAOCOR
033700010122     C                   ENDIF
033800010122     C*
033900010122     C                   TESTN                   a_vaocra             30
034000010122     C  N30              TESTN                   a_vaocra               30
034100010122     C  N30              TESTN                   a_vaocra                 30
034200010122     C                   MOVE      a_vaocra      W001A             1
034300010122     C     *IN30         IFEQ      *OFF
034400010122     C     W001A         ORLT      '0'
034500010122     C                   SETON                                        32
034600010122     C                   MOVE      *ZEROS        VAOCRA
034700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
034800010122     C                   ELSE
034900010122     C                   MOVE      a_vaocra      VAOCRA
035000010122     C                   ENDIF
035100010122     C*
035200010719     C                   IF        a_vaocrc = *blanks or
035300010719     C                             a_vaocrc = *zeros
035400010719     C                   ELSE
035500010719     C                   TESTN                   a_vaocrc             30
035600010719     C  N30              TESTN                   a_vaocrc               30
035700010719     C  N30              TESTN                   a_vaocrc                 30
035800010719     C                   MOVE      a_vaocrc      W001A             1
035900010719     C     *IN30         IFEQ      *OFF
036000010719     C     W001A         ORLT      '0'
036100010719     C                   SETON                                        32
036200010719     C                   MOVE      *ZEROS        VAOCRC
036300010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
036400010719     C                   ELSE
036500010719     C                   MOVE      a_vaocrc      VAOCRC
036600010719     C                   ENDIF
036700010719     C                   ENDIF
036800010122     C*
036900010122     C                   TESTN                   a_vaodar             30
037000010122     C  N30              TESTN                   a_vaodar               30
037100010122     C  N30              TESTN                   a_vaodar                 30
037200010122     C                   MOVE      a_vaodar      W001A             1
037300010122     C     *IN30         IFEQ      *OFF
037400010122     C     W001A         ORLT      '0'
037500010122     C                   SETON                                        32
037600010122     C                   MOVE      *ZEROS        VAODAR
037700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
037800010122     C                   ELSE
037900010122     C                   MOVE      a_vaodar      VAODAR
038000010122     C                   ENDIF
038100010122     C*
038200010122     C                   TESTN                   a_vaoorr             30
038300010122     C  N30              TESTN                   a_vaoorr               30
038400010122     C  N30              TESTN                   a_vaoorr                 30
038500010122     C                   MOVE      a_vaoorr      W001A             1
038600010122     C     *IN30         IFEQ      *OFF
038700010122     C     W001A         ORLT      '0'
038800010122     C                   SETON                                        32
038900010122     C                   MOVE      *ZEROS        VAOORR
039000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
039100010122     C                   ELSE
039200010122     C                   MOVE      a_vaoorr      VAOORR
039300010122     C                   ENDIF
039400010122     C*
039500010122     C                   TESTN                   a_vaoncl             30
039600010122     C  N30              TESTN                   a_vaoncl               30
039700010122     C  N30              TESTN                   a_vaoncl                 30
039800010122     C                   MOVE      a_vaoncl      W001A             1
039900010122     C     *IN30         IFEQ      *OFF
040000010122     C     W001A         ORLT      '0'
040100010122     C                   SETON                                        32
040200010122     C                   MOVE      *ZEROS        VAONCL
040300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
040400010122     C                   ELSE
040500010122     C                   MOVE      a_vaoncl      VAONCL
040600010122     C                   ENDIF
040700010122     C*
040800010122     C                   MOVE      *ZERO         VAOPKG
040900010122     C                   MOVE      a_vaopkg      WASC09
041000010122     C                   TESTN                   WI09                 30
041100010122     C  N30              TESTN                   WI09                   30
041200010122     C  N30              TESTN                   WI09                     30
041300010122     C                   MOVE      WI09          W001A             1
041400010122     C     *IN30         IFEQ      *OFF
041500010122     C     W001A         ORLT      '0'
041600010122     C     W001A         ANDNE     *BLANK
041700010122     C                   SETON                                        32
041800010122     C                   MOVE      *ZERO         VAOPKG
041900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
042000010122     C                   ELSE
042100010122     C                   MOVEL     WI09          VAOPKG
042200010122     C                   TESTN                   WD09                 30
042300010122     C  N30              TESTN                   WD09                     30
042400010122     C                   MOVE      WD09          W001A             1
042500010122     C     *IN30         IFEQ      *OFF
042600010122     C     W001A         ORLT      '0'
042700010122     C     W001A         ANDNE     *BLANK
042800010122     C                   SETON                                        32
042900010122     C                   MOVE      *ZERO         VAOPKG
043000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
043100010122     C                   ELSE
043200010122     C                   MOVE      WD09          VAOPKG
043300010122     C                   ENDIF
043400050118     C                   ENDIF
043500010122     C*
043600010122     C                   MOVE      *ZERO         VAOVLM
043700010122     C                   MOVE      a_vaovlm      WASC07
043800010122     C                   TESTN                   WI07                 30
043900010122     C  N30              TESTN                   WI07                   30
044000010122     C  N30              TESTN                   WI07                     30
044100010122     C                   MOVE      WI07          W001A             1
044200010122     C     *IN30         IFEQ      *OFF
044300010122     C     W001A         ORLT      '0'
044400010122     C     W001A         ANDNE     *BLANK
044500010122     C                   SETON                                        32
044600010122     C                   MOVE      *ZERO         VAOVLM
044700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
044800010122     C                   ELSE
044900010122     C                   MOVEL     WI07          VAOVLM
045000010122     C                   TESTN                   WD07                 30
045100010122     C  N30              TESTN                   WD07                   30
045200010122     C  N30              TESTN                   WD07                     30
045300010122     C                   MOVE      WD07          W001A             1
045400010122     C     *IN30         IFEQ      *OFF
045500010122     C     W001A         ORLT      '0'
045600010122     C     W001A         ANDNE     *BLANK
045700010122     C                   SETON                                        32
045800010122     C                   MOVE      *ZERO         VAOVLM
045900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
046000010122     C                   ELSE
046100010122     C                   MOVE      WD07          VAOVLM
046200010122     C                   ENDIF
046300050118     C                   ENDIF
046400010122     C*
046500010122     C                   TESTN                   a_vaobnc             30
046600010122     C  N30              TESTN                   a_vaobnc               30
046700010122     C  N30              TESTN                   a_vaobnc                 30
046800010122     C                   MOVE      a_vaobnc      W001A             1
046900010122     C     *IN30         IFEQ      *OFF
047000010122     C     W001A         ORLT      '0'
047100010122     C                   SETON                                        32
047200010122     C                   MOVE      *ZEROS        VAOBNC
047300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
047400010122     C                   ELSE
047500010122     C                   MOVE      a_vaobnc      VAOBNC
047600010122     C                   ENDIF
047700010122     C*
047800010122     C                   TESTN                   a_vaoblc             30
047900010122     C  N30              TESTN                   a_vaoblc               30
048000010122     C  N30              TESTN                   a_vaoblc                 30
048100010122     C                   MOVE      a_vaoblc      W001A             1
048200010122     C     *IN30         IFEQ      *OFF
048300010122     C     W001A         ORLT      '0'
048400010122     C                   SETON                                        32
048500010122     C                   MOVE      *ZEROS        VAOBLC
048600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
048700010122     C                   ELSE
048800010122     C                   MOVE      a_vaoblc      VAOBLC
048900010122     C                   ENDIF
049000010122     C*
049100010122     C                   TESTN                   a_vaoatt             30
049200010122     C  N30              TESTN                   a_vaoatt               30
049300010122     C  N30              TESTN                   a_vaoatt                 30
049400010122     C                   MOVE      a_vaoatt      W001A             1
049500010122     C     *IN30         IFEQ      *OFF
049600010122     C     W001A         ORLT      '0'
049700010122     C                   SETON                                        32
049800010122     C                   MOVE      *ZEROS        VAOATT
049900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
050000010122     C                   ELSE
050100010122     C                   MOVE      a_vaoatt      VAOATT
050200010122     C                   ENDIF
050300010122     C*
050400010122     C                   TESTN                   a_vaomtc             30
050500010122     C  N30              TESTN                   a_vaomtc               30
050600010122     C  N30              TESTN                   a_vaomtc                 30
050700010122     C                   MOVE      a_vaomtc      W001A             1
050800010122     C     *IN30         IFEQ      *OFF
050900010122     C     W001A         ORLT      '0'
051000010122     C                   SETON                                        32
051100010122     C                   MOVE      *ZEROS        VAOMTC
051200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
051300010122     C                   ELSE
051400010122     C                   MOVE      a_vaomtc      VAOMTC
051500010122     C                   ENDIF
051600010122     C*
051700010719     C                   IF        a_vaoksc = *blanks or
051800010719     C                             a_vaoksc = *zeros
051900010719     C                   ELSE
052000010719     C                   TESTN                   a_vaoksc             30
052100010719     C  N30              TESTN                   a_vaoksc               30
052200010719     C  N30              TESTN                   a_vaoksc                 30
052300010719     C                   MOVE      a_vaoksc      W001A             1
052400010719     C     *IN30         IFEQ      *OFF
052500010719     C     W001A         ORLT      '0'
052600010719     C                   SETON                                        32
052700010719     C                   MOVE      *ZEROS        VAOKSC
052800010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
052900010719     C                   ELSE
053000010719     C                   MOVE      a_vaoksc      VAOKSC
053100010719     C                   ENDIF
053200010719     C                   ENDIF
053300010122     C*
053400010710     C                   IF        a_vaoctr > *blanks
053500010122     C                   TESTN                   a_vaoctr             30
053600010122     C  N30              TESTN                   a_vaoctr               30
053700010122     C  N30              TESTN                   a_vaoctr                 30
053800010122     C                   MOVE      a_vaoctr      W001A             1
053900010122     C     *IN30         IFEQ      *OFF
054000010122     C     W001A         ORLT      '0'
054100010122     C                   SETON                                        32
054200010627     C                   MOVE      *blanks       VAOCTR
054300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
054400010122     C                   ELSE
054500010122     C                   MOVE      a_vaoctr      VAOCTR
054600010122     C                   ENDIF
054700010710     C                   ENDIF
054800010122     C*
054900010122     C                   TESTN                   a_vaopor             30
055000010122     C  N30              TESTN                   a_vaopor               30
055100010122     C  N30              TESTN                   a_vaopor                 30
055200010122     C                   MOVE      a_vaopor      W001A             1
055300010122     C     *IN30         IFEQ      *OFF
055400010122     C     W001A         ORLT      '0'
055500010122     C                   SETON                                        32
055600010122     C                   MOVE      *ZEROS        VAOPOR
055700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
055800010122     C                   ELSE
055900010122     C                   MOVE      a_vaopor      VAOPOR
056000010122     C                   ENDIF
056100010122     C*
056200010719     C                   IF        a_vaopoc = *blanks or
056300010719     C                             a_vaopoc = *zeros
056400010719     C                   ELSE
056500010122     C                   TESTN                   a_vaopoc             30
056600010122     C  N30              TESTN                   a_vaopoc               30
056700010122     C  N30              TESTN                   a_vaopoc                 30
056800010122     C                   MOVE      a_vaopoc      W001A             1
056900010122     C     *IN30         IFEQ      *OFF
057000010122     C     W001A         ORLT      '0'
057100010122     C                   SETON                                        32
057200010122     C                   MOVE      *ZEROS        VAOPOC
057300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
057400010122     C                   ELSE
057500010122     C                   MOVE      a_vaopoc      VAOPOC
057600010122     C                   ENDIF
057700010719     C                   ENDIF
057800010122     C*
057900010719     C                   IF        a_vaosto = *blanks or
058000010719     C                             a_vaosto = *zeros
058100010719     C                   ELSE
058200010122     C                   TESTN                   a_vaosto             30
058300010122     C  N30              TESTN                   a_vaosto               30
058400010122     C  N30              TESTN                   a_vaosto                 30
058500010122     C                   MOVE      a_vaosto      W001A             1
058600010122     C     *IN30         IFEQ      *OFF
058700010122     C     W001A         ORLT      '0'
058800010122     C                   SETON                                        32
058900010122     C                   MOVE      *ZEROS        VAOSTO
059000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
059100010122     C                   ELSE
059200010122     C                   MOVE      a_vaosto      VAOSTO
059300010122     C                   ENDIF
059400010719     C                   ENDIF
059500971021     C*
059600971021     C* CAMPI ALFABETICI:
059700010122     C                   MOVE      a_vaoatb      VAOATB
059800010122     C                   MOVE      a_vaotor      VAOTOR
059900010531     C*
060000010531     C                   IF        a_vaotco = *blanks
060100010531     C                   MOVE      'F'           VAOTCO
060200010531     C                   ELSE
060300010531     C                   MOVE      a_vaotco      VAOTCO
060400010531     C                   ENDIF
060500010531     C*
060600010122     C                   MOVE      a_vaorso      VAORSO
060700010122     C                   MOVE      a_vaoino      VAOINO
060800010122     C                   MOVE      a_vaocao      VAOCAO
060900010122     C                   MOVE      a_vaoloo      VAOLOO
061000031014     C                   MOVE      a_vaopro      VAOPRO
061100090129     C                   MOVE      a_vaonao      VAONAO
061200131007     C*
061300131007     C                   MOVE      a_vaorsr      VAORSR
061400131007     C                   MOVE      a_vaoinr      VAOINR
061500131007     C                   MOVE      a_vaocar      VAOCAR
061600131007     C                   MOVE      a_vaolor      VAOLOR
061700131007     C                   MOVE      a_vaoprr      VAOPRR
061800131007     C                   MOVE      a_vaonar      VAONAR
061900131007     C*
062000131007     C                   MOVE      a_vaorer      VAORER
062100131007     C                   MOVE      a_vaoter      VAOTER
062200131007     C                   MOVE      a_vaorsc      VAORSC
062300131007     C                   MOVE      a_vaoinc      VAOINC
062400131007     C                   MOVE      a_vaocac      VAOCAC
062500131007     C                   MOVE      a_vaoloc      VAOLOC
062600131007     C                   MOVE      a_vaoprc      VAOPRC
062700131007     C                   MOVE      a_vaonac      VAONAC
062800131007     C*
062900131007     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
063000131007     C*** VAONAO
063100131007     C                   IF        %trim(VAONAO) = 'I'  OR
063200131007     C                             %trim(VAONAO) = 'IT' OR
063300131007     C                             %trim(VAONAO) = 'VA' OR
063400131007     C                             %trim(VAONAO) = 'SM'
063500131007     C                   EVAL      VAONAO = *blanks
063600131007     C                   ENDIF
063700131007     C*** VAONAR
063800131007     C                   IF        %trim(VAONAR) = 'I'  OR
063900131007     C                             %trim(VAONAR) = 'IT' OR
064000131007     C                             %trim(VAONAR) = 'VA' OR
064100131007     C                             %trim(VAONAR) = 'SM'
064200131007     C
064300131007     C                   EVAL      VAONAR = *blanks
064400131007     C                   ENDIF
064500131007     C*** VAONAC
064600131007     C                   IF        %trim(VAONAC) = 'I'  OR
064700131007     C                             %trim(VAONAC) = 'IT' OR
064800131007     C                             %trim(VAONAC) = 'VA' OR
064900131007     C                             %trim(VAONAC) = 'SM'
065000131007     C                   EVAL      VAONAC = *blanks
065100131007     C                   ENDIF
065200131007     C*
065300031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
065400031014     C                   IF        VAOPRO  = *blanks AND
065500050907     C                             VAOCAO <> *blanks AND
065600050907     C                             VAONAO  = *blanks                            * solo x Italia
065700031014     C                   CLEAR                   TISI95DS
065800031014     C                   EVAL      I95TCN = '3'
065900031014     C                   Z-ADD     datcor        I95DAT
066000031014     C                   EVAL      I95CAP = VAOCAO
066100031014     C                   EVAL      I95LOC = VAOLOO
066200031014     C                   CALL      'TISI95R'
066300031014     C                   PARM                    TISI95DS
066400031014     C                   EVAL      VAOPRO = O95PRV
066500031014     C                   ENDIF
066600131007     C*
066700031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
066800031014     C                   IF        VAOPRR  = *blanks AND
066900050907     C                             VAOCAR <> *blanks AND
067000050907     C                             VAONAR  = *blanks                            * solo x Italia
067100031014     C                   CLEAR                   TISI95DS
067200031014     C                   EVAL      I95TCN = '3'
067300031014     C                   Z-ADD     datcor        I95DAT
067400031014     C                   EVAL      I95CAP = VAOCAR
067500031014     C                   EVAL      I95LOC = VAOLOR
067600031014     C                   CALL      'TISI95R'
067700031014     C                   PARM                    TISI95DS
067800031014     C                   EVAL      VAOPRR = O95PRV
067900031014     C                   ENDIF
068000131007     C*
068100031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
068200031014     C                   IF        VAOPRC  = *blanks AND
068300050907     C                             VAOCAC <> *blanks AND
068400050907     C                             VAONAC  = *blanks                            * solo x Italia
068500031014     C                   CLEAR                   TISI95DS
068600031014     C                   EVAL      I95TCN = '3'
068700031014     C                   Z-ADD     datcor        I95DAT
068800031014     C                   EVAL      I95CAP = VAOCAC
068900031014     C                   EVAL      I95LOC = VAOLOC
069000031014     C                   CALL      'TISI95R'
069100031014     C                   PARM                    TISI95DS
069200031014     C                   EVAL      VAOPRC = O95PRV
069300031014     C                   ENDIF
069400031014     C*
069500010122     C                   MOVE      a_vaoffd      VAOFFD
069600010122     C                   MOVE      a_vaormp      VAORMP
069700010122     C                   MOVE      a_vaonam      VAONAM
069800010122     C                   MOVE      a_vaopag      VAOPAG
069900010122     C                   MOVE      a_vaozor      VAOZOR
070000010122     C                   MOVE      a_vaono1      VAONO1
070100010122     C                   MOVE      a_vaono2      VAONO2
070200010122     C                   MOVE      a_vaoddt      VAODDT
070300010122     C                   MOVE      a_vaorfa      VAORFA
070400010122     C                   MOVE      a_vaospi      VAOSPI
070500010122     C                   MOVE      a_vaoflo      VAOFLO
070600050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
070700050304     C                   IF        VAOPKG =  *zeros AND
070800050304     C                             VAOVLM =  *zeros AND
070900050304     C                             VAOBNC =  *zeros AND
071000050304     C                             VAONCL <> *zeros
071100050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
071200050304     C                   ENDIF
071300030603     C*
071400030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
071500030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
071600030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
071700050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
071800150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
071900930409     C*
072000010604     C  N31              ADD       1             §CTROK            7 0
072100010604     C   32              ADD       1             §CTRMO            7 0
072200010604     C   31              ADD       1             §CTRNO            7 0
072300030624     C  N31              EXSR      CHKCOR
072400011105     C  N31              EXSR      WRIORI
072500050826     C  N31              EXSR      WRIVGD
072600050421     C*
072700050421     C  N31              WRITE     FNVAO000
072800140416     C*
072900151019     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
073000151106     C                   if        not *in31
073100151019     C                   exsr      EXEFORZ
073200151106     C                   call      'TIS7T7IIR'
073300140416     C                   parm      'W'           pIn_Opz           1
073400140416     C                   parm                    tivlrds
073500151106     C                   parm                    FNVAOEIIR
073600140416     C                   parm                    oriPRG
073700140528     C                   parm      *zeros        pIn_Dst           3
073800140416     C                   parm      *blanks       pOut_Esito        1
073900140416     C                   endif
074000140416     C                   if        pOut_Esito = '2'
074100140416     C                   seton                                        32
074200140416     C                   endif
074300990910     C*
074400160601     C                   endif
074500160601     C*
074600010604     C                   if        *in31 = *off and
074700010604     C                             *in32 = *off
074800990910     C                   eval      vinflg = '1'
074900990910     C                   else
075000990910     C                   eval      vinflg = '2'
075100990910     C                   endif
075200991022     C                   endif
075300990910     C                   endif
075400010604     C*
075500010604     C                   endif
075600000905     C*
075700000905     C                   else
075800000905     C                   eval      vinflg = '1'
075900000905     C                   endif
076000000905     C*
076100000905     C  N70              update    tivin000
076200991022     C*
076300991022     C  N70              ENDdo
076400140416     C*
076500140416     C     FINE          tag
076600140528     C*
076700140528     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
076800151106     C                   call      'TIS7T7IIR'
076900140528     C                   parm      'C'           pIn_Opz           1
077000140528     C                   parm                    tivlrds
077100151106     C                   parm                    FNVAOEIIR
077200140528     C                   parm                    oriPRG
077300140528     C                   parm      *zeros        pIn_Dst           3
077400140528     C                   parm      *blanks       pOut_Esito        1
077500010528     C*
077600010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
077700010528     C                   if        cntNonEl = *zeros or
077800010528     C                             flgMulti = '0'
077900010528     C* Se non ci sono record con errori ...
078000010528     C                   if        §ctrno = 0 and
078100010604     C                             §ctrmo = 0 and
078200010528     C                             flgStato <> '2'
078300990910     C* ... restituisco esito OK.
078400990921     C                   eval      wrkesito = '0'
078500990910     C                   else
078600000710     C                   if        §ctrok > 0
078700990921     C                   eval      wrkesito = '1'
078800000710     C                   else
078900010615     C                   if        flgOk = '0'
079000010615     C                   eval      wrkesito = '2'
079100010615     C                   else
079200010615     C                   eval      wrkesito = '6'
079300010615     C                   endif
079400990910     C                   endif
079500010528     C                   endif
079600010529     C                   else
079700010529     C                   eval      wrkesito = '9'
079800000710     C                   endif
079900990910     C*
080000990914     C                   if        %open(tivin00r)
080100990908     C                   close     tivin00r
080200990914     C                   endif
080300010122     C                   if        %open(fnvaowwr)
080400010122     C                   close     fnvaowwr
080500990914     C                   endif
080600990910     C*
080700010528     C                   if        vlrpoi <> 999
080800010528     C                   eval      invfil = vlrpoi
080900010528     C                   endif
081000010528     C*
081100990920     C                   if        §ctrok > 0
081200010528     C                             and invfil > *zeros
081300140528     C                   exsr      invio
081400140528     C*
081500140528     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
081600140528     C                   move      invfil        cmzdst
081700151106     C                   call      'TIS7T7IIR'
081800140528     C                   parm      'S'           pIn_Opz           1
081900140528     C                   parm                    tivlrds
082000151106     C                   parm                    FNVAOEIIR
082100140528     C                   parm                    oriPRG
082200140528     C                   parm      cmzdst        pIn_Dst           3
082300140528     C                   parm      *blanks       pOut_Esito        1
082400990920     C                   endif
082500010612     C*
082600010612     C                   if        flgGiro = '1'
082700010612     C                   exsr      endela
082800010612     C                   endif
082900010528     C*
083000910830     C                   ENDSR
083100000613     C***
083200140417
083300140417
083400140417     C*------------------------------------------------------------------------*
083500151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
083600140417     C*------------------------------------------------------------------------*
083700151019     C     RTVNUM        BEGSR
083800140417     C*
083900151019     C* Stacco un numeratore da AZNUM
084000151019     C                   clear                   §numori
084100140417     C                   clear                   trul33ds
084200140417     C                   eval      I33OPE = *zeros
084300140417     C                   eval      I33CNU = 600
084400140417     C                   eval      I33NUM = 1
084500140417     C                   movel     TRUL33DS      KPJBU
084600140417     C                   call      'TRUL33R'
084700140417     C                   parm                    KPJBA
084800140417     C                   movel     KPJBU         TRUL33DS
084900140417     C                   if        O33ERR = *zeros
085000140417     C                   z-add     O33NRF        §numori
085100140417     c                   else
085200140417     c                   eval      §numori = *all'9'
085300140417     C                   seton                                        31        * errore bloccante
085400140417     C                   endif
085500140417     C*
085600151106     C                   eval      vaoNOR = §numori
085700140417     C*
085800140417     C                   ENDSR
085900151019
086000151019
086100151019     C*------------------------------------------------------------------------*
086200151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
086300151019     C*------------------------------------------------------------------------*
086400151106     C     EXEFORZ       BEGSR
086500151019     C*
086600151106     C                   move(p)   VAOPOE        a_vaopoe                       * forzatura necessar
086700151106     C                   move(p)   VAOCOR        a_vaocor                       * forzatura necessar
086800151106     C                   move(p)   VAONOR        a_vaonor                       * forzatura necessar
086900151019     C*
087000151019     C                   ENDSR
087100140417
087200150319
087300150319     C*----------------------------------------------------*
087400150319     C*  CONTROLLO NUMERICITA' CAMPI
087500150319     C*----------------------------------------------------*
087600150319     C     CHKNUM        BEGSR
087700150319     C*
087800150319     C                   IF        PiDecChr = *blanks
087900150319     C                   EVAL      PiDecChr = '.'
088000150319     C                   ENDIF
088100150319     C*
088200150319     C                   CALL(e)   'ISNUMERIC'
088300150319     C                   PARM                    PiStr            30
088400150319     C                   PARM                    PiDecChr          1
088500150319     C                   PARM      *ZEROS        PiVal            30 9
088600150319     C                   PARM      '0'           PiInt             1
088700150319     C                   PARM      '0'           PiNum             1
088800150319     C                   IF        %error
088900150319     C                   EVAL      PiNum=*off
089000150319     C                   ENDIF
089100150319     C*
089200150319     C                   ENDSR
089300150319     C***
089400030624
089500030624
089600030624     C*------------------------------------------------------------------------*
089700030624     C* CHKCOR - Considerazioni su VAOCOR
089800030624     C*------------------------------------------------------------------------*
089900030624     C     CHKCOR        BEGSR
090000030624     C*
090100030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
090200030624     C                   if        vaoTCO = 'F'
090300030624     C* Quindi verifico se il codice ordinante non è valorizzato
090400050323     C                   if        vaoCOR = *zeros
090500050323     C*                  if        vaoCOR = *zeros  AND
090600050323     C*                            vaoRSO = *blanks AND
090700050323     C*                            vaoINO = *blanks AND
090800050323     C*                            vaoCAO = *blanks AND
090900050323     C*                            vaoLOO = *blanks AND
091000050323     C*                            vaoPRO = *blanks AND
091100050323     C*                            vaoNAO = *blanks
091200030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
091300030624     C                   move      vlrKSC        wCli              7 0
091400050323     C                   movel(p)  wCli          acrCRO
091500030624     C     acrCRO        setll     fnacr01l
091600030624     C                   if        %found(fnacr01l)
091700030624     C                   read      fnacr01l
091800030624     C                   dow       not %eof(fnacr01l)
091900030624     C                   movel     acrCRO        wCliAcr           7 0
092000030624     C                   if        wCliAcr = wCli
092100030624     C                   eval      vaoCOR = acrCRO
092200030624     C                   leave
092300030624     C                   else
092400030624     C                   leave
092500030624     C                   endif
092600030624     C                   read      fnacr01l
092700030624     C                   enddo
092800030624     C                   endif
092900030624     C                   endif
093000030624     C                   endif
093100030624     C*
093200030624     C                   if        vaoTCO = 'I'
093300030624     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
093400030624     C                   if        vlrKSC = '0ORM0000'
093500030624     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
093600061031     C***                eval      vaoINO = 'ORM DA INTERNET'
093700061031     C                   movel     ConstINO      vaoINO
093800030624     C                   eval      vaoCAO = '40138'
093900030624     C                   eval      vaoLOO = 'BOLOGNA'
094000030624     C                   eval      vaoPRO = 'BO'
094100030624     C                   endif
094200030624     C                   endif
094300030624     C*
094400030624     C                   ENDSR
094500030624
094600990920
094700010528
094800011105     C*------------------------------------------------------------------------*
094900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
095000011105     C*------------------------------------------------------------------------*
095100011105     C     WRIORI        BEGSR
095200011105     C*
095300090617     C* Come prima cosa stacco un numeratore da AZNUM
095400151019     C***                clear                   trul33ds
095500151019     C***                eval      I33OPE = *zeros
095600151019     C***                eval      I33CNU = 600
095700151019     C***                eval      I33NUM = 1
095800151019     C***                movel     TRUL33DS      KPJBU
095900151019     C***                call      'TRUL33R'
096000151019     C***                parm                    KPJBA
096100151019     C***                movel     KPJBU         TRUL33DS
096200151019     C***                if        O33ERR = *zeros
096300151019     C***                z-add     O33NRF        §numori
096400151019     c***                else
096500151019     C***                eval      §numori = *all'9'
096600151019     C***                endif
096700070925     C*
096800070925     C* Giro "standard" (ovvero x cliente d invio dati)
096900070925     C                   if        *in51 = *off
097000011105     C*
097100011122     C* Quindi imposto i campi "extra-VAO"
097200011122     C                   if        vlrKSC = '0ORM0000'
097300011122     C*
097400011122     C                   if        vaoCOR > *zeros
097500011122     C                   movel     vaoCOR        wrkCOR7           7 0
097600011122     C*
097700011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
097800011122     C* codice padre. Se è un padre assegno già il codice ordinante.
097900011122     C                   clear                   tibs10ds
098000011122     C                   eval      d10drf = datcor                              *data riferimento
098100011122     C                   eval      d10paf = 'P'                                 *cerca il padre
098200011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
098300091026     C*
098400091026     C* Preparazione chiamata al TIBS10R1
098500091026     C                   clear                   tibs10r1ds
098600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
098700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
098800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
098900091026     C*
099000091026     C                   call      'TIBS10R1'
099100091026     C                   parm                    tibs10r1ds
099200091026     C*
099300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
099400011122     C*
099500011122     C                   if        d10err <> *blanks or                         *No unif. o padre
099600011122     C                             d10cop =  *zeros  or
099700011122     C                             d10cop =  d10cod
099800011122     C                   move      d10cod        oriIDC
099900011122     C                   else                                                   *Figlio
100000011122     C                   move      d10cop        oriIDC
100100011122     C                   endif
100200011122     C*
100300011122     C                   else
100400011122     C                   eval      oriIDC = *zeros
100500011122     C                   endif
100600011122     C*
100700011122     C                   else
100800011122     C                   eval      oriIDC = vlrKSC
100900011122     C                   endif
101000070925     C*
101100070925     C* Giro "forzato" (ovvero x codice ordinante)
101200070925     C                   else
101300070925     C*
101400070925     C                   if        vaoCOR > *zeros
101500070925     C                   movel     vaoCOR        wrkCOR7           7 0
101600070925     C*
101700070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
101800070925     C* codice padre. Se è un padre assegno già il codice ordinante.
101900070925     C                   clear                   tibs10ds
102000070925     C                   eval      d10drf = datcor                              *data riferimento
102100070925     C                   eval      d10paf = 'P'                                 *cerca il padre
102200070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
102300091026     C*
102400091026     C* Preparazione chiamata al TIBS10R1
102500091026     C                   clear                   tibs10r1ds
102600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
102700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
102800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
102900091026     C*
103000091026     C                   call      'TIBS10R1'
103100091026     C                   parm                    tibs10r1ds
103200091026     C*
103300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
103400070925     C*
103500070925     C                   if        d10err <> *blanks or                         *No unif. o padre
103600070925     C                             d10cop =  *zeros  or
103700070925     C                             d10cop =  d10cod
103800070925     C                   move      d10cod        oriIDC
103900070925     C                   else                                                   *Figlio
104000070925     C                   move      d10cop        oriIDC
104100070925     C                   endif
104200070926     C*
104300070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
104400070926     C* al servizio d rintraccia ritiri on-line
104500070926     C                   eval      vssKSU = oriIDC
104600070926     C                   eval      vssISV = 'T3'
104700070926     C     keyvss02_p    chain     tivss02l
104800070926     C                   if        %found(tivss02l)
104900070926     C                   if        vssdde <= datcor AND
105000070926     C                             datcor <= vssdsc
105100070926     C                   else
105200070926     C                   eval      oriIDC = vlrKSC
105300070926     C                   endif
105400070926     C                   else
105500070926     C                   eval      oriIDC = vlrKSC
105600070926     C                   endif
105700070926     C*
105800070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
105900070926     C* anagrafica clienti ritiro
106000070926     C                   setoff                                       52
106100070926     C                   eval      acrCRO = wrkCOR7 * 1000
106200070926     C     acrCRO        setll     fnacr01l
106300070926     C                   if        %found(fnacr01l)
106400070926     C                   read      fnacr01l
106500070926     C                   dow       not %eof(fnacr01l)
106600070926     C                   movel     acrCRO        wCliAcr           7 0
106700070926     C                   if        wCliAcr = wrkCOR7
106800070926     C                   seton                                        52
106900070926     C                   leave
107000070926     C                   else
107100070926     C                   leave
107200070926     C                   endif
107300070926     C                   read      fnacr01l
107400070926     C                   enddo
107500070926     C                   endif
107600070926     C*
107700070926     C                   if        *in52  = *off
107800070926     C                   eval      oriIDC = vlrKSC
107900070926     C                   endif
108000070925     C*
108100070925     C                   else
108200070925     C                   if        vlrKSC = '0ORM0000'
108300070925     C                   eval      oriIDC = *zeros
108400070925     C                   else
108500070925     C                   eval      oriIDC = vlrKSC
108600070925     C                   endif
108700070925     C                   endif
108800070925     C                   endif
108900011122     C*
109000011105     C                   eval      oriPRG = §numori
109100011105     C                   eval      oriDER = datcor
109200011105     C                   eval      oriOER = wn6
109300011105     C                   eval      oriPOE = vaoPOE
109400011105     C                   eval      oriNSR = vaoNSR
109500011105     C                   eval      oriNOR = vaoNOR
109600011105     C                   eval      oriNRV = vaoNRV
109700011105     C                   eval      oriFLO = *blanks
109800011105     C*
109900011122     C* Infine valorizzo la chiave esterna sul file FNVAO
110000011105     C                   eval      dorm01 = vaoFLO
110100011105     C                   movel     §numori       §ormpg
110200011105     C                   eval      vaoFLO = dorm01
110300011105     C*
110400060306     C* ...quindi scrivo il file TIORI00F
110500121205     C  N55              WRITE     TIORI000
110600011105     C*
110700011105     C                   ENDSR
110800050826
110900050826
111000050826
111100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
111200050826     C     wriVGD        BEGSR
111300050826     C*
111400050826     C* Reperisco la descrizione della fase da tabella
111500050826     C                   movel(p)  'FAR'         KteCOD
111600050826     C                   movel(p)  '000'         KteKE1
111700050826     C     KEYtbe        chain     tntbe01l
111800050826     C                   if        %found(tntbe01l)
111900050826     C                   movel(P)  tbeuni        DFAR
112000050826     C                   else
112100050826     C                   clear                   DFAR
112200050826     C                   endif
112300050826     C*
112400050826     C* Valorizzo buffer record
112500050826     C                   clear                   FNVAPDS
112600050826     C                   eval      vapIDC = oriIDC
112700050826     C                   eval      vapPOE = oriPOE
112800050826     C                   eval      vapRFA = vaoRFA
112900050826     C                   eval      vapPOG = oriPOE
113000050826     C                   eval      vapDAE = oriDER
113100050826     C                   eval      vapORE = oriOER
113200050826     C                   if        d§fardva <> *blanks
113300050826     C                   eval      vapDFA = d§fardva
113400050826     C                   else
113500050826     C                   eval      vapDFA = d§fardes
113600050826     C                   endif
113700050826     C*
113800050826     C                   clear                   tivgd000
113900080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
114000050826     C                   eval      vgdTIP = 'VP'
114100060306     C                   eval      vgdKSU = vapIDC
114200050826     C                   eval      vgdTSC = 'WW'
114300050826     C                   eval      vgdDAT = datcor
114400151106     C                   eval      vgdPGM = 'TIS7T7IR'
114500060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
114600060306     C                   exsr      sr_Control
114700060306     C                   if        Cli_Abil = *on
114800060306     C                   eval      vgdKSU = woriIDC
114900121205     C  N55              write     tivgd000
115000060306     C                   endif
115100050826     C*
115200050826     C                   ENDSR
115300050826     C*------------------------------------------------------------------------*
115400011119
115500010528
115600010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
115700010528     C     repfil        BEGSR
115800010528     C*
115900010528     C                   if        invfil = *zeros and
116000010528     C                             depfil > *zeros and
116100010613     C                             (vinflg = *blanks or
116200010613     C                              vinflg = *zeros)
116300010528     C
116400010528     C                   eval      invfil = depfil
116500010528     C                   endif
116600010528     C*
116700010529     C                   if        depfil <> invfil and
116800010529     C                             invfil > *zeros
116900010528     C                   eval      flgMulti = '1'
117000010528     C                   if        vinflg = *blanks
117100010528     C                   add       1             cntNonEl
117200010528     C                   endif
117300010528     C                   endif
117400010528     C*
117500010528     C                   if        vinflg = '2'
117600010528     C                   eval      flgStato = '2'
117700010528     C                   endif
117800010528     C*
117900010528     C                   ENDSR
118000010528     C***
118100010528
118200010528
118300010528
118400010528
118500990920      /TITLE Invio dei dati al punto operativo.
118600000613     C     invio         BEGSR
118700990920     C*
118800990920     C                   reset                   dscmz
118900010528     C                   move      invfil        cmzdst
119000990920     C                   eval      cmzfld = vlrfou
119100990920     C                   eval      cmzmbd = vlrhdl
119200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
119300000710     C                   if        prmfir = *blanks
119400010122     C                   eval      cmzfla = 'FNVAO00F'
119500010122     C                   eval      cmzmba = 'FNVAO00F'
119600000710     C                   else
119700000710     C                   eval      cmzfla = prmfir
119800000710     C                   eval      cmzmba = prmfir
119900000710     C                   endif
120000990920     C                   eval      cmznrr = *zeros
120100990920     C                   move      §ctrok        cmznrr
120200010123     C                   eval      cmzlba = vlrfl1
120300990920     C                   call(e)   'TIS711C'
120400990920     C                   parm                    dscmz
120500990921     C                   parm      *blanks       esito
120600990920     C                   if        %error
120700990920     C                             or cmzerr = '1'
120800990921     C                             or esito  = '1'
120900000710     C                   eval      wrkesito = '3'
121000990920     C                   endif
121100990920     C*
121200000613     C                   ENDSR
121300000613     C***
121400990910
121500010528
121600010528
121700010528
121800010528      /TITLE Invio dei dati al punto operativo.
121900010528     C     opeini        BEGSR
122000010528     C*
122100010528     C* Inizializzo flag e contatori operativi
122200010528     C                   movel     '0'           flgGiro           1
122300010528     C                   movel     '0'           flgMulti          1
122400010528     C                   movel     '1'           flgStato          1
122500010615     C                   movel     '0'           flgOk             1
122600010528     C                   z-add     *zeros        cntNonEl         10 0
122700010528     C                   z-add     *zeros        depfil            3 0
122800010528     C                   z-add     *zeros        invfil            3 0
122900030407     C*
123000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
123100030407     C                   if        %subst(vlrppt:1:1) = 'S'
123200030407     C                   seton                                        50
123300030407     C                   else
123400030407     C                   setoff                                       50
123500030407     C                   endif
123600070925     C*
123700070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
123800070925     C                   if        %subst(vlrppt:2:1) = 'S'
123900070925     C                   seton                                        51
124000070925     C                   else
124100070925     C                   setoff                                       51
124200070925     C                   endif
124300150319     C*
124400150319     C* se il 4° char dei parametri è T=Test
124500150319     C* vince questo test su quello precedente per impostare VAOPOE
124600150319     C                   SETOFF                                       55
124700150319     C                   IF        %subst(vlrppt:4:1) = 'T'
124800150319     C* inibisco la scrittura dei file TIORI e TIVGD
124900150319     C                   SETON                                        55
125000150319     C* imposto il PO di emissione come filiale fittizia
125100150319     C                   EVAL      VAOPOE = 999
125200150319     C                   ENDIF
125300010528     C*
125400010528     C                   ENDSR
125500010528     C***
125600060306
125700060306
125800060306     C*-------------------------------------------------------------------------*
125900060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
126000060306     C*-------------------------------------------------------------------------*
126100060306     C     sr_Control    begsr
126200060306     C*
126300060306     C* inizializzo a ok il flag d controllo abilitazione
126400060306     C                   eval      Cli_Abil = *on
126500060306     C
126600060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
126700060306     C                   clear                   woriIDC
126800060306     C                   eval      kteCOD = 'VAP'
126900060306     C                   eval      kteKE1 = oriIDC
127000060306     C     keytbe        chain     TNTBE01L
127100060306     C                   if        %found(Tntbe01l)
127200060306     C                   eval      woriIDC = TbeKe2
127300060306     C                   else
127400060306     C                   eval      Cli_Abil = *off
127500060306     C                   endif
127600060306     C*
127700060306     C                   endsr
127800010528
127900010528
128000010528
128100010528
128200000613     C     *inzsr        BEGSR
128300990910     C*
128400990910     C     *entry        plist
128500990920     C                   parm                    tivlrds
128600990921     C                   parm      wrkesito      esito
128700000724     C                   parm                    prmlit
128800000710     C                   parm                    prmfir
128900011105     C*
129000011105     C* Campi riferito al database
129100011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
129200011105     C     *like         define    tbeke1        kteKE1
129300011105     C*
129400011105     C* Definizione chiavi di lettura
129500011105     C* TNTBE01L - parziale
129600011105     C     keytbe        klist
129700011105     C                   kfld                    kteCOD                         *UTENTE
129800011105     C                   kfld                    kteKE1                         *CODICE
129900070926     C* TIVSS02L - parziale
130000070926     C     keyvss02_p    klist
130100070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
130200070926     C                   kfld                    vssISV                         *SERVIZIO
130300011105     C*
130400011105     C* Reperimento data e ora corrente
130500011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
130600011105     C                   movel     wn14          wn6               6 0          *ORA (6)
130700151106     C                   z-add     *zeros        datcor            8 0
130800151106     C                   eval      datcor = %dec(%date() : *ISO)
130900010529     C*
131000000613     C                   ENDSR
131100000613     C***
