000100160727      /TITLE Upload via Internet: traduzione in FNVAOWWR.
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
016600160606     C                   EXSR      CHKCOR_A
016700160601     C*
016800160601     C* Verifico univocità "fisica" ORM corrente
016900160601     C                   call      'UBORMCNTR'
017000160601     C                   parm      '1'           UBORM_pIn_OPZ
017100160601     C                   parm                    fnvaoeiir
017200160601     C                   parm      *zeros        UBORM_pOut_CNT
017300160601     C                   parm      *blanks       UBORM_pOut_ESI
017400160601     C*
017500160601     C* Se ORM "fisicamente" duplicato lo marco a warning e non lo acquisisco
017600160601     C                   if         UBORM_pOut_ESI = 'E' OR
017700160601     C                             (UBORM_pOut_ESI = '1' AND
017800160601     C                              UBORM_pOut_CNT > 0)
017900160601     C                   seton                                        31
018000160603     C                   eval      vinflg = '2'
018100160601     C                   else
018200151019     C*
018300151019     C* Stacco subito un numeratore univoco "ORM"
018400151106     C                   exsr      RTVNUM
018500080521     C*
018600080521     C* Verifico se richiesto nei parametri anagrafica cliente/tipo-file d particolari
018700080521     C* impostazioni dei flag operativi
018800080521     C                   if        %subst(vlrppt:3:1) = 'S'
018900080521     C                   eval      dorm01 = a_vaoFLO
019000080521     C                   eval      §ormks = 'S'
019100080521     C                   eval      a_vaoFLO = dorm01
019200080521     C                   endif
019300991022     C*
019400010122     C                   testb     x'3F'         a_vaoatb                 02
019500991022     C                   if        not *in02
019600991022     C*
019700971021     C                   SETOFF                                       303132
019800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
020000971023     C*    OPPURE TUTTI BLANKS
020100010122     C*-------------------
020200010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
020300010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
020400010122     C*-------------------
020500010122     C*
020600011119 xxx C*                  TESTN                   a_vaopoe             30
020700010417  "  C* N30              TESTN                   a_vaopoe               30
020800010417  "  C* N30              TESTN                   a_vaopoe                 30
020900010417     C*                  MOVE      a_vaopoe      W001A             1
021000010417     C*    *IN30         IFEQ      *OFF
021100010417     C*    W001A         ORLT      '0'
021200010417     C*                  SETON                                        32
021300010417     C*                  MOVE      *ZEROS        VAOPOE
021400010417     C*                  EVAL      vinmsg = 'VAOPOE'
021500010417     C*                  ELSE
021600010417     C*                  MOVE      a_vaopoe      VAOPOE
021700010417     C*                  ENDIF
021800010417  "  C*
021900010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
022000010604 xxx C                   IF        vlrpoi <> 999
022100010528     C                   EVAL      VAOPOE = vlrpoi
022200150331     C                   ELSE
022300150331     C*
022400150331     C                   IF        VAOPOE <> 999
022500150331     C*
022600150331     C* se i char dal 5° al 7° dei parametri è <> blank ed è numerico, allora è VAOPOE
022700150331     C                   IF        %subst(vlrppt:5:3) <> *blank
022800150331     C                   EVAL      PiStr=%trim(%subst(vlrppt:5:3))
022900150331     C                   EXSR      CHKNUM
023000150331     C                   IF        PiInt=*on
023100150331     C                   Z-ADD     PiVal         VAOPOE
023200150331     C                   ENDIF
023300150331     C                   ELSE
023400150331     C*
023500010528     C                   IF        a_vaopoe = *blanks or
023600011010     C                             a_vaopoe = *zeros  or
023700011010     C                             a_vaopoe = ' 000'
023800010528     C                   IF        a_vaopor <> *blanks and
023900011010     C                             a_vaopor <> *zeros  and
024000011010     C                             a_vaopor <> ' 000'
024100010528     C                   MOVE      a_vaopor      VAOPOE
024200010528     C                   ELSE
024300011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
024400010528     C                   MOVE      a_vaopoe      VAOPOE
024500010528     C                   ENDIF
024600010529     C                   ELSE
024700010529     C                   MOVE      a_vaopoe      VAOPOE
024800010528     C                   ENDIF
024900010528     C                   ENDIF
025000150331     C                   ENDIF
025100150331     C                   ENDIF
025200010122     C*
025300010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025400010528     C                   eval      depfil = VAOPOE
025500010528     C                   exsr      repfil
025600010528     C                   if        depfil = invfil and
025700010528     C                             (vinflg = *blanks
025800010528     C                              or vinflg = '0'
025900010528     C                              or vinflg = '2')
026000010528     C*
026100010528     C                   exsr      PREELA
026200010710     C*
026300010719     C*                  TESTN                   a_vaonsr             30
026400010719     C* N30              TESTN                   a_vaonsr               30
026500010719     C* N30              TESTN                   a_vaonsr                 30
026600010719     C*                  MOVE      a_vaonsr      W001A             1
026700010719     C*    *IN30         IFEQ      *OFF
026800010719     C*    W001A         ORLT      '0'
026900010719     C*                  SETON                                        32
027000010719     C*                  MOVE      *ZEROS        VAONSR
027100010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
027200010719     C*                  ELSE
027300010719     C*                  MOVE      a_vaonsr      VAONSR
027400010719     C*                  ENDIF
027500010122     C*
027600010719     C*                  TESTN                   a_vaonor             30
027700010719     C* N30              TESTN                   a_vaonor               30
027800010719     C* N30              TESTN                   a_vaonor                 30
027900010719     C*                  MOVE      a_vaonor      W001A             1
028000010719     C*    *IN30         IFEQ      *OFF
028100010719     C*    W001A         ORLT      '0'
028200010719     C*                  SETON                                        32
028300010719     C*                  MOVE      *ZEROS        VAONOR
028400010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
028500010719     C*                  ELSE
028600010719     C*                  MOVE      a_vaonor      VAONOR
028700010719     C*                  ENDIF
028800010122     C*
028900010719     C*                  TESTN                   a_vaonrv             30
029000010719     C* N30              TESTN                   a_vaonrv               30
029100010719     C* N30              TESTN                   a_vaonrv                 30
029200010719     C*                  MOVE      a_vaonrv      W001A             1
029300010719     C*    *IN30         IFEQ      *OFF
029400010719     C*    W001A         ORLT      '0'
029500010719     C*                  SETON                                        32
029600010719     C*                  MOVE      *ZEROS        VAONRV
029700010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
029800010719     C*                  ELSE
029900010719     C*                  MOVE      a_vaonrv      VAONRV
030000010719     C*                  ENDIF
030100010122     C*
030200010122     C                   TESTN                   a_vaodao             30
030300010122     C  N30              TESTN                   a_vaodao               30
030400010122     C  N30              TESTN                   a_vaodao                 30
030500010122     C                   MOVE      a_vaodao      W001A             1
030600010122     C     *IN30         IFEQ      *OFF
030700010122     C     W001A         ORLT      '0'
030800010122     C                   SETON                                        32
030900050421     C                   MOVE      *ZEROS        VAODAO
031000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
031100010122     C                   ELSE
031200050421     C                   MOVE      a_vaodao      VAODAO
031300010122     C                   ENDIF
031400010122     C*
031500010122     C                   TESTN                   a_vaooao             30
031600010122     C  N30              TESTN                   a_vaooao               30
031700010122     C  N30              TESTN                   a_vaooao                 30
031800010122     C                   MOVE      a_vaooao      W001A             1
031900010122     C     *IN30         IFEQ      *OFF
032000010122     C     W001A         ORLT      '0'
032100010122     C                   SETON                                        32
032200050421     C                   MOVE      *ZEROS        VAOOAO
032300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
032400010122     C                   ELSE
032500050421     C                   MOVE      a_vaooao      VAOOAO
032600010122     C                   ENDIF
032700010122     C*
032800010122     C                   TESTN                   a_vaocor             30
032900010122     C  N30              TESTN                   a_vaocor               30
033000010122     C  N30              TESTN                   a_vaocor                 30
033100010122     C                   MOVE      a_vaocor      W001A             1
033200010122     C     *IN30         IFEQ      *OFF
033300010122     C     W001A         ORLT      '0'
033400010122     C                   SETON                                        32
033500010122     C                   MOVE      *ZEROS        VAOCOR
033600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
033700010122     C                   ELSE
033800050421     C                   MOVE      a_vaocor      VAOCOR
033900010122     C                   ENDIF
034000010122     C*
034100010122     C                   TESTN                   a_vaocra             30
034200010122     C  N30              TESTN                   a_vaocra               30
034300010122     C  N30              TESTN                   a_vaocra                 30
034400010122     C                   MOVE      a_vaocra      W001A             1
034500010122     C     *IN30         IFEQ      *OFF
034600010122     C     W001A         ORLT      '0'
034700010122     C                   SETON                                        32
034800010122     C                   MOVE      *ZEROS        VAOCRA
034900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
035000010122     C                   ELSE
035100010122     C                   MOVE      a_vaocra      VAOCRA
035200010122     C                   ENDIF
035300010122     C*
035400010719     C                   IF        a_vaocrc = *blanks or
035500010719     C                             a_vaocrc = *zeros
035600010719     C                   ELSE
035700010719     C                   TESTN                   a_vaocrc             30
035800010719     C  N30              TESTN                   a_vaocrc               30
035900010719     C  N30              TESTN                   a_vaocrc                 30
036000010719     C                   MOVE      a_vaocrc      W001A             1
036100010719     C     *IN30         IFEQ      *OFF
036200010719     C     W001A         ORLT      '0'
036300010719     C                   SETON                                        32
036400010719     C                   MOVE      *ZEROS        VAOCRC
036500010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
036600010719     C                   ELSE
036700010719     C                   MOVE      a_vaocrc      VAOCRC
036800010719     C                   ENDIF
036900010719     C                   ENDIF
037000010122     C*
037100010122     C                   TESTN                   a_vaodar             30
037200010122     C  N30              TESTN                   a_vaodar               30
037300010122     C  N30              TESTN                   a_vaodar                 30
037400010122     C                   MOVE      a_vaodar      W001A             1
037500010122     C     *IN30         IFEQ      *OFF
037600010122     C     W001A         ORLT      '0'
037700010122     C                   SETON                                        32
037800010122     C                   MOVE      *ZEROS        VAODAR
037900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
038000010122     C                   ELSE
038100010122     C                   MOVE      a_vaodar      VAODAR
038200010122     C                   ENDIF
038300010122     C*
038400010122     C                   TESTN                   a_vaoorr             30
038500010122     C  N30              TESTN                   a_vaoorr               30
038600010122     C  N30              TESTN                   a_vaoorr                 30
038700010122     C                   MOVE      a_vaoorr      W001A             1
038800010122     C     *IN30         IFEQ      *OFF
038900010122     C     W001A         ORLT      '0'
039000010122     C                   SETON                                        32
039100010122     C                   MOVE      *ZEROS        VAOORR
039200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
039300010122     C                   ELSE
039400010122     C                   MOVE      a_vaoorr      VAOORR
039500010122     C                   ENDIF
039600010122     C*
039700010122     C                   TESTN                   a_vaoncl             30
039800010122     C  N30              TESTN                   a_vaoncl               30
039900010122     C  N30              TESTN                   a_vaoncl                 30
040000010122     C                   MOVE      a_vaoncl      W001A             1
040100010122     C     *IN30         IFEQ      *OFF
040200010122     C     W001A         ORLT      '0'
040300010122     C                   SETON                                        32
040400010122     C                   MOVE      *ZEROS        VAONCL
040500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
040600010122     C                   ELSE
040700010122     C                   MOVE      a_vaoncl      VAONCL
040800010122     C                   ENDIF
040900010122     C*
041000010122     C                   MOVE      *ZERO         VAOPKG
041100010122     C                   MOVE      a_vaopkg      WASC09
041200010122     C                   TESTN                   WI09                 30
041300010122     C  N30              TESTN                   WI09                   30
041400010122     C  N30              TESTN                   WI09                     30
041500010122     C                   MOVE      WI09          W001A             1
041600010122     C     *IN30         IFEQ      *OFF
041700010122     C     W001A         ORLT      '0'
041800010122     C     W001A         ANDNE     *BLANK
041900010122     C                   SETON                                        32
042000010122     C                   MOVE      *ZERO         VAOPKG
042100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
042200010122     C                   ELSE
042300010122     C                   MOVEL     WI09          VAOPKG
042400010122     C                   TESTN                   WD09                 30
042500010122     C  N30              TESTN                   WD09                     30
042600010122     C                   MOVE      WD09          W001A             1
042700010122     C     *IN30         IFEQ      *OFF
042800010122     C     W001A         ORLT      '0'
042900010122     C     W001A         ANDNE     *BLANK
043000010122     C                   SETON                                        32
043100010122     C                   MOVE      *ZERO         VAOPKG
043200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
043300010122     C                   ELSE
043400010122     C                   MOVE      WD09          VAOPKG
043500010122     C                   ENDIF
043600050118     C                   ENDIF
043700010122     C*
043800010122     C                   MOVE      *ZERO         VAOVLM
043900010122     C                   MOVE      a_vaovlm      WASC07
044000010122     C                   TESTN                   WI07                 30
044100010122     C  N30              TESTN                   WI07                   30
044200010122     C  N30              TESTN                   WI07                     30
044300010122     C                   MOVE      WI07          W001A             1
044400010122     C     *IN30         IFEQ      *OFF
044500010122     C     W001A         ORLT      '0'
044600010122     C     W001A         ANDNE     *BLANK
044700010122     C                   SETON                                        32
044800010122     C                   MOVE      *ZERO         VAOVLM
044900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
045000010122     C                   ELSE
045100010122     C                   MOVEL     WI07          VAOVLM
045200010122     C                   TESTN                   WD07                 30
045300010122     C  N30              TESTN                   WD07                   30
045400010122     C  N30              TESTN                   WD07                     30
045500010122     C                   MOVE      WD07          W001A             1
045600010122     C     *IN30         IFEQ      *OFF
045700010122     C     W001A         ORLT      '0'
045800010122     C     W001A         ANDNE     *BLANK
045900010122     C                   SETON                                        32
046000010122     C                   MOVE      *ZERO         VAOVLM
046100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
046200010122     C                   ELSE
046300010122     C                   MOVE      WD07          VAOVLM
046400010122     C                   ENDIF
046500050118     C                   ENDIF
046600010122     C*
046700010122     C                   TESTN                   a_vaobnc             30
046800010122     C  N30              TESTN                   a_vaobnc               30
046900010122     C  N30              TESTN                   a_vaobnc                 30
047000010122     C                   MOVE      a_vaobnc      W001A             1
047100010122     C     *IN30         IFEQ      *OFF
047200010122     C     W001A         ORLT      '0'
047300010122     C                   SETON                                        32
047400010122     C                   MOVE      *ZEROS        VAOBNC
047500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
047600010122     C                   ELSE
047700010122     C                   MOVE      a_vaobnc      VAOBNC
047800010122     C                   ENDIF
047900010122     C*
048000010122     C                   TESTN                   a_vaoblc             30
048100010122     C  N30              TESTN                   a_vaoblc               30
048200010122     C  N30              TESTN                   a_vaoblc                 30
048300010122     C                   MOVE      a_vaoblc      W001A             1
048400010122     C     *IN30         IFEQ      *OFF
048500010122     C     W001A         ORLT      '0'
048600010122     C                   SETON                                        32
048700010122     C                   MOVE      *ZEROS        VAOBLC
048800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
048900010122     C                   ELSE
049000010122     C                   MOVE      a_vaoblc      VAOBLC
049100010122     C                   ENDIF
049200010122     C*
049300010122     C                   TESTN                   a_vaoatt             30
049400010122     C  N30              TESTN                   a_vaoatt               30
049500010122     C  N30              TESTN                   a_vaoatt                 30
049600010122     C                   MOVE      a_vaoatt      W001A             1
049700010122     C     *IN30         IFEQ      *OFF
049800010122     C     W001A         ORLT      '0'
049900010122     C                   SETON                                        32
050000010122     C                   MOVE      *ZEROS        VAOATT
050100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
050200010122     C                   ELSE
050300010122     C                   MOVE      a_vaoatt      VAOATT
050400010122     C                   ENDIF
050500010122     C*
050600010122     C                   TESTN                   a_vaomtc             30
050700010122     C  N30              TESTN                   a_vaomtc               30
050800010122     C  N30              TESTN                   a_vaomtc                 30
050900010122     C                   MOVE      a_vaomtc      W001A             1
051000010122     C     *IN30         IFEQ      *OFF
051100010122     C     W001A         ORLT      '0'
051200010122     C                   SETON                                        32
051300010122     C                   MOVE      *ZEROS        VAOMTC
051400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
051500010122     C                   ELSE
051600010122     C                   MOVE      a_vaomtc      VAOMTC
051700010122     C                   ENDIF
051800010122     C*
051900010719     C                   IF        a_vaoksc = *blanks or
052000010719     C                             a_vaoksc = *zeros
052100010719     C                   ELSE
052200010719     C                   TESTN                   a_vaoksc             30
052300010719     C  N30              TESTN                   a_vaoksc               30
052400010719     C  N30              TESTN                   a_vaoksc                 30
052500010719     C                   MOVE      a_vaoksc      W001A             1
052600010719     C     *IN30         IFEQ      *OFF
052700010719     C     W001A         ORLT      '0'
052800010719     C                   SETON                                        32
052900010719     C                   MOVE      *ZEROS        VAOKSC
053000010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
053100010719     C                   ELSE
053200010719     C                   MOVE      a_vaoksc      VAOKSC
053300010719     C                   ENDIF
053400010719     C                   ENDIF
053500010122     C*
053600010710     C                   IF        a_vaoctr > *blanks
053700010122     C                   TESTN                   a_vaoctr             30
053800010122     C  N30              TESTN                   a_vaoctr               30
053900010122     C  N30              TESTN                   a_vaoctr                 30
054000010122     C                   MOVE      a_vaoctr      W001A             1
054100010122     C     *IN30         IFEQ      *OFF
054200010122     C     W001A         ORLT      '0'
054300010122     C                   SETON                                        32
054400010627     C                   MOVE      *blanks       VAOCTR
054500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
054600010122     C                   ELSE
054700010122     C                   MOVE      a_vaoctr      VAOCTR
054800010122     C                   ENDIF
054900010710     C                   ENDIF
055000010122     C*
055100010122     C                   TESTN                   a_vaopor             30
055200010122     C  N30              TESTN                   a_vaopor               30
055300010122     C  N30              TESTN                   a_vaopor                 30
055400010122     C                   MOVE      a_vaopor      W001A             1
055500010122     C     *IN30         IFEQ      *OFF
055600010122     C     W001A         ORLT      '0'
055700010122     C                   SETON                                        32
055800010122     C                   MOVE      *ZEROS        VAOPOR
055900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
056000010122     C                   ELSE
056100010122     C                   MOVE      a_vaopor      VAOPOR
056200010122     C                   ENDIF
056300010122     C*
056400010719     C                   IF        a_vaopoc = *blanks or
056500010719     C                             a_vaopoc = *zeros
056600010719     C                   ELSE
056700010122     C                   TESTN                   a_vaopoc             30
056800010122     C  N30              TESTN                   a_vaopoc               30
056900010122     C  N30              TESTN                   a_vaopoc                 30
057000010122     C                   MOVE      a_vaopoc      W001A             1
057100010122     C     *IN30         IFEQ      *OFF
057200010122     C     W001A         ORLT      '0'
057300010122     C                   SETON                                        32
057400010122     C                   MOVE      *ZEROS        VAOPOC
057500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
057600010122     C                   ELSE
057700010122     C                   MOVE      a_vaopoc      VAOPOC
057800010122     C                   ENDIF
057900010719     C                   ENDIF
058000010122     C*
058100010719     C                   IF        a_vaosto = *blanks or
058200010719     C                             a_vaosto = *zeros
058300010719     C                   ELSE
058400010122     C                   TESTN                   a_vaosto             30
058500010122     C  N30              TESTN                   a_vaosto               30
058600010122     C  N30              TESTN                   a_vaosto                 30
058700010122     C                   MOVE      a_vaosto      W001A             1
058800010122     C     *IN30         IFEQ      *OFF
058900010122     C     W001A         ORLT      '0'
059000010122     C                   SETON                                        32
059100010122     C                   MOVE      *ZEROS        VAOSTO
059200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
059300010122     C                   ELSE
059400010122     C                   MOVE      a_vaosto      VAOSTO
059500010122     C                   ENDIF
059600010719     C                   ENDIF
059700971021     C*
059800971021     C* CAMPI ALFABETICI:
059900010122     C                   MOVE      a_vaoatb      VAOATB
060000010122     C                   MOVE      a_vaotor      VAOTOR
060100010531     C*
060200010531     C                   IF        a_vaotco = *blanks
060300010531     C                   MOVE      'F'           VAOTCO
060400010531     C                   ELSE
060500010531     C                   MOVE      a_vaotco      VAOTCO
060600010531     C                   ENDIF
060700010531     C*
060800010122     C                   MOVE      a_vaorso      VAORSO
060900010122     C                   MOVE      a_vaoino      VAOINO
061000010122     C                   MOVE      a_vaocao      VAOCAO
061100010122     C                   MOVE      a_vaoloo      VAOLOO
061200031014     C                   MOVE      a_vaopro      VAOPRO
061300090129     C                   MOVE      a_vaonao      VAONAO
061400131007     C*
061500131007     C                   MOVE      a_vaorsr      VAORSR
061600131007     C                   MOVE      a_vaoinr      VAOINR
061700131007     C                   MOVE      a_vaocar      VAOCAR
061800131007     C                   MOVE      a_vaolor      VAOLOR
061900131007     C                   MOVE      a_vaoprr      VAOPRR
062000131007     C                   MOVE      a_vaonar      VAONAR
062100131007     C*
062200131007     C                   MOVE      a_vaorer      VAORER
062300131007     C                   MOVE      a_vaoter      VAOTER
062400131007     C                   MOVE      a_vaorsc      VAORSC
062500131007     C                   MOVE      a_vaoinc      VAOINC
062600131007     C                   MOVE      a_vaocac      VAOCAC
062700131007     C                   MOVE      a_vaoloc      VAOLOC
062800131007     C                   MOVE      a_vaoprc      VAOPRC
062900131007     C                   MOVE      a_vaonac      VAONAC
063000131007     C*
063100131007     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
063200131007     C*** VAONAO
063300131007     C                   IF        %trim(VAONAO) = 'I'  OR
063400131007     C                             %trim(VAONAO) = 'IT' OR
063500131007     C                             %trim(VAONAO) = 'VA' OR
063600131007     C                             %trim(VAONAO) = 'SM'
063700131007     C                   EVAL      VAONAO = *blanks
063800131007     C                   ENDIF
063900131007     C*** VAONAR
064000131007     C                   IF        %trim(VAONAR) = 'I'  OR
064100131007     C                             %trim(VAONAR) = 'IT' OR
064200131007     C                             %trim(VAONAR) = 'VA' OR
064300131007     C                             %trim(VAONAR) = 'SM'
064400131007     C
064500131007     C                   EVAL      VAONAR = *blanks
064600131007     C                   ENDIF
064700131007     C*** VAONAC
064800131007     C                   IF        %trim(VAONAC) = 'I'  OR
064900131007     C                             %trim(VAONAC) = 'IT' OR
065000131007     C                             %trim(VAONAC) = 'VA' OR
065100131007     C                             %trim(VAONAC) = 'SM'
065200131007     C                   EVAL      VAONAC = *blanks
065300131007     C                   ENDIF
065400131007     C*
065500031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
065600031014     C                   IF        VAOPRO  = *blanks AND
065700050907     C                             VAOCAO <> *blanks AND
065800050907     C                             VAONAO  = *blanks                            * solo x Italia
065900031014     C                   CLEAR                   TISI95DS
066000031014     C                   EVAL      I95TCN = '3'
066100031014     C                   Z-ADD     datcor        I95DAT
066200031014     C                   EVAL      I95CAP = VAOCAO
066300031014     C                   EVAL      I95LOC = VAOLOO
066400031014     C                   CALL      'TISI95R'
066500031014     C                   PARM                    TISI95DS
066600031014     C                   EVAL      VAOPRO = O95PRV
066700031014     C                   ENDIF
066800131007     C*
066900031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
067000031014     C                   IF        VAOPRR  = *blanks AND
067100050907     C                             VAOCAR <> *blanks AND
067200050907     C                             VAONAR  = *blanks                            * solo x Italia
067300031014     C                   CLEAR                   TISI95DS
067400031014     C                   EVAL      I95TCN = '3'
067500031014     C                   Z-ADD     datcor        I95DAT
067600031014     C                   EVAL      I95CAP = VAOCAR
067700031014     C                   EVAL      I95LOC = VAOLOR
067800031014     C                   CALL      'TISI95R'
067900031014     C                   PARM                    TISI95DS
068000031014     C                   EVAL      VAOPRR = O95PRV
068100031014     C                   ENDIF
068200131007     C*
068300031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
068400031014     C                   IF        VAOPRC  = *blanks AND
068500050907     C                             VAOCAC <> *blanks AND
068600050907     C                             VAONAC  = *blanks                            * solo x Italia
068700031014     C                   CLEAR                   TISI95DS
068800031014     C                   EVAL      I95TCN = '3'
068900031014     C                   Z-ADD     datcor        I95DAT
069000031014     C                   EVAL      I95CAP = VAOCAC
069100031014     C                   EVAL      I95LOC = VAOLOC
069200031014     C                   CALL      'TISI95R'
069300031014     C                   PARM                    TISI95DS
069400031014     C                   EVAL      VAOPRC = O95PRV
069500031014     C                   ENDIF
069600031014     C*
069700010122     C                   MOVE      a_vaoffd      VAOFFD
069800010122     C                   MOVE      a_vaormp      VAORMP
069900010122     C                   MOVE      a_vaonam      VAONAM
070000010122     C                   MOVE      a_vaopag      VAOPAG
070100010122     C                   MOVE      a_vaozor      VAOZOR
070200010122     C                   MOVE      a_vaono1      VAONO1
070300010122     C                   MOVE      a_vaono2      VAONO2
070400010122     C                   MOVE      a_vaoddt      VAODDT
070500010122     C                   MOVE      a_vaorfa      VAORFA
070600010122     C                   MOVE      a_vaospi      VAOSPI
070700010122     C                   MOVE      a_vaoflo      VAOFLO
070800050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
070900050304     C                   IF        VAOPKG =  *zeros AND
071000050304     C                             VAOVLM =  *zeros AND
071100050304     C                             VAOBNC =  *zeros AND
071200050304     C                             VAONCL <> *zeros
071300050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
071400050304     C                   ENDIF
071500030603     C*
071600030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
071700030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
071800030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
071900050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
072000150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
072100930409     C*
072200010604     C  N31              ADD       1             §CTROK            7 0
072300010604     C   32              ADD       1             §CTRMO            7 0
072400010604     C   31              ADD       1             §CTRNO            7 0
072500030624     C  N31              EXSR      CHKCOR
072600011105     C  N31              EXSR      WRIORI
072700050826     C  N31              EXSR      WRIVGD
072800050421     C*
072900050421     C  N31              WRITE     FNVAO000
073000140416     C*
073100151019     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
073200151106     C                   if        not *in31
073300151019     C                   exsr      EXEFORZ
073400151106     C                   call      'TIS7T7IIR'
073500140416     C                   parm      'W'           pIn_Opz           1
073600140416     C                   parm                    tivlrds
073700151106     C                   parm                    FNVAOEIIR
073800140416     C                   parm                    oriPRG
073900140528     C                   parm      *zeros        pIn_Dst           3
074000140416     C                   parm      *blanks       pOut_Esito        1
074100140416     C                   endif
074200140416     C                   if        pOut_Esito = '2'
074300140416     C                   seton                                        32
074400140416     C                   endif
074500160601     C*
074600010604     C                   if        *in31 = *off and
074700010604     C                             *in32 = *off
074800990910     C                   eval      vinflg = '1'
074900990910     C                   else
075000990910     C                   eval      vinflg = '2'
075100990910     C                   endif
075200991022     C                   endif
075300990910     C                   endif
075400160603     C*
075500160603     C                   endif
075600010604     C*
075700010604     C                   endif
075800000905     C*
075900000905     C                   else
076000000905     C                   eval      vinflg = '1'
076100000905     C                   endif
076200000905     C*
076300000905     C  N70              update    tivin000
076400991022     C*
076500991022     C  N70              ENDdo
076600140416     C*
076700140416     C     FINE          tag
076800140528     C*
076900140528     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
077000151106     C                   call      'TIS7T7IIR'
077100140528     C                   parm      'C'           pIn_Opz           1
077200140528     C                   parm                    tivlrds
077300151106     C                   parm                    FNVAOEIIR
077400140528     C                   parm                    oriPRG
077500140528     C                   parm      *zeros        pIn_Dst           3
077600140528     C                   parm      *blanks       pOut_Esito        1
077700010528     C*
077800010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
077900010528     C                   if        cntNonEl = *zeros or
078000010528     C                             flgMulti = '0'
078100010528     C* Se non ci sono record con errori ...
078200010528     C                   if        §ctrno = 0 and
078300010604     C                             §ctrmo = 0 and
078400010528     C                             flgStato <> '2'
078500990910     C* ... restituisco esito OK.
078600990921     C                   eval      wrkesito = '0'
078700990910     C                   else
078800000710     C                   if        §ctrok > 0
078900990921     C                   eval      wrkesito = '1'
079000000710     C                   else
079100010615     C                   if        flgOk = '0'
079200010615     C                   eval      wrkesito = '2'
079300010615     C                   else
079400010615     C                   eval      wrkesito = '6'
079500010615     C                   endif
079600990910     C                   endif
079700010528     C                   endif
079800010529     C                   else
079900010529     C                   eval      wrkesito = '9'
080000000710     C                   endif
080100990910     C*
080200990914     C                   if        %open(tivin00r)
080300990908     C                   close     tivin00r
080400990914     C                   endif
080500010122     C                   if        %open(fnvaowwr)
080600010122     C                   close     fnvaowwr
080700990914     C                   endif
080800990910     C*
080900010528     C                   if        vlrpoi <> 999
081000010528     C                   eval      invfil = vlrpoi
081100010528     C                   endif
081200010528     C*
081300990920     C                   if        §ctrok > 0
081400010528     C                             and invfil > *zeros
081500140528     C                   exsr      invio
081600140528     C*
081700140528     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
081800140528     C                   move      invfil        cmzdst
081900151106     C                   call      'TIS7T7IIR'
082000140528     C                   parm      'S'           pIn_Opz           1
082100140528     C                   parm                    tivlrds
082200151106     C                   parm                    FNVAOEIIR
082300140528     C                   parm                    oriPRG
082400140528     C                   parm      cmzdst        pIn_Dst           3
082500140528     C                   parm      *blanks       pOut_Esito        1
082600990920     C                   endif
082700010612     C*
082800010612     C                   if        flgGiro = '1'
082900010612     C                   exsr      endela
083000010612     C                   endif
083100010528     C*
083200910830     C                   ENDSR
083300000613     C***
083400140417
083500140417
083600140417     C*------------------------------------------------------------------------*
083700151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
083800140417     C*------------------------------------------------------------------------*
083900151019     C     RTVNUM        BEGSR
084000140417     C*
084100151019     C* Stacco un numeratore da AZNUM
084200151019     C                   clear                   §numori
084300140417     C                   clear                   trul33ds
084400140417     C                   eval      I33OPE = *zeros
084500140417     C                   eval      I33CNU = 600
084600140417     C                   eval      I33NUM = 1
084700140417     C                   movel     TRUL33DS      KPJBU
084800140417     C                   call      'TRUL33R'
084900140417     C                   parm                    KPJBA
085000140417     C                   movel     KPJBU         TRUL33DS
085100140417     C                   if        O33ERR = *zeros
085200140417     C                   z-add     O33NRF        §numori
085300140417     c                   else
085400140417     c                   eval      §numori = *all'9'
085500140417     C                   seton                                        31        * errore bloccante
085600140417     C                   endif
085700140417     C*
085800151106     C                   eval      vaoNOR = §numori
085900140417     C*
086000140417     C                   ENDSR
086100151019
086200151019
086300151019     C*------------------------------------------------------------------------*
086400151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
086500151019     C*------------------------------------------------------------------------*
086600151106     C     EXEFORZ       BEGSR
086700151019     C*
086800151106     C                   move(p)   VAOPOE        a_vaopoe                       * forzatura necessar
086900151106     C                   move(p)   VAOCOR        a_vaocor                       * forzatura necessar
087000151106     C                   move(p)   VAONOR        a_vaonor                       * forzatura necessar
087100151019     C*
087200151019     C                   ENDSR
087300140417
087400150319
087500150319     C*----------------------------------------------------*
087600150319     C*  CONTROLLO NUMERICITA' CAMPI
087700150319     C*----------------------------------------------------*
087800150319     C     CHKNUM        BEGSR
087900150319     C*
088000150319     C                   IF        PiDecChr = *blanks
088100150319     C                   EVAL      PiDecChr = '.'
088200150319     C                   ENDIF
088300150319     C*
088400150319     C                   CALL(e)   'ISNUMERIC'
088500150319     C                   PARM                    PiStr            30
088600150319     C                   PARM                    PiDecChr          1
088700150319     C                   PARM      *ZEROS        PiVal            30 9
088800150319     C                   PARM      '0'           PiInt             1
088900150319     C                   PARM      '0'           PiNum             1
089000150319     C                   IF        %error
089100150319     C                   EVAL      PiNum=*off
089200150319     C                   ENDIF
089300150319     C*
089400150319     C                   ENDSR
089500150319     C***
089600030624
089700030624
089800030624     C*------------------------------------------------------------------------*
089900030624     C* CHKCOR - Considerazioni su VAOCOR
090000030624     C*------------------------------------------------------------------------*
090100030624     C     CHKCOR        BEGSR
090200030624     C*
090300030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
090400030624     C                   if        vaoTCO = 'F'
090500030624     C* Quindi verifico se il codice ordinante non è valorizzato
090600050323     C                   if        vaoCOR = *zeros
090700030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
090800030624     C                   move      vlrKSC        wCli              7 0
090900050323     C                   movel(p)  wCli          acrCRO
091000030624     C     acrCRO        setll     fnacr01l
091100030624     C                   if        %found(fnacr01l)
091200030624     C                   read      fnacr01l
091300030624     C                   dow       not %eof(fnacr01l)
091400030624     C                   movel     acrCRO        wCliAcr           7 0
091500030624     C                   if        wCliAcr = wCli
091600030624     C                   eval      vaoCOR = acrCRO
091700030624     C                   leave
091800030624     C                   else
091900030624     C                   leave
092000030624     C                   endif
092100030624     C                   read      fnacr01l
092200030624     C                   enddo
092300030624     C                   endif
092400030624     C                   endif
092500030624     C                   endif
092600030624     C*
092700030624     C                   if        vaoTCO = 'I'
092800030624     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
092900030624     C                   if        vlrKSC = '0ORM0000'
093000030624     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
093100061031     C                   movel     ConstINO      vaoINO
093200030624     C                   eval      vaoCAO = '40138'
093300030624     C                   eval      vaoLOO = 'BOLOGNA'
093400030624     C                   eval      vaoPRO = 'BO'
093500030624     C                   endif
093600030624     C                   endif
093700030624     C*
093800030624     C                   ENDSR
093900160606
094000160606
094100160606     C*------------------------------------------------------------------------*
094200160606     C* CHKCOR_A - Considerazioni su A_VAOCOR ai fini TIORI
094300160606     C*------------------------------------------------------------------------*
094400160606     C     CHKCOR_A      BEGSR
094500160606     C*
094600160606     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
094700160606     C                   if        a_vaoTCO = 'F'
094800160606     C* Quindi verifico se il codice ordinante non è valorizzato
094900160606     C                   if        a_vaoCOR = *zeros
095000160606     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
095100160606     C                   move      vlrKSC        wCli              7 0
095200160606     C                   movel(p)  wCli          acrCRO
095300160606     C     acrCRO        setll     fnacr01l
095400160606     C                   if        %found(fnacr01l)
095500160606     C                   read      fnacr01l
095600160606     C                   dow       not %eof(fnacr01l)
095700160606     C                   movel     acrCRO        wCliAcr           7 0
095800160606     C                   if        wCliAcr = wCli
095900160606     C                   move(p)   acrCRO        a_vaoCOR
096000160606     C                   leave
096100160606     C                   else
096200160606     C                   leave
096300160606     C                   endif
096400160606     C                   read      fnacr01l
096500160606     C                   enddo
096600160606     C                   endif
096700160606     C                   endif
096800160606     C                   endif
096900160606     C*
097000160606     C                   if        a_vaoTCO = 'I'
097100160606     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
097200160606     C                   if        vlrKSC = '0ORM0000'
097300160606     C                   eval      a_vaoRSO = 'ORDINANTE SENZA PASSWORD'
097400160606     C                   movel     ConstINO      a_vaoINO
097500160606     C                   eval      a_vaoCAO = '40138'
097600160606     C                   eval      a_vaoLOO = 'BOLOGNA'
097700160606     C                   eval      a_vaoPRO = 'BO'
097800160606     C                   endif
097900160606     C                   endif
098000160606     C*
098100160606     C                   ENDSR
098200030624
098300990920
098400010528
098500011105     C*------------------------------------------------------------------------*
098600011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
098700011105     C*------------------------------------------------------------------------*
098800011105     C     WRIORI        BEGSR
098900011105     C*
099000090617     C* Come prima cosa stacco un numeratore da AZNUM
099100151019     C***                clear                   trul33ds
099200151019     C***                eval      I33OPE = *zeros
099300151019     C***                eval      I33CNU = 600
099400151019     C***                eval      I33NUM = 1
099500151019     C***                movel     TRUL33DS      KPJBU
099600151019     C***                call      'TRUL33R'
099700151019     C***                parm                    KPJBA
099800151019     C***                movel     KPJBU         TRUL33DS
099900151019     C***                if        O33ERR = *zeros
100000151019     C***                z-add     O33NRF        §numori
100100151019     c***                else
100200151019     C***                eval      §numori = *all'9'
100300151019     C***                endif
100400070925     C*
100500070925     C* Giro "standard" (ovvero x cliente d invio dati)
100600070925     C                   if        *in51 = *off
100700011105     C*
100800011122     C* Quindi imposto i campi "extra-VAO"
100900011122     C                   if        vlrKSC = '0ORM0000'
101000011122     C*
101100011122     C                   if        vaoCOR > *zeros
101200011122     C                   movel     vaoCOR        wrkCOR7           7 0
101300011122     C*
101400011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
101500011122     C* codice padre. Se è un padre assegno già il codice ordinante.
101600011122     C                   clear                   tibs10ds
101700011122     C                   eval      d10drf = datcor                              *data riferimento
101800011122     C                   eval      d10paf = 'P'                                 *cerca il padre
101900011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
102000091026     C*
102100091026     C* Preparazione chiamata al TIBS10R1
102200091026     C                   clear                   tibs10r1ds
102300091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
102400091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
102500091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
102600091026     C*
102700091026     C                   call      'TIBS10R1'
102800091026     C                   parm                    tibs10r1ds
102900091026     C*
103000091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
103100011122     C*
103200011122     C                   if        d10err <> *blanks or                         *No unif. o padre
103300011122     C                             d10cop =  *zeros  or
103400011122     C                             d10cop =  d10cod
103500011122     C                   move      d10cod        oriIDC
103600011122     C                   else                                                   *Figlio
103700011122     C                   move      d10cop        oriIDC
103800011122     C                   endif
103900011122     C*
104000011122     C                   else
104100011122     C                   eval      oriIDC = *zeros
104200011122     C                   endif
104300011122     C*
104400011122     C                   else
104500011122     C                   eval      oriIDC = vlrKSC
104600011122     C                   endif
104700070925     C*
104800070925     C* Giro "forzato" (ovvero x codice ordinante)
104900070925     C                   else
105000070925     C*
105100070925     C                   if        vaoCOR > *zeros
105200070925     C                   movel     vaoCOR        wrkCOR7           7 0
105300070925     C*
105400070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
105500070925     C* codice padre. Se è un padre assegno già il codice ordinante.
105600070925     C                   clear                   tibs10ds
105700070925     C                   eval      d10drf = datcor                              *data riferimento
105800070925     C                   eval      d10paf = 'P'                                 *cerca il padre
105900070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
106000091026     C*
106100091026     C* Preparazione chiamata al TIBS10R1
106200091026     C                   clear                   tibs10r1ds
106300091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
106400091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
106500091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
106600091026     C*
106700091026     C                   call      'TIBS10R1'
106800091026     C                   parm                    tibs10r1ds
106900091026     C*
107000091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
107100070925     C*
107200070925     C                   if        d10err <> *blanks or                         *No unif. o padre
107300070925     C                             d10cop =  *zeros  or
107400070925     C                             d10cop =  d10cod
107500070925     C                   move      d10cod        oriIDC
107600070925     C                   else                                                   *Figlio
107700070925     C                   move      d10cop        oriIDC
107800070925     C                   endif
107900070926     C*
108000070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
108100070926     C* al servizio d rintraccia ritiri on-line
108200070926     C                   eval      vssKSU = oriIDC
108300070926     C                   eval      vssISV = 'T3'
108400070926     C     keyvss02_p    chain     tivss02l
108500070926     C                   if        %found(tivss02l)
108600070926     C                   if        vssdde <= datcor AND
108700070926     C                             datcor <= vssdsc
108800070926     C                   else
108900070926     C                   eval      oriIDC = vlrKSC
109000070926     C                   endif
109100070926     C                   else
109200070926     C                   eval      oriIDC = vlrKSC
109300070926     C                   endif
109400070926     C*
109500070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
109600070926     C* anagrafica clienti ritiro
109700070926     C                   setoff                                       52
109800070926     C                   eval      acrCRO = wrkCOR7 * 1000
109900070926     C     acrCRO        setll     fnacr01l
110000070926     C                   if        %found(fnacr01l)
110100070926     C                   read      fnacr01l
110200070926     C                   dow       not %eof(fnacr01l)
110300070926     C                   movel     acrCRO        wCliAcr           7 0
110400070926     C                   if        wCliAcr = wrkCOR7
110500070926     C                   seton                                        52
110600070926     C                   leave
110700070926     C                   else
110800070926     C                   leave
110900070926     C                   endif
111000070926     C                   read      fnacr01l
111100070926     C                   enddo
111200070926     C                   endif
111300070926     C*
111400070926     C                   if        *in52  = *off
111500070926     C                   eval      oriIDC = vlrKSC
111600070926     C                   endif
111700070925     C*
111800070925     C                   else
111900070925     C                   if        vlrKSC = '0ORM0000'
112000070925     C                   eval      oriIDC = *zeros
112100070925     C                   else
112200070925     C                   eval      oriIDC = vlrKSC
112300070925     C                   endif
112400070925     C                   endif
112500070925     C                   endif
112600011122     C*
112700011105     C                   eval      oriPRG = §numori
112800011105     C                   eval      oriDER = datcor
112900011105     C                   eval      oriOER = wn6
113000011105     C                   eval      oriPOE = vaoPOE
113100011105     C                   eval      oriNSR = vaoNSR
113200011105     C                   eval      oriNOR = vaoNOR
113300011105     C                   eval      oriNRV = vaoNRV
113400011105     C                   eval      oriFLO = *blanks
113500011105     C*
113600011122     C* Infine valorizzo la chiave esterna sul file FNVAO
113700011105     C                   eval      dorm01 = vaoFLO
113800011105     C                   movel     §numori       §ormpg
113900011105     C                   eval      vaoFLO = dorm01
114000011105     C*
114100060306     C* ...quindi scrivo il file TIORI00F
114200121205     C  N55              WRITE     TIORI000
114300011105     C*
114400011105     C                   ENDSR
114500050826
114600050826
114700050826
114800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
114900050826     C     wriVGD        BEGSR
115000050826     C*
115100050826     C* Reperisco la descrizione della fase da tabella
115200050826     C                   movel(p)  'FAR'         KteCOD
115300050826     C                   movel(p)  '000'         KteKE1
115400050826     C     KEYtbe        chain     tntbe01l
115500050826     C                   if        %found(tntbe01l)
115600050826     C                   movel(P)  tbeuni        DFAR
115700050826     C                   else
115800050826     C                   clear                   DFAR
115900050826     C                   endif
116000050826     C*
116100050826     C* Valorizzo buffer record
116200050826     C                   clear                   FNVAPDS
116300050826     C                   eval      vapIDC = oriIDC
116400050826     C                   eval      vapPOE = oriPOE
116500050826     C                   eval      vapRFA = vaoRFA
116600050826     C                   eval      vapPOG = oriPOE
116700050826     C                   eval      vapDAE = oriDER
116800050826     C                   eval      vapORE = oriOER
116900050826     C                   if        d§fardva <> *blanks
117000050826     C                   eval      vapDFA = d§fardva
117100050826     C                   else
117200050826     C                   eval      vapDFA = d§fardes
117300050826     C                   endif
117400050826     C*
117500050826     C                   clear                   tivgd000
117600080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
117700050826     C                   eval      vgdTIP = 'VP'
117800060306     C                   eval      vgdKSU = vapIDC
117900050826     C                   eval      vgdTSC = 'WW'
118000050826     C                   eval      vgdDAT = datcor
118100151106     C                   eval      vgdPGM = 'TIS7T7IR'
118200060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
118300060306     C                   exsr      sr_Control
118400060306     C                   if        Cli_Abil = *on
118500060306     C                   eval      vgdKSU = woriIDC
118600121205     C  N55              write     tivgd000
118700060306     C                   endif
118800050826     C*
118900050826     C                   ENDSR
119000050826     C*------------------------------------------------------------------------*
119100011119
119200010528
119300010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
119400010528     C     repfil        BEGSR
119500010528     C*
119600010528     C                   if        invfil = *zeros and
119700010528     C                             depfil > *zeros and
119800010613     C                             (vinflg = *blanks or
119900010613     C                              vinflg = *zeros)
120000010528     C
120100010528     C                   eval      invfil = depfil
120200010528     C                   endif
120300010528     C*
120400010529     C                   if        depfil <> invfil and
120500010529     C                             invfil > *zeros
120600010528     C                   eval      flgMulti = '1'
120700010528     C                   if        vinflg = *blanks
120800010528     C                   add       1             cntNonEl
120900010528     C                   endif
121000010528     C                   endif
121100010528     C*
121200010528     C                   if        vinflg = '2'
121300010528     C                   eval      flgStato = '2'
121400010528     C                   endif
121500010528     C*
121600010528     C                   ENDSR
121700010528     C***
121800010528
121900010528
122000010528
122100010528
122200990920      /TITLE Invio dei dati al punto operativo.
122300000613     C     invio         BEGSR
122400990920     C*
122500990920     C                   reset                   dscmz
122600010528     C                   move      invfil        cmzdst
122700990920     C                   eval      cmzfld = vlrfou
122800990920     C                   eval      cmzmbd = vlrhdl
122900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
123000000710     C                   if        prmfir = *blanks
123100010122     C                   eval      cmzfla = 'FNVAO00F'
123200010122     C                   eval      cmzmba = 'FNVAO00F'
123300000710     C                   else
123400000710     C                   eval      cmzfla = prmfir
123500000710     C                   eval      cmzmba = prmfir
123600000710     C                   endif
123700990920     C                   eval      cmznrr = *zeros
123800990920     C                   move      §ctrok        cmznrr
123900010123     C                   eval      cmzlba = vlrfl1
124000990920     C                   call(e)   'TIS711C'
124100990920     C                   parm                    dscmz
124200990921     C                   parm      *blanks       esito
124300990920     C                   if        %error
124400990920     C                             or cmzerr = '1'
124500990921     C                             or esito  = '1'
124600000710     C                   eval      wrkesito = '3'
124700990920     C                   endif
124800990920     C*
124900000613     C                   ENDSR
125000000613     C***
125100990910
125200010528
125300010528
125400010528
125500010528      /TITLE Invio dei dati al punto operativo.
125600010528     C     opeini        BEGSR
125700010528     C*
125800010528     C* Inizializzo flag e contatori operativi
125900010528     C                   movel     '0'           flgGiro           1
126000010528     C                   movel     '0'           flgMulti          1
126100010528     C                   movel     '1'           flgStato          1
126200010615     C                   movel     '0'           flgOk             1
126300010528     C                   z-add     *zeros        cntNonEl         10 0
126400010528     C                   z-add     *zeros        depfil            3 0
126500010528     C                   z-add     *zeros        invfil            3 0
126600030407     C*
126700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
126800030407     C                   if        %subst(vlrppt:1:1) = 'S'
126900030407     C                   seton                                        50
127000030407     C                   else
127100030407     C                   setoff                                       50
127200030407     C                   endif
127300070925     C*
127400070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
127500070925     C                   if        %subst(vlrppt:2:1) = 'S'
127600070925     C                   seton                                        51
127700070925     C                   else
127800070925     C                   setoff                                       51
127900070925     C                   endif
128000150319     C*
128100150319     C* se il 4° char dei parametri è T=Test
128200150319     C* vince questo test su quello precedente per impostare VAOPOE
128300150319     C                   SETOFF                                       55
128400150319     C                   IF        %subst(vlrppt:4:1) = 'T'
128500150319     C* inibisco la scrittura dei file TIORI e TIVGD
128600150319     C                   SETON                                        55
128700150319     C* imposto il PO di emissione come filiale fittizia
128800150319     C                   EVAL      VAOPOE = 999
128900150319     C                   ENDIF
129000010528     C*
129100010528     C                   ENDSR
129200010528     C***
129300060306
129400060306
129500060306     C*-------------------------------------------------------------------------*
129600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
129700060306     C*-------------------------------------------------------------------------*
129800060306     C     sr_Control    begsr
129900060306     C*
130000060306     C* inizializzo a ok il flag d controllo abilitazione
130100060306     C                   eval      Cli_Abil = *on
130200060306     C
130300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
130400060306     C                   clear                   woriIDC
130500060306     C                   eval      kteCOD = 'VAP'
130600060306     C                   eval      kteKE1 = oriIDC
130700060306     C     keytbe        chain     TNTBE01L
130800060306     C                   if        %found(Tntbe01l)
130900060306     C                   eval      woriIDC = TbeKe2
131000060306     C                   else
131100060306     C                   eval      Cli_Abil = *off
131200060306     C                   endif
131300060306     C*
131400060306     C                   endsr
131500010528
131600010528
131700010528
131800010528
131900000613     C     *inzsr        BEGSR
132000990910     C*
132100990910     C     *entry        plist
132200990920     C                   parm                    tivlrds
132300990921     C                   parm      wrkesito      esito
132400000724     C                   parm                    prmlit
132500000710     C                   parm                    prmfir
132600011105     C*
132700011105     C* Campi riferito al database
132800011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
132900011105     C     *like         define    tbeke1        kteKE1
133000011105     C*
133100011105     C* Definizione chiavi di lettura
133200011105     C* TNTBE01L - parziale
133300011105     C     keytbe        klist
133400011105     C                   kfld                    kteCOD                         *UTENTE
133500011105     C                   kfld                    kteKE1                         *CODICE
133600070926     C* TIVSS02L - parziale
133700070926     C     keyvss02_p    klist
133800070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
133900070926     C                   kfld                    vssISV                         *SERVIZIO
134000011105     C*
134100011105     C* Reperimento data e ora corrente
134200011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
134300011105     C                   movel     wn14          wn6               6 0          *ORA (6)
134400151106     C                   z-add     *zeros        datcor            8 0
134500151106     C                   eval      datcor = %dec(%date() : *ISO)
134600010529     C*
134700000613     C                   ENDSR
134800000613     C***
