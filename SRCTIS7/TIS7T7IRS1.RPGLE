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
003800031014     D tisi95ds      e ds
003900030624     D dorm01        e ds
004000011119     D dfar          e ds
004100090617     d trul33ds      e ds
004200090617     d kpjba         e ds
004300990910     D esito           s              1
004400000724     D prmlit          s             10
004500000710     D prmfir          s             10
004600990921     D wrkesito        s                   like(esito)
004700000613     D rrnum           s              6  0 INZ(*zeros)
004800060306     D Cli_Abil        s              1    inz(*off)
004900060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005000061031     D ConstINO        s             15    inz('ORM DA INTERNET')
005100090617     d §numori         s              7  0 inz
005200011122     D*-------------------
005300010731     D* COSTANTI
005400011122     D*-------------------
005500010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005600010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005700990908
005800091026
005900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006100091026
006200010528
006300010528
006400010528
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800000724     C*
006900010528     C                   exsr      opeini
007000010528     C                   exsr      rwvao
007100010528     C*
007200010528     C                   seton                                        lr
007300010528
007400010528
007500010528
007600010528
007700010528     C*--------------------------------------------------------
007800010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007900010528     C*--------------------------------------------------------
008000010528     C     PREELA        BEGSR
008100010528     C*
008200000724     C* SE OCCORRE SPEDIRE IN FILIALE
008300010528     C                   if        invfil <> *zeros and
008400010528     C                             flgGiro = '0'
008500010528     C*
008600010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008700010528     C                   eval      flgGiro = '1'
008800010529     C*
008900010529     C                   endif
009000010528     C*
009100010528     C                   ENDSR
009200010528     C***
009300010528
009400010528
009500010528
009600010528     C*--------------------------------------------------------
009700010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009800010528     C*--------------------------------------------------------
009900010528     C     ENDELA        BEGSR
010000020917     C*
010100010528     C                   ENDSR
010200010528     C***
010300010528
010400010528
010500010528
010600910830     C*--------------------------------------------------------
010700010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
010800910830     C*--------------------------------------------------------
010900010122     C     RWVAO         BEGSR
011000010528     C*
011100990914     C                   if        not %open(tivin00r)
011200990908     C                   open      tivin00r
011300990914     C                   endif
011400010122     C                   if        not %open(fnvaowwr)
011500010122     C                   open      fnvaowwr
011600990914     C                   endif
011700140416     C*
011800140416     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
011900151106     C                   call      'TIS7T7IIR'
012000140416     C                   parm      'O'           pIn_Opz           1
012100140416     C                   parm                    tivlrds
012200151106     C                   parm                    FNVAOEIIR
012300140416     C                   parm                    oriPRG
012400140528     C                   parm      *zeros        pIn_Dst           3
012500140416     C                   parm      *blanks       pOut_Esito        1
012600140416     C                   if        pOut_Esito = '2'
012700140416     C                   goto      FINE
012800140416     C                   endif
012900990910     C*
013000990910     C                   clear                   §CTROK
013100990910     C                   clear                   §CTRMO
013200990910     C                   clear                   §CTRNO
013300990910     C*
013400921023     C                   DO        *HIVAL
013500990913     C*
013600990915     C                   READ      tivin00r                               70
013700010731     C*
013800010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
013900010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014000010618     C*
014100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014200010618     C                   if        vinflg = '1'
014300010618     C                   eval      flgOk = '1'
014400010618     C                   endif
014500010618     C*
014600000905     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800990913     C*
014900010711     C                   if        *in70 = *off and
015000010711     C                             (vinflg = *blanks
015100010711     C                              or vinflg = '0'
015200010711     C                              or vinflg = '2')
015300990913     C*
015400010528     C                   clear                   FNVAO000
015500011105     C                   clear                   TIORI000
015600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015700010711     C                   if        vinflg = *blanks or vinflg = '0'
015800010711     C                   clear                   vinmsg
015900010711     C                   endif
016000010711     C*
016100151106     C                   eval      fnvaoeiir = vindta
016200151019     C*
016300151019     C* Stacco subito un numeratore univoco "ORM"
016400151106     C                   exsr      RTVNUM
016500080521     C*
016600080521     C* Verifico se richiesto nei parametri anagrafica cliente/tipo-file d particolari
016700080521     C* impostazioni dei flag operativi
016800080521     C                   if        %subst(vlrppt:3:1) = 'S'
016900080521     C                   eval      dorm01 = a_vaoFLO
017000080521     C                   eval      §ormks = 'S'
017100080521     C                   eval      a_vaoFLO = dorm01
017200080521     C                   endif
017300991022     C*
017400010122     C                   testb     x'3F'         a_vaoatb                 02
017500991022     C                   if        not *in02
017600991022     C*
017700971021     C                   SETOFF                                       303132
017800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
018000971023     C*    OPPURE TUTTI BLANKS
018100010122     C*-------------------
018200010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
018300010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
018400010122     C*-------------------
018500010122     C*
018600011119 xxx C*                  TESTN                   a_vaopoe             30
018700010417  "  C* N30              TESTN                   a_vaopoe               30
018800010417  "  C* N30              TESTN                   a_vaopoe                 30
018900010417     C*                  MOVE      a_vaopoe      W001A             1
019000010417     C*    *IN30         IFEQ      *OFF
019100010417     C*    W001A         ORLT      '0'
019200010417     C*                  SETON                                        32
019300010417     C*                  MOVE      *ZEROS        VAOPOE
019400010417     C*                  EVAL      vinmsg = 'VAOPOE'
019500010417     C*                  ELSE
019600010417     C*                  MOVE      a_vaopoe      VAOPOE
019700010417     C*                  ENDIF
019800010417  "  C*
019900010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
020000010604 xxx C                   IF        vlrpoi <> 999
020100010528     C                   EVAL      VAOPOE = vlrpoi
020200150331     C                   ELSE
020300150331     C*
020400150331     C                   IF        VAOPOE <> 999
020500150331     C*
020600150331     C* se i char dal 5° al 7° dei parametri è <> blank ed è numerico, allora è VAOPOE
020700150331     C                   IF        %subst(vlrppt:5:3) <> *blank
020800150331     C                   EVAL      PiStr=%trim(%subst(vlrppt:5:3))
020900150331     C                   EXSR      CHKNUM
021000150331     C                   IF        PiInt=*on
021100150331     C                   Z-ADD     PiVal         VAOPOE
021200150331     C                   ENDIF
021300150331     C                   ELSE
021400150331     C*
021500010528     C                   IF        a_vaopoe = *blanks or
021600011010     C                             a_vaopoe = *zeros  or
021700011010     C                             a_vaopoe = ' 000'
021800010528     C                   IF        a_vaopor <> *blanks and
021900011010     C                             a_vaopor <> *zeros  and
022000011010     C                             a_vaopor <> ' 000'
022100010528     C                   MOVE      a_vaopor      VAOPOE
022200010528     C                   ELSE
022300011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
022400010528     C                   MOVE      a_vaopoe      VAOPOE
022500010528     C                   ENDIF
022600010529     C                   ELSE
022700010529     C                   MOVE      a_vaopoe      VAOPOE
022800010528     C                   ENDIF
022900010528     C                   ENDIF
023000150331     C                   ENDIF
023100150331     C                   ENDIF
023200010122     C*
023300010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023400010528     C                   eval      depfil = VAOPOE
023500010528     C                   exsr      repfil
023600010528     C                   if        depfil = invfil and
023700010528     C                             (vinflg = *blanks
023800010528     C                              or vinflg = '0'
023900010528     C                              or vinflg = '2')
024000010528     C*
024100010528     C                   exsr      PREELA
024200010710     C*
024300010719     C*                  TESTN                   a_vaonsr             30
024400010719     C* N30              TESTN                   a_vaonsr               30
024500010719     C* N30              TESTN                   a_vaonsr                 30
024600010719     C*                  MOVE      a_vaonsr      W001A             1
024700010719     C*    *IN30         IFEQ      *OFF
024800010719     C*    W001A         ORLT      '0'
024900010719     C*                  SETON                                        32
025000010719     C*                  MOVE      *ZEROS        VAONSR
025100010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
025200010719     C*                  ELSE
025300010719     C*                  MOVE      a_vaonsr      VAONSR
025400010719     C*                  ENDIF
025500010122     C*
025600010719     C*                  TESTN                   a_vaonor             30
025700010719     C* N30              TESTN                   a_vaonor               30
025800010719     C* N30              TESTN                   a_vaonor                 30
025900010719     C*                  MOVE      a_vaonor      W001A             1
026000010719     C*    *IN30         IFEQ      *OFF
026100010719     C*    W001A         ORLT      '0'
026200010719     C*                  SETON                                        32
026300010719     C*                  MOVE      *ZEROS        VAONOR
026400010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
026500010719     C*                  ELSE
026600010719     C*                  MOVE      a_vaonor      VAONOR
026700010719     C*                  ENDIF
026800010122     C*
026900010719     C*                  TESTN                   a_vaonrv             30
027000010719     C* N30              TESTN                   a_vaonrv               30
027100010719     C* N30              TESTN                   a_vaonrv                 30
027200010719     C*                  MOVE      a_vaonrv      W001A             1
027300010719     C*    *IN30         IFEQ      *OFF
027400010719     C*    W001A         ORLT      '0'
027500010719     C*                  SETON                                        32
027600010719     C*                  MOVE      *ZEROS        VAONRV
027700010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
027800010719     C*                  ELSE
027900010719     C*                  MOVE      a_vaonrv      VAONRV
028000010719     C*                  ENDIF
028100010122     C*
028200010122     C                   TESTN                   a_vaodao             30
028300010122     C  N30              TESTN                   a_vaodao               30
028400010122     C  N30              TESTN                   a_vaodao                 30
028500010122     C                   MOVE      a_vaodao      W001A             1
028600010122     C     *IN30         IFEQ      *OFF
028700010122     C     W001A         ORLT      '0'
028800010122     C                   SETON                                        32
028900050421     C                   MOVE      *ZEROS        VAODAO
029000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
029100010122     C                   ELSE
029200050421     C                   MOVE      a_vaodao      VAODAO
029300010122     C                   ENDIF
029400010122     C*
029500010122     C                   TESTN                   a_vaooao             30
029600010122     C  N30              TESTN                   a_vaooao               30
029700010122     C  N30              TESTN                   a_vaooao                 30
029800010122     C                   MOVE      a_vaooao      W001A             1
029900010122     C     *IN30         IFEQ      *OFF
030000010122     C     W001A         ORLT      '0'
030100010122     C                   SETON                                        32
030200050421     C                   MOVE      *ZEROS        VAOOAO
030300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
030400010122     C                   ELSE
030500050421     C                   MOVE      a_vaooao      VAOOAO
030600010122     C                   ENDIF
030700010122     C*
030800010122     C                   TESTN                   a_vaocor             30
030900010122     C  N30              TESTN                   a_vaocor               30
031000010122     C  N30              TESTN                   a_vaocor                 30
031100010122     C                   MOVE      a_vaocor      W001A             1
031200010122     C     *IN30         IFEQ      *OFF
031300010122     C     W001A         ORLT      '0'
031400010122     C                   SETON                                        32
031500010122     C                   MOVE      *ZEROS        VAOCOR
031600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
031700010122     C                   ELSE
031800050421     C                   MOVE      a_vaocor      VAOCOR
031900010122     C                   ENDIF
032000010122     C*
032100010122     C                   TESTN                   a_vaocra             30
032200010122     C  N30              TESTN                   a_vaocra               30
032300010122     C  N30              TESTN                   a_vaocra                 30
032400010122     C                   MOVE      a_vaocra      W001A             1
032500010122     C     *IN30         IFEQ      *OFF
032600010122     C     W001A         ORLT      '0'
032700010122     C                   SETON                                        32
032800010122     C                   MOVE      *ZEROS        VAOCRA
032900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
033000010122     C                   ELSE
033100010122     C                   MOVE      a_vaocra      VAOCRA
033200010122     C                   ENDIF
033300010122     C*
033400010719     C                   IF        a_vaocrc = *blanks or
033500010719     C                             a_vaocrc = *zeros
033600010719     C                   ELSE
033700010719     C                   TESTN                   a_vaocrc             30
033800010719     C  N30              TESTN                   a_vaocrc               30
033900010719     C  N30              TESTN                   a_vaocrc                 30
034000010719     C                   MOVE      a_vaocrc      W001A             1
034100010719     C     *IN30         IFEQ      *OFF
034200010719     C     W001A         ORLT      '0'
034300010719     C                   SETON                                        32
034400010719     C                   MOVE      *ZEROS        VAOCRC
034500010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
034600010719     C                   ELSE
034700010719     C                   MOVE      a_vaocrc      VAOCRC
034800010719     C                   ENDIF
034900010719     C                   ENDIF
035000010122     C*
035100010122     C                   TESTN                   a_vaodar             30
035200010122     C  N30              TESTN                   a_vaodar               30
035300010122     C  N30              TESTN                   a_vaodar                 30
035400010122     C                   MOVE      a_vaodar      W001A             1
035500010122     C     *IN30         IFEQ      *OFF
035600010122     C     W001A         ORLT      '0'
035700010122     C                   SETON                                        32
035800010122     C                   MOVE      *ZEROS        VAODAR
035900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
036000010122     C                   ELSE
036100010122     C                   MOVE      a_vaodar      VAODAR
036200010122     C                   ENDIF
036300010122     C*
036400010122     C                   TESTN                   a_vaoorr             30
036500010122     C  N30              TESTN                   a_vaoorr               30
036600010122     C  N30              TESTN                   a_vaoorr                 30
036700010122     C                   MOVE      a_vaoorr      W001A             1
036800010122     C     *IN30         IFEQ      *OFF
036900010122     C     W001A         ORLT      '0'
037000010122     C                   SETON                                        32
037100010122     C                   MOVE      *ZEROS        VAOORR
037200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
037300010122     C                   ELSE
037400010122     C                   MOVE      a_vaoorr      VAOORR
037500010122     C                   ENDIF
037600010122     C*
037700010122     C                   TESTN                   a_vaoncl             30
037800010122     C  N30              TESTN                   a_vaoncl               30
037900010122     C  N30              TESTN                   a_vaoncl                 30
038000010122     C                   MOVE      a_vaoncl      W001A             1
038100010122     C     *IN30         IFEQ      *OFF
038200010122     C     W001A         ORLT      '0'
038300010122     C                   SETON                                        32
038400010122     C                   MOVE      *ZEROS        VAONCL
038500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
038600010122     C                   ELSE
038700010122     C                   MOVE      a_vaoncl      VAONCL
038800010122     C                   ENDIF
038900010122     C*
039000010122     C                   MOVE      *ZERO         VAOPKG
039100010122     C                   MOVE      a_vaopkg      WASC09
039200010122     C                   TESTN                   WI09                 30
039300010122     C  N30              TESTN                   WI09                   30
039400010122     C  N30              TESTN                   WI09                     30
039500010122     C                   MOVE      WI09          W001A             1
039600010122     C     *IN30         IFEQ      *OFF
039700010122     C     W001A         ORLT      '0'
039800010122     C     W001A         ANDNE     *BLANK
039900010122     C                   SETON                                        32
040000010122     C                   MOVE      *ZERO         VAOPKG
040100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
040200010122     C                   ELSE
040300010122     C                   MOVEL     WI09          VAOPKG
040400010122     C                   TESTN                   WD09                 30
040500010122     C  N30              TESTN                   WD09                     30
040600010122     C                   MOVE      WD09          W001A             1
040700010122     C     *IN30         IFEQ      *OFF
040800010122     C     W001A         ORLT      '0'
040900010122     C     W001A         ANDNE     *BLANK
041000010122     C                   SETON                                        32
041100010122     C                   MOVE      *ZERO         VAOPKG
041200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
041300010122     C                   ELSE
041400010122     C                   MOVE      WD09          VAOPKG
041500010122     C                   ENDIF
041600050118     C                   ENDIF
041700010122     C*
041800010122     C                   MOVE      *ZERO         VAOVLM
041900010122     C                   MOVE      a_vaovlm      WASC07
042000010122     C                   TESTN                   WI07                 30
042100010122     C  N30              TESTN                   WI07                   30
042200010122     C  N30              TESTN                   WI07                     30
042300010122     C                   MOVE      WI07          W001A             1
042400010122     C     *IN30         IFEQ      *OFF
042500010122     C     W001A         ORLT      '0'
042600010122     C     W001A         ANDNE     *BLANK
042700010122     C                   SETON                                        32
042800010122     C                   MOVE      *ZERO         VAOVLM
042900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
043000010122     C                   ELSE
043100010122     C                   MOVEL     WI07          VAOVLM
043200010122     C                   TESTN                   WD07                 30
043300010122     C  N30              TESTN                   WD07                   30
043400010122     C  N30              TESTN                   WD07                     30
043500010122     C                   MOVE      WD07          W001A             1
043600010122     C     *IN30         IFEQ      *OFF
043700010122     C     W001A         ORLT      '0'
043800010122     C     W001A         ANDNE     *BLANK
043900010122     C                   SETON                                        32
044000010122     C                   MOVE      *ZERO         VAOVLM
044100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
044200010122     C                   ELSE
044300010122     C                   MOVE      WD07          VAOVLM
044400010122     C                   ENDIF
044500050118     C                   ENDIF
044600010122     C*
044700010122     C                   TESTN                   a_vaobnc             30
044800010122     C  N30              TESTN                   a_vaobnc               30
044900010122     C  N30              TESTN                   a_vaobnc                 30
045000010122     C                   MOVE      a_vaobnc      W001A             1
045100010122     C     *IN30         IFEQ      *OFF
045200010122     C     W001A         ORLT      '0'
045300010122     C                   SETON                                        32
045400010122     C                   MOVE      *ZEROS        VAOBNC
045500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
045600010122     C                   ELSE
045700010122     C                   MOVE      a_vaobnc      VAOBNC
045800010122     C                   ENDIF
045900010122     C*
046000010122     C                   TESTN                   a_vaoblc             30
046100010122     C  N30              TESTN                   a_vaoblc               30
046200010122     C  N30              TESTN                   a_vaoblc                 30
046300010122     C                   MOVE      a_vaoblc      W001A             1
046400010122     C     *IN30         IFEQ      *OFF
046500010122     C     W001A         ORLT      '0'
046600010122     C                   SETON                                        32
046700010122     C                   MOVE      *ZEROS        VAOBLC
046800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
046900010122     C                   ELSE
047000010122     C                   MOVE      a_vaoblc      VAOBLC
047100010122     C                   ENDIF
047200010122     C*
047300010122     C                   TESTN                   a_vaoatt             30
047400010122     C  N30              TESTN                   a_vaoatt               30
047500010122     C  N30              TESTN                   a_vaoatt                 30
047600010122     C                   MOVE      a_vaoatt      W001A             1
047700010122     C     *IN30         IFEQ      *OFF
047800010122     C     W001A         ORLT      '0'
047900010122     C                   SETON                                        32
048000010122     C                   MOVE      *ZEROS        VAOATT
048100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
048200010122     C                   ELSE
048300010122     C                   MOVE      a_vaoatt      VAOATT
048400010122     C                   ENDIF
048500010122     C*
048600010122     C                   TESTN                   a_vaomtc             30
048700010122     C  N30              TESTN                   a_vaomtc               30
048800010122     C  N30              TESTN                   a_vaomtc                 30
048900010122     C                   MOVE      a_vaomtc      W001A             1
049000010122     C     *IN30         IFEQ      *OFF
049100010122     C     W001A         ORLT      '0'
049200010122     C                   SETON                                        32
049300010122     C                   MOVE      *ZEROS        VAOMTC
049400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
049500010122     C                   ELSE
049600010122     C                   MOVE      a_vaomtc      VAOMTC
049700010122     C                   ENDIF
049800010122     C*
049900010719     C                   IF        a_vaoksc = *blanks or
050000010719     C                             a_vaoksc = *zeros
050100010719     C                   ELSE
050200010719     C                   TESTN                   a_vaoksc             30
050300010719     C  N30              TESTN                   a_vaoksc               30
050400010719     C  N30              TESTN                   a_vaoksc                 30
050500010719     C                   MOVE      a_vaoksc      W001A             1
050600010719     C     *IN30         IFEQ      *OFF
050700010719     C     W001A         ORLT      '0'
050800010719     C                   SETON                                        32
050900010719     C                   MOVE      *ZEROS        VAOKSC
051000010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
051100010719     C                   ELSE
051200010719     C                   MOVE      a_vaoksc      VAOKSC
051300010719     C                   ENDIF
051400010719     C                   ENDIF
051500010122     C*
051600010710     C                   IF        a_vaoctr > *blanks
051700010122     C                   TESTN                   a_vaoctr             30
051800010122     C  N30              TESTN                   a_vaoctr               30
051900010122     C  N30              TESTN                   a_vaoctr                 30
052000010122     C                   MOVE      a_vaoctr      W001A             1
052100010122     C     *IN30         IFEQ      *OFF
052200010122     C     W001A         ORLT      '0'
052300010122     C                   SETON                                        32
052400010627     C                   MOVE      *blanks       VAOCTR
052500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
052600010122     C                   ELSE
052700010122     C                   MOVE      a_vaoctr      VAOCTR
052800010122     C                   ENDIF
052900010710     C                   ENDIF
053000010122     C*
053100010122     C                   TESTN                   a_vaopor             30
053200010122     C  N30              TESTN                   a_vaopor               30
053300010122     C  N30              TESTN                   a_vaopor                 30
053400010122     C                   MOVE      a_vaopor      W001A             1
053500010122     C     *IN30         IFEQ      *OFF
053600010122     C     W001A         ORLT      '0'
053700010122     C                   SETON                                        32
053800010122     C                   MOVE      *ZEROS        VAOPOR
053900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
054000010122     C                   ELSE
054100010122     C                   MOVE      a_vaopor      VAOPOR
054200010122     C                   ENDIF
054300010122     C*
054400010719     C                   IF        a_vaopoc = *blanks or
054500010719     C                             a_vaopoc = *zeros
054600010719     C                   ELSE
054700010122     C                   TESTN                   a_vaopoc             30
054800010122     C  N30              TESTN                   a_vaopoc               30
054900010122     C  N30              TESTN                   a_vaopoc                 30
055000010122     C                   MOVE      a_vaopoc      W001A             1
055100010122     C     *IN30         IFEQ      *OFF
055200010122     C     W001A         ORLT      '0'
055300010122     C                   SETON                                        32
055400010122     C                   MOVE      *ZEROS        VAOPOC
055500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
055600010122     C                   ELSE
055700010122     C                   MOVE      a_vaopoc      VAOPOC
055800010122     C                   ENDIF
055900010719     C                   ENDIF
056000010122     C*
056100010719     C                   IF        a_vaosto = *blanks or
056200010719     C                             a_vaosto = *zeros
056300010719     C                   ELSE
056400010122     C                   TESTN                   a_vaosto             30
056500010122     C  N30              TESTN                   a_vaosto               30
056600010122     C  N30              TESTN                   a_vaosto                 30
056700010122     C                   MOVE      a_vaosto      W001A             1
056800010122     C     *IN30         IFEQ      *OFF
056900010122     C     W001A         ORLT      '0'
057000010122     C                   SETON                                        32
057100010122     C                   MOVE      *ZEROS        VAOSTO
057200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
057300010122     C                   ELSE
057400010122     C                   MOVE      a_vaosto      VAOSTO
057500010122     C                   ENDIF
057600010719     C                   ENDIF
057700971021     C*
057800971021     C* CAMPI ALFABETICI:
057900010122     C                   MOVE      a_vaoatb      VAOATB
058000010122     C                   MOVE      a_vaotor      VAOTOR
058100010531     C*
058200010531     C                   IF        a_vaotco = *blanks
058300010531     C                   MOVE      'F'           VAOTCO
058400010531     C                   ELSE
058500010531     C                   MOVE      a_vaotco      VAOTCO
058600010531     C                   ENDIF
058700010531     C*
058800010122     C                   MOVE      a_vaorso      VAORSO
058900010122     C                   MOVE      a_vaoino      VAOINO
059000010122     C                   MOVE      a_vaocao      VAOCAO
059100010122     C                   MOVE      a_vaoloo      VAOLOO
059200031014     C                   MOVE      a_vaopro      VAOPRO
059300090129     C                   MOVE      a_vaonao      VAONAO
059400131007     C*
059500131007     C                   MOVE      a_vaorsr      VAORSR
059600131007     C                   MOVE      a_vaoinr      VAOINR
059700131007     C                   MOVE      a_vaocar      VAOCAR
059800131007     C                   MOVE      a_vaolor      VAOLOR
059900131007     C                   MOVE      a_vaoprr      VAOPRR
060000131007     C                   MOVE      a_vaonar      VAONAR
060100131007     C*
060200131007     C                   MOVE      a_vaorer      VAORER
060300131007     C                   MOVE      a_vaoter      VAOTER
060400131007     C                   MOVE      a_vaorsc      VAORSC
060500131007     C                   MOVE      a_vaoinc      VAOINC
060600131007     C                   MOVE      a_vaocac      VAOCAC
060700131007     C                   MOVE      a_vaoloc      VAOLOC
060800131007     C                   MOVE      a_vaoprc      VAOPRC
060900131007     C                   MOVE      a_vaonac      VAONAC
061000131007     C*
061100131007     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
061200131007     C*** VAONAO
061300131007     C                   IF        %trim(VAONAO) = 'I'  OR
061400131007     C                             %trim(VAONAO) = 'IT' OR
061500131007     C                             %trim(VAONAO) = 'VA' OR
061600131007     C                             %trim(VAONAO) = 'SM'
061700131007     C                   EVAL      VAONAO = *blanks
061800131007     C                   ENDIF
061900131007     C*** VAONAR
062000131007     C                   IF        %trim(VAONAR) = 'I'  OR
062100131007     C                             %trim(VAONAR) = 'IT' OR
062200131007     C                             %trim(VAONAR) = 'VA' OR
062300131007     C                             %trim(VAONAR) = 'SM'
062400131007     C
062500131007     C                   EVAL      VAONAR = *blanks
062600131007     C                   ENDIF
062700131007     C*** VAONAC
062800131007     C                   IF        %trim(VAONAC) = 'I'  OR
062900131007     C                             %trim(VAONAC) = 'IT' OR
063000131007     C                             %trim(VAONAC) = 'VA' OR
063100131007     C                             %trim(VAONAC) = 'SM'
063200131007     C                   EVAL      VAONAC = *blanks
063300131007     C                   ENDIF
063400131007     C*
063500031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
063600031014     C                   IF        VAOPRO  = *blanks AND
063700050907     C                             VAOCAO <> *blanks AND
063800050907     C                             VAONAO  = *blanks                            * solo x Italia
063900031014     C                   CLEAR                   TISI95DS
064000031014     C                   EVAL      I95TCN = '3'
064100031014     C                   Z-ADD     datcor        I95DAT
064200031014     C                   EVAL      I95CAP = VAOCAO
064300031014     C                   EVAL      I95LOC = VAOLOO
064400031014     C                   CALL      'TISI95R'
064500031014     C                   PARM                    TISI95DS
064600031014     C                   EVAL      VAOPRO = O95PRV
064700031014     C                   ENDIF
064800131007     C*
064900031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
065000031014     C                   IF        VAOPRR  = *blanks AND
065100050907     C                             VAOCAR <> *blanks AND
065200050907     C                             VAONAR  = *blanks                            * solo x Italia
065300031014     C                   CLEAR                   TISI95DS
065400031014     C                   EVAL      I95TCN = '3'
065500031014     C                   Z-ADD     datcor        I95DAT
065600031014     C                   EVAL      I95CAP = VAOCAR
065700031014     C                   EVAL      I95LOC = VAOLOR
065800031014     C                   CALL      'TISI95R'
065900031014     C                   PARM                    TISI95DS
066000031014     C                   EVAL      VAOPRR = O95PRV
066100031014     C                   ENDIF
066200131007     C*
066300031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
066400031014     C                   IF        VAOPRC  = *blanks AND
066500050907     C                             VAOCAC <> *blanks AND
066600050907     C                             VAONAC  = *blanks                            * solo x Italia
066700031014     C                   CLEAR                   TISI95DS
066800031014     C                   EVAL      I95TCN = '3'
066900031014     C                   Z-ADD     datcor        I95DAT
067000031014     C                   EVAL      I95CAP = VAOCAC
067100031014     C                   EVAL      I95LOC = VAOLOC
067200031014     C                   CALL      'TISI95R'
067300031014     C                   PARM                    TISI95DS
067400031014     C                   EVAL      VAOPRC = O95PRV
067500031014     C                   ENDIF
067600031014     C*
067700010122     C                   MOVE      a_vaoffd      VAOFFD
067800010122     C                   MOVE      a_vaormp      VAORMP
067900010122     C                   MOVE      a_vaonam      VAONAM
068000010122     C                   MOVE      a_vaopag      VAOPAG
068100010122     C                   MOVE      a_vaozor      VAOZOR
068200010122     C                   MOVE      a_vaono1      VAONO1
068300010122     C                   MOVE      a_vaono2      VAONO2
068400010122     C                   MOVE      a_vaoddt      VAODDT
068500010122     C                   MOVE      a_vaorfa      VAORFA
068600010122     C                   MOVE      a_vaospi      VAOSPI
068700010122     C                   MOVE      a_vaoflo      VAOFLO
068800050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
068900050304     C                   IF        VAOPKG =  *zeros AND
069000050304     C                             VAOVLM =  *zeros AND
069100050304     C                             VAOBNC =  *zeros AND
069200050304     C                             VAONCL <> *zeros
069300050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
069400050304     C                   ENDIF
069500030603     C*
069600030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
069700030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
069800030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
069900050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
070000150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
070100930409     C*
070200010604     C  N31              ADD       1             §CTROK            7 0
070300010604     C   32              ADD       1             §CTRMO            7 0
070400010604     C   31              ADD       1             §CTRNO            7 0
070500030624     C  N31              EXSR      CHKCOR
070600011105     C  N31              EXSR      WRIORI
070700050826     C  N31              EXSR      WRIVGD
070800050421     C*
070900050421     C  N31              WRITE     FNVAO000
071000140416     C*
071100151019     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
071200151106     C                   if        not *in31
071300151019     C                   exsr      EXEFORZ
071400151106     C                   call      'TIS7T7IIR'
071500140416     C                   parm      'W'           pIn_Opz           1
071600140416     C                   parm                    tivlrds
071700151106     C                   parm                    FNVAOEIIR
071800140416     C                   parm                    oriPRG
071900140528     C                   parm      *zeros        pIn_Dst           3
072000140416     C                   parm      *blanks       pOut_Esito        1
072100140416     C                   endif
072200140416     C                   if        pOut_Esito = '2'
072300140416     C                   seton                                        32
072400140416     C                   endif
072500990910     C*
072600010604     C                   if        *in31 = *off and
072700010604     C                             *in32 = *off
072800990910     C                   eval      vinflg = '1'
072900990910     C                   else
073000990910     C                   eval      vinflg = '2'
073100990910     C                   endif
073200991022     C                   endif
073300990910     C                   endif
073400010604     C*
073500010604     C                   endif
073600000905     C*
073700000905     C                   else
073800000905     C                   eval      vinflg = '1'
073900000905     C                   endif
074000000905     C*
074100000905     C  N70              update    tivin000
074200991022     C*
074300991022     C  N70              ENDdo
074400140416     C*
074500140416     C     FINE          tag
074600140528     C*
074700140528     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
074800151106     C                   call      'TIS7T7IIR'
074900140528     C                   parm      'C'           pIn_Opz           1
075000140528     C                   parm                    tivlrds
075100151106     C                   parm                    FNVAOEIIR
075200140528     C                   parm                    oriPRG
075300140528     C                   parm      *zeros        pIn_Dst           3
075400140528     C                   parm      *blanks       pOut_Esito        1
075500010528     C*
075600010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075700010528     C                   if        cntNonEl = *zeros or
075800010528     C                             flgMulti = '0'
075900010528     C* Se non ci sono record con errori ...
076000010528     C                   if        §ctrno = 0 and
076100010604     C                             §ctrmo = 0 and
076200010528     C                             flgStato <> '2'
076300990910     C* ... restituisco esito OK.
076400990921     C                   eval      wrkesito = '0'
076500990910     C                   else
076600000710     C                   if        §ctrok > 0
076700990921     C                   eval      wrkesito = '1'
076800000710     C                   else
076900010615     C                   if        flgOk = '0'
077000010615     C                   eval      wrkesito = '2'
077100010615     C                   else
077200010615     C                   eval      wrkesito = '6'
077300010615     C                   endif
077400990910     C                   endif
077500010528     C                   endif
077600010529     C                   else
077700010529     C                   eval      wrkesito = '9'
077800000710     C                   endif
077900990910     C*
078000990914     C                   if        %open(tivin00r)
078100990908     C                   close     tivin00r
078200990914     C                   endif
078300010122     C                   if        %open(fnvaowwr)
078400010122     C                   close     fnvaowwr
078500990914     C                   endif
078600990910     C*
078700010528     C                   if        vlrpoi <> 999
078800010528     C                   eval      invfil = vlrpoi
078900010528     C                   endif
079000010528     C*
079100990920     C                   if        §ctrok > 0
079200010528     C                             and invfil > *zeros
079300140528     C                   exsr      invio
079400140528     C*
079500140528     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
079600140528     C                   move      invfil        cmzdst
079700151106     C                   call      'TIS7T7IIR'
079800140528     C                   parm      'S'           pIn_Opz           1
079900140528     C                   parm                    tivlrds
080000151106     C                   parm                    FNVAOEIIR
080100140528     C                   parm                    oriPRG
080200140528     C                   parm      cmzdst        pIn_Dst           3
080300140528     C                   parm      *blanks       pOut_Esito        1
080400990920     C                   endif
080500010612     C*
080600010612     C                   if        flgGiro = '1'
080700010612     C                   exsr      endela
080800010612     C                   endif
080900010528     C*
081000910830     C                   ENDSR
081100000613     C***
081200140417
081300140417
081400140417     C*------------------------------------------------------------------------*
081500151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
081600140417     C*------------------------------------------------------------------------*
081700151019     C     RTVNUM        BEGSR
081800140417     C*
081900151019     C* Stacco un numeratore da AZNUM
082000151019     C                   clear                   §numori
082100140417     C                   clear                   trul33ds
082200140417     C                   eval      I33OPE = *zeros
082300140417     C                   eval      I33CNU = 600
082400140417     C                   eval      I33NUM = 1
082500140417     C                   movel     TRUL33DS      KPJBU
082600140417     C                   call      'TRUL33R'
082700140417     C                   parm                    KPJBA
082800140417     C                   movel     KPJBU         TRUL33DS
082900140417     C                   if        O33ERR = *zeros
083000140417     C                   z-add     O33NRF        §numori
083100140417     c                   else
083200140417     c                   eval      §numori = *all'9'
083300140417     C                   seton                                        31        * errore bloccante
083400140417     C                   endif
083500140417     C*
083600151106     C                   eval      vaoNOR = §numori
083700140417     C*
083800140417     C                   ENDSR
083900151019
084000151019
084100151019     C*------------------------------------------------------------------------*
084200151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
084300151019     C*------------------------------------------------------------------------*
084400151106     C     EXEFORZ       BEGSR
084500151019     C*
084600151106     C                   move(p)   VAOPOE        a_vaopoe                       * forzatura necessar
084700151106     C                   move(p)   VAOCOR        a_vaocor                       * forzatura necessar
084800151106     C                   move(p)   VAONOR        a_vaonor                       * forzatura necessar
084900151019     C*
085000151019     C                   ENDSR
085100140417
085200150319
085300150319     C*----------------------------------------------------*
085400150319     C*  CONTROLLO NUMERICITA' CAMPI
085500150319     C*----------------------------------------------------*
085600150319     C     CHKNUM        BEGSR
085700150319     C*
085800150319     C                   IF        PiDecChr = *blanks
085900150319     C                   EVAL      PiDecChr = '.'
086000150319     C                   ENDIF
086100150319     C*
086200150319     C                   CALL(e)   'ISNUMERIC'
086300150319     C                   PARM                    PiStr            30
086400150319     C                   PARM                    PiDecChr          1
086500150319     C                   PARM      *ZEROS        PiVal            30 9
086600150319     C                   PARM      '0'           PiInt             1
086700150319     C                   PARM      '0'           PiNum             1
086800150319     C                   IF        %error
086900150319     C                   EVAL      PiNum=*off
087000150319     C                   ENDIF
087100150319     C*
087200150319     C                   ENDSR
087300150319     C***
087400030624
087500030624
087600030624     C*------------------------------------------------------------------------*
087700030624     C* CHKCOR - Considerazioni su VAOCOR
087800030624     C*------------------------------------------------------------------------*
087900030624     C     CHKCOR        BEGSR
088000030624     C*
088100030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
088200030624     C                   if        vaoTCO = 'F'
088300030624     C* Quindi verifico se il codice ordinante non è valorizzato
088400050323     C                   if        vaoCOR = *zeros
088500050323     C*                  if        vaoCOR = *zeros  AND
088600050323     C*                            vaoRSO = *blanks AND
088700050323     C*                            vaoINO = *blanks AND
088800050323     C*                            vaoCAO = *blanks AND
088900050323     C*                            vaoLOO = *blanks AND
089000050323     C*                            vaoPRO = *blanks AND
089100050323     C*                            vaoNAO = *blanks
089200030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
089300030624     C                   move      vlrKSC        wCli              7 0
089400050323     C                   movel(p)  wCli          acrCRO
089500030624     C     acrCRO        setll     fnacr01l
089600030624     C                   if        %found(fnacr01l)
089700030624     C                   read      fnacr01l
089800030624     C                   dow       not %eof(fnacr01l)
089900030624     C                   movel     acrCRO        wCliAcr           7 0
090000030624     C                   if        wCliAcr = wCli
090100030624     C                   eval      vaoCOR = acrCRO
090200030624     C                   leave
090300030624     C                   else
090400030624     C                   leave
090500030624     C                   endif
090600030624     C                   read      fnacr01l
090700030624     C                   enddo
090800030624     C                   endif
090900030624     C                   endif
091000030624     C                   endif
091100030624     C*
091200030624     C                   if        vaoTCO = 'I'
091300030624     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
091400030624     C                   if        vlrKSC = '0ORM0000'
091500030624     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
091600061031     C***                eval      vaoINO = 'ORM DA INTERNET'
091700061031     C                   movel     ConstINO      vaoINO
091800030624     C                   eval      vaoCAO = '40138'
091900030624     C                   eval      vaoLOO = 'BOLOGNA'
092000030624     C                   eval      vaoPRO = 'BO'
092100030624     C                   endif
092200030624     C                   endif
092300030624     C*
092400030624     C                   ENDSR
092500030624
092600990920
092700010528
092800011105     C*------------------------------------------------------------------------*
092900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
093000011105     C*------------------------------------------------------------------------*
093100011105     C     WRIORI        BEGSR
093200011105     C*
093300090617     C* Come prima cosa stacco un numeratore da AZNUM
093400151019     C***                clear                   trul33ds
093500151019     C***                eval      I33OPE = *zeros
093600151019     C***                eval      I33CNU = 600
093700151019     C***                eval      I33NUM = 1
093800151019     C***                movel     TRUL33DS      KPJBU
093900151019     C***                call      'TRUL33R'
094000151019     C***                parm                    KPJBA
094100151019     C***                movel     KPJBU         TRUL33DS
094200151019     C***                if        O33ERR = *zeros
094300151019     C***                z-add     O33NRF        §numori
094400151019     c***                else
094500151019     C***                eval      §numori = *all'9'
094600151019     C***                endif
094700070925     C*
094800070925     C* Giro "standard" (ovvero x cliente d invio dati)
094900070925     C                   if        *in51 = *off
095000011105     C*
095100011122     C* Quindi imposto i campi "extra-VAO"
095200011122     C                   if        vlrKSC = '0ORM0000'
095300011122     C*
095400011122     C                   if        vaoCOR > *zeros
095500011122     C                   movel     vaoCOR        wrkCOR7           7 0
095600011122     C*
095700011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
095800011122     C* codice padre. Se è un padre assegno già il codice ordinante.
095900011122     C                   clear                   tibs10ds
096000011122     C                   eval      d10drf = datcor                              *data riferimento
096100011122     C                   eval      d10paf = 'P'                                 *cerca il padre
096200011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
096300091026     C*
096400091026     C* Preparazione chiamata al TIBS10R1
096500091026     C                   clear                   tibs10r1ds
096600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
096700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
096800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
096900091026     C*
097000091026     C                   call      'TIBS10R1'
097100091026     C                   parm                    tibs10r1ds
097200091026     C*
097300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
097400011122     C*
097500011122     C                   if        d10err <> *blanks or                         *No unif. o padre
097600011122     C                             d10cop =  *zeros  or
097700011122     C                             d10cop =  d10cod
097800011122     C                   move      d10cod        oriIDC
097900011122     C                   else                                                   *Figlio
098000011122     C                   move      d10cop        oriIDC
098100011122     C                   endif
098200011122     C*
098300011122     C                   else
098400011122     C                   eval      oriIDC = *zeros
098500011122     C                   endif
098600011122     C*
098700011122     C                   else
098800011122     C                   eval      oriIDC = vlrKSC
098900011122     C                   endif
099000070925     C*
099100070925     C* Giro "forzato" (ovvero x codice ordinante)
099200070925     C                   else
099300070925     C*
099400070925     C                   if        vaoCOR > *zeros
099500070925     C                   movel     vaoCOR        wrkCOR7           7 0
099600070925     C*
099700070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
099800070925     C* codice padre. Se è un padre assegno già il codice ordinante.
099900070925     C                   clear                   tibs10ds
100000070925     C                   eval      d10drf = datcor                              *data riferimento
100100070925     C                   eval      d10paf = 'P'                                 *cerca il padre
100200070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
100300091026     C*
100400091026     C* Preparazione chiamata al TIBS10R1
100500091026     C                   clear                   tibs10r1ds
100600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
100700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
100800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
100900091026     C*
101000091026     C                   call      'TIBS10R1'
101100091026     C                   parm                    tibs10r1ds
101200091026     C*
101300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
101400070925     C*
101500070925     C                   if        d10err <> *blanks or                         *No unif. o padre
101600070925     C                             d10cop =  *zeros  or
101700070925     C                             d10cop =  d10cod
101800070925     C                   move      d10cod        oriIDC
101900070925     C                   else                                                   *Figlio
102000070925     C                   move      d10cop        oriIDC
102100070925     C                   endif
102200070926     C*
102300070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
102400070926     C* al servizio d rintraccia ritiri on-line
102500070926     C                   eval      vssKSU = oriIDC
102600070926     C                   eval      vssISV = 'T3'
102700070926     C     keyvss02_p    chain     tivss02l
102800070926     C                   if        %found(tivss02l)
102900070926     C                   if        vssdde <= datcor AND
103000070926     C                             datcor <= vssdsc
103100070926     C                   else
103200070926     C                   eval      oriIDC = vlrKSC
103300070926     C                   endif
103400070926     C                   else
103500070926     C                   eval      oriIDC = vlrKSC
103600070926     C                   endif
103700070926     C*
103800070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
103900070926     C* anagrafica clienti ritiro
104000070926     C                   setoff                                       52
104100070926     C                   eval      acrCRO = wrkCOR7 * 1000
104200070926     C     acrCRO        setll     fnacr01l
104300070926     C                   if        %found(fnacr01l)
104400070926     C                   read      fnacr01l
104500070926     C                   dow       not %eof(fnacr01l)
104600070926     C                   movel     acrCRO        wCliAcr           7 0
104700070926     C                   if        wCliAcr = wrkCOR7
104800070926     C                   seton                                        52
104900070926     C                   leave
105000070926     C                   else
105100070926     C                   leave
105200070926     C                   endif
105300070926     C                   read      fnacr01l
105400070926     C                   enddo
105500070926     C                   endif
105600070926     C*
105700070926     C                   if        *in52  = *off
105800070926     C                   eval      oriIDC = vlrKSC
105900070926     C                   endif
106000070925     C*
106100070925     C                   else
106200070925     C                   if        vlrKSC = '0ORM0000'
106300070925     C                   eval      oriIDC = *zeros
106400070925     C                   else
106500070925     C                   eval      oriIDC = vlrKSC
106600070925     C                   endif
106700070925     C                   endif
106800070925     C                   endif
106900011122     C*
107000011105     C                   eval      oriPRG = §numori
107100011105     C                   eval      oriDER = datcor
107200011105     C                   eval      oriOER = wn6
107300011105     C                   eval      oriPOE = vaoPOE
107400011105     C                   eval      oriNSR = vaoNSR
107500011105     C                   eval      oriNOR = vaoNOR
107600011105     C                   eval      oriNRV = vaoNRV
107700011105     C                   eval      oriFLO = *blanks
107800011105     C*
107900011122     C* Infine valorizzo la chiave esterna sul file FNVAO
108000011105     C                   eval      dorm01 = vaoFLO
108100011105     C                   movel     §numori       §ormpg
108200011105     C                   eval      vaoFLO = dorm01
108300011105     C*
108400060306     C* ...quindi scrivo il file TIORI00F
108500121205     C  N55              WRITE     TIORI000
108600011105     C*
108700011105     C                   ENDSR
108800050826
108900050826
109000050826
109100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
109200050826     C     wriVGD        BEGSR
109300050826     C*
109400050826     C* Reperisco la descrizione della fase da tabella
109500050826     C                   movel(p)  'FAR'         KteCOD
109600050826     C                   movel(p)  '000'         KteKE1
109700050826     C     KEYtbe        chain     tntbe01l
109800050826     C                   if        %found(tntbe01l)
109900050826     C                   movel(P)  tbeuni        DFAR
110000050826     C                   else
110100050826     C                   clear                   DFAR
110200050826     C                   endif
110300050826     C*
110400050826     C* Valorizzo buffer record
110500050826     C                   clear                   FNVAPDS
110600050826     C                   eval      vapIDC = oriIDC
110700050826     C                   eval      vapPOE = oriPOE
110800050826     C                   eval      vapRFA = vaoRFA
110900050826     C                   eval      vapPOG = oriPOE
111000050826     C                   eval      vapDAE = oriDER
111100050826     C                   eval      vapORE = oriOER
111200050826     C                   if        d§fardva <> *blanks
111300050826     C                   eval      vapDFA = d§fardva
111400050826     C                   else
111500050826     C                   eval      vapDFA = d§fardes
111600050826     C                   endif
111700050826     C*
111800050826     C                   clear                   tivgd000
111900080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
112000050826     C                   eval      vgdTIP = 'VP'
112100060306     C                   eval      vgdKSU = vapIDC
112200050826     C                   eval      vgdTSC = 'WW'
112300050826     C                   eval      vgdDAT = datcor
112400151106     C                   eval      vgdPGM = 'TIS7T7IR'
112500060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
112600060306     C                   exsr      sr_Control
112700060306     C                   if        Cli_Abil = *on
112800060306     C                   eval      vgdKSU = woriIDC
112900121205     C  N55              write     tivgd000
113000060306     C                   endif
113100050826     C*
113200050826     C                   ENDSR
113300050826     C*------------------------------------------------------------------------*
113400011119
113500010528
113600010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
113700010528     C     repfil        BEGSR
113800010528     C*
113900010528     C                   if        invfil = *zeros and
114000010528     C                             depfil > *zeros and
114100010613     C                             (vinflg = *blanks or
114200010613     C                              vinflg = *zeros)
114300010528     C
114400010528     C                   eval      invfil = depfil
114500010528     C                   endif
114600010528     C*
114700010529     C                   if        depfil <> invfil and
114800010529     C                             invfil > *zeros
114900010528     C                   eval      flgMulti = '1'
115000010528     C                   if        vinflg = *blanks
115100010528     C                   add       1             cntNonEl
115200010528     C                   endif
115300010528     C                   endif
115400010528     C*
115500010528     C                   if        vinflg = '2'
115600010528     C                   eval      flgStato = '2'
115700010528     C                   endif
115800010528     C*
115900010528     C                   ENDSR
116000010528     C***
116100010528
116200010528
116300010528
116400010528
116500990920      /TITLE Invio dei dati al punto operativo.
116600000613     C     invio         BEGSR
116700990920     C*
116800990920     C                   reset                   dscmz
116900010528     C                   move      invfil        cmzdst
117000990920     C                   eval      cmzfld = vlrfou
117100990920     C                   eval      cmzmbd = vlrhdl
117200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
117300000710     C                   if        prmfir = *blanks
117400010122     C                   eval      cmzfla = 'FNVAO00F'
117500010122     C                   eval      cmzmba = 'FNVAO00F'
117600000710     C                   else
117700000710     C                   eval      cmzfla = prmfir
117800000710     C                   eval      cmzmba = prmfir
117900000710     C                   endif
118000990920     C                   eval      cmznrr = *zeros
118100990920     C                   move      §ctrok        cmznrr
118200010123     C                   eval      cmzlba = vlrfl1
118300990920     C                   call(e)   'TIS711C'
118400990920     C                   parm                    dscmz
118500990921     C                   parm      *blanks       esito
118600990920     C                   if        %error
118700990920     C                             or cmzerr = '1'
118800990921     C                             or esito  = '1'
118900000710     C                   eval      wrkesito = '3'
119000990920     C                   endif
119100990920     C*
119200000613     C                   ENDSR
119300000613     C***
119400990910
119500010528
119600010528
119700010528
119800010528      /TITLE Invio dei dati al punto operativo.
119900010528     C     opeini        BEGSR
120000010528     C*
120100010528     C* Inizializzo flag e contatori operativi
120200010528     C                   movel     '0'           flgGiro           1
120300010528     C                   movel     '0'           flgMulti          1
120400010528     C                   movel     '1'           flgStato          1
120500010615     C                   movel     '0'           flgOk             1
120600010528     C                   z-add     *zeros        cntNonEl         10 0
120700010528     C                   z-add     *zeros        depfil            3 0
120800010528     C                   z-add     *zeros        invfil            3 0
120900030407     C*
121000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
121100030407     C                   if        %subst(vlrppt:1:1) = 'S'
121200030407     C                   seton                                        50
121300030407     C                   else
121400030407     C                   setoff                                       50
121500030407     C                   endif
121600070925     C*
121700070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
121800070925     C                   if        %subst(vlrppt:2:1) = 'S'
121900070925     C                   seton                                        51
122000070925     C                   else
122100070925     C                   setoff                                       51
122200070925     C                   endif
122300150319     C*
122400150319     C* se il 4° char dei parametri è T=Test
122500150319     C* vince questo test su quello precedente per impostare VAOPOE
122600150319     C                   SETOFF                                       55
122700150319     C                   IF        %subst(vlrppt:4:1) = 'T'
122800150319     C* inibisco la scrittura dei file TIORI e TIVGD
122900150319     C                   SETON                                        55
123000150319     C* imposto il PO di emissione come filiale fittizia
123100150319     C                   EVAL      VAOPOE = 999
123200150319     C                   ENDIF
123300010528     C*
123400010528     C                   ENDSR
123500010528     C***
123600060306
123700060306
123800060306     C*-------------------------------------------------------------------------*
123900060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
124000060306     C*-------------------------------------------------------------------------*
124100060306     C     sr_Control    begsr
124200060306     C*
124300060306     C* inizializzo a ok il flag d controllo abilitazione
124400060306     C                   eval      Cli_Abil = *on
124500060306     C
124600060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
124700060306     C                   clear                   woriIDC
124800060306     C                   eval      kteCOD = 'VAP'
124900060306     C                   eval      kteKE1 = oriIDC
125000060306     C     keytbe        chain     TNTBE01L
125100060306     C                   if        %found(Tntbe01l)
125200060306     C                   eval      woriIDC = TbeKe2
125300060306     C                   else
125400060306     C                   eval      Cli_Abil = *off
125500060306     C                   endif
125600060306     C*
125700060306     C                   endsr
125800010528
125900010528
126000010528
126100010528
126200000613     C     *inzsr        BEGSR
126300990910     C*
126400990910     C     *entry        plist
126500990920     C                   parm                    tivlrds
126600990921     C                   parm      wrkesito      esito
126700000724     C                   parm                    prmlit
126800000710     C                   parm                    prmfir
126900011105     C*
127000011105     C* Campi riferito al database
127100011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
127200011105     C     *like         define    tbeke1        kteKE1
127300011105     C*
127400011105     C* Definizione chiavi di lettura
127500011105     C* TNTBE01L - parziale
127600011105     C     keytbe        klist
127700011105     C                   kfld                    kteCOD                         *UTENTE
127800011105     C                   kfld                    kteKE1                         *CODICE
127900070926     C* TIVSS02L - parziale
128000070926     C     keyvss02_p    klist
128100070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
128200070926     C                   kfld                    vssISV                         *SERVIZIO
128300011105     C*
128400011105     C* Reperimento data e ora corrente
128500011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
128600011105     C                   movel     wn14          wn6               6 0          *ORA (6)
128700151106     C                   z-add     *zeros        datcor            8 0
128800151106     C                   eval      datcor = %dec(%date() : *ISO)
128900010529     C*
129000000613     C                   ENDSR
129100000613     C***
