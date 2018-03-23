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
003600010122     D fnvaoaar      e ds                  inz
003700011105     D                                     prefix(a_)
003800140416     D fnvaoear      e ds                  inz
003900140416     D                                     prefix(w_)
004000031014     D tisi95ds      e ds
004100030624     D dorm01        e ds
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
005400011105     D*-------------------
005500011105     D* DS "XSRDA8" - controllo data (8)
005600011105     D*-------------------
005700011105     D wlbda8          DS                  INZ
005800011105     D  g08dat                 1      8  0
005900011105     D  g08inv                 9     16  0
006000011105     D  g08err                17     17
006100011105     D  g08tgi                18     22  0
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
011300020917     C*
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
013200140416     C*
013300140416     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
013400140416     C                   call      'TIS7T7ER'
013500140416     C                   parm      'O'           pIn_Opz           1
013600140416     C                   parm                    tivlrds
013700140416     C                   parm                    FNVAOEAR
013800140416     C                   parm                    oriPRG
013900140528     C                   parm      *zeros        pIn_Dst           3
014000140416     C                   parm      *blanks       pOut_Esito        1
014100140416     C                   if        pOut_Esito = '2'
014200140416     C                   goto      FINE
014300140416     C                   endif
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
017000011105     C                   clear                   TIORI000
017100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017200010711     C                   if        vinflg = *blanks or vinflg = '0'
017300010711     C                   clear                   vinmsg
017400010711     C                   endif
017500010711     C*
017600010122     C                   eval      fnvaoaar = vindta
017700151019     C*
017800151019     C* Stacco subito un numeratore univoco "ORM"
017900151019     C                   exsr      RTVNUM
018000140416     C*
018100140416     C* Verifico se trattasi di formato ORM "esteso"
018200140416     C                   setoff                                       53
018300140416     C                   if        %subst(vindta:717+1:%size(fnvaoear)-717+1)
018400140416     C                             > *blanks
018500140416     C                   eval      fnvaoear = vindta
018600140416     C                   seton                                        53
018700140417     C                   endif
018800080521     C*
018900080521     C* Verifico se richiesto nei parametri anagrafica cliente/tipo-file d particolari
019000080521     C* impostazioni dei flag operativi
019100080521     C                   if        %subst(vlrppt:3:1) = 'S'
019200080521     C                   eval      dorm01 = a_vaoFLO
019300080521     C                   eval      §ormks = 'S'
019400080521     C                   eval      a_vaoFLO = dorm01
019500080521     C                   endif
019600991022     C*
019700010122     C                   testb     x'3F'         a_vaoatb                 02
019800991022     C                   if        not *in02
019900991022     C*
020000971021     C                   SETOFF                                       303132
020100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
020200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
020300971023     C*    OPPURE TUTTI BLANKS
020400010122     C*-------------------
020500010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
020600010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
020700010122     C*-------------------
020800010122     C*
020900011119 xxx C*                  TESTN                   a_vaopoe             30
021000010417  "  C* N30              TESTN                   a_vaopoe               30
021100010417  "  C* N30              TESTN                   a_vaopoe                 30
021200010417     C*                  MOVE      a_vaopoe      W001A             1
021300010417     C*    *IN30         IFEQ      *OFF
021400010417     C*    W001A         ORLT      '0'
021500010417     C*                  SETON                                        32
021600010417     C*                  MOVE      *ZEROS        VAOPOE
021700010417     C*                  EVAL      vinmsg = 'VAOPOE'
021800010417     C*                  ELSE
021900010417     C*                  MOVE      a_vaopoe      VAOPOE
022000010417     C*                  ENDIF
022100010417  "  C*
022200010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
022300010604 xxx C                   IF        vlrpoi <> 999
022400010528     C                   EVAL      VAOPOE = vlrpoi
022500150331     C                   ELSE
022600150331     C*
022700150331     C                   IF        VAOPOE <> 999
022800150331     C*
022900150331     C* se i char dal 5° al 7° dei parametri è <> blank ed è numerico, allora è VAOPOE
023000150331     C                   IF        %subst(vlrppt:5:3) <> *blank
023100150331     C                   EVAL      PiStr=%trim(%subst(vlrppt:5:3))
023200150331     C                   EXSR      CHKNUM
023300150331     C                   IF        PiInt=*on
023400150331     C                   Z-ADD     PiVal         VAOPOE
023500150331     C                   ENDIF
023600150331     C                   ELSE
023700150331     C*
023800010528     C                   IF        a_vaopoe = *blanks or
023900011010     C                             a_vaopoe = *zeros  or
024000011010     C                             a_vaopoe = ' 000'
024100010528     C                   IF        a_vaopor <> *blanks and
024200011010     C                             a_vaopor <> *zeros  and
024300011010     C                             a_vaopor <> ' 000'
024400010528     C                   MOVE      a_vaopor      VAOPOE
024500010528     C                   ELSE
024600011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
024700010528     C                   MOVE      a_vaopoe      VAOPOE
024800010528     C                   ENDIF
024900010529     C                   ELSE
025000010529     C                   MOVE      a_vaopoe      VAOPOE
025100010528     C                   ENDIF
025200010528     C                   ENDIF
025300150331     C                   ENDIF
025400150331     C                   ENDIF
025500010122     C*
025600010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025700010528     C                   eval      depfil = VAOPOE
025800010528     C                   exsr      repfil
025900010528     C                   if        depfil = invfil and
026000010528     C                             (vinflg = *blanks
026100010528     C                              or vinflg = '0'
026200010528     C                              or vinflg = '2')
026300010528     C*
026400010528     C                   exsr      PREELA
026500010710     C*
026600010719     C*                  TESTN                   a_vaonsr             30
026700010719     C* N30              TESTN                   a_vaonsr               30
026800010719     C* N30              TESTN                   a_vaonsr                 30
026900010719     C*                  MOVE      a_vaonsr      W001A             1
027000010719     C*    *IN30         IFEQ      *OFF
027100010719     C*    W001A         ORLT      '0'
027200010719     C*                  SETON                                        32
027300010719     C*                  MOVE      *ZEROS        VAONSR
027400010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
027500010719     C*                  ELSE
027600010719     C*                  MOVE      a_vaonsr      VAONSR
027700010719     C*                  ENDIF
027800010122     C*
027900010719     C*                  TESTN                   a_vaonor             30
028000010719     C* N30              TESTN                   a_vaonor               30
028100010719     C* N30              TESTN                   a_vaonor                 30
028200010719     C*                  MOVE      a_vaonor      W001A             1
028300010719     C*    *IN30         IFEQ      *OFF
028400010719     C*    W001A         ORLT      '0'
028500010719     C*                  SETON                                        32
028600010719     C*                  MOVE      *ZEROS        VAONOR
028700010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
028800010719     C*                  ELSE
028900010719     C*                  MOVE      a_vaonor      VAONOR
029000010719     C*                  ENDIF
029100010122     C*
029200010719     C*                  TESTN                   a_vaonrv             30
029300010719     C* N30              TESTN                   a_vaonrv               30
029400010719     C* N30              TESTN                   a_vaonrv                 30
029500010719     C*                  MOVE      a_vaonrv      W001A             1
029600010719     C*    *IN30         IFEQ      *OFF
029700010719     C*    W001A         ORLT      '0'
029800010719     C*                  SETON                                        32
029900010719     C*                  MOVE      *ZEROS        VAONRV
030000010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
030100010719     C*                  ELSE
030200010719     C*                  MOVE      a_vaonrv      VAONRV
030300010719     C*                  ENDIF
030400010122     C*
030500010122     C                   TESTN                   a_vaodao             30
030600010122     C  N30              TESTN                   a_vaodao               30
030700010122     C  N30              TESTN                   a_vaodao                 30
030800010122     C                   MOVE      a_vaodao      W001A             1
030900010122     C     *IN30         IFEQ      *OFF
031000010122     C     W001A         ORLT      '0'
031100010122     C                   SETON                                        32
031200050421     C                   MOVE      *ZEROS        VAODAO
031300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
031400010122     C                   ELSE
031500050421     C                   MOVE      a_vaodao      VAODAO
031600010122     C                   ENDIF
031700010122     C*
031800010122     C                   TESTN                   a_vaooao             30
031900010122     C  N30              TESTN                   a_vaooao               30
032000010122     C  N30              TESTN                   a_vaooao                 30
032100010122     C                   MOVE      a_vaooao      W001A             1
032200010122     C     *IN30         IFEQ      *OFF
032300010122     C     W001A         ORLT      '0'
032400010122     C                   SETON                                        32
032500050421     C                   MOVE      *ZEROS        VAOOAO
032600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
032700010122     C                   ELSE
032800050421     C                   MOVE      a_vaooao      VAOOAO
032900010122     C                   ENDIF
033000010122     C*
033100010122     C                   TESTN                   a_vaocor             30
033200010122     C  N30              TESTN                   a_vaocor               30
033300010122     C  N30              TESTN                   a_vaocor                 30
033400010122     C                   MOVE      a_vaocor      W001A             1
033500010122     C     *IN30         IFEQ      *OFF
033600010122     C     W001A         ORLT      '0'
033700010122     C                   SETON                                        32
033800010122     C                   MOVE      *ZEROS        VAOCOR
033900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
034000010122     C                   ELSE
034100050421     C                   MOVE      a_vaocor      VAOCOR
034200010122     C                   ENDIF
034300010122     C*
034400010122     C                   TESTN                   a_vaocra             30
034500010122     C  N30              TESTN                   a_vaocra               30
034600010122     C  N30              TESTN                   a_vaocra                 30
034700010122     C                   MOVE      a_vaocra      W001A             1
034800010122     C     *IN30         IFEQ      *OFF
034900010122     C     W001A         ORLT      '0'
035000010122     C                   SETON                                        32
035100010122     C                   MOVE      *ZEROS        VAOCRA
035200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
035300010122     C                   ELSE
035400010122     C                   MOVE      a_vaocra      VAOCRA
035500010122     C                   ENDIF
035600010122     C*
035700010719     C                   IF        a_vaocrc = *blanks or
035800010719     C                             a_vaocrc = *zeros
035900010719     C                   ELSE
036000010719     C                   TESTN                   a_vaocrc             30
036100010719     C  N30              TESTN                   a_vaocrc               30
036200010719     C  N30              TESTN                   a_vaocrc                 30
036300010719     C                   MOVE      a_vaocrc      W001A             1
036400010719     C     *IN30         IFEQ      *OFF
036500010719     C     W001A         ORLT      '0'
036600010719     C                   SETON                                        32
036700010719     C                   MOVE      *ZEROS        VAOCRC
036800010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
036900010719     C                   ELSE
037000010719     C                   MOVE      a_vaocrc      VAOCRC
037100010719     C                   ENDIF
037200010719     C                   ENDIF
037300010122     C*
037400010122     C                   TESTN                   a_vaodar             30
037500010122     C  N30              TESTN                   a_vaodar               30
037600010122     C  N30              TESTN                   a_vaodar                 30
037700010122     C                   MOVE      a_vaodar      W001A             1
037800010122     C     *IN30         IFEQ      *OFF
037900010122     C     W001A         ORLT      '0'
038000010122     C                   SETON                                        32
038100010122     C                   MOVE      *ZEROS        VAODAR
038200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
038300010122     C                   ELSE
038400010122     C                   MOVE      a_vaodar      VAODAR
038500010122     C                   ENDIF
038600010122     C*
038700010122     C                   TESTN                   a_vaoorr             30
038800010122     C  N30              TESTN                   a_vaoorr               30
038900010122     C  N30              TESTN                   a_vaoorr                 30
039000010122     C                   MOVE      a_vaoorr      W001A             1
039100010122     C     *IN30         IFEQ      *OFF
039200010122     C     W001A         ORLT      '0'
039300010122     C                   SETON                                        32
039400010122     C                   MOVE      *ZEROS        VAOORR
039500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
039600010122     C                   ELSE
039700010122     C                   MOVE      a_vaoorr      VAOORR
039800010122     C                   ENDIF
039900010122     C*
040000010122     C                   TESTN                   a_vaoncl             30
040100010122     C  N30              TESTN                   a_vaoncl               30
040200010122     C  N30              TESTN                   a_vaoncl                 30
040300010122     C                   MOVE      a_vaoncl      W001A             1
040400010122     C     *IN30         IFEQ      *OFF
040500010122     C     W001A         ORLT      '0'
040600010122     C                   SETON                                        32
040700010122     C                   MOVE      *ZEROS        VAONCL
040800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
040900010122     C                   ELSE
041000010122     C                   MOVE      a_vaoncl      VAONCL
041100010122     C                   ENDIF
041200010122     C*
041300010122     C                   MOVE      *ZERO         VAOPKG
041400010122     C                   MOVE      a_vaopkg      WASC09
041500010122     C                   TESTN                   WI09                 30
041600010122     C  N30              TESTN                   WI09                   30
041700010122     C  N30              TESTN                   WI09                     30
041800010122     C                   MOVE      WI09          W001A             1
041900010122     C     *IN30         IFEQ      *OFF
042000010122     C     W001A         ORLT      '0'
042100010122     C     W001A         ANDNE     *BLANK
042200010122     C                   SETON                                        32
042300010122     C                   MOVE      *ZERO         VAOPKG
042400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
042500010122     C                   ELSE
042600010122     C                   MOVEL     WI09          VAOPKG
042700010122     C                   TESTN                   WD09                 30
042800010122     C  N30              TESTN                   WD09                     30
042900010122     C                   MOVE      WD09          W001A             1
043000010122     C     *IN30         IFEQ      *OFF
043100010122     C     W001A         ORLT      '0'
043200010122     C     W001A         ANDNE     *BLANK
043300010122     C                   SETON                                        32
043400010122     C                   MOVE      *ZERO         VAOPKG
043500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
043600010122     C                   ELSE
043700010122     C                   MOVE      WD09          VAOPKG
043800010122     C                   ENDIF
043900050118     C                   ENDIF
044000010122     C*
044100010122     C                   MOVE      *ZERO         VAOVLM
044200010122     C                   MOVE      a_vaovlm      WASC07
044300010122     C                   TESTN                   WI07                 30
044400010122     C  N30              TESTN                   WI07                   30
044500010122     C  N30              TESTN                   WI07                     30
044600010122     C                   MOVE      WI07          W001A             1
044700010122     C     *IN30         IFEQ      *OFF
044800010122     C     W001A         ORLT      '0'
044900010122     C     W001A         ANDNE     *BLANK
045000010122     C                   SETON                                        32
045100010122     C                   MOVE      *ZERO         VAOVLM
045200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
045300010122     C                   ELSE
045400010122     C                   MOVEL     WI07          VAOVLM
045500010122     C                   TESTN                   WD07                 30
045600010122     C  N30              TESTN                   WD07                   30
045700010122     C  N30              TESTN                   WD07                     30
045800010122     C                   MOVE      WD07          W001A             1
045900010122     C     *IN30         IFEQ      *OFF
046000010122     C     W001A         ORLT      '0'
046100010122     C     W001A         ANDNE     *BLANK
046200010122     C                   SETON                                        32
046300010122     C                   MOVE      *ZERO         VAOVLM
046400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
046500010122     C                   ELSE
046600010122     C                   MOVE      WD07          VAOVLM
046700010122     C                   ENDIF
046800050118     C                   ENDIF
046900010122     C*
047000010122     C                   TESTN                   a_vaobnc             30
047100010122     C  N30              TESTN                   a_vaobnc               30
047200010122     C  N30              TESTN                   a_vaobnc                 30
047300010122     C                   MOVE      a_vaobnc      W001A             1
047400010122     C     *IN30         IFEQ      *OFF
047500010122     C     W001A         ORLT      '0'
047600010122     C                   SETON                                        32
047700010122     C                   MOVE      *ZEROS        VAOBNC
047800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
047900010122     C                   ELSE
048000010122     C                   MOVE      a_vaobnc      VAOBNC
048100010122     C                   ENDIF
048200010122     C*
048300010122     C                   TESTN                   a_vaoblc             30
048400010122     C  N30              TESTN                   a_vaoblc               30
048500010122     C  N30              TESTN                   a_vaoblc                 30
048600010122     C                   MOVE      a_vaoblc      W001A             1
048700010122     C     *IN30         IFEQ      *OFF
048800010122     C     W001A         ORLT      '0'
048900010122     C                   SETON                                        32
049000010122     C                   MOVE      *ZEROS        VAOBLC
049100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
049200010122     C                   ELSE
049300010122     C                   MOVE      a_vaoblc      VAOBLC
049400010122     C                   ENDIF
049500010122     C*
049600010122     C                   TESTN                   a_vaoatt             30
049700010122     C  N30              TESTN                   a_vaoatt               30
049800010122     C  N30              TESTN                   a_vaoatt                 30
049900010122     C                   MOVE      a_vaoatt      W001A             1
050000010122     C     *IN30         IFEQ      *OFF
050100010122     C     W001A         ORLT      '0'
050200010122     C                   SETON                                        32
050300010122     C                   MOVE      *ZEROS        VAOATT
050400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
050500010122     C                   ELSE
050600010122     C                   MOVE      a_vaoatt      VAOATT
050700010122     C                   ENDIF
050800010122     C*
050900010122     C                   TESTN                   a_vaomtc             30
051000010122     C  N30              TESTN                   a_vaomtc               30
051100010122     C  N30              TESTN                   a_vaomtc                 30
051200010122     C                   MOVE      a_vaomtc      W001A             1
051300010122     C     *IN30         IFEQ      *OFF
051400010122     C     W001A         ORLT      '0'
051500010122     C                   SETON                                        32
051600010122     C                   MOVE      *ZEROS        VAOMTC
051700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
051800010122     C                   ELSE
051900010122     C                   MOVE      a_vaomtc      VAOMTC
052000010122     C                   ENDIF
052100010122     C*
052200010719     C                   IF        a_vaoksc = *blanks or
052300010719     C                             a_vaoksc = *zeros
052400010719     C                   ELSE
052500010719     C                   TESTN                   a_vaoksc             30
052600010719     C  N30              TESTN                   a_vaoksc               30
052700010719     C  N30              TESTN                   a_vaoksc                 30
052800010719     C                   MOVE      a_vaoksc      W001A             1
052900010719     C     *IN30         IFEQ      *OFF
053000010719     C     W001A         ORLT      '0'
053100010719     C                   SETON                                        32
053200010719     C                   MOVE      *ZEROS        VAOKSC
053300010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
053400010719     C                   ELSE
053500010719     C                   MOVE      a_vaoksc      VAOKSC
053600010719     C                   ENDIF
053700010719     C                   ENDIF
053800010122     C*
053900010710     C                   IF        a_vaoctr > *blanks
054000010122     C                   TESTN                   a_vaoctr             30
054100010122     C  N30              TESTN                   a_vaoctr               30
054200010122     C  N30              TESTN                   a_vaoctr                 30
054300010122     C                   MOVE      a_vaoctr      W001A             1
054400010122     C     *IN30         IFEQ      *OFF
054500010122     C     W001A         ORLT      '0'
054600010122     C                   SETON                                        32
054700010627     C                   MOVE      *blanks       VAOCTR
054800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
054900010122     C                   ELSE
055000010122     C                   MOVE      a_vaoctr      VAOCTR
055100010122     C                   ENDIF
055200010710     C                   ENDIF
055300010122     C*
055400010122     C                   TESTN                   a_vaopor             30
055500010122     C  N30              TESTN                   a_vaopor               30
055600010122     C  N30              TESTN                   a_vaopor                 30
055700010122     C                   MOVE      a_vaopor      W001A             1
055800010122     C     *IN30         IFEQ      *OFF
055900010122     C     W001A         ORLT      '0'
056000010122     C                   SETON                                        32
056100010122     C                   MOVE      *ZEROS        VAOPOR
056200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
056300010122     C                   ELSE
056400010122     C                   MOVE      a_vaopor      VAOPOR
056500010122     C                   ENDIF
056600010122     C*
056700010719     C                   IF        a_vaopoc = *blanks or
056800010719     C                             a_vaopoc = *zeros
056900010719     C                   ELSE
057000010122     C                   TESTN                   a_vaopoc             30
057100010122     C  N30              TESTN                   a_vaopoc               30
057200010122     C  N30              TESTN                   a_vaopoc                 30
057300010122     C                   MOVE      a_vaopoc      W001A             1
057400010122     C     *IN30         IFEQ      *OFF
057500010122     C     W001A         ORLT      '0'
057600010122     C                   SETON                                        32
057700010122     C                   MOVE      *ZEROS        VAOPOC
057800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
057900010122     C                   ELSE
058000010122     C                   MOVE      a_vaopoc      VAOPOC
058100010122     C                   ENDIF
058200010719     C                   ENDIF
058300010122     C*
058400010719     C                   IF        a_vaosto = *blanks or
058500010719     C                             a_vaosto = *zeros
058600010719     C                   ELSE
058700010122     C                   TESTN                   a_vaosto             30
058800010122     C  N30              TESTN                   a_vaosto               30
058900010122     C  N30              TESTN                   a_vaosto                 30
059000010122     C                   MOVE      a_vaosto      W001A             1
059100010122     C     *IN30         IFEQ      *OFF
059200010122     C     W001A         ORLT      '0'
059300010122     C                   SETON                                        32
059400010122     C                   MOVE      *ZEROS        VAOSTO
059500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
059600010122     C                   ELSE
059700010122     C                   MOVE      a_vaosto      VAOSTO
059800010122     C                   ENDIF
059900010719     C                   ENDIF
060000971021     C*
060100971021     C* CAMPI ALFABETICI:
060200010122     C                   MOVE      a_vaoatb      VAOATB
060300010122     C                   MOVE      a_vaotor      VAOTOR
060400010531     C*
060500010531     C                   IF        a_vaotco = *blanks
060600010531     C                   MOVE      'F'           VAOTCO
060700010531     C                   ELSE
060800010531     C                   MOVE      a_vaotco      VAOTCO
060900010531     C                   ENDIF
061000010531     C*
061100010122     C                   MOVE      a_vaorso      VAORSO
061200010122     C                   MOVE      a_vaoino      VAOINO
061300010122     C                   MOVE      a_vaocao      VAOCAO
061400010122     C                   MOVE      a_vaoloo      VAOLOO
061500031014     C                   MOVE      a_vaopro      VAOPRO
061600090129     C                   MOVE      a_vaonao      VAONAO
061700131007     C*
061800131007     C                   MOVE      a_vaorsr      VAORSR
061900131007     C                   MOVE      a_vaoinr      VAOINR
062000131007     C                   MOVE      a_vaocar      VAOCAR
062100131007     C                   MOVE      a_vaolor      VAOLOR
062200131007     C                   MOVE      a_vaoprr      VAOPRR
062300131007     C                   MOVE      a_vaonar      VAONAR
062400131007     C*
062500131007     C                   MOVE      a_vaorer      VAORER
062600131007     C                   MOVE      a_vaoter      VAOTER
062700131007     C                   MOVE      a_vaorsc      VAORSC
062800131007     C                   MOVE      a_vaoinc      VAOINC
062900131007     C                   MOVE      a_vaocac      VAOCAC
063000131007     C                   MOVE      a_vaoloc      VAOLOC
063100131007     C                   MOVE      a_vaoprc      VAOPRC
063200131007     C                   MOVE      a_vaonac      VAONAC
063300131007     C*
063400131007     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
063500131007     C*** VAONAO
063600131007     C                   IF        %trim(VAONAO) = 'I'  OR
063700131007     C                             %trim(VAONAO) = 'IT' OR
063800131007     C                             %trim(VAONAO) = 'VA' OR
063900131007     C                             %trim(VAONAO) = 'SM'
064000131007     C                   EVAL      VAONAO = *blanks
064100131007     C                   ENDIF
064200131007     C*** VAONAR
064300131007     C                   IF        %trim(VAONAR) = 'I'  OR
064400131007     C                             %trim(VAONAR) = 'IT' OR
064500131007     C                             %trim(VAONAR) = 'VA' OR
064600131007     C                             %trim(VAONAR) = 'SM'
064700131007     C
064800131007     C                   EVAL      VAONAR = *blanks
064900131007     C                   ENDIF
065000131007     C*** VAONAC
065100131007     C                   IF        %trim(VAONAC) = 'I'  OR
065200131007     C                             %trim(VAONAC) = 'IT' OR
065300131007     C                             %trim(VAONAC) = 'VA' OR
065400131007     C                             %trim(VAONAC) = 'SM'
065500131007     C                   EVAL      VAONAC = *blanks
065600131007     C                   ENDIF
065700131007     C*
065800031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
065900031014     C                   IF        VAOPRO  = *blanks AND
066000050907     C                             VAOCAO <> *blanks AND
066100050907     C                             VAONAO  = *blanks                            * solo x Italia
066200031014     C                   CLEAR                   TISI95DS
066300031014     C                   EVAL      I95TCN = '3'
066400031014     C                   Z-ADD     datcor        I95DAT
066500031014     C                   EVAL      I95CAP = VAOCAO
066600031014     C                   EVAL      I95LOC = VAOLOO
066700031014     C                   CALL      'TISI95R'
066800031014     C                   PARM                    TISI95DS
066900031014     C                   EVAL      VAOPRO = O95PRV
067000031014     C                   ENDIF
067100131007     C*
067200031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
067300031014     C                   IF        VAOPRR  = *blanks AND
067400050907     C                             VAOCAR <> *blanks AND
067500050907     C                             VAONAR  = *blanks                            * solo x Italia
067600031014     C                   CLEAR                   TISI95DS
067700031014     C                   EVAL      I95TCN = '3'
067800031014     C                   Z-ADD     datcor        I95DAT
067900031014     C                   EVAL      I95CAP = VAOCAR
068000031014     C                   EVAL      I95LOC = VAOLOR
068100031014     C                   CALL      'TISI95R'
068200031014     C                   PARM                    TISI95DS
068300031014     C                   EVAL      VAOPRR = O95PRV
068400031014     C                   ENDIF
068500131007     C*
068600031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
068700031014     C                   IF        VAOPRC  = *blanks AND
068800050907     C                             VAOCAC <> *blanks AND
068900050907     C                             VAONAC  = *blanks                            * solo x Italia
069000031014     C                   CLEAR                   TISI95DS
069100031014     C                   EVAL      I95TCN = '3'
069200031014     C                   Z-ADD     datcor        I95DAT
069300031014     C                   EVAL      I95CAP = VAOCAC
069400031014     C                   EVAL      I95LOC = VAOLOC
069500031014     C                   CALL      'TISI95R'
069600031014     C                   PARM                    TISI95DS
069700031014     C                   EVAL      VAOPRC = O95PRV
069800031014     C                   ENDIF
069900031014     C*
070000010122     C                   MOVE      a_vaoffd      VAOFFD
070100010122     C                   MOVE      a_vaormp      VAORMP
070200010122     C                   MOVE      a_vaonam      VAONAM
070300010122     C                   MOVE      a_vaopag      VAOPAG
070400010122     C                   MOVE      a_vaozor      VAOZOR
070500010122     C                   MOVE      a_vaono1      VAONO1
070600010122     C                   MOVE      a_vaono2      VAONO2
070700010122     C                   MOVE      a_vaoddt      VAODDT
070800010122     C                   MOVE      a_vaorfa      VAORFA
070900010122     C                   MOVE      a_vaospi      VAOSPI
071000010122     C                   MOVE      a_vaoflo      VAOFLO
071100050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
071200050304     C                   IF        VAOPKG =  *zeros AND
071300050304     C                             VAOVLM =  *zeros AND
071400050304     C                             VAOBNC =  *zeros AND
071500050304     C                             VAONCL <> *zeros
071600050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
071700050304     C                   ENDIF
071800030603     C*
071900030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
072000030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
072100030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
072200050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
072300150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
072400930409     C*
072500010604     C  N31              ADD       1             §CTROK            7 0
072600010604     C   32              ADD       1             §CTRMO            7 0
072700010604     C   31              ADD       1             §CTRNO            7 0
072800030624     C  N31              EXSR      CHKCOR
072900011105     C  N31              EXSR      WRIORI
073000050826     C  N31              EXSR      WRIVGD
073100050421     C*
073200050421     C***
073300050421     C* FORZATURA SU DATA/ORA => NN CONSIDERO QUELLE PASSATE SUL FILE DAL CLIENTE MA SEMPRE
073400050421     C* DATA/ORA DI TRADUZIONE (OVVERO VERA IMMISSIONE DEL DATO SEPPUR DA CONFERMARE SUL
073500050421     C* NS. GESTIONALE). QUESTO SOLO X LA SCRITTURA DEL VAO!!!
073600050421     C                   eval      VAODAO = datcor
073700050421     C                   eval      VAOOAO = wn6
073800050421     C***
073900050421     C  N31              WRITE     FNVAO000
074000140416     C*
074100151019     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
074200140416     C                   if        *in53 and not *in31
074300151019     C                   exsr      EXEFORZ
074400140416     C                   call      'TIS7T7ER'
074500140416     C                   parm      'W'           pIn_Opz           1
074600140416     C                   parm                    tivlrds
074700140416     C                   parm                    FNVAOEAR
074800140416     C                   parm                    oriPRG
074900140528     C                   parm      *zeros        pIn_Dst           3
075000140416     C                   parm      *blanks       pOut_Esito        1
075100140416     C                   endif
075200140416     C                   if        pOut_Esito = '2'
075300140416     C                   seton                                        32
075400140416     C                   endif
075500990910     C*
075600010604     C                   if        *in31 = *off and
075700010604     C                             *in32 = *off
075800990910     C                   eval      vinflg = '1'
075900990910     C                   else
076000990910     C                   eval      vinflg = '2'
076100990910     C                   endif
076200991022     C                   endif
076300990910     C                   endif
076400010604     C*
076500010604     C                   endif
076600000905     C*
076700000905     C                   else
076800000905     C                   eval      vinflg = '1'
076900000905     C                   endif
077000000905     C*
077100000905     C  N70              update    tivin000
077200991022     C*
077300991022     C  N70              ENDdo
077400140416     C*
077500140416     C     FINE          tag
077600140528     C*
077700140528     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
077800140528     C                   call      'TIS7T7ER'
077900140528     C                   parm      'C'           pIn_Opz           1
078000140528     C                   parm                    tivlrds
078100140528     C                   parm                    FNVAOEAR
078200140528     C                   parm                    oriPRG
078300140528     C                   parm      *zeros        pIn_Dst           3
078400140528     C                   parm      *blanks       pOut_Esito        1
078500010528     C*
078600010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
078700010528     C                   if        cntNonEl = *zeros or
078800010528     C                             flgMulti = '0'
078900010528     C* Se non ci sono record con errori ...
079000010528     C                   if        §ctrno = 0 and
079100010604     C                             §ctrmo = 0 and
079200010528     C                             flgStato <> '2'
079300990910     C* ... restituisco esito OK.
079400990921     C                   eval      wrkesito = '0'
079500990910     C                   else
079600000710     C                   if        §ctrok > 0
079700990921     C                   eval      wrkesito = '1'
079800000710     C                   else
079900010615     C                   if        flgOk = '0'
080000010615     C                   eval      wrkesito = '2'
080100010615     C                   else
080200010615     C                   eval      wrkesito = '6'
080300010615     C                   endif
080400990910     C                   endif
080500010528     C                   endif
080600010529     C                   else
080700010529     C                   eval      wrkesito = '9'
080800000710     C                   endif
080900990910     C*
081000990914     C                   if        %open(tivin00r)
081100990908     C                   close     tivin00r
081200990914     C                   endif
081300010122     C                   if        %open(fnvaowwr)
081400010122     C                   close     fnvaowwr
081500990914     C                   endif
081600990910     C*
081700010528     C                   if        vlrpoi <> 999
081800010528     C                   eval      invfil = vlrpoi
081900010528     C                   endif
082000010528     C*
082100990920     C                   if        §ctrok > 0
082200010528     C                             and invfil > *zeros
082300140528     C                   exsr      invio
082400140528     C*
082500140528     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
082600140528     C                   move      invfil        cmzdst
082700140528     C                   call      'TIS7T7ER'
082800140528     C                   parm      'S'           pIn_Opz           1
082900140528     C                   parm                    tivlrds
083000140528     C                   parm                    FNVAOEAR
083100140528     C                   parm                    oriPRG
083200140528     C                   parm      cmzdst        pIn_Dst           3
083300140528     C                   parm      *blanks       pOut_Esito        1
083400990920     C                   endif
083500010612     C*
083600010612     C                   if        flgGiro = '1'
083700010612     C                   exsr      endela
083800010612     C                   endif
083900010528     C*
084000910830     C                   ENDSR
084100000613     C***
084200140417
084300140417
084400140417     C*------------------------------------------------------------------------*
084500151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
084600140417     C*------------------------------------------------------------------------*
084700151019     C     RTVNUM        BEGSR
084800140417     C*
084900151019     C* Stacco un numeratore da AZNUM
085000151019     C                   clear                   §numori
085100140417     C                   clear                   trul33ds
085200140417     C                   eval      I33OPE = *zeros
085300140417     C                   eval      I33CNU = 600
085400140417     C                   eval      I33NUM = 1
085500140417     C                   movel     TRUL33DS      KPJBU
085600140417     C                   call      'TRUL33R'
085700140417     C                   parm                    KPJBA
085800140417     C                   movel     KPJBU         TRUL33DS
085900140417     C                   if        O33ERR = *zeros
086000140417     C                   z-add     O33NRF        §numori
086100140417     c                   else
086200140417     c                   eval      §numori = *all'9'
086300140417     C                   seton                                        31        * errore bloccante
086400140417     C                   endif
086500140417     C*
086600140417     C                   eval      vaoNOR = §numori
086700140417     C*
086800140417     C                   ENDSR
086900151019
087000151019
087100151019     C*------------------------------------------------------------------------*
087200151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
087300151019     C*------------------------------------------------------------------------*
087400151019     C     EXEFORZ       BEGSR
087500151019     C*
087600151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
087700151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
087800151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
087900151019     C*
088000151019     C                   ENDSR
088100140417
088200150319
088300150319     C*----------------------------------------------------*
088400150319     C*  CONTROLLO NUMERICITA' CAMPI
088500150319     C*----------------------------------------------------*
088600150319     C     CHKNUM        BEGSR
088700150319     C*
088800150319     C                   IF        PiDecChr = *blanks
088900150319     C                   EVAL      PiDecChr = '.'
089000150319     C                   ENDIF
089100150319     C*
089200150319     C                   CALL(e)   'ISNUMERIC'
089300150319     C                   PARM                    PiStr            30
089400150319     C                   PARM                    PiDecChr          1
089500150319     C                   PARM      *ZEROS        PiVal            30 9
089600150319     C                   PARM      '0'           PiInt             1
089700150319     C                   PARM      '0'           PiNum             1
089800150319     C                   IF        %error
089900150319     C                   EVAL      PiNum=*off
090000150319     C                   ENDIF
090100150319     C*
090200150319     C                   ENDSR
090300150319     C***
090400030624
090500030624
090600030624     C*------------------------------------------------------------------------*
090700030624     C* CHKCOR - Considerazioni su VAOCOR
090800030624     C*------------------------------------------------------------------------*
090900030624     C     CHKCOR        BEGSR
091000030624     C*
091100030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
091200030624     C                   if        vaoTCO = 'F'
091300030624     C* Quindi verifico se il codice ordinante non è valorizzato
091400050323     C                   if        vaoCOR = *zeros
091500050323     C*                  if        vaoCOR = *zeros  AND
091600050323     C*                            vaoRSO = *blanks AND
091700050323     C*                            vaoINO = *blanks AND
091800050323     C*                            vaoCAO = *blanks AND
091900050323     C*                            vaoLOO = *blanks AND
092000050323     C*                            vaoPRO = *blanks AND
092100050323     C*                            vaoNAO = *blanks
092200030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
092300030624     C                   move      vlrKSC        wCli              7 0
092400050323     C                   movel(p)  wCli          acrCRO
092500030624     C     acrCRO        setll     fnacr01l
092600030624     C                   if        %found(fnacr01l)
092700030624     C                   read      fnacr01l
092800030624     C                   dow       not %eof(fnacr01l)
092900030624     C                   movel     acrCRO        wCliAcr           7 0
093000030624     C                   if        wCliAcr = wCli
093100030624     C                   eval      vaoCOR = acrCRO
093200030624     C                   leave
093300030624     C                   else
093400030624     C                   leave
093500030624     C                   endif
093600030624     C                   read      fnacr01l
093700030624     C                   enddo
093800030624     C                   endif
093900030624     C                   endif
094000030624     C                   endif
094100030624     C*
094200030624     C                   if        vaoTCO = 'I'
094300030624     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
094400030624     C                   if        vlrKSC = '0ORM0000'
094500030624     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
094600061031     C***                eval      vaoINO = 'ORM DA INTERNET'
094700061031     C                   movel     ConstINO      vaoINO
094800030624     C                   eval      vaoCAO = '40138'
094900030624     C                   eval      vaoLOO = 'BOLOGNA'
095000030624     C                   eval      vaoPRO = 'BO'
095100030624     C                   endif
095200030624     C                   endif
095300030624     C*
095400030624     C                   ENDSR
095500030624
095600990920
095700010528
095800011105     C*------------------------------------------------------------------------*
095900011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
096000011105     C*------------------------------------------------------------------------*
096100011105     C     WRIORI        BEGSR
096200011105     C*
096300090617     C* Come prima cosa stacco un numeratore da AZNUM
096400151019     C***                clear                   trul33ds
096500151019     C***                eval      I33OPE = *zeros
096600151019     C***                eval      I33CNU = 600
096700151019     C***                eval      I33NUM = 1
096800151019     C***                movel     TRUL33DS      KPJBU
096900151019     C***                call      'TRUL33R'
097000151019     C***                parm                    KPJBA
097100151019     C***                movel     KPJBU         TRUL33DS
097200151019     C***                if        O33ERR = *zeros
097300151019     C***                z-add     O33NRF        §numori
097400151019     c***                else
097500151019     C***                eval      §numori = *all'9'
097600151019     C***                endif
097700070925     C*
097800070925     C* Giro "standard" (ovvero x cliente d invio dati)
097900070925     C                   if        *in51 = *off
098000011105     C*
098100011122     C* Quindi imposto i campi "extra-VAO"
098200011122     C                   if        vlrKSC = '0ORM0000'
098300011122     C*
098400011122     C                   if        vaoCOR > *zeros
098500011122     C                   movel     vaoCOR        wrkCOR7           7 0
098600011122     C*
098700011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
098800011122     C* codice padre. Se è un padre assegno già il codice ordinante.
098900011122     C                   clear                   tibs10ds
099000011122     C                   eval      d10drf = datcor                              *data riferimento
099100011122     C                   eval      d10paf = 'P'                                 *cerca il padre
099200011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
099300091026     C*
099400091026     C* Preparazione chiamata al TIBS10R1
099500091026     C                   clear                   tibs10r1ds
099600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
099700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
099800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
099900091026     C*
100000091026     C                   call      'TIBS10R1'
100100091026     C                   parm                    tibs10r1ds
100200091026     C*
100300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
100400011122     C*
100500011122     C                   if        d10err <> *blanks or                         *No unif. o padre
100600011122     C                             d10cop =  *zeros  or
100700011122     C                             d10cop =  d10cod
100800011122     C                   move      d10cod        oriIDC
100900011122     C                   else                                                   *Figlio
101000011122     C                   move      d10cop        oriIDC
101100011122     C                   endif
101200011122     C*
101300011122     C                   else
101400011122     C                   eval      oriIDC = *zeros
101500011122     C                   endif
101600011122     C*
101700011122     C                   else
101800011122     C                   eval      oriIDC = vlrKSC
101900011122     C                   endif
102000070925     C*
102100070925     C* Giro "forzato" (ovvero x codice ordinante)
102200070925     C                   else
102300070925     C*
102400070925     C                   if        vaoCOR > *zeros
102500070925     C                   movel     vaoCOR        wrkCOR7           7 0
102600070925     C*
102700070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
102800070925     C* codice padre. Se è un padre assegno già il codice ordinante.
102900070925     C                   clear                   tibs10ds
103000070925     C                   eval      d10drf = datcor                              *data riferimento
103100070925     C                   eval      d10paf = 'P'                                 *cerca il padre
103200070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
103300091026     C*
103400091026     C* Preparazione chiamata al TIBS10R1
103500091026     C                   clear                   tibs10r1ds
103600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
103700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
103800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
103900091026     C*
104000091026     C                   call      'TIBS10R1'
104100091026     C                   parm                    tibs10r1ds
104200091026     C*
104300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
104400070925     C*
104500070925     C                   if        d10err <> *blanks or                         *No unif. o padre
104600070925     C                             d10cop =  *zeros  or
104700070925     C                             d10cop =  d10cod
104800070925     C                   move      d10cod        oriIDC
104900070925     C                   else                                                   *Figlio
105000070925     C                   move      d10cop        oriIDC
105100070925     C                   endif
105200070926     C*
105300070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
105400070926     C* al servizio d rintraccia ritiri on-line
105500070926     C                   eval      vssKSU = oriIDC
105600070926     C                   eval      vssISV = 'T3'
105700070926     C     keyvss02_p    chain     tivss02l
105800070926     C                   if        %found(tivss02l)
105900070926     C                   if        vssdde <= datcor AND
106000070926     C                             datcor <= vssdsc
106100070926     C                   else
106200070926     C                   eval      oriIDC = vlrKSC
106300070926     C                   endif
106400070926     C                   else
106500070926     C                   eval      oriIDC = vlrKSC
106600070926     C                   endif
106700070926     C*
106800070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
106900070926     C* anagrafica clienti ritiro
107000070926     C                   setoff                                       52
107100070926     C                   eval      acrCRO = wrkCOR7 * 1000
107200070926     C     acrCRO        setll     fnacr01l
107300070926     C                   if        %found(fnacr01l)
107400070926     C                   read      fnacr01l
107500070926     C                   dow       not %eof(fnacr01l)
107600070926     C                   movel     acrCRO        wCliAcr           7 0
107700070926     C                   if        wCliAcr = wrkCOR7
107800070926     C                   seton                                        52
107900070926     C                   leave
108000070926     C                   else
108100070926     C                   leave
108200070926     C                   endif
108300070926     C                   read      fnacr01l
108400070926     C                   enddo
108500070926     C                   endif
108600070926     C*
108700070926     C                   if        *in52  = *off
108800070926     C                   eval      oriIDC = vlrKSC
108900070926     C                   endif
109000070925     C*
109100070925     C                   else
109200070925     C                   if        vlrKSC = '0ORM0000'
109300070925     C                   eval      oriIDC = *zeros
109400070925     C                   else
109500070925     C                   eval      oriIDC = vlrKSC
109600070925     C                   endif
109700070925     C                   endif
109800070925     C                   endif
109900011122     C*
110000011105     C                   eval      oriPRG = §numori
110100011105     C                   eval      oriDER = datcor
110200011105     C                   eval      oriOER = wn6
110300011105     C                   eval      oriPOE = vaoPOE
110400011105     C                   eval      oriNSR = vaoNSR
110500011105     C                   eval      oriNOR = vaoNOR
110600011105     C                   eval      oriNRV = vaoNRV
110700011105     C                   eval      oriFLO = *blanks
110800011105     C*
110900011122     C* Infine valorizzo la chiave esterna sul file FNVAO
111000011105     C                   eval      dorm01 = vaoFLO
111100011105     C                   movel     §numori       §ormpg
111200011105     C                   eval      vaoFLO = dorm01
111300011105     C*
111400060306     C* ...quindi scrivo il file TIORI00F
111500121205     C  N55              WRITE     TIORI000
111600011105     C*
111700011105     C                   ENDSR
111800050826
111900050826
112000050826
112100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
112200050826     C     wriVGD        BEGSR
112300050826     C*
112400050826     C* Reperisco la descrizione della fase da tabella
112500050826     C                   movel(p)  'FAR'         KteCOD
112600050826     C                   movel(p)  '000'         KteKE1
112700050826     C     KEYtbe        chain     tntbe01l
112800050826     C                   if        %found(tntbe01l)
112900050826     C                   movel(P)  tbeuni        DFAR
113000050826     C                   else
113100050826     C                   clear                   DFAR
113200050826     C                   endif
113300050826     C*
113400050826     C* Valorizzo buffer record
113500050826     C                   clear                   FNVAPDS
113600050826     C                   eval      vapIDC = oriIDC
113700050826     C                   eval      vapPOE = oriPOE
113800050826     C                   eval      vapRFA = vaoRFA
113900050826     C                   eval      vapPOG = oriPOE
114000050826     C                   eval      vapDAE = oriDER
114100050826     C                   eval      vapORE = oriOER
114200050826     C                   if        d§fardva <> *blanks
114300050826     C                   eval      vapDFA = d§fardva
114400050826     C                   else
114500050826     C                   eval      vapDFA = d§fardes
114600050826     C                   endif
114700050826     C*
114800050826     C                   clear                   tivgd000
114900080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
115000050826     C                   eval      vgdTIP = 'VP'
115100060306     C                   eval      vgdKSU = vapIDC
115200050826     C                   eval      vgdTSC = 'WW'
115300050826     C                   eval      vgdDAT = datcor
115400050826     C                   eval      vgdPGM = 'TIS7T7R'
115500060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
115600060306     C                   exsr      sr_Control
115700060306     C                   if        Cli_Abil = *on
115800060306     C                   eval      vgdKSU = woriIDC
115900121205     C  N55              write     tivgd000
116000060306     C                   endif
116100050826     C*
116200050826     C                   ENDSR
116300050826     C*------------------------------------------------------------------------*
116400011119
116500010528
116600010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
116700010528     C     repfil        BEGSR
116800010528     C*
116900010528     C                   if        invfil = *zeros and
117000010528     C                             depfil > *zeros and
117100010613     C                             (vinflg = *blanks or
117200010613     C                              vinflg = *zeros)
117300010528     C
117400010528     C                   eval      invfil = depfil
117500010528     C                   endif
117600010528     C*
117700010529     C                   if        depfil <> invfil and
117800010529     C                             invfil > *zeros
117900010528     C                   eval      flgMulti = '1'
118000010528     C                   if        vinflg = *blanks
118100010528     C                   add       1             cntNonEl
118200010528     C                   endif
118300010528     C                   endif
118400010528     C*
118500010528     C                   if        vinflg = '2'
118600010528     C                   eval      flgStato = '2'
118700010528     C                   endif
118800010528     C*
118900010528     C                   ENDSR
119000010528     C***
119100010528
119200010528
119300010528
119400010528
119500990920      /TITLE Invio dei dati al punto operativo.
119600000613     C     invio         BEGSR
119700990920     C*
119800990920     C                   reset                   dscmz
119900010528     C                   move      invfil        cmzdst
120000990920     C                   eval      cmzfld = vlrfou
120100990920     C                   eval      cmzmbd = vlrhdl
120200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
120300000710     C                   if        prmfir = *blanks
120400010122     C                   eval      cmzfla = 'FNVAO00F'
120500010122     C                   eval      cmzmba = 'FNVAO00F'
120600000710     C                   else
120700000710     C                   eval      cmzfla = prmfir
120800000710     C                   eval      cmzmba = prmfir
120900000710     C                   endif
121000990920     C                   eval      cmznrr = *zeros
121100990920     C                   move      §ctrok        cmznrr
121200010123     C                   eval      cmzlba = vlrfl1
121300990920     C                   call(e)   'TIS711C'
121400990920     C                   parm                    dscmz
121500990921     C                   parm      *blanks       esito
121600990920     C                   if        %error
121700990920     C                             or cmzerr = '1'
121800990921     C                             or esito  = '1'
121900000710     C                   eval      wrkesito = '3'
122000990920     C                   endif
122100990920     C*
122200000613     C                   ENDSR
122300000613     C***
122400990910
122500010528
122600010528
122700010528
122800010528      /TITLE Invio dei dati al punto operativo.
122900010528     C     opeini        BEGSR
123000010528     C*
123100010528     C* Inizializzo flag e contatori operativi
123200010528     C                   movel     '0'           flgGiro           1
123300010528     C                   movel     '0'           flgMulti          1
123400010528     C                   movel     '1'           flgStato          1
123500010615     C                   movel     '0'           flgOk             1
123600010528     C                   z-add     *zeros        cntNonEl         10 0
123700010528     C                   z-add     *zeros        depfil            3 0
123800010528     C                   z-add     *zeros        invfil            3 0
123900030407     C*
124000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
124100030407     C                   if        %subst(vlrppt:1:1) = 'S'
124200030407     C                   seton                                        50
124300030407     C                   else
124400030407     C                   setoff                                       50
124500030407     C                   endif
124600070925     C*
124700070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
124800070925     C                   if        %subst(vlrppt:2:1) = 'S'
124900070925     C                   seton                                        51
125000070925     C                   else
125100070925     C                   setoff                                       51
125200070925     C                   endif
125300150319     C*
125400150319     C* se il 4° char dei parametri è T=Test
125500150319     C* vince questo test su quello precedente per impostare VAOPOE
125600150319     C                   SETOFF                                       55
125700150319     C                   IF        %subst(vlrppt:4:1) = 'T'
125800150319     C* inibisco la scrittura dei file TIORI e TIVGD
125900150319     C                   SETON                                        55
126000150319     C* imposto il PO di emissione come filiale fittizia
126100150319     C                   EVAL      VAOPOE = 999
126200150319     C                   ENDIF
126300010528     C*
126400010528     C                   ENDSR
126500010528     C***
126600060306
126700060306
126800060306     C*-------------------------------------------------------------------------*
126900060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
127000060306     C*-------------------------------------------------------------------------*
127100060306     C     sr_Control    begsr
127200060306     C*
127300060306     C* inizializzo a ok il flag d controllo abilitazione
127400060306     C                   eval      Cli_Abil = *on
127500060306     C
127600060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
127700060306     C                   clear                   woriIDC
127800060306     C                   eval      kteCOD = 'VAP'
127900060306     C                   eval      kteKE1 = oriIDC
128000060306     C     keytbe        chain     TNTBE01L
128100060306     C                   if        %found(Tntbe01l)
128200060306     C                   eval      woriIDC = TbeKe2
128300060306     C                   else
128400060306     C                   eval      Cli_Abil = *off
128500060306     C                   endif
128600060306     C*
128700060306     C                   endsr
128800010528
128900010528
129000010528
129100010528
129200000613     C     *inzsr        BEGSR
129300990910     C*
129400990910     C     *entry        plist
129500990920     C                   parm                    tivlrds
129600990921     C                   parm      wrkesito      esito
129700000724     C                   parm                    prmlit
129800000710     C                   parm                    prmfir
129900011105     C*
130000011105     C* Campi riferito al database
130100011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
130200011105     C     *like         define    tbeke1        kteKE1
130300011105     C*
130400011105     C* Definizione chiavi di lettura
130500011105     C* TNTBE01L - parziale
130600011105     C     keytbe        klist
130700011105     C                   kfld                    kteCOD                         *UTENTE
130800011105     C                   kfld                    kteKE1                         *CODICE
130900070926     C* TIVSS02L - parziale
131000070926     C     keyvss02_p    klist
131100070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
131200070926     C                   kfld                    vssISV                         *SERVIZIO
131300011105     C*
131400011105     C* Reperimento data e ora corrente
131500011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
131600011105     C                   movel     wn14          wn6               6 0          *ORA (6)
131700011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
131800011105     C                   z-add     wn8           g08dat
131900011105     C                   z-add     *zeros        g08inv
132000011105     C                   movel     '0'           g08err
132100011105     C                   call      'XSRDA8'
132200011105     C                   parm                    wlbda8
132300011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
132400010529     C*
132500000613     C                   ENDSR
132600000613     C***
