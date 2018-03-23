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
005200011105     D*-------------------
005300011105     D* DS "XSRDA8" - controllo data (8)
005400011105     D*-------------------
005500011105     D wlbda8          DS                  INZ
005600011105     D  g08dat                 1      8  0
005700011105     D  g08inv                 9     16  0
005800011105     D  g08err                17     17
005900011105     D  g08tgi                18     22  0
006000011122     D*-------------------
006100010731     D* COSTANTI
006200011122     D*-------------------
006300010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006400010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006500990908
006600091026
006700091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006800091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006900091026
007000010528
007100010528
007200010528
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600000724     C*
007700010528     C                   exsr      opeini
007800010528     C                   exsr      rwvao
007900010528     C*
008000010528     C                   seton                                        lr
008100010528
008200010528
008300010528
008400010528
008500010528     C*--------------------------------------------------------
008600010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008700010528     C*--------------------------------------------------------
008800010528     C     PREELA        BEGSR
008900010528     C*
009000000724     C* SE OCCORRE SPEDIRE IN FILIALE
009100010528     C                   if        invfil <> *zeros and
009200010528     C                             flgGiro = '0'
009300010528     C*
009400010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009500010528     C                   eval      flgGiro = '1'
009600010529     C*
009700010529     C                   endif
009800010528     C*
009900010528     C                   ENDSR
010000010528     C***
010100010528
010200010528
010300010528
010400010528
010500010528
010600010528
010700010528     C*--------------------------------------------------------
010800010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010528     C*--------------------------------------------------------
011000010528     C     ENDELA        BEGSR
011100020917     C*
011200010528     C                   ENDSR
011300010528     C***
011400990908
011500010528
011600010528
011700010528
011800010528
011900910830     C*--------------------------------------------------------
012000010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
012100910830     C*--------------------------------------------------------
012200010122     C     RWVAO         BEGSR
012300010528     C*
012400990914     C                   if        not %open(tivin00r)
012500990908     C                   open      tivin00r
012600990914     C                   endif
012700010122     C                   if        not %open(fnvaowwr)
012800010122     C                   open      fnvaowwr
012900990914     C                   endif
013000990910     C*
013100990910     C                   clear                   §CTROK
013200990910     C                   clear                   §CTRMO
013300990910     C                   clear                   §CTRNO
013400990910     C*
013500921023     C                   DO        *HIVAL
013600990913     C*
013700990915     C                   READ      tivin00r                               70
013800010731     C*
013900010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
014000010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
014100010618     C*
014200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014300010618     C                   if        vinflg = '1'
014400010618     C                   eval      flgOk = '1'
014500010618     C                   endif
014600010618     C*
014700000905     C                   if        vindta > *blanks
014800000613     C                   add       1             rrnum
014900990913     C*
015000010711     C                   if        *in70 = *off and
015100010711     C                             (vinflg = *blanks
015200010711     C                              or vinflg = '0'
015300010711     C                              or vinflg = '2')
015400990913     C*
015500010528     C                   clear                   FNVAO000
015600011105     C                   clear                   TIORI000
015700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015800010711     C                   if        vinflg = *blanks or vinflg = '0'
015900010711     C                   clear                   vinmsg
016000010711     C                   endif
016100010711     C*
016200010122     C                   eval      fnvaoaar = vindta
016300080521     C*
016400080521     C* Verifico se richiesto nei parametri anagrafica cliente/tipo-file d particolari
016500080521     C* impostazioni dei flag operativi
016600080521     C                   if        %subst(vlrppt:3:1) = 'S'
016700080521     C                   eval      dorm01 = a_vaoFLO
016800080521     C                   eval      §ormks = 'S'
016900080521     C                   eval      a_vaoFLO = dorm01
017000080521     C                   endif
017100991022     C*
017200010122     C                   testb     x'3F'         a_vaoatb                 02
017300991022     C                   if        not *in02
017400991022     C*
017500971021     C                   SETOFF                                       303132
017600971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017700971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
017800971023     C*    OPPURE TUTTI BLANKS
017900010122     C*-------------------
018000010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
018100010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
018200010122     C*-------------------
018300010122     C*
018400011119 xxx C*                  TESTN                   a_vaopoe             30
018500010417  "  C* N30              TESTN                   a_vaopoe               30
018600010417  "  C* N30              TESTN                   a_vaopoe                 30
018700010417     C*                  MOVE      a_vaopoe      W001A             1
018800010417     C*    *IN30         IFEQ      *OFF
018900010417     C*    W001A         ORLT      '0'
019000010417     C*                  SETON                                        32
019100010417     C*                  MOVE      *ZEROS        VAOPOE
019200010417     C*                  EVAL      vinmsg = 'VAOPOE'
019300010417     C*                  ELSE
019400010417     C*                  MOVE      a_vaopoe      VAOPOE
019500010417     C*                  ENDIF
019600010417  "  C*
019700010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
019800010604 xxx C                   IF        vlrpoi <> 999
019900010528     C                   EVAL      VAOPOE = vlrpoi
020000010528     C                   ELSE
020100010528     C                   IF        a_vaopoe = *blanks or
020200011010     C                             a_vaopoe = *zeros  or
020300011010     C                             a_vaopoe = ' 000'
020400010528     C                   IF        a_vaopor <> *blanks and
020500011010     C                             a_vaopor <> *zeros  and
020600011010     C                             a_vaopor <> ' 000'
020700010528     C                   MOVE      a_vaopor      VAOPOE
020800010528     C                   ELSE
020900011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
021000010528     C                   MOVE      a_vaopoe      VAOPOE
021100010528     C                   ENDIF
021200010529     C                   ELSE
021300010529     C                   MOVE      a_vaopoe      VAOPOE
021400010528     C                   ENDIF
021500010528     C                   ENDIF
021600010122     C*
021700010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021800010528     C                   eval      depfil = VAOPOE
021900010528     C                   exsr      repfil
022000010528     C                   if        depfil = invfil and
022100010528     C                             (vinflg = *blanks
022200010528     C                              or vinflg = '0'
022300010528     C                              or vinflg = '2')
022400010528     C*
022500010528     C                   exsr      PREELA
022600010710     C*
022700010719     C*                  TESTN                   a_vaonsr             30
022800010719     C* N30              TESTN                   a_vaonsr               30
022900010719     C* N30              TESTN                   a_vaonsr                 30
023000010719     C*                  MOVE      a_vaonsr      W001A             1
023100010719     C*    *IN30         IFEQ      *OFF
023200010719     C*    W001A         ORLT      '0'
023300010719     C*                  SETON                                        32
023400010719     C*                  MOVE      *ZEROS        VAONSR
023500010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
023600010719     C*                  ELSE
023700010719     C*                  MOVE      a_vaonsr      VAONSR
023800010719     C*                  ENDIF
023900010122     C*
024000010719     C*                  TESTN                   a_vaonor             30
024100010719     C* N30              TESTN                   a_vaonor               30
024200010719     C* N30              TESTN                   a_vaonor                 30
024300010719     C*                  MOVE      a_vaonor      W001A             1
024400010719     C*    *IN30         IFEQ      *OFF
024500010719     C*    W001A         ORLT      '0'
024600010719     C*                  SETON                                        32
024700010719     C*                  MOVE      *ZEROS        VAONOR
024800010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
024900010719     C*                  ELSE
025000010719     C*                  MOVE      a_vaonor      VAONOR
025100010719     C*                  ENDIF
025200010122     C*
025300010719     C*                  TESTN                   a_vaonrv             30
025400010719     C* N30              TESTN                   a_vaonrv               30
025500010719     C* N30              TESTN                   a_vaonrv                 30
025600010719     C*                  MOVE      a_vaonrv      W001A             1
025700010719     C*    *IN30         IFEQ      *OFF
025800010719     C*    W001A         ORLT      '0'
025900010719     C*                  SETON                                        32
026000010719     C*                  MOVE      *ZEROS        VAONRV
026100010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
026200010719     C*                  ELSE
026300010719     C*                  MOVE      a_vaonrv      VAONRV
026400010719     C*                  ENDIF
026500010122     C*
026600010122     C                   TESTN                   a_vaodao             30
026700010122     C  N30              TESTN                   a_vaodao               30
026800010122     C  N30              TESTN                   a_vaodao                 30
026900010122     C                   MOVE      a_vaodao      W001A             1
027000010122     C     *IN30         IFEQ      *OFF
027100010122     C     W001A         ORLT      '0'
027200010122     C                   SETON                                        32
027300050421     C                   MOVE      *ZEROS        VAODAO
027400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
027500010122     C                   ELSE
027600050421     C                   MOVE      a_vaodao      VAODAO
027700010122     C                   ENDIF
027800010122     C*
027900010122     C                   TESTN                   a_vaooao             30
028000010122     C  N30              TESTN                   a_vaooao               30
028100010122     C  N30              TESTN                   a_vaooao                 30
028200010122     C                   MOVE      a_vaooao      W001A             1
028300010122     C     *IN30         IFEQ      *OFF
028400010122     C     W001A         ORLT      '0'
028500010122     C                   SETON                                        32
028600050421     C                   MOVE      *ZEROS        VAOOAO
028700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
028800010122     C                   ELSE
028900050421     C                   MOVE      a_vaooao      VAOOAO
029000010122     C                   ENDIF
029100010122     C*
029200010122     C                   TESTN                   a_vaocor             30
029300010122     C  N30              TESTN                   a_vaocor               30
029400010122     C  N30              TESTN                   a_vaocor                 30
029500010122     C                   MOVE      a_vaocor      W001A             1
029600010122     C     *IN30         IFEQ      *OFF
029700010122     C     W001A         ORLT      '0'
029800010122     C                   SETON                                        32
029900010122     C                   MOVE      *ZEROS        VAOCOR
030000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
030100010122     C                   ELSE
030200050421     C                   MOVE      a_vaocor      VAOCOR
030300010122     C                   ENDIF
030400010122     C*
030500010122     C                   TESTN                   a_vaocra             30
030600010122     C  N30              TESTN                   a_vaocra               30
030700010122     C  N30              TESTN                   a_vaocra                 30
030800010122     C                   MOVE      a_vaocra      W001A             1
030900010122     C     *IN30         IFEQ      *OFF
031000010122     C     W001A         ORLT      '0'
031100010122     C                   SETON                                        32
031200010122     C                   MOVE      *ZEROS        VAOCRA
031300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
031400010122     C                   ELSE
031500010122     C                   MOVE      a_vaocra      VAOCRA
031600010122     C                   ENDIF
031700010122     C*
031800010719     C                   IF        a_vaocrc = *blanks or
031900010719     C                             a_vaocrc = *zeros
032000010719     C                   ELSE
032100010719     C                   TESTN                   a_vaocrc             30
032200010719     C  N30              TESTN                   a_vaocrc               30
032300010719     C  N30              TESTN                   a_vaocrc                 30
032400010719     C                   MOVE      a_vaocrc      W001A             1
032500010719     C     *IN30         IFEQ      *OFF
032600010719     C     W001A         ORLT      '0'
032700010719     C                   SETON                                        32
032800010719     C                   MOVE      *ZEROS        VAOCRC
032900010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
033000010719     C                   ELSE
033100010719     C                   MOVE      a_vaocrc      VAOCRC
033200010719     C                   ENDIF
033300010719     C                   ENDIF
033400010122     C*
033500010122     C                   TESTN                   a_vaodar             30
033600010122     C  N30              TESTN                   a_vaodar               30
033700010122     C  N30              TESTN                   a_vaodar                 30
033800010122     C                   MOVE      a_vaodar      W001A             1
033900010122     C     *IN30         IFEQ      *OFF
034000010122     C     W001A         ORLT      '0'
034100010122     C                   SETON                                        32
034200010122     C                   MOVE      *ZEROS        VAODAR
034300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
034400010122     C                   ELSE
034500010122     C                   MOVE      a_vaodar      VAODAR
034600010122     C                   ENDIF
034700010122     C*
034800010122     C                   TESTN                   a_vaoorr             30
034900010122     C  N30              TESTN                   a_vaoorr               30
035000010122     C  N30              TESTN                   a_vaoorr                 30
035100010122     C                   MOVE      a_vaoorr      W001A             1
035200010122     C     *IN30         IFEQ      *OFF
035300010122     C     W001A         ORLT      '0'
035400010122     C                   SETON                                        32
035500010122     C                   MOVE      *ZEROS        VAOORR
035600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
035700010122     C                   ELSE
035800010122     C                   MOVE      a_vaoorr      VAOORR
035900010122     C                   ENDIF
036000010122     C*
036100010122     C                   TESTN                   a_vaoncl             30
036200010122     C  N30              TESTN                   a_vaoncl               30
036300010122     C  N30              TESTN                   a_vaoncl                 30
036400010122     C                   MOVE      a_vaoncl      W001A             1
036500010122     C     *IN30         IFEQ      *OFF
036600010122     C     W001A         ORLT      '0'
036700010122     C                   SETON                                        32
036800010122     C                   MOVE      *ZEROS        VAONCL
036900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
037000010122     C                   ELSE
037100010122     C                   MOVE      a_vaoncl      VAONCL
037200010122     C                   ENDIF
037300010122     C*
037400010122     C                   MOVE      *ZERO         VAOPKG
037500010122     C                   MOVE      a_vaopkg      WASC09
037600010122     C                   TESTN                   WI09                 30
037700010122     C  N30              TESTN                   WI09                   30
037800010122     C  N30              TESTN                   WI09                     30
037900010122     C                   MOVE      WI09          W001A             1
038000010122     C     *IN30         IFEQ      *OFF
038100010122     C     W001A         ORLT      '0'
038200010122     C     W001A         ANDNE     *BLANK
038300010122     C                   SETON                                        32
038400010122     C                   MOVE      *ZERO         VAOPKG
038500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
038600010122     C                   ELSE
038700010122     C                   MOVEL     WI09          VAOPKG
038800010122     C                   TESTN                   WD09                 30
038900010122     C  N30              TESTN                   WD09                     30
039000010122     C                   MOVE      WD09          W001A             1
039100010122     C     *IN30         IFEQ      *OFF
039200010122     C     W001A         ORLT      '0'
039300010122     C     W001A         ANDNE     *BLANK
039400010122     C                   SETON                                        32
039500010122     C                   MOVE      *ZERO         VAOPKG
039600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
039700010122     C                   ELSE
039800010122     C                   MOVE      WD09          VAOPKG
039900010122     C                   ENDIF
040000050118     C                   ENDIF
040100010122     C*
040200010122     C                   MOVE      *ZERO         VAOVLM
040300010122     C                   MOVE      a_vaovlm      WASC07
040400010122     C                   TESTN                   WI07                 30
040500010122     C  N30              TESTN                   WI07                   30
040600010122     C  N30              TESTN                   WI07                     30
040700010122     C                   MOVE      WI07          W001A             1
040800010122     C     *IN30         IFEQ      *OFF
040900010122     C     W001A         ORLT      '0'
041000010122     C     W001A         ANDNE     *BLANK
041100010122     C                   SETON                                        32
041200010122     C                   MOVE      *ZERO         VAOVLM
041300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
041400010122     C                   ELSE
041500010122     C                   MOVEL     WI07          VAOVLM
041600010122     C                   TESTN                   WD07                 30
041700010122     C  N30              TESTN                   WD07                   30
041800010122     C  N30              TESTN                   WD07                     30
041900010122     C                   MOVE      WD07          W001A             1
042000010122     C     *IN30         IFEQ      *OFF
042100010122     C     W001A         ORLT      '0'
042200010122     C     W001A         ANDNE     *BLANK
042300010122     C                   SETON                                        32
042400010122     C                   MOVE      *ZERO         VAOVLM
042500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
042600010122     C                   ELSE
042700010122     C                   MOVE      WD07          VAOVLM
042800010122     C                   ENDIF
042900050118     C                   ENDIF
043000010122     C*
043100010122     C                   TESTN                   a_vaobnc             30
043200010122     C  N30              TESTN                   a_vaobnc               30
043300010122     C  N30              TESTN                   a_vaobnc                 30
043400010122     C                   MOVE      a_vaobnc      W001A             1
043500010122     C     *IN30         IFEQ      *OFF
043600010122     C     W001A         ORLT      '0'
043700010122     C                   SETON                                        32
043800010122     C                   MOVE      *ZEROS        VAOBNC
043900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
044000010122     C                   ELSE
044100010122     C                   MOVE      a_vaobnc      VAOBNC
044200010122     C                   ENDIF
044300010122     C*
044400010122     C                   TESTN                   a_vaoblc             30
044500010122     C  N30              TESTN                   a_vaoblc               30
044600010122     C  N30              TESTN                   a_vaoblc                 30
044700010122     C                   MOVE      a_vaoblc      W001A             1
044800010122     C     *IN30         IFEQ      *OFF
044900010122     C     W001A         ORLT      '0'
045000010122     C                   SETON                                        32
045100010122     C                   MOVE      *ZEROS        VAOBLC
045200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
045300010122     C                   ELSE
045400010122     C                   MOVE      a_vaoblc      VAOBLC
045500010122     C                   ENDIF
045600010122     C*
045700010122     C                   TESTN                   a_vaoatt             30
045800010122     C  N30              TESTN                   a_vaoatt               30
045900010122     C  N30              TESTN                   a_vaoatt                 30
046000010122     C                   MOVE      a_vaoatt      W001A             1
046100010122     C     *IN30         IFEQ      *OFF
046200010122     C     W001A         ORLT      '0'
046300010122     C                   SETON                                        32
046400010122     C                   MOVE      *ZEROS        VAOATT
046500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
046600010122     C                   ELSE
046700010122     C                   MOVE      a_vaoatt      VAOATT
046800010122     C                   ENDIF
046900010122     C*
047000010122     C                   TESTN                   a_vaomtc             30
047100010122     C  N30              TESTN                   a_vaomtc               30
047200010122     C  N30              TESTN                   a_vaomtc                 30
047300010122     C                   MOVE      a_vaomtc      W001A             1
047400010122     C     *IN30         IFEQ      *OFF
047500010122     C     W001A         ORLT      '0'
047600010122     C                   SETON                                        32
047700010122     C                   MOVE      *ZEROS        VAOMTC
047800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
047900010122     C                   ELSE
048000010122     C                   MOVE      a_vaomtc      VAOMTC
048100010122     C                   ENDIF
048200010122     C*
048300010719     C                   IF        a_vaoksc = *blanks or
048400010719     C                             a_vaoksc = *zeros
048500010719     C                   ELSE
048600010719     C                   TESTN                   a_vaoksc             30
048700010719     C  N30              TESTN                   a_vaoksc               30
048800010719     C  N30              TESTN                   a_vaoksc                 30
048900010719     C                   MOVE      a_vaoksc      W001A             1
049000010719     C     *IN30         IFEQ      *OFF
049100010719     C     W001A         ORLT      '0'
049200010719     C                   SETON                                        32
049300010719     C                   MOVE      *ZEROS        VAOKSC
049400010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
049500010719     C                   ELSE
049600010719     C                   MOVE      a_vaoksc      VAOKSC
049700010719     C                   ENDIF
049800010719     C                   ENDIF
049900010122     C*
050000010710     C                   IF        a_vaoctr > *blanks
050100010122     C                   TESTN                   a_vaoctr             30
050200010122     C  N30              TESTN                   a_vaoctr               30
050300010122     C  N30              TESTN                   a_vaoctr                 30
050400010122     C                   MOVE      a_vaoctr      W001A             1
050500010122     C     *IN30         IFEQ      *OFF
050600010122     C     W001A         ORLT      '0'
050700010122     C                   SETON                                        32
050800010627     C                   MOVE      *blanks       VAOCTR
050900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
051000010122     C                   ELSE
051100010122     C                   MOVE      a_vaoctr      VAOCTR
051200010122     C                   ENDIF
051300010710     C                   ENDIF
051400010122     C*
051500010122     C                   TESTN                   a_vaopor             30
051600010122     C  N30              TESTN                   a_vaopor               30
051700010122     C  N30              TESTN                   a_vaopor                 30
051800010122     C                   MOVE      a_vaopor      W001A             1
051900010122     C     *IN30         IFEQ      *OFF
052000010122     C     W001A         ORLT      '0'
052100010122     C                   SETON                                        32
052200010122     C                   MOVE      *ZEROS        VAOPOR
052300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
052400010122     C                   ELSE
052500010122     C                   MOVE      a_vaopor      VAOPOR
052600010122     C                   ENDIF
052700010122     C*
052800010719     C                   IF        a_vaopoc = *blanks or
052900010719     C                             a_vaopoc = *zeros
053000010719     C                   ELSE
053100010122     C                   TESTN                   a_vaopoc             30
053200010122     C  N30              TESTN                   a_vaopoc               30
053300010122     C  N30              TESTN                   a_vaopoc                 30
053400010122     C                   MOVE      a_vaopoc      W001A             1
053500010122     C     *IN30         IFEQ      *OFF
053600010122     C     W001A         ORLT      '0'
053700010122     C                   SETON                                        32
053800010122     C                   MOVE      *ZEROS        VAOPOC
053900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
054000010122     C                   ELSE
054100010122     C                   MOVE      a_vaopoc      VAOPOC
054200010122     C                   ENDIF
054300010719     C                   ENDIF
054400010122     C*
054500010719     C                   IF        a_vaosto = *blanks or
054600010719     C                             a_vaosto = *zeros
054700010719     C                   ELSE
054800010122     C                   TESTN                   a_vaosto             30
054900010122     C  N30              TESTN                   a_vaosto               30
055000010122     C  N30              TESTN                   a_vaosto                 30
055100010122     C                   MOVE      a_vaosto      W001A             1
055200010122     C     *IN30         IFEQ      *OFF
055300010122     C     W001A         ORLT      '0'
055400010122     C                   SETON                                        32
055500010122     C                   MOVE      *ZEROS        VAOSTO
055600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
055700010122     C                   ELSE
055800010122     C                   MOVE      a_vaosto      VAOSTO
055900010122     C                   ENDIF
056000010719     C                   ENDIF
056100971021     C*
056200971021     C* CAMPI ALFABETICI:
056300010122     C                   MOVE      a_vaoatb      VAOATB
056400010122     C                   MOVE      a_vaotor      VAOTOR
056500010531     C*
056600010531     C                   IF        a_vaotco = *blanks
056700010531     C                   MOVE      'F'           VAOTCO
056800010531     C                   ELSE
056900010531     C                   MOVE      a_vaotco      VAOTCO
057000010531     C                   ENDIF
057100010531     C*
057200010122     C                   MOVE      a_vaorso      VAORSO
057300010122     C                   MOVE      a_vaoino      VAOINO
057400010122     C                   MOVE      a_vaocao      VAOCAO
057500010122     C                   MOVE      a_vaoloo      VAOLOO
057600031014     C                   MOVE      a_vaopro      VAOPRO
057700090129     C                   MOVE      a_vaonao      VAONAO
057800131007     C*
057900131007     C                   MOVE      a_vaorsr      VAORSR
058000131007     C                   MOVE      a_vaoinr      VAOINR
058100131007     C                   MOVE      a_vaocar      VAOCAR
058200131007     C                   MOVE      a_vaolor      VAOLOR
058300131007     C                   MOVE      a_vaoprr      VAOPRR
058400131007     C                   MOVE      a_vaonar      VAONAR
058500131007     C*
058600131007     C                   MOVE      a_vaorer      VAORER
058700131007     C                   MOVE      a_vaoter      VAOTER
058800131007     C                   MOVE      a_vaorsc      VAORSC
058900131007     C                   MOVE      a_vaoinc      VAOINC
059000131007     C                   MOVE      a_vaocac      VAOCAC
059100131007     C                   MOVE      a_vaoloc      VAOLOC
059200131007     C                   MOVE      a_vaoprc      VAOPRC
059300131007     C                   MOVE      a_vaonac      VAONAC
059400131007     C*
059500131007     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
059600131007     C*** VAONAO
059700131007     C                   IF        %trim(VAONAO) = 'I'  OR
059800131007     C                             %trim(VAONAO) = 'IT' OR
059900131007     C                             %trim(VAONAO) = 'VA' OR
060000131007     C                             %trim(VAONAO) = 'SM'
060100131007     C                   EVAL      VAONAO = *blanks
060200131007     C                   ENDIF
060300131007     C*** VAONAR
060400131007     C                   IF        %trim(VAONAR) = 'I'  OR
060500131007     C                             %trim(VAONAR) = 'IT' OR
060600131007     C                             %trim(VAONAR) = 'VA' OR
060700131007     C                             %trim(VAONAR) = 'SM'
060800131007     C
060900131007     C                   EVAL      VAONAR = *blanks
061000131007     C                   ENDIF
061100131007     C*** VAONAC
061200131007     C                   IF        %trim(VAONAC) = 'I'  OR
061300131007     C                             %trim(VAONAC) = 'IT' OR
061400131007     C                             %trim(VAONAC) = 'VA' OR
061500131007     C                             %trim(VAONAC) = 'SM'
061600131007     C                   EVAL      VAONAC = *blanks
061700131007     C                   ENDIF
061800131007     C*
061900031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
062000031014     C                   IF        VAOPRO  = *blanks AND
062100050907     C                             VAOCAO <> *blanks AND
062200050907     C                             VAONAO  = *blanks                            * solo x Italia
062300031014     C                   CLEAR                   TISI95DS
062400031014     C                   EVAL      I95TCN = '3'
062500031014     C                   Z-ADD     datcor        I95DAT
062600031014     C                   EVAL      I95CAP = VAOCAO
062700031014     C                   EVAL      I95LOC = VAOLOO
062800031014     C                   CALL      'TISI95R'
062900031014     C                   PARM                    TISI95DS
063000031014     C                   EVAL      VAOPRO = O95PRV
063100031014     C                   ENDIF
063200131007     C*
063300031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
063400031014     C                   IF        VAOPRR  = *blanks AND
063500050907     C                             VAOCAR <> *blanks AND
063600050907     C                             VAONAR  = *blanks                            * solo x Italia
063700031014     C                   CLEAR                   TISI95DS
063800031014     C                   EVAL      I95TCN = '3'
063900031014     C                   Z-ADD     datcor        I95DAT
064000031014     C                   EVAL      I95CAP = VAOCAR
064100031014     C                   EVAL      I95LOC = VAOLOR
064200031014     C                   CALL      'TISI95R'
064300031014     C                   PARM                    TISI95DS
064400031014     C                   EVAL      VAOPRR = O95PRV
064500031014     C                   ENDIF
064600131007     C*
064700031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
064800031014     C                   IF        VAOPRC  = *blanks AND
064900050907     C                             VAOCAC <> *blanks AND
065000050907     C                             VAONAC  = *blanks                            * solo x Italia
065100031014     C                   CLEAR                   TISI95DS
065200031014     C                   EVAL      I95TCN = '3'
065300031014     C                   Z-ADD     datcor        I95DAT
065400031014     C                   EVAL      I95CAP = VAOCAC
065500031014     C                   EVAL      I95LOC = VAOLOC
065600031014     C                   CALL      'TISI95R'
065700031014     C                   PARM                    TISI95DS
065800031014     C                   EVAL      VAOPRC = O95PRV
065900031014     C                   ENDIF
066000031014     C*
066100010122     C                   MOVE      a_vaoffd      VAOFFD
066200010122     C                   MOVE      a_vaormp      VAORMP
066300010122     C                   MOVE      a_vaonam      VAONAM
066400010122     C                   MOVE      a_vaopag      VAOPAG
066500010122     C                   MOVE      a_vaozor      VAOZOR
066600010122     C                   MOVE      a_vaono1      VAONO1
066700010122     C                   MOVE      a_vaono2      VAONO2
066800010122     C                   MOVE      a_vaoddt      VAODDT
066900010122     C                   MOVE      a_vaorfa      VAORFA
067000010122     C                   MOVE      a_vaospi      VAOSPI
067100010122     C                   MOVE      a_vaoflo      VAOFLO
067200050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
067300050304     C                   IF        VAOPKG =  *zeros AND
067400050304     C                             VAOVLM =  *zeros AND
067500050304     C                             VAOBNC =  *zeros AND
067600050304     C                             VAONCL <> *zeros
067700050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
067800050304     C                   ENDIF
067900030603     C*
068000030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
068100030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
068200030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
068300050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
068400930409     C*
068500010604     C  N31              ADD       1             §CTROK            7 0
068600010604     C   32              ADD       1             §CTRMO            7 0
068700010604     C   31              ADD       1             §CTRNO            7 0
068800030624     C  N31              EXSR      CHKCOR
068900011105     C  N31              EXSR      WRIORI
069000050826     C  N31              EXSR      WRIVGD
069100050421     C*
069200050421     C***
069300050421     C* FORZATURA SU DATA/ORA => NN CONSIDERO QUELLE PASSATE SUL FILE DAL CLIENTE MA SEMPRE
069400050421     C* DATA/ORA DI TRADUZIONE (OVVERO VERA IMMISSIONE DEL DATO SEPPUR DA CONFERMARE SUL
069500050421     C* NS. GESTIONALE). QUESTO SOLO X LA SCRITTURA DEL VAO!!!
069600050421     C                   eval      VAODAO = datcor
069700050421     C                   eval      VAOOAO = wn6
069800050421     C***
069900050421     C  N31              WRITE     FNVAO000
070000990910     C*
070100010604     C                   if        *in31 = *off and
070200010604     C                             *in32 = *off
070300990910     C                   eval      vinflg = '1'
070400990910     C                   else
070500990910     C                   eval      vinflg = '2'
070600990910     C                   endif
070700991022     C                   endif
070800990910     C                   endif
070900010604     C*
071000010604     C                   endif
071100000905     C*
071200000905     C                   else
071300000905     C                   eval      vinflg = '1'
071400000905     C                   endif
071500000905     C*
071600000905     C  N70              update    tivin000
071700991022     C*
071800991022     C  N70              ENDdo
071900010528     C*
072000010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
072100010528     C                   if        cntNonEl = *zeros or
072200010528     C                             flgMulti = '0'
072300010528     C* Se non ci sono record con errori ...
072400010528     C                   if        §ctrno = 0 and
072500010604     C                             §ctrmo = 0 and
072600010528     C                             flgStato <> '2'
072700990910     C* ... restituisco esito OK.
072800990921     C                   eval      wrkesito = '0'
072900990910     C                   else
073000000710     C                   if        §ctrok > 0
073100990921     C                   eval      wrkesito = '1'
073200000710     C                   else
073300010615     C                   if        flgOk = '0'
073400010615     C                   eval      wrkesito = '2'
073500010615     C                   else
073600010615     C                   eval      wrkesito = '6'
073700010615     C                   endif
073800990910     C                   endif
073900010528     C                   endif
074000010529     C                   else
074100010529     C                   eval      wrkesito = '9'
074200000710     C                   endif
074300990910     C*
074400990914     C                   if        %open(tivin00r)
074500990908     C                   close     tivin00r
074600990914     C                   endif
074700010122     C                   if        %open(fnvaowwr)
074800010122     C                   close     fnvaowwr
074900990914     C                   endif
075000990910     C*
075100010528     C                   if        vlrpoi <> 999
075200010528     C                   eval      invfil = vlrpoi
075300010528     C                   endif
075400010528     C*
075500990920     C                   if        §ctrok > 0
075600010528     C                             and invfil > *zeros
075700000613     C                   exsr      invio
075800990920     C                   endif
075900010612     C*
076000010612     C                   if        flgGiro = '1'
076100010612     C                   exsr      endela
076200010612     C                   endif
076300010528     C*
076400910830     C                   ENDSR
076500000613     C***
076600030624
076700030624
076800030624     C*------------------------------------------------------------------------*
076900030624     C* CHKCOR - Considerazioni su VAOCOR
077000030624     C*------------------------------------------------------------------------*
077100030624     C     CHKCOR        BEGSR
077200030624     C*
077300030624     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
077400030624     C                   if        vaoTCO = 'F'
077500030624     C* Quindi verifico se il codice ordinante non è valorizzato
077600050323     C                   if        vaoCOR = *zeros
077700050323     C*                  if        vaoCOR = *zeros  AND
077800050323     C*                            vaoRSO = *blanks AND
077900050323     C*                            vaoINO = *blanks AND
078000050323     C*                            vaoCAO = *blanks AND
078100050323     C*                            vaoLOO = *blanks AND
078200050323     C*                            vaoPRO = *blanks AND
078300050323     C*                            vaoNAO = *blanks
078400030624     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
078500030624     C                   move      vlrKSC        wCli              7 0
078600050323     C                   movel(p)  wCli          acrCRO
078700030624     C     acrCRO        setll     fnacr01l
078800030624     C                   if        %found(fnacr01l)
078900030624     C                   read      fnacr01l
079000030624     C                   dow       not %eof(fnacr01l)
079100030624     C                   movel     acrCRO        wCliAcr           7 0
079200030624     C                   if        wCliAcr = wCli
079300030624     C                   eval      vaoCOR = acrCRO
079400030624     C                   leave
079500030624     C                   else
079600030624     C                   leave
079700030624     C                   endif
079800030624     C                   read      fnacr01l
079900030624     C                   enddo
080000030624     C                   endif
080100030624     C                   endif
080200030624     C                   endif
080300030624     C*
080400030624     C                   if        vaoTCO = 'I'
080500030624     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
080600030624     C                   if        vlrKSC = '0ORM0000'
080700030624     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
080800061031     C***                eval      vaoINO = 'ORM DA INTERNET'
080900061031     C                   movel     ConstINO      vaoINO
081000030624     C                   eval      vaoCAO = '40138'
081100030624     C                   eval      vaoLOO = 'BOLOGNA'
081200030624     C                   eval      vaoPRO = 'BO'
081300030624     C                   endif
081400030624     C                   endif
081500030624     C*
081600030624     C                   ENDSR
081700030624
081800990920
081900010528
082000011105     C*------------------------------------------------------------------------*
082100011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
082200011105     C*------------------------------------------------------------------------*
082300011105     C     WRIORI        BEGSR
082400011105     C*
082500090617     C* Come prima cosa stacco un numeratore da AZNUM
082600090617     C                   clear                   trul33ds
082700090617     C                   eval      I33OPE = *zeros
082800090617     C                   eval      I33CNU = 600
082900090617     C                   eval      I33NUM = 1
083000090617     C                   movel     TRUL33DS      KPJBU
083100090617     C                   call      'TRUL33R'
083200090617     C                   parm                    KPJBA
083300090617     C                   movel     KPJBU         TRUL33DS
083400090617     C                   if        O33ERR = *zeros
083500090617     C                   z-add     O33NRF        §numori
083600090618     c                   else
083700090618     C                   eval      §numori = *all'9'
083800090617     C                   endif
083900070925     C*
084000070925     C* Giro "standard" (ovvero x cliente d invio dati)
084100070925     C                   if        *in51 = *off
084200011105     C*
084300011122     C* Quindi imposto i campi "extra-VAO"
084400011122     C                   if        vlrKSC = '0ORM0000'
084500011122     C*
084600011122     C                   if        vaoCOR > *zeros
084700011122     C                   movel     vaoCOR        wrkCOR7           7 0
084800011122     C*
084900011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
085000011122     C* codice padre. Se è un padre assegno già il codice ordinante.
085100011122     C                   clear                   tibs10ds
085200011122     C                   eval      d10drf = datcor                              *data riferimento
085300011122     C                   eval      d10paf = 'P'                                 *cerca il padre
085400011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
085500091026     C*
085600091026     C* Preparazione chiamata al TIBS10R1
085700091026     C                   clear                   tibs10r1ds
085800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
085900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
086000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
086100091026     C*
086200091026     C                   call      'TIBS10R1'
086300091026     C                   parm                    tibs10r1ds
086400091026     C*
086500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
086600011122     C*
086700011122     C                   if        d10err <> *blanks or                         *No unif. o padre
086800011122     C                             d10cop =  *zeros  or
086900011122     C                             d10cop =  d10cod
087000011122     C                   move      d10cod        oriIDC
087100011122     C                   else                                                   *Figlio
087200011122     C                   move      d10cop        oriIDC
087300011122     C                   endif
087400011122     C*
087500011122     C                   else
087600011122     C                   eval      oriIDC = *zeros
087700011122     C                   endif
087800011122     C*
087900011122     C                   else
088000011122     C                   eval      oriIDC = vlrKSC
088100011122     C                   endif
088200070925     C*
088300070925     C* Giro "forzato" (ovvero x codice ordinante)
088400070925     C                   else
088500070925     C*
088600070925     C                   if        vaoCOR > *zeros
088700070925     C                   movel     vaoCOR        wrkCOR7           7 0
088800070925     C*
088900070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
089000070925     C* codice padre. Se è un padre assegno già il codice ordinante.
089100070925     C                   clear                   tibs10ds
089200070925     C                   eval      d10drf = datcor                              *data riferimento
089300070925     C                   eval      d10paf = 'P'                                 *cerca il padre
089400070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
089500091026     C*
089600091026     C* Preparazione chiamata al TIBS10R1
089700091026     C                   clear                   tibs10r1ds
089800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
089900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
090000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
090100091026     C*
090200091026     C                   call      'TIBS10R1'
090300091026     C                   parm                    tibs10r1ds
090400091026     C*
090500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
090600070925     C*
090700070925     C                   if        d10err <> *blanks or                         *No unif. o padre
090800070925     C                             d10cop =  *zeros  or
090900070925     C                             d10cop =  d10cod
091000070925     C                   move      d10cod        oriIDC
091100070925     C                   else                                                   *Figlio
091200070925     C                   move      d10cop        oriIDC
091300070925     C                   endif
091400070926     C*
091500070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
091600070926     C* al servizio d rintraccia ritiri on-line
091700070926     C                   eval      vssKSU = oriIDC
091800070926     C                   eval      vssISV = 'T3'
091900070926     C     keyvss02_p    chain     tivss02l
092000070926     C                   if        %found(tivss02l)
092100070926     C                   if        vssdde <= datcor AND
092200070926     C                             datcor <= vssdsc
092300070926     C                   else
092400070926     C                   eval      oriIDC = vlrKSC
092500070926     C                   endif
092600070926     C                   else
092700070926     C                   eval      oriIDC = vlrKSC
092800070926     C                   endif
092900070926     C*
093000070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
093100070926     C* anagrafica clienti ritiro
093200070926     C                   setoff                                       52
093300070926     C                   eval      acrCRO = wrkCOR7 * 1000
093400070926     C     acrCRO        setll     fnacr01l
093500070926     C                   if        %found(fnacr01l)
093600070926     C                   read      fnacr01l
093700070926     C                   dow       not %eof(fnacr01l)
093800070926     C                   movel     acrCRO        wCliAcr           7 0
093900070926     C                   if        wCliAcr = wrkCOR7
094000070926     C                   seton                                        52
094100070926     C                   leave
094200070926     C                   else
094300070926     C                   leave
094400070926     C                   endif
094500070926     C                   read      fnacr01l
094600070926     C                   enddo
094700070926     C                   endif
094800070926     C*
094900070926     C                   if        *in52  = *off
095000070926     C                   eval      oriIDC = vlrKSC
095100070926     C                   endif
095200070925     C*
095300070925     C                   else
095400070925     C                   if        vlrKSC = '0ORM0000'
095500070925     C                   eval      oriIDC = *zeros
095600070925     C                   else
095700070925     C                   eval      oriIDC = vlrKSC
095800070925     C                   endif
095900070925     C                   endif
096000070925     C                   endif
096100011122     C*
096200011105     C                   eval      oriPRG = §numori
096300011105     C                   eval      oriDER = datcor
096400011105     C                   eval      oriOER = wn6
096500011105     C                   eval      oriPOE = vaoPOE
096600011105     C                   eval      oriNSR = vaoNSR
096700011105     C                   eval      oriNOR = vaoNOR
096800011105     C                   eval      oriNRV = vaoNRV
096900011105     C                   eval      oriFLO = *blanks
097000011105     C*
097100011122     C* Infine valorizzo la chiave esterna sul file FNVAO
097200011105     C                   eval      dorm01 = vaoFLO
097300011105     C                   movel     §numori       §ormpg
097400011105     C                   eval      vaoFLO = dorm01
097500011105     C*
097600060306     C* ...quindi scrivo il file TIORI00F
097700121205     C  N55              WRITE     TIORI000
097800011105     C*
097900011105     C                   ENDSR
098000050826
098100050826
098200050826
098300050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
098400050826     C     wriVGD        BEGSR
098500050826     C*
098600050826     C* Reperisco la descrizione della fase da tabella
098700050826     C                   movel(p)  'FAR'         KteCOD
098800050826     C                   movel(p)  '000'         KteKE1
098900050826     C     KEYtbe        chain     tntbe01l
099000050826     C                   if        %found(tntbe01l)
099100050826     C                   movel(P)  tbeuni        DFAR
099200050826     C                   else
099300050826     C                   clear                   DFAR
099400050826     C                   endif
099500050826     C*
099600050826     C* Valorizzo buffer record
099700050826     C                   clear                   FNVAPDS
099800050826     C                   eval      vapIDC = oriIDC
099900050826     C                   eval      vapPOE = oriPOE
100000050826     C                   eval      vapRFA = vaoRFA
100100050826     C                   eval      vapPOG = oriPOE
100200050826     C                   eval      vapDAE = oriDER
100300050826     C                   eval      vapORE = oriOER
100400050826     C                   if        d§fardva <> *blanks
100500050826     C                   eval      vapDFA = d§fardva
100600050826     C                   else
100700050826     C                   eval      vapDFA = d§fardes
100800050826     C                   endif
100900050826     C*
101000050826     C                   clear                   tivgd000
101100080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
101200050826     C                   eval      vgdTIP = 'VP'
101300060306     C                   eval      vgdKSU = vapIDC
101400050826     C                   eval      vgdTSC = 'WW'
101500050826     C                   eval      vgdDAT = datcor
101600050826     C                   eval      vgdPGM = 'TIS7T7R'
101700060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
101800060306     C                   exsr      sr_Control
101900060306     C                   if        Cli_Abil = *on
102000060306     C                   eval      vgdKSU = woriIDC
102100121205     C  N55              write     tivgd000
102200060306     C                   endif
102300050826     C*
102400050826     C                   ENDSR
102500050826     C*------------------------------------------------------------------------*
102600011119
102700010528
102800010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
102900010528     C     repfil        BEGSR
103000010528     C*
103100010528     C                   if        invfil = *zeros and
103200010528     C                             depfil > *zeros and
103300010613     C                             (vinflg = *blanks or
103400010613     C                              vinflg = *zeros)
103500010528     C
103600010528     C                   eval      invfil = depfil
103700010528     C                   endif
103800010528     C*
103900010529     C                   if        depfil <> invfil and
104000010529     C                             invfil > *zeros
104100010528     C                   eval      flgMulti = '1'
104200010528     C                   if        vinflg = *blanks
104300010528     C                   add       1             cntNonEl
104400010528     C                   endif
104500010528     C                   endif
104600010528     C*
104700010528     C                   if        vinflg = '2'
104800010528     C                   eval      flgStato = '2'
104900010528     C                   endif
105000010528     C*
105100010528     C                   ENDSR
105200010528     C***
105300010528
105400010528
105500010528
105600010528
105700990920      /TITLE Invio dei dati al punto operativo.
105800000613     C     invio         BEGSR
105900990920     C*
106000990920     C                   reset                   dscmz
106100010528     C                   move      invfil        cmzdst
106200990920     C                   eval      cmzfld = vlrfou
106300990920     C                   eval      cmzmbd = vlrhdl
106400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
106500000710     C                   if        prmfir = *blanks
106600010122     C                   eval      cmzfla = 'FNVAO00F'
106700010122     C                   eval      cmzmba = 'FNVAO00F'
106800000710     C                   else
106900000710     C                   eval      cmzfla = prmfir
107000000710     C                   eval      cmzmba = prmfir
107100000710     C                   endif
107200990920     C                   eval      cmznrr = *zeros
107300990920     C                   move      §ctrok        cmznrr
107400010123     C                   eval      cmzlba = vlrfl1
107500990920     C                   call(e)   'TIS711C'
107600990920     C                   parm                    dscmz
107700990921     C                   parm      *blanks       esito
107800990920     C                   if        %error
107900990920     C                             or cmzerr = '1'
108000990921     C                             or esito  = '1'
108100000710     C                   eval      wrkesito = '3'
108200990920     C                   endif
108300990920     C*
108400000613     C                   ENDSR
108500000613     C***
108600990910
108700010528
108800010528
108900010528
109000010528      /TITLE Invio dei dati al punto operativo.
109100010528     C     opeini        BEGSR
109200010528     C*
109300010528     C* Inizializzo flag e contatori operativi
109400010528     C                   movel     '0'           flgGiro           1
109500010528     C                   movel     '0'           flgMulti          1
109600010528     C                   movel     '1'           flgStato          1
109700010615     C                   movel     '0'           flgOk             1
109800010528     C                   z-add     *zeros        cntNonEl         10 0
109900010528     C                   z-add     *zeros        depfil            3 0
110000010528     C                   z-add     *zeros        invfil            3 0
110100030407     C*
110200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
110300030407     C                   if        %subst(vlrppt:1:1) = 'S'
110400030407     C                   seton                                        50
110500030407     C                   else
110600030407     C                   setoff                                       50
110700030407     C                   endif
110800070925     C*
110900070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
111000070925     C                   if        %subst(vlrppt:2:1) = 'S'
111100070925     C                   seton                                        51
111200070925     C                   else
111300070925     C                   setoff                                       51
111400070925     C                   endif
111500121205     C*
111600121205     C* se il 4° char dei parametri è T=Test
111700121205     C                   SETOFF                                       55
111800121205     C                   IF        %subst(vlrppt:4:1) = 'T'
111900121205     C* inibisco la scrittura dei file TIORI e TIVGD
112000121205     C                   SETON                                        55
112100121205     C* imposto il PO di emissione come filiale fittizia
112200121205     C                   EVAL      VAOPOE = 999
112300121205     C                   ENDIF
112400010528     C*
112500010528     C                   ENDSR
112600010528     C***
112700060306
112800060306
112900060306     C*-------------------------------------------------------------------------*
113000060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
113100060306     C*-------------------------------------------------------------------------*
113200060306     C     sr_Control    begsr
113300060306     C*
113400060306     C* inizializzo a ok il flag d controllo abilitazione
113500060306     C                   eval      Cli_Abil = *on
113600060306     C
113700060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
113800060306     C                   clear                   woriIDC
113900060306     C                   eval      kteCOD = 'VAP'
114000060306     C                   eval      kteKE1 = oriIDC
114100060306     C     keytbe        chain     TNTBE01L
114200060306     C                   if        %found(Tntbe01l)
114300060306     C                   eval      woriIDC = TbeKe2
114400060306     C                   else
114500060306     C                   eval      Cli_Abil = *off
114600060306     C                   endif
114700060306     C*
114800060306     C                   endsr
114900010528
115000010528
115100010528
115200010528
115300000613     C     *inzsr        BEGSR
115400990910     C*
115500990910     C     *entry        plist
115600990920     C                   parm                    tivlrds
115700990921     C                   parm      wrkesito      esito
115800000724     C                   parm                    prmlit
115900000710     C                   parm                    prmfir
116000011105     C*
116100011105     C* Campi riferito al database
116200011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
116300011105     C     *like         define    tbeke1        kteKE1
116400011105     C*
116500011105     C* Definizione chiavi di lettura
116600011105     C* TNTBE01L - parziale
116700011105     C     keytbe        klist
116800011105     C                   kfld                    kteCOD                         *UTENTE
116900011105     C                   kfld                    kteKE1                         *CODICE
117000070926     C* TIVSS02L - parziale
117100070926     C     keyvss02_p    klist
117200070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
117300070926     C                   kfld                    vssISV                         *SERVIZIO
117400011105     C*
117500011105     C* Reperimento data e ora corrente
117600011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
117700011105     C                   movel     wn14          wn6               6 0          *ORA (6)
117800011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
117900011105     C                   z-add     wn8           g08dat
118000011105     C                   z-add     *zeros        g08inv
118100011105     C                   movel     '0'           g08err
118200011105     C                   call      'XSRDA8'
118300011105     C                   parm                    wlbda8
118400011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
118500010529     C*
118600000613     C                   ENDSR
118700000613     C***
