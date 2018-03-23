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
002100150209     D  WI07                   1      6
002200150209     D  WD07                   7      7
002300150209     D  WASC07                 1      7
002400941229     D*  2+3
002500941229     D                 DS
002600150209     D  WI05                   1      2
002700150209     D  WD05                   3      5
002800150209     D  WASC05                 1      5
002900010122     D*
003000990920     D tivlrds       e ds                  extname(tivlr00f)
003100050826     D fnvapds       e ds                  extname(fnvap00f)
003200010613     D fnvaoaer      e ds                  inz
003300010122     D                                     prefix(a_)
003400141209     D fnvaoeaer     e ds                  inz
003500141209     D                                     prefix(w_)
003600031014     D tisi95ds      e ds
003700011112     D dorm01        e ds
003800011119     D dfar          e ds
003900090617     d trul33ds      e ds
004000090617     d kpjba         e ds
004100990910     D esito           s              1
004200000724     D prmlit          s             10
004300000710     D prmfir          s             10
004400990921     D wrkesito        s                   like(esito)
004500000613     D rrnum           s              6  0 INZ(*zeros)
004600060306     D Cli_Abil        s              1    inz(*off)
004700060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004800061031     D ConstINO        s             15    inz('ORM DA INTERNET')
004900090617     d §numori         s              7  0 inz
005000011112     D*-------------------
005100011112     D* DS "XSRDA8" - controllo data (8)
005200011112     D*-------------------
005300011112     D wlbda8          DS                  INZ
005400011112     D  g08dat                 1      8  0
005500011112     D  g08inv                 9     16  0
005600011112     D  g08err                17     17
005700011112     D  g08tgi                18     22  0
005800011122     D*-------------------
005900010731     D* COSTANTI
006000011122     D*-------------------
006100010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006200010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006300990908
006400091026
006500091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006600091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006700091026
006800010528
006900010528
007000010528
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000724     C*
007500010528     C                   exsr      opeini
007600010528     C                   exsr      rwvao
007700010528     C*
007800010528     C                   seton                                        lr
007900010528
008000010528
008100010528
008200010528
008300010528     C*--------------------------------------------------------
008400010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010528     C*--------------------------------------------------------
008600010528     C     PREELA        BEGSR
008700010528     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900010528     C                   if        invfil <> *zeros and
009000010528     C                             flgGiro = '0'
009100010528     C*
009200010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010528     C                   eval      flgGiro = '1'
009400010529     C*
009500010529     C                   endif
009600010528     C*
009700010528     C                   ENDSR
009800010528     C***
009900010528
010000010528
010100010528
010200010528
010300010528
010400010528
010500010528     C*--------------------------------------------------------
010600010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010528     C*--------------------------------------------------------
010800010528     C     ENDELA        BEGSR
010900000613     C*
011000010528     C                   ENDSR
011100010528     C***
011200990908
011300010528
011400010528
011500010528
011600010528
011700910830     C*--------------------------------------------------------
011800010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
011900910830     C*--------------------------------------------------------
012000010122     C     RWVAO         BEGSR
012100010528     C*
012200990914     C                   if        not %open(tivin00r)
012300990908     C                   open      tivin00r
012400990914     C                   endif
012500010122     C                   if        not %open(fnvaowwr)
012600010122     C                   open      fnvaowwr
012700990914     C                   endif
012800141209     C*
012900141209     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
013000141209     C                   call      'TIS7T7ER2'
013100141209     C                   parm      'O'           pIn_Opz           1
013200141209     C                   parm                    tivlrds
013300141209     C                   parm                    FNVAOEAER
013400141209     C                   parm                    oriPRG
013500141209     C                   parm      *zeros        pIn_Dst           3
013600141209     C                   parm      *blanks       pOut_Esito        1
013700141209     C                   if        pOut_Esito = '2'
013800141209     C                   goto      FINE
013900141209     C                   endif
014000990910     C*
014100990910     C                   clear                   §CTROK
014200990910     C                   clear                   §CTRMO
014300990910     C                   clear                   §CTRNO
014400990910     C*
014500921023     C                   DO        *HIVAL
014600990913     C*
014700990915     C                   READ      tivin00r                               70
014800010731     C*
014900010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015000010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
015100010618     C*
015200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015300010618     C                   if        vinflg = '1'
015400010618     C                   eval      flgOk = '1'
015500010618     C                   endif
015600010618     C*
015700000905     C                   if        vindta > *blanks
015800000613     C                   add       1             rrnum
015900990913     C*
016000010711     C                   if        *in70 = *off and
016100010711     C                             (vinflg = *blanks
016200010711     C                              or vinflg = '0'
016300010711     C                              or vinflg = '2')
016400990913     C*
016500010528     C                   clear                   FNVAO000
016600011112     C                   clear                   TIORI000
016700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016800010711     C                   if        vinflg = *blanks or vinflg = '0'
016900010711     C                   clear                   vinmsg
017000010711     C                   endif
017100010711     C*
017200010613     C                   eval      fnvaoaer = vindta
017300151104     C*
017400151104     C* Stacco subito un numeratore univoco "ORM"
017500151104     C                   exsr      RTVNUM
017600141209     C*
017700141209     C* Verifico se trattasi di formato ORM "esteso"
017800141209     C                   setoff                                       53
017900150209     C                   if        %subst(vindta:684+1:%size(fnvaoeaer)-684+1)
018000141209     C                             > *blanks
018100141209     C                   eval      fnvaoeaer = vindta
018200141209     C                   seton                                        53
018300141209     C                   endif
018400991022     C*
018500010122     C                   testb     x'3F'         a_vaoatb                 02
018600991022     C                   if        not *in02
018700991022     C*
018800971021     C                   SETOFF                                       303132
018900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019100971023     C*    OPPURE TUTTI BLANKS
019200010122     C*-------------------
019300010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
019400010122     C* IN32 => Campo facoltativo, muovo tutti zero e passo cimunque il record marcandolo come OK
019500010122     C*-------------------
019600010122     C*
019700010417 xxx C*                  TESTN                   a_vaopoe             30
019800010417  "  C* N30              TESTN                   a_vaopoe               30
019900010417  "  C* N30              TESTN                   a_vaopoe                 30
020000010417     C*                  MOVE      a_vaopoe      W001A             1
020100010417     C*    *IN30         IFEQ      *OFF
020200010417     C*    W001A         ORLT      '0'
020300010417     C*                  SETON                                        32
020400010417     C*                  MOVE      *ZEROS        VAOPOE
020500010417     C*                  EVAL      vinmsg = 'VAOPOE'
020600010417     C*                  ELSE
020700010417     C*                  MOVE      a_vaopoe      VAOPOE
020800010417     C*                  ENDIF
020900010417  "  C*
021000010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
021100010604 xxx C                   IF        vlrpoi <> 999
021200010528     C                   EVAL      VAOPOE = vlrpoi
021300010528     C                   ELSE
021400010528     C                   IF        a_vaopoe = *blanks or
021500011010     C                             a_vaopoe = *zeros  or
021600011010     C                             a_vaopoe = ' 000'
021700010528     C                   IF        a_vaopor <> *blanks and
021800011010     C                             a_vaopor <> *zeros  and
021900011010     C                             a_vaopor <> ' 000'
022000010528     C                   MOVE      a_vaopor      VAOPOE
022100010528     C                   ELSE
022200011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
022300010528     C                   MOVE      a_vaopoe      VAOPOE
022400010528     C                   ENDIF
022500010529     C                   ELSE
022600010529     C                   MOVE      a_vaopoe      VAOPOE
022700010528     C                   ENDIF
022800010528     C                   ENDIF
022900010122     C*
023000010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023100010528     C                   eval      depfil = VAOPOE
023200010528     C                   exsr      repfil
023300010528     C                   if        depfil = invfil and
023400010528     C                             (vinflg = *blanks
023500010528     C                              or vinflg = '0'
023600010528     C                              or vinflg = '2')
023700010528     C*
023800010528     C                   exsr      PREELA
023900010528     C*
024000010719     C*                  TESTN                   a_vaonsr             30
024100010719     C* N30              TESTN                   a_vaonsr               30
024200010719     C* N30              TESTN                   a_vaonsr                 30
024300010719     C*                  MOVE      a_vaonsr      W001A             1
024400010719     C*    *IN30         IFEQ      *OFF
024500010719     C*    W001A         ORLT      '0'
024600010719     C*                  SETON                                        32
024700010719     C*                  MOVE      *ZEROS        VAONSR
024800010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
024900010719     C*                  ELSE
025000010719     C*                  MOVE      a_vaonsr      VAONSR
025100010719     C*                  ENDIF
025200010122     C*
025300010719     C*                  TESTN                   a_vaonor             30
025400010719     C* N30              TESTN                   a_vaonor               30
025500010719     C* N30              TESTN                   a_vaonor                 30
025600010719     C*                  MOVE      a_vaonor      W001A             1
025700010719     C*    *IN30         IFEQ      *OFF
025800010719     C*    W001A         ORLT      '0'
025900010719     C*                  SETON                                        32
026000010719     C*                  MOVE      *ZEROS        VAONOR
026100010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
026200010719     C*                  ELSE
026300010719     C*                  MOVE      a_vaonor      VAONOR
026400010719     C*                  ENDIF
026500010122     C*
026600010719     C*                  TESTN                   a_vaonrv             30
026700010719     C* N30              TESTN                   a_vaonrv               30
026800010719     C* N30              TESTN                   a_vaonrv                 30
026900010719     C*                  MOVE      a_vaonrv      W001A             1
027000010719     C*    *IN30         IFEQ      *OFF
027100010719     C*    W001A         ORLT      '0'
027200010719     C*                  SETON                                        32
027300010719     C*                  MOVE      *ZEROS        VAONRV
027400010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
027500010719     C*                  ELSE
027600010719     C*                  MOVE      a_vaonrv      VAONRV
027700010719     C*                  ENDIF
027800010122     C*
027900010122     C                   TESTN                   a_vaodao             30
028000010122     C  N30              TESTN                   a_vaodao               30
028100010122     C  N30              TESTN                   a_vaodao                 30
028200010122     C                   MOVE      a_vaodao      W001A             1
028300010122     C     *IN30         IFEQ      *OFF
028400010122     C     W001A         ORLT      '0'
028500010122     C                   SETON                                        32
028600010122     C                   MOVE      *ZEROS        VAODAO
028700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
028800010122     C                   ELSE
028900010122     C                   MOVE      a_vaodao      VAODAO
029000010122     C                   ENDIF
029100010122     C*
029200010122     C                   TESTN                   a_vaooao             30
029300010122     C  N30              TESTN                   a_vaooao               30
029400010122     C  N30              TESTN                   a_vaooao                 30
029500010122     C                   MOVE      a_vaooao      W001A             1
029600010122     C     *IN30         IFEQ      *OFF
029700010122     C     W001A         ORLT      '0'
029800010122     C                   SETON                                        32
029900010122     C                   MOVE      *ZEROS        VAOOAO
030000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
030100010122     C                   ELSE
030200010122     C                   MOVE      a_vaooao      VAOOAO
030300010122     C                   ENDIF
030400010122     C*
030500010122     C                   TESTN                   a_vaocor             30
030600010122     C  N30              TESTN                   a_vaocor               30
030700010122     C  N30              TESTN                   a_vaocor                 30
030800010122     C                   MOVE      a_vaocor      W001A             1
030900010122     C     *IN30         IFEQ      *OFF
031000010122     C     W001A         ORLT      '0'
031100010122     C                   SETON                                        32
031200010122     C                   MOVE      *ZEROS        VAOCOR
031300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
031400010122     C                   ELSE
031500010122     C                   MOVE      a_vaocor      VAOCOR
031600010122     C                   ENDIF
031700010122     C*
031800010122     C                   TESTN                   a_vaocra             30
031900010122     C  N30              TESTN                   a_vaocra               30
032000010122     C  N30              TESTN                   a_vaocra                 30
032100010122     C                   MOVE      a_vaocra      W001A             1
032200010122     C     *IN30         IFEQ      *OFF
032300010122     C     W001A         ORLT      '0'
032400010122     C                   SETON                                        32
032500010122     C                   MOVE      *ZEROS        VAOCRA
032600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
032700010122     C                   ELSE
032800010122     C                   MOVE      a_vaocra      VAOCRA
032900010122     C                   ENDIF
033000010122     C*
033100010719     C                   IF        a_vaocrc = *blanks or
033200010719     C                             a_vaocrc = *zeros
033300010719     C                   ELSE
033400010122     C                   TESTN                   a_vaocrc             30
033500010122     C  N30              TESTN                   a_vaocrc               30
033600010122     C  N30              TESTN                   a_vaocrc                 30
033700010122     C                   MOVE      a_vaocrc      W001A             1
033800010122     C     *IN30         IFEQ      *OFF
033900010122     C     W001A         ORLT      '0'
034000010122     C                   SETON                                        32
034100010122     C                   MOVE      *ZEROS        VAOCRC
034200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
034300010122     C                   ELSE
034400010122     C                   MOVE      a_vaocrc      VAOCRC
034500010122     C                   ENDIF
034600010719     C                   ENDIF
034700010122     C*
034800010122     C                   TESTN                   a_vaodar             30
034900010122     C  N30              TESTN                   a_vaodar               30
035000010122     C  N30              TESTN                   a_vaodar                 30
035100010122     C                   MOVE      a_vaodar      W001A             1
035200010122     C     *IN30         IFEQ      *OFF
035300010122     C     W001A         ORLT      '0'
035400010122     C                   SETON                                        32
035500010122     C                   MOVE      *ZEROS        VAODAR
035600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
035700010122     C                   ELSE
035800010122     C                   MOVE      a_vaodar      VAODAR
035900010122     C                   ENDIF
036000010122     C*
036100010122     C                   TESTN                   a_vaoorr             30
036200010122     C  N30              TESTN                   a_vaoorr               30
036300010122     C  N30              TESTN                   a_vaoorr                 30
036400010122     C                   MOVE      a_vaoorr      W001A             1
036500010122     C     *IN30         IFEQ      *OFF
036600010122     C     W001A         ORLT      '0'
036700010122     C                   SETON                                        32
036800010122     C                   MOVE      *ZEROS        VAOORR
036900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
037000010122     C                   ELSE
037100010122     C                   MOVE      a_vaoorr      VAOORR
037200010122     C                   ENDIF
037300010122     C*
037400010122     C                   TESTN                   a_vaoncl             30
037500010122     C  N30              TESTN                   a_vaoncl               30
037600010122     C  N30              TESTN                   a_vaoncl                 30
037700010122     C                   MOVE      a_vaoncl      W001A             1
037800010122     C     *IN30         IFEQ      *OFF
037900010122     C     W001A         ORLT      '0'
038000010122     C                   SETON                                        32
038100010122     C                   MOVE      *ZEROS        VAONCL
038200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
038300010122     C                   ELSE
038400010122     C                   MOVE      a_vaoncl      VAONCL
038500010122     C                   ENDIF
038600010122     C*
038700010122     C                   MOVE      *ZERO         VAOPKG
038800150209     C                   MOVE      a_vaopkg      WASC07
038900150209     C                   TESTN                   WI07                 30
039000150209     C  N30              TESTN                   WI07                   30
039100150209     C  N30              TESTN                   WI07                     30
039200150209     C                   MOVE      WI07          W001A             1
039300010122     C     *IN30         IFEQ      *OFF
039400010122     C     W001A         ORLT      '0'
039500010122     C     W001A         ANDNE     *BLANK
039600010122     C                   SETON                                        32
039700010122     C                   MOVE      *ZERO         VAOPKG
039800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
039900010122     C                   ELSE
040000150209     C                   MOVEL     WI07          VAOPKG
040100150209     C                   TESTN                   WD07                 30
040200150209     C  N30              TESTN                   WD07                     30
040300150209     C                   MOVE      WD07          W001A             1
040400010122     C     *IN30         IFEQ      *OFF
040500010122     C     W001A         ORLT      '0'
040600010122     C     W001A         ANDNE     *BLANK
040700010122     C                   SETON                                        32
040800010122     C                   MOVE      *ZERO         VAOPKG
040900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
041000010122     C                   ELSE
041100150209     C                   MOVE      WD07          VAOPKG
041200010122     C                   ENDIF
041300050118     C                   ENDIF
041400010122     C*
041500010122     C                   MOVE      *ZERO         VAOVLM
041600150209     C                   MOVE      a_vaovlm      WASC05
041700150209     C                   TESTN                   WI05                 30
041800150209     C  N30              TESTN                   WI05                   30
041900150209     C  N30              TESTN                   WI05                     30
042000150209     C                   MOVE      WI05          W001A             1
042100010122     C     *IN30         IFEQ      *OFF
042200010122     C     W001A         ORLT      '0'
042300010122     C     W001A         ANDNE     *BLANK
042400010122     C                   SETON                                        32
042500010122     C                   MOVE      *ZERO         VAOVLM
042600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
042700010122     C                   ELSE
042800150209     C                   MOVEL     WI05          VAOVLM
042900150209     C                   TESTN                   WD05                 30
043000150209     C  N30              TESTN                   WD05                   30
043100150209     C  N30              TESTN                   WD05                     30
043200150209     C                   MOVE      WD05          W001A             1
043300010122     C     *IN30         IFEQ      *OFF
043400010122     C     W001A         ORLT      '0'
043500010122     C     W001A         ANDNE     *BLANK
043600010122     C                   SETON                                        32
043700010122     C                   MOVE      *ZERO         VAOVLM
043800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
043900010122     C                   ELSE
044000150209     C                   MOVE      WD05          VAOVLM
044100010122     C                   ENDIF
044200050118     C                   ENDIF
044300010122     C*
044400010122     C                   TESTN                   a_vaobnc             30
044500010122     C  N30              TESTN                   a_vaobnc               30
044600010122     C  N30              TESTN                   a_vaobnc                 30
044700010122     C                   MOVE      a_vaobnc      W001A             1
044800010122     C     *IN30         IFEQ      *OFF
044900010122     C     W001A         ORLT      '0'
045000010122     C                   SETON                                        32
045100010122     C                   MOVE      *ZEROS        VAOBNC
045200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
045300010122     C                   ELSE
045400010122     C                   MOVE      a_vaobnc      VAOBNC
045500010122     C                   ENDIF
045600010122     C*
045700010122     C                   TESTN                   a_vaoblc             30
045800010122     C  N30              TESTN                   a_vaoblc               30
045900010122     C  N30              TESTN                   a_vaoblc                 30
046000010122     C                   MOVE      a_vaoblc      W001A             1
046100010122     C     *IN30         IFEQ      *OFF
046200010122     C     W001A         ORLT      '0'
046300010122     C                   SETON                                        32
046400010122     C                   MOVE      *ZEROS        VAOBLC
046500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
046600010122     C                   ELSE
046700010122     C                   MOVE      a_vaoblc      VAOBLC
046800010122     C                   ENDIF
046900010122     C*
047000010122     C                   TESTN                   a_vaoatt             30
047100010122     C  N30              TESTN                   a_vaoatt               30
047200010122     C  N30              TESTN                   a_vaoatt                 30
047300010122     C                   MOVE      a_vaoatt      W001A             1
047400010122     C     *IN30         IFEQ      *OFF
047500010122     C     W001A         ORLT      '0'
047600010122     C                   SETON                                        32
047700010122     C                   MOVE      *ZEROS        VAOATT
047800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
047900010122     C                   ELSE
048000010122     C                   MOVE      a_vaoatt      VAOATT
048100010122     C                   ENDIF
048200010122     C*
048300010122     C                   TESTN                   a_vaomtc             30
048400010122     C  N30              TESTN                   a_vaomtc               30
048500010122     C  N30              TESTN                   a_vaomtc                 30
048600010122     C                   MOVE      a_vaomtc      W001A             1
048700010122     C     *IN30         IFEQ      *OFF
048800010122     C     W001A         ORLT      '0'
048900010122     C                   SETON                                        32
049000010122     C                   MOVE      *ZEROS        VAOMTC
049100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
049200010122     C                   ELSE
049300010122     C                   MOVE      a_vaomtc      VAOMTC
049400010122     C                   ENDIF
049500010122     C*
049600010719     C                   IF        a_vaoksc = *blanks or
049700010719     C                             a_vaoksc = *zeros
049800010719     C                   ELSE
049900010122     C                   TESTN                   a_vaoksc             30
050000010122     C  N30              TESTN                   a_vaoksc               30
050100010122     C  N30              TESTN                   a_vaoksc                 30
050200010122     C                   MOVE      a_vaoksc      W001A             1
050300010122     C     *IN30         IFEQ      *OFF
050400010122     C     W001A         ORLT      '0'
050500010122     C                   SETON                                        32
050600010122     C                   MOVE      *ZEROS        VAOKSC
050700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
050800010122     C                   ELSE
050900010122     C                   MOVE      a_vaoksc      VAOKSC
051000010122     C                   ENDIF
051100010719     C                   ENDIF
051200010122     C*
051300010710     C                   IF        a_vaoctr > *blanks
051400010122     C                   TESTN                   a_vaoctr             30
051500010122     C  N30              TESTN                   a_vaoctr               30
051600010122     C  N30              TESTN                   a_vaoctr                 30
051700010122     C                   MOVE      a_vaoctr      W001A             1
051800010122     C     *IN30         IFEQ      *OFF
051900010122     C     W001A         ORLT      '0'
052000010122     C                   SETON                                        32
052100010627     C                   MOVE      *blanks       VAOCTR
052200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
052300010122     C                   ELSE
052400010122     C                   MOVE      a_vaoctr      VAOCTR
052500010122     C                   ENDIF
052600010710     C                   ENDIF
052700010122     C*
052800010122     C                   TESTN                   a_vaopor             30
052900010122     C  N30              TESTN                   a_vaopor               30
053000010122     C  N30              TESTN                   a_vaopor                 30
053100010122     C                   MOVE      a_vaopor      W001A             1
053200010122     C     *IN30         IFEQ      *OFF
053300010122     C     W001A         ORLT      '0'
053400010122     C                   SETON                                        32
053500010122     C                   MOVE      *ZEROS        VAOPOR
053600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
053700010122     C                   ELSE
053800010122     C                   MOVE      a_vaopor      VAOPOR
053900010122     C                   ENDIF
054000010122     C*
054100010719     C                   IF        a_vaopoc = *blanks or
054200010719     C                             a_vaopoc = *zeros
054300010719     C                   ELSE
054400010122     C                   TESTN                   a_vaopoc             30
054500010122     C  N30              TESTN                   a_vaopoc               30
054600010122     C  N30              TESTN                   a_vaopoc                 30
054700010122     C                   MOVE      a_vaopoc      W001A             1
054800010122     C     *IN30         IFEQ      *OFF
054900010122     C     W001A         ORLT      '0'
055000010122     C                   SETON                                        32
055100010122     C                   MOVE      *ZEROS        VAOPOC
055200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
055300010122     C                   ELSE
055400010122     C                   MOVE      a_vaopoc      VAOPOC
055500010122     C                   ENDIF
055600010719     C                   ENDIF
055700010122     C*
055800010719     C                   IF        a_vaosto = *blanks or
055900010719     C                             a_vaosto = *zeros
056000010719     C                   ELSE
056100010122     C                   TESTN                   a_vaosto             30
056200010122     C  N30              TESTN                   a_vaosto               30
056300010122     C  N30              TESTN                   a_vaosto                 30
056400010122     C                   MOVE      a_vaosto      W001A             1
056500010122     C     *IN30         IFEQ      *OFF
056600010122     C     W001A         ORLT      '0'
056700010122     C                   SETON                                        32
056800010122     C                   MOVE      *ZEROS        VAOSTO
056900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
057000010122     C                   ELSE
057100010122     C                   MOVE      a_vaosto      VAOSTO
057200010122     C                   ENDIF
057300010719     C                   ENDIF
057400971021     C*
057500971021     C* CAMPI ALFABETICI:
057600010122     C                   MOVE      a_vaoatb      VAOATB
057700010122     C                   MOVE      a_vaotor      VAOTOR
057800010531     C*
057900010531     C                   IF        a_vaotco = *blanks
058000010531     C                   MOVE      'F'           VAOTCO
058100010531     C                   ELSE
058200010531     C                   MOVE      a_vaotco      VAOTCO
058300010531     C                   ENDIF
058400010531     C*
058500010122     C                   MOVE      a_vaorso      VAORSO
058600010122     C                   MOVE      a_vaoino      VAOINO
058700010122     C                   MOVE      a_vaocao      VAOCAO
058800010122     C                   MOVE      a_vaoloo      VAOLOO
058900010122     C                   MOVE      a_vaopro      VAOPRO
059000090129     C                   MOVE      a_vaonao      VAONAO
059100031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
059200031014     C                   IF        VAOPRO  = *blanks AND
059300050907     C                             VAOCAO <> *blanks AND
059400050907     C                             VAONAO  = *blanks                            * solo x Italia
059500031014     C                   CLEAR                   TISI95DS
059600031014     C                   EVAL      I95TCN = '3'
059700031014     C                   Z-ADD     datcor        I95DAT
059800031014     C                   EVAL      I95CAP = VAOCAO
059900031014     C                   EVAL      I95LOC = VAOLOO
060000031014     C                   CALL      'TISI95R'
060100031014     C                   PARM                    TISI95DS
060200031014     C                   EVAL      VAOPRO = O95PRV
060300031014     C                   ENDIF
060400031014     C*
060500010122     C                   MOVE      a_vaorsr      VAORSR
060600010122     C                   MOVE      a_vaoinr      VAOINR
060700010122     C                   MOVE      a_vaocar      VAOCAR
060800010122     C                   MOVE      a_vaolor      VAOLOR
060900010122     C                   MOVE      a_vaoprr      VAOPRR
061000090129     C                   MOVE      a_vaonar      VAONAR
061100031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
061200031014     C                   IF        VAOPRR  = *blanks AND
061300050907     C                             VAOCAR <> *blanks AND
061400050907     C                             VAONAR  = *blanks                            * solo x Italia
061500031014     C                   CLEAR                   TISI95DS
061600031014     C                   EVAL      I95TCN = '3'
061700031014     C                   Z-ADD     datcor        I95DAT
061800031014     C                   EVAL      I95CAP = VAOCAR
061900031014     C                   EVAL      I95LOC = VAOLOR
062000031014     C                   CALL      'TISI95R'
062100031014     C                   PARM                    TISI95DS
062200031014     C                   EVAL      VAOPRR = O95PRV
062300031014     C                   ENDIF
062400031014     C*
062500010122     C                   MOVE      a_vaorer      VAORER
062600010122     C                   MOVE      a_vaoter      VAOTER
062700010122     C                   MOVE      a_vaorsc      VAORSC
062800010122     C                   MOVE      a_vaoinc      VAOINC
062900010122     C                   MOVE      a_vaocac      VAOCAC
063000010122     C                   MOVE      a_vaoloc      VAOLOC
063100010122     C                   MOVE      a_vaoprc      VAOPRC
063200090129     C                   MOVE      a_vaonac      VAONAC
063300031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
063400031014     C                   IF        VAOPRC  = *blanks AND
063500050907     C                             VAOCAC <> *blanks AND
063600050907     C                             VAONAC  = *blanks                            * solo x Italia
063700031014     C                   CLEAR                   TISI95DS
063800031014     C                   EVAL      I95TCN = '3'
063900031014     C                   Z-ADD     datcor        I95DAT
064000031014     C                   EVAL      I95CAP = VAOCAC
064100031014     C                   EVAL      I95LOC = VAOLOC
064200031014     C                   CALL      'TISI95R'
064300031014     C                   PARM                    TISI95DS
064400031014     C                   EVAL      VAOPRC = O95PRV
064500031014     C                   ENDIF
064600031014     C*
064700010122     C                   MOVE      a_vaoffd      VAOFFD
064800010122     C                   MOVE      a_vaormp      VAORMP
064900010122     C                   MOVE      a_vaonam      VAONAM
065000010122     C                   MOVE      a_vaopag      VAOPAG
065100010122     C                   MOVE      a_vaozor      VAOZOR
065200010122     C                   MOVE      a_vaono1      VAONO1
065300010122     C                   MOVE      a_vaono2      VAONO2
065400010122     C                   MOVE      a_vaoddt      VAODDT
065500010122     C                   MOVE      a_vaorfa      VAORFA
065600010122     C                   MOVE      a_vaospi      VAOSPI
065700010122     C                   MOVE      a_vaoflo      VAOFLO
065800050304     C*
065900050304     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
066000050304     C*** VAONAO
066100050304     C                   IF        %trim(VAONAO) = 'I' OR
066200050304     C                             %trim(VAONAO) = 'IT'
066300050304     C                   EVAL      VAONAO = *blanks
066400050304     C                   ENDIF
066500050304     C*** VAONAR
066600050304     C                   IF        %trim(VAONAR) = 'I' OR
066700050304     C                             %trim(VAONAR) = 'IT'
066800050304     C                   EVAL      VAONAR = *blanks
066900050304     C                   ENDIF
067000050304     C*** VAONAC
067100050304     C                   IF        %trim(VAONAC) = 'I' OR
067200050304     C                             %trim(VAONAC) = 'IT'
067300050304     C                   EVAL      VAONAC = *blanks
067400050304     C                   ENDIF
067500050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
067600050304     C                   IF        VAOPKG =  *zeros AND
067700050304     C                             VAOVLM =  *zeros AND
067800050304     C                             VAOBNC =  *zeros AND
067900050304     C                             VAONCL <> *zeros
068000050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
068100050304     C                   ENDIF
068200030603     C*
068300030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
068400030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
068500030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
068600050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
068700150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
068800930409     C*
068900010604     C  N31              ADD       1             §CTROK            7 0
069000010604     C   32              ADD       1             §CTRMO            7 0
069100010604     C   31              ADD       1             §CTRNO            7 0
069200030624     C  N31              EXSR      CHKCOR
069300011112     C  N31              EXSR      WRIORI
069400050826     C  N31              EXSR      WRIVGD
069500050421     C*
069600050421     C***
069700050421     C* FORZATURA SU DATA/ORA => NN CONSIDERO QUELLE PASSATE SUL FILE DAL CLIENTE MA SEMPRE
069800050421     C* DATA/ORA DI TRADUZIONE (OVVERO VERA IMMISSIONE DEL DATO SEPPUR DA CONFERMARE SUL
069900050421     C* NS. GESTIONALE). QUESTO SOLO X LA SCRITTURA DEL VAO!!!
070000050421     C                   eval      VAODAO = datcor
070100050421     C                   eval      VAOOAO = wn6
070200050421     C***
070300010122     C  N31              WRITE     FNVAO000
070400141209     C*
070500141209     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
070600141209     C                   if        *in53 and not *in31
070700151104     C                   exsr      EXEFORZ
070800141209     C                   call      'TIS7T7ER2'
070900141209     C                   parm      'W'           pIn_Opz           1
071000141209     C                   parm                    tivlrds
071100141209     C                   parm                    FNVAOEAER
071200141209     C                   parm                    oriPRG
071300141209     C                   parm      *zeros        pIn_Dst           3
071400141209     C                   parm      *blanks       pOut_Esito        1
071500141209     C                   endif
071600141209     C                   if        pOut_Esito = '2'
071700141209     C                   seton                                        32
071800141209     C                   endif
071900990910     C*
072000010604     C                   if        *in31 = *off and
072100010604     C                             *in32 = *off
072200990910     C                   eval      vinflg = '1'
072300990910     C                   else
072400990910     C                   eval      vinflg = '2'
072500990910     C                   endif
072600991022     C                   endif
072700990910     C                   endif
072800010604     C*
072900010604     C                   endif
073000000905     C*
073100000905     C                   else
073200000905     C                   eval      vinflg = '1'
073300000905     C                   endif
073400000905     C*
073500000905     C  N70              update    tivin000
073600991022     C*
073700991022     C  N70              ENDdo
073800141209     C*
073900141209     C     FINE          tag
074000141209     C*
074100141209     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
074200141209     C                   call      'TIS7T7ER2'
074300141209     C                   parm      'C'           pIn_Opz           1
074400141209     C                   parm                    tivlrds
074500141209     C                   parm                    FNVAOEAER
074600141209     C                   parm                    oriPRG
074700141209     C                   parm      *zeros        pIn_Dst           3
074800141209     C                   parm      *blanks       pOut_Esito        1
074900010528     C*
075000010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075100010528     C                   if        cntNonEl = *zeros or
075200010528     C                             flgMulti = '0'
075300010528     C* Se non ci sono record con errori ...
075400010528     C                   if        §ctrno = 0 and
075500010604     C                             §ctrmo = 0 and
075600010528     C                             flgStato <> '2'
075700990910     C* ... restituisco esito OK.
075800990921     C                   eval      wrkesito = '0'
075900990910     C                   else
076000000710     C                   if        §ctrok > 0
076100990921     C                   eval      wrkesito = '1'
076200000710     C                   else
076300010615     C                   if        flgOk = '0'
076400010615     C                   eval      wrkesito = '2'
076500010615     C                   else
076600010615     C                   eval      wrkesito = '6'
076700010615     C                   endif
076800990910     C                   endif
076900010528     C                   endif
077000010529     C                   else
077100010529     C                   eval      wrkesito = '9'
077200000710     C                   endif
077300990910     C*
077400990914     C                   if        %open(tivin00r)
077500990908     C                   close     tivin00r
077600990914     C                   endif
077700010122     C                   if        %open(fnvaowwr)
077800010122     C                   close     fnvaowwr
077900990914     C                   endif
078000990910     C*
078100010528     C                   if        vlrpoi <> 999
078200010528     C                   eval      invfil = vlrpoi
078300010528     C                   endif
078400010528     C*
078500990920     C                   if        §ctrok > 0
078600010528     C                             and invfil > *zeros
078700000613     C                   exsr      invio
078800141209     C*
078900141209     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
079000141209     C                   move      invfil        cmzdst
079100141209     C                   call      'TIS7T7ER2'
079200141209     C                   parm      'S'           pIn_Opz           1
079300141209     C                   parm                    tivlrds
079400141209     C                   parm                    FNVAOEAER
079500141209     C                   parm                    oriPRG
079600141209     C                   parm      cmzdst        pIn_Dst           3
079700141209     C                   parm      *blanks       pOut_Esito        1
079800990920     C                   endif
079900010612     C*
080000010612     C                   if        flgGiro = '1'
080100010612     C                   exsr      endela
080200010612     C                   endif
080300010528     C*
080400910830     C                   ENDSR
080500000613     C***
080600151104
080700151104
080800151104     C*------------------------------------------------------------------------*
080900151104     C* RTVNUM - Stacco un numeratore univoco "ORM"
081000151104     C*------------------------------------------------------------------------*
081100151104     C     RTVNUM        BEGSR
081200151104     C*
081300151104     C* Stacco un numeratore da AZNUM
081400151104     C                   clear                   §numori
081500151104     C                   clear                   trul33ds
081600151104     C                   eval      I33OPE = *zeros
081700151104     C                   eval      I33CNU = 600
081800151104     C                   eval      I33NUM = 1
081900151104     C                   movel     TRUL33DS      KPJBU
082000151104     C                   call      'TRUL33R'
082100151104     C                   parm                    KPJBA
082200151104     C                   movel     KPJBU         TRUL33DS
082300151104     C                   if        O33ERR = *zeros
082400151104     C                   z-add     O33NRF        §numori
082500151104     c                   else
082600151104     c                   eval      §numori = *all'9'
082700151104     C                   seton                                        31        * errore bloccante
082800151104     C                   endif
082900151104     C*
083000151104     C                   eval      vaoNOR = §numori
083100151104     C*
083200151104     C                   ENDSR
083300151104
083400151104
083500151104     C*------------------------------------------------------------------------*
083600151104     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
083700151104     C*------------------------------------------------------------------------*
083800151104     C     EXEFORZ       BEGSR
083900151104     C*
084000151104     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
084100151104     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
084200151104     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
084300151104     C*
084400151104     C                   ENDSR
084500141209
084600050323
084700050323
084800050323     C*------------------------------------------------------------------------*
084900050323     C* CHKCOR - Considerazioni su VAOCOR
085000050323     C*------------------------------------------------------------------------*
085100050323     C     CHKCOR        BEGSR
085200050323     C*
085300050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
085400050323     C                   if        vaoTCO = 'F'
085500050323     C* Quindi verifico se il codice ordinante non è valorizzato
085600050323     C                   if        vaoCOR = *zeros
085700050323     C*                  if        vaoCOR = *zeros  AND
085800050323     C*                            vaoRSO = *blanks AND
085900050323     C*                            vaoINO = *blanks AND
086000050323     C*                            vaoCAO = *blanks AND
086100050323     C*                            vaoLOO = *blanks AND
086200050323     C*                            vaoPRO = *blanks AND
086300050323     C*                            vaoNAO = *blanks
086400050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
086500050323     C                   move      vlrKSC        wCli              7 0
086600050323     C                   movel(p)  wCli          acrCRO
086700050323     C     acrCRO        setll     fnacr01l
086800050323     C                   if        %found(fnacr01l)
086900050323     C                   read      fnacr01l
087000050323     C                   dow       not %eof(fnacr01l)
087100050323     C                   movel     acrCRO        wCliAcr           7 0
087200050323     C                   if        wCliAcr = wCli
087300050323     C                   eval      vaoCOR = acrCRO
087400050323     C                   leave
087500050323     C                   else
087600050323     C                   leave
087700050323     C                   endif
087800050323     C                   read      fnacr01l
087900050323     C                   enddo
088000050323     C                   endif
088100050323     C                   endif
088200050323     C                   endif
088300050323     C*
088400050323     C                   if        vaoTCO = 'I'
088500050323     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
088600050323     C                   if        vlrKSC = '0ORM0000'
088700050323     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
088800061031     C***                eval      vaoINO = 'ORM DA INTERNET'
088900061031     C                   movel     ConstINO      vaoINO
089000050323     C                   eval      vaoCAO = '40138'
089100050323     C                   eval      vaoLOO = 'BOLOGNA'
089200050323     C                   eval      vaoPRO = 'BO'
089300050323     C                   endif
089400050323     C                   endif
089500050323     C*
089600050323     C                   ENDSR
089700990920
089800010528
089900011112
090000011112     C*------------------------------------------------------------------------*
090100011112     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
090200011112     C*------------------------------------------------------------------------*
090300011112     C     WRIORI        BEGSR
090400011112     C*
090500090617     C* Come prima cosa stacco un numeratore da AZNUM
090600151104     C***                clear                   trul33ds
090700151104     C***                eval      I33OPE = *zeros
090800151104     C***                eval      I33CNU = 600
090900151104     C***                eval      I33NUM = 1
091000151104     C***                movel     TRUL33DS      KPJBU
091100151104     C***                call      'TRUL33R'
091200151104     C***                parm                    KPJBA
091300151104     C***                movel     KPJBU         TRUL33DS
091400151104     C***                if        O33ERR = *zeros
091500151104     C***                z-add     O33NRF        §numori
091600151104     c***                else
091700151104     c***                eval      §numori = *all'9'
091800151104     C***                endif
091900070925     C*
092000070925     C* Giro "standard" (ovvero x cliente d invio dati)
092100070925     C                   if        *in51 = *off
092200011112     C*
092300011122     C* Qindi imposto i campi "extra-VAO"
092400011122     C                   if        vlrKSC = '0ORM0000'
092500011122     C*
092600011122     C                   if        vaoCOR > *zeros
092700011122     C                   movel     vaoCOR        wrkCOR7           7 0
092800011122     C*
092900011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
093000011122     C* codice padre. Se è un padre assegno già il codice ordinante.
093100011122     C                   clear                   tibs10ds
093200011122     C                   eval      d10drf = datcor                              *data riferimento
093300011122     C                   eval      d10paf = 'P'                                 *cerca il padre
093400011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
093500091026     C*
093600091026     C* Preparazione chiamata al TIBS10R1
093700091026     C                   clear                   tibs10r1ds
093800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
093900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
094000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
094100091026     C*
094200091026     C                   call      'TIBS10R1'
094300091026     C                   parm                    tibs10r1ds
094400091026     C*
094500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
094600011122     C*
094700011122     C                   if        d10err <> *blanks or                         *No unif. o padre
094800011122     C                             d10cop =  *zeros  or
094900011122     C                             d10cop =  d10cod
095000011122     C                   move      d10cod        oriIDC
095100011122     C                   else                                                   *Figlio
095200011122     C                   move      d10cop        oriIDC
095300011122     C                   endif
095400011122     C*
095500011122     C                   else
095600011122     C                   eval      oriIDC = *zeros
095700011122     C                   endif
095800011122     C*
095900011122     C                   else
096000011122     C                   eval      oriIDC = vlrKSC
096100011122     C                   endif
096200070925     C*
096300070925     C* Giro "forzato" (ovvero x codice ordinante)
096400070925     C                   else
096500070925     C*
096600070925     C                   if        vaoCOR > *zeros
096700070925     C                   movel     vaoCOR        wrkCOR7           7 0
096800070925     C*
096900070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
097000070925     C* codice padre. Se è un padre assegno già il codice ordinante.
097100070925     C                   clear                   tibs10ds
097200070925     C                   eval      d10drf = datcor                              *data riferimento
097300070925     C                   eval      d10paf = 'P'                                 *cerca il padre
097400070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
097500091026     C*
097600091026     C* Preparazione chiamata al TIBS10R1
097700091026     C                   clear                   tibs10r1ds
097800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
097900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
098000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
098100091026     C*
098200091026     C                   call      'TIBS10R1'
098300091026     C                   parm                    tibs10r1ds
098400091026     C*
098500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
098600070925     C*
098700070925     C                   if        d10err <> *blanks or                         *No unif. o padre
098800070925     C                             d10cop =  *zeros  or
098900070925     C                             d10cop =  d10cod
099000070925     C                   move      d10cod        oriIDC
099100070925     C                   else                                                   *Figlio
099200070925     C                   move      d10cop        oriIDC
099300070925     C                   endif
099400070926     C*
099500070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
099600070926     C* al servizio d rintraccia ritiri on-line
099700070926     C                   eval      vssKSU = oriIDC
099800070926     C                   eval      vssISV = 'T3'
099900070926     C     keyvss02_p    chain     tivss02l
100000070926     C                   if        %found(tivss02l)
100100070926     C                   if        vssdde <= datcor AND
100200070926     C                             datcor <= vssdsc
100300070926     C                   else
100400070926     C                   eval      oriIDC = vlrKSC
100500070926     C                   endif
100600070926     C                   else
100700070926     C                   eval      oriIDC = vlrKSC
100800070926     C                   endif
100900070926     C*
101000070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
101100070926     C* anagrafica clienti ritiro
101200070926     C                   setoff                                       52
101300070926     C                   eval      acrCRO = wrkCOR7 * 1000
101400070926     C     acrCRO        setll     fnacr01l
101500070926     C                   if        %found(fnacr01l)
101600070926     C                   read      fnacr01l
101700070926     C                   dow       not %eof(fnacr01l)
101800070926     C                   movel     acrCRO        wCliAcr           7 0
101900070926     C                   if        wCliAcr = wrkCOR7
102000070926     C                   seton                                        52
102100070926     C                   leave
102200070926     C                   else
102300070926     C                   leave
102400070926     C                   endif
102500070926     C                   read      fnacr01l
102600070926     C                   enddo
102700070926     C                   endif
102800070926     C*
102900070926     C                   if        *in52  = *off
103000070926     C                   eval      oriIDC = vlrKSC
103100070926     C                   endif
103200070925     C*
103300070925     C                   else
103400070925     C                   if        vlrKSC = '0ORM0000'
103500070925     C                   eval      oriIDC = *zeros
103600070925     C                   else
103700070925     C                   eval      oriIDC = vlrKSC
103800070925     C                   endif
103900070925     C                   endif
104000070925     C                   endif
104100011122     C*
104200011112     C                   eval      oriPRG = §numori
104300011112     C                   eval      oriDER = datcor
104400011112     C                   eval      oriOER = wn6
104500011112     C                   eval      oriPOE = vaoPOE
104600011112     C                   eval      oriNSR = vaoNSR
104700011112     C                   eval      oriNOR = vaoNOR
104800011112     C                   eval      oriNRV = vaoNRV
104900011112     C                   eval      oriFLO = *blanks
105000011112     C*
105100011112     C* Infine valorizzo la chieve esterna sul file FNVAO
105200011112     C                   eval      dorm01 = vaoFLO
105300011112     C                   movel     §numori       §ormpg
105400011112     C                   eval      vaoFLO = dorm01
105500011112     C*
105600011112     C* ...quindi scrivo il file TIORI00F
105700011112     C                   WRITE     TIORI000
105800011112     C*
105900011112     C                   ENDSR
106000050826
106100050826
106200050826
106300050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
106400050826     C     wriVGD        BEGSR
106500050826     C*
106600050826     C* Reperisco la descrizione della fase da tabella
106700050826     C                   movel(p)  'FAR'         KteCOD
106800050826     C                   movel(p)  '000'         KteKE1
106900050826     C     KEYtbe        chain     tntbe01l
107000050826     C                   if        %found(tntbe01l)
107100050826     C                   movel(P)  tbeuni        DFAR
107200050826     C                   else
107300050826     C                   clear                   DFAR
107400050826     C                   endif
107500050826     C*
107600050826     C* Valorizzo buffer record
107700050826     C                   clear                   FNVAPDS
107800050826     C                   eval      vapIDC = oriIDC
107900050826     C                   eval      vapPOE = oriPOE
108000050826     C                   eval      vapRFA = vaoRFA
108100050826     C                   eval      vapPOG = oriPOE
108200050826     C                   eval      vapDAE = oriDER
108300050826     C                   eval      vapORE = oriOER
108400050826     C                   if        d§fardva <> *blanks
108500050826     C                   eval      vapDFA = d§fardva
108600050826     C                   else
108700050826     C                   eval      vapDFA = d§fardes
108800050826     C                   endif
108900050826     C*
109000050826     C                   clear                   tivgd000
109100080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
109200050826     C                   eval      vgdTIP = 'VP'
109300050826     C                   eval      vgdKSU = vapIDC
109400050826     C                   eval      vgdTSC = 'WW'
109500050826     C                   eval      vgdDAT = datcor
109600050826     C                   eval      vgdPGM = 'TIS7T11R'
109700060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
109800060306     C                   exsr      sr_Control
109900060306     C                   if        Cli_Abil = *on
110000060306     C                   eval      vgdKSU = woriIDC
110100060306     C                   write     tivgd000
110200060306     C                   endif
110300050826     C*
110400050826     C                   ENDSR
110500050826     C*------------------------------------------------------------------------*
110600011119
110700010528
110800010528
110900010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
111000010528     C     repfil        BEGSR
111100010528     C*
111200010528     C                   if        invfil = *zeros and
111300010528     C                             depfil > *zeros and
111400010613     C                             (vinflg = *blanks or
111500010613     C                              vinflg = *zeros)
111600010528     C
111700010528     C                   eval      invfil = depfil
111800010528     C                   endif
111900010528     C*
112000010529     C                   if        depfil <> invfil and
112100010529     C                             invfil > *zeros
112200010528     C                   eval      flgMulti = '1'
112300010528     C                   if        vinflg = *blanks
112400010528     C                   add       1             cntNonEl
112500010528     C                   endif
112600010528     C                   endif
112700010528     C*
112800010528     C                   if        vinflg = '2'
112900010528     C                   eval      flgStato = '2'
113000010528     C                   endif
113100010528     C*
113200010528     C                   ENDSR
113300010528     C***
113400010528
113500010528
113600010528
113700010528
113800990920      /TITLE Invio dei dati al punto operativo.
113900000613     C     invio         BEGSR
114000990920     C*
114100990920     C                   reset                   dscmz
114200010528     C                   move      invfil        cmzdst
114300990920     C                   eval      cmzfld = vlrfou
114400990920     C                   eval      cmzmbd = vlrhdl
114500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
114600000710     C                   if        prmfir = *blanks
114700010122     C                   eval      cmzfla = 'FNVAO00F'
114800010122     C                   eval      cmzmba = 'FNVAO00F'
114900000710     C                   else
115000000710     C                   eval      cmzfla = prmfir
115100000710     C                   eval      cmzmba = prmfir
115200000710     C                   endif
115300990920     C                   eval      cmznrr = *zeros
115400990920     C                   move      §ctrok        cmznrr
115500010123     C                   eval      cmzlba = vlrfl1
115600990920     C                   call(e)   'TIS711C'
115700990920     C                   parm                    dscmz
115800990921     C                   parm      *blanks       esito
115900990920     C                   if        %error
116000990920     C                             or cmzerr = '1'
116100990921     C                             or esito  = '1'
116200000710     C                   eval      wrkesito = '3'
116300990920     C                   endif
116400990920     C*
116500000613     C                   ENDSR
116600000613     C***
116700990910
116800010528
116900010528
117000010528
117100010528      /TITLE Invio dei dati al punto operativo.
117200010528     C     opeini        BEGSR
117300010528     C*
117400010528     C* Inizializzo flag e contatori operativi
117500010528     C                   movel     '0'           flgGiro           1
117600010528     C                   movel     '0'           flgMulti          1
117700010528     C                   movel     '1'           flgStato          1
117800010615     C                   movel     '0'           flgOk             1
117900010528     C                   z-add     *zeros        cntNonEl         10 0
118000010528     C                   z-add     *zeros        depfil            3 0
118100010528     C                   z-add     *zeros        invfil            3 0
118200030407     C*
118300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
118400030407     C                   if        %subst(vlrppt:1:1) = 'S'
118500030407     C                   seton                                        50
118600030407     C                   else
118700030407     C                   setoff                                       50
118800030407     C                   endif
118900070925     C*
119000070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
119100070925     C                   if        %subst(vlrppt:2:1) = 'S'
119200070925     C                   seton                                        51
119300070925     C                   else
119400070925     C                   setoff                                       51
119500070925     C                   endif
119600010528     C*
119700010528     C                   ENDSR
119800010528     C***
119900060306
120000060306
120100060306     C*-------------------------------------------------------------------------*
120200060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
120300060306     C*-------------------------------------------------------------------------*
120400060306     C     sr_Control    begsr
120500060306     C*
120600060306     C* inizializzo a ok il flag d controllo abilitazione
120700060306     C                   eval      Cli_Abil = *on
120800060306     C
120900060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
121000060306     C                   clear                   woriIDC
121100060306     C                   eval      kteCOD = 'VAP'
121200060306     C                   eval      kteKE1 = oriIDC
121300060306     C     keytbe        chain     TNTBE01L
121400060306     C                   if        %found(Tntbe01l)
121500060306     C                   eval      woriIDC = TbeKe2
121600060306     C                   else
121700060306     C                   eval      Cli_Abil = *off
121800060306     C                   endif
121900060306     C*
122000060306     C                   endsr
122100010528
122200010528
122300010528
122400010528
122500000613     C     *inzsr        BEGSR
122600990910     C*
122700990910     C     *entry        plist
122800990920     C                   parm                    tivlrds
122900990921     C                   parm      wrkesito      esito
123000000724     C                   parm                    prmlit
123100000710     C                   parm                    prmfir
123200011112     C*
123300011112     C* Campi riferito al database
123400011112     C     *like         define    tbecod        kteCOD                         *TNTBE01L
123500011112     C     *like         define    tbeke1        kteKE1
123600011112     C*
123700011112     C* Definizione chiavi di lettura
123800011112     C* TNTBE01L - parziale
123900011112     C     keytbe        klist
124000011112     C                   kfld                    kteCOD                         *UTENTE
124100011112     C                   kfld                    kteKE1                         *CODICE
124200070926     C* TIVSS02L - parziale
124300070926     C     keyvss02_p    klist
124400070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
124500070926     C                   kfld                    vssISV                         *SERVIZIO
124600011112     C*
124700011112     C* Reperimento data e ora corrente
124800011112     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
124900011112     C                   movel     wn14          wn6               6 0          *ORA (6)
125000011112     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
125100011112     C                   z-add     wn8           g08dat
125200011112     C                   z-add     *zeros        g08inv
125300011112     C                   movel     '0'           g08err
125400011112     C                   call      'XSRDA8'
125500011112     C                   parm                    wlbda8
125600011112     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
125700010529     C*
125800000613     C                   ENDSR
125900000613     C***
