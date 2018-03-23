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
017300141209     C*
017400141209     C* Verifico se trattasi di formato ORM "esteso"
017500141209     C                   setoff                                       53
017600150209     C                   if        %subst(vindta:684+1:%size(fnvaoeaer)-684+1)
017700141209     C                             > *blanks
017800141209     C                   eval      fnvaoeaer = vindta
017900141209     C                   seton                                        53
018000141209     C                   exsr      EXEFORZ
018100141209     C                   endif
018200991022     C*
018300010122     C                   testb     x'3F'         a_vaoatb                 02
018400991022     C                   if        not *in02
018500991022     C*
018600971021     C                   SETOFF                                       303132
018700971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
018800971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
018900971023     C*    OPPURE TUTTI BLANKS
019000010122     C*-------------------
019100010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
019200010122     C* IN32 => Campo facoltativo, muovo tutti zero e passo cimunque il record marcandolo come OK
019300010122     C*-------------------
019400010122     C*
019500010417 xxx C*                  TESTN                   a_vaopoe             30
019600010417  "  C* N30              TESTN                   a_vaopoe               30
019700010417  "  C* N30              TESTN                   a_vaopoe                 30
019800010417     C*                  MOVE      a_vaopoe      W001A             1
019900010417     C*    *IN30         IFEQ      *OFF
020000010417     C*    W001A         ORLT      '0'
020100010417     C*                  SETON                                        32
020200010417     C*                  MOVE      *ZEROS        VAOPOE
020300010417     C*                  EVAL      vinmsg = 'VAOPOE'
020400010417     C*                  ELSE
020500010417     C*                  MOVE      a_vaopoe      VAOPOE
020600010417     C*                  ENDIF
020700010417  "  C*
020800010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
020900010604 xxx C                   IF        vlrpoi <> 999
021000010528     C                   EVAL      VAOPOE = vlrpoi
021100010528     C                   ELSE
021200010528     C                   IF        a_vaopoe = *blanks or
021300011010     C                             a_vaopoe = *zeros  or
021400011010     C                             a_vaopoe = ' 000'
021500010528     C                   IF        a_vaopor <> *blanks and
021600011010     C                             a_vaopor <> *zeros  and
021700011010     C                             a_vaopor <> ' 000'
021800010528     C                   MOVE      a_vaopor      VAOPOE
021900010528     C                   ELSE
022000011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
022100010528     C                   MOVE      a_vaopoe      VAOPOE
022200010528     C                   ENDIF
022300010529     C                   ELSE
022400010529     C                   MOVE      a_vaopoe      VAOPOE
022500010528     C                   ENDIF
022600010528     C                   ENDIF
022700010122     C*
022800010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022900010528     C                   eval      depfil = VAOPOE
023000010528     C                   exsr      repfil
023100010528     C                   if        depfil = invfil and
023200010528     C                             (vinflg = *blanks
023300010528     C                              or vinflg = '0'
023400010528     C                              or vinflg = '2')
023500010528     C*
023600010528     C                   exsr      PREELA
023700010528     C*
023800010719     C*                  TESTN                   a_vaonsr             30
023900010719     C* N30              TESTN                   a_vaonsr               30
024000010719     C* N30              TESTN                   a_vaonsr                 30
024100010719     C*                  MOVE      a_vaonsr      W001A             1
024200010719     C*    *IN30         IFEQ      *OFF
024300010719     C*    W001A         ORLT      '0'
024400010719     C*                  SETON                                        32
024500010719     C*                  MOVE      *ZEROS        VAONSR
024600010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
024700010719     C*                  ELSE
024800010719     C*                  MOVE      a_vaonsr      VAONSR
024900010719     C*                  ENDIF
025000010122     C*
025100010719     C*                  TESTN                   a_vaonor             30
025200010719     C* N30              TESTN                   a_vaonor               30
025300010719     C* N30              TESTN                   a_vaonor                 30
025400010719     C*                  MOVE      a_vaonor      W001A             1
025500010719     C*    *IN30         IFEQ      *OFF
025600010719     C*    W001A         ORLT      '0'
025700010719     C*                  SETON                                        32
025800010719     C*                  MOVE      *ZEROS        VAONOR
025900010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
026000010719     C*                  ELSE
026100010719     C*                  MOVE      a_vaonor      VAONOR
026200010719     C*                  ENDIF
026300010122     C*
026400010719     C*                  TESTN                   a_vaonrv             30
026500010719     C* N30              TESTN                   a_vaonrv               30
026600010719     C* N30              TESTN                   a_vaonrv                 30
026700010719     C*                  MOVE      a_vaonrv      W001A             1
026800010719     C*    *IN30         IFEQ      *OFF
026900010719     C*    W001A         ORLT      '0'
027000010719     C*                  SETON                                        32
027100010719     C*                  MOVE      *ZEROS        VAONRV
027200010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
027300010719     C*                  ELSE
027400010719     C*                  MOVE      a_vaonrv      VAONRV
027500010719     C*                  ENDIF
027600010122     C*
027700010122     C                   TESTN                   a_vaodao             30
027800010122     C  N30              TESTN                   a_vaodao               30
027900010122     C  N30              TESTN                   a_vaodao                 30
028000010122     C                   MOVE      a_vaodao      W001A             1
028100010122     C     *IN30         IFEQ      *OFF
028200010122     C     W001A         ORLT      '0'
028300010122     C                   SETON                                        32
028400010122     C                   MOVE      *ZEROS        VAODAO
028500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
028600010122     C                   ELSE
028700010122     C                   MOVE      a_vaodao      VAODAO
028800010122     C                   ENDIF
028900010122     C*
029000010122     C                   TESTN                   a_vaooao             30
029100010122     C  N30              TESTN                   a_vaooao               30
029200010122     C  N30              TESTN                   a_vaooao                 30
029300010122     C                   MOVE      a_vaooao      W001A             1
029400010122     C     *IN30         IFEQ      *OFF
029500010122     C     W001A         ORLT      '0'
029600010122     C                   SETON                                        32
029700010122     C                   MOVE      *ZEROS        VAOOAO
029800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
029900010122     C                   ELSE
030000010122     C                   MOVE      a_vaooao      VAOOAO
030100010122     C                   ENDIF
030200010122     C*
030300010122     C                   TESTN                   a_vaocor             30
030400010122     C  N30              TESTN                   a_vaocor               30
030500010122     C  N30              TESTN                   a_vaocor                 30
030600010122     C                   MOVE      a_vaocor      W001A             1
030700010122     C     *IN30         IFEQ      *OFF
030800010122     C     W001A         ORLT      '0'
030900010122     C                   SETON                                        32
031000010122     C                   MOVE      *ZEROS        VAOCOR
031100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
031200010122     C                   ELSE
031300010122     C                   MOVE      a_vaocor      VAOCOR
031400010122     C                   ENDIF
031500010122     C*
031600010122     C                   TESTN                   a_vaocra             30
031700010122     C  N30              TESTN                   a_vaocra               30
031800010122     C  N30              TESTN                   a_vaocra                 30
031900010122     C                   MOVE      a_vaocra      W001A             1
032000010122     C     *IN30         IFEQ      *OFF
032100010122     C     W001A         ORLT      '0'
032200010122     C                   SETON                                        32
032300010122     C                   MOVE      *ZEROS        VAOCRA
032400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
032500010122     C                   ELSE
032600010122     C                   MOVE      a_vaocra      VAOCRA
032700010122     C                   ENDIF
032800010122     C*
032900010719     C                   IF        a_vaocrc = *blanks or
033000010719     C                             a_vaocrc = *zeros
033100010719     C                   ELSE
033200010122     C                   TESTN                   a_vaocrc             30
033300010122     C  N30              TESTN                   a_vaocrc               30
033400010122     C  N30              TESTN                   a_vaocrc                 30
033500010122     C                   MOVE      a_vaocrc      W001A             1
033600010122     C     *IN30         IFEQ      *OFF
033700010122     C     W001A         ORLT      '0'
033800010122     C                   SETON                                        32
033900010122     C                   MOVE      *ZEROS        VAOCRC
034000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
034100010122     C                   ELSE
034200010122     C                   MOVE      a_vaocrc      VAOCRC
034300010122     C                   ENDIF
034400010719     C                   ENDIF
034500010122     C*
034600010122     C                   TESTN                   a_vaodar             30
034700010122     C  N30              TESTN                   a_vaodar               30
034800010122     C  N30              TESTN                   a_vaodar                 30
034900010122     C                   MOVE      a_vaodar      W001A             1
035000010122     C     *IN30         IFEQ      *OFF
035100010122     C     W001A         ORLT      '0'
035200010122     C                   SETON                                        32
035300010122     C                   MOVE      *ZEROS        VAODAR
035400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
035500010122     C                   ELSE
035600010122     C                   MOVE      a_vaodar      VAODAR
035700010122     C                   ENDIF
035800010122     C*
035900010122     C                   TESTN                   a_vaoorr             30
036000010122     C  N30              TESTN                   a_vaoorr               30
036100010122     C  N30              TESTN                   a_vaoorr                 30
036200010122     C                   MOVE      a_vaoorr      W001A             1
036300010122     C     *IN30         IFEQ      *OFF
036400010122     C     W001A         ORLT      '0'
036500010122     C                   SETON                                        32
036600010122     C                   MOVE      *ZEROS        VAOORR
036700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
036800010122     C                   ELSE
036900010122     C                   MOVE      a_vaoorr      VAOORR
037000010122     C                   ENDIF
037100010122     C*
037200010122     C                   TESTN                   a_vaoncl             30
037300010122     C  N30              TESTN                   a_vaoncl               30
037400010122     C  N30              TESTN                   a_vaoncl                 30
037500010122     C                   MOVE      a_vaoncl      W001A             1
037600010122     C     *IN30         IFEQ      *OFF
037700010122     C     W001A         ORLT      '0'
037800010122     C                   SETON                                        32
037900010122     C                   MOVE      *ZEROS        VAONCL
038000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
038100010122     C                   ELSE
038200010122     C                   MOVE      a_vaoncl      VAONCL
038300010122     C                   ENDIF
038400010122     C*
038500010122     C                   MOVE      *ZERO         VAOPKG
038600150209     C                   MOVE      a_vaopkg      WASC07
038700150209     C                   TESTN                   WI07                 30
038800150209     C  N30              TESTN                   WI07                   30
038900150209     C  N30              TESTN                   WI07                     30
039000150209     C                   MOVE      WI07          W001A             1
039100010122     C     *IN30         IFEQ      *OFF
039200010122     C     W001A         ORLT      '0'
039300010122     C     W001A         ANDNE     *BLANK
039400010122     C                   SETON                                        32
039500010122     C                   MOVE      *ZERO         VAOPKG
039600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
039700010122     C                   ELSE
039800150209     C                   MOVEL     WI07          VAOPKG
039900150209     C                   TESTN                   WD07                 30
040000150209     C  N30              TESTN                   WD07                     30
040100150209     C                   MOVE      WD07          W001A             1
040200010122     C     *IN30         IFEQ      *OFF
040300010122     C     W001A         ORLT      '0'
040400010122     C     W001A         ANDNE     *BLANK
040500010122     C                   SETON                                        32
040600010122     C                   MOVE      *ZERO         VAOPKG
040700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
040800010122     C                   ELSE
040900150209     C                   MOVE      WD07          VAOPKG
041000010122     C                   ENDIF
041100050118     C                   ENDIF
041200010122     C*
041300010122     C                   MOVE      *ZERO         VAOVLM
041400150209     C                   MOVE      a_vaovlm      WASC05
041500150209     C                   TESTN                   WI05                 30
041600150209     C  N30              TESTN                   WI05                   30
041700150209     C  N30              TESTN                   WI05                     30
041800150209     C                   MOVE      WI05          W001A             1
041900010122     C     *IN30         IFEQ      *OFF
042000010122     C     W001A         ORLT      '0'
042100010122     C     W001A         ANDNE     *BLANK
042200010122     C                   SETON                                        32
042300010122     C                   MOVE      *ZERO         VAOVLM
042400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
042500010122     C                   ELSE
042600150209     C                   MOVEL     WI05          VAOVLM
042700150209     C                   TESTN                   WD05                 30
042800150209     C  N30              TESTN                   WD05                   30
042900150209     C  N30              TESTN                   WD05                     30
043000150209     C                   MOVE      WD05          W001A             1
043100010122     C     *IN30         IFEQ      *OFF
043200010122     C     W001A         ORLT      '0'
043300010122     C     W001A         ANDNE     *BLANK
043400010122     C                   SETON                                        32
043500010122     C                   MOVE      *ZERO         VAOVLM
043600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
043700010122     C                   ELSE
043800150209     C                   MOVE      WD05          VAOVLM
043900010122     C                   ENDIF
044000050118     C                   ENDIF
044100010122     C*
044200010122     C                   TESTN                   a_vaobnc             30
044300010122     C  N30              TESTN                   a_vaobnc               30
044400010122     C  N30              TESTN                   a_vaobnc                 30
044500010122     C                   MOVE      a_vaobnc      W001A             1
044600010122     C     *IN30         IFEQ      *OFF
044700010122     C     W001A         ORLT      '0'
044800010122     C                   SETON                                        32
044900010122     C                   MOVE      *ZEROS        VAOBNC
045000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
045100010122     C                   ELSE
045200010122     C                   MOVE      a_vaobnc      VAOBNC
045300010122     C                   ENDIF
045400010122     C*
045500010122     C                   TESTN                   a_vaoblc             30
045600010122     C  N30              TESTN                   a_vaoblc               30
045700010122     C  N30              TESTN                   a_vaoblc                 30
045800010122     C                   MOVE      a_vaoblc      W001A             1
045900010122     C     *IN30         IFEQ      *OFF
046000010122     C     W001A         ORLT      '0'
046100010122     C                   SETON                                        32
046200010122     C                   MOVE      *ZEROS        VAOBLC
046300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
046400010122     C                   ELSE
046500010122     C                   MOVE      a_vaoblc      VAOBLC
046600010122     C                   ENDIF
046700010122     C*
046800010122     C                   TESTN                   a_vaoatt             30
046900010122     C  N30              TESTN                   a_vaoatt               30
047000010122     C  N30              TESTN                   a_vaoatt                 30
047100010122     C                   MOVE      a_vaoatt      W001A             1
047200010122     C     *IN30         IFEQ      *OFF
047300010122     C     W001A         ORLT      '0'
047400010122     C                   SETON                                        32
047500010122     C                   MOVE      *ZEROS        VAOATT
047600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
047700010122     C                   ELSE
047800010122     C                   MOVE      a_vaoatt      VAOATT
047900010122     C                   ENDIF
048000010122     C*
048100010122     C                   TESTN                   a_vaomtc             30
048200010122     C  N30              TESTN                   a_vaomtc               30
048300010122     C  N30              TESTN                   a_vaomtc                 30
048400010122     C                   MOVE      a_vaomtc      W001A             1
048500010122     C     *IN30         IFEQ      *OFF
048600010122     C     W001A         ORLT      '0'
048700010122     C                   SETON                                        32
048800010122     C                   MOVE      *ZEROS        VAOMTC
048900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
049000010122     C                   ELSE
049100010122     C                   MOVE      a_vaomtc      VAOMTC
049200010122     C                   ENDIF
049300010122     C*
049400010719     C                   IF        a_vaoksc = *blanks or
049500010719     C                             a_vaoksc = *zeros
049600010719     C                   ELSE
049700010122     C                   TESTN                   a_vaoksc             30
049800010122     C  N30              TESTN                   a_vaoksc               30
049900010122     C  N30              TESTN                   a_vaoksc                 30
050000010122     C                   MOVE      a_vaoksc      W001A             1
050100010122     C     *IN30         IFEQ      *OFF
050200010122     C     W001A         ORLT      '0'
050300010122     C                   SETON                                        32
050400010122     C                   MOVE      *ZEROS        VAOKSC
050500010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
050600010122     C                   ELSE
050700010122     C                   MOVE      a_vaoksc      VAOKSC
050800010122     C                   ENDIF
050900010719     C                   ENDIF
051000010122     C*
051100010710     C                   IF        a_vaoctr > *blanks
051200010122     C                   TESTN                   a_vaoctr             30
051300010122     C  N30              TESTN                   a_vaoctr               30
051400010122     C  N30              TESTN                   a_vaoctr                 30
051500010122     C                   MOVE      a_vaoctr      W001A             1
051600010122     C     *IN30         IFEQ      *OFF
051700010122     C     W001A         ORLT      '0'
051800010122     C                   SETON                                        32
051900010627     C                   MOVE      *blanks       VAOCTR
052000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
052100010122     C                   ELSE
052200010122     C                   MOVE      a_vaoctr      VAOCTR
052300010122     C                   ENDIF
052400010710     C                   ENDIF
052500010122     C*
052600010122     C                   TESTN                   a_vaopor             30
052700010122     C  N30              TESTN                   a_vaopor               30
052800010122     C  N30              TESTN                   a_vaopor                 30
052900010122     C                   MOVE      a_vaopor      W001A             1
053000010122     C     *IN30         IFEQ      *OFF
053100010122     C     W001A         ORLT      '0'
053200010122     C                   SETON                                        32
053300010122     C                   MOVE      *ZEROS        VAOPOR
053400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
053500010122     C                   ELSE
053600010122     C                   MOVE      a_vaopor      VAOPOR
053700010122     C                   ENDIF
053800010122     C*
053900010719     C                   IF        a_vaopoc = *blanks or
054000010719     C                             a_vaopoc = *zeros
054100010719     C                   ELSE
054200010122     C                   TESTN                   a_vaopoc             30
054300010122     C  N30              TESTN                   a_vaopoc               30
054400010122     C  N30              TESTN                   a_vaopoc                 30
054500010122     C                   MOVE      a_vaopoc      W001A             1
054600010122     C     *IN30         IFEQ      *OFF
054700010122     C     W001A         ORLT      '0'
054800010122     C                   SETON                                        32
054900010122     C                   MOVE      *ZEROS        VAOPOC
055000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
055100010122     C                   ELSE
055200010122     C                   MOVE      a_vaopoc      VAOPOC
055300010122     C                   ENDIF
055400010719     C                   ENDIF
055500010122     C*
055600010719     C                   IF        a_vaosto = *blanks or
055700010719     C                             a_vaosto = *zeros
055800010719     C                   ELSE
055900010122     C                   TESTN                   a_vaosto             30
056000010122     C  N30              TESTN                   a_vaosto               30
056100010122     C  N30              TESTN                   a_vaosto                 30
056200010122     C                   MOVE      a_vaosto      W001A             1
056300010122     C     *IN30         IFEQ      *OFF
056400010122     C     W001A         ORLT      '0'
056500010122     C                   SETON                                        32
056600010122     C                   MOVE      *ZEROS        VAOSTO
056700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
056800010122     C                   ELSE
056900010122     C                   MOVE      a_vaosto      VAOSTO
057000010122     C                   ENDIF
057100010719     C                   ENDIF
057200971021     C*
057300971021     C* CAMPI ALFABETICI:
057400010122     C                   MOVE      a_vaoatb      VAOATB
057500010122     C                   MOVE      a_vaotor      VAOTOR
057600010531     C*
057700010531     C                   IF        a_vaotco = *blanks
057800010531     C                   MOVE      'F'           VAOTCO
057900010531     C                   ELSE
058000010531     C                   MOVE      a_vaotco      VAOTCO
058100010531     C                   ENDIF
058200010531     C*
058300010122     C                   MOVE      a_vaorso      VAORSO
058400010122     C                   MOVE      a_vaoino      VAOINO
058500010122     C                   MOVE      a_vaocao      VAOCAO
058600010122     C                   MOVE      a_vaoloo      VAOLOO
058700010122     C                   MOVE      a_vaopro      VAOPRO
058800090129     C                   MOVE      a_vaonao      VAONAO
058900031014     C* Reperisco la provincia dal CAP e dalla località dell'ORDINANTE
059000031014     C                   IF        VAOPRO  = *blanks AND
059100050907     C                             VAOCAO <> *blanks AND
059200050907     C                             VAONAO  = *blanks                            * solo x Italia
059300031014     C                   CLEAR                   TISI95DS
059400031014     C                   EVAL      I95TCN = '3'
059500031014     C                   Z-ADD     datcor        I95DAT
059600031014     C                   EVAL      I95CAP = VAOCAO
059700031014     C                   EVAL      I95LOC = VAOLOO
059800031014     C                   CALL      'TISI95R'
059900031014     C                   PARM                    TISI95DS
060000031014     C                   EVAL      VAOPRO = O95PRV
060100031014     C                   ENDIF
060200031014     C*
060300010122     C                   MOVE      a_vaorsr      VAORSR
060400010122     C                   MOVE      a_vaoinr      VAOINR
060500010122     C                   MOVE      a_vaocar      VAOCAR
060600010122     C                   MOVE      a_vaolor      VAOLOR
060700010122     C                   MOVE      a_vaoprr      VAOPRR
060800090129     C                   MOVE      a_vaonar      VAONAR
060900031014     C* Reperisco la provincia dal CAP e dalla località RITIRO
061000031014     C                   IF        VAOPRR  = *blanks AND
061100050907     C                             VAOCAR <> *blanks AND
061200050907     C                             VAONAR  = *blanks                            * solo x Italia
061300031014     C                   CLEAR                   TISI95DS
061400031014     C                   EVAL      I95TCN = '3'
061500031014     C                   Z-ADD     datcor        I95DAT
061600031014     C                   EVAL      I95CAP = VAOCAR
061700031014     C                   EVAL      I95LOC = VAOLOR
061800031014     C                   CALL      'TISI95R'
061900031014     C                   PARM                    TISI95DS
062000031014     C                   EVAL      VAOPRR = O95PRV
062100031014     C                   ENDIF
062200031014     C*
062300010122     C                   MOVE      a_vaorer      VAORER
062400010122     C                   MOVE      a_vaoter      VAOTER
062500010122     C                   MOVE      a_vaorsc      VAORSC
062600010122     C                   MOVE      a_vaoinc      VAOINC
062700010122     C                   MOVE      a_vaocac      VAOCAC
062800010122     C                   MOVE      a_vaoloc      VAOLOC
062900010122     C                   MOVE      a_vaoprc      VAOPRC
063000090129     C                   MOVE      a_vaonac      VAONAC
063100031014     C* Reperisco la provincia dal CAP e dalla località CONSEGNA
063200031014     C                   IF        VAOPRC  = *blanks AND
063300050907     C                             VAOCAC <> *blanks AND
063400050907     C                             VAONAC  = *blanks                            * solo x Italia
063500031014     C                   CLEAR                   TISI95DS
063600031014     C                   EVAL      I95TCN = '3'
063700031014     C                   Z-ADD     datcor        I95DAT
063800031014     C                   EVAL      I95CAP = VAOCAC
063900031014     C                   EVAL      I95LOC = VAOLOC
064000031014     C                   CALL      'TISI95R'
064100031014     C                   PARM                    TISI95DS
064200031014     C                   EVAL      VAOPRC = O95PRV
064300031014     C                   ENDIF
064400031014     C*
064500010122     C                   MOVE      a_vaoffd      VAOFFD
064600010122     C                   MOVE      a_vaormp      VAORMP
064700010122     C                   MOVE      a_vaonam      VAONAM
064800010122     C                   MOVE      a_vaopag      VAOPAG
064900010122     C                   MOVE      a_vaozor      VAOZOR
065000010122     C                   MOVE      a_vaono1      VAONO1
065100010122     C                   MOVE      a_vaono2      VAONO2
065200010122     C                   MOVE      a_vaoddt      VAODDT
065300010122     C                   MOVE      a_vaorfa      VAORFA
065400010122     C                   MOVE      a_vaospi      VAOSPI
065500010122     C                   MOVE      a_vaoflo      VAOFLO
065600050304     C*
065700050304     C* Effettuo alcune considerazioni x forzare all'evenienza determinati valori
065800050304     C*** VAONAO
065900050304     C                   IF        %trim(VAONAO) = 'I' OR
066000050304     C                             %trim(VAONAO) = 'IT'
066100050304     C                   EVAL      VAONAO = *blanks
066200050304     C                   ENDIF
066300050304     C*** VAONAR
066400050304     C                   IF        %trim(VAONAR) = 'I' OR
066500050304     C                             %trim(VAONAR) = 'IT'
066600050304     C                   EVAL      VAONAR = *blanks
066700050304     C                   ENDIF
066800050304     C*** VAONAC
066900050304     C                   IF        %trim(VAONAC) = 'I' OR
067000050304     C                             %trim(VAONAC) = 'IT'
067100050304     C                   EVAL      VAONAC = *blanks
067200050304     C                   ENDIF
067300050304     C*** Calcolo "automatico" volume in alternativa alla mancanza d ogni dato
067400050304     C                   IF        VAOPKG =  *zeros AND
067500050304     C                             VAOVLM =  *zeros AND
067600050304     C                             VAOBNC =  *zeros AND
067700050304     C                             VAONCL <> *zeros
067800050304     C                   EVAL      VAOVLM =  VAONCL * 0,01
067900050304     C                   ENDIF
068000030603     C*
068100030603     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
068200030603     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
068300030603     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
068400050303     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
068500150924     C                   EVAL      %subst(VAOFLO:9:20-9+1) = *blanks
068600930409     C*
068700010604     C  N31              ADD       1             §CTROK            7 0
068800010604     C   32              ADD       1             §CTRMO            7 0
068900010604     C   31              ADD       1             §CTRNO            7 0
069000030624     C  N31              EXSR      CHKCOR
069100011112     C  N31              EXSR      WRIORI
069200050826     C  N31              EXSR      WRIVGD
069300050421     C*
069400050421     C***
069500050421     C* FORZATURA SU DATA/ORA => NN CONSIDERO QUELLE PASSATE SUL FILE DAL CLIENTE MA SEMPRE
069600050421     C* DATA/ORA DI TRADUZIONE (OVVERO VERA IMMISSIONE DEL DATO SEPPUR DA CONFERMARE SUL
069700050421     C* NS. GESTIONALE). QUESTO SOLO X LA SCRITTURA DEL VAO!!!
069800050421     C                   eval      VAODAO = datcor
069900050421     C                   eval      VAOOAO = wn6
070000050421     C***
070100010122     C  N31              WRITE     FNVAO000
070200141209     C*
070300141209     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
070400141209     C                   if        *in53 and not *in31
070500141209     C                   move      VAOPOE        w_vaopoe                       * forzatura necessar
070600141209     C                   move      VAOCOR        w_vaocor                       * forzatura necessar
070700141209     C                   move      VAONOR        w_vaonor                       * forzatura necessar
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
080600141209
080700141209
080800141209     C*------------------------------------------------------------------------*
080900141209     C* EXEFORZ - Effettuo forzature
081000141209     C*------------------------------------------------------------------------*
081100141209     C     EXEFORZ       BEGSR
081200141209     C*
081300141209     C* Se necessario e VAONOR non è valorizzato stacco un numeratore da AZNUM
081400141209     C                   if        vaoNOR = *zeros
081500141209     C                   clear                   trul33ds
081600141209     C                   eval      I33OPE = *zeros
081700141209     C                   eval      I33CNU = 600
081800141209     C                   eval      I33NUM = 1
081900141209     C                   movel     TRUL33DS      KPJBU
082000141209     C                   call      'TRUL33R'
082100141209     C                   parm                    KPJBA
082200141209     C                   movel     KPJBU         TRUL33DS
082300141209     C                   if        O33ERR = *zeros
082400141209     C                   z-add     O33NRF        §numori
082500141209     c                   else
082600141209     c                   eval      §numori = *all'9'
082700141209     C                   seton                                        31        * errore bloccante
082800141209     C                   endif
082900141209     C*
083000141209     C                   eval      vaoNOR = §numori
083100141209     C                   endif
083200141209     C*
083300141209     C                   ENDSR
083400141209
083500050323
083600050323
083700050323     C*------------------------------------------------------------------------*
083800050323     C* CHKCOR - Considerazioni su VAOCOR
083900050323     C*------------------------------------------------------------------------*
084000050323     C     CHKCOR        BEGSR
084100050323     C*
084200050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
084300050323     C                   if        vaoTCO = 'F'
084400050323     C* Quindi verifico se il codice ordinante non è valorizzato
084500050323     C                   if        vaoCOR = *zeros
084600050323     C*                  if        vaoCOR = *zeros  AND
084700050323     C*                            vaoRSO = *blanks AND
084800050323     C*                            vaoINO = *blanks AND
084900050323     C*                            vaoCAO = *blanks AND
085000050323     C*                            vaoLOO = *blanks AND
085100050323     C*                            vaoPRO = *blanks AND
085200050323     C*                            vaoNAO = *blanks
085300050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
085400050323     C                   move      vlrKSC        wCli              7 0
085500050323     C                   movel(p)  wCli          acrCRO
085600050323     C     acrCRO        setll     fnacr01l
085700050323     C                   if        %found(fnacr01l)
085800050323     C                   read      fnacr01l
085900050323     C                   dow       not %eof(fnacr01l)
086000050323     C                   movel     acrCRO        wCliAcr           7 0
086100050323     C                   if        wCliAcr = wCli
086200050323     C                   eval      vaoCOR = acrCRO
086300050323     C                   leave
086400050323     C                   else
086500050323     C                   leave
086600050323     C                   endif
086700050323     C                   read      fnacr01l
086800050323     C                   enddo
086900050323     C                   endif
087000050323     C                   endif
087100050323     C                   endif
087200050323     C*
087300050323     C                   if        vaoTCO = 'I'
087400050323     C* Quindi verifico se l'immissine via Internet è stata effettuata da 1 cliente NN codificato
087500050323     C                   if        vlrKSC = '0ORM0000'
087600050323     C                   eval      vaoRSO = 'ORDINANTE SENZA PASSWORD'
087700061031     C***                eval      vaoINO = 'ORM DA INTERNET'
087800061031     C                   movel     ConstINO      vaoINO
087900050323     C                   eval      vaoCAO = '40138'
088000050323     C                   eval      vaoLOO = 'BOLOGNA'
088100050323     C                   eval      vaoPRO = 'BO'
088200050323     C                   endif
088300050323     C                   endif
088400050323     C*
088500050323     C                   ENDSR
088600990920
088700010528
088800011112
088900011112     C*------------------------------------------------------------------------*
089000011112     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
089100011112     C*------------------------------------------------------------------------*
089200011112     C     WRIORI        BEGSR
089300011112     C*
089400090617     C* Come prima cosa stacco un numeratore da AZNUM
089500090617     C                   clear                   trul33ds
089600090617     C                   eval      I33OPE = *zeros
089700090617     C                   eval      I33CNU = 600
089800090617     C                   eval      I33NUM = 1
089900090617     C                   movel     TRUL33DS      KPJBU
090000090617     C                   call      'TRUL33R'
090100090617     C                   parm                    KPJBA
090200090617     C                   movel     KPJBU         TRUL33DS
090300090617     C                   if        O33ERR = *zeros
090400090617     C                   z-add     O33NRF        §numori
090500090618     c                   else
090600090618     c                   eval      §numori = *all'9'
090700011112     C                   endif
090800070925     C*
090900070925     C* Giro "standard" (ovvero x cliente d invio dati)
091000070925     C                   if        *in51 = *off
091100011112     C*
091200011122     C* Qindi imposto i campi "extra-VAO"
091300011122     C                   if        vlrKSC = '0ORM0000'
091400011122     C*
091500011122     C                   if        vaoCOR > *zeros
091600011122     C                   movel     vaoCOR        wrkCOR7           7 0
091700011122     C*
091800011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
091900011122     C* codice padre. Se è un padre assegno già il codice ordinante.
092000011122     C                   clear                   tibs10ds
092100011122     C                   eval      d10drf = datcor                              *data riferimento
092200011122     C                   eval      d10paf = 'P'                                 *cerca il padre
092300011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
092400091026     C*
092500091026     C* Preparazione chiamata al TIBS10R1
092600091026     C                   clear                   tibs10r1ds
092700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
092800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
092900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
093000091026     C*
093100091026     C                   call      'TIBS10R1'
093200091026     C                   parm                    tibs10r1ds
093300091026     C*
093400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
093500011122     C*
093600011122     C                   if        d10err <> *blanks or                         *No unif. o padre
093700011122     C                             d10cop =  *zeros  or
093800011122     C                             d10cop =  d10cod
093900011122     C                   move      d10cod        oriIDC
094000011122     C                   else                                                   *Figlio
094100011122     C                   move      d10cop        oriIDC
094200011122     C                   endif
094300011122     C*
094400011122     C                   else
094500011122     C                   eval      oriIDC = *zeros
094600011122     C                   endif
094700011122     C*
094800011122     C                   else
094900011122     C                   eval      oriIDC = vlrKSC
095000011122     C                   endif
095100070925     C*
095200070925     C* Giro "forzato" (ovvero x codice ordinante)
095300070925     C                   else
095400070925     C*
095500070925     C                   if        vaoCOR > *zeros
095600070925     C                   movel     vaoCOR        wrkCOR7           7 0
095700070925     C*
095800070925     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
095900070925     C* codice padre. Se è un padre assegno già il codice ordinante.
096000070925     C                   clear                   tibs10ds
096100070925     C                   eval      d10drf = datcor                              *data riferimento
096200070925     C                   eval      d10paf = 'P'                                 *cerca il padre
096300070925     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
096400091026     C*
096500091026     C* Preparazione chiamata al TIBS10R1
096600091026     C                   clear                   tibs10r1ds
096700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
096800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
096900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
097000091026     C*
097100091026     C                   call      'TIBS10R1'
097200091026     C                   parm                    tibs10r1ds
097300091026     C*
097400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
097500070925     C*
097600070925     C                   if        d10err <> *blanks or                         *No unif. o padre
097700070925     C                             d10cop =  *zeros  or
097800070925     C                             d10cop =  d10cod
097900070925     C                   move      d10cod        oriIDC
098000070925     C                   else                                                   *Figlio
098100070925     C                   move      d10cop        oriIDC
098200070925     C                   endif
098300070926     C*
098400070926     C* Controllo che il codice ordinante "padre" appena reperito sia effettivamente abilitato
098500070926     C* al servizio d rintraccia ritiri on-line
098600070926     C                   eval      vssKSU = oriIDC
098700070926     C                   eval      vssISV = 'T3'
098800070926     C     keyvss02_p    chain     tivss02l
098900070926     C                   if        %found(tivss02l)
099000070926     C                   if        vssdde <= datcor AND
099100070926     C                             datcor <= vssdsc
099200070926     C                   else
099300070926     C                   eval      oriIDC = vlrKSC
099400070926     C                   endif
099500070926     C                   else
099600070926     C                   eval      oriIDC = vlrKSC
099700070926     C                   endif
099800070926     C*
099900070926     C* Controllo che il codice ordinante indicato sul file sia effettivamente presente in
100000070926     C* anagrafica clienti ritiro
100100070926     C                   setoff                                       52
100200070926     C                   eval      acrCRO = wrkCOR7 * 1000
100300070926     C     acrCRO        setll     fnacr01l
100400070926     C                   if        %found(fnacr01l)
100500070926     C                   read      fnacr01l
100600070926     C                   dow       not %eof(fnacr01l)
100700070926     C                   movel     acrCRO        wCliAcr           7 0
100800070926     C                   if        wCliAcr = wrkCOR7
100900070926     C                   seton                                        52
101000070926     C                   leave
101100070926     C                   else
101200070926     C                   leave
101300070926     C                   endif
101400070926     C                   read      fnacr01l
101500070926     C                   enddo
101600070926     C                   endif
101700070926     C*
101800070926     C                   if        *in52  = *off
101900070926     C                   eval      oriIDC = vlrKSC
102000070926     C                   endif
102100070925     C*
102200070925     C                   else
102300070925     C                   if        vlrKSC = '0ORM0000'
102400070925     C                   eval      oriIDC = *zeros
102500070925     C                   else
102600070925     C                   eval      oriIDC = vlrKSC
102700070925     C                   endif
102800070925     C                   endif
102900070925     C                   endif
103000011122     C*
103100011112     C                   eval      oriPRG = §numori
103200011112     C                   eval      oriDER = datcor
103300011112     C                   eval      oriOER = wn6
103400011112     C                   eval      oriPOE = vaoPOE
103500011112     C                   eval      oriNSR = vaoNSR
103600011112     C                   eval      oriNOR = vaoNOR
103700011112     C                   eval      oriNRV = vaoNRV
103800011112     C                   eval      oriFLO = *blanks
103900011112     C*
104000011112     C* Infine valorizzo la chieve esterna sul file FNVAO
104100011112     C                   eval      dorm01 = vaoFLO
104200011112     C                   movel     §numori       §ormpg
104300011112     C                   eval      vaoFLO = dorm01
104400011112     C*
104500011112     C* ...quindi scrivo il file TIORI00F
104600011112     C                   WRITE     TIORI000
104700011112     C*
104800011112     C                   ENDSR
104900050826
105000050826
105100050826
105200050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
105300050826     C     wriVGD        BEGSR
105400050826     C*
105500050826     C* Reperisco la descrizione della fase da tabella
105600050826     C                   movel(p)  'FAR'         KteCOD
105700050826     C                   movel(p)  '000'         KteKE1
105800050826     C     KEYtbe        chain     tntbe01l
105900050826     C                   if        %found(tntbe01l)
106000050826     C                   movel(P)  tbeuni        DFAR
106100050826     C                   else
106200050826     C                   clear                   DFAR
106300050826     C                   endif
106400050826     C*
106500050826     C* Valorizzo buffer record
106600050826     C                   clear                   FNVAPDS
106700050826     C                   eval      vapIDC = oriIDC
106800050826     C                   eval      vapPOE = oriPOE
106900050826     C                   eval      vapRFA = vaoRFA
107000050826     C                   eval      vapPOG = oriPOE
107100050826     C                   eval      vapDAE = oriDER
107200050826     C                   eval      vapORE = oriOER
107300050826     C                   if        d§fardva <> *blanks
107400050826     C                   eval      vapDFA = d§fardva
107500050826     C                   else
107600050826     C                   eval      vapDFA = d§fardes
107700050826     C                   endif
107800050826     C*
107900050826     C                   clear                   tivgd000
108000080407     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
108100050826     C                   eval      vgdTIP = 'VP'
108200050826     C                   eval      vgdKSU = vapIDC
108300050826     C                   eval      vgdTSC = 'WW'
108400050826     C                   eval      vgdDAT = datcor
108500050826     C                   eval      vgdPGM = 'TIS7T11R'
108600060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
108700060306     C                   exsr      sr_Control
108800060306     C                   if        Cli_Abil = *on
108900060306     C                   eval      vgdKSU = woriIDC
109000060306     C                   write     tivgd000
109100060306     C                   endif
109200050826     C*
109300050826     C                   ENDSR
109400050826     C*------------------------------------------------------------------------*
109500011119
109600010528
109700010528
109800010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
109900010528     C     repfil        BEGSR
110000010528     C*
110100010528     C                   if        invfil = *zeros and
110200010528     C                             depfil > *zeros and
110300010613     C                             (vinflg = *blanks or
110400010613     C                              vinflg = *zeros)
110500010528     C
110600010528     C                   eval      invfil = depfil
110700010528     C                   endif
110800010528     C*
110900010529     C                   if        depfil <> invfil and
111000010529     C                             invfil > *zeros
111100010528     C                   eval      flgMulti = '1'
111200010528     C                   if        vinflg = *blanks
111300010528     C                   add       1             cntNonEl
111400010528     C                   endif
111500010528     C                   endif
111600010528     C*
111700010528     C                   if        vinflg = '2'
111800010528     C                   eval      flgStato = '2'
111900010528     C                   endif
112000010528     C*
112100010528     C                   ENDSR
112200010528     C***
112300010528
112400010528
112500010528
112600010528
112700990920      /TITLE Invio dei dati al punto operativo.
112800000613     C     invio         BEGSR
112900990920     C*
113000990920     C                   reset                   dscmz
113100010528     C                   move      invfil        cmzdst
113200990920     C                   eval      cmzfld = vlrfou
113300990920     C                   eval      cmzmbd = vlrhdl
113400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
113500000710     C                   if        prmfir = *blanks
113600010122     C                   eval      cmzfla = 'FNVAO00F'
113700010122     C                   eval      cmzmba = 'FNVAO00F'
113800000710     C                   else
113900000710     C                   eval      cmzfla = prmfir
114000000710     C                   eval      cmzmba = prmfir
114100000710     C                   endif
114200990920     C                   eval      cmznrr = *zeros
114300990920     C                   move      §ctrok        cmznrr
114400010123     C                   eval      cmzlba = vlrfl1
114500990920     C                   call(e)   'TIS711C'
114600990920     C                   parm                    dscmz
114700990921     C                   parm      *blanks       esito
114800990920     C                   if        %error
114900990920     C                             or cmzerr = '1'
115000990921     C                             or esito  = '1'
115100000710     C                   eval      wrkesito = '3'
115200990920     C                   endif
115300990920     C*
115400000613     C                   ENDSR
115500000613     C***
115600990910
115700010528
115800010528
115900010528
116000010528      /TITLE Invio dei dati al punto operativo.
116100010528     C     opeini        BEGSR
116200010528     C*
116300010528     C* Inizializzo flag e contatori operativi
116400010528     C                   movel     '0'           flgGiro           1
116500010528     C                   movel     '0'           flgMulti          1
116600010528     C                   movel     '1'           flgStato          1
116700010615     C                   movel     '0'           flgOk             1
116800010528     C                   z-add     *zeros        cntNonEl         10 0
116900010528     C                   z-add     *zeros        depfil            3 0
117000010528     C                   z-add     *zeros        invfil            3 0
117100030407     C*
117200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
117300030407     C                   if        %subst(vlrppt:1:1) = 'S'
117400030407     C                   seton                                        50
117500030407     C                   else
117600030407     C                   setoff                                       50
117700030407     C                   endif
117800070925     C*
117900070925     C* Verifico se sui parametri del traduttore è richiesta la valorizzazione d IDC da COR
118000070925     C                   if        %subst(vlrppt:2:1) = 'S'
118100070925     C                   seton                                        51
118200070925     C                   else
118300070925     C                   setoff                                       51
118400070925     C                   endif
118500010528     C*
118600010528     C                   ENDSR
118700010528     C***
118800060306
118900060306
119000060306     C*-------------------------------------------------------------------------*
119100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
119200060306     C*-------------------------------------------------------------------------*
119300060306     C     sr_Control    begsr
119400060306     C*
119500060306     C* inizializzo a ok il flag d controllo abilitazione
119600060306     C                   eval      Cli_Abil = *on
119700060306     C
119800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
119900060306     C                   clear                   woriIDC
120000060306     C                   eval      kteCOD = 'VAP'
120100060306     C                   eval      kteKE1 = oriIDC
120200060306     C     keytbe        chain     TNTBE01L
120300060306     C                   if        %found(Tntbe01l)
120400060306     C                   eval      woriIDC = TbeKe2
120500060306     C                   else
120600060306     C                   eval      Cli_Abil = *off
120700060306     C                   endif
120800060306     C*
120900060306     C                   endsr
121000010528
121100010528
121200010528
121300010528
121400000613     C     *inzsr        BEGSR
121500990910     C*
121600990910     C     *entry        plist
121700990920     C                   parm                    tivlrds
121800990921     C                   parm      wrkesito      esito
121900000724     C                   parm                    prmlit
122000000710     C                   parm                    prmfir
122100011112     C*
122200011112     C* Campi riferito al database
122300011112     C     *like         define    tbecod        kteCOD                         *TNTBE01L
122400011112     C     *like         define    tbeke1        kteKE1
122500011112     C*
122600011112     C* Definizione chiavi di lettura
122700011112     C* TNTBE01L - parziale
122800011112     C     keytbe        klist
122900011112     C                   kfld                    kteCOD                         *UTENTE
123000011112     C                   kfld                    kteKE1                         *CODICE
123100070926     C* TIVSS02L - parziale
123200070926     C     keyvss02_p    klist
123300070926     C                   kfld                    vssKSU                         *CLIENTE UNIF.
123400070926     C                   kfld                    vssISV                         *SERVIZIO
123500011112     C*
123600011112     C* Reperimento data e ora corrente
123700011112     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
123800011112     C                   movel     wn14          wn6               6 0          *ORA (6)
123900011112     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
124000011112     C                   z-add     wn8           g08dat
124100011112     C                   z-add     *zeros        g08inv
124200011112     C                   movel     '0'           g08err
124300011112     C                   call      'XSRDA8'
124400011112     C                   parm                    wlbda8
124500011112     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
124600010529     C*
124700000613     C                   ENDSR
124800000613     C***
