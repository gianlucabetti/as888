000100010122      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500090618     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700010122     FFNVAOwwr  O    E             DISK    usropn
000800011105     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000010122     Ftis7t7p   O    f  132        PRINTER usropn
001100000621     F                                     oflind(*inoa)
001200010122     Ftis7t7ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200020730     D  procname         *PROC
002300941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002400941229     D*  6+1
002500941229     D                 DS
002600941229     D  WS09                   1      1
002700941229     D  WI09                   2      7
002800941229     D  WV09                   8      8
002900941229     D  WD09                   9      9
003000941229     D  WASC09                 1      9
003100941229     D*  2+3
003200941229     D                 DS
003300941229     D  WS07                   1      1
003400941229     D  WI07                   2      3
003500941229     D  WV07                   4      4
003600941229     D  WD07                   5      7
003700941229     D  WASC07                 1      7
003800010122     D*
003900990920     D tivlrds       e ds                  extname(tivlr00f)
004000050826     D fnvapds       e ds                  extname(fnvap00f)
004100010122     D fnvaoaar      e ds                  inz
004200011105     D                                     prefix(a_)
004300011105     D dorm01        e ds
004400011119     D dfar          e ds
004500090618     d trul33ds      e ds
004600090618     d kpjba         e ds
004700990910     D esito           s              1
004800000724     D prmlit          s             10
004900000710     D prmfir          s             10
005000990921     D wrkesito        s                   like(esito)
005100990915     D wrkdata         s               d
005200990915     D wrkora          s               t
005300000613     D rrnum           s              6  0 INZ(*zeros)
005400000621     D recko           s            150    INZ(*blanks)
005500011123     D depcmd          s            150    INZ(*blanks)
005600090618     d §numori         s              7  0 inz
005700011105     D*-------------------
005800011105     D* DS "XSRDA8" - controllo data (8)
005900011105     D*-------------------
006000011105     D wlbda8          DS                  INZ
006100011105     D  g08dat                 1      8  0
006200011105     D  g08inv                 9     16  0
006300011105     D  g08err                17     17
006400011105     D  g08tgi                18     22  0
006500011122     D*-------------------
006600010731     D* COSTANTI
006700011122     D*-------------------
006800010731     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900010731     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000990908
007100091026
007200091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007300091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
007400091026
007500010528
007600010528
007700010528
007800990915     C                   time                    wrkdata
007900990915     C                   time                    wrkora
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000724     C*
008400010528     C                   exsr      opeini
008500010528     C                   exsr      rwvao
008600010528     C*
008700010528     C                   seton                                        lr
008800010528
008900010528
009000010528
009100010528
009200010528     C*--------------------------------------------------------
009300010528     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010528     C*--------------------------------------------------------
009500010528     C     PREELA        BEGSR
009600010528     C*
009700000724     C* SE OCCORRE SPEDIRE IN FILIALE
009800010528     C                   if        invfil <> *zeros and
009900010528     C                             flgGiro = '0'
010000010528     C*
010100010528     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010528     C                   eval      flgGiro = '1'
010300000724     C*
010400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010600010529     C     invfil        chain     azorg01l
010700000724     C                   if        %found
010800000616     C                   movel(p)  CMD(1)        depcmd
010900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011000000616     C*
011100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011200011123     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    depcmd
011500000616     C                   PARM                    LENGH
011600000724     C*
011700000724     C                   endif
011800000616     C*
011900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012000000616     C                   movel(p)  CMD(2)        depcmd
012100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012200000616     C*
012300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012400011123     C                   Z-ADD     150           LENGH            15 5
012500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    depcmd
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010122     C                   if        not %open(tis7t7ps)
013000010122     C                   open      tis7t7ps
013100000616     C                   except    testdett
013200010528     C                   endif
013300010529     C*
013400010529     C                   endif
013500010528     C*
013600010528     C                   ENDSR
013700010528     C***
013800010528
013900010528
014000010528
014100010528
014200010528
014300010528
014400010528     C*--------------------------------------------------------
014500010528     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010528     C*--------------------------------------------------------
014700010528     C     ENDELA        BEGSR
014800000613     C*
014900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015000000613     C*
015100010122     C                   if        %open(tis7t7ps)
015200000616     C                   except    findett
015300010122     C                   close     tis7t7ps
015400000613     C                   endif
015500000616     C*
015600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015700011123     C                   Z-ADD     150           LENGH            15 5
015800010529     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015900000616     C                   PARM                    CMD(3)
016000000616     C                   PARM                    LENGH
016100000613     C*
016200010528     C                   ENDSR
016300010528     C***
016400010528
016500010528
016600010528
016700000613
016800000613
016900000613     C*--------------------------------------------------------
017000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017100000613     C*--------------------------------------------------------
017200000621     C     STPR          BEGSR
017300000613     C*
017400010122     C                   if        not %open(tis7t7p)
017500010122     C                   open      tis7t7p
017600990915     C                   endif
017700990915     C*
017800990915     C                   except    riepilogo
017900990915     C*
018000010122     C                   if        %open(tis7t7p)
018100010122     C                   close     tis7t7p
018200990914     C                   endif
018300990910     C*
018400000613     C                   ENDSR
018500000613     C***
018600990908
018700010528
018800010528
018900010528
019000010528
019100910830     C*--------------------------------------------------------
019200010122     C* RWVAO   LEGGE tivin00r E SCRIVE FNVAOWWR              *
019300910830     C*--------------------------------------------------------
019400010122     C     RWVAO         BEGSR
019500010528     C*
019600990914     C                   if        not %open(tivin00r)
019700990908     C                   open      tivin00r
019800990914     C                   endif
019900010122     C                   if        not %open(fnvaowwr)
020000010122     C                   open      fnvaowwr
020100990914     C                   endif
020200990910     C*
020300990910     C                   clear                   §CTROK
020400990910     C                   clear                   §CTRMO
020500990910     C                   clear                   §CTRNO
020600990910     C*
020700921023     C                   DO        *HIVAL
020800990913     C*
020900990915     C                   READ      tivin00r                               70
021000010731     C*
021100010731     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
021200010731     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
021300010618     C*
021400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021500010618     C                   if        vinflg = '1'
021600010618     C                   eval      flgOk = '1'
021700010618     C                   endif
021800010618     C*
021900000905     C                   if        vindta > *blanks
022000000613     C                   add       1             rrnum
022100990913     C*
022200010711     C                   if        *in70 = *off and
022300010711     C                             (vinflg = *blanks
022400010711     C                              or vinflg = '0'
022500010711     C                              or vinflg = '2')
022600990913     C*
022700010528     C                   clear                   FNVAO000
022800011105     C                   clear                   TIORI000
022900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023000010711     C                   if        vinflg = *blanks or vinflg = '0'
023100010711     C                   clear                   vinmsg
023200010711     C                   endif
023300010711     C*
023400010122     C                   eval      fnvaoaar = vindta
023500991022     C*
023600010122     C                   testb     x'3F'         a_vaoatb                 02
023700991022     C                   if        not *in02
023800991022     C*
023900971021     C                   SETOFF                                       303132
024000971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024100971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
024200971023     C*    OPPURE TUTTI BLANKS
024300010122     C*-------------------
024400010122     C* IN31 => Campo obbligatorio, escludo il record e lo marco in errore
024500010719     C* IN32 => Campo facoltativo, muovo tutti zero e passo comunque il record marcandolo come OK
024600010122     C*-------------------
024700010122     C*
024800011119 xxx C*                  TESTN                   a_vaopoe             30
024900010417  "  C* N30              TESTN                   a_vaopoe               30
025000010417  "  C* N30              TESTN                   a_vaopoe                 30
025100010417     C*                  MOVE      a_vaopoe      W001A             1
025200010417     C*    *IN30         IFEQ      *OFF
025300010417     C*    W001A         ORLT      '0'
025400010417     C*                  SETON                                        32
025500010417     C*                  MOVE      *ZEROS        VAOPOE
025600010417     C*                  EVAL      vinmsg = 'VAOPOE'
025700010417     C*                  ELSE
025800010417     C*                  MOVE      a_vaopoe      VAOPOE
025900010417     C*                  ENDIF
026000010417  "  C*
026100010417  "  C* FORZATURA IMPOSTAZIOINE PUNTO OPERATIVO MITTENTE
026200010604 xxx C                   IF        vlrpoi <> 999
026300010528     C                   EVAL      VAOPOE = vlrpoi
026400010528     C                   ELSE
026500010528     C                   IF        a_vaopoe = *blanks or
026600011010     C                             a_vaopoe = *zeros  or
026700011010     C                             a_vaopoe = ' 000'
026800010528     C                   IF        a_vaopor <> *blanks and
026900011010     C                             a_vaopor <> *zeros  and
027000011010     C                             a_vaopor <> ' 000'
027100010528     C                   MOVE      a_vaopor      VAOPOE
027200010528     C                   ELSE
027300011010     C                   EVAL      a_vaopoe = %subst(vlrksc:1:4)
027400010528     C                   MOVE      a_vaopoe      VAOPOE
027500010528     C                   ENDIF
027600010529     C                   ELSE
027700010529     C                   MOVE      a_vaopoe      VAOPOE
027800010528     C                   ENDIF
027900010528     C                   ENDIF
028000010122     C*
028100010528     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028200010528     C                   eval      depfil = VAOPOE
028300010528     C                   exsr      repfil
028400010528     C                   if        depfil = invfil and
028500010528     C                             (vinflg = *blanks
028600010528     C                              or vinflg = '0'
028700010528     C                              or vinflg = '2')
028800010528     C*
028900010528     C                   exsr      PREELA
029000010710     C*
029100010719     C*                  TESTN                   a_vaonsr             30
029200010719     C* N30              TESTN                   a_vaonsr               30
029300010719     C* N30              TESTN                   a_vaonsr                 30
029400010719     C*                  MOVE      a_vaonsr      W001A             1
029500010719     C*    *IN30         IFEQ      *OFF
029600010719     C*    W001A         ORLT      '0'
029700010719     C*                  SETON                                        32
029800010719     C*                  MOVE      *ZEROS        VAONSR
029900010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONSR'
030000010719     C*                  ELSE
030100010719     C*                  MOVE      a_vaonsr      VAONSR
030200010719     C*                  ENDIF
030300010122     C*
030400010719     C*                  TESTN                   a_vaonor             30
030500010719     C* N30              TESTN                   a_vaonor               30
030600010719     C* N30              TESTN                   a_vaonor                 30
030700010719     C*                  MOVE      a_vaonor      W001A             1
030800010719     C*    *IN30         IFEQ      *OFF
030900010719     C*    W001A         ORLT      '0'
031000010719     C*                  SETON                                        32
031100010719     C*                  MOVE      *ZEROS        VAONOR
031200010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONOR'
031300010719     C*                  ELSE
031400010719     C*                  MOVE      a_vaonor      VAONOR
031500010719     C*                  ENDIF
031600010122     C*
031700010719     C*                  TESTN                   a_vaonrv             30
031800010719     C* N30              TESTN                   a_vaonrv               30
031900010719     C* N30              TESTN                   a_vaonrv                 30
032000010719     C*                  MOVE      a_vaonrv      W001A             1
032100010719     C*    *IN30         IFEQ      *OFF
032200010719     C*    W001A         ORLT      '0'
032300010719     C*                  SETON                                        32
032400010719     C*                  MOVE      *ZEROS        VAONRV
032500010719     C*                  EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONRV'
032600010719     C*                  ELSE
032700010719     C*                  MOVE      a_vaonrv      VAONRV
032800010719     C*                  ENDIF
032900010122     C*
033000010122     C                   TESTN                   a_vaodao             30
033100010122     C  N30              TESTN                   a_vaodao               30
033200010122     C  N30              TESTN                   a_vaodao                 30
033300010122     C                   MOVE      a_vaodao      W001A             1
033400010122     C     *IN30         IFEQ      *OFF
033500010122     C     W001A         ORLT      '0'
033600010122     C                   SETON                                        32
033700010122     C                   MOVE      *ZEROS        VAODAO
033800010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAO'
033900010122     C                   ELSE
034000010122     C                   MOVE      a_vaodao      VAODAO
034100010122     C                   ENDIF
034200010122     C*
034300010122     C                   TESTN                   a_vaooao             30
034400010122     C  N30              TESTN                   a_vaooao               30
034500010122     C  N30              TESTN                   a_vaooao                 30
034600010122     C                   MOVE      a_vaooao      W001A             1
034700010122     C     *IN30         IFEQ      *OFF
034800010122     C     W001A         ORLT      '0'
034900010122     C                   SETON                                        32
035000010122     C                   MOVE      *ZEROS        VAOOAO
035100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOOAO'
035200010122     C                   ELSE
035300010122     C                   MOVE      a_vaooao      VAOOAO
035400010122     C                   ENDIF
035500010122     C*
035600010122     C                   TESTN                   a_vaocor             30
035700010122     C  N30              TESTN                   a_vaocor               30
035800010122     C  N30              TESTN                   a_vaocor                 30
035900010122     C                   MOVE      a_vaocor      W001A             1
036000010122     C     *IN30         IFEQ      *OFF
036100010122     C     W001A         ORLT      '0'
036200010122     C                   SETON                                        32
036300010122     C                   MOVE      *ZEROS        VAOCOR
036400010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCOR'
036500010122     C                   ELSE
036600010122     C                   MOVE      a_vaocor      VAOCOR
036700010122     C                   ENDIF
036800010122     C*
036900010122     C                   TESTN                   a_vaocra             30
037000010122     C  N30              TESTN                   a_vaocra               30
037100010122     C  N30              TESTN                   a_vaocra                 30
037200010122     C                   MOVE      a_vaocra      W001A             1
037300010122     C     *IN30         IFEQ      *OFF
037400010122     C     W001A         ORLT      '0'
037500010122     C                   SETON                                        32
037600010122     C                   MOVE      *ZEROS        VAOCRA
037700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRA'
037800010122     C                   ELSE
037900010122     C                   MOVE      a_vaocra      VAOCRA
038000010122     C                   ENDIF
038100010122     C*
038200010719     C                   IF        a_vaocrc = *blanks or
038300010719     C                             a_vaocrc = *zeros
038400010719     C                   ELSE
038500010719     C                   TESTN                   a_vaocrc             30
038600010719     C  N30              TESTN                   a_vaocrc               30
038700010719     C  N30              TESTN                   a_vaocrc                 30
038800010719     C                   MOVE      a_vaocrc      W001A             1
038900010719     C     *IN30         IFEQ      *OFF
039000010719     C     W001A         ORLT      '0'
039100010719     C                   SETON                                        32
039200010719     C                   MOVE      *ZEROS        VAOCRC
039300010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCRC'
039400010719     C                   ELSE
039500010719     C                   MOVE      a_vaocrc      VAOCRC
039600010719     C                   ENDIF
039700010719     C                   ENDIF
039800010122     C*
039900010122     C                   TESTN                   a_vaodar             30
040000010122     C  N30              TESTN                   a_vaodar               30
040100010122     C  N30              TESTN                   a_vaodar                 30
040200010122     C                   MOVE      a_vaodar      W001A             1
040300010122     C     *IN30         IFEQ      *OFF
040400010122     C     W001A         ORLT      '0'
040500010122     C                   SETON                                        32
040600010122     C                   MOVE      *ZEROS        VAODAR
040700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAODAR'
040800010122     C                   ELSE
040900010122     C                   MOVE      a_vaodar      VAODAR
041000010122     C                   ENDIF
041100010122     C*
041200010122     C                   TESTN                   a_vaoorr             30
041300010122     C  N30              TESTN                   a_vaoorr               30
041400010122     C  N30              TESTN                   a_vaoorr                 30
041500010122     C                   MOVE      a_vaoorr      W001A             1
041600010122     C     *IN30         IFEQ      *OFF
041700010122     C     W001A         ORLT      '0'
041800010122     C                   SETON                                        32
041900010122     C                   MOVE      *ZEROS        VAOORR
042000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOORR'
042100010122     C                   ELSE
042200010122     C                   MOVE      a_vaoorr      VAOORR
042300010122     C                   ENDIF
042400010122     C*
042500010122     C                   TESTN                   a_vaoncl             30
042600010122     C  N30              TESTN                   a_vaoncl               30
042700010122     C  N30              TESTN                   a_vaoncl                 30
042800010122     C                   MOVE      a_vaoncl      W001A             1
042900010122     C     *IN30         IFEQ      *OFF
043000010122     C     W001A         ORLT      '0'
043100010122     C                   SETON                                        32
043200010122     C                   MOVE      *ZEROS        VAONCL
043300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAONCL'
043400010122     C                   ELSE
043500010122     C                   MOVE      a_vaoncl      VAONCL
043600010122     C                   ENDIF
043700010122     C*
043800010122     C                   MOVE      *ZERO         VAOPKG
043900010122     C                   MOVE      a_vaopkg      WASC09
044000010122     C                   TESTN                   WI09                 30
044100010122     C  N30              TESTN                   WI09                   30
044200010122     C  N30              TESTN                   WI09                     30
044300010122     C                   MOVE      WI09          W001A             1
044400010122     C     *IN30         IFEQ      *OFF
044500010122     C     W001A         ORLT      '0'
044600010122     C     W001A         ANDNE     *BLANK
044700010122     C                   SETON                                        32
044800010122     C                   MOVE      *ZERO         VAOPKG
044900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
045000010122     C                   ELSE
045100010122     C                   MOVEL     WI09          VAOPKG
045200010122     C                   ENDIF
045300010122     C                   TESTN                   WD09                 30
045400010122     C  N30              TESTN                   WD09                     30
045500010122     C                   MOVE      WD09          W001A             1
045600010122     C     *IN30         IFEQ      *OFF
045700010122     C     W001A         ORLT      '0'
045800010122     C     W001A         ANDNE     *BLANK
045900010122     C                   SETON                                        32
046000010122     C                   MOVE      *ZERO         VAOPKG
046100010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPKG'
046200010122     C                   ELSE
046300010122     C                   MOVE      WD09          VAOPKG
046400010122     C                   ENDIF
046500010122     C*
046600010122     C                   MOVE      *ZERO         VAOVLM
046700010122     C                   MOVE      a_vaovlm      WASC07
046800010122     C                   TESTN                   WI07                 30
046900010122     C  N30              TESTN                   WI07                   30
047000010122     C  N30              TESTN                   WI07                     30
047100010122     C                   MOVE      WI07          W001A             1
047200010122     C     *IN30         IFEQ      *OFF
047300010122     C     W001A         ORLT      '0'
047400010122     C     W001A         ANDNE     *BLANK
047500010122     C                   SETON                                        32
047600010122     C                   MOVE      *ZERO         VAOVLM
047700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
047800010122     C                   ELSE
047900010122     C                   MOVEL     WI07          VAOVLM
048000010122     C                   ENDIF
048100010122     C                   TESTN                   WD07                 30
048200010122     C  N30              TESTN                   WD07                   30
048300010122     C  N30              TESTN                   WD07                     30
048400010122     C                   MOVE      WD07          W001A             1
048500010122     C     *IN30         IFEQ      *OFF
048600010122     C     W001A         ORLT      '0'
048700010122     C     W001A         ANDNE     *BLANK
048800010122     C                   SETON                                        32
048900010122     C                   MOVE      *ZERO         VAOVLM
049000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOVLM'
049100010122     C                   ELSE
049200010122     C                   MOVE      WD07          VAOVLM
049300010122     C                   ENDIF
049400010122     C*
049500010122     C                   TESTN                   a_vaobnc             30
049600010122     C  N30              TESTN                   a_vaobnc               30
049700010122     C  N30              TESTN                   a_vaobnc                 30
049800010122     C                   MOVE      a_vaobnc      W001A             1
049900010122     C     *IN30         IFEQ      *OFF
050000010122     C     W001A         ORLT      '0'
050100010122     C                   SETON                                        32
050200010122     C                   MOVE      *ZEROS        VAOBNC
050300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBNC'
050400010122     C                   ELSE
050500010122     C                   MOVE      a_vaobnc      VAOBNC
050600010122     C                   ENDIF
050700010122     C*
050800010122     C                   TESTN                   a_vaoblc             30
050900010122     C  N30              TESTN                   a_vaoblc               30
051000010122     C  N30              TESTN                   a_vaoblc                 30
051100010122     C                   MOVE      a_vaoblc      W001A             1
051200010122     C     *IN30         IFEQ      *OFF
051300010122     C     W001A         ORLT      '0'
051400010122     C                   SETON                                        32
051500010122     C                   MOVE      *ZEROS        VAOBLC
051600010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOBLC'
051700010122     C                   ELSE
051800010122     C                   MOVE      a_vaoblc      VAOBLC
051900010122     C                   ENDIF
052000010122     C*
052100010122     C                   TESTN                   a_vaoatt             30
052200010122     C  N30              TESTN                   a_vaoatt               30
052300010122     C  N30              TESTN                   a_vaoatt                 30
052400010122     C                   MOVE      a_vaoatt      W001A             1
052500010122     C     *IN30         IFEQ      *OFF
052600010122     C     W001A         ORLT      '0'
052700010122     C                   SETON                                        32
052800010122     C                   MOVE      *ZEROS        VAOATT
052900010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOATT'
053000010122     C                   ELSE
053100010122     C                   MOVE      a_vaoatt      VAOATT
053200010122     C                   ENDIF
053300010122     C*
053400010122     C                   TESTN                   a_vaomtc             30
053500010122     C  N30              TESTN                   a_vaomtc               30
053600010122     C  N30              TESTN                   a_vaomtc                 30
053700010122     C                   MOVE      a_vaomtc      W001A             1
053800010122     C     *IN30         IFEQ      *OFF
053900010122     C     W001A         ORLT      '0'
054000010122     C                   SETON                                        32
054100010122     C                   MOVE      *ZEROS        VAOMTC
054200010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOMTC'
054300010122     C                   ELSE
054400010122     C                   MOVE      a_vaomtc      VAOMTC
054500010122     C                   ENDIF
054600010122     C*
054700010719     C                   IF        a_vaoksc = *blanks or
054800010719     C                             a_vaoksc = *zeros
054900010719     C                   ELSE
055000010719     C                   TESTN                   a_vaoksc             30
055100010719     C  N30              TESTN                   a_vaoksc               30
055200010719     C  N30              TESTN                   a_vaoksc                 30
055300010719     C                   MOVE      a_vaoksc      W001A             1
055400010719     C     *IN30         IFEQ      *OFF
055500010719     C     W001A         ORLT      '0'
055600010719     C                   SETON                                        32
055700010719     C                   MOVE      *ZEROS        VAOKSC
055800010719     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOKSC'
055900010719     C                   ELSE
056000010719     C                   MOVE      a_vaoksc      VAOKSC
056100010719     C                   ENDIF
056200010719     C                   ENDIF
056300010122     C*
056400010710     C                   IF        a_vaoctr > *blanks
056500010122     C                   TESTN                   a_vaoctr             30
056600010122     C  N30              TESTN                   a_vaoctr               30
056700010122     C  N30              TESTN                   a_vaoctr                 30
056800010122     C                   MOVE      a_vaoctr      W001A             1
056900010122     C     *IN30         IFEQ      *OFF
057000010122     C     W001A         ORLT      '0'
057100010122     C                   SETON                                        32
057200010627     C                   MOVE      *blanks       VAOCTR
057300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOCTR'
057400010122     C                   ELSE
057500010122     C                   MOVE      a_vaoctr      VAOCTR
057600010122     C                   ENDIF
057700010710     C                   ENDIF
057800010122     C*
057900010122     C                   TESTN                   a_vaopor             30
058000010122     C  N30              TESTN                   a_vaopor               30
058100010122     C  N30              TESTN                   a_vaopor                 30
058200010122     C                   MOVE      a_vaopor      W001A             1
058300010122     C     *IN30         IFEQ      *OFF
058400010122     C     W001A         ORLT      '0'
058500010122     C                   SETON                                        32
058600010122     C                   MOVE      *ZEROS        VAOPOR
058700010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOR'
058800010122     C                   ELSE
058900010122     C                   MOVE      a_vaopor      VAOPOR
059000010122     C                   ENDIF
059100010122     C*
059200010719     C                   IF        a_vaopoc = *blanks or
059300010719     C                             a_vaopoc = *zeros
059400010719     C                   ELSE
059500010122     C                   TESTN                   a_vaopoc             30
059600010122     C  N30              TESTN                   a_vaopoc               30
059700010122     C  N30              TESTN                   a_vaopoc                 30
059800010122     C                   MOVE      a_vaopoc      W001A             1
059900010122     C     *IN30         IFEQ      *OFF
060000010122     C     W001A         ORLT      '0'
060100010122     C                   SETON                                        32
060200010122     C                   MOVE      *ZEROS        VAOPOC
060300010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOPOC'
060400010122     C                   ELSE
060500010122     C                   MOVE      a_vaopoc      VAOPOC
060600010122     C                   ENDIF
060700010719     C                   ENDIF
060800010122     C*
060900010719     C                   IF        a_vaosto = *blanks or
061000010719     C                             a_vaosto = *zeros
061100010719     C                   ELSE
061200010122     C                   TESTN                   a_vaosto             30
061300010122     C  N30              TESTN                   a_vaosto               30
061400010122     C  N30              TESTN                   a_vaosto                 30
061500010122     C                   MOVE      a_vaosto      W001A             1
061600010122     C     *IN30         IFEQ      *OFF
061700010122     C     W001A         ORLT      '0'
061800010122     C                   SETON                                        32
061900010122     C                   MOVE      *ZEROS        VAOSTO
062000010604     C                   EVAL      vinmsg = %trimr(vinmsg) + ' ' + 'VAOSTO'
062100010122     C                   ELSE
062200010122     C                   MOVE      a_vaosto      VAOSTO
062300010122     C                   ENDIF
062400010719     C                   ENDIF
062500971021     C*
062600971021     C* CAMPI ALFABETICI:
062700010122     C                   MOVE      a_vaoatb      VAOATB
062800010122     C                   MOVE      a_vaotor      VAOTOR
062900010531     C*
063000010531     C                   IF        a_vaotco = *blanks
063100010531     C                   MOVE      'F'           VAOTCO
063200010531     C                   ELSE
063300010531     C                   MOVE      a_vaotco      VAOTCO
063400010531     C                   ENDIF
063500010531     C*
063600010122     C                   MOVE      a_vaorso      VAORSO
063700010122     C                   MOVE      a_vaoino      VAOINO
063800010122     C                   MOVE      a_vaocao      VAOCAO
063900010122     C                   MOVE      a_vaoloo      VAOLOO
064000010122     C                   MOVE      a_vaopro      VAOPRO
064100010122     C                   MOVE      a_vaonao      VAONAO
064200010122     C                   MOVE      a_vaorsr      VAORSR
064300010122     C                   MOVE      a_vaoinr      VAOINR
064400010122     C                   MOVE      a_vaocar      VAOCAR
064500010122     C                   MOVE      a_vaolor      VAOLOR
064600010122     C                   MOVE      a_vaoprr      VAOPRR
064700010122     C                   MOVE      a_vaonar      VAONAR
064800010122     C                   MOVE      a_vaorer      VAORER
064900010122     C                   MOVE      a_vaoter      VAOTER
065000010122     C                   MOVE      a_vaorsc      VAORSC
065100010122     C                   MOVE      a_vaoinc      VAOINC
065200010122     C                   MOVE      a_vaocac      VAOCAC
065300010122     C                   MOVE      a_vaoloc      VAOLOC
065400010122     C                   MOVE      a_vaoprc      VAOPRC
065500010122     C                   MOVE      a_vaonac      VAONAC
065600010122     C                   MOVE      a_vaoffd      VAOFFD
065700010122     C                   MOVE      a_vaormp      VAORMP
065800010122     C                   MOVE      a_vaonam      VAONAM
065900010122     C                   MOVE      a_vaopag      VAOPAG
066000010122     C                   MOVE      a_vaozor      VAOZOR
066100010122     C                   MOVE      a_vaono1      VAONO1
066200010122     C                   MOVE      a_vaono2      VAONO2
066300010122     C                   MOVE      a_vaoddt      VAODDT
066400010122     C                   MOVE      a_vaorfa      VAORFA
066500010122     C                   MOVE      a_vaospi      VAOSPI
066600010122     C                   MOVE      a_vaoflo      VAOFLO
066700930409     C*
066800010604     C  N31              ADD       1             §CTROK            7 0
066900010604     C   32              ADD       1             §CTRMO            7 0
067000010604     C   31              ADD       1             §CTRNO            7 0
067100011105     C  N31              EXSR      WRIORI
067200050826     C  N31              EXSR      WRIVGD
067300010122     C  N31              WRITE     FNVAO000
067400990910     C*
067500010604     C                   if        *in31 = *off and
067600010604     C                             *in32 = *off
067700990910     C                   eval      vinflg = '1'
067800990910     C                   else
067900000621     C                   eval      recko = vindta
068000010612     C                   if        %open(tis7t7ps)
068100000616     C                   except    rigadett
068200010612     C                   endif
068300990910     C                   eval      vinflg = '2'
068400990910     C                   endif
068500991022     C                   endif
068600990910     C                   endif
068700010604     C*
068800010604     C                   endif
068900000905     C*
069000000905     C                   else
069100000905     C                   eval      vinflg = '1'
069200000905     C                   endif
069300000905     C*
069400000905     C  N70              update    tivin000
069500991022     C*
069600991022     C  N70              ENDdo
069700010528     C*
069800010528     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
069900010528     C                   if        cntNonEl = *zeros or
070000010528     C                             flgMulti = '0'
070100010528     C* Se non ci sono record con errori ...
070200010528     C                   if        §ctrno = 0 and
070300010604     C                             §ctrmo = 0 and
070400010528     C                             flgStato <> '2'
070500990910     C* ... restituisco esito OK.
070600990921     C                   eval      wrkesito = '0'
070700990910     C                   else
070800000710     C                   if        §ctrok > 0
070900990921     C                   eval      wrkesito = '1'
071000000710     C                   else
071100010615     C                   if        flgOk = '0'
071200010615     C                   eval      wrkesito = '2'
071300010615     C                   else
071400010615     C                   eval      wrkesito = '6'
071500010615     C                   endif
071600990910     C                   endif
071700010528     C                   endif
071800010529     C                   else
071900010529     C                   eval      wrkesito = '9'
072000000710     C                   endif
072100990910     C*
072200990914     C                   if        %open(tivin00r)
072300990908     C                   close     tivin00r
072400990914     C                   endif
072500010122     C                   if        %open(fnvaowwr)
072600010122     C                   close     fnvaowwr
072700990914     C                   endif
072800990910     C*
072900010528     C                   if        vlrpoi <> 999
073000010528     C                   eval      invfil = vlrpoi
073100010528     C                   endif
073200010528     C*
073300990920     C                   if        §ctrok > 0
073400010528     C                             and invfil > *zeros
073500000613     C                   exsr      invio
073600990920     C                   endif
073700010612     C*
073800010612     C                   if        flgGiro = '1'
073900010612     C                   exsr      endela
074000010612     C                   endif
074100010528     C*
074200910830     C                   ENDSR
074300000613     C***
074400990920
074500010528
074600011105     C*------------------------------------------------------------------------*
074700011105     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
074800011105     C*------------------------------------------------------------------------*
074900011105     C     WRIORI        BEGSR
075000011105     C*
075100090618     C* Come prima cosa stacco un numeratore da AZNUM
075200090618     C                   clear                   trul33ds
075300090618     C                   eval      I33OPE = *zeros
075400090618     C                   eval      I33CNU = 600
075500090618     C                   eval      I33NUM = 1
075600090618     C                   movel     TRUL33DS      KPJBU
075700090618     C                   call      'TRUL33R'
075800090618     C                   parm                    KPJBA
075900090618     C                   movel     KPJBU         TRUL33DS
076000090618     C                   if        O33ERR = *zeros
076100090618     C                   z-add     O33NRF        §numori
076200090618     c                   else
076300090618     c                   eval      §numori = *all'9'
076400090618     C                   endif
076500011105     C*
076600011122     C* Quindi imposto i campi "extra-VAO"
076700011122     C                   if        vlrKSC = '0ORM0000'
076800011122     C*
076900011122     C                   if        vaoCOR > *zeros
077000011122     C                   movel     vaoCOR        wrkCOR7           7 0
077100011122     C*
077200011122     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
077300011122     C* codice padre. Se è un padre assegno già il codice ordinante.
077400011122     C                   clear                   tibs10ds
077500011122     C                   eval      d10drf = datcor                              *data riferimento
077600011122     C                   eval      d10paf = 'P'                                 *cerca il padre
077700011122     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
077800091026     C*
077900091026     C* Preparazione chiamata al TIBS10R1
078000091026     C                   clear                   tibs10r1ds
078100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
078200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
078300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
078400091026     C*
078500091026     C                   call      'TIBS10R1'
078600091026     C                   parm                    tibs10r1ds
078700091026     C*
078800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
078900011122     C*
079000011122     C                   if        d10err <> *blanks or                         *No unif. o padre
079100011122     C                             d10cop =  *zeros  or
079200011122     C                             d10cop =  d10cod
079300011122     C                   move      d10cod        oriIDC
079400011122     C                   else                                                   *Figlio
079500011122     C                   move      d10cop        oriIDC
079600011122     C                   endif
079700011122     C*
079800011122     C                   else
079900011122     C                   eval      oriIDC = *zeros
080000011122     C                   endif
080100011122     C*
080200011122     C                   else
080300011122     C                   eval      oriIDC = vlrKSC
080400011122     C                   endif
080500011122     C*
080600011105     C                   eval      oriPRG = §numori
080700011105     C                   eval      oriDER = datcor
080800011105     C                   eval      oriOER = wn6
080900011105     C                   eval      oriPOE = vaoPOE
081000011105     C                   eval      oriNSR = vaoNSR
081100011105     C                   eval      oriNOR = vaoNOR
081200011105     C                   eval      oriNRV = vaoNRV
081300011105     C                   eval      oriFLO = *blanks
081400011105     C*
081500011122     C* Infine valorizzo la chiave esterna sul file FNVAO
081600011105     C                   eval      dorm01 = vaoFLO
081700011105     C                   movel     §numori       §ormpg
081800011105     C                   eval      vaoFLO = dorm01
081900011105     C*
082000011105     C* ...quindi scrivo il file TIORI00F
082100011105     C                   WRITE     TIORI000
082200011105     C*
082300011105     C                   ENDSR
082400050826
082500050826
082600050826
082700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
082800050826     C     wriVGD        BEGSR
082900050826     C*
083000050826     C* Reperisco la descrizione della fase da tabella
083100050826     C                   movel(p)  'FAR'         KteCOD
083200050826     C                   movel(p)  '000'         KteKE1
083300050826     C     KEYtbe        chain     tntbe01l
083400050826     C                   if        %found(tntbe01l)
083500050826     C                   movel(P)  tbeuni        DFAR
083600050826     C                   else
083700050826     C                   clear                   DFAR
083800050826     C                   endif
083900050826     C*
084000050826     C* Valorizzo buffer record
084100050826     C                   clear                   FNVAPDS
084200050826     C                   eval      vapIDC = oriIDC
084300050826     C                   eval      vapPOE = oriPOE
084400050826     C                   eval      vapRFA = vaoRFA
084500050826     C                   eval      vapPOG = oriPOE
084600050826     C                   eval      vapDAE = oriDER
084700050826     C                   eval      vapORE = oriOER
084800050826     C                   if        d§fardva <> *blanks
084900050826     C                   eval      vapDFA = d§fardva
085000050826     C                   else
085100050826     C                   eval      vapDFA = d§fardes
085200050826     C                   endif
085300050826     C*
085400050826     C                   clear                   tivgd000
085500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
085600050826     C                   eval      vgdTIP = 'VP'
085700050826     C                   eval      vgdKSU = vapIDC
085800050826     C                   eval      vgdTSC = 'WW'
085900050826     C                   eval      vgdDAT = datcor
086000050826     C                   eval      vgdPGM = 'TITV_VO'
086100050826     C                   write     tivgd000
086200050826     C*
086300050826     C                   ENDSR
086400050826     C*------------------------------------------------------------------------*
086500011119
086600010528
086700010528      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
086800010528     C     repfil        BEGSR
086900010528     C*
087000010528     C                   if        invfil = *zeros and
087100010528     C                             depfil > *zeros and
087200010613     C                             (vinflg = *blanks or
087300010613     C                              vinflg = *zeros)
087400010528     C
087500010528     C                   eval      invfil = depfil
087600010528     C                   endif
087700010528     C*
087800010529     C                   if        depfil <> invfil and
087900010529     C                             invfil > *zeros
088000010528     C                   eval      flgMulti = '1'
088100010528     C                   if        vinflg = *blanks
088200010528     C                   add       1             cntNonEl
088300010528     C                   endif
088400010528     C                   endif
088500010528     C*
088600010528     C                   if        vinflg = '2'
088700010528     C                   eval      flgStato = '2'
088800010528     C                   endif
088900010528     C*
089000010528     C                   ENDSR
089100010528     C***
089200010528
089300010528
089400010528
089500010528
089600990920      /TITLE Invio dei dati al punto operativo.
089700000613     C     invio         BEGSR
089800990920     C*
089900990920     C                   reset                   dscmz
090000010528     C                   move      invfil        cmzdst
090100990920     C                   eval      cmzfld = vlrfou
090200990920     C                   eval      cmzmbd = vlrhdl
090300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
090400000710     C                   if        prmfir = *blanks
090500010122     C                   eval      cmzfla = 'FNVAO00F'
090600010122     C                   eval      cmzmba = 'FNVAO00F'
090700000710     C                   else
090800000710     C                   eval      cmzfla = prmfir
090900000710     C                   eval      cmzmba = prmfir
091000000710     C                   endif
091100990920     C                   eval      cmznrr = *zeros
091200990920     C                   move      §ctrok        cmznrr
091300010123     C                   eval      cmzlba = vlrfl1
091400990920     C                   call(e)   'TIS711C'
091500990920     C                   parm                    dscmz
091600990921     C                   parm      *blanks       esito
091700990920     C                   if        %error
091800990920     C                             or cmzerr = '1'
091900990921     C                             or esito  = '1'
092000000710     C                   eval      wrkesito = '3'
092100990920     C                   endif
092200990920     C*
092300000613     C                   ENDSR
092400000613     C***
092500990910
092600010528
092700010528
092800010528
092900010528      /TITLE Invio dei dati al punto operativo.
093000010528     C     opeini        BEGSR
093100010528     C*
093200010528     C* Inizializzo flag e contatori operativi
093300010528     C                   movel     '0'           flgGiro           1
093400010528     C                   movel     '0'           flgMulti          1
093500010528     C                   movel     '1'           flgStato          1
093600010615     C                   movel     '0'           flgOk             1
093700010528     C                   z-add     *zeros        cntNonEl         10 0
093800010528     C                   z-add     *zeros        depfil            3 0
093900010528     C                   z-add     *zeros        invfil            3 0
094000010528     C*
094100010528     C                   ENDSR
094200010528     C***
094300010528
094400010528
094500010528
094600010528
094700000613     C     *inzsr        BEGSR
094800990910     C*
094900990910     C     *entry        plist
095000990920     C                   parm                    tivlrds
095100990921     C                   parm      wrkesito      esito
095200000724     C                   parm                    prmlit
095300000710     C                   parm                    prmfir
095400011105     C*
095500011105     C* Campi riferito al database
095600011105     C     *like         define    tbecod        kteCOD                         *TNTBE01L
095700011105     C     *like         define    tbeke1        kteKE1
095800011105     C*
095900011105     C* Definizione chiavi di lettura
096000011105     C* TNTBE01L - parziale
096100011105     C     keytbe        klist
096200011105     C                   kfld                    kteCOD                         *UTENTE
096300011105     C                   kfld                    kteKE1                         *CODICE
096400011105     C*
096500011105     C* Reperimento data e ora corrente
096600011105     C                   time                    wn14             14 0          *ORA (6)+ DATA (8)
096700011105     C                   movel     wn14          wn6               6 0          *ORA (6)
096800011105     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
096900011105     C                   z-add     wn8           g08dat
097000011105     C                   z-add     *zeros        g08inv
097100011105     C                   movel     '0'           g08err
097200011105     C                   call      'XSRDA8'
097300011105     C                   parm                    wlbda8
097400011105     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
097500010529     C*
097600000613     C                   ENDSR
097700000613     C***
097800990908
097900010122     Otis7t7p   E            riepilogo         2
098000990915     O                                              'Upload via Internet'
098100990915     O                                           +1 'Traduzione TIVIN00R -'
098200010122     O                                           +1 'FNVAOWWR'
098300010122     O                                           +1 'Ordini ritiro merce'
098400990915     O          E            riepilogo   2
098500990915     O                       wrkdata
098600990915     O                       wrkora              +1
098700990915     O                       procname            +1
098800990915     O          E            riepilogo   2
098900990915     O                                              'Cliente..................:'
099000010122     O                       VAOKSC        z     +1
099100990915     O          E            riepilogo   2
099200990920     O                                              'Riferimento Strategi.....:'
099300990920     O                       vlrhdl              +1
099400990915     O          E            riepilogo   2
099500990915     O                                              'Giusti...................:'
099600971022     O                       §CTROK        2   +  1
099700990915     O          E            riepilogo   2
099800990915     O                                              'Sbagliati e corretti.....:'
099900971022     O                       §CTRMO        2   +  1
100000990915     O          E            riepilogo   2
100100990915     O                                              'Sbagliati e scartati.....:'
100200971022     O                       §CTRNO        2   +  1
100300000613
100400010122     Otis7t7ps  E            testdett          2
100500000613     O                                              'Upload via Internet'
100600000613     O                                           +1 'Traduzione TIVIN00R -'
100700010122     O                                           +1 'FNVAOWWR'
100800010122     O                                           +1 'Ordini ritiro merce'
100900000616     O          E            testdett    3
101000000613     O                                           +2 'N° rec'
101100000613     O                                           +3 'Anteprima contenuto'
101200000616     O          E            rigadett    2
101300000613     O                       rrnum               +2
101400000621     O                       recko               +3
101500000616     O          E            findett     2
101600000613     O                       wrkdata
101700000613     O                       wrkora              +1
101800000613     O                       procname            +1
101900000616     O          E            findett     2
102000000613     O                                              'Cliente..................:'
102100010122     O                       VAOKSC        z     +1
102200000616     O          E            findett     2
102300000613     O                                              'Riferimento Strategi.....:'
102400000613     O                       vlrhdl              +1
102500000616     O          E            findett     2
102600000613     O                                              'Giusti...................:'
102700000613     O                       §CTROK        2   +  1
102800000616     O          E            findett     2
102900000613     O                                              'Sbagliati e corretti.....:'
103000000613     O                       §CTRMO        2   +  1
103100000616     O          E            findett     2
103200000613     O                                              'Sbagliati e scartati.....:'
103300000613     O                       §CTRNO        2   +  1
103400000512** CMD - COMANDI CL
103500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T7P)  FORMTYPE(RICCLI) OUTQ(
103600011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T7PS) FORMTYPE(RICCLI) OUTQ(
103700010122DLTOVR FILE(TIS7T7P TIS7T7PS) LVL(*)
103800000512
103900000512
