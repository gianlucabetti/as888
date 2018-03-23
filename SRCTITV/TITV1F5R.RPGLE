000100080411      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente CTA - Cod. 0770381)
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600050209     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000512     D*------------
000900990920     D dscmz         e ds                  inz
001000990910     D psds           sds
001100990910     D  procname         *PROC
001200080411     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001300080411     D* 10+3
001400080411     D                 DS
001500080411     D  WS15                   1      1
001600080411     D  WI15                   2     11
001700080411     D  WV15                  12     12
001800080411     D  WD15                  13     15
001900080411     D  WASC15                 1     15
002000080411     D*  6+1
002100080411     D                 DS
002200080411     D  WS09                   1      1
002300080411     D  WI09                   2      7
002400080411     D  WV09                   8      8
002500080411     D  WD09                   9      9
002600080411     D  WASC09                 1      9
002700080411     D*  2+3
002800080411     D                 DS
002900080411     D  WS07                   1      1
003000080411     D  WI07                   2      3
003100080411     D  WV07                   4      4
003200080411     D  WD07                   5      7
003300080411     D  WASC07                 1      7
003400990920     D tivlrds       e ds                  extname(tivlr00f)
003500080411     D tisi95ds      e ds
003600990910     D fnvabaar      e ds                  inz
003700990910     D esito           s              1
003800000724     D prmlit          s             10
003900000710     D prmfir          s             10
004000990921     D wrkesito        s                   like(esito)
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200050209     D parccm          s              8    INZ(*blanks)
004300050209     D parmbr          s             10    INZ(*blanks)
004400050209     D paropz          s              1    INZ(*blanks)
004500050209     D chkcall         s              1    INZ(*blanks)
004600080411     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004700010606     D*------------------
004800010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900010606     D*------------------
005000010606     D WLBDA8          DS                  INZ
005100010606     D  G08DAT                 1      8  0
005200010606     D  G08INV                 9     16  0
005300010606     D  G08ERR                17     17
005400010606     D  G08TGI                18     22  0
005500050209     D*------------------
005600050209     D* DS REPERIMENTO NUMERATORE
005700050209     D*------------------
005800050209     D trul33ds      e ds                  inz
005900050209     D*------------------
006000050209     D* DS ARCHITETTURA
006100050209     D*------------------
006200050209     D kpjba         e ds                  inz
006300080411     D*------------------
006400080411     D* DS CALCOLO CHECK-DIGITS MOD 10 (cod. 103)
006500080411     D*------------------
006600090916     D TRUL28DSX     e ds
006700050209     D*------------------
006800080411     D* VARIABILI X "XCHKCHAR"
006900080411     D*------------------
007000080411     D TxtInOut        S           2048
007100080411     D ElencoChar      S            256
007200080411     D TipoElenco      S              1
007300080411     D CharSost        S              1
007400080411     D UpperCase       S              1
007500080411     D ChkNull         S              1
007600080411     D CharNull        S              1
007700080411     D EsitoBon        S              1
007800080411     D*------------------
007900990908
008000050209
008100050209
008200050209     C* /MAIN
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600010530     C*
008700010530     C                   exsr      opeini
008800010530     C                   exsr      rwvab
008900050209     C*
009000050209     C* Effettuo la chiamata al CLLE preposto
009100050209     C                   call(e)   'TITVVTC'
009200050209     C                   parm                    parccm
009300050209     C                   parm                    parmbr
009400050209     C                   parm      '2'           paropz
009500080411     C*
009600080411     C* Esegue lancio TISI95R solo x chiusura
009700080411     C                   CLEAR                   TISI95DS
009800080411     C                   EVAL      I95TLA = 'C'
009900080411     C                   CALL      'TISI95R'
010000080411     C                   PARM                    TISI95DS
010100010530     C*
010200010530     C                   seton                                        lr
010300010530
010400010530
010500010530
010600010530
010700010530     C*--------------------------------------------------------
010800010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010530     C*--------------------------------------------------------
011000010530     C     PREELA        BEGSR
011100000724     C*
011200000724     C* SE OCCORRE SPEDIRE IN FILIALE
011300010530     C                   if        invfil <> *zeros and
011400010530     C                             flgGiro = '0'
011500010530     C*
011600010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010530     C                   eval      flgGiro = '1'
011800010530     C*
011900010530     C                   endif
012000010530     C*
012100010530     C                   ENDSR
012200010530     C***
012300010530
012400010530
012500010530
012600010530
012700010530
012800010530
012900010530     C*--------------------------------------------------------
013000010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010530     C*--------------------------------------------------------
013200010530     C     ENDELA        BEGSR
013300010530     C*
013400010530     C                   ENDSR
013500010530     C***
013600990908
013700010530
013800010530
013900010530
014000910830     C*--------------------------------------------------------
014100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
014200910830     C*--------------------------------------------------------
014300941229     C     RWVAB         BEGSR
014400990910     C*
014500990914     C                   if        not %open(tivin00r)
014600990908     C                   open      tivin00r
014700990914     C                   endif
014800020913     C                   if        not %open(fivabwwr)
014900020913     C                   open      fivabwwr
015000990914     C                   endif
015100050209     C*
015200050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300050209     C                   exsr      prevat
015400050209     C*
015500050209     C                   if        chkcall = '0'
015600050209     C*
015700050209     C                   if        not %open(fivatwwr)
015800050209     C                   open      fivatwwr
015900050209     C                   endif
016000050209     C*
016100990910     C                   clear                   §CTROK
016200990910     C                   clear                   §CTRMO
016300990910     C                   clear                   §CTRNO
016400990910     C*
016500921023     C                   DO        *HIVAL
016600990913     C*
016700990915     C                   READ      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300010705     C*
017400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
017500010705     C                   if        %subst(vindta:1:1) = '='
017600010705     C                   eval      %subst(vindta:1:1) = *blanks
017700010705     C                   endif
017800010705     C*
017900000905     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100010601     C* ...elimino eventuali segni
018200010601     C     '+':' '       xlate     vindta        vindta
018300990913     C*
018400010601     C                   if        *in70 = *off and
018500990913     C                             (vinflg = *blanks
018600010711     C                              or vinflg = '0'
018700010711     C                              or vinflg = '2')
018800990913     C*
018900020913     C                   clear                   FIVAB000
019000050209     C                   clear                   FIVAT000
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010711     C*
019600990913     C                   eval      fnvabaar = vindta
019700080411     C*
019800080411     C                   testb     x'3F'         vhbatb                   02
019900080411     C                   if        not *in02
020000080411     C*
020100080411     C                   SETOFF                                       303132
020200080411     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
020300080411     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
020400080411     C*    OPPURE TUTTI BLANKS
020500080411     C*
020600080411     C                   TESTN                   VHBLNP               30
020700080411     C  N30              TESTN                   VHBLNP                 30
020800080411     C  N30              TESTN                   VHBLNP                   30
020900080411     C                   MOVE      VHBLNP        W001A             1
021000080411     C     *IN30         IFEQ      *OFF
021100080411     C     W001A         ORLT      '0'
021200080411     C                   SETON                                        31
021300080411     C                   eval      vinmsg = %trimr(vinmsg)
021400080411     C                             + ' ' + 'VHBLNP'
021500080411     C                   ELSE
021600080411     C                   MOVE      VHBLNP        VABLNP
021700080411     C     VABLNP        IFLE      *ZEROS
021800080411     C                   SETON                                        31
021900080411     C                   eval      vinmsg = %trimr(vinmsg)
022000080411     C                             + ' ' + 'VHBLNP'
022100080411     C                   ENDIF
022200080411     C                   ENDIF
022300080411     C*
022400080411     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022500080411     C                   eval      depfil = VABLNP
022600080411     C                   exsr      repfil
022700080411     C                   if        depfil = invfil
022800080411     C                   if        vlrpoi = 999
022900080411     C                   MOVE(P)   invfil        VABFGS
023000080411     C                   else
023100080411     C                   MOVE(P)   vlrpoi        VABFGS
023200080411     C                   endif
023300080411     C*
023400080411     C                   exsr      PREELA
023500080411     C*
023600080411     C                   TESTN                   VHBCCM               30
023700080411     C  N30              TESTN                   VHBCCM                 30
023800080411     C  N30              TESTN                   VHBCCM                   30
023900080411     C                   MOVE      VHBCCM        W001A             1
024000080411     C     *IN30         IFEQ      *OFF
024100080411     C     W001A         ORLT      '0'
024200080411     C                   SETON                                        31
024300080411     C                   eval      vinmsg = %trimr(vinmsg)
024400080411     C                             + ' ' + 'VHBCCM'
024500080411     C                   ELSE
024600080411     C                   MOVE      VHBCCM        VABCCM
024700080411     C     VABCCM        IFLE      *ZEROS
024800080411     C                   SETON                                        31
024900080411     C                   eval      vinmsg = %trimr(vinmsg)
025000080411     C                             + ' ' + 'VHBCCM'
025100080411     C                   ENDIF
025200080411     C                   ENDIF
025300080411     C*
025400080411     C                   TESTN                   VHBAAS               30
025500080411     C  N30              TESTN                   VHBAAS                 30
025600080411     C  N30              TESTN                   VHBAAS                   30
025700080411     C                   MOVE      VHBAAS        W001A             1
025800080411     C     *IN30         IFEQ      *OFF
025900080411     C     W001A         ORLT      '0'
026000080411     C     W001A         ANDNE     *BLANK
026100080411     C                   SETON                                        32
026200080411     C                   MOVEL     datcor        VABAAS
026300080411     C                   eval      vinmsg = %trimr(vinmsg)
026400080411     C                             + ' ' + 'VHBAAS'
026500080411     C                   ELSE
026600080411     C                   MOVE      VHBAAS        VABAAS
026700080411     C                   ENDIF
026800080411     C*
026900080411     C                   TESTN                   VHBMGS               30
027000080411     C  N30              TESTN                   VHBMGS                 30
027100080411     C  N30              TESTN                   VHBMGS                   30
027200080411     C                   MOVE      VHBMGS        W001A             1
027300080411     C     *IN30         IFEQ      *OFF
027400080411     C     W001A         ORLT      '0'
027500080411     C     W001A         ANDNE     *BLANK
027600080411     C                   SETON                                        32
027700080411     C                   MOVE      '0101'        VABMGS
027800080411     C                   eval      vinmsg = %trimr(vinmsg)
027900080411     C                             + ' ' + 'VHBMGS'
028000080411     C                   ELSE
028100080411     C                   MOVE      VHBMGS        VABMGS
028200080411     C                   ENDIF
028300080411     C*
028400080411     C                   TESTN                   VHBNRS               30
028500080411     C  N30              TESTN                   VHBNRS                 30
028600080411     C  N30              TESTN                   VHBNRS                   30
028700080411     C                   MOVE      VHBNRS        W001A             1
028800080411     C     *IN30         IFEQ      *OFF
028900080411     C     W001A         ORLT      '0'
029000080411     C     W001A         ANDNE     *BLANK
029100080411     C                   SETON                                        32
029200080411     C                   MOVE      *ZERO         VABNRS
029300080411     C                   eval      vinmsg = %trimr(vinmsg)
029400080411     C                             + ' ' + 'VHBNRS'
029500080411     C                   ELSE
029600080411     C                   MOVE      VHBNRS        VABNRS
029700080411     C                   ENDIF
029800080411     C*
029900080411     C                   TESTN                   VHBNSP               30
030000080411     C  N30              TESTN                   VHBNSP                 30
030100080411     C  N30              TESTN                   VHBNSP                   30
030200080411     C                   MOVE      VHBNSP        W001A             1
030300080411     C     *IN30         IFEQ      *OFF
030400080411     C     W001A         ORLT      '0'
030500080411     C     W001A         ANDNE     *BLANK
030600080411     C                   SETON                                        32
030700080411     C                   MOVE      *ZERO         VABNSP
030800080411     C                   eval      vinmsg = %trimr(vinmsg)
030900080411     C                             + ' ' + 'VHBNSP'
031000080411     C                   ELSE
031100080411     C                   MOVE      VHBNSP        VABNSP
031200080411     C                   ENDIF
031300080411     C*
031400080411     C                   TESTN                   VHBLNA               30
031500080411     C  N30              TESTN                   VHBLNA                 30
031600080411     C  N30              TESTN                   VHBLNA                   30
031700080411     C                   MOVE      VHBLNA        W001A             1
031800080411     C     *IN30         IFEQ      *OFF
031900080411     C     W001A         ORLT      '0'
032000080411     C     W001A         ANDNE     *BLANK
032100080411     C                   SETON                                        32
032200080411     C                   MOVE      *ZERO         VABLNA
032300080411     C                   eval      vinmsg = %trimr(vinmsg)
032400080411     C                             + ' ' + 'VHBLNA'
032500080411     C                   ELSE
032600080411     C                   MOVE      VHBLNA        VABLNA
032700080411     C                   ENDIF
032800080411     C*
032900080411     C                   TESTN                   VHBCTR               30
033000080411     C  N30              TESTN                   VHBCTR                 30
033100080411     C  N30              TESTN                   VHBCTR                   30
033200080411     C                   MOVE      VHBCTR        W001A             1
033300080411     C     *IN30         IFEQ      *OFF
033400080411     C     W001A         ORLT      '0'
033500080411     C     W001A         ANDNE     *BLANK
033600080411     C                   SETON                                        32
033700080411     C                   MOVE      *ZERO         VABCTR
033800080411     C                   eval      vinmsg = %trimr(vinmsg)
033900080411     C                             + ' ' + 'VHBCTR'
034000080411     C                   ELSE
034100080411     C                   MOVE      VHBCTR        VABCTR
034200080411     C                   ENDIF
034300080411     C*
034400080411     C                   MOVE      *ZERO         VABIAS
034500080411     C                   MOVE      VHBIAS        WASC15
034600080411     C                   TESTN                   WI15                 30
034700080411     C  N30              TESTN                   WI15                   30
034800080411     C  N30              TESTN                   WI15                     30
034900080411     C                   MOVE      WI15          W001A             1
035000080411     C     *IN30         IFEQ      *OFF
035100080411     C     W001A         ORLT      '0'
035200080411     C     W001A         ANDNE     *BLANK
035300080411     C                   SETON                                        32
035400080411     C                   MOVE      *ZERO         VABIAS
035500080411     C                   eval      vinmsg = %trimr(vinmsg)
035600080411     C                             + ' ' + 'VHBIAS'
035700080411     C                   ELSE
035800080411     C                   MOVEL     WI15          VABIAS
035900080411     C                   TESTN                   WD15                 30
036000080411     C  N30              TESTN                   WD15                   30
036100080411     C  N30              TESTN                   WD15                     30
036200080411     C                   MOVE      WD15          W001A             1
036300080411     C     *IN30         IFEQ      *OFF
036400080411     C     W001A         ORLT      '0'
036500080411     C     W001A         ANDNE     *BLANK
036600080411     C                   SETON                                        32
036700080411     C                   MOVE      *ZERO         VABIAS
036800080411     C                   eval      vinmsg = %trimr(vinmsg)
036900080411     C                             + ' ' + 'VHBIAS'
037000080411     C                   ELSE
037100080411     C                   MOVE      WD15          VABIAS
037200080411     C                   ENDIF
037300080411     C                   ENDIF
037400080411     C*
037500080411     C                   TESTN                   VHBNCL               30
037600080411     C  N30              TESTN                   VHBNCL                 30
037700080411     C  N30              TESTN                   VHBNCL                   30
037800080411     C                   MOVE      VHBNCL        W001A             1
037900080411     C     *IN30         IFEQ      *OFF
038000080411     C     W001A         ORLT      '0'
038100080411     C                   SETON                                        32
038200080411     C                   MOVE      *ZERO         VABNCL
038300080411     C                   eval      vinmsg = %trimr(vinmsg)
038400080411     C                             + ' ' + 'VHBNCL'
038500080411     C                   ELSE
038600080411     C                   MOVE      VHBNCL        VABNCL
038700080411     C                   ENDIF
038800080411     C*
038900080411     C                   MOVE      *ZERO         VABPKB
039000080411     C                   MOVE      VHBPKB        WASC09
039100080411     C                   IF        WI09 = *blanks
039200080411     C                   EVAL      WI09 = *zeros
039300080411     C                   ENDIF
039400080411     C                   TESTN                   WI09                 30
039500080411     C  N30              TESTN                   WI09                   30
039600080411     C  N30              TESTN                   WI09                     30
039700080411     C                   MOVE      WI09          W001A             1
039800080411     C     *IN30         IFEQ      *OFF
039900080411     C     W001A         ORLT      '0'
040000080411     C                   SETON                                        32
040100080411     C                   MOVE      *ZERO         VABPKB
040200080411     C                   eval      vinmsg = %trimr(vinmsg)
040300080411     C                             + ' ' + 'VHBPKB'
040400080411     C                   ELSE
040500080411     C                   MOVEL     WI09          VABPKB
040600080411     C                   IF        WD09 = *blanks
040700080411     C                   EVAL      WD09 = *zeros
040800080411     C                   ENDIF
040900080411     C                   TESTN                   WD09                 30
041000080411     C  N30              TESTN                   WD09                     30
041100080411     C                   MOVE      WD09          W001A             1
041200080411     C     *IN30         IFEQ      *OFF
041300080411     C     W001A         ORLT      '0'
041400080411     C     W001A         ANDNE     *BLANK
041500080411     C                   SETON                                        32
041600080411     C                   MOVE      *ZERO         VABPKB
041700080411     C                   eval      vinmsg = %trimr(vinmsg)
041800080411     C                             + ' ' + 'VHBPKB'
041900080411     C                   ELSE
042000080411     C                   MOVE      WD09          VABPKB
042100080411     C                   ENDIF
042200080411     C                   ENDIF
042300080411     C*
042400080411     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
042500080411     C                   IF        *IN51 = *OFF
042600080411     C                   MOVE      *ZERO         VABVLB
042700080411     C                   MOVE      VHBVLB        WASC07
042800080411     C                   IF        WI07 = *blanks
042900080411     C                   EVAL      WI07 = *zeros
043000080411     C                   ENDIF
043100080411     C                   TESTN                   WI07                 30
043200080411     C  N30              TESTN                   WI07                   30
043300080411     C  N30              TESTN                   WI07                     30
043400080411     C                   MOVE      WI07          W001A             1
043500080411     C     *IN30         IFEQ      *OFF
043600080411     C     W001A         ORLT      '0'
043700080411     C     W001A         ANDNE     *BLANK
043800080411     C                   SETON                                        32
043900080411     C                   MOVE      *ZERO         VABVLB
044000080411     C                   eval      vinmsg = %trimr(vinmsg)
044100080411     C                             + ' ' + 'VHBVLB'
044200080411     C                   ELSE
044300080411     C                   MOVEL     WI07          VABVLB
044400080411     C                   IF        WD07 = *blanks
044500080411     C                   EVAL      WD07 = *zeros
044600080411     C                   ENDIF
044700080411     C                   TESTN                   WD07                 30
044800080411     C  N30              TESTN                   WD07                   30
044900080411     C  N30              TESTN                   WD07                     30
045000080411     C                   MOVE      WD07          W001A             1
045100080411     C     *IN30         IFEQ      *OFF
045200080411     C     W001A         ORLT      '0'
045300080411     C     W001A         ANDNE     *BLANK
045400080411     C                   SETON                                        32
045500080411     C                   MOVE      *ZERO         VABVLB
045600080411     C                   eval      vinmsg = %trimr(vinmsg)
045700080411     C                             + ' ' + 'VHBVLB'
045800080411     C                   ELSE
045900080411     C                   MOVE      WD07          VABVLB
046000080411     C                   ENDIF
046100080411     C                   ENDIF
046200080411     C                   ELSE
046300080411     C* ...altrimenti forzo azzeramento del volume
046400080411     C                   Z-ADD     *zeros        VABVLB
046500080411     C                   ENDIF
046600080411     C*
046700080411     C                   MOVE      *ZERO         VABQFT
046800080411     C                   MOVE      VHBQFT        WASC15
046900080411     C                   IF        WI15 = *blanks
047000080411     C                   EVAL      WI15 = *zeros
047100080411     C                   ENDIF
047200080411     C                   TESTN                   WI15                 30
047300080411     C  N30              TESTN                   WI15                   30
047400080411     C  N30              TESTN                   WI15                     30
047500080411     C                   MOVE      WI15          W001A             1
047600080411     C     *IN30         IFEQ      *OFF
047700080411     C     W001A         ORLT      '0'
047800080411     C     W001A         ANDNE     *BLANK
047900080411     C                   SETON                                        32
048000080411     C                   MOVE      *ZERO         VABQFT
048100080411     C                   eval      vinmsg = %trimr(vinmsg)
048200080411     C                             + ' ' + 'VHBQFT'
048300080411     C                   ELSE
048400080411     C                   MOVEL     WI15          VABQFT
048500080411     C                   IF        WD15 = *blanks
048600080411     C                   EVAL      WD15 = *zeros
048700080411     C                   ENDIF
048800080411     C                   TESTN                   WD15                 30
048900080411     C  N30              TESTN                   WD15                   30
049000080411     C  N30              TESTN                   WD15                     30
049100080411     C                   MOVE      WD15          W001A             1
049200080411     C     *IN30         IFEQ      *OFF
049300080411     C     W001A         ORLT      '0'
049400080411     C     W001A         ANDNE     *BLANK
049500080411     C                   SETON                                        32
049600080411     C                   MOVE      *ZERO         VABQFT
049700080411     C                   eval      vinmsg = %trimr(vinmsg)
049800080411     C                             + ' ' + 'VHBQFT'
049900080411     C                   ELSE
050000080411     C                   MOVE      WD15          VABQFT
050100080411     C                   ENDIF
050200080411     C                   ENDIF
050300080411     C*
050400080411     C                   MOVE      *ZERO         VABCAS
050500080411     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
050600080411     C                   EVAL      wVHBCAS = VHBCAS
050700080411     C                   IF        %subst(wVHBCAS:12:1) = '.'
050800080411     C                   EVAL      %subst(wVHBCAS:12:1) = ','
050900080411     C                   ENDIF
051000080411     C*
051100080411     C                   MOVEL(P)  wVHBCAS       PiStr
051200080411     C                   EXSR      CHKNUM
051300080411     C                   IF        PiNum=*on
051400080411     C                   Z-ADD     PiVal         VABCAS
051500080411     C                   ELSE
051600080411     C                   SETON                                        32
051700080411     C                   Z-ADD     *zeros        VABCAS
051800080411     C                   EVAL      vinmsg = %trimr(vinmsg)
051900080411     C                             + ' ' + 'VHBCAS'
052000080411     C                   ENDIF
052100080411     C*
052200080411     C                   TESTN                   VHBRMN               30
052300080411     C  N30              TESTN                   VHBRMN                 30
052400080411     C  N30              TESTN                   VHBRMN                   30
052500080411     C                   MOVE      VHBRMN        W001A             1
052600080411     C     *IN30         IFEQ      *OFF
052700080411     C     W001A         ORLT      '0'
052800080411     C                   SETON                                        32
052900080411     C                   MOVE      *ZERO         VABRMN
053000080411     C                   eval      vinmsg = %trimr(vinmsg)
053100080411     C                             + ' ' + 'VHBRMN'
053200080411     C                   ELSE
053300080411     C                   MOVE      VHBRMN        VABRMN
053400080411     C                   ENDIF
053500080411     C*
053600080411     C                   TESTN                   VHBNCD               30
053700080411     C  N30              TESTN                   VHBNCD                 30
053800080411     C  N30              TESTN                   VHBNCD                   30
053900080411     C                   MOVE      VHBNCD        W001A             1
054000080411     C     *IN30         IFEQ      *OFF
054100080411     C     W001A         ORLT      '0'
054200080411     C     W001A         ANDNE     *BLANK
054300080411     C                   SETON                                        32
054400080411     C                   MOVE      *ZERO         VABNCD
054500080411     C                   eval      vinmsg = %trimr(vinmsg)
054600080411     C                             + ' ' + 'VHBNCD'
054700080411     C                   ELSE
054800080411     C                   MOVE      VHBNCD        VABNCD
054900080411     C                   ENDIF
055000080411     C*
055100080411     C                   TESTN                   VHBNCA               30
055200080411     C  N30              TESTN                   VHBNCA                 30
055300080411     C  N30              TESTN                   VHBNCA                   30
055400080411     C                   MOVE      VHBNCA        W001A             1
055500080411     C     *IN30         IFEQ      *OFF
055600080411     C     W001A         ORLT      '0'
055700080411     C     W001A         ANDNE     *BLANK
055800080411     C                   SETON                                        32
055900080411     C                   MOVE      *ZERO         VABNCA
056000080411     C                   eval      vinmsg = %trimr(vinmsg)
056100080411     C                             + ' ' + 'VHBNCA'
056200080411     C                   ELSE
056300080411     C                   MOVE      VHBNCA        VABNCA
056400080411     C                   ENDIF
056500080411     C*
056600080411     C                   TESTN                   VHBZNC               30
056700080411     C  N30              TESTN                   VHBZNC                 30
056800080411     C  N30              TESTN                   VHBZNC                   30
056900080411     C                   MOVE      VHBZNC        W001A             1
057000080411     C     *IN30         IFEQ      *OFF
057100080411     C     W001A         ORLT      '0'
057200080411     C     W001A         ANDNE     *BLANK
057300080411     C                   SETON                                        32
057400080411     C                   MOVE      *ZERO         VABZNC
057500080411     C                   eval      vinmsg = %trimr(vinmsg)
057600080411     C                             + ' ' + 'VHBZNC'
057700080411     C                   ELSE
057800080411     C                   MOVE      VHBZNC        VABZNC
057900080411     C                   ENDIF
058000080411     C*
058100080411     C                   TESTN                   VHBDCR               30
058200080411     C  N30              TESTN                   VHBDCR                 30
058300080411     C  N30              TESTN                   VHBDCR                   30
058400080411     C                   MOVE      VHBDCR        W001A             1
058500080411     C     *IN30         IFEQ      *OFF
058600080411     C     W001A         ORLT      '0'
058700080411     C     W001A         ANDNE     *BLANK
058800080411     C                   SETON                                        32
058900080411     C                   MOVE      *ZERO         VABDCR
059000080411     C                   eval      vinmsg = %trimr(vinmsg)
059100080411     C                             + ' ' + 'VHBDCR'
059200080411     C                   ELSE
059300080411     C                   MOVE      VHBDCR        VABDCR
059400080411     C                   ENDIF
059500080411     C*
059600080411     C                   TESTN                   VHBHCR               30
059700080411     C  N30              TESTN                   VHBHCR                 30
059800080411     C  N30              TESTN                   VHBHCR                   30
059900080411     C                   MOVE      VHBHCR        W001A             1
060000080411     C     *IN30         IFEQ      *OFF
060100080411     C     W001A         ORLT      '0'
060200080411     C     W001A         ANDNE     *BLANK
060300080411     C                   SETON                                        32
060400080411     C                   MOVE      *ZERO         VABHCR
060500080411     C                   eval      vinmsg = %trimr(vinmsg)
060600080411     C                             + ' ' + 'VHBHCR'
060700080411     C                   ELSE
060800080411     C                   MOVE      VHBHCR        VABHCR
060900080411     C                   ENDIF
061000080411     C*
061100080411     C                   MOVE      *ZERO         VABVMD
061200080411     C                   MOVE      VHBVMD        WASC15
061300080411     C                   IF        WI15 = *blanks
061400080411     C                   EVAL      WI15 = *zeros
061500080411     C                   ENDIF
061600080411     C                   TESTN                   WI15                 30
061700080411     C  N30              TESTN                   WI15                   30
061800080411     C  N30              TESTN                   WI15                     30
061900080411     C                   MOVE      WI15          W001A             1
062000080411     C     *IN30         IFEQ      *OFF
062100080411     C     W001A         ORLT      '0'
062200080411     C     W001A         ANDNE     *BLANK
062300080411     C                   SETON                                        32
062400080411     C                   MOVE      *ZERO         VABVMD
062500080411     C                   eval      vinmsg = %trimr(vinmsg)
062600080411     C                             + ' ' + 'VHBVMD'
062700080411     C                   ELSE
062800080411     C                   MOVEL     WI15          VABVMD
062900080411     C                   IF        WD15 = *blanks
063000080411     C                   EVAL      WD15 = *zeros
063100080411     C                   ENDIF
063200080411     C                   TESTN                   WD15                 30
063300080411     C  N30              TESTN                   WD15                   30
063400080411     C  N30              TESTN                   WD15                     30
063500080411     C                   MOVE      WD15          W001A             1
063600080411     C     *IN30         IFEQ      *OFF
063700080411     C     W001A         ORLT      '0'
063800080411     C     W001A         ANDNE     *BLANK
063900080411     C                   SETON                                        32
064000080411     C                   MOVE      *ZERO         VABVMD
064100080411     C                   eval      vinmsg = %trimr(vinmsg)
064200080411     C                             + ' ' + 'VHBVMD'
064300080411     C                   ELSE
064400080411     C                   MOVE      WD15          VABVMD
064500080411     C                   ENDIF
064600080411     C                   ENDIF
064700080411     C*
064800080411     C                   TESTN                   VHBANT               30
064900080411     C  N30              TESTN                   VHBANT                 30
065000080411     C  N30              TESTN                   VHBANT                   30
065100080411     C                   MOVE      VHBANT        W001A             1
065200080411     C     *IN30         IFEQ      *OFF
065300080411     C     W001A         ORLT      '0'
065400080411     C     W001A         ANDNE     *BLANK
065500080411     C                   SETON                                        32
065600080411     C                   MOVE      *ZERO         VABANT
065700080411     C                   eval      vinmsg = %trimr(vinmsg)
065800080411     C                             + ' ' + 'VHBANT'
065900080411     C                   ELSE
066000080411     C                   MOVE      VHBANT        VABANT
066100080411     C                   ENDIF
066200080411     C*
066300080411     C* CAMPI ALFABETICI:
066400080411     C                   MOVE      VHBATB        VABATB
066500080411     C                   MOVE      VHBCBO        VABCBO
066600080411     C                   MOVE      VHBRSD        VABRSD
066700080411     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
066800080411     C     '@':'A'       XLATE     VABRSD        VABRSD
066900080411     C* ==
067000080411     C                   MOVE      VHBRD2        VABRD2
067100080411     C                   MOVE      VHBIND        VABIND
067200080411     C                   MOVE      VHBCAD        VABCAD
067300080411     C                   MOVE      VHBLOD        VABLOD
067400080411     C                   MOVE      VHBPRD        VABPRD
067500080411     C                   MOVE      VHBNZD        VABNZD
067600080411     C                   MOVE      VHBGC1        VABGC1
067700080411     C                   MOVE      VHBGC2        VABGC2
067800080411     C                   MOVE      VHBTSP        VABTSP
067900080411     C                   MOVE      VHBVAS        VABVAS
068000080411     C                   MOVE      VHBNAS        VABNAS
068100080411     C                   MOVE      VHBTIC        VABTIC
068200080411     C                   MOVE      VHBVCA        VABVCA
068300080411     C                   MOVE      VHBGCA        VABGCA
068400080411     C                   MOVE      VHBRMA        VABRMA
068500080411     C                   MOVE      VHBXCO        VABXCO
068600080411     C                   MOVE      VHBNOT        VABNOT
068700080411     C                   MOVE      VHBNT2        VABNT2
068800080411     C                   MOVE      VHBCTM        VABCTM
068900080411     C                   MOVE      VHBFFD        VABFFD
069000080411     C                   MOVE      VHBTCR        VABTCR
069100080411     C                   MOVE      VHBCTS        VABCTS
069200080411     C                   MOVE      VHBFTM        VABFTM
069300080411     C                   MOVE      VHBVAD        VABVAD
069400080411     C                   MOVE      VHBGMA        VABGMA
069500080411     C                   MOVE      VHBGGA        VABGGA
069600080411     C                   MOVE      VHBGVA        VABGVA
069700080411     C                   MOVE      VHBTC1        VABTC1
069800080411     C                   MOVE      VHBTC2        VABTC2
069900080411     C                   MOVE      VHBSCL        VABSCL
070000080411     C                   MOVE      VHBRMO        VABRMO
070100080411     C                   MOVE      VHBCMO        VABCMO
070200080411     C                   MOVE      VHBNMO        VABNMO
070300080411     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
070400080411     C* NELLA NAZIONE MITTENTE ORIGINALE
070500080411     C                   IF        %trim(VHBNMO) < 'A'
070600080411     C                   MOVE      *BLANKS       VABNMO
070700080411     C                   ENDIF
070800080411     C*
070900080411     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
071000080411     C                   IF        %trim(VABNZD) = 'I'   OR
071100080411     C                             %trim(VABNZD) = 'IT'  OR
071200080411     C                             %trim(VABNZD) = 'ITA'
071300080411     C                   EVAL      VABNZD = *blanks
071400080411     C                   ENDIF
071500080411     C*
071600080411     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
071700080411     C                   IF        *IN52 = *ON
071800080411     C* Reperisco la provincia dal CAP e dalla località
071900080411     C                   IF        VABCAD <> *blanks AND
072000080411     C                             VABPRD  = *blanks AND
072100080411     C                             VABNZD  = *blanks
072200080411     C                   CLEAR                   TISI95DS
072300080411     C                   EVAL      I95TCN = '3'
072400080411     C                   Z-ADD     datcor        I95DAT
072500080411     C                   EVAL      I95CAP = VABCAD
072600080411     C                   EVAL      I95LOC = VABLOD
072700080411     C                   CALL      'TISI95R'
072800080411     C                   PARM                    TISI95DS
072900080411     C                   EVAL      VABPRD = O95PRV
073000080411     C                   ENDIF
073100080411     C                   ENDIF
073200080411     C*
073300080411     C* "bonifico" i dati d input da eventuali caratteri indesiderati
073400080411     C                   exsr      exeBon
073500991022     C*
073600050209     C*
073700050209     C* NSP => Stacco un numeratore da AZNUM x la valorizzazione del numero spedizione
073800050209     C                   clear                   TRUL33DS
073900050209     C                   eval      I33OPE = *zeros
074000050209     C                   eval      I33CNU = 302
074100050209     C                   eval      I33NUM = 1
074200050209     C                   movel     TRUL33DS      KPJBU
074300050209     C                   call      'TRUL33R'
074400050209     C                   parm                    KPJBA
074500050209     C                   movel     KPJBU         TRUL33DS
074600050209     C                   if        O33ERR = *zeros
074700050209     C                   z-add     O33NRF        VABNSP
074800050209     C                   z-add     O33NRF        VATNSP
074900050209     C                   else
075000050209     C                   SETON                                        31
075100050209     C                   EVAL      vinmsg = %trimr(vinmsg)
075200050209     C                             + ' ' + 'VABNSP VATNSP'
075300050209     C                   endif
075400050209     C*
075500050209     C* Forzo il codice trattamento merce a 7Q
075600050209     C                   EVAL      VABCTM = '7Q'
075700930409     C*
075800010604     C  N31              ADD       1             §CTROK            7 0
075900010604     C   32              ADD       1             §CTRMO            7 0
076000010604     C   31              ADD       1             §CTRNO            7 0
076100080411     C  N31              WRITE     FIVAB000
076200050209     C  N31              EXSR      WRIVAT
076300990910     C*
076400010601     C                   if        *in31 = *off and
076500010601     C                             *in32 = *off
076600990910     C                   eval      vinflg = '1'
076700990910     C                   else
076800990910     C                   eval      vinflg = '2'
076900990910     C                   endif
077000991022     C                   endif
077100990910     C                   endif
077200010601     C*
077300010601     C                   endif
077400000905     C*
077500000905     C                   else
077600000905     C                   eval      vinflg = '1'
077700000905     C                   endif
077800000905     C*
077900000905     C  N70              update    tivin000
078000991022     C*
078100991022     C  N70              ENDdo
078200010530     C
078300010530     C*
078400010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
078500010530     C                   if        cntNonEl = *zeros or
078600010530     C                             flgMulti = '0'
078700990910     C* Se non ci sono record con errori ...
078800010530     C                   if        §ctrno = 0 and
078900010601     C                             §ctrmo = 0 and
079000010530     C                             flgStato <> '2'
079100990910     C* ... restituisco esito OK.
079200990921     C                   eval      wrkesito = '0'
079300990910     C                   else
079400010601     C                   if        §ctrok > 0
079500990921     C                   eval      wrkesito = '1'
079600000710     C                   else
079700010615     C                   if        flgOk = '0'
079800010615     C                   eval      wrkesito = '2'
079900010615     C                   else
080000010615     C                   eval      wrkesito = '6'
080100010615     C                   endif
080200990910     C                   endif
080300000710     C                   endif
080400010530     C                   else
080500010530     C                   eval      wrkesito = '9'
080600010530     C                   endif
080700050209     C*
080800050209     C                   else
080900050209     C                   eval      wrkesito = '2'
081000050209     C                   endif
081100990910     C*
081200990914     C                   if        %open(tivin00r)
081300990908     C                   close     tivin00r
081400990914     C                   endif
081500020913     C                   if        %open(fivabwwr)
081600020913     C                   close     fivabwwr
081700990914     C                   endif
081800050209     C                   if        %open(fivatwwr)
081900050209     C                   close     fivatwwr
082000050209     C                   endif
082100010530     C*
082200010530     C                   if        vlrpoi <> 999
082300010530     C                   eval      invfil = vlrpoi
082400010530     C                   endif
082500990910     C*
082600990920     C                   if        §ctrok > 0
082700010530     C                             and invfil > *zeros
082800000613     C                   exsr      invio
082900990920     C                   endif
083000010612     C*
083100010612     C                   if        flgGiro = '1'
083200010612     C                   exsr      endela
083300010612     C                   endif
083400990920     C*
083500910830     C                   ENDSR
083600000613     C***
083700050209
083800050209
083900050209
084000050209     C*----------------------------------------------------*
084100050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
084200050209     C*----------------------------------------------------*
084300050209     C     PREVAT        BEGSR
084400050209     C*
084500050209     C* Compongo il nome del membro da dare al FIVATWWR
084600050209     C                   eval      parmbr = vlrhdl
084700050209     C                   movel     'M'           parmbr
084800050209     C                   eval      parccm = %subst(vlrKSC:2:7)
084900050209     C                   eval      paropz = '1'
085000050209     C* Effettuo la chiamata al CLLE preposto
085100050209     C                   call(e)   'TITVVTC'
085200050209     C                   parm                    parccm
085300050209     C                   parm                    parmbr
085400050209     C                   parm                    paropz
085500050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
085600050209     C                   if        %error
085700050209     C                   movel     '1'           chkcall
085800050209     C                   else
085900050209     C                   movel     '0'           chkcall
086000050209     C                   endif
086100050209     C*
086200050209     C                   ENDSR
086300050209
086400050209
086500990920
086600050209     C*----------------------------------------------------*
086700050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
086800050209     C*----------------------------------------------------*
086900050209     C     WRIVAT        BEGSR
087000050209     C*
087100050209     C* Valorizzo il buffer del file dettagli barcode colli
087200050209     C                   EVAL      VATFGS = VABFGS
087300050209     C                   EVAL      VATAAS = VABAAS
087400050209     C                   EVAL      VATLNP = VABLNP
087500050209     C                   EVAL      VATNSP = VABNSP
087600050209     C                   EVAL      VATCCM = VABCCM
087700050209     C                   EVAL      VATTRC='E'
087800080414     C                   EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))
087900080411     C
088000090916     C                   clear                   TRUL28DSX
088100090916     C                   eval      I284STD = 'E10'
088200080411     C                   eval      I284COD = VATNOT
088300080411     C                   call      'TRUL28R4'
088400090916     C                   parm                    TRUL28DSX
088500080411     C*
088600080411     C* Se ok => valorizzo "CHI SONO", altrimenti nn effettuo nemmeno la scrittura
088700080411     C                   if        O284err = *blanks
088800080411     C                   eval      VATNOT = %trim(O284COD)
088900080411     C                   write     FIVAT000
089000080411     C                   endif
089100050209     C*
089200050209     C                   ENDSR
089300010530
089400010530
089500050209
089600010530
089700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
089800010530     C     repfil        BEGSR
089900010530     C*
090000010530     C                   if        invfil = *zeros and
090100010530     C                             depfil > *zeros and
090200010613     C                             (vinflg = *blanks or
090300010613     C                              vinflg = *zeros)
090400010530     C
090500010530     C                   eval      invfil = depfil
090600010530     C                   endif
090700010530     C*
090800010530     C                   if        depfil <> invfil and
090900010530     C                             invfil > *zeros
091000010530     C                   eval      flgMulti = '1'
091100010530     C                   if        vinflg = *blanks
091200010530     C                   add       1             cntNonEl
091300010530     C                   endif
091400010530     C                   endif
091500010530     C*
091600010530     C                   if        vinflg = '2'
091700010530     C                   eval      flgStato = '2'
091800010530     C                   endif
091900010530     C*
092000010530     C                   ENDSR
092100010530     C***
092200050209
092300050209
092400050209
092500050209
092600050209      /TITLE Invio dei dati al punto operativo.
092700050209     C     invio         BEGSR
092800050209     C*
092900050209     C* 1° invio FIVAT
093000050209     C                   reset                   dscmz
093100050209     C                   move      vlrpoi        cmzdst
093200050209     C                   eval      cmzfld = 'FIVATWWR'
093300050209     C                   eval      cmzmbd = vlrhdl
093400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
093500050209     C***                if        prmfir = *blanks
093600050209     C                   eval      cmzfla = 'FIVAT00F'
093700050209     C                   eval      cmzmba = 'FIVAT00F'
093800050209     C***                else
093900050209     C***                eval      cmzfla = prmfir
094000050209     C***                eval      cmzmba = prmfir
094100050209     C***                endif
094200050209     C                   eval      cmznrr = *zeros
094300050209     C                   move      §ctrok        cmznrr
094400050209     C                   eval      cmzlba = vlrfl1
094500050209     C                   call(e)   'TIS711C'
094600050209     C                   parm                    dscmz
094700050209     C                   parm      *blanks       esito
094800050209     C                   if        %error
094900050209     C                             or cmzerr = '1'
095000050209     C                             or esito  = '1'
095100050209     C                   eval      wrkesito = '3'
095200050209     C                   else
095300050209     C*
095400050209     C* 2° invio FIVAB
095500050209     C                   reset                   dscmz
095600050209     C                   move      vlrpoi        cmzdst
095700050209     C                   eval      cmzfld = vlrfou
095800050209     C                   eval      cmzmbd = vlrhdl
095900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
096000050209     C***                if        prmfir = *blanks
096100050209     C                   eval      cmzfla = 'FIVAB00F'
096200050209     C                   eval      cmzmba = 'FIVAB00F'
096300050209     C***                else
096400050209     C***                eval      cmzfla = prmfir
096500050209     C***                eval      cmzmba = prmfir
096600050209     C***                endif
096700050209     C                   eval      cmznrr = *zeros
096800050209     C                   move      §ctrok        cmznrr
096900050209     C                   eval      cmzlba = vlrfl1
097000050209     C                   call(e)   'TIS711C'
097100050209     C                   parm                    dscmz
097200050209     C                   parm      *blanks       esito
097300050209     C                   if        %error
097400050209     C                             or cmzerr = '1'
097500050209     C                             or esito  = '1'
097600050209     C                   eval      wrkesito = '3'
097700050209     C                   endif
097800050209     C                   endif
097900050209     C*
098000050209     C                   ENDSR
098100050209     C***
098200010530
098300010530
098400010530
098500010530
098600010530      /TITLE Invio dei dati al punto operativo.
098700010530     C     opeini        BEGSR
098800010530     C*
098900010530     C* Inizializzo flag e contatori operativi
099000010530     C                   movel     '0'           flgGiro           1
099100010530     C                   movel     '0'           flgMulti          1
099200010530     C                   movel     '1'           flgStato          1
099300010615     C                   movel     '0'           flgOk             1
099400010530     C                   z-add     *zeros        cntNonEl         10 0
099500010530     C                   z-add     *zeros        depfil            3 0
099600010530     C                   z-add     *zeros        invfil            3 0
099700030407     C*
099800050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
099900030407     C                   if        %subst(vlrppt:1:1) = 'S'
100000030407     C                   seton                                        50
100100030407     C                   else
100200030407     C                   setoff                                       50
100300030407     C                   endif
100400041001     C*
100500050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
100600041001     C                   if        %subst(vlrppt:2:1) = 'S'
100700041001     C                   seton                                        51
100800041001     C                   else
100900041001     C                   setoff                                       51
101000041001     C                   endif
101100080411     C*
101200080411     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
101300080411     C* (naturalmente solo se nn già presente)
101400080411     C                   if        %subst(vlrppt:3:1) = 'S'
101500080411     C                   seton                                        52
101600080411     C                   else
101700080411     C                   setoff                                       52
101800080411     C                   endif
101900010530     C*
102000010530     C                   ENDSR
102100010530     C***
102200010530
102300010530
102400070411
102500070411     C     *pssr         BEGSR
102600070411     C*
102700070411     C                   if        %open(tivin00r)
102800070411     C                   close     tivin00r
102900070411     C                   endif
103000070411     C                   if        %open(fivabwwr)
103100070411     C                   close     fivabwwr
103200070411     C                   endif
103300070411     C                   if        %open(fivatwwr)
103400070411     C                   close     fivatwwr
103500070411     C                   endif
103600070411     C*
103700070411     C* Effettuo la chiamata al CLLE preposto
103800070411     C                   call(e)   'TITVVTC'
103900070411     C                   parm                    parccm
104000070411     C                   parm                    parmbr
104100070411     C                   parm      '2'           paropz
104200070411     C*
104300070411     C                   eval      wrkesito = '2'
104400070411     C*
104500070411     C                   seton                                        LR
104600070411     C*
104700070411     C                   ENDSR     '*CANCL'
104800070411     C***
104900080411
105000080411
105100080411
105200080411      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
105300080411     C     exeBon        BEGSR
105400080411     C*
105500080411     C* Imposto i parametri x il *pgm d "bonifica"
105600080411     C                   EVAL      ElencoChar = *blanks
105700080411     C                   EVAL      TipoElenco = *blanks
105800080411     C                   EVAL      CharSost   = *blanks
105900080411     C                   EVAL      UpperCase  = '1'
106000080411     C                   EVAL      ChkNull    = '1'
106100080411     C                   EVAL      CharNull   = *blanks
106200080411     C*
106300080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
106400080411     C                   EVAL      TxtInOut   = VABRSD
106500080411     C                   EVAL      EsitoBon   = *blanks
106600080411     C*
106700080411     C                   CALL      'XCHKCHAR'
106800080411     C                   PARM                    TxtInOut
106900080411     C                   PARM                    ElencoChar
107000080411     C                   PARM                    TipoElenco
107100080411     C                   PARM                    CharSost
107200080411     C                   PARM                    UpperCase
107300080411     C                   PARM                    ChkNull
107400080411     C                   PARM                    CharNull
107500080411     C                   PARM                    EsitoBon
107600080411     C*
107700080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
107800080411     C                   IF        EsitoBon = '1'
107900080411     C                   EVAL      VABRSD = TxtInOut
108000080411     C                   ENDIF
108100080411     C*
108200080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
108300080411     C                   EVAL      TxtInOut   = VABIND
108400080411     C                   EVAL      EsitoBon   = *blanks
108500080411     C*
108600080411     C                   CALL      'XCHKCHAR'
108700080411     C                   PARM                    TxtInOut
108800080411     C                   PARM                    ElencoChar
108900080411     C                   PARM                    TipoElenco
109000080411     C                   PARM                    CharSost
109100080411     C                   PARM                    UpperCase
109200080411     C                   PARM                    ChkNull
109300080411     C                   PARM                    CharNull
109400080411     C                   PARM                    EsitoBon
109500080411     C*
109600080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
109700080411     C                   IF        EsitoBon = '1'
109800080411     C                   EVAL      VABIND = TxtInOut
109900080411     C                   ENDIF
110000080411     C*
110100080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
110200080411     C                   EVAL      TxtInOut   = VABLOD
110300080411     C                   EVAL      EsitoBon   = *blanks
110400080411     C*
110500080411     C                   CALL      'XCHKCHAR'
110600080411     C                   PARM                    TxtInOut
110700080411     C                   PARM                    ElencoChar
110800080411     C                   PARM                    TipoElenco
110900080411     C                   PARM                    CharSost
111000080411     C                   PARM                    UpperCase
111100080411     C                   PARM                    ChkNull
111200080411     C                   PARM                    CharNull
111300080411     C                   PARM                    EsitoBon
111400080411     C*
111500080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
111600080411     C                   IF        EsitoBon = '1'
111700080411     C                   EVAL      VABLOD = TxtInOut
111800080411     C                   ENDIF
111900080411     C*
112000080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
112100080411     C                   EVAL      TxtInOut   = VABNOT
112200080411     C                   EVAL      EsitoBon   = *blanks
112300080411     C                   EVAL      UpperCase  = *blanks
112400080411     C*
112500080411     C                   CALL      'XCHKCHAR'
112600080411     C                   PARM                    TxtInOut
112700080411     C                   PARM                    ElencoChar
112800080411     C                   PARM                    TipoElenco
112900080411     C                   PARM                    CharSost
113000080411     C                   PARM                    UpperCase
113100080411     C                   PARM                    ChkNull
113200080411     C                   PARM                    CharNull
113300080411     C                   PARM                    EsitoBon
113400080411     C*
113500080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
113600080411     C                   IF        EsitoBon = '1'
113700080411     C                   EVAL      VABNOT = TxtInOut
113800080411     C                   ENDIF
113900080411     C*
114000080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
114100080411     C                   EVAL      TxtInOut   = VABNT2
114200080411     C                   EVAL      EsitoBon   = *blanks
114300080411     C                   EVAL      UpperCase  = *blanks
114400080411     C*
114500080411     C                   CALL      'XCHKCHAR'
114600080411     C                   PARM                    TxtInOut
114700080411     C                   PARM                    ElencoChar
114800080411     C                   PARM                    TipoElenco
114900080411     C                   PARM                    CharSost
115000080411     C                   PARM                    UpperCase
115100080411     C                   PARM                    ChkNull
115200080411     C                   PARM                    CharNull
115300080411     C                   PARM                    EsitoBon
115400080411     C*
115500080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
115600080411     C                   IF        EsitoBon = '1'
115700080411     C                   EVAL      VABNT2 = TxtInOut
115800080411     C                   ENDIF
115900080411     C*
116000080411     C                   ENDSR
116100080411
116200080411
116300080411     C*----------------------------------------------------*
116400080411     C*  CONTROLLO NUMERICITA' CAMPI
116500080411     C*----------------------------------------------------*
116600080411     C     CHKNUM        BEGSR
116700080411     C*
116800080411     C                   IF        PiDecChr = *blanks
116900080411     C                   EVAL      PiDecChr = ','
117000080411     C                   ENDIF
117100080411     C*
117200080411     C                   CALL(e)   'ISNUMERIC'
117300080411     C                   PARM                    PiStr            30
117400080411     C                   PARM                    PiDecChr          1
117500080411     C                   PARM      *ZEROS        PiVal            30 9
117600080411     C                   PARM      '0'           PiInt             1
117700080411     C                   PARM      '0'           PiNum             1
117800080411     C                   IF        %error
117900080411     C                   EVAL      PiNum=*off
118000080411     C                   ENDIF
118100080411     C*
118200080411     C                   ENDSR
118300080411     C***
118400080411
118500080411
118600010530
118700990910
118800000613     C     *inzsr        BEGSR
118900990910     C*
119000990910     C     *entry        plist
119100990920     C                   parm                    tivlrds
119200990921     C                   parm      wrkesito      esito
119300000724     C                   parm                    prmlit
119400000710     C                   parm                    prmfir
119500010606     C*
119600010606     C* CALCOLA LA DATA CORRENTE
119700010606     C                   time                    wn14             14 0
119800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
119900010606     C                   z-add     wn8           g08dat
120000010606     C                   z-add     *zeros        g08inv
120100010606     C                   movel     '0'           g08err
120200010606     C                   call      'XSRDA8'
120300010606     C                   parm                    wlbda8
120400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
120500000613     C*
120600000613     C                   ENDSR
120700000613     C***
