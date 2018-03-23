000100100518      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200130117     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
004600010606     D*------------------
004700010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800010606     D*------------------
004900010606     D WLBDA8          DS                  INZ
005000010606     D  G08DAT                 1      8  0
005100010606     D  G08INV                 9     16  0
005200010606     D  G08ERR                17     17
005300010606     D  G08TGI                18     22  0
005400050209     D*------------------
005500100518     D* VARIABILI X "XCHKCHAR"
005600100518     D*------------------
005700100518     D TxtInOut        S           2048
005800100518     D ElencoChar      S            256
005900100518     D TipoElenco      S              1
006000100518     D CharSost        S              1
006100100518     D UpperCase       S              1
006200100518     D ChkNull         S              1
006300100518     D CharNull        S              1
006400100518     D EsitoBon        S              1
006500100518     D*------------------
006600100518
006700050209
006800050209
006900050209     C* /MAIN
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300010530     C*
007400010530     C                   exsr      opeini
007500010530     C                   exsr      rwvab
007600050209     C*
007700050209     C* Effettuo la chiamata al CLLE preposto
007800050209     C                   call(e)   'TITVVTC'
007900050209     C                   parm                    parccm
008000050209     C                   parm                    parmbr
008100050209     C                   parm      '2'           paropz
008200080411     C*
008300080411     C* Esegue lancio TISI95R solo x chiusura
008400080411     C                   CLEAR                   TISI95DS
008500080411     C                   EVAL      I95TLA = 'C'
008600080411     C                   CALL      'TISI95R'
008700080411     C                   PARM                    TISI95DS
008800010530     C*
008900010530     C                   seton                                        lr
009000010530
009100010530
009200010530
009300010530
009400010530     C*--------------------------------------------------------
009500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009600010530     C*--------------------------------------------------------
009700010530     C     PREELA        BEGSR
009800000724     C*
009900000724     C* SE OCCORRE SPEDIRE IN FILIALE
010000010530     C                   if        invfil <> *zeros and
010100010530     C                             flgGiro = '0'
010200010530     C*
010300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010530     C                   eval      flgGiro = '1'
010500010530     C*
010600010530     C                   endif
010700010530     C*
010800010530     C                   ENDSR
010900010530     C***
011000010530
011100010530
011200010530
011300010530
011400010530
011500010530
011600010530     C*--------------------------------------------------------
011700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010530     C*--------------------------------------------------------
011900010530     C     ENDELA        BEGSR
012000000613     C*
012100010530     C*
012200010530     C                   ENDSR
012300010530     C***
012400010530
012500910830     C*--------------------------------------------------------
012600020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
012700910830     C*--------------------------------------------------------
012800941229     C     RWVAB         BEGSR
012900990910     C*
013000990914     C                   if        not %open(tivin00r)
013100990908     C                   open      tivin00r
013200990914     C                   endif
013300020913     C                   if        not %open(fivabwwr)
013400020913     C                   open      fivabwwr
013500990914     C                   endif
013600050209     C*
013700050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013800050209     C                   exsr      prevat
013900050209     C*
014000050209     C                   if        chkcall = '0'
014100050209     C*
014200050209     C                   if        not %open(fivatwwr)
014300050209     C                   open      fivatwwr
014400050209     C                   endif
014500050209     C*
014600990910     C                   clear                   §CTROK
014700990910     C                   clear                   §CTRMO
014800990910     C                   clear                   §CTRNO
014900990910     C*
015000921023     C                   DO        *HIVAL
015100990913     C*
015200990915     C                   READ      tivin00r                               70
015300010618     C*
015400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015500010618     C                   if        vinflg = '1'
015600010618     C                   eval      flgOk = '1'
015700010618     C                   endif
015800010705     C*
015900010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
016000010705     C                   if        %subst(vindta:1:1) = '='
016100010705     C                   eval      %subst(vindta:1:1) = *blanks
016200010705     C                   endif
016300010705     C*
016400000905     C                   if        vindta > *blanks
016500000613     C                   add       1             rrnum
016600010601     C* ...elimino eventuali segni
016700010601     C     '+':' '       xlate     vindta        vindta
016800990913     C*
016900010601     C                   if        *in70 = *off and
017000990913     C                             (vinflg = *blanks
017100010711     C                              or vinflg = '0'
017200010711     C                              or vinflg = '2')
017300990913     C*
017400020913     C                   clear                   FIVAB000
017500050209     C                   clear                   FIVAT000
017600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017700010711     C                   if        vinflg = *blanks or vinflg = '0'
017800010711     C                   clear                   vinmsg
017900010711     C                   endif
018000010711     C*
018100990913     C                   eval      fnvabaar = vindta
018200080411     C*
018300080411     C                   testb     x'3F'         vhbatb                   02
018400080411     C                   if        not *in02
018500080411     C*
018600080411     C                   SETOFF                                       303132
018700080411     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
018800080411     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
018900080411     C*    OPPURE TUTTI BLANKS
019000080411     C*
019100080411     C                   TESTN                   VHBLNP               30
019200080411     C  N30              TESTN                   VHBLNP                 30
019300080411     C  N30              TESTN                   VHBLNP                   30
019400080411     C                   MOVE      VHBLNP        W001A             1
019500080411     C     *IN30         IFEQ      *OFF
019600080411     C     W001A         ORLT      '0'
019700080411     C                   SETON                                        31
019800080411     C                   eval      vinmsg = %trimr(vinmsg)
019900080411     C                             + ' ' + 'VHBLNP'
020000080411     C                   ELSE
020100080411     C                   MOVE      VHBLNP        VABLNP
020200080411     C     VABLNP        IFLE      *ZEROS
020300080411     C                   SETON                                        31
020400080411     C                   eval      vinmsg = %trimr(vinmsg)
020500080411     C                             + ' ' + 'VHBLNP'
020600080411     C                   ENDIF
020700080411     C                   ENDIF
020800080411     C*
020900080411     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000080411     C                   eval      depfil = VABLNP
021100080411     C                   exsr      repfil
021200080411     C                   if        depfil = invfil
021300080411     C                   if        vlrpoi = 999
021400080411     C                   MOVE(P)   invfil        VABFGS
021500080411     C                   else
021600080411     C                   MOVE(P)   vlrpoi        VABFGS
021700080411     C                   endif
021800080411     C*
021900080411     C                   exsr      PREELA
022000080411     C*
022100080411     C                   TESTN                   VHBCCM               30
022200080411     C  N30              TESTN                   VHBCCM                 30
022300080411     C  N30              TESTN                   VHBCCM                   30
022400080411     C                   MOVE      VHBCCM        W001A             1
022500080411     C     *IN30         IFEQ      *OFF
022600080411     C     W001A         ORLT      '0'
022700080411     C                   SETON                                        31
022800080411     C                   eval      vinmsg = %trimr(vinmsg)
022900080411     C                             + ' ' + 'VHBCCM'
023000080411     C                   ELSE
023100080411     C                   MOVE      VHBCCM        VABCCM
023200080411     C     VABCCM        IFLE      *ZEROS
023300080411     C                   SETON                                        31
023400080411     C                   eval      vinmsg = %trimr(vinmsg)
023500080411     C                             + ' ' + 'VHBCCM'
023600080411     C                   ENDIF
023700080411     C                   ENDIF
023800080411     C*
023900080411     C                   TESTN                   VHBAAS               30
024000080411     C  N30              TESTN                   VHBAAS                 30
024100080411     C  N30              TESTN                   VHBAAS                   30
024200080411     C                   MOVE      VHBAAS        W001A             1
024300080411     C     *IN30         IFEQ      *OFF
024400080411     C     W001A         ORLT      '0'
024500080411     C     W001A         ANDNE     *BLANK
024600080411     C                   SETON                                        32
024700080411     C                   MOVEL     datcor        VABAAS
024800080411     C                   eval      vinmsg = %trimr(vinmsg)
024900080411     C                             + ' ' + 'VHBAAS'
025000080411     C                   ELSE
025100080411     C                   MOVE      VHBAAS        VABAAS
025200080411     C                   ENDIF
025300080411     C*
025400080411     C                   TESTN                   VHBMGS               30
025500080411     C  N30              TESTN                   VHBMGS                 30
025600080411     C  N30              TESTN                   VHBMGS                   30
025700080411     C                   MOVE      VHBMGS        W001A             1
025800080411     C     *IN30         IFEQ      *OFF
025900080411     C     W001A         ORLT      '0'
026000080411     C     W001A         ANDNE     *BLANK
026100080411     C                   SETON                                        32
026200080411     C                   MOVE      '0101'        VABMGS
026300080411     C                   eval      vinmsg = %trimr(vinmsg)
026400080411     C                             + ' ' + 'VHBMGS'
026500080411     C                   ELSE
026600080411     C                   MOVE      VHBMGS        VABMGS
026700080411     C                   ENDIF
026800080411     C*
026900080411     C                   TESTN                   VHBNRS               30
027000080411     C  N30              TESTN                   VHBNRS                 30
027100080411     C  N30              TESTN                   VHBNRS                   30
027200080411     C                   MOVE      VHBNRS        W001A             1
027300080411     C     *IN30         IFEQ      *OFF
027400080411     C     W001A         ORLT      '0'
027500080411     C     W001A         ANDNE     *BLANK
027600080411     C                   SETON                                        32
027700080411     C                   MOVE      *ZERO         VABNRS
027800080411     C                   eval      vinmsg = %trimr(vinmsg)
027900080411     C                             + ' ' + 'VHBNRS'
028000080411     C                   ELSE
028100080411     C                   MOVE      VHBNRS        VABNRS
028200080411     C                   ENDIF
028300080411     C*
028400080411     C                   TESTN                   VHBNSP               30
028500080411     C  N30              TESTN                   VHBNSP                 30
028600080411     C  N30              TESTN                   VHBNSP                   30
028700080411     C                   MOVE      VHBNSP        W001A             1
028800080411     C     *IN30         IFEQ      *OFF
028900080411     C     W001A         ORLT      '0'
029000080411     C     W001A         ANDNE     *BLANK
029100080411     C                   SETON                                        32
029200080411     C                   MOVE      *ZERO         VABNSP
029300080411     C                   eval      vinmsg = %trimr(vinmsg)
029400080411     C                             + ' ' + 'VHBNSP'
029500080411     C                   ELSE
029600080411     C                   MOVE      VHBNSP        VABNSP
029700080411     C                   ENDIF
029800080411     C*
029900080411     C                   TESTN                   VHBLNA               30
030000080411     C  N30              TESTN                   VHBLNA                 30
030100080411     C  N30              TESTN                   VHBLNA                   30
030200080411     C                   MOVE      VHBLNA        W001A             1
030300080411     C     *IN30         IFEQ      *OFF
030400080411     C     W001A         ORLT      '0'
030500080411     C     W001A         ANDNE     *BLANK
030600080411     C                   SETON                                        32
030700080411     C                   MOVE      *ZERO         VABLNA
030800080411     C                   eval      vinmsg = %trimr(vinmsg)
030900080411     C                             + ' ' + 'VHBLNA'
031000080411     C                   ELSE
031100080411     C                   MOVE      VHBLNA        VABLNA
031200080411     C                   ENDIF
031300080411     C*
031400080411     C                   TESTN                   VHBCTR               30
031500080411     C  N30              TESTN                   VHBCTR                 30
031600080411     C  N30              TESTN                   VHBCTR                   30
031700080411     C                   MOVE      VHBCTR        W001A             1
031800080411     C     *IN30         IFEQ      *OFF
031900080411     C     W001A         ORLT      '0'
032000080411     C     W001A         ANDNE     *BLANK
032100080411     C                   SETON                                        32
032200080411     C                   MOVE      *ZERO         VABCTR
032300080411     C                   eval      vinmsg = %trimr(vinmsg)
032400080411     C                             + ' ' + 'VHBCTR'
032500080411     C                   ELSE
032600080411     C                   MOVE      VHBCTR        VABCTR
032700080411     C                   ENDIF
032800080411     C*
032900080411     C                   MOVE      *ZERO         VABIAS
033000080411     C                   MOVE      VHBIAS        WASC15
033100080411     C                   TESTN                   WI15                 30
033200080411     C  N30              TESTN                   WI15                   30
033300080411     C  N30              TESTN                   WI15                     30
033400080411     C                   MOVE      WI15          W001A             1
033500080411     C     *IN30         IFEQ      *OFF
033600080411     C     W001A         ORLT      '0'
033700080411     C     W001A         ANDNE     *BLANK
033800080411     C                   SETON                                        32
033900080411     C                   MOVE      *ZERO         VABIAS
034000080411     C                   eval      vinmsg = %trimr(vinmsg)
034100080411     C                             + ' ' + 'VHBIAS'
034200080411     C                   ELSE
034300080411     C                   MOVEL     WI15          VABIAS
034400080411     C                   TESTN                   WD15                 30
034500080411     C  N30              TESTN                   WD15                   30
034600080411     C  N30              TESTN                   WD15                     30
034700080411     C                   MOVE      WD15          W001A             1
034800080411     C     *IN30         IFEQ      *OFF
034900080411     C     W001A         ORLT      '0'
035000080411     C     W001A         ANDNE     *BLANK
035100080411     C                   SETON                                        32
035200080411     C                   MOVE      *ZERO         VABIAS
035300080411     C                   eval      vinmsg = %trimr(vinmsg)
035400080411     C                             + ' ' + 'VHBIAS'
035500080411     C                   ELSE
035600080411     C                   MOVE      WD15          VABIAS
035700080411     C                   ENDIF
035800080411     C                   ENDIF
035900080411     C*
036000080411     C                   TESTN                   VHBNCL               30
036100080411     C  N30              TESTN                   VHBNCL                 30
036200080411     C  N30              TESTN                   VHBNCL                   30
036300080411     C                   MOVE      VHBNCL        W001A             1
036400080411     C     *IN30         IFEQ      *OFF
036500080411     C     W001A         ORLT      '0'
036600080411     C                   SETON                                        32
036700080411     C                   MOVE      *ZERO         VABNCL
036800080411     C                   eval      vinmsg = %trimr(vinmsg)
036900080411     C                             + ' ' + 'VHBNCL'
037000080411     C                   ELSE
037100080411     C                   MOVE      VHBNCL        VABNCL
037200080411     C                   ENDIF
037300080411     C*
037400080411     C                   MOVE      *ZERO         VABPKB
037500080411     C                   MOVE      VHBPKB        WASC09
037600080411     C                   IF        WI09 = *blanks
037700080411     C                   EVAL      WI09 = *zeros
037800080411     C                   ENDIF
037900080411     C                   TESTN                   WI09                 30
038000080411     C  N30              TESTN                   WI09                   30
038100080411     C  N30              TESTN                   WI09                     30
038200080411     C                   MOVE      WI09          W001A             1
038300080411     C     *IN30         IFEQ      *OFF
038400080411     C     W001A         ORLT      '0'
038500080411     C                   SETON                                        32
038600080411     C                   MOVE      *ZERO         VABPKB
038700080411     C                   eval      vinmsg = %trimr(vinmsg)
038800080411     C                             + ' ' + 'VHBPKB'
038900080411     C                   ELSE
039000080411     C                   MOVEL     WI09          VABPKB
039100080411     C                   IF        WD09 = *blanks
039200080411     C                   EVAL      WD09 = *zeros
039300080411     C                   ENDIF
039400080411     C                   TESTN                   WD09                 30
039500080411     C  N30              TESTN                   WD09                     30
039600080411     C                   MOVE      WD09          W001A             1
039700080411     C     *IN30         IFEQ      *OFF
039800080411     C     W001A         ORLT      '0'
039900080411     C     W001A         ANDNE     *BLANK
040000080411     C                   SETON                                        32
040100080411     C                   MOVE      *ZERO         VABPKB
040200080411     C                   eval      vinmsg = %trimr(vinmsg)
040300080411     C                             + ' ' + 'VHBPKB'
040400080411     C                   ELSE
040500080411     C                   MOVE      WD09          VABPKB
040600080411     C                   ENDIF
040700080411     C                   ENDIF
040800080411     C*
040900080411     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
041000080411     C                   IF        *IN51 = *OFF
041100080411     C                   MOVE      *ZERO         VABVLB
041200080411     C                   MOVE      VHBVLB        WASC07
041300080411     C                   IF        WI07 = *blanks
041400080411     C                   EVAL      WI07 = *zeros
041500080411     C                   ENDIF
041600080411     C                   TESTN                   WI07                 30
041700080411     C  N30              TESTN                   WI07                   30
041800080411     C  N30              TESTN                   WI07                     30
041900080411     C                   MOVE      WI07          W001A             1
042000080411     C     *IN30         IFEQ      *OFF
042100080411     C     W001A         ORLT      '0'
042200080411     C     W001A         ANDNE     *BLANK
042300080411     C                   SETON                                        32
042400080411     C                   MOVE      *ZERO         VABVLB
042500080411     C                   eval      vinmsg = %trimr(vinmsg)
042600080411     C                             + ' ' + 'VHBVLB'
042700080411     C                   ELSE
042800080411     C                   MOVEL     WI07          VABVLB
042900080411     C                   IF        WD07 = *blanks
043000080411     C                   EVAL      WD07 = *zeros
043100080411     C                   ENDIF
043200080411     C                   TESTN                   WD07                 30
043300080411     C  N30              TESTN                   WD07                   30
043400080411     C  N30              TESTN                   WD07                     30
043500080411     C                   MOVE      WD07          W001A             1
043600080411     C     *IN30         IFEQ      *OFF
043700080411     C     W001A         ORLT      '0'
043800080411     C     W001A         ANDNE     *BLANK
043900080411     C                   SETON                                        32
044000080411     C                   MOVE      *ZERO         VABVLB
044100080411     C                   eval      vinmsg = %trimr(vinmsg)
044200080411     C                             + ' ' + 'VHBVLB'
044300080411     C                   ELSE
044400080411     C                   MOVE      WD07          VABVLB
044500080411     C                   ENDIF
044600080411     C                   ENDIF
044700080411     C                   ELSE
044800080411     C* ...altrimenti forzo azzeramento del volume
044900080411     C                   Z-ADD     *zeros        VABVLB
045000080411     C                   ENDIF
045100080411     C*
045200080411     C                   MOVE      *ZERO         VABQFT
045300080411     C                   MOVE      VHBQFT        WASC15
045400080411     C                   IF        WI15 = *blanks
045500080411     C                   EVAL      WI15 = *zeros
045600080411     C                   ENDIF
045700080411     C                   TESTN                   WI15                 30
045800080411     C  N30              TESTN                   WI15                   30
045900080411     C  N30              TESTN                   WI15                     30
046000080411     C                   MOVE      WI15          W001A             1
046100080411     C     *IN30         IFEQ      *OFF
046200080411     C     W001A         ORLT      '0'
046300080411     C     W001A         ANDNE     *BLANK
046400080411     C                   SETON                                        32
046500080411     C                   MOVE      *ZERO         VABQFT
046600080411     C                   eval      vinmsg = %trimr(vinmsg)
046700080411     C                             + ' ' + 'VHBQFT'
046800080411     C                   ELSE
046900080411     C                   MOVEL     WI15          VABQFT
047000080411     C                   IF        WD15 = *blanks
047100080411     C                   EVAL      WD15 = *zeros
047200080411     C                   ENDIF
047300080411     C                   TESTN                   WD15                 30
047400080411     C  N30              TESTN                   WD15                   30
047500080411     C  N30              TESTN                   WD15                     30
047600080411     C                   MOVE      WD15          W001A             1
047700080411     C     *IN30         IFEQ      *OFF
047800080411     C     W001A         ORLT      '0'
047900080411     C     W001A         ANDNE     *BLANK
048000080411     C                   SETON                                        32
048100080411     C                   MOVE      *ZERO         VABQFT
048200080411     C                   eval      vinmsg = %trimr(vinmsg)
048300080411     C                             + ' ' + 'VHBQFT'
048400080411     C                   ELSE
048500080411     C                   MOVE      WD15          VABQFT
048600080411     C                   ENDIF
048700080411     C                   ENDIF
048800080411     C*
048900100518     C                   MOVEL(P)  VHBCAS        PiStr
049000080411     C                   EXSR      CHKNUM
049100080411     C                   IF        PiNum=*on
049200080411     C                   Z-ADD     PiVal         VABCAS
049300080411     C                   ELSE
049400080411     C                   SETON                                        32
049500080411     C                   Z-ADD     *zeros        VABCAS
049600080411     C                   EVAL      vinmsg = %trimr(vinmsg)
049700080411     C                             + ' ' + 'VHBCAS'
049800080411     C                   ENDIF
049900080411     C*
050000080411     C                   TESTN                   VHBRMN               30
050100080411     C  N30              TESTN                   VHBRMN                 30
050200080411     C  N30              TESTN                   VHBRMN                   30
050300080411     C                   MOVE      VHBRMN        W001A             1
050400080411     C     *IN30         IFEQ      *OFF
050500080411     C     W001A         ORLT      '0'
050600080411     C                   SETON                                        32
050700080411     C                   MOVE      *ZERO         VABRMN
050800080411     C                   eval      vinmsg = %trimr(vinmsg)
050900080411     C                             + ' ' + 'VHBRMN'
051000080411     C                   ELSE
051100080411     C                   MOVE      VHBRMN        VABRMN
051200080411     C                   ENDIF
051300080411     C*
051400080411     C                   TESTN                   VHBNCD               30
051500080411     C  N30              TESTN                   VHBNCD                 30
051600080411     C  N30              TESTN                   VHBNCD                   30
051700080411     C                   MOVE      VHBNCD        W001A             1
051800080411     C     *IN30         IFEQ      *OFF
051900080411     C     W001A         ORLT      '0'
052000080411     C     W001A         ANDNE     *BLANK
052100080411     C                   SETON                                        32
052200080411     C                   MOVE      *ZERO         VABNCD
052300080411     C                   eval      vinmsg = %trimr(vinmsg)
052400080411     C                             + ' ' + 'VHBNCD'
052500080411     C                   ELSE
052600080411     C                   MOVE      VHBNCD        VABNCD
052700080411     C                   ENDIF
052800080411     C*
052900080411     C                   TESTN                   VHBNCA               30
053000080411     C  N30              TESTN                   VHBNCA                 30
053100080411     C  N30              TESTN                   VHBNCA                   30
053200080411     C                   MOVE      VHBNCA        W001A             1
053300080411     C     *IN30         IFEQ      *OFF
053400080411     C     W001A         ORLT      '0'
053500080411     C     W001A         ANDNE     *BLANK
053600080411     C                   SETON                                        32
053700080411     C                   MOVE      *ZERO         VABNCA
053800080411     C                   eval      vinmsg = %trimr(vinmsg)
053900080411     C                             + ' ' + 'VHBNCA'
054000080411     C                   ELSE
054100080411     C                   MOVE      VHBNCA        VABNCA
054200080411     C                   ENDIF
054300080411     C*
054400080411     C                   TESTN                   VHBZNC               30
054500080411     C  N30              TESTN                   VHBZNC                 30
054600080411     C  N30              TESTN                   VHBZNC                   30
054700080411     C                   MOVE      VHBZNC        W001A             1
054800080411     C     *IN30         IFEQ      *OFF
054900080411     C     W001A         ORLT      '0'
055000080411     C     W001A         ANDNE     *BLANK
055100080411     C                   SETON                                        32
055200080411     C                   MOVE      *ZERO         VABZNC
055300080411     C                   eval      vinmsg = %trimr(vinmsg)
055400080411     C                             + ' ' + 'VHBZNC'
055500080411     C                   ELSE
055600080411     C                   MOVE      VHBZNC        VABZNC
055700080411     C                   ENDIF
055800080411     C*
055900080411     C                   TESTN                   VHBDCR               30
056000080411     C  N30              TESTN                   VHBDCR                 30
056100080411     C  N30              TESTN                   VHBDCR                   30
056200080411     C                   MOVE      VHBDCR        W001A             1
056300080411     C     *IN30         IFEQ      *OFF
056400080411     C     W001A         ORLT      '0'
056500080411     C     W001A         ANDNE     *BLANK
056600080411     C                   SETON                                        32
056700080411     C                   MOVE      *ZERO         VABDCR
056800080411     C                   eval      vinmsg = %trimr(vinmsg)
056900080411     C                             + ' ' + 'VHBDCR'
057000080411     C                   ELSE
057100080411     C                   MOVE      VHBDCR        VABDCR
057200080411     C                   ENDIF
057300080411     C*
057400080411     C                   TESTN                   VHBHCR               30
057500080411     C  N30              TESTN                   VHBHCR                 30
057600080411     C  N30              TESTN                   VHBHCR                   30
057700080411     C                   MOVE      VHBHCR        W001A             1
057800080411     C     *IN30         IFEQ      *OFF
057900080411     C     W001A         ORLT      '0'
058000080411     C     W001A         ANDNE     *BLANK
058100080411     C                   SETON                                        32
058200080411     C                   MOVE      *ZERO         VABHCR
058300080411     C                   eval      vinmsg = %trimr(vinmsg)
058400080411     C                             + ' ' + 'VHBHCR'
058500080411     C                   ELSE
058600080411     C                   MOVE      VHBHCR        VABHCR
058700080411     C                   ENDIF
058800080411     C*
058900080411     C                   MOVE      *ZERO         VABVMD
059000080411     C                   MOVE      VHBVMD        WASC15
059100080411     C                   IF        WI15 = *blanks
059200080411     C                   EVAL      WI15 = *zeros
059300080411     C                   ENDIF
059400080411     C                   TESTN                   WI15                 30
059500080411     C  N30              TESTN                   WI15                   30
059600080411     C  N30              TESTN                   WI15                     30
059700080411     C                   MOVE      WI15          W001A             1
059800080411     C     *IN30         IFEQ      *OFF
059900080411     C     W001A         ORLT      '0'
060000080411     C     W001A         ANDNE     *BLANK
060100080411     C                   SETON                                        32
060200080411     C                   MOVE      *ZERO         VABVMD
060300080411     C                   eval      vinmsg = %trimr(vinmsg)
060400080411     C                             + ' ' + 'VHBVMD'
060500080411     C                   ELSE
060600080411     C                   MOVEL     WI15          VABVMD
060700080411     C                   IF        WD15 = *blanks
060800080411     C                   EVAL      WD15 = *zeros
060900080411     C                   ENDIF
061000080411     C                   TESTN                   WD15                 30
061100080411     C  N30              TESTN                   WD15                   30
061200080411     C  N30              TESTN                   WD15                     30
061300080411     C                   MOVE      WD15          W001A             1
061400080411     C     *IN30         IFEQ      *OFF
061500080411     C     W001A         ORLT      '0'
061600080411     C     W001A         ANDNE     *BLANK
061700080411     C                   SETON                                        32
061800080411     C                   MOVE      *ZERO         VABVMD
061900080411     C                   eval      vinmsg = %trimr(vinmsg)
062000080411     C                             + ' ' + 'VHBVMD'
062100080411     C                   ELSE
062200080411     C                   MOVE      WD15          VABVMD
062300080411     C                   ENDIF
062400080411     C                   ENDIF
062500080411     C*
062600080411     C                   TESTN                   VHBANT               30
062700080411     C  N30              TESTN                   VHBANT                 30
062800080411     C  N30              TESTN                   VHBANT                   30
062900080411     C                   MOVE      VHBANT        W001A             1
063000080411     C     *IN30         IFEQ      *OFF
063100080411     C     W001A         ORLT      '0'
063200080411     C     W001A         ANDNE     *BLANK
063300080411     C                   SETON                                        32
063400080411     C                   MOVE      *ZERO         VABANT
063500080411     C                   eval      vinmsg = %trimr(vinmsg)
063600080411     C                             + ' ' + 'VHBANT'
063700080411     C                   ELSE
063800080411     C                   MOVE      VHBANT        VABANT
063900080411     C                   ENDIF
064000080411     C*
064100080411     C* CAMPI ALFABETICI:
064200080411     C                   MOVE      VHBATB        VABATB
064300080411     C                   MOVE      VHBCBO        VABCBO
064400080411     C                   MOVE      VHBRSD        VABRSD
064500080411     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
064600080411     C     '@':'A'       XLATE     VABRSD        VABRSD
064700080411     C* ==
064800080411     C                   MOVE      VHBRD2        VABRD2
064900080411     C                   MOVE      VHBIND        VABIND
065000080411     C                   MOVE      VHBCAD        VABCAD
065100080411     C                   MOVE      VHBLOD        VABLOD
065200080411     C                   MOVE      VHBPRD        VABPRD
065300080411     C                   MOVE      VHBNZD        VABNZD
065400080411     C                   MOVE      VHBGC1        VABGC1
065500080411     C                   MOVE      VHBGC2        VABGC2
065600080411     C                   MOVE      VHBTSP        VABTSP
065700080411     C                   MOVE      VHBVAS        VABVAS
065800080411     C                   MOVE      VHBNAS        VABNAS
065900080411     C                   MOVE      VHBTIC        VABTIC
066000080411     C                   MOVE      VHBVCA        VABVCA
066100080411     C                   MOVE      VHBGCA        VABGCA
066200080411     C                   MOVE      VHBRMA        VABRMA
066300080411     C                   MOVE      VHBXCO        VABXCO
066400080411     C                   MOVE      VHBNOT        VABNOT
066500080411     C                   MOVE      VHBNT2        VABNT2
066600080411     C                   MOVE      VHBCTM        VABCTM
066700080411     C                   MOVE      VHBFFD        VABFFD
066800080411     C                   MOVE      VHBTCR        VABTCR
066900080411     C                   MOVE      VHBCTS        VABCTS
067000080411     C                   MOVE      VHBFTM        VABFTM
067100080411     C                   MOVE      VHBVAD        VABVAD
067200080411     C                   MOVE      VHBGMA        VABGMA
067300080411     C                   MOVE      VHBGGA        VABGGA
067400080411     C                   MOVE      VHBGVA        VABGVA
067500080411     C                   MOVE      VHBTC1        VABTC1
067600080411     C                   MOVE      VHBTC2        VABTC2
067700080411     C                   MOVE      VHBSCL        VABSCL
067800080411     C                   MOVE      VHBRMO        VABRMO
067900080411     C                   MOVE      VHBCMO        VABCMO
068000080411     C                   MOVE      VHBNMO        VABNMO
068100080411     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
068200080411     C* NELLA NAZIONE MITTENTE ORIGINALE
068300080411     C                   IF        %trim(VHBNMO) < 'A'
068400080411     C                   MOVE      *BLANKS       VABNMO
068500080411     C                   ENDIF
068600080411     C*
068700080411     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
068800080411     C                   IF        %trim(VABNZD) = 'I'   OR
068900080411     C                             %trim(VABNZD) = 'IT'  OR
069000080411     C                             %trim(VABNZD) = 'ITA'
069100080411     C                   EVAL      VABNZD = *blanks
069200080411     C                   ENDIF
069300080411     C*
069400080411     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
069500080411     C                   IF        *IN52 = *ON
069600080411     C* Reperisco la provincia dal CAP e dalla località
069700080411     C                   IF        VABCAD <> *blanks AND
069800080411     C                             VABPRD  = *blanks AND
069900080411     C                             VABNZD  = *blanks
070000080411     C                   CLEAR                   TISI95DS
070100080411     C                   EVAL      I95TCN = '3'
070200080411     C                   Z-ADD     datcor        I95DAT
070300080411     C                   EVAL      I95CAP = VABCAD
070400080411     C                   EVAL      I95LOC = VABLOD
070500080411     C                   CALL      'TISI95R'
070600080411     C                   PARM                    TISI95DS
070700080411     C                   EVAL      VABPRD = O95PRV
070800080411     C                   ENDIF
070900080411     C                   ENDIF
071000080411     C*
071100080411     C* "bonifico" i dati d input da eventuali caratteri indesiderati
071200080411     C                   exsr      exeBon
071300930409     C*
071400010604     C  N31              ADD       1             §CTROK            7 0
071500010604     C   32              ADD       1             §CTRMO            7 0
071600010604     C   31              ADD       1             §CTRNO            7 0
071700080411     C  N31              WRITE     FIVAB000
071800100518     C  N31              EXSR      WRIVAT_E
071900990910     C*
072000010601     C                   if        *in31 = *off and
072100010601     C                             *in32 = *off
072200990910     C                   eval      vinflg = '1'
072300990910     C                   else
072400990910     C                   eval      vinflg = '2'
072500990910     C                   endif
072600991022     C                   endif
072700990910     C                   endif
072800010601     C*
072900010601     C                   endif
073000000905     C*
073100000905     C                   else
073200000905     C                   eval      vinflg = '1'
073300000905     C                   endif
073400000905     C*
073500000905     C  N70              update    tivin000
073600991022     C*
073700991022     C  N70              ENDdo
073800010530     C
073900010530     C*
074000010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
074100010530     C                   if        cntNonEl = *zeros or
074200010530     C                             flgMulti = '0'
074300990910     C* Se non ci sono record con errori ...
074400010530     C                   if        §ctrno = 0 and
074500010601     C                             §ctrmo = 0 and
074600010530     C                             flgStato <> '2'
074700990910     C* ... restituisco esito OK.
074800990921     C                   eval      wrkesito = '0'
074900990910     C                   else
075000010601     C                   if        §ctrok > 0
075100990921     C                   eval      wrkesito = '1'
075200000710     C                   else
075300010615     C                   if        flgOk = '0'
075400010615     C                   eval      wrkesito = '2'
075500010615     C                   else
075600010615     C                   eval      wrkesito = '6'
075700010615     C                   endif
075800990910     C                   endif
075900000710     C                   endif
076000010530     C                   else
076100010530     C                   eval      wrkesito = '9'
076200010530     C                   endif
076300050209     C*
076400050209     C                   else
076500050209     C                   eval      wrkesito = '2'
076600050209     C                   endif
076700990910     C*
076800990914     C                   if        %open(tivin00r)
076900990908     C                   close     tivin00r
077000990914     C                   endif
077100020913     C                   if        %open(fivabwwr)
077200020913     C                   close     fivabwwr
077300990914     C                   endif
077400050209     C                   if        %open(fivatwwr)
077500050209     C                   close     fivatwwr
077600050209     C                   endif
077700010530     C*
077800010530     C                   if        vlrpoi <> 999
077900010530     C                   eval      invfil = vlrpoi
078000010530     C                   endif
078100990910     C*
078200990920     C                   if        §ctrok > 0
078300010530     C                             and invfil > *zeros
078400000613     C                   exsr      invio
078500990920     C                   endif
078600010612     C*
078700010612     C                   if        flgGiro = '1'
078800010612     C                   exsr      endela
078900010612     C                   endif
079000990920     C*
079100910830     C                   ENDSR
079200000613     C***
079300050209
079400050209
079500050209
079600050209     C*----------------------------------------------------*
079700050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
079800050209     C*----------------------------------------------------*
079900050209     C     PREVAT        BEGSR
080000050209     C*
080100050209     C* Compongo il nome del membro da dare al FIVATWWR
080200050209     C                   eval      parmbr = vlrhdl
080300050209     C                   movel     'M'           parmbr
080400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
080500050209     C                   eval      paropz = '1'
080600050209     C* Effettuo la chiamata al CLLE preposto
080700050209     C                   call(e)   'TITVVTC'
080800050209     C                   parm                    parccm
080900050209     C                   parm                    parmbr
081000050209     C                   parm                    paropz
081100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
081200050209     C                   if        %error
081300050209     C                   movel     '1'           chkcall
081400050209     C                   else
081500050209     C                   movel     '0'           chkcall
081600050209     C                   endif
081700050209     C*
081800050209     C                   ENDSR
081900050209
082000050209
082100990920
082200050209     C*----------------------------------------------------*
082300050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
082400050209     C*----------------------------------------------------*
082500100518     C     WRIVAT_E      BEGSR
082600050209     C*
082700050209     C* Valorizzo il buffer del file dettagli barcode colli
082800050209     C                   EVAL      VATFGS = VABFGS
082900050209     C                   EVAL      VATAAS = VABAAS
083000050209     C                   EVAL      VATLNP = VABLNP
083100050209     C                   EVAL      VATNSP = VABNSP
083200050209     C                   EVAL      VATCCM = VABCCM
083300050209     C                   EVAL      VATTRC='E'
083400100518     C*
083500100518     C* X ogni collo indicato in testata
083600100518     C                   z-add     1             wColli            5 0
083700100518     C                   dow       wColli <= VABNCL
083800100518     C*
083900100518     C* Valorizzo il buffer di scrittura del FIVAT
084000100518     C                   move(p)   wColli        wColliAlfa        4
084100100519     C                   eval      VATNOT = %subst(VHBRMN:7:10)+wColliAlfa
084200100518     C                   exsr      wrivat                                       => scarico VAT
084300100518     C*
084400100518     C                   add       1             wColli
084500100518     C                   enddo
084600050209     C*
084700050209     C                   ENDSR
084800100518     C*----------------------------------------------------*
084900100518     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
085000100518     C*----------------------------------------------------*
085100100518     C     WRIVAT        BEGSR
085200100518     C*
085300100518     C* Scrivo solo se valorizzato qualcosa
085400100518     C                   IF        VATNOT <> *blanks
085500100518     C                   WRITE     FIVAT000
085600100518     C                   ENDIF
085700100518     C*
085800100518     C                   ENDSR
085900010530
086000010530
086100050209
086200010530
086300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
086400010530     C     repfil        BEGSR
086500010530     C*
086600010530     C                   if        invfil = *zeros and
086700010530     C                             depfil > *zeros and
086800010613     C                             (vinflg = *blanks or
086900010613     C                              vinflg = *zeros)
087000010530     C
087100010530     C                   eval      invfil = depfil
087200010530     C                   endif
087300010530     C*
087400010530     C                   if        depfil <> invfil and
087500010530     C                             invfil > *zeros
087600010530     C                   eval      flgMulti = '1'
087700010530     C                   if        vinflg = *blanks
087800010530     C                   add       1             cntNonEl
087900010530     C                   endif
088000010530     C                   endif
088100010530     C*
088200010530     C                   if        vinflg = '2'
088300010530     C                   eval      flgStato = '2'
088400010530     C                   endif
088500010530     C*
088600010530     C                   ENDSR
088700010530     C***
088800050209
088900050209
089000050209
089100050209
089200050209      /TITLE Invio dei dati al punto operativo.
089300050209     C     invio         BEGSR
089400050209     C*
089500050209     C* 1° invio FIVAT
089600050209     C                   reset                   dscmz
089700050209     C                   move      vlrpoi        cmzdst
089800050209     C                   eval      cmzfld = 'FIVATWWR'
089900050209     C                   eval      cmzmbd = vlrhdl
090000050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
090100050209     C***                if        prmfir = *blanks
090200050209     C                   eval      cmzfla = 'FIVAT00F'
090300050209     C                   eval      cmzmba = 'FIVAT00F'
090400050209     C***                else
090500050209     C***                eval      cmzfla = prmfir
090600050209     C***                eval      cmzmba = prmfir
090700050209     C***                endif
090800050209     C                   eval      cmznrr = *zeros
090900050209     C                   move      §ctrok        cmznrr
091000050209     C                   eval      cmzlba = vlrfl1
091100050209     C                   call(e)   'TIS711C'
091200050209     C                   parm                    dscmz
091300050209     C                   parm      *blanks       esito
091400050209     C                   if        %error
091500050209     C                             or cmzerr = '1'
091600050209     C                             or esito  = '1'
091700050209     C                   eval      wrkesito = '3'
091800050209     C                   else
091900050209     C*
092000050209     C* 2° invio FIVAB
092100050209     C                   reset                   dscmz
092200050209     C                   move      vlrpoi        cmzdst
092300050209     C                   eval      cmzfld = vlrfou
092400050209     C                   eval      cmzmbd = vlrhdl
092500050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
092600050209     C***                if        prmfir = *blanks
092700050209     C                   eval      cmzfla = 'FIVAB00F'
092800050209     C                   eval      cmzmba = 'FIVAB00F'
092900050209     C***                else
093000050209     C***                eval      cmzfla = prmfir
093100050209     C***                eval      cmzmba = prmfir
093200050209     C***                endif
093300050209     C                   eval      cmznrr = *zeros
093400050209     C                   move      §ctrok        cmznrr
093500050209     C                   eval      cmzlba = vlrfl1
093600050209     C                   call(e)   'TIS711C'
093700050209     C                   parm                    dscmz
093800050209     C                   parm      *blanks       esito
093900050209     C                   if        %error
094000050209     C                             or cmzerr = '1'
094100050209     C                             or esito  = '1'
094200050209     C                   eval      wrkesito = '3'
094300050209     C                   endif
094400050209     C                   endif
094500050209     C*
094600050209     C                   ENDSR
094700050209     C***
094800010530
094900010530
095000010530
095100010530
095200010530      /TITLE Invio dei dati al punto operativo.
095300010530     C     opeini        BEGSR
095400010530     C*
095500010530     C* Inizializzo flag e contatori operativi
095600010530     C                   movel     '0'           flgGiro           1
095700010530     C                   movel     '0'           flgMulti          1
095800010530     C                   movel     '1'           flgStato          1
095900010615     C                   movel     '0'           flgOk             1
096000010530     C                   z-add     *zeros        cntNonEl         10 0
096100010530     C                   z-add     *zeros        depfil            3 0
096200010530     C                   z-add     *zeros        invfil            3 0
096300030407     C*
096400050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
096500030407     C                   if        %subst(vlrppt:1:1) = 'S'
096600030407     C                   seton                                        50
096700030407     C                   else
096800030407     C                   setoff                                       50
096900030407     C                   endif
097000041001     C*
097100050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
097200041001     C                   if        %subst(vlrppt:2:1) = 'S'
097300041001     C                   seton                                        51
097400041001     C                   else
097500041001     C                   setoff                                       51
097600041001     C                   endif
097700080411     C*
097800080411     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
097900080411     C* (naturalmente solo se nn già presente)
098000080411     C                   if        %subst(vlrppt:3:1) = 'S'
098100080411     C                   seton                                        52
098200080411     C                   else
098300080411     C                   setoff                                       52
098400080411     C                   endif
098500010530     C*
098600010530     C                   ENDSR
098700010530     C***
098800010530
098900010530
099000070411
099100070411     C     *pssr         BEGSR
099200070411     C*
099300070411     C                   if        %open(tivin00r)
099400070411     C                   close     tivin00r
099500070411     C                   endif
099600070411     C                   if        %open(fivabwwr)
099700070411     C                   close     fivabwwr
099800070411     C                   endif
099900070411     C                   if        %open(fivatwwr)
100000070411     C                   close     fivatwwr
100100070411     C                   endif
100200070411     C*
100300070411     C* Effettuo la chiamata al CLLE preposto
100400070411     C                   call(e)   'TITVVTC'
100500070411     C                   parm                    parccm
100600070411     C                   parm                    parmbr
100700070411     C                   parm      '2'           paropz
100800070411     C*
100900070411     C                   eval      wrkesito = '2'
101000070411     C*
101100070411     C                   seton                                        LR
101200070411     C*
101300070411     C                   ENDSR     '*CANCL'
101400070411     C***
101500080411
101600080411
101700080411
101800080411      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
101900080411     C     exeBon        BEGSR
102000080411     C*
102100080411     C* Imposto i parametri x il *pgm d "bonifica"
102200080411     C                   EVAL      ElencoChar = *blanks
102300080411     C                   EVAL      TipoElenco = *blanks
102400080411     C                   EVAL      CharSost   = *blanks
102500080411     C                   EVAL      UpperCase  = '1'
102600080411     C                   EVAL      ChkNull    = '1'
102700080411     C                   EVAL      CharNull   = *blanks
102800080411     C*
102900080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
103000080411     C                   EVAL      TxtInOut   = VABRSD
103100080411     C                   EVAL      EsitoBon   = *blanks
103200080411     C*
103300080411     C                   CALL      'XCHKCHAR'
103400080411     C                   PARM                    TxtInOut
103500080411     C                   PARM                    ElencoChar
103600080411     C                   PARM                    TipoElenco
103700080411     C                   PARM                    CharSost
103800080411     C                   PARM                    UpperCase
103900080411     C                   PARM                    ChkNull
104000080411     C                   PARM                    CharNull
104100080411     C                   PARM                    EsitoBon
104200080411     C*
104300080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104400080411     C                   IF        EsitoBon = '1'
104500080411     C                   EVAL      VABRSD = TxtInOut
104600080411     C                   ENDIF
104700080411     C*
104800080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
104900080411     C                   EVAL      TxtInOut   = VABIND
105000080411     C                   EVAL      EsitoBon   = *blanks
105100080411     C*
105200080411     C                   CALL      'XCHKCHAR'
105300080411     C                   PARM                    TxtInOut
105400080411     C                   PARM                    ElencoChar
105500080411     C                   PARM                    TipoElenco
105600080411     C                   PARM                    CharSost
105700080411     C                   PARM                    UpperCase
105800080411     C                   PARM                    ChkNull
105900080411     C                   PARM                    CharNull
106000080411     C                   PARM                    EsitoBon
106100080411     C*
106200080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106300080411     C                   IF        EsitoBon = '1'
106400080411     C                   EVAL      VABIND = TxtInOut
106500080411     C                   ENDIF
106600080411     C*
106700080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
106800080411     C                   EVAL      TxtInOut   = VABLOD
106900080411     C                   EVAL      EsitoBon   = *blanks
107000080411     C*
107100080411     C                   CALL      'XCHKCHAR'
107200080411     C                   PARM                    TxtInOut
107300080411     C                   PARM                    ElencoChar
107400080411     C                   PARM                    TipoElenco
107500080411     C                   PARM                    CharSost
107600080411     C                   PARM                    UpperCase
107700080411     C                   PARM                    ChkNull
107800080411     C                   PARM                    CharNull
107900080411     C                   PARM                    EsitoBon
108000080411     C*
108100080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108200080411     C                   IF        EsitoBon = '1'
108300080411     C                   EVAL      VABLOD = TxtInOut
108400080411     C                   ENDIF
108500080411     C*
108600080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
108700080411     C                   EVAL      TxtInOut   = VABNOT
108800080411     C                   EVAL      EsitoBon   = *blanks
108900080411     C                   EVAL      UpperCase  = *blanks
109000080411     C*
109100080411     C                   CALL      'XCHKCHAR'
109200080411     C                   PARM                    TxtInOut
109300080411     C                   PARM                    ElencoChar
109400080411     C                   PARM                    TipoElenco
109500080411     C                   PARM                    CharSost
109600080411     C                   PARM                    UpperCase
109700080411     C                   PARM                    ChkNull
109800080411     C                   PARM                    CharNull
109900080411     C                   PARM                    EsitoBon
110000080411     C*
110100080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110200080411     C                   IF        EsitoBon = '1'
110300080411     C                   EVAL      VABNOT = TxtInOut
110400080411     C                   ENDIF
110500080411     C*
110600080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
110700080411     C                   EVAL      TxtInOut   = VABNT2
110800080411     C                   EVAL      EsitoBon   = *blanks
110900080411     C                   EVAL      UpperCase  = *blanks
111000080411     C*
111100080411     C                   CALL      'XCHKCHAR'
111200080411     C                   PARM                    TxtInOut
111300080411     C                   PARM                    ElencoChar
111400080411     C                   PARM                    TipoElenco
111500080411     C                   PARM                    CharSost
111600080411     C                   PARM                    UpperCase
111700080411     C                   PARM                    ChkNull
111800080411     C                   PARM                    CharNull
111900080411     C                   PARM                    EsitoBon
112000080411     C*
112100080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112200080411     C                   IF        EsitoBon = '1'
112300080411     C                   EVAL      VABNT2 = TxtInOut
112400080411     C                   ENDIF
112500080411     C*
112600080411     C                   ENDSR
112700080411
112800080411
112900080411     C*----------------------------------------------------*
113000080411     C*  CONTROLLO NUMERICITA' CAMPI
113100080411     C*----------------------------------------------------*
113200080411     C     CHKNUM        BEGSR
113300080411     C*
113400080411     C                   IF        PiDecChr = *blanks
113500080411     C                   EVAL      PiDecChr = ','
113600080411     C                   ENDIF
113700080411     C*
113800080411     C                   CALL(e)   'ISNUMERIC'
113900080411     C                   PARM                    PiStr            30
114000080411     C                   PARM                    PiDecChr          1
114100080411     C                   PARM      *ZEROS        PiVal            30 9
114200080411     C                   PARM      '0'           PiInt             1
114300080411     C                   PARM      '0'           PiNum             1
114400080411     C                   IF        %error
114500080411     C                   EVAL      PiNum=*off
114600080411     C                   ENDIF
114700080411     C*
114800080411     C                   ENDSR
114900080411     C***
115000080411
115100080411
115200010530
115300990910
115400000613     C     *inzsr        BEGSR
115500990910     C*
115600990910     C     *entry        plist
115700990920     C                   parm                    tivlrds
115800990921     C                   parm      wrkesito      esito
115900000724     C                   parm                    prmlit
116000000710     C                   parm                    prmfir
116100010606     C*
116200010606     C* CALCOLA LA DATA CORRENTE
116300010606     C                   time                    wn14             14 0
116400010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
116500010606     C                   z-add     wn8           g08dat
116600010606     C                   z-add     *zeros        g08inv
116700010606     C                   movel     '0'           g08err
116800010606     C                   call      'XSRDA8'
116900010606     C                   parm                    wlbda8
117000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
117100000613     C*
117200000613     C                   ENDSR
117300000613     C***
