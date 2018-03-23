000100100518      /TITLE Upload via Internet: traduzione in FIVABWWR.
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
003600120516     D fnvabaar      e ds                  inz
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
006700120516
006800120516     D* DS RIDEFINIZIONE STRUTTURA DATI CLIENTE
006900120516     D*
007000120516     D fnvabds         ds
007100120516     D   §vabaar                           like(fnvabaar)
007200120516     D   §vatnot_a                   35
007300120517     D   §vatnot_b                   16
007400120516     D   §filler01                    4
007500120516     D   §vatnot_i                   35
007600120516     D   §vatnot_j                   35
007700120516
007800050209
007900050209
008000050209     C* /MAIN
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010530     C*
008500010530     C                   exsr      opeini
008600010530     C                   exsr      rwvab
008700050209     C*
008800050209     C* Effettuo la chiamata al CLLE preposto
008900050209     C                   call(e)   'TITVVTC'
009000050209     C                   parm                    parccm
009100050209     C                   parm                    parmbr
009200050209     C                   parm      '2'           paropz
009300080411     C*
009400080411     C* Esegue lancio TISI95R solo x chiusura
009500080411     C                   CLEAR                   TISI95DS
009600080411     C                   EVAL      I95TLA = 'C'
009700080411     C                   CALL      'TISI95R'
009800080411     C                   PARM                    TISI95DS
009900010530     C*
010000010530     C                   seton                                        lr
010100010530
010200010530
010300010530
010400010530
010500010530     C*--------------------------------------------------------
010600010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010530     C*--------------------------------------------------------
010800010530     C     PREELA        BEGSR
010900000724     C*
011000000724     C* SE OCCORRE SPEDIRE IN FILIALE
011100010530     C                   if        invfil <> *zeros and
011200010530     C                             flgGiro = '0'
011300010530     C*
011400010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011500010530     C                   eval      flgGiro = '1'
011600010530     C*
011700010530     C                   endif
011800010530     C*
011900010530     C                   ENDSR
012000010530     C***
012100010530
012200010530
012300010530
012400010530
012500010530
012600010530
012700010530     C*--------------------------------------------------------
012800010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010530     C*--------------------------------------------------------
013000010530     C     ENDELA        BEGSR
013100000613     C*
013200010530     C*
013300010530     C                   ENDSR
013400010530     C***
013500010530
013600910830     C*--------------------------------------------------------
013700020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
013800910830     C*--------------------------------------------------------
013900941229     C     RWVAB         BEGSR
014000990910     C*
014100990914     C                   if        not %open(tivin00r)
014200990908     C                   open      tivin00r
014300990914     C                   endif
014400020913     C                   if        not %open(fivabwwr)
014500020913     C                   open      fivabwwr
014600990914     C                   endif
014700050209     C*
014800050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014900050209     C                   exsr      prevat
015000050209     C*
015100050209     C                   if        chkcall = '0'
015200050209     C*
015300050209     C                   if        not %open(fivatwwr)
015400050209     C                   open      fivatwwr
015500050209     C                   endif
015600050209     C*
015700990910     C                   clear                   §CTROK
015800990910     C                   clear                   §CTRMO
015900990910     C                   clear                   §CTRNO
016000990910     C*
016100921023     C                   DO        *HIVAL
016200990913     C*
016300990915     C                   READ      tivin00r                               70
016400010618     C*
016500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016600010618     C                   if        vinflg = '1'
016700010618     C                   eval      flgOk = '1'
016800010618     C                   endif
016900010705     C*
017000010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
017100010705     C                   if        %subst(vindta:1:1) = '='
017200010705     C                   eval      %subst(vindta:1:1) = *blanks
017300010705     C                   endif
017400010705     C*
017500000905     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700010601     C* ...elimino eventuali segni
017800010601     C     '+':' '       xlate     vindta        vindta
017900990913     C*
018000010601     C                   if        *in70 = *off and
018100990913     C                             (vinflg = *blanks
018200010711     C                              or vinflg = '0'
018300010711     C                              or vinflg = '2')
018400990913     C*
018500020913     C                   clear                   FIVAB000
018600050209     C                   clear                   FIVAT000
018700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018800010711     C                   if        vinflg = *blanks or vinflg = '0'
018900010711     C                   clear                   vinmsg
019000010711     C                   endif
019100010711     C*
019200120516     C                   eval      fnvabds  = vindta
019300120516     C                   eval      fnvabaar = §vabaar
019400080411     C*
019500080411     C                   testb     x'3F'         vhbatb                   02
019600080411     C                   if        not *in02
019700080411     C*
019800080411     C                   SETOFF                                       303132
019900080411     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
020000080411     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
020100080411     C*    OPPURE TUTTI BLANKS
020200080411     C*
020300080411     C                   TESTN                   VHBLNP               30
020400080411     C  N30              TESTN                   VHBLNP                 30
020500080411     C  N30              TESTN                   VHBLNP                   30
020600080411     C                   MOVE      VHBLNP        W001A             1
020700080411     C     *IN30         IFEQ      *OFF
020800080411     C     W001A         ORLT      '0'
020900080411     C                   SETON                                        31
021000080411     C                   eval      vinmsg = %trimr(vinmsg)
021100080411     C                             + ' ' + 'VHBLNP'
021200080411     C                   ELSE
021300080411     C                   MOVE      VHBLNP        VABLNP
021400080411     C     VABLNP        IFLE      *ZEROS
021500080411     C                   SETON                                        31
021600080411     C                   eval      vinmsg = %trimr(vinmsg)
021700080411     C                             + ' ' + 'VHBLNP'
021800080411     C                   ENDIF
021900080411     C                   ENDIF
022000080411     C*
022100080411     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200080411     C                   eval      depfil = VABLNP
022300080411     C                   exsr      repfil
022400080411     C                   if        depfil = invfil
022500080411     C                   if        vlrpoi = 999
022600080411     C                   MOVE(P)   invfil        VABFGS
022700080411     C                   else
022800080411     C                   MOVE(P)   vlrpoi        VABFGS
022900080411     C                   endif
023000080411     C*
023100080411     C                   exsr      PREELA
023200080411     C*
023300080411     C                   TESTN                   VHBCCM               30
023400080411     C  N30              TESTN                   VHBCCM                 30
023500080411     C  N30              TESTN                   VHBCCM                   30
023600080411     C                   MOVE      VHBCCM        W001A             1
023700080411     C     *IN30         IFEQ      *OFF
023800080411     C     W001A         ORLT      '0'
023900080411     C                   SETON                                        31
024000080411     C                   eval      vinmsg = %trimr(vinmsg)
024100080411     C                             + ' ' + 'VHBCCM'
024200080411     C                   ELSE
024300080411     C                   MOVE      VHBCCM        VABCCM
024400080411     C     VABCCM        IFLE      *ZEROS
024500080411     C                   SETON                                        31
024600080411     C                   eval      vinmsg = %trimr(vinmsg)
024700080411     C                             + ' ' + 'VHBCCM'
024800080411     C                   ENDIF
024900080411     C                   ENDIF
025000080411     C*
025100080411     C                   TESTN                   VHBAAS               30
025200080411     C  N30              TESTN                   VHBAAS                 30
025300080411     C  N30              TESTN                   VHBAAS                   30
025400080411     C                   MOVE      VHBAAS        W001A             1
025500080411     C     *IN30         IFEQ      *OFF
025600080411     C     W001A         ORLT      '0'
025700080411     C     W001A         ANDNE     *BLANK
025800080411     C                   SETON                                        32
025900080411     C                   MOVEL     datcor        VABAAS
026000080411     C                   eval      vinmsg = %trimr(vinmsg)
026100080411     C                             + ' ' + 'VHBAAS'
026200080411     C                   ELSE
026300080411     C                   MOVE      VHBAAS        VABAAS
026400080411     C                   ENDIF
026500080411     C*
026600080411     C                   TESTN                   VHBMGS               30
026700080411     C  N30              TESTN                   VHBMGS                 30
026800080411     C  N30              TESTN                   VHBMGS                   30
026900080411     C                   MOVE      VHBMGS        W001A             1
027000080411     C     *IN30         IFEQ      *OFF
027100080411     C     W001A         ORLT      '0'
027200080411     C     W001A         ANDNE     *BLANK
027300080411     C                   SETON                                        32
027400080411     C                   MOVE      '0101'        VABMGS
027500080411     C                   eval      vinmsg = %trimr(vinmsg)
027600080411     C                             + ' ' + 'VHBMGS'
027700080411     C                   ELSE
027800080411     C                   MOVE      VHBMGS        VABMGS
027900080411     C                   ENDIF
028000080411     C*
028100080411     C                   TESTN                   VHBNRS               30
028200080411     C  N30              TESTN                   VHBNRS                 30
028300080411     C  N30              TESTN                   VHBNRS                   30
028400080411     C                   MOVE      VHBNRS        W001A             1
028500080411     C     *IN30         IFEQ      *OFF
028600080411     C     W001A         ORLT      '0'
028700080411     C     W001A         ANDNE     *BLANK
028800080411     C                   SETON                                        32
028900080411     C                   MOVE      *ZERO         VABNRS
029000080411     C                   eval      vinmsg = %trimr(vinmsg)
029100080411     C                             + ' ' + 'VHBNRS'
029200080411     C                   ELSE
029300080411     C                   MOVE      VHBNRS        VABNRS
029400080411     C                   ENDIF
029500080411     C*
029600080411     C                   TESTN                   VHBNSP               30
029700080411     C  N30              TESTN                   VHBNSP                 30
029800080411     C  N30              TESTN                   VHBNSP                   30
029900080411     C                   MOVE      VHBNSP        W001A             1
030000080411     C     *IN30         IFEQ      *OFF
030100080411     C     W001A         ORLT      '0'
030200080411     C     W001A         ANDNE     *BLANK
030300080411     C                   SETON                                        32
030400080411     C                   MOVE      *ZERO         VABNSP
030500080411     C                   eval      vinmsg = %trimr(vinmsg)
030600080411     C                             + ' ' + 'VHBNSP'
030700080411     C                   ELSE
030800080411     C                   MOVE      VHBNSP        VABNSP
030900080411     C                   ENDIF
031000080411     C*
031100080411     C                   TESTN                   VHBLNA               30
031200080411     C  N30              TESTN                   VHBLNA                 30
031300080411     C  N30              TESTN                   VHBLNA                   30
031400080411     C                   MOVE      VHBLNA        W001A             1
031500080411     C     *IN30         IFEQ      *OFF
031600080411     C     W001A         ORLT      '0'
031700080411     C     W001A         ANDNE     *BLANK
031800080411     C                   SETON                                        32
031900080411     C                   MOVE      *ZERO         VABLNA
032000080411     C                   eval      vinmsg = %trimr(vinmsg)
032100080411     C                             + ' ' + 'VHBLNA'
032200080411     C                   ELSE
032300080411     C                   MOVE      VHBLNA        VABLNA
032400080411     C                   ENDIF
032500080411     C*
032600080411     C                   TESTN                   VHBCTR               30
032700080411     C  N30              TESTN                   VHBCTR                 30
032800080411     C  N30              TESTN                   VHBCTR                   30
032900080411     C                   MOVE      VHBCTR        W001A             1
033000080411     C     *IN30         IFEQ      *OFF
033100080411     C     W001A         ORLT      '0'
033200080411     C     W001A         ANDNE     *BLANK
033300080411     C                   SETON                                        32
033400080411     C                   MOVE      *ZERO         VABCTR
033500080411     C                   eval      vinmsg = %trimr(vinmsg)
033600080411     C                             + ' ' + 'VHBCTR'
033700080411     C                   ELSE
033800080411     C                   MOVE      VHBCTR        VABCTR
033900080411     C                   ENDIF
034000080411     C*
034100080411     C                   MOVE      *ZERO         VABIAS
034200080411     C                   MOVE      VHBIAS        WASC15
034300080411     C                   TESTN                   WI15                 30
034400080411     C  N30              TESTN                   WI15                   30
034500080411     C  N30              TESTN                   WI15                     30
034600080411     C                   MOVE      WI15          W001A             1
034700080411     C     *IN30         IFEQ      *OFF
034800080411     C     W001A         ORLT      '0'
034900080411     C     W001A         ANDNE     *BLANK
035000080411     C                   SETON                                        32
035100080411     C                   MOVE      *ZERO         VABIAS
035200080411     C                   eval      vinmsg = %trimr(vinmsg)
035300080411     C                             + ' ' + 'VHBIAS'
035400080411     C                   ELSE
035500080411     C                   MOVEL     WI15          VABIAS
035600080411     C                   TESTN                   WD15                 30
035700080411     C  N30              TESTN                   WD15                   30
035800080411     C  N30              TESTN                   WD15                     30
035900080411     C                   MOVE      WD15          W001A             1
036000080411     C     *IN30         IFEQ      *OFF
036100080411     C     W001A         ORLT      '0'
036200080411     C     W001A         ANDNE     *BLANK
036300080411     C                   SETON                                        32
036400080411     C                   MOVE      *ZERO         VABIAS
036500080411     C                   eval      vinmsg = %trimr(vinmsg)
036600080411     C                             + ' ' + 'VHBIAS'
036700080411     C                   ELSE
036800080411     C                   MOVE      WD15          VABIAS
036900080411     C                   ENDIF
037000080411     C                   ENDIF
037100080411     C*
037200080411     C                   TESTN                   VHBNCL               30
037300080411     C  N30              TESTN                   VHBNCL                 30
037400080411     C  N30              TESTN                   VHBNCL                   30
037500080411     C                   MOVE      VHBNCL        W001A             1
037600080411     C     *IN30         IFEQ      *OFF
037700080411     C     W001A         ORLT      '0'
037800080411     C                   SETON                                        32
037900080411     C                   MOVE      *ZERO         VABNCL
038000080411     C                   eval      vinmsg = %trimr(vinmsg)
038100080411     C                             + ' ' + 'VHBNCL'
038200080411     C                   ELSE
038300080411     C                   MOVE      VHBNCL        VABNCL
038400080411     C                   ENDIF
038500080411     C*
038600080411     C                   MOVE      *ZERO         VABPKB
038700080411     C                   MOVE      VHBPKB        WASC09
038800080411     C                   IF        WI09 = *blanks
038900080411     C                   EVAL      WI09 = *zeros
039000080411     C                   ENDIF
039100080411     C                   TESTN                   WI09                 30
039200080411     C  N30              TESTN                   WI09                   30
039300080411     C  N30              TESTN                   WI09                     30
039400080411     C                   MOVE      WI09          W001A             1
039500080411     C     *IN30         IFEQ      *OFF
039600080411     C     W001A         ORLT      '0'
039700080411     C                   SETON                                        32
039800080411     C                   MOVE      *ZERO         VABPKB
039900080411     C                   eval      vinmsg = %trimr(vinmsg)
040000080411     C                             + ' ' + 'VHBPKB'
040100080411     C                   ELSE
040200080411     C                   MOVEL     WI09          VABPKB
040300080411     C                   IF        WD09 = *blanks
040400080411     C                   EVAL      WD09 = *zeros
040500080411     C                   ENDIF
040600080411     C                   TESTN                   WD09                 30
040700080411     C  N30              TESTN                   WD09                     30
040800080411     C                   MOVE      WD09          W001A             1
040900080411     C     *IN30         IFEQ      *OFF
041000080411     C     W001A         ORLT      '0'
041100080411     C     W001A         ANDNE     *BLANK
041200080411     C                   SETON                                        32
041300080411     C                   MOVE      *ZERO         VABPKB
041400080411     C                   eval      vinmsg = %trimr(vinmsg)
041500080411     C                             + ' ' + 'VHBPKB'
041600080411     C                   ELSE
041700080411     C                   MOVE      WD09          VABPKB
041800080411     C                   ENDIF
041900080411     C                   ENDIF
042000080411     C*
042100080411     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
042200080411     C                   IF        *IN51 = *OFF
042300080411     C                   MOVE      *ZERO         VABVLB
042400080411     C                   MOVE      VHBVLB        WASC07
042500080411     C                   IF        WI07 = *blanks
042600080411     C                   EVAL      WI07 = *zeros
042700080411     C                   ENDIF
042800080411     C                   TESTN                   WI07                 30
042900080411     C  N30              TESTN                   WI07                   30
043000080411     C  N30              TESTN                   WI07                     30
043100080411     C                   MOVE      WI07          W001A             1
043200080411     C     *IN30         IFEQ      *OFF
043300080411     C     W001A         ORLT      '0'
043400080411     C     W001A         ANDNE     *BLANK
043500080411     C                   SETON                                        32
043600080411     C                   MOVE      *ZERO         VABVLB
043700080411     C                   eval      vinmsg = %trimr(vinmsg)
043800080411     C                             + ' ' + 'VHBVLB'
043900080411     C                   ELSE
044000080411     C                   MOVEL     WI07          VABVLB
044100080411     C                   IF        WD07 = *blanks
044200080411     C                   EVAL      WD07 = *zeros
044300080411     C                   ENDIF
044400080411     C                   TESTN                   WD07                 30
044500080411     C  N30              TESTN                   WD07                   30
044600080411     C  N30              TESTN                   WD07                     30
044700080411     C                   MOVE      WD07          W001A             1
044800080411     C     *IN30         IFEQ      *OFF
044900080411     C     W001A         ORLT      '0'
045000080411     C     W001A         ANDNE     *BLANK
045100080411     C                   SETON                                        32
045200080411     C                   MOVE      *ZERO         VABVLB
045300080411     C                   eval      vinmsg = %trimr(vinmsg)
045400080411     C                             + ' ' + 'VHBVLB'
045500080411     C                   ELSE
045600080411     C                   MOVE      WD07          VABVLB
045700080411     C                   ENDIF
045800080411     C                   ENDIF
045900080411     C                   ELSE
046000080411     C* ...altrimenti forzo azzeramento del volume
046100080411     C                   Z-ADD     *zeros        VABVLB
046200080411     C                   ENDIF
046300080411     C*
046400080411     C                   MOVE      *ZERO         VABQFT
046500080411     C                   MOVE      VHBQFT        WASC15
046600080411     C                   IF        WI15 = *blanks
046700080411     C                   EVAL      WI15 = *zeros
046800080411     C                   ENDIF
046900080411     C                   TESTN                   WI15                 30
047000080411     C  N30              TESTN                   WI15                   30
047100080411     C  N30              TESTN                   WI15                     30
047200080411     C                   MOVE      WI15          W001A             1
047300080411     C     *IN30         IFEQ      *OFF
047400080411     C     W001A         ORLT      '0'
047500080411     C     W001A         ANDNE     *BLANK
047600080411     C                   SETON                                        32
047700080411     C                   MOVE      *ZERO         VABQFT
047800080411     C                   eval      vinmsg = %trimr(vinmsg)
047900080411     C                             + ' ' + 'VHBQFT'
048000080411     C                   ELSE
048100080411     C                   MOVEL     WI15          VABQFT
048200080411     C                   IF        WD15 = *blanks
048300080411     C                   EVAL      WD15 = *zeros
048400080411     C                   ENDIF
048500080411     C                   TESTN                   WD15                 30
048600080411     C  N30              TESTN                   WD15                   30
048700080411     C  N30              TESTN                   WD15                     30
048800080411     C                   MOVE      WD15          W001A             1
048900080411     C     *IN30         IFEQ      *OFF
049000080411     C     W001A         ORLT      '0'
049100080411     C     W001A         ANDNE     *BLANK
049200080411     C                   SETON                                        32
049300080411     C                   MOVE      *ZERO         VABQFT
049400080411     C                   eval      vinmsg = %trimr(vinmsg)
049500080411     C                             + ' ' + 'VHBQFT'
049600080411     C                   ELSE
049700080411     C                   MOVE      WD15          VABQFT
049800080411     C                   ENDIF
049900080411     C                   ENDIF
050000080411     C*
050100100518     C                   MOVEL(P)  VHBCAS        PiStr
050200080411     C                   EXSR      CHKNUM
050300080411     C                   IF        PiNum=*on
050400080411     C                   Z-ADD     PiVal         VABCAS
050500080411     C                   ELSE
050600080411     C                   SETON                                        32
050700080411     C                   Z-ADD     *zeros        VABCAS
050800080411     C                   EVAL      vinmsg = %trimr(vinmsg)
050900080411     C                             + ' ' + 'VHBCAS'
051000080411     C                   ENDIF
051100080411     C*
051200080411     C                   TESTN                   VHBRMN               30
051300080411     C  N30              TESTN                   VHBRMN                 30
051400080411     C  N30              TESTN                   VHBRMN                   30
051500080411     C                   MOVE      VHBRMN        W001A             1
051600080411     C     *IN30         IFEQ      *OFF
051700080411     C     W001A         ORLT      '0'
051800080411     C                   SETON                                        32
051900080411     C                   MOVE      *ZERO         VABRMN
052000080411     C                   eval      vinmsg = %trimr(vinmsg)
052100080411     C                             + ' ' + 'VHBRMN'
052200080411     C                   ELSE
052300080411     C                   MOVE      VHBRMN        VABRMN
052400080411     C                   ENDIF
052500080411     C*
052600080411     C                   TESTN                   VHBNCD               30
052700080411     C  N30              TESTN                   VHBNCD                 30
052800080411     C  N30              TESTN                   VHBNCD                   30
052900080411     C                   MOVE      VHBNCD        W001A             1
053000080411     C     *IN30         IFEQ      *OFF
053100080411     C     W001A         ORLT      '0'
053200080411     C     W001A         ANDNE     *BLANK
053300080411     C                   SETON                                        32
053400080411     C                   MOVE      *ZERO         VABNCD
053500080411     C                   eval      vinmsg = %trimr(vinmsg)
053600080411     C                             + ' ' + 'VHBNCD'
053700080411     C                   ELSE
053800080411     C                   MOVE      VHBNCD        VABNCD
053900080411     C                   ENDIF
054000080411     C*
054100080411     C                   TESTN                   VHBNCA               30
054200080411     C  N30              TESTN                   VHBNCA                 30
054300080411     C  N30              TESTN                   VHBNCA                   30
054400080411     C                   MOVE      VHBNCA        W001A             1
054500080411     C     *IN30         IFEQ      *OFF
054600080411     C     W001A         ORLT      '0'
054700080411     C     W001A         ANDNE     *BLANK
054800080411     C                   SETON                                        32
054900080411     C                   MOVE      *ZERO         VABNCA
055000080411     C                   eval      vinmsg = %trimr(vinmsg)
055100080411     C                             + ' ' + 'VHBNCA'
055200080411     C                   ELSE
055300080411     C                   MOVE      VHBNCA        VABNCA
055400080411     C                   ENDIF
055500080411     C*
055600080411     C                   TESTN                   VHBZNC               30
055700080411     C  N30              TESTN                   VHBZNC                 30
055800080411     C  N30              TESTN                   VHBZNC                   30
055900080411     C                   MOVE      VHBZNC        W001A             1
056000080411     C     *IN30         IFEQ      *OFF
056100080411     C     W001A         ORLT      '0'
056200080411     C     W001A         ANDNE     *BLANK
056300080411     C                   SETON                                        32
056400080411     C                   MOVE      *ZERO         VABZNC
056500080411     C                   eval      vinmsg = %trimr(vinmsg)
056600080411     C                             + ' ' + 'VHBZNC'
056700080411     C                   ELSE
056800080411     C                   MOVE      VHBZNC        VABZNC
056900080411     C                   ENDIF
057000080411     C*
057100080411     C                   TESTN                   VHBDCR               30
057200080411     C  N30              TESTN                   VHBDCR                 30
057300080411     C  N30              TESTN                   VHBDCR                   30
057400080411     C                   MOVE      VHBDCR        W001A             1
057500080411     C     *IN30         IFEQ      *OFF
057600080411     C     W001A         ORLT      '0'
057700080411     C     W001A         ANDNE     *BLANK
057800080411     C                   SETON                                        32
057900080411     C                   MOVE      *ZERO         VABDCR
058000080411     C                   eval      vinmsg = %trimr(vinmsg)
058100080411     C                             + ' ' + 'VHBDCR'
058200080411     C                   ELSE
058300080411     C                   MOVE      VHBDCR        VABDCR
058400080411     C                   ENDIF
058500080411     C*
058600080411     C                   TESTN                   VHBHCR               30
058700080411     C  N30              TESTN                   VHBHCR                 30
058800080411     C  N30              TESTN                   VHBHCR                   30
058900080411     C                   MOVE      VHBHCR        W001A             1
059000080411     C     *IN30         IFEQ      *OFF
059100080411     C     W001A         ORLT      '0'
059200080411     C     W001A         ANDNE     *BLANK
059300080411     C                   SETON                                        32
059400080411     C                   MOVE      *ZERO         VABHCR
059500080411     C                   eval      vinmsg = %trimr(vinmsg)
059600080411     C                             + ' ' + 'VHBHCR'
059700080411     C                   ELSE
059800080411     C                   MOVE      VHBHCR        VABHCR
059900080411     C                   ENDIF
060000080411     C*
060100080411     C                   MOVE      *ZERO         VABVMD
060200080411     C                   MOVE      VHBVMD        WASC15
060300080411     C                   IF        WI15 = *blanks
060400080411     C                   EVAL      WI15 = *zeros
060500080411     C                   ENDIF
060600080411     C                   TESTN                   WI15                 30
060700080411     C  N30              TESTN                   WI15                   30
060800080411     C  N30              TESTN                   WI15                     30
060900080411     C                   MOVE      WI15          W001A             1
061000080411     C     *IN30         IFEQ      *OFF
061100080411     C     W001A         ORLT      '0'
061200080411     C     W001A         ANDNE     *BLANK
061300080411     C                   SETON                                        32
061400080411     C                   MOVE      *ZERO         VABVMD
061500080411     C                   eval      vinmsg = %trimr(vinmsg)
061600080411     C                             + ' ' + 'VHBVMD'
061700080411     C                   ELSE
061800080411     C                   MOVEL     WI15          VABVMD
061900080411     C                   IF        WD15 = *blanks
062000080411     C                   EVAL      WD15 = *zeros
062100080411     C                   ENDIF
062200080411     C                   TESTN                   WD15                 30
062300080411     C  N30              TESTN                   WD15                   30
062400080411     C  N30              TESTN                   WD15                     30
062500080411     C                   MOVE      WD15          W001A             1
062600080411     C     *IN30         IFEQ      *OFF
062700080411     C     W001A         ORLT      '0'
062800080411     C     W001A         ANDNE     *BLANK
062900080411     C                   SETON                                        32
063000080411     C                   MOVE      *ZERO         VABVMD
063100080411     C                   eval      vinmsg = %trimr(vinmsg)
063200080411     C                             + ' ' + 'VHBVMD'
063300080411     C                   ELSE
063400080411     C                   MOVE      WD15          VABVMD
063500080411     C                   ENDIF
063600080411     C                   ENDIF
063700080411     C*
063800080411     C                   TESTN                   VHBANT               30
063900080411     C  N30              TESTN                   VHBANT                 30
064000080411     C  N30              TESTN                   VHBANT                   30
064100080411     C                   MOVE      VHBANT        W001A             1
064200080411     C     *IN30         IFEQ      *OFF
064300080411     C     W001A         ORLT      '0'
064400080411     C     W001A         ANDNE     *BLANK
064500080411     C                   SETON                                        32
064600080411     C                   MOVE      *ZERO         VABANT
064700080411     C                   eval      vinmsg = %trimr(vinmsg)
064800080411     C                             + ' ' + 'VHBANT'
064900080411     C                   ELSE
065000080411     C                   MOVE      VHBANT        VABANT
065100080411     C                   ENDIF
065200080411     C*
065300080411     C* CAMPI ALFABETICI:
065400080411     C                   MOVE      VHBATB        VABATB
065500080411     C                   MOVE      VHBCBO        VABCBO
065600080411     C                   MOVE      VHBRSD        VABRSD
065700080411     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
065800080411     C     '@':'A'       XLATE     VABRSD        VABRSD
065900080411     C* ==
066000080411     C                   MOVE      VHBRD2        VABRD2
066100080411     C                   MOVE      VHBIND        VABIND
066200080411     C                   MOVE      VHBCAD        VABCAD
066300080411     C                   MOVE      VHBLOD        VABLOD
066400080411     C                   MOVE      VHBPRD        VABPRD
066500080411     C                   MOVE      VHBNZD        VABNZD
066600080411     C                   MOVE      VHBGC1        VABGC1
066700080411     C                   MOVE      VHBGC2        VABGC2
066800080411     C                   MOVE      VHBTSP        VABTSP
066900080411     C                   MOVE      VHBVAS        VABVAS
067000080411     C                   MOVE      VHBNAS        VABNAS
067100080411     C                   MOVE      VHBTIC        VABTIC
067200080411     C                   MOVE      VHBVCA        VABVCA
067300080411     C                   MOVE      VHBGCA        VABGCA
067400080411     C                   MOVE      VHBRMA        VABRMA
067500080411     C                   MOVE      VHBXCO        VABXCO
067600080411     C                   MOVE      VHBNOT        VABNOT
067700080411     C                   MOVE      VHBNT2        VABNT2
067800080411     C                   MOVE      VHBCTM        VABCTM
067900080411     C                   MOVE      VHBFFD        VABFFD
068000080411     C                   MOVE      VHBTCR        VABTCR
068100080411     C                   MOVE      VHBCTS        VABCTS
068200080411     C                   MOVE      VHBFTM        VABFTM
068300080411     C                   MOVE      VHBVAD        VABVAD
068400080411     C                   MOVE      VHBGMA        VABGMA
068500080411     C                   MOVE      VHBGGA        VABGGA
068600080411     C                   MOVE      VHBGVA        VABGVA
068700080411     C                   MOVE      VHBTC1        VABTC1
068800080411     C                   MOVE      VHBTC2        VABTC2
068900080411     C                   MOVE      VHBSCL        VABSCL
069000080411     C                   MOVE      VHBRMO        VABRMO
069100080411     C                   MOVE      VHBCMO        VABCMO
069200080411     C                   MOVE      VHBNMO        VABNMO
069300080411     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
069400080411     C* NELLA NAZIONE MITTENTE ORIGINALE
069500080411     C                   IF        %trim(VHBNMO) < 'A'
069600080411     C                   MOVE      *BLANKS       VABNMO
069700080411     C                   ENDIF
069800080411     C*
069900080411     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
070000080411     C                   IF        %trim(VABNZD) = 'I'   OR
070100080411     C                             %trim(VABNZD) = 'IT'  OR
070200080411     C                             %trim(VABNZD) = 'ITA'
070300080411     C                   EVAL      VABNZD = *blanks
070400080411     C                   ENDIF
070500080411     C*
070600080411     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
070700080411     C                   IF        *IN52 = *ON
070800080411     C* Reperisco la provincia dal CAP e dalla località
070900080411     C                   IF        VABCAD <> *blanks AND
071000080411     C                             VABPRD  = *blanks AND
071100080411     C                             VABNZD  = *blanks
071200080411     C                   CLEAR                   TISI95DS
071300080411     C                   EVAL      I95TCN = '3'
071400080411     C                   Z-ADD     datcor        I95DAT
071500080411     C                   EVAL      I95CAP = VABCAD
071600080411     C                   EVAL      I95LOC = VABLOD
071700080411     C                   CALL      'TISI95R'
071800080411     C                   PARM                    TISI95DS
071900080411     C                   EVAL      VABPRD = O95PRV
072000080411     C                   ENDIF
072100080411     C                   ENDIF
072200080411     C*
072300080411     C* "bonifico" i dati d input da eventuali caratteri indesiderati
072400080411     C                   exsr      exeBon
072500930409     C*
072600010604     C  N31              ADD       1             §CTROK            7 0
072700010604     C   32              ADD       1             §CTRMO            7 0
072800010604     C   31              ADD       1             §CTRNO            7 0
072900080411     C  N31              WRITE     FIVAB000
073000120516     C  N31              EXSR      WRIVATS
073100990910     C*
073200010601     C                   if        *in31 = *off and
073300010601     C                             *in32 = *off
073400990910     C                   eval      vinflg = '1'
073500990910     C                   else
073600990910     C                   eval      vinflg = '2'
073700990910     C                   endif
073800991022     C                   endif
073900990910     C                   endif
074000010601     C*
074100010601     C                   endif
074200000905     C*
074300000905     C                   else
074400000905     C                   eval      vinflg = '1'
074500000905     C                   endif
074600000905     C*
074700000905     C  N70              update    tivin000
074800991022     C*
074900991022     C  N70              ENDdo
075000010530     C
075100010530     C*
075200010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075300010530     C                   if        cntNonEl = *zeros or
075400010530     C                             flgMulti = '0'
075500990910     C* Se non ci sono record con errori ...
075600010530     C                   if        §ctrno = 0 and
075700010601     C                             §ctrmo = 0 and
075800010530     C                             flgStato <> '2'
075900990910     C* ... restituisco esito OK.
076000990921     C                   eval      wrkesito = '0'
076100990910     C                   else
076200010601     C                   if        §ctrok > 0
076300990921     C                   eval      wrkesito = '1'
076400000710     C                   else
076500010615     C                   if        flgOk = '0'
076600010615     C                   eval      wrkesito = '2'
076700010615     C                   else
076800010615     C                   eval      wrkesito = '6'
076900010615     C                   endif
077000990910     C                   endif
077100000710     C                   endif
077200010530     C                   else
077300010530     C                   eval      wrkesito = '9'
077400010530     C                   endif
077500050209     C*
077600050209     C                   else
077700050209     C                   eval      wrkesito = '2'
077800050209     C                   endif
077900990910     C*
078000990914     C                   if        %open(tivin00r)
078100990908     C                   close     tivin00r
078200990914     C                   endif
078300020913     C                   if        %open(fivabwwr)
078400020913     C                   close     fivabwwr
078500990914     C                   endif
078600050209     C                   if        %open(fivatwwr)
078700050209     C                   close     fivatwwr
078800050209     C                   endif
078900010530     C*
079000010530     C                   if        vlrpoi <> 999
079100010530     C                   eval      invfil = vlrpoi
079200010530     C                   endif
079300990910     C*
079400990920     C                   if        §ctrok > 0
079500010530     C                             and invfil > *zeros
079600000613     C                   exsr      invio
079700990920     C                   endif
079800010612     C*
079900010612     C                   if        flgGiro = '1'
080000010612     C                   exsr      endela
080100010612     C                   endif
080200990920     C*
080300910830     C                   ENDSR
080400000613     C***
080500050209
080600050209
080700050209
080800050209     C*----------------------------------------------------*
080900050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
081000050209     C*----------------------------------------------------*
081100050209     C     PREVAT        BEGSR
081200050209     C*
081300050209     C* Compongo il nome del membro da dare al FIVATWWR
081400050209     C                   eval      parmbr = vlrhdl
081500050209     C                   movel     'M'           parmbr
081600050209     C                   eval      parccm = %subst(vlrKSC:2:7)
081700050209     C                   eval      paropz = '1'
081800050209     C* Effettuo la chiamata al CLLE preposto
081900050209     C                   call(e)   'TITVVTC'
082000050209     C                   parm                    parccm
082100050209     C                   parm                    parmbr
082200050209     C                   parm                    paropz
082300050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
082400050209     C                   if        %error
082500050209     C                   movel     '1'           chkcall
082600050209     C                   else
082700050209     C                   movel     '0'           chkcall
082800050209     C                   endif
082900050209     C*
083000050209     C                   ENDSR
083100050209
083200050209
083300990920
083400050209     C*----------------------------------------------------*
083500050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
083600050209     C*----------------------------------------------------*
083700120516     C     WRIVATS       BEGSR
083800050209     C*
083900050209     C* Valorizzo il buffer del file dettagli barcode colli
084000050209     C                   EVAL      VATFGS = VABFGS
084100050209     C                   EVAL      VATAAS = VABAAS
084200050209     C                   EVAL      VATLNP = VABLNP
084300050209     C                   EVAL      VATNSP = VABNSP
084400050209     C                   EVAL      VATCCM = VABCCM
084500120516     C*
084600120516     C* Gestione tipo record 'A'
084700120516     C                   EVAL      VATTRC='A'
084800120516     C                   EVAL      VATNOT=§vatnot_a
084900120516     C                   EXSR      WRIVAT
085000120516     C*
085100120516     C* Gestione tipo record 'B'
085200120516     C                   EVAL      VATTRC='B'
085300120516     C                   EVAL      VATNOT=§vatnot_b
085400120516     C                   EXSR      WRIVAT
085500120516     C*
085600120516     C* Gestione tipo record 'I'
085700120516     C                   EVAL      VATTRC='I'
085800120516     C                   EVAL      VATNOT=§vatnot_i
085900120516     C                   EXSR      WRIVAT
086000120516     C*
086100120516     C* Gestione tipo record 'J'
086200120516     C                   EVAL      VATTRC='J'
086300120516     C                   EVAL      VATNOT=§vatnot_j
086400120516     C                   EXSR      WRIVAT
086500050209     C*
086600050209     C                   ENDSR
086700100518     C*----------------------------------------------------*
086800100518     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
086900100518     C*----------------------------------------------------*
087000100518     C     WRIVAT        BEGSR
087100100518     C*
087200100518     C* Scrivo solo se valorizzato qualcosa
087300100518     C                   IF        VATNOT <> *blanks
087400100518     C                   WRITE     FIVAT000
087500100518     C                   ENDIF
087600100518     C*
087700100518     C                   ENDSR
087800010530
087900010530
088000050209
088100010530
088200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
088300010530     C     repfil        BEGSR
088400010530     C*
088500010530     C                   if        invfil = *zeros and
088600010530     C                             depfil > *zeros and
088700010613     C                             (vinflg = *blanks or
088800010613     C                              vinflg = *zeros)
088900010530     C
089000010530     C                   eval      invfil = depfil
089100010530     C                   endif
089200010530     C*
089300010530     C                   if        depfil <> invfil and
089400010530     C                             invfil > *zeros
089500010530     C                   eval      flgMulti = '1'
089600010530     C                   if        vinflg = *blanks
089700010530     C                   add       1             cntNonEl
089800010530     C                   endif
089900010530     C                   endif
090000010530     C*
090100010530     C                   if        vinflg = '2'
090200010530     C                   eval      flgStato = '2'
090300010530     C                   endif
090400010530     C*
090500010530     C                   ENDSR
090600010530     C***
090700050209
090800050209
090900050209
091000050209
091100050209      /TITLE Invio dei dati al punto operativo.
091200050209     C     invio         BEGSR
091300050209     C*
091400050209     C* 1° invio FIVAT
091500050209     C                   reset                   dscmz
091600050209     C                   move      vlrpoi        cmzdst
091700050209     C                   eval      cmzfld = 'FIVATWWR'
091800050209     C                   eval      cmzmbd = vlrhdl
091900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
092000050209     C***                if        prmfir = *blanks
092100050209     C                   eval      cmzfla = 'FIVAT00F'
092200050209     C                   eval      cmzmba = 'FIVAT00F'
092300050209     C***                else
092400050209     C***                eval      cmzfla = prmfir
092500050209     C***                eval      cmzmba = prmfir
092600050209     C***                endif
092700050209     C                   eval      cmznrr = *zeros
092800050209     C                   move      §ctrok        cmznrr
092900050209     C                   eval      cmzlba = vlrfl1
093000050209     C                   call(e)   'TIS711C'
093100050209     C                   parm                    dscmz
093200050209     C                   parm      *blanks       esito
093300050209     C                   if        %error
093400050209     C                             or cmzerr = '1'
093500050209     C                             or esito  = '1'
093600050209     C                   eval      wrkesito = '3'
093700050209     C                   else
093800050209     C*
093900050209     C* 2° invio FIVAB
094000050209     C                   reset                   dscmz
094100050209     C                   move      vlrpoi        cmzdst
094200050209     C                   eval      cmzfld = vlrfou
094300050209     C                   eval      cmzmbd = vlrhdl
094400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
094500050209     C***                if        prmfir = *blanks
094600050209     C                   eval      cmzfla = 'FIVAB00F'
094700050209     C                   eval      cmzmba = 'FIVAB00F'
094800050209     C***                else
094900050209     C***                eval      cmzfla = prmfir
095000050209     C***                eval      cmzmba = prmfir
095100050209     C***                endif
095200050209     C                   eval      cmznrr = *zeros
095300050209     C                   move      §ctrok        cmznrr
095400050209     C                   eval      cmzlba = vlrfl1
095500050209     C                   call(e)   'TIS711C'
095600050209     C                   parm                    dscmz
095700050209     C                   parm      *blanks       esito
095800050209     C                   if        %error
095900050209     C                             or cmzerr = '1'
096000050209     C                             or esito  = '1'
096100050209     C                   eval      wrkesito = '3'
096200050209     C                   endif
096300050209     C                   endif
096400050209     C*
096500050209     C                   ENDSR
096600050209     C***
096700010530
096800010530
096900010530
097000010530
097100010530      /TITLE Invio dei dati al punto operativo.
097200010530     C     opeini        BEGSR
097300010530     C*
097400010530     C* Inizializzo flag e contatori operativi
097500010530     C                   movel     '0'           flgGiro           1
097600010530     C                   movel     '0'           flgMulti          1
097700010530     C                   movel     '1'           flgStato          1
097800010615     C                   movel     '0'           flgOk             1
097900010530     C                   z-add     *zeros        cntNonEl         10 0
098000010530     C                   z-add     *zeros        depfil            3 0
098100010530     C                   z-add     *zeros        invfil            3 0
098200030407     C*
098300050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
098400030407     C                   if        %subst(vlrppt:1:1) = 'S'
098500030407     C                   seton                                        50
098600030407     C                   else
098700030407     C                   setoff                                       50
098800030407     C                   endif
098900041001     C*
099000050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
099100041001     C                   if        %subst(vlrppt:2:1) = 'S'
099200041001     C                   seton                                        51
099300041001     C                   else
099400041001     C                   setoff                                       51
099500041001     C                   endif
099600080411     C*
099700080411     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
099800080411     C* (naturalmente solo se nn già presente)
099900080411     C                   if        %subst(vlrppt:3:1) = 'S'
100000080411     C                   seton                                        52
100100080411     C                   else
100200080411     C                   setoff                                       52
100300080411     C                   endif
100400010530     C*
100500010530     C                   ENDSR
100600010530     C***
100700010530
100800010530
100900070411
101000070411     C     *pssr         BEGSR
101100070411     C*
101200070411     C                   if        %open(tivin00r)
101300070411     C                   close     tivin00r
101400070411     C                   endif
101500070411     C                   if        %open(fivabwwr)
101600070411     C                   close     fivabwwr
101700070411     C                   endif
101800070411     C                   if        %open(fivatwwr)
101900070411     C                   close     fivatwwr
102000070411     C                   endif
102100070411     C*
102200070411     C* Effettuo la chiamata al CLLE preposto
102300070411     C                   call(e)   'TITVVTC'
102400070411     C                   parm                    parccm
102500070411     C                   parm                    parmbr
102600070411     C                   parm      '2'           paropz
102700070411     C*
102800070411     C                   eval      wrkesito = '2'
102900070411     C*
103000070411     C                   seton                                        LR
103100070411     C*
103200070411     C                   ENDSR     '*CANCL'
103300070411     C***
103400080411
103500080411
103600080411
103700080411      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
103800080411     C     exeBon        BEGSR
103900080411     C*
104000080411     C* Imposto i parametri x il *pgm d "bonifica"
104100080411     C                   EVAL      ElencoChar = *blanks
104200080411     C                   EVAL      TipoElenco = *blanks
104300080411     C                   EVAL      CharSost   = *blanks
104400080411     C                   EVAL      UpperCase  = '1'
104500080411     C                   EVAL      ChkNull    = '1'
104600080411     C                   EVAL      CharNull   = *blanks
104700080411     C*
104800080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
104900080411     C                   EVAL      TxtInOut   = VABRSD
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
106400080411     C                   EVAL      VABRSD = TxtInOut
106500080411     C                   ENDIF
106600080411     C*
106700080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
106800080411     C                   EVAL      TxtInOut   = VABIND
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
108300080411     C                   EVAL      VABIND = TxtInOut
108400080411     C                   ENDIF
108500080411     C*
108600080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
108700080411     C                   EVAL      TxtInOut   = VABLOD
108800080411     C                   EVAL      EsitoBon   = *blanks
108900080411     C*
109000080411     C                   CALL      'XCHKCHAR'
109100080411     C                   PARM                    TxtInOut
109200080411     C                   PARM                    ElencoChar
109300080411     C                   PARM                    TipoElenco
109400080411     C                   PARM                    CharSost
109500080411     C                   PARM                    UpperCase
109600080411     C                   PARM                    ChkNull
109700080411     C                   PARM                    CharNull
109800080411     C                   PARM                    EsitoBon
109900080411     C*
110000080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110100080411     C                   IF        EsitoBon = '1'
110200080411     C                   EVAL      VABLOD = TxtInOut
110300080411     C                   ENDIF
110400080411     C*
110500080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
110600080411     C                   EVAL      TxtInOut   = VABNOT
110700080411     C                   EVAL      EsitoBon   = *blanks
110800080411     C                   EVAL      UpperCase  = *blanks
110900080411     C*
111000080411     C                   CALL      'XCHKCHAR'
111100080411     C                   PARM                    TxtInOut
111200080411     C                   PARM                    ElencoChar
111300080411     C                   PARM                    TipoElenco
111400080411     C                   PARM                    CharSost
111500080411     C                   PARM                    UpperCase
111600080411     C                   PARM                    ChkNull
111700080411     C                   PARM                    CharNull
111800080411     C                   PARM                    EsitoBon
111900080411     C*
112000080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112100080411     C                   IF        EsitoBon = '1'
112200080411     C                   EVAL      VABNOT = TxtInOut
112300080411     C                   ENDIF
112400080411     C*
112500080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
112600080411     C                   EVAL      TxtInOut   = VABNT2
112700080411     C                   EVAL      EsitoBon   = *blanks
112800080411     C                   EVAL      UpperCase  = *blanks
112900080411     C*
113000080411     C                   CALL      'XCHKCHAR'
113100080411     C                   PARM                    TxtInOut
113200080411     C                   PARM                    ElencoChar
113300080411     C                   PARM                    TipoElenco
113400080411     C                   PARM                    CharSost
113500080411     C                   PARM                    UpperCase
113600080411     C                   PARM                    ChkNull
113700080411     C                   PARM                    CharNull
113800080411     C                   PARM                    EsitoBon
113900080411     C*
114000080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
114100080411     C                   IF        EsitoBon = '1'
114200080411     C                   EVAL      VABNT2 = TxtInOut
114300080411     C                   ENDIF
114400080411     C*
114500080411     C                   ENDSR
114600080411
114700080411
114800080411     C*----------------------------------------------------*
114900080411     C*  CONTROLLO NUMERICITA' CAMPI
115000080411     C*----------------------------------------------------*
115100080411     C     CHKNUM        BEGSR
115200080411     C*
115300080411     C                   IF        PiDecChr = *blanks
115400080411     C                   EVAL      PiDecChr = ','
115500080411     C                   ENDIF
115600080411     C*
115700080411     C                   CALL(e)   'ISNUMERIC'
115800080411     C                   PARM                    PiStr            30
115900080411     C                   PARM                    PiDecChr          1
116000080411     C                   PARM      *ZEROS        PiVal            30 9
116100080411     C                   PARM      '0'           PiInt             1
116200080411     C                   PARM      '0'           PiNum             1
116300080411     C                   IF        %error
116400080411     C                   EVAL      PiNum=*off
116500080411     C                   ENDIF
116600080411     C*
116700080411     C                   ENDSR
116800080411     C***
116900080411
117000080411
117100010530
117200990910
117300000613     C     *inzsr        BEGSR
117400990910     C*
117500990910     C     *entry        plist
117600990920     C                   parm                    tivlrds
117700990921     C                   parm      wrkesito      esito
117800000724     C                   parm                    prmlit
117900000710     C                   parm                    prmfir
118000010606     C*
118100010606     C* CALCOLA LA DATA CORRENTE
118200010606     C                   time                    wn14             14 0
118300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
118400010606     C                   z-add     wn8           g08dat
118500010606     C                   z-add     *zeros        g08inv
118600010606     C                   movel     '0'           g08err
118700010606     C                   call      'XSRDA8'
118800010606     C                   parm                    wlbda8
118900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
119000000613     C*
119100000613     C                   ENDSR
119200000613     C***
