000100100518      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121205      *
000300121205      * PARTICOLARITA':
000400121205     C* Il barcode si calcola:
000500121205     C* primi 6 digits = ultimi 6 digits del campo VABRMN
000600121205     C* digits 7 e 8   = progressivo collo
000700121205     C* digits 9 e 10  = numero colli
000800121205     C* VABCCM = 1661824
000900121205     C* VABNRS   non tradurre
001000121205     C* VABNZD   non tradurre
001100121205     C* VABCTR = 000
001200121205     C* VABTSP   non tradurre
001300121205     C* VABNCD   non tradurre
001400121205     C* VABNCA   non tradurre
001500121205     C* VABCTM = 7Q
001600121205     C* VABLNP = 166
001700121205      *
001800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001900990908
002000990910     Ftivin00r  uF   E             DISK    usropn
002100020913     FFIVABwwr  O    E             DISK    usropn
002200050209     FFIVATwwr  O    E             DISK    usropn
002300990908
002400000512     D*------------
002500990920     D dscmz         e ds                  inz
002600990910     D psds           sds
002700990910     D  procname         *PROC
002800080411     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002900080411     D* 10+3
003000080411     D                 DS
003100080411     D  WS15                   1      1
003200080411     D  WI15                   2     11
003300080411     D  WV15                  12     12
003400080411     D  WD15                  13     15
003500080411     D  WASC15                 1     15
003600080411     D*  6+1
003700080411     D                 DS
003800080411     D  WS09                   1      1
003900080411     D  WI09                   2      7
004000080411     D  WV09                   8      8
004100080411     D  WD09                   9      9
004200080411     D  WASC09                 1      9
004300080411     D*  2+3
004400080411     D                 DS
004500080411     D  WS07                   1      1
004600080411     D  WI07                   2      3
004700080411     D  WV07                   4      4
004800080411     D  WD07                   5      7
004900080411     D  WASC07                 1      7
005000990920     D tivlrds       e ds                  extname(tivlr00f)
005100080411     D tisi95ds      e ds
005200990910     D fnvabaar      e ds                  inz
005300990910     D esito           s              1
005400000724     D prmlit          s             10
005500000710     D prmfir          s             10
005600990921     D wrkesito        s                   like(esito)
005700000613     D rrnum           s              6  0 INZ(*zeros)
005800121205     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
005900050209     D parccm          s              8    INZ(*blanks)
006000050209     D parmbr          s             10    INZ(*blanks)
006100050209     D paropz          s              1    INZ(*blanks)
006200050209     D chkcall         s              1    INZ(*blanks)
006300050209     D*------------------
006400100518     D* VARIABILI X "XCHKCHAR"
006500100518     D*------------------
006600100518     D TxtInOut        S           2048
006700100518     D ElencoChar      S            256
006800100518     D TipoElenco      S              1
006900100518     D CharSost        S              1
007000100518     D UpperCase       S              1
007100100518     D ChkNull         S              1
007200100518     D CharNull        S              1
007300100518     D EsitoBon        S              1
007400100518     D*------------------
007500100518
007600050209
007700050209
007800050209     C* /MAIN
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010530     C*
008300010530     C                   exsr      opeini
008400010530     C                   exsr      rwvab
008500050209     C*
008600050209     C* Effettuo la chiamata al CLLE preposto
008700050209     C                   call(e)   'TITVVTC'
008800050209     C                   parm                    parccm
008900050209     C                   parm                    parmbr
009000050209     C                   parm      '2'           paropz
009100080411     C*
009200080411     C* Esegue lancio TISI95R solo x chiusura
009300080411     C                   CLEAR                   TISI95DS
009400080411     C                   EVAL      I95TLA = 'C'
009500080411     C                   CALL      'TISI95R'
009600080411     C                   PARM                    TISI95DS
009700010530     C*
009800010530     C                   seton                                        lr
009900010530
010000010530
010100010530
010200010530
010300010530     C*--------------------------------------------------------
010400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010500010530     C*--------------------------------------------------------
010600010530     C     PREELA        BEGSR
010700000724     C*
010800000724     C* SE OCCORRE SPEDIRE IN FILIALE
010900010530     C                   if        invfil <> *zeros and
011000010530     C                             flgGiro = '0'
011100010530     C*
011200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011300010530     C                   eval      flgGiro = '1'
011400010530     C*
011500010530     C                   endif
011600010530     C*
011700010530     C                   ENDSR
011800010530     C***
011900010530
012000010530
012100010530
012200010530
012300010530
012400010530
012500010530     C*--------------------------------------------------------
012600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010530     C*--------------------------------------------------------
012800010530     C     ENDELA        BEGSR
012900000613     C*
013000010530     C*
013100010530     C                   ENDSR
013200010530     C***
013300010530
013400910830     C*--------------------------------------------------------
013500020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
013600910830     C*--------------------------------------------------------
013700941229     C     RWVAB         BEGSR
013800990910     C*
013900990914     C                   if        not %open(tivin00r)
014000990908     C                   open      tivin00r
014100990914     C                   endif
014200020913     C                   if        not %open(fivabwwr)
014300020913     C                   open      fivabwwr
014400990914     C                   endif
014500050209     C*
014600050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014700050209     C                   exsr      prevat
014800050209     C*
014900050209     C                   if        chkcall = '0'
015000050209     C*
015100050209     C                   if        not %open(fivatwwr)
015200050209     C                   open      fivatwwr
015300050209     C                   endif
015400050209     C*
015500990910     C                   clear                   §CTROK
015600990910     C                   clear                   §CTRMO
015700990910     C                   clear                   §CTRNO
015800990910     C*
015900921023     C                   DO        *HIVAL
016000990913     C*
016100990915     C                   READ      tivin00r                               70
016200010618     C*
016300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016400010618     C                   if        vinflg = '1'
016500010618     C                   eval      flgOk = '1'
016600010618     C                   endif
016700010705     C*
016800010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
016900010705     C                   if        %subst(vindta:1:1) = '='
017000010705     C                   eval      %subst(vindta:1:1) = *blanks
017100010705     C                   endif
017200010705     C*
017300000905     C                   if        vindta > *blanks
017400000613     C                   add       1             rrnum
017500010601     C* ...elimino eventuali segni
017600010601     C     '+':' '       xlate     vindta        vindta
017700990913     C*
017800010601     C                   if        *in70 = *off and
017900990913     C                             (vinflg = *blanks
018000010711     C                              or vinflg = '0'
018100010711     C                              or vinflg = '2')
018200990913     C*
018300020913     C                   clear                   FIVAB000
018400050209     C                   clear                   FIVAT000
018500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018600010711     C                   if        vinflg = *blanks or vinflg = '0'
018700010711     C                   clear                   vinmsg
018800010711     C                   endif
018900010711     C*
019000990913     C                   eval      fnvabaar = vindta
019100080411     C*
019200080411     C                   testb     x'3F'         vhbatb                   02
019300080411     C                   if        not *in02
019400080411     C*
019500080411     C                   SETOFF                                       303132
019600121205     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019700121205     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019800121205     C*    OPPURE TUTTI BLANKS
019900121205     C*
020000121205     C*** LNP fisso 166
020100121205     C                   Z-ADD     166           VABLNP
020200121205     C***                TESTN                   VHBLNP               30
020300121205     C**N30              TESTN                   VHBLNP                 30
020400121205     C**N30              TESTN                   VHBLNP                   30
020500121205     C***                MOVE      VHBLNP        W001A             1
020600121205     C***  *IN30         IFEQ      *OFF
020700121205     C***  W001A         ORLT      '0'
020800121205     C***                SETON                                        31
020900121205     C***                eval      vinmsg = %trimr(vinmsg)
021000121205     C***                          + ' ' + 'VHBLNP'
021100121205     C***                ELSE
021200121205     C***                MOVE      VHBLNP        VABLNP
021300121205     C***  VABLNP        IFLE      *ZEROS
021400121205     C***                SETON                                        31
021500121205     C***                eval      vinmsg = %trimr(vinmsg)
021600121205     C***                          + ' ' + 'VHBLNP'
021700121205     C***                ENDIF
021800121205     C***                ENDIF
021900121205     C*
022000121205     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022100121205     C                   eval      depfil = VABLNP
022200121205     C                   exsr      repfil
022300121205     C                   if        depfil = invfil
022400121205     C                   if        vlrpoi = 999
022500121205     C                   MOVE(P)   invfil        VABFGS
022600121205     C                   else
022700121205     C                   MOVE(P)   vlrpoi        VABFGS
022800121205     C                   endif
022900121205     C*
023000121205     C                   exsr      PREELA
023100121205     C*** CCM = 1661824
023200121205     C                   EVAL      VABCCM = 1661824
023300121205     C***                TESTN                   VHBCCM               30
023400121205     C**N30              TESTN                   VHBCCM                 30
023500121205     C**N30              TESTN                   VHBCCM                   30
023600121205     C***                MOVE      VHBCCM        W001A             1
023700121205     C***  *IN30         IFEQ      *OFF
023800121205     C***  W001A         ORLT      '0'
023900121205     C***                SETON                                        31
024000121205     C***                eval      vinmsg = %trimr(vinmsg)
024100121205     C***                          + ' ' + 'VHBCCM'
024200121205     C***                ELSE
024300121205     C***                MOVE      VHBCCM        VABCCM
024400121205     C***  VABCCM        IFLE      *ZEROS
024500121205     C***                SETON                                        31
024600121205     C***                eval      vinmsg = %trimr(vinmsg)
024700121205     C***                          + ' ' + 'VHBCCM'
024800121205     C***                ENDIF
024900121205     C***                ENDIF
025000121205     C*
025100121205     C                   TESTN                   VHBAAS               30
025200121205     C  N30              TESTN                   VHBAAS                 30
025300121205     C  N30              TESTN                   VHBAAS                   30
025400121205     C                   MOVE      VHBAAS        W001A             1
025500121205     C     *IN30         IFEQ      *OFF
025600121205     C     W001A         ORLT      '0'
025700121205     C     W001A         ANDNE     *BLANK
025800121205     C                   SETON                                        32
025900121205     C                   MOVEL     datcor        VABAAS
026000121205     C                   eval      vinmsg = %trimr(vinmsg)
026100121205     C                             + ' ' + 'VHBAAS'
026200121205     C                   ELSE
026300121205     C                   MOVE      VHBAAS        VABAAS
026400121205     C                   ENDIF
026500121205     C*
026600121205     C                   TESTN                   VHBMGS               30
026700121205     C  N30              TESTN                   VHBMGS                 30
026800121205     C  N30              TESTN                   VHBMGS                   30
026900121205     C                   MOVE      VHBMGS        W001A             1
027000121205     C     *IN30         IFEQ      *OFF
027100121205     C     W001A         ORLT      '0'
027200121205     C     W001A         ANDNE     *BLANK
027300121205     C                   SETON                                        32
027400121205     C                   MOVE      '0101'        VABMGS
027500121205     C                   eval      vinmsg = %trimr(vinmsg)
027600121205     C                             + ' ' + 'VHBMGS'
027700121205     C                   ELSE
027800121205     C                   MOVE      VHBMGS        VABMGS
027900121205     C                   ENDIF
028000121205     C*
028100121205     C*** VABNRS non va tradotto
028200121205     C***                TESTN                   VHBNRS               30
028300121205     C**N30              TESTN                   VHBNRS                 30
028400121205     C**N30              TESTN                   VHBNRS                   30
028500121205     C***                MOVE      VHBNRS        W001A             1
028600121205     C***  *IN30         IFEQ      *OFF
028700121205     C***  W001A         ORLT      '0'
028800121205     C***  W001A         ANDNE     *BLANK
028900121205     C***                SETON                                        32
029000121205     C***                MOVE      *ZERO         VABNRS
029100121205     C***                eval      vinmsg = %trimr(vinmsg)
029200121205     C***                          + ' ' + 'VHBNRS'
029300121205     C***                ELSE
029400121205     C***                MOVE      VHBNRS        VABNRS
029500121205     C***                ENDIF
029600121205     C*
029700121219     C*** VABNSP viene reperito mediante RMN
029800121219     C***
029900121219     C***                TESTN                   VHBNSP               30
030000121219     C**N30              TESTN                   VHBNSP                 30
030100121219     C**N30              TESTN                   VHBNSP                   30
030200121219     C***                MOVE      VHBNSP        W001A             1
030300121219     C***  *IN30         IFEQ      *OFF
030400121219     C***  W001A         ORLT      '0'
030500121219     C***  W001A         ANDNE     *BLANK
030600121219     C***                SETON                                        32
030700121219     C***                MOVE      *ZERO         VABNSP
030800121219     C***                eval      vinmsg = %trimr(vinmsg)
030900121219     C***                          + ' ' + 'VHBNSP'
031000121219     C***                ELSE
031100121219     C***                MOVE      VHBNSP        VABNSP
031200121219     C***                ENDIF
031300121205     C*
031400121205     C                   TESTN                   VHBLNA               30
031500121205     C  N30              TESTN                   VHBLNA                 30
031600121205     C  N30              TESTN                   VHBLNA                   30
031700121205     C                   MOVE      VHBLNA        W001A             1
031800121205     C     *IN30         IFEQ      *OFF
031900121205     C     W001A         ORLT      '0'
032000121205     C     W001A         ANDNE     *BLANK
032100121205     C                   SETON                                        32
032200121205     C                   MOVE      *ZERO         VABLNA
032300121205     C                   eval      vinmsg = %trimr(vinmsg)
032400121205     C                             + ' ' + 'VHBLNA'
032500121205     C                   ELSE
032600121205     C                   MOVE      VHBLNA        VABLNA
032700121205     C                   ENDIF
032800121205     C*** il codice tariffa è fisso 000
032900121205     C                   EVAL      VABCTR = 000
033000121205     C***                TESTN                   VHBCTR               30
033100121205     C**N30              TESTN                   VHBCTR                 30
033200121205     C**N30              TESTN                   VHBCTR                   30
033300121205     C***                MOVE      VHBCTR        W001A             1
033400121205     C***  *IN30         IFEQ      *OFF
033500121205     C***  W001A         ORLT      '0'
033600121205     C***  W001A         ANDNE     *BLANK
033700121205     C***                SETON                                        32
033800121205     C***                MOVE      *ZERO         VABCTR
033900121205     C***                eval      vinmsg = %trimr(vinmsg)
034000121205     C***                          + ' ' + 'VHBCTR'
034100121205     C***                ELSE
034200121205     C***                MOVE      VHBCTR        VABCTR
034300121205     C***                ENDIF
034400121205     C*
034500121205     C                   MOVE      *ZERO         VABIAS
034600121205     C                   MOVE      VHBIAS        WASC15
034700121205     C                   TESTN                   WI15                 30
034800121205     C  N30              TESTN                   WI15                   30
034900121205     C  N30              TESTN                   WI15                     30
035000121205     C                   MOVE      WI15          W001A             1
035100121205     C     *IN30         IFEQ      *OFF
035200121205     C     W001A         ORLT      '0'
035300121205     C     W001A         ANDNE     *BLANK
035400121205     C                   SETON                                        32
035500121205     C                   MOVE      *ZERO         VABIAS
035600121205     C                   eval      vinmsg = %trimr(vinmsg)
035700121205     C                             + ' ' + 'VHBIAS'
035800121205     C                   ELSE
035900121205     C                   MOVEL     WI15          VABIAS
036000121205     C                   TESTN                   WD15                 30
036100121205     C  N30              TESTN                   WD15                   30
036200121205     C  N30              TESTN                   WD15                     30
036300121205     C                   MOVE      WD15          W001A             1
036400121205     C     *IN30         IFEQ      *OFF
036500121205     C     W001A         ORLT      '0'
036600121205     C     W001A         ANDNE     *BLANK
036700121205     C                   SETON                                        32
036800121205     C                   MOVE      *ZERO         VABIAS
036900121205     C                   eval      vinmsg = %trimr(vinmsg)
037000121205     C                             + ' ' + 'VHBIAS'
037100121205     C                   ELSE
037200121205     C                   MOVE      WD15          VABIAS
037300121205     C                   ENDIF
037400121205     C                   ENDIF
037500121205     C*
037600121205     C                   TESTN                   VHBNCL               30
037700121205     C  N30              TESTN                   VHBNCL                 30
037800121205     C  N30              TESTN                   VHBNCL                   30
037900121205     C                   MOVE      VHBNCL        W001A             1
038000121205     C     *IN30         IFEQ      *OFF
038100121205     C     W001A         ORLT      '0'
038200121205     C                   SETON                                        32
038300121205     C                   MOVE      *ZERO         VABNCL
038400121205     C                   eval      vinmsg = %trimr(vinmsg)
038500121205     C                             + ' ' + 'VHBNCL'
038600121205     C                   ELSE
038700121205     C                   MOVE      VHBNCL        VABNCL
038800121205     C                   ENDIF
038900121205     C*
039000121205     C                   MOVE      *ZERO         VABPKB
039100121205     C                   MOVE      VHBPKB        WASC09
039200121205     C                   IF        WI09 = *blanks
039300121205     C                   EVAL      WI09 = *zeros
039400121205     C                   ENDIF
039500121205     C                   TESTN                   WI09                 30
039600121205     C  N30              TESTN                   WI09                   30
039700121205     C  N30              TESTN                   WI09                     30
039800121205     C                   MOVE      WI09          W001A             1
039900121205     C     *IN30         IFEQ      *OFF
040000121205     C     W001A         ORLT      '0'
040100121205     C                   SETON                                        32
040200121205     C                   MOVE      *ZERO         VABPKB
040300121205     C                   eval      vinmsg = %trimr(vinmsg)
040400121205     C                             + ' ' + 'VHBPKB'
040500121205     C                   ELSE
040600121205     C                   MOVEL     WI09          VABPKB
040700121205     C                   IF        WD09 = *blanks
040800121205     C                   EVAL      WD09 = *zeros
040900121205     C                   ENDIF
041000121205     C                   TESTN                   WD09                 30
041100121205     C  N30              TESTN                   WD09                     30
041200121205     C                   MOVE      WD09          W001A             1
041300121205     C     *IN30         IFEQ      *OFF
041400121205     C     W001A         ORLT      '0'
041500121205     C     W001A         ANDNE     *BLANK
041600121205     C                   SETON                                        32
041700121205     C                   MOVE      *ZERO         VABPKB
041800121205     C                   eval      vinmsg = %trimr(vinmsg)
041900121205     C                             + ' ' + 'VHBPKB'
042000121205     C                   ELSE
042100121205     C                   MOVE      WD09          VABPKB
042200121205     C                   ENDIF
042300121205     C                   ENDIF
042400121205     C*
042500121205     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
042600121205     C                   IF        *IN51 = *OFF
042700121205     C                   MOVE      *ZERO         VABVLB
042800121205     C                   MOVE      VHBVLB        WASC07
042900121205     C                   IF        WI07 = *blanks
043000121205     C                   EVAL      WI07 = *zeros
043100121205     C                   ENDIF
043200121205     C                   TESTN                   WI07                 30
043300121205     C  N30              TESTN                   WI07                   30
043400121205     C  N30              TESTN                   WI07                     30
043500121205     C                   MOVE      WI07          W001A             1
043600121205     C     *IN30         IFEQ      *OFF
043700121205     C     W001A         ORLT      '0'
043800121205     C     W001A         ANDNE     *BLANK
043900121205     C                   SETON                                        32
044000121205     C                   MOVE      *ZERO         VABVLB
044100121205     C                   eval      vinmsg = %trimr(vinmsg)
044200121205     C                             + ' ' + 'VHBVLB'
044300121205     C                   ELSE
044400121205     C                   MOVEL     WI07          VABVLB
044500121205     C                   IF        WD07 = *blanks
044600121205     C                   EVAL      WD07 = *zeros
044700121205     C                   ENDIF
044800121205     C                   TESTN                   WD07                 30
044900121205     C  N30              TESTN                   WD07                   30
045000121205     C  N30              TESTN                   WD07                     30
045100121205     C                   MOVE      WD07          W001A             1
045200121205     C     *IN30         IFEQ      *OFF
045300121205     C     W001A         ORLT      '0'
045400121205     C     W001A         ANDNE     *BLANK
045500121205     C                   SETON                                        32
045600121205     C                   MOVE      *ZERO         VABVLB
045700121205     C                   eval      vinmsg = %trimr(vinmsg)
045800121205     C                             + ' ' + 'VHBVLB'
045900121205     C                   ELSE
046000121205     C                   MOVE      WD07          VABVLB
046100121205     C                   ENDIF
046200121205     C                   ENDIF
046300121205     C                   ELSE
046400121205     C* ...altrimenti forzo azzeramento del volume
046500121205     C                   Z-ADD     *zeros        VABVLB
046600121205     C                   ENDIF
046700121205     C*
046800121205     C                   MOVE      *ZERO         VABQFT
046900121205     C                   MOVE      VHBQFT        WASC15
047000121205     C                   IF        WI15 = *blanks
047100121205     C                   EVAL      WI15 = *zeros
047200121205     C                   ENDIF
047300121205     C                   TESTN                   WI15                 30
047400121205     C  N30              TESTN                   WI15                   30
047500121205     C  N30              TESTN                   WI15                     30
047600121205     C                   MOVE      WI15          W001A             1
047700121205     C     *IN30         IFEQ      *OFF
047800121205     C     W001A         ORLT      '0'
047900121205     C     W001A         ANDNE     *BLANK
048000121205     C                   SETON                                        32
048100121205     C                   MOVE      *ZERO         VABQFT
048200121205     C                   eval      vinmsg = %trimr(vinmsg)
048300121205     C                             + ' ' + 'VHBQFT'
048400121205     C                   ELSE
048500121205     C                   MOVEL     WI15          VABQFT
048600121205     C                   IF        WD15 = *blanks
048700121205     C                   EVAL      WD15 = *zeros
048800121205     C                   ENDIF
048900121205     C                   TESTN                   WD15                 30
049000121205     C  N30              TESTN                   WD15                   30
049100121205     C  N30              TESTN                   WD15                     30
049200121205     C                   MOVE      WD15          W001A             1
049300121205     C     *IN30         IFEQ      *OFF
049400121205     C     W001A         ORLT      '0'
049500121205     C     W001A         ANDNE     *BLANK
049600121205     C                   SETON                                        32
049700121205     C                   MOVE      *ZERO         VABQFT
049800121205     C                   eval      vinmsg = %trimr(vinmsg)
049900121205     C                             + ' ' + 'VHBQFT'
050000121205     C                   ELSE
050100121205     C                   MOVE      WD15          VABQFT
050200121205     C                   ENDIF
050300121205     C                   ENDIF
050400121205     C*
050500121205     C                   MOVE      *ZERO         VABCAS
050600121205     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
050700121205     C                   EVAL      wVHBCAS = VHBCAS
050800121205     C                   IF        %subst(wVHBCAS:12:1) = '.'
050900121205     C                   EVAL      %subst(wVHBCAS:12:1) = ','
051000121205     C                   ENDIF
051100121205     C*
051200121205     C                   MOVEL(P)  wVHBCAS       PiStr
051300121205     C                   EXSR      CHKNUM
051400121205     C                   IF        PiNum=*on
051500121205     C                   Z-ADD     PiVal         VABCAS
051600121205     C                   ELSE
051700121205     C                   SETON                                        32
051800121205     C                   Z-ADD     *zeros        VABCAS
051900121205     C                   EVAL      vinmsg = %trimr(vinmsg)
052000121205     C                             + ' ' + 'VHBCAS'
052100121205     C                   ENDIF
052200121205     C*
052300121205     C                   TESTN                   VHBRMN               30
052400121205     C  N30              TESTN                   VHBRMN                 30
052500121205     C  N30              TESTN                   VHBRMN                   30
052600121205     C                   MOVE      VHBRMN        W001A             1
052700121205     C     *IN30         IFEQ      *OFF
052800121205     C     W001A         ORLT      '0'
052900121205     C                   SETON                                        32
053000121205     C                   MOVE      *ZERO         VABRMN
053100121205     C                   eval      vinmsg = %trimr(vinmsg)
053200121219     C                             + ' ' + 'VHBRMN VHBNSP'
053300121205     C                   ELSE
053400121205     C                   MOVE      VHBRMN        VABRMN
053500121219     C                   MOVE      VHBRMN        VABNSP
053600121205     C                   ENDIF
053700121205     C*
053800121205     C*** VABNCD non va tradotto
053900121205     C***
054000121205     C***                TESTN                   VHBNCD               30
054100121205     C**N30              TESTN                   VHBNCD                 30
054200121205     C**N30              TESTN                   VHBNCD                   30
054300121205     C***                MOVE      VHBNCD        W001A             1
054400121205     C***  *IN30         IFEQ      *OFF
054500121205     C***  W001A         ORLT      '0'
054600121205     C***  W001A         ANDNE     *BLANK
054700121205     C***                SETON                                        32
054800121205     C***                MOVE      *ZERO         VABNCD
054900121205     C***                eval      vinmsg = %trimr(vinmsg)
055000121205     C***                          + ' ' + 'VHBNCD'
055100121205     C***                ELSE
055200121205     C***                MOVE      VHBNCD        VABNCD
055300121205     C***                ENDIF
055400121205     C*
055500121205     C*** VABNCA non va tradotto
055600121205     C***                TESTN                   VHBNCA               30
055700121205     C**N30              TESTN                   VHBNCA                 30
055800121205     C**N30              TESTN                   VHBNCA                   30
055900121205     C***                MOVE      VHBNCA        W001A             1
056000121205     C***  *IN30         IFEQ      *OFF
056100121205     C***  W001A         ORLT      '0'
056200121205     C***  W001A         ANDNE     *BLANK
056300121205     C***                SETON                                        32
056400121205     C***                MOVE      *ZERO         VABNCA
056500121205     C***                eval      vinmsg = %trimr(vinmsg)
056600121205     C***                          + ' ' + 'VHBNCA'
056700121205     C***                ELSE
056800121205     C***                MOVE      VHBNCA        VABNCA
056900121205     C***                ENDIF
057000121205     C*
057100121205     C                   TESTN                   VHBZNC               30
057200121205     C  N30              TESTN                   VHBZNC                 30
057300121205     C  N30              TESTN                   VHBZNC                   30
057400121205     C                   MOVE      VHBZNC        W001A             1
057500121205     C     *IN30         IFEQ      *OFF
057600121205     C     W001A         ORLT      '0'
057700121205     C     W001A         ANDNE     *BLANK
057800121205     C                   SETON                                        32
057900121205     C                   MOVE      *ZERO         VABZNC
058000121205     C                   eval      vinmsg = %trimr(vinmsg)
058100121205     C                             + ' ' + 'VHBZNC'
058200121205     C                   ELSE
058300121205     C                   MOVE      VHBZNC        VABZNC
058400121205     C                   ENDIF
058500121205     C*
058600121205     C                   TESTN                   VHBDCR               30
058700121205     C  N30              TESTN                   VHBDCR                 30
058800121205     C  N30              TESTN                   VHBDCR                   30
058900121205     C                   MOVE      VHBDCR        W001A             1
059000121205     C     *IN30         IFEQ      *OFF
059100121205     C     W001A         ORLT      '0'
059200121205     C     W001A         ANDNE     *BLANK
059300121205     C                   SETON                                        32
059400121205     C                   MOVE      *ZERO         VABDCR
059500121205     C                   eval      vinmsg = %trimr(vinmsg)
059600121205     C                             + ' ' + 'VHBDCR'
059700121205     C                   ELSE
059800121205     C                   MOVE      VHBDCR        VABDCR
059900121205     C                   ENDIF
060000121205     C*
060100121205     C                   TESTN                   VHBHCR               30
060200121205     C  N30              TESTN                   VHBHCR                 30
060300121205     C  N30              TESTN                   VHBHCR                   30
060400121205     C                   MOVE      VHBHCR        W001A             1
060500121205     C     *IN30         IFEQ      *OFF
060600121205     C     W001A         ORLT      '0'
060700121205     C     W001A         ANDNE     *BLANK
060800121205     C                   SETON                                        32
060900121205     C                   MOVE      *ZERO         VABHCR
061000121205     C                   eval      vinmsg = %trimr(vinmsg)
061100121205     C                             + ' ' + 'VHBHCR'
061200121205     C                   ELSE
061300121205     C                   MOVE      VHBHCR        VABHCR
061400121205     C                   ENDIF
061500121205     C*
061600121205     C                   MOVE      *ZERO         VABVMD
061700121205     C                   MOVE      VHBVMD        WASC15
061800121205     C                   IF        WI15 = *blanks
061900121205     C                   EVAL      WI15 = *zeros
062000121205     C                   ENDIF
062100121205     C                   TESTN                   WI15                 30
062200121205     C  N30              TESTN                   WI15                   30
062300121205     C  N30              TESTN                   WI15                     30
062400121205     C                   MOVE      WI15          W001A             1
062500121205     C     *IN30         IFEQ      *OFF
062600121205     C     W001A         ORLT      '0'
062700121205     C     W001A         ANDNE     *BLANK
062800121205     C                   SETON                                        32
062900121205     C                   MOVE      *ZERO         VABVMD
063000121205     C                   eval      vinmsg = %trimr(vinmsg)
063100121205     C                             + ' ' + 'VHBVMD'
063200121205     C                   ELSE
063300121205     C                   MOVEL     WI15          VABVMD
063400121205     C                   IF        WD15 = *blanks
063500121205     C                   EVAL      WD15 = *zeros
063600121205     C                   ENDIF
063700121205     C                   TESTN                   WD15                 30
063800121205     C  N30              TESTN                   WD15                   30
063900121205     C  N30              TESTN                   WD15                     30
064000121205     C                   MOVE      WD15          W001A             1
064100121205     C     *IN30         IFEQ      *OFF
064200121205     C     W001A         ORLT      '0'
064300121205     C     W001A         ANDNE     *BLANK
064400121205     C                   SETON                                        32
064500121205     C                   MOVE      *ZERO         VABVMD
064600121205     C                   eval      vinmsg = %trimr(vinmsg)
064700121205     C                             + ' ' + 'VHBVMD'
064800121205     C                   ELSE
064900121205     C                   MOVE      WD15          VABVMD
065000121205     C                   ENDIF
065100121205     C                   ENDIF
065200121205     C*
065300121205     C                   TESTN                   VHBANT               30
065400121205     C  N30              TESTN                   VHBANT                 30
065500121205     C  N30              TESTN                   VHBANT                   30
065600121205     C                   MOVE      VHBANT        W001A             1
065700121205     C     *IN30         IFEQ      *OFF
065800121205     C     W001A         ORLT      '0'
065900121205     C     W001A         ANDNE     *BLANK
066000121205     C                   SETON                                        32
066100121205     C                   MOVE      *ZERO         VABANT
066200121205     C                   eval      vinmsg = %trimr(vinmsg)
066300121205     C                             + ' ' + 'VHBANT'
066400121205     C                   ELSE
066500121205     C                   MOVE      VHBANT        VABANT
066600121205     C                   ENDIF
066700121205     C*
066800121205     C* CAMPI ALFABETICI:
066900121205     C                   MOVE      VHBATB        VABATB
067000121205     C                   MOVE      VHBCBO        VABCBO
067100121205     C                   MOVE      VHBRSD        VABRSD
067200121205     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067300121205     C     '@':'A'       XLATE     VABRSD        VABRSD
067400121205     C* ==
067500121205     C                   MOVE      VHBRD2        VABRD2
067600121205     C                   MOVE      VHBIND        VABIND
067700121205     C                   MOVE      VHBCAD        VABCAD
067800121205     C                   MOVE      VHBLOD        VABLOD
067900121205     C                   MOVE      VHBPRD        VABPRD
068000121205     C*** VABNZD non va tradotto
068100121205     C***                MOVE      VHBNZD        VABNZD
068200121205     C                   MOVE      VHBGC1        VABGC1
068300121205     C                   MOVE      VHBGC2        VABGC2
068400121205     C*** VABTSP non va tradotto
068500121205     C***                MOVE      VHBTSP        VABTSP
068600121205     C                   MOVE      VHBVAS        VABVAS
068700121205     C                   MOVE      VHBNAS        VABNAS
068800121205     C                   MOVE      VHBTIC        VABTIC
068900121205     C                   MOVE      VHBVCA        VABVCA
069000121205     C                   MOVE      VHBGCA        VABGCA
069100121205     C                   MOVE      VHBRMA        VABRMA
069200121205     C                   MOVE      VHBXCO        VABXCO
069300121205     C                   MOVE      VHBNOT        VABNOT
069400121205     C                   MOVE      VHBNT2        VABNT2
069500121205     C*** VABCTM fisso 7Q
069600121205     C                   EVAL      VABCTM = '7Q'
069700121205     C***                MOVE      VHBCTM        VABCTM
069800121205     C                   MOVE      VHBFFD        VABFFD
069900121205     C                   MOVE      VHBTCR        VABTCR
070000121205     C                   MOVE      VHBCTS        VABCTS
070100121205     C                   MOVE      VHBFTM        VABFTM
070200121205     C                   MOVE      VHBVAD        VABVAD
070300121205     C                   MOVE      VHBGMA        VABGMA
070400121205     C                   MOVE      VHBGGA        VABGGA
070500121205     C                   MOVE      VHBGVA        VABGVA
070600121205     C                   MOVE      VHBTC1        VABTC1
070700121205     C                   MOVE      VHBTC2        VABTC2
070800121205     C                   MOVE      VHBSCL        VABSCL
070900121205     C                   MOVE      VHBRMO        VABRMO
071000121205     C                   MOVE      VHBCMO        VABCMO
071100121205     C                   MOVE      VHBNMO        VABNMO
071200121205     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071300121205     C* NELLA NAZIONE MITTENTE ORIGINALE
071400121205     C                   IF        %trim(VHBNMO) < 'A'
071500121205     C                   MOVE      *BLANKS       VABNMO
071600121205     C                   ENDIF
071700121205     C*
071800121205     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
071900121205     C                   IF        %trim(VABNZD) = 'I'   OR
072000121205     C                             %trim(VABNZD) = 'IT'  OR
072100121205     C                             %trim(VABNZD) = 'ITA'
072200121205     C                   EVAL      VABNZD = *blanks
072300121205     C                   ENDIF
072400121205     C*
072500121205     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
072600121205     C                   IF        *IN52 = *ON
072700121205     C* Reperisco la provincia dal CAP e dalla località
072800121205     C                   IF        VABCAD <> *blanks AND
072900121205     C                             VABPRD  = *blanks AND
073000121205     C                             VABNZD  = *blanks
073100121205     C                   CLEAR                   TISI95DS
073200121205     C                   EVAL      I95TCN = '3'
073300121205     C                   Z-ADD     datcor        I95DAT
073400121205     C                   EVAL      I95CAP = VABCAD
073500121205     C                   EVAL      I95LOC = VABLOD
073600121205     C                   CALL      'TISI95R'
073700121205     C                   PARM                    TISI95DS
073800121205     C                   EVAL      VABPRD = O95PRV
073900121205     C                   ENDIF
074000121205     C                   ENDIF
074100121205     C*
074200121205     C* "bonifico" i dati d input da eventuali caratteri indesiderati
074300121205     C                   exsr      exeBon
074400121205     C*
074500121205     C* Forzatura su campi DIVISE
074600121205     C                   IF        VABVAS = *blanks
074700121205     C                   EVAL      VABVAS = 'EUR'
074800121205     C                   ENDIF
074900121205     C                   IF        VABVCA = *blanks
075000121205     C                   EVAL      VABVCA = 'EUR'
075100121205     C                   ENDIF
075200121205     C                   IF        VABVAD = *blanks
075300121205     C                   EVAL      VABVAD = 'EUR'
075400121205     C                   ENDIF
075500121205     C*
075600121205     C* Forzature da parametri d anagrafica
075700121205     C                   if        %subst(vlrppt:4:2) <> *blanks
075800121205     C                   eval      VABTIC = %subst(vlrppt:4:2)
075900121205     C                   endif
076000121205     C*
076100121205     C* Forzature CODICE BOLLA
076200121205     C                   eval      VABCBO = '1'
076300080411     C*
076400080411     C* "bonifico" i dati d input da eventuali caratteri indesiderati
076500080411     C                   exsr      exeBon
076600930409     C*
076700010604     C  N31              ADD       1             §CTROK            7 0
076800010604     C   32              ADD       1             §CTRMO            7 0
076900010604     C   31              ADD       1             §CTRNO            7 0
077000080411     C  N31              WRITE     FIVAB000
077100100518     C  N31              EXSR      WRIVAT_E
077200990910     C*
077300010601     C                   if        *in31 = *off and
077400010601     C                             *in32 = *off
077500990910     C                   eval      vinflg = '1'
077600990910     C                   else
077700990910     C                   eval      vinflg = '2'
077800990910     C                   endif
077900991022     C                   endif
078000990910     C                   endif
078100010601     C*
078200010601     C                   endif
078300000905     C*
078400000905     C                   else
078500000905     C                   eval      vinflg = '1'
078600000905     C                   endif
078700000905     C*
078800000905     C  N70              update    tivin000
078900991022     C*
079000991022     C  N70              ENDdo
079100010530     C
079200010530     C*
079300010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
079400010530     C                   if        cntNonEl = *zeros or
079500010530     C                             flgMulti = '0'
079600990910     C* Se non ci sono record con errori ...
079700010530     C                   if        §ctrno = 0 and
079800010601     C                             §ctrmo = 0 and
079900010530     C                             flgStato <> '2'
080000990910     C* ... restituisco esito OK.
080100990921     C                   eval      wrkesito = '0'
080200990910     C                   else
080300010601     C                   if        §ctrok > 0
080400990921     C                   eval      wrkesito = '1'
080500000710     C                   else
080600010615     C                   if        flgOk = '0'
080700010615     C                   eval      wrkesito = '2'
080800010615     C                   else
080900010615     C                   eval      wrkesito = '6'
081000010615     C                   endif
081100990910     C                   endif
081200000710     C                   endif
081300010530     C                   else
081400010530     C                   eval      wrkesito = '9'
081500010530     C                   endif
081600050209     C*
081700050209     C                   else
081800050209     C                   eval      wrkesito = '2'
081900050209     C                   endif
082000990910     C*
082100990914     C                   if        %open(tivin00r)
082200990908     C                   close     tivin00r
082300990914     C                   endif
082400020913     C                   if        %open(fivabwwr)
082500020913     C                   close     fivabwwr
082600990914     C                   endif
082700050209     C                   if        %open(fivatwwr)
082800050209     C                   close     fivatwwr
082900050209     C                   endif
083000010530     C*
083100010530     C                   if        vlrpoi <> 999
083200010530     C                   eval      invfil = vlrpoi
083300010530     C                   endif
083400990910     C*
083500990920     C                   if        §ctrok > 0
083600010530     C                             and invfil > *zeros
083700000613     C                   exsr      invio
083800990920     C                   endif
083900010612     C*
084000010612     C                   if        flgGiro = '1'
084100010612     C                   exsr      endela
084200010612     C                   endif
084300990920     C*
084400910830     C                   ENDSR
084500000613     C***
084600050209
084700050209
084800050209
084900050209     C*----------------------------------------------------*
085000050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
085100050209     C*----------------------------------------------------*
085200050209     C     PREVAT        BEGSR
085300050209     C*
085400050209     C* Compongo il nome del membro da dare al FIVATWWR
085500050209     C                   eval      parmbr = vlrhdl
085600050209     C                   movel     'M'           parmbr
085700050209     C                   eval      parccm = %subst(vlrKSC:2:7)
085800050209     C                   eval      paropz = '1'
085900050209     C* Effettuo la chiamata al CLLE preposto
086000050209     C                   call(e)   'TITVVTC'
086100050209     C                   parm                    parccm
086200050209     C                   parm                    parmbr
086300050209     C                   parm                    paropz
086400050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
086500050209     C                   if        %error
086600050209     C                   movel     '1'           chkcall
086700050209     C                   else
086800050209     C                   movel     '0'           chkcall
086900050209     C                   endif
087000050209     C*
087100050209     C                   ENDSR
087200050209
087300050209
087400990920
087500050209     C*----------------------------------------------------*
087600050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
087700050209     C*----------------------------------------------------*
087800100518     C     WRIVAT_E      BEGSR
087900050209     C*
088000050209     C* Valorizzo il buffer del file dettagli barcode colli
088100050209     C                   EVAL      VATFGS = VABFGS
088200050209     C                   EVAL      VATAAS = VABAAS
088300050209     C                   EVAL      VATLNP = VABLNP
088400050209     C                   EVAL      VATNSP = VABNSP
088500050209     C                   EVAL      VATCCM = VABCCM
088600050209     C                   EVAL      VATTRC='E'
088700100518     C*
088800100518     C* X ogni collo indicato in testata
088900121221     C                   z-add     1             wColli            5 0
089000100518     C                   dow       wColli <= VABNCL
089100100518     C*
089200100518     C* Valorizzo il buffer di scrittura del FIVAT
089300121205     C* Primi 6 digits = ultimi 6 digits del campo VABRMN
089400121205     C* Digits 7 e 8   = progressivo collo
089500121205     C* Digits 9 e 10  = numero colli
089600121205     C                   eval      VATNOT =
089700121205     C                                      %subst(%editc(VABRMN:'X') : 10 : 6)+
089800121221     C                                      %subst(%editc(wColli:'X') : 4 : 2) +
089900121205     C                                      %subst(%editc(VABNCL:'X') : 4 : 2)
090000100518     C                   exsr      wrivat                                       => scarico VAT
090100100518     C*
090200100518     C                   add       1             wColli
090300100518     C                   enddo
090400050209     C*
090500050209     C                   ENDSR
090600100518     C*----------------------------------------------------*
090700100518     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
090800100518     C*----------------------------------------------------*
090900100518     C     WRIVAT        BEGSR
091000100518     C*
091100100518     C* Scrivo solo se valorizzato qualcosa
091200100518     C                   IF        VATNOT <> *blanks
091300100518     C                   WRITE     FIVAT000
091400100518     C                   ENDIF
091500100518     C*
091600100518     C                   ENDSR
091700010530
091800010530
091900050209
092000010530
092100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
092200010530     C     repfil        BEGSR
092300010530     C*
092400010530     C                   if        invfil = *zeros and
092500010530     C                             depfil > *zeros and
092600010613     C                             (vinflg = *blanks or
092700010613     C                              vinflg = *zeros)
092800010530     C
092900010530     C                   eval      invfil = depfil
093000010530     C                   endif
093100010530     C*
093200010530     C                   if        depfil <> invfil and
093300010530     C                             invfil > *zeros
093400010530     C                   eval      flgMulti = '1'
093500010530     C                   if        vinflg = *blanks
093600010530     C                   add       1             cntNonEl
093700010530     C                   endif
093800010530     C                   endif
093900010530     C*
094000010530     C                   if        vinflg = '2'
094100010530     C                   eval      flgStato = '2'
094200010530     C                   endif
094300010530     C*
094400010530     C                   ENDSR
094500010530     C***
094600050209
094700050209
094800050209
094900050209
095000050209      /TITLE Invio dei dati al punto operativo.
095100050209     C     invio         BEGSR
095200050209     C*
095300050209     C* 1° invio FIVAT
095400050209     C                   reset                   dscmz
095500050209     C                   move      vlrpoi        cmzdst
095600050209     C                   eval      cmzfld = 'FIVATWWR'
095700050209     C                   eval      cmzmbd = vlrhdl
095800050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
095900050209     C***                if        prmfir = *blanks
096000050209     C                   eval      cmzfla = 'FIVAT00F'
096100050209     C                   eval      cmzmba = 'FIVAT00F'
096200050209     C***                else
096300050209     C***                eval      cmzfla = prmfir
096400050209     C***                eval      cmzmba = prmfir
096500050209     C***                endif
096600050209     C                   eval      cmznrr = *zeros
096700050209     C                   move      §ctrok        cmznrr
096800050209     C                   eval      cmzlba = vlrfl1
096900050209     C                   call(e)   'TIS711C'
097000050209     C                   parm                    dscmz
097100050209     C                   parm      *blanks       esito
097200050209     C                   if        %error
097300050209     C                             or cmzerr = '1'
097400050209     C                             or esito  = '1'
097500050209     C                   eval      wrkesito = '3'
097600050209     C                   else
097700050209     C*
097800050209     C* 2° invio FIVAB
097900050209     C                   reset                   dscmz
098000050209     C                   move      vlrpoi        cmzdst
098100050209     C                   eval      cmzfld = vlrfou
098200050209     C                   eval      cmzmbd = vlrhdl
098300050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
098400050209     C***                if        prmfir = *blanks
098500050209     C                   eval      cmzfla = 'FIVAB00F'
098600050209     C                   eval      cmzmba = 'FIVAB00F'
098700050209     C***                else
098800050209     C***                eval      cmzfla = prmfir
098900050209     C***                eval      cmzmba = prmfir
099000050209     C***                endif
099100050209     C                   eval      cmznrr = *zeros
099200050209     C                   move      §ctrok        cmznrr
099300050209     C                   eval      cmzlba = vlrfl1
099400050209     C                   call(e)   'TIS711C'
099500050209     C                   parm                    dscmz
099600050209     C                   parm      *blanks       esito
099700050209     C                   if        %error
099800050209     C                             or cmzerr = '1'
099900050209     C                             or esito  = '1'
100000050209     C                   eval      wrkesito = '3'
100100050209     C                   endif
100200050209     C                   endif
100300050209     C*
100400050209     C                   ENDSR
100500050209     C***
100600010530
100700010530
100800010530
100900010530
101000010530      /TITLE Invio dei dati al punto operativo.
101100010530     C     opeini        BEGSR
101200010530     C*
101300010530     C* Inizializzo flag e contatori operativi
101400010530     C                   movel     '0'           flgGiro           1
101500010530     C                   movel     '0'           flgMulti          1
101600010530     C                   movel     '1'           flgStato          1
101700010615     C                   movel     '0'           flgOk             1
101800010530     C                   z-add     *zeros        cntNonEl         10 0
101900010530     C                   z-add     *zeros        depfil            3 0
102000010530     C                   z-add     *zeros        invfil            3 0
102100030407     C*
102200050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
102300030407     C                   if        %subst(vlrppt:1:1) = 'S'
102400030407     C                   seton                                        50
102500030407     C                   else
102600030407     C                   setoff                                       50
102700030407     C                   endif
102800041001     C*
102900050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
103000041001     C                   if        %subst(vlrppt:2:1) = 'S'
103100041001     C                   seton                                        51
103200041001     C                   else
103300041001     C                   setoff                                       51
103400041001     C                   endif
103500080411     C*
103600080411     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
103700080411     C* (naturalmente solo se nn già presente)
103800080411     C                   if        %subst(vlrppt:3:1) = 'S'
103900080411     C                   seton                                        52
104000080411     C                   else
104100080411     C                   setoff                                       52
104200080411     C                   endif
104300010530     C*
104400010530     C                   ENDSR
104500010530     C***
104600010530
104700010530
104800070411
104900070411     C     *pssr         BEGSR
105000070411     C*
105100070411     C                   if        %open(tivin00r)
105200070411     C                   close     tivin00r
105300070411     C                   endif
105400070411     C                   if        %open(fivabwwr)
105500070411     C                   close     fivabwwr
105600070411     C                   endif
105700070411     C                   if        %open(fivatwwr)
105800070411     C                   close     fivatwwr
105900070411     C                   endif
106000070411     C*
106100070411     C* Effettuo la chiamata al CLLE preposto
106200070411     C                   call(e)   'TITVVTC'
106300070411     C                   parm                    parccm
106400070411     C                   parm                    parmbr
106500070411     C                   parm      '2'           paropz
106600070411     C*
106700070411     C                   eval      wrkesito = '2'
106800070411     C*
106900070411     C                   seton                                        LR
107000070411     C*
107100070411     C                   ENDSR     '*CANCL'
107200070411     C***
107300080411
107400080411
107500080411
107600080411      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
107700080411     C     exeBon        BEGSR
107800080411     C*
107900080411     C* Imposto i parametri x il *pgm d "bonifica"
108000080411     C                   EVAL      ElencoChar = *blanks
108100080411     C                   EVAL      TipoElenco = *blanks
108200080411     C                   EVAL      CharSost   = *blanks
108300080411     C                   EVAL      UpperCase  = '1'
108400080411     C                   EVAL      ChkNull    = '1'
108500080411     C                   EVAL      CharNull   = *blanks
108600080411     C*
108700080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
108800080411     C                   EVAL      TxtInOut   = VABRSD
108900080411     C                   EVAL      EsitoBon   = *blanks
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
110300080411     C                   EVAL      VABRSD = TxtInOut
110400080411     C                   ENDIF
110500080411     C*
110600080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
110700080411     C                   EVAL      TxtInOut   = VABIND
110800080411     C                   EVAL      EsitoBon   = *blanks
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
112200080411     C                   EVAL      VABIND = TxtInOut
112300080411     C                   ENDIF
112400080411     C*
112500080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
112600080411     C                   EVAL      TxtInOut   = VABLOD
112700080411     C                   EVAL      EsitoBon   = *blanks
112800080411     C*
112900080411     C                   CALL      'XCHKCHAR'
113000080411     C                   PARM                    TxtInOut
113100080411     C                   PARM                    ElencoChar
113200080411     C                   PARM                    TipoElenco
113300080411     C                   PARM                    CharSost
113400080411     C                   PARM                    UpperCase
113500080411     C                   PARM                    ChkNull
113600080411     C                   PARM                    CharNull
113700080411     C                   PARM                    EsitoBon
113800080411     C*
113900080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
114000080411     C                   IF        EsitoBon = '1'
114100080411     C                   EVAL      VABLOD = TxtInOut
114200080411     C                   ENDIF
114300080411     C*
114400080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
114500080411     C                   EVAL      TxtInOut   = VABNOT
114600080411     C                   EVAL      EsitoBon   = *blanks
114700080411     C                   EVAL      UpperCase  = *blanks
114800080411     C*
114900080411     C                   CALL      'XCHKCHAR'
115000080411     C                   PARM                    TxtInOut
115100080411     C                   PARM                    ElencoChar
115200080411     C                   PARM                    TipoElenco
115300080411     C                   PARM                    CharSost
115400080411     C                   PARM                    UpperCase
115500080411     C                   PARM                    ChkNull
115600080411     C                   PARM                    CharNull
115700080411     C                   PARM                    EsitoBon
115800080411     C*
115900080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
116000080411     C                   IF        EsitoBon = '1'
116100080411     C                   EVAL      VABNOT = TxtInOut
116200080411     C                   ENDIF
116300080411     C*
116400080411     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
116500080411     C                   EVAL      TxtInOut   = VABNT2
116600080411     C                   EVAL      EsitoBon   = *blanks
116700080411     C                   EVAL      UpperCase  = *blanks
116800080411     C*
116900080411     C                   CALL      'XCHKCHAR'
117000080411     C                   PARM                    TxtInOut
117100080411     C                   PARM                    ElencoChar
117200080411     C                   PARM                    TipoElenco
117300080411     C                   PARM                    CharSost
117400080411     C                   PARM                    UpperCase
117500080411     C                   PARM                    ChkNull
117600080411     C                   PARM                    CharNull
117700080411     C                   PARM                    EsitoBon
117800080411     C*
117900080411     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
118000080411     C                   IF        EsitoBon = '1'
118100080411     C                   EVAL      VABNT2 = TxtInOut
118200080411     C                   ENDIF
118300080411     C*
118400080411     C                   ENDSR
118500080411
118600080411
118700080411     C*----------------------------------------------------*
118800080411     C*  CONTROLLO NUMERICITA' CAMPI
118900080411     C*----------------------------------------------------*
119000080411     C     CHKNUM        BEGSR
119100080411     C*
119200080411     C                   IF        PiDecChr = *blanks
119300080411     C                   EVAL      PiDecChr = ','
119400080411     C                   ENDIF
119500080411     C*
119600080411     C                   CALL(e)   'ISNUMERIC'
119700080411     C                   PARM                    PiStr            30
119800080411     C                   PARM                    PiDecChr          1
119900080411     C                   PARM      *ZEROS        PiVal            30 9
120000080411     C                   PARM      '0'           PiInt             1
120100080411     C                   PARM      '0'           PiNum             1
120200080411     C                   IF        %error
120300080411     C                   EVAL      PiNum=*off
120400080411     C                   ENDIF
120500080411     C*
120600080411     C                   ENDSR
120700080411     C***
120800080411
120900080411
121000010530
121100990910
121200000613     C     *inzsr        BEGSR
121300990910     C*
121400990910     C     *entry        plist
121500990920     C                   parm                    tivlrds
121600990921     C                   parm      wrkesito      esito
121700000724     C                   parm                    prmlit
121800000710     C                   parm                    prmfir
121900010606     C*
122000010606     C* CALCOLA LA DATA CORRENTE
122100121205     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
122200121205     C                   eval      datcor = %dec(%date() : *ISO)
122300000613     C*
122400000613     C                   ENDSR
122500000613     C***
