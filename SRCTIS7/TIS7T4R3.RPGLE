000100100719      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200130116     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600050209     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000512     D*------------
000900990920     D dscmz         e ds                  inz
001000990910     D psds           sds
001100100719     D  procname         *PROC
001200100719     D*------------
001300100719     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001400100719     D*------------
001500100719     D* 10+3
001600100719     D                 DS
001700100719     D  WI15                   1     10
001800100719     D  WD15                  11     13
001900100719     D  WASC15                 1     13
002000100719     D*  6+1
002100100719     D                 DS
002200100719     D  WI09                   1      6
002300100719     D  WD09                   7      7
002400100719     D  WASC09                 1      7
002500100719     D*  2+3
002600100719     D                 DS
002700100719     D  WI07                   1      2
002800100719     D  WD07                   3      5
002900100719     D  WASC07                 1      5
003000100719     D*
003100990920     D tivlrds       e ds                  extname(tivlr00f)
003200100719     D tisi95ds      e ds                  inz
003300990910     D esito           s              1
003400000724     D prmlit          s             10
003500000710     D prmfir          s             10
003600990921     D wrkesito        s                   like(esito)
003700000613     D rrnum           s              6  0 INZ(*zeros)
003800050209     D parccm          s              8    INZ(*blanks)
003900050209     D parmbr          s             10    INZ(*blanks)
004000050209     D paropz          s              1    INZ(*blanks)
004100050209     D chkcall         s              1    INZ(*blanks)
004200100719     D*---------------------------------------------------------
004300100719     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
004400100719     D*---------------------------------------------------------
004500100719     D                 ds
004600100719     D vhbatb                  1      1
004700100719     D vhbccm                  2      8
004800100719     D vhblnp                  9     11
004900100719     D vhbaas                 12     15
005000100719     D vhbmgs                 16     19
005100100719     D vhbnrs                 20     21
005200100719     D vhbnsp                 22     28
005300100719     D vhbcbo                 29     30
005400100719     D vhblna                 31     33
005500100719     D vhbrsd                 34     68
005600100719     D vhbrd2                 69    103
005700100719     D vhbind                104    138
005800100719     D vhbcad                139    147
005900100719     D vhblod                148    182
006000100719     D vhbprd                183    184
006100100719     D vhbnzd                185    187
006200100719     D vhbgc1                188    189
006300100719     D vhbgc2                190    191
006400100719     D vhbctr                192    194
006500100719     D vhbtsp                195    195
006600100719     D vhbias                196    208
006700100719     D vhbvas                209    211
006800100719     D vhbnas                212    226
006900100719     D vhbncl                227    231
007000100719     D vhbpkb                232    238
007100100719     D vhbvlb                239    243
007200100719     D vhbqft                244    256
007300100719     D vhbcas                257    269
007400100719     D vhbtic                270    271
007500100719     D vhbvca                272    274
007600100719     D vhbgca                275    276
007700100719     D vhbrmn                277    291
007800100719     D vhbrma                292    306
007900100719     D vhbncd                307    313
008000100719     D vhbnca                314    320
008100100719     D vhbxco                321    321
008200100719     D vhbnot                322    356
008300100719     D vhbnt2                357    391
008400100719     D vhbznc                392    393
008500100719     D vhbctm                394    395
008600100719     D vhbffd                396    396
008700100719     D vhbdcr                397    404
008800100719     D vhbtcr                405    405
008900100719     D vhbhcr                406    409
009000100719     D vhbcts                410    411
009100100719     D vhbftm                412    412
009200100719     D vhbvmd                413    425
009300100719     D vhbvad                426    428
009400100719     D vhbgma                429    430
009500100719     D vhbgga                431    432
009600100719     D vhbgva                433    434
009700100719     D vhbtc1                435    435
009800100719     D vhbtc2                436    436
009900100719     D vhbscl                437    437
010000100719     D vhbant                438    446
010100100719     D vhbrmo                447    471
010200100719     D vhbcmo                472    480
010300100719     D vhbnmo                481    483
010400100719     D  X_rec                  1    483    inz
010500100719     D*
010600050209     D*------------------
010700050209     D* DS REPERIMENTO NUMERATORE
010800050209     D*------------------
010900050303     D trul33ds      e ds                  inz
011000050209     D*------------------
011100050209     D* DS ARCHITETTURA
011200050209     D*------------------
011300050209     D kpjba         e ds                  inz
011400050209     D*------------------
011500100719
011600100719
011700100719     D*------------------
011800100719     D* VARIABILI X *PGM UTILITA XCHKCHAR
011900100719     D*------------------
012000100719     D TxtInOut        S           2048
012100100719     D ElencoChar      S            256
012200100719     D TipoElenco      S              1
012300100719     D CharSost        S              1
012400100719     D UpperCase       S              1
012500100719     D ChkNull         S              1
012600100719     D CharNull        S              1
012700100719     D EsitoBon        S              1
012800100719
012900100719
013000100719     D*------------------
013100100719     D* LINKING A DEFINIZIONI ESTERNE
013200100719     D*------------------
013300100719     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
013400100719     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
013500100719
013600100719
013700050209
013800050209     C* /MAIN
013900000913     C                   reset                   rrnum
014000990921     C                   reset                   esito
014100990921     C                   reset                   wrkesito
014200010530     C*
014300010530     C                   exsr      opeini
014400010530     C                   exsr      rwvab
014500050209     C*
014600050209     C* Effettuo la chiamata al CLLE preposto
014700050209     C                   call(e)   'TITVVTC'
014800050209     C                   parm                    parccm
014900050209     C                   parm                    parmbr
015000050209     C                   parm      '2'           paropz
015100010530     C*
015200010530     C                   seton                                        lr
015300010530
015400010530
015500010530
015600010530
015700010530     C*--------------------------------------------------------
015800010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015900010530     C*--------------------------------------------------------
016000010530     C     PREELA        BEGSR
016100000724     C*
016200000724     C* SE OCCORRE SPEDIRE IN FILIALE
016300010530     C                   if        invfil <> *zeros and
016400010530     C                             flgGiro = '0'
016500010530     C*
016600010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016700010530     C                   eval      flgGiro = '1'
016800010530     C*
016900010530     C                   endif
017000010530     C*
017100010530     C                   ENDSR
017200010530     C***
017300010530
017400010530
017500010530
017600010530
017700010530
017800010530
017900010530     C*--------------------------------------------------------
018000010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
018100010530     C*--------------------------------------------------------
018200010530     C     ENDELA        BEGSR
018300010530     C*
018400010530     C                   ENDSR
018500010530     C***
018600990908
018700010530
018800010530
018900010530
019000910830     C*--------------------------------------------------------
019100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
019200910830     C*--------------------------------------------------------
019300941229     C     RWVAB         BEGSR
019400990910     C*
019500990914     C                   if        not %open(tivin00r)
019600990908     C                   open      tivin00r
019700990914     C                   endif
019800020913     C                   if        not %open(fivabwwr)
019900020913     C                   open      fivabwwr
020000990914     C                   endif
020100050209     C*
020200050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
020300050209     C                   exsr      prevat
020400050209     C*
020500050209     C                   if        chkcall = '0'
020600050209     C*
020700050209     C                   if        not %open(fivatwwr)
020800050209     C                   open      fivatwwr
020900050209     C                   endif
021000050209     C*
021100990910     C                   clear                   §CTROK
021200990910     C                   clear                   §CTRMO
021300990910     C                   clear                   §CTRNO
021400990910     C*
021500921023     C                   DO        *HIVAL
021600990913     C*
021700990915     C                   READ      tivin00r                               70
021800010618     C*
021900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022000010618     C                   if        vinflg = '1'
022100010618     C                   eval      flgOk = '1'
022200010618     C                   endif
022300010705     C*
022400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
022500010705     C                   if        %subst(vindta:1:1) = '='
022600010705     C                   eval      %subst(vindta:1:1) = *blanks
022700010705     C                   endif
022800010705     C*
022900000905     C                   if        vindta > *blanks
023000000613     C                   add       1             rrnum
023100010601     C* ...elimino eventuali segni
023200010601     C     '+':' '       xlate     vindta        vindta
023300990913     C*
023400010601     C                   if        *in70 = *off and
023500990913     C                             (vinflg = *blanks
023600010711     C                              or vinflg = '0'
023700010711     C                              or vinflg = '2')
023800990913     C*
023900020913     C                   clear                   FIVAB000
024000050209     C                   clear                   FIVAT000
024100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024200010711     C                   if        vinflg = *blanks or vinflg = '0'
024300010711     C                   clear                   vinmsg
024400010711     C                   endif
024500010711     C*
024600100719     C                   eval      x_rec  = vindta
024700991022     C*
024800991022     C                   testb     x'3F'         vhbatb                   02
024900991022     C                   if        not *in02
025000991022     C*
025100971021     C                   SETOFF                                       303132
025200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
025300971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
025400971023     C*    OPPURE TUTTI BLANKS
025500100719     C*
025600100719     C                   TESTN                   VHBLNP               30
025700100719     C  N30              TESTN                   VHBLNP                 30
025800100719     C  N30              TESTN                   VHBLNP                   30
025900100719     C                   MOVE      VHBLNP        W001A             1
026000100719     C     *IN30         IFEQ      *OFF
026100100719     C     W001A         ORLT      '0'
026200100719     C                   SETON                                        31
026300100719     C                   eval      vinmsg = %trimr(vinmsg)
026400100719     C                             + ' ' + 'VHBLNP'
026500100719     C                   ELSE
026600100719     C                   MOVE      VHBLNP        VABLNP
026700100719     C     VABLNP        IFLE      *ZEROS
026800100719     C                   SETON                                        31
026900100719     C                   eval      vinmsg = %trimr(vinmsg)
027000100719     C                             + ' ' + 'VHBLNP'
027100100719     C                   ENDIF
027200100719     C                   ENDIF
027300100719     C*
027400100719     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027500100719     C                   eval      depfil = VABLNP
027600100719     C                   exsr      repfil
027700100719     C                   if        depfil = invfil
027800100719     C                   if        vlrpoi = 999
027900100719     C                   MOVE(P)   invfil        VABFGS
028000100719     C                   else
028100100719     C                   MOVE(P)   vlrpoi        VABFGS
028200100719     C                   endif
028300100719     C*
028400100719     C                   exsr      PREELA
028500100719     C*
028600100719     C                   TESTN                   VHBCCM               30
028700100719     C  N30              TESTN                   VHBCCM                 30
028800100719     C  N30              TESTN                   VHBCCM                   30
028900100719     C                   MOVE      VHBCCM        W001A             1
029000100719     C     *IN30         IFEQ      *OFF
029100100719     C     W001A         ORLT      '0'
029200100719     C                   SETON                                        31
029300100719     C                   eval      vinmsg = %trimr(vinmsg)
029400100719     C                             + ' ' + 'VHBCCM'
029500100719     C                   ELSE
029600100719     C                   MOVE      VHBCCM        VABCCM
029700100719     C     VABCCM        IFLE      *ZEROS
029800100719     C                   SETON                                        31
029900100719     C                   eval      vinmsg = %trimr(vinmsg)
030000100719     C                             + ' ' + 'VHBCCM'
030100100719     C                   ENDIF
030200100719     C                   ENDIF
030300100719     C*
030400100719     C                   TESTN                   VHBAAS               30
030500100719     C  N30              TESTN                   VHBAAS                 30
030600100719     C  N30              TESTN                   VHBAAS                   30
030700100719     C                   MOVE      VHBAAS        W001A             1
030800100719     C     *IN30         IFEQ      *OFF
030900100719     C     W001A         ORLT      '0'
031000100719     C     W001A         ANDNE     *BLANK
031100100719     C                   SETON                                        32
031200100719     C                   MOVEL     datcor        VABAAS
031300100719     C                   eval      vinmsg = %trimr(vinmsg)
031400100719     C                             + ' ' + 'VHBAAS'
031500100719     C                   ELSE
031600100719     C                   MOVE      VHBAAS        VABAAS
031700100719     C                   ENDIF
031800100719     C*
031900100719     C                   TESTN                   VHBMGS               30
032000100719     C  N30              TESTN                   VHBMGS                 30
032100100719     C  N30              TESTN                   VHBMGS                   30
032200100719     C                   MOVE      VHBMGS        W001A             1
032300100719     C     *IN30         IFEQ      *OFF
032400100719     C     W001A         ORLT      '0'
032500100719     C     W001A         ANDNE     *BLANK
032600100719     C                   SETON                                        32
032700100719     C                   MOVE      '0101'        VABMGS
032800100719     C                   eval      vinmsg = %trimr(vinmsg)
032900100719     C                             + ' ' + 'VHBMGS'
033000100719     C                   ELSE
033100100719     C                   MOVE      VHBMGS        VABMGS
033200100719     C                   ENDIF
033300100719     C*
033400100719     C                   TESTN                   VHBNRS               30
033500100719     C  N30              TESTN                   VHBNRS                 30
033600100719     C  N30              TESTN                   VHBNRS                   30
033700100719     C                   MOVE      VHBNRS        W001A             1
033800100719     C     *IN30         IFEQ      *OFF
033900100719     C     W001A         ORLT      '0'
034000100719     C     W001A         ANDNE     *BLANK
034100100719     C                   SETON                                        32
034200100719     C                   MOVE      *ZERO         VABNRS
034300100719     C                   eval      vinmsg = %trimr(vinmsg)
034400100719     C                             + ' ' + 'VHBNRS'
034500100719     C                   ELSE
034600100719     C                   MOVE      VHBNRS        VABNRS
034700100719     C                   ENDIF
034800100719     C*
034900100719     C                   TESTN                   VHBNSP               30
035000100719     C  N30              TESTN                   VHBNSP                 30
035100100719     C  N30              TESTN                   VHBNSP                   30
035200100719     C                   MOVE      VHBNSP        W001A             1
035300100719     C     *IN30         IFEQ      *OFF
035400100719     C     W001A         ORLT      '0'
035500100719     C     W001A         ANDNE     *BLANK
035600100719     C                   SETON                                        32
035700100719     C                   MOVE      *ZERO         VABNSP
035800100719     C                   eval      vinmsg = %trimr(vinmsg)
035900100719     C                             + ' ' + 'VHBNSP'
036000100719     C                   ELSE
036100100719     C                   MOVE      VHBNSP        VABNSP
036200100719     C                   ENDIF
036300100719     C*
036400100719     C                   TESTN                   VHBLNA               30
036500100719     C  N30              TESTN                   VHBLNA                 30
036600100719     C  N30              TESTN                   VHBLNA                   30
036700100719     C                   MOVE      VHBLNA        W001A             1
036800100719     C     *IN30         IFEQ      *OFF
036900100719     C     W001A         ORLT      '0'
037000100719     C     W001A         ANDNE     *BLANK
037100100719     C                   SETON                                        32
037200100719     C                   MOVE      *ZERO         VABLNA
037300100719     C                   eval      vinmsg = %trimr(vinmsg)
037400100719     C                             + ' ' + 'VHBLNA'
037500100719     C                   ELSE
037600100719     C                   MOVE      VHBLNA        VABLNA
037700100719     C                   ENDIF
037800100719     C*
037900100719     C                   TESTN                   VHBCTR               30
038000100719     C  N30              TESTN                   VHBCTR                 30
038100100719     C  N30              TESTN                   VHBCTR                   30
038200100719     C                   MOVE      VHBCTR        W001A             1
038300100719     C     *IN30         IFEQ      *OFF
038400100719     C     W001A         ORLT      '0'
038500100719     C     W001A         ANDNE     *BLANK
038600100719     C                   SETON                                        32
038700100719     C                   MOVE      *ZERO         VABCTR
038800100719     C                   eval      vinmsg = %trimr(vinmsg)
038900100719     C                             + ' ' + 'VHBCTR'
039000100719     C                   ELSE
039100100719     C                   MOVE      VHBCTR        VABCTR
039200100719     C                   ENDIF
039300100719     C*
039400100719     C                   MOVE      *ZERO         VABIAS
039500100719     C                   MOVE      VHBIAS        WASC15
039600100719     C                   TESTN                   WI15                 30
039700100719     C  N30              TESTN                   WI15                   30
039800100719     C  N30              TESTN                   WI15                     30
039900100719     C                   MOVE      WI15          W001A             1
040000100719     C     *IN30         IFEQ      *OFF
040100100719     C     W001A         ORLT      '0'
040200100719     C     W001A         ANDNE     *BLANK
040300100719     C                   SETON                                        32
040400100719     C                   MOVE      *ZERO         VABIAS
040500100719     C                   eval      vinmsg = %trimr(vinmsg)
040600100719     C                             + ' ' + 'VHBIAS'
040700100719     C                   ELSE
040800100719     C                   MOVEL     WI15          VABIAS
040900100719     C                   TESTN                   WD15                 30
041000100719     C  N30              TESTN                   WD15                   30
041100100719     C  N30              TESTN                   WD15                     30
041200100719     C                   MOVE      WD15          W001A             1
041300100719     C     *IN30         IFEQ      *OFF
041400100719     C     W001A         ORLT      '0'
041500100719     C     W001A         ANDNE     *BLANK
041600100719     C                   SETON                                        32
041700100719     C                   MOVE      *ZERO         VABIAS
041800100719     C                   eval      vinmsg = %trimr(vinmsg)
041900100719     C                             + ' ' + 'VHBIAS'
042000100719     C                   ELSE
042100100719     C                   MOVE      WD15          VABIAS
042200100719     C                   ENDIF
042300100719     C                   ENDIF
042400100719     C*
042500100719     C                   TESTN                   VHBNCL               30
042600100719     C  N30              TESTN                   VHBNCL                 30
042700100719     C  N30              TESTN                   VHBNCL                   30
042800100719     C                   MOVE      VHBNCL        W001A             1
042900100719     C     *IN30         IFEQ      *OFF
043000100719     C     W001A         ORLT      '0'
043100100719     C                   SETON                                        32
043200100719     C                   MOVE      *ZERO         VABNCL
043300100719     C                   eval      vinmsg = %trimr(vinmsg)
043400100719     C                             + ' ' + 'VHBNCL'
043500100719     C                   ELSE
043600100719     C                   MOVE      VHBNCL        VABNCL
043700100719     C                   ENDIF
043800100719     C*
043900100719     C                   MOVE      *ZERO         VABPKB
044000100719     C                   MOVE      VHBPKB        WASC09
044100100719     C                   IF        WI09 = *blanks
044200100719     C                   EVAL      WI09 = *zeros
044300100719     C                   ENDIF
044400100719     C                   TESTN                   WI09                 30
044500100719     C  N30              TESTN                   WI09                   30
044600100719     C  N30              TESTN                   WI09                     30
044700100719     C                   MOVE      WI09          W001A             1
044800100719     C     *IN30         IFEQ      *OFF
044900100719     C     W001A         ORLT      '0'
045000100719     C                   SETON                                        32
045100100719     C                   MOVE      *ZERO         VABPKB
045200100719     C                   eval      vinmsg = %trimr(vinmsg)
045300100719     C                             + ' ' + 'VHBPKB'
045400100719     C                   ELSE
045500100719     C                   MOVEL     WI09          VABPKB
045600100719     C                   TESTN                   WD09                 30
045700100719     C  N30              TESTN                   WD09                     30
045800100719     C                   MOVE      WD09          W001A             1
045900100719     C     *IN30         IFEQ      *OFF
046000100719     C     W001A         ORLT      '0'
046100100719     C     W001A         ANDNE     *BLANK
046200100719     C                   SETON                                        32
046300100719     C                   MOVE      *ZERO         VABPKB
046400100719     C                   eval      vinmsg = %trimr(vinmsg)
046500100719     C                             + ' ' + 'VHBPKB'
046600100719     C                   ELSE
046700100719     C                   MOVE      WD09          VABPKB
046800100719     C                   ENDIF
046900100719     C                   ENDIF
047000100719     C*
047100100719     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
047200100719     C                   IF        *IN51 = *OFF
047300100719     C                   MOVE      *ZERO         VABVLB
047400100719     C                   MOVE      VHBVLB        WASC07
047500100719     C                   IF        WI07 = *blanks
047600100719     C                   EVAL      WI07 = *zeros
047700100719     C                   ENDIF
047800100719     C                   TESTN                   WI07                 30
047900100719     C  N30              TESTN                   WI07                   30
048000100719     C  N30              TESTN                   WI07                     30
048100100719     C                   MOVE      WI07          W001A             1
048200100719     C     *IN30         IFEQ      *OFF
048300100719     C     W001A         ORLT      '0'
048400100719     C     W001A         ANDNE     *BLANK
048500100719     C                   SETON                                        32
048600100719     C                   MOVE      *ZERO         VABVLB
048700100719     C                   eval      vinmsg = %trimr(vinmsg)
048800100719     C                             + ' ' + 'VHBVLB'
048900100719     C                   ELSE
049000100719     C                   MOVEL     WI07          VABVLB
049100100719     C                   TESTN                   WD07                 30
049200100719     C  N30              TESTN                   WD07                   30
049300100719     C  N30              TESTN                   WD07                     30
049400100719     C                   MOVE      WD07          W001A             1
049500100719     C     *IN30         IFEQ      *OFF
049600100719     C     W001A         ORLT      '0'
049700100719     C     W001A         ANDNE     *BLANK
049800100719     C                   SETON                                        32
049900100719     C                   MOVE      *ZERO         VABVLB
050000100719     C                   eval      vinmsg = %trimr(vinmsg)
050100100719     C                             + ' ' + 'VHBVLB'
050200100719     C                   ELSE
050300100719     C                   MOVE      WD07          VABVLB
050400100719     C                   ENDIF
050500100719     C                   ENDIF
050600100719     C                   ELSE
050700100719     C* ...altrimenti forzo azzeramento del volume
050800100719     C                   Z-ADD     *zeros        VABVLB
050900100719     C                   ENDIF
051000100719     C*
051100100719     C                   MOVE      *ZERO         VABQFT
051200100719     C                   MOVE      VHBQFT        WASC15
051300100719     C                   TESTN                   WI15                 30
051400100719     C  N30              TESTN                   WI15                   30
051500100719     C  N30              TESTN                   WI15                     30
051600100719     C                   MOVE      WI15          W001A             1
051700100719     C     *IN30         IFEQ      *OFF
051800100719     C     W001A         ORLT      '0'
051900100719     C     W001A         ANDNE     *BLANK
052000100719     C                   SETON                                        32
052100100719     C                   MOVE      *ZERO         VABQFT
052200100719     C                   eval      vinmsg = %trimr(vinmsg)
052300100719     C                             + ' ' + 'VHBQFT'
052400100719     C                   ELSE
052500100719     C                   MOVEL     WI15          VABQFT
052600100719     C                   TESTN                   WD15                 30
052700100719     C  N30              TESTN                   WD15                   30
052800100719     C  N30              TESTN                   WD15                     30
052900100719     C                   MOVE      WD15          W001A             1
053000100719     C     *IN30         IFEQ      *OFF
053100100719     C     W001A         ORLT      '0'
053200100719     C     W001A         ANDNE     *BLANK
053300100719     C                   SETON                                        32
053400100719     C                   MOVE      *ZERO         VABQFT
053500100719     C                   eval      vinmsg = %trimr(vinmsg)
053600100719     C                             + ' ' + 'VHBQFT'
053700100719     C                   ELSE
053800100719     C                   MOVE      WD15          VABQFT
053900100719     C                   ENDIF
054000100719     C                   ENDIF
054100100719     C*
054200100719     C                   MOVE      *ZERO         VABCAS
054300100719     C                   MOVE      VHBCAS        WASC15
054400100719     C                   TESTN                   WI15                 30
054500100719     C  N30              TESTN                   WI15                   30
054600100719     C  N30              TESTN                   WI15                     30
054700100719     C                   MOVE      WI15          W001A             1
054800100719     C     *IN30         IFEQ      *OFF
054900100719     C     W001A         ORLT      '0'
055000100719     C     W001A         ANDNE     *BLANK
055100100719     C                   SETON                                        32
055200100719     C                   MOVE      *ZERO         VABCAS
055300100719     C                   eval      vinmsg = %trimr(vinmsg)
055400100719     C                             + ' ' + 'VHBCAS'
055500100719     C                   ELSE
055600100719     C                   MOVEL     WI15          VABCAS
055700100719     C                   TESTN                   WD15                 30
055800100719     C  N30              TESTN                   WD15                   30
055900100719     C  N30              TESTN                   WD15                     30
056000100719     C                   MOVE      WD15          W001A             1
056100100719     C     *IN30         IFEQ      *OFF
056200100719     C     W001A         ORLT      '0'
056300100719     C     W001A         ANDNE     *BLANK
056400100719     C                   SETON                                        32
056500100719     C                   MOVE      *ZERO         VABCAS
056600100719     C                   eval      vinmsg = %trimr(vinmsg)
056700100719     C                             + ' ' + 'VHBCAS'
056800100719     C                   ELSE
056900100719     C                   MOVE      WD15          VABCAS
057000100719     C                   ENDIF
057100100719     C                   ENDIF
057200100719     C*
057300100719     C                   TESTN                   VHBRMN               30
057400100719     C  N30              TESTN                   VHBRMN                 30
057500100719     C  N30              TESTN                   VHBRMN                   30
057600100719     C                   MOVE      VHBRMN        W001A             1
057700100719     C     *IN30         IFEQ      *OFF
057800100719     C     W001A         ORLT      '0'
057900100719     C                   SETON                                        32
058000100719     C                   MOVE      *ZERO         VABRMN
058100100719     C                   eval      vinmsg = %trimr(vinmsg)
058200100719     C                             + ' ' + 'VHBRMN'
058300100719     C                   ELSE
058400100719     C                   MOVE      VHBRMN        VABRMN
058500100719     C                   ENDIF
058600100719     C*
058700100719     C                   TESTN                   VHBNCD               30
058800100719     C  N30              TESTN                   VHBNCD                 30
058900100719     C  N30              TESTN                   VHBNCD                   30
059000100719     C                   MOVE      VHBNCD        W001A             1
059100100719     C     *IN30         IFEQ      *OFF
059200100719     C     W001A         ORLT      '0'
059300100719     C     W001A         ANDNE     *BLANK
059400100719     C                   SETON                                        32
059500100719     C                   MOVE      *ZERO         VABNCD
059600100719     C                   eval      vinmsg = %trimr(vinmsg)
059700100719     C                             + ' ' + 'VHBNCD'
059800100719     C                   ELSE
059900100719     C                   MOVE      VHBNCD        VABNCD
060000100719     C                   ENDIF
060100100719     C*
060200100719     C                   TESTN                   VHBNCA               30
060300100719     C  N30              TESTN                   VHBNCA                 30
060400100719     C  N30              TESTN                   VHBNCA                   30
060500100719     C                   MOVE      VHBNCA        W001A             1
060600100719     C     *IN30         IFEQ      *OFF
060700100719     C     W001A         ORLT      '0'
060800100719     C     W001A         ANDNE     *BLANK
060900100719     C                   SETON                                        32
061000100719     C                   MOVE      *ZERO         VABNCA
061100100719     C                   eval      vinmsg = %trimr(vinmsg)
061200100719     C                             + ' ' + 'VHBNCA'
061300100719     C                   ELSE
061400100719     C                   MOVE      VHBNCA        VABNCA
061500100719     C                   ENDIF
061600100719     C*
061700100719     C                   TESTN                   VHBZNC               30
061800100719     C  N30              TESTN                   VHBZNC                 30
061900100719     C  N30              TESTN                   VHBZNC                   30
062000100719     C                   MOVE      VHBZNC        W001A             1
062100100719     C     *IN30         IFEQ      *OFF
062200100719     C     W001A         ORLT      '0'
062300100719     C     W001A         ANDNE     *BLANK
062400100719     C                   SETON                                        32
062500100719     C                   MOVE      *ZERO         VABZNC
062600100719     C                   eval      vinmsg = %trimr(vinmsg)
062700100719     C                             + ' ' + 'VHBZNC'
062800100719     C                   ELSE
062900100719     C                   MOVE      VHBZNC        VABZNC
063000100719     C                   ENDIF
063100100719     C*
063200100719     C                   TESTN                   VHBDCR               30
063300100719     C  N30              TESTN                   VHBDCR                 30
063400100719     C  N30              TESTN                   VHBDCR                   30
063500100719     C                   MOVE      VHBDCR        W001A             1
063600100719     C     *IN30         IFEQ      *OFF
063700100719     C     W001A         ORLT      '0'
063800100719     C     W001A         ANDNE     *BLANK
063900100719     C                   SETON                                        32
064000100719     C                   MOVE      *ZERO         VABDCR
064100100719     C                   eval      vinmsg = %trimr(vinmsg)
064200100719     C                             + ' ' + 'VHBDCR'
064300100719     C                   ELSE
064400100719     C                   MOVE      VHBDCR        VABDCR
064500100719     C                   ENDIF
064600100719     C*
064700100719     C                   TESTN                   VHBHCR               30
064800100719     C  N30              TESTN                   VHBHCR                 30
064900100719     C  N30              TESTN                   VHBHCR                   30
065000100719     C                   MOVE      VHBHCR        W001A             1
065100100719     C     *IN30         IFEQ      *OFF
065200100719     C     W001A         ORLT      '0'
065300100719     C     W001A         ANDNE     *BLANK
065400100719     C                   SETON                                        32
065500100719     C                   MOVE      *ZERO         VABHCR
065600100719     C                   eval      vinmsg = %trimr(vinmsg)
065700100719     C                             + ' ' + 'VHBHCR'
065800100719     C                   ELSE
065900100719     C                   MOVE      VHBHCR        VABHCR
066000100719     C                   ENDIF
066100100719     C*
066200100719     C                   MOVE      *ZERO         VABVMD
066300100719     C                   MOVE      VHBVMD        WASC15
066400100719     C                   TESTN                   WI15                 30
066500100719     C  N30              TESTN                   WI15                   30
066600100719     C  N30              TESTN                   WI15                     30
066700100719     C                   MOVE      WI15          W001A             1
066800100719     C     *IN30         IFEQ      *OFF
066900100719     C     W001A         ORLT      '0'
067000100719     C     W001A         ANDNE     *BLANK
067100100719     C                   SETON                                        32
067200100719     C                   MOVE      *ZERO         VABVMD
067300100719     C                   eval      vinmsg = %trimr(vinmsg)
067400100719     C                             + ' ' + 'VHBVMD'
067500100719     C                   ELSE
067600100719     C                   MOVEL     WI15          VABVMD
067700100719     C                   TESTN                   WD15                 30
067800100719     C  N30              TESTN                   WD15                   30
067900100719     C  N30              TESTN                   WD15                     30
068000100719     C                   MOVE      WD15          W001A             1
068100100719     C     *IN30         IFEQ      *OFF
068200100719     C     W001A         ORLT      '0'
068300100719     C     W001A         ANDNE     *BLANK
068400100719     C                   SETON                                        32
068500100719     C                   MOVE      *ZERO         VABVMD
068600100719     C                   eval      vinmsg = %trimr(vinmsg)
068700100719     C                             + ' ' + 'VHBVMD'
068800100719     C                   ELSE
068900100719     C                   MOVE      WD15          VABVMD
069000100719     C                   ENDIF
069100100719     C                   ENDIF
069200100719     C*
069300100719     C                   TESTN                   VHBANT               30
069400100719     C  N30              TESTN                   VHBANT                 30
069500100719     C  N30              TESTN                   VHBANT                   30
069600100719     C                   MOVE      VHBANT        W001A             1
069700100719     C     *IN30         IFEQ      *OFF
069800100719     C     W001A         ORLT      '0'
069900100719     C     W001A         ANDNE     *BLANK
070000100719     C                   SETON                                        32
070100100719     C                   MOVE      *ZERO         VABANT
070200100719     C                   eval      vinmsg = %trimr(vinmsg)
070300100719     C                             + ' ' + 'VHBANT'
070400100719     C                   ELSE
070500100719     C                   MOVE      VHBANT        VABANT
070600100719     C                   ENDIF
070700100719     C*
070800100719     C* CAMPI ALFABETICI:
070900100719     C                   MOVE      VHBATB        VABATB
071000100719     C                   MOVE      VHBCBO        VABCBO
071100100719     C                   MOVE      VHBRSD        VABRSD
071200100719     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071300100719     C     '@':'A'       XLATE     VABRSD        VABRSD
071400100719     C* ==
071500100719     C                   MOVE      VHBRD2        VABRD2
071600100719     C                   MOVE      VHBIND        VABIND
071700100719     C                   MOVE      VHBCAD        VABCAD
071800100719     C                   MOVE      VHBLOD        VABLOD
071900100719     C                   MOVE      VHBPRD        VABPRD
072000100719     C                   MOVE      VHBNZD        VABNZD
072100100719     C                   MOVE      VHBGC1        VABGC1
072200100719     C                   MOVE      VHBGC2        VABGC2
072300100719     C                   MOVE      VHBTSP        VABTSP
072400100719     C                   MOVE      VHBVAS        VABVAS
072500100719     C                   MOVE      VHBNAS        VABNAS
072600100719     C                   MOVE      VHBTIC        VABTIC
072700100719     C                   MOVE      VHBVCA        VABVCA
072800100719     C                   MOVE      VHBGCA        VABGCA
072900100719     C                   MOVE      VHBRMA        VABRMA
073000100719     C                   MOVE      VHBXCO        VABXCO
073100100719     C                   MOVE      VHBNOT        VABNOT
073200100719     C                   MOVE      VHBNT2        VABNT2
073300100719     C  N53              MOVE      VHBCTM        VABCTM
073400100719     C                   MOVE      VHBFFD        VABFFD
073500100719     C                   MOVE      VHBTCR        VABTCR
073600100719     C                   MOVE      VHBCTS        VABCTS
073700100719     C                   MOVE      VHBFTM        VABFTM
073800100719     C                   MOVE      VHBVAD        VABVAD
073900100719     C                   MOVE      VHBGMA        VABGMA
074000100719     C                   MOVE      VHBGGA        VABGGA
074100100719     C                   MOVE      VHBGVA        VABGVA
074200100719     C                   MOVE      VHBTC1        VABTC1
074300100719     C                   MOVE      VHBTC2        VABTC2
074400100719     C                   MOVE      VHBSCL        VABSCL
074500100719     C                   MOVE      VHBRMO        VABRMO
074600100719     C                   MOVE      VHBCMO        VABCMO
074700100719     C                   MOVE      VHBNMO        VABNMO
074800050209     C*
074900050303     C* NSP => Se nn valorizzata numero spedizione => stacco numeratore da AZNUM
075000050303     C                   if        VABNSP = *zeros
075100050209     C                   clear                   TRUL33DS
075200050209     C                   eval      I33OPE = *zeros
075300050209     C                   eval      I33CNU = 302
075400050209     C                   eval      I33NUM = 1
075500050209     C                   movel     TRUL33DS      KPJBU
075600050209     C                   call      'TRUL33R'
075700050209     C                   parm                    KPJBA
075800050209     C                   movel     KPJBU         TRUL33DS
075900050209     C                   if        O33ERR = *zeros
076000050209     C                   z-add     O33NRF        VABNSP
076100050209     C                   z-add     O33NRF        VATNSP
076200050209     C                   else
076300050209     C                   SETON                                        31
076400050209     C                   EVAL      vinmsg = %trimr(vinmsg)
076500050209     C                             + ' ' + 'VABNSP VATNSP'
076600050209     C                   endif
076700050303     C                   endif
076800100719     C*
076900100719     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077000100719     C* NELLA NAZIONE MITTENTE ORIGINALE
077100100719     C                   IF        %trim(VHBNMO) < 'A'
077200100719     C                   MOVE      *BLANKS       VABNMO
077300100719     C                   ENDIF
077400100719     C*
077500100719     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
077600100719     C                   IF        %trim(VABNZD) = 'I'   OR
077700100719     C                             %trim(VABNZD) = 'IT'  OR
077800100719     C                             %trim(VABNZD) = 'ITA'
077900100719     C                   EVAL      VABNZD = *blanks
078000100719     C                   ENDIF
078100100719     C*
078200100719     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
078300100719     C                   IF        *IN52 = *ON
078400100719     C* Reperisco la provincia dal CAP e dalla località
078500100719     C                   IF        VABCAD <> *blanks AND
078600100719     C                             VABPRD  = *blanks AND
078700100719     C                             VABNZD  = *blanks
078800100719     C                   CLEAR                   TISI95DS
078900100719     C                   EVAL      I95TCN = '3'
079000100719     C                   Z-ADD     datcor        I95DAT
079100100719     C                   EVAL      I95CAP = VABCAD
079200100719     C                   EVAL      I95LOC = VABLOD
079300100719     C                   CALL      'TISI95R'
079400100719     C                   PARM                    TISI95DS
079500100719     C                   EVAL      VABPRD = O95PRV
079600100719     C                   ENDIF
079700100719     C                   ENDIF
079800100719     C*
079900100719     C* "bonifico" i dati d input da eventuali caratteri indesiderati
080000100719     C                   exsr      exeBon
080100100719     C*
080200100719     C* Forzatura su campi DIVISE
080300100719     C                   IF        VABVAS = *blanks
080400100719     C                   EVAL      VABVAS = 'EUR'
080500100719     C                   ENDIF
080600100719     C                   IF        VABVCA = *blanks
080700100719     C                   EVAL      VABVCA = 'EUR'
080800100719     C                   ENDIF
080900100719     C                   IF        VABVAD = *blanks
081000100719     C                   EVAL      VABVAD = 'EUR'
081100100719     C                   ENDIF
081200930409     C*
081300010604     C  N31              ADD       1             §CTROK            7 0
081400010604     C   32              ADD       1             §CTRMO            7 0
081500010604     C   31              ADD       1             §CTRNO            7 0
081600020913     C  N31              WRITE     FIVAB000
081700100719     C  N31
081800100719     CAN 55              EXSR      WRIVAT
081900990910     C*
082000010601     C                   if        *in31 = *off and
082100010601     C                             *in32 = *off
082200990910     C                   eval      vinflg = '1'
082300990910     C                   else
082400990910     C                   eval      vinflg = '2'
082500990910     C                   endif
082600991022     C                   endif
082700990910     C                   endif
082800010601     C*
082900010601     C                   endif
083000000905     C*
083100000905     C                   else
083200000905     C                   eval      vinflg = '1'
083300000905     C                   endif
083400000905     C*
083500000905     C  N70              update    tivin000
083600991022     C*
083700991022     C  N70              ENDdo
083800010530     C
083900010530     C*
084000010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
084100010530     C                   if        cntNonEl = *zeros or
084200010530     C                             flgMulti = '0'
084300990910     C* Se non ci sono record con errori ...
084400010530     C                   if        §ctrno = 0 and
084500010601     C                             §ctrmo = 0 and
084600010530     C                             flgStato <> '2'
084700990910     C* ... restituisco esito OK.
084800990921     C                   eval      wrkesito = '0'
084900990910     C                   else
085000010601     C                   if        §ctrok > 0
085100990921     C                   eval      wrkesito = '1'
085200000710     C                   else
085300010615     C                   if        flgOk = '0'
085400010615     C                   eval      wrkesito = '2'
085500010615     C                   else
085600010615     C                   eval      wrkesito = '6'
085700010615     C                   endif
085800990910     C                   endif
085900000710     C                   endif
086000010530     C                   else
086100010530     C                   eval      wrkesito = '9'
086200010530     C                   endif
086300050209     C*
086400050209     C                   else
086500050209     C                   eval      wrkesito = '2'
086600050209     C                   endif
086700990910     C*
086800990914     C                   if        %open(tivin00r)
086900990908     C                   close     tivin00r
087000990914     C                   endif
087100020913     C                   if        %open(fivabwwr)
087200020913     C                   close     fivabwwr
087300990914     C                   endif
087400050209     C                   if        %open(fivatwwr)
087500050209     C                   close     fivatwwr
087600050209     C                   endif
087700010530     C*
087800010530     C                   if        vlrpoi <> 999
087900010530     C                   eval      invfil = vlrpoi
088000010530     C                   endif
088100990910     C*
088200990920     C                   if        §ctrok > 0
088300010530     C                             and invfil > *zeros
088400000613     C                   exsr      invio
088500990920     C                   endif
088600010612     C*
088700010612     C                   if        flgGiro = '1'
088800010612     C                   exsr      endela
088900010612     C                   endif
089000990920     C*
089100910830     C                   ENDSR
089200000613     C***
089300050209
089400050209
089500050209
089600050209     C*----------------------------------------------------*
089700050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
089800050209     C*----------------------------------------------------*
089900050209     C     PREVAT        BEGSR
090000050209     C*
090100050209     C* Compongo il nome del membro da dare al FIVATWWR
090200050209     C                   eval      parmbr = vlrhdl
090300050209     C                   movel     'M'           parmbr
090400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
090500050209     C                   eval      paropz = '1'
090600050209     C* Effettuo la chiamata al CLLE preposto
090700050209     C                   call(e)   'TITVVTC'
090800050209     C                   parm                    parccm
090900050209     C                   parm                    parmbr
091000050209     C                   parm                    paropz
091100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
091200050209     C                   if        %error
091300050209     C                   movel     '1'           chkcall
091400050209     C                   else
091500050209     C                   movel     '0'           chkcall
091600050209     C                   endif
091700050209     C*
091800050209     C                   ENDSR
091900050209
092000050209
092100990920
092200050209     C*----------------------------------------------------*
092300050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
092400050209     C*----------------------------------------------------*
092500050209     C     WRIVAT        BEGSR
092600050209     C*
092700050209     C* Valorizzo il buffer del file dettagli barcode colli
092800050209     C                   EVAL      VATFGS = VABFGS
092900050209     C                   EVAL      VATAAS = VABAAS
093000050209     C                   EVAL      VATLNP = VABLNP
093100050209     C                   EVAL      VATNSP = VABNSP
093200050209     C                   EVAL      VATCCM = VABCCM
093300050209     C                   EVAL      VATTRC='E'
093400050303     C*
093500100719     C* Compongo tanti "chi sono" quanti sono i colli della spedizione sulla base del RMN
093600100719     C                   z-add     10            wIdxRMN           9 0
093700100719     C                   z-add     10            wIdxRMNsav        9 0
093800100719     C     2             do        wMultRMN
093900100719     C                   eval      wIdxRMN = wIdxRMN * 10
094000100719     C                   enddo
094100100719     C*
094200100719     C                   eval      wIdxRMNsav = wIdxRMN
094300100719     C                   movel     *blanks       wVATNOT_E        35
094400100719     C*
094500100719     C                   if        %subst(vlrppt:8:1) = 'N'
094600100719     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
094700100719     C                   else
094800100719     C                   eval      wVATNOT_E = %trim(VABRMA)
094900100719     C                   endif
095000100719     C*
095100100719     C     1             do        VABNCL        wColloNum         9 0
095200100719     C                   eval      wIdxRMN = wIdxRMNsav + wColloNum
095300100719     C                   eval      VATNOT = %trim(wVATNOT_E) +
095400100719     C                                      %subst(%trim(%char(wIdxRMN)):2)
095500100719     C                   if        VATNOT <> *blanks
095600100719     C                   write     FIVAT000
095700100719     C                   endif
095800100719     C                   enddo
095900050209     C*
096000050209     C                   ENDSR
096100010530
096200010530
096300050209
096400010530
096500010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
096600010530     C     repfil        BEGSR
096700010530     C*
096800010530     C                   if        invfil = *zeros and
096900010530     C                             depfil > *zeros and
097000010613     C                             (vinflg = *blanks or
097100010613     C                              vinflg = *zeros)
097200010530     C
097300010530     C                   eval      invfil = depfil
097400010530     C                   endif
097500010530     C*
097600010530     C                   if        depfil <> invfil and
097700010530     C                             invfil > *zeros
097800010530     C                   eval      flgMulti = '1'
097900010530     C                   if        vinflg = *blanks
098000010530     C                   add       1             cntNonEl
098100010530     C                   endif
098200010530     C                   endif
098300010530     C*
098400010530     C                   if        vinflg = '2'
098500010530     C                   eval      flgStato = '2'
098600010530     C                   endif
098700010530     C*
098800010530     C                   ENDSR
098900010530     C***
099000050209
099100050209
099200050209
099300050209
099400050209      /TITLE Invio dei dati al punto operativo.
099500050209     C     invio         BEGSR
099600050209     C*
099700050209     C* 1° invio FIVAT
099800050209     C                   reset                   dscmz
099900050209     C                   move      vlrpoi        cmzdst
100000050209     C                   eval      cmzfld = 'FIVATWWR'
100100050209     C                   eval      cmzmbd = vlrhdl
100200050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
100300050209     C***                if        prmfir = *blanks
100400050209     C                   eval      cmzfla = 'FIVAT00F'
100500050209     C                   eval      cmzmba = 'FIVAT00F'
100600050209     C***                else
100700050209     C***                eval      cmzfla = prmfir
100800050209     C***                eval      cmzmba = prmfir
100900050209     C***                endif
101000050209     C                   eval      cmznrr = *zeros
101100050209     C                   move      §ctrok        cmznrr
101200050209     C                   eval      cmzlba = vlrfl1
101300050209     C                   call(e)   'TIS711C'
101400050209     C                   parm                    dscmz
101500050209     C                   parm      *blanks       esito
101600050209     C                   if        %error
101700050209     C                             or cmzerr = '1'
101800050209     C                             or esito  = '1'
101900050209     C                   eval      wrkesito = '3'
102000050209     C                   else
102100050209     C*
102200050209     C* 2° invio FIVAB
102300050209     C                   reset                   dscmz
102400050209     C                   move      vlrpoi        cmzdst
102500050209     C                   eval      cmzfld = vlrfou
102600050209     C                   eval      cmzmbd = vlrhdl
102700050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
102800050209     C***                if        prmfir = *blanks
102900050209     C                   eval      cmzfla = 'FIVAB00F'
103000050209     C                   eval      cmzmba = 'FIVAB00F'
103100050209     C***                else
103200050209     C***                eval      cmzfla = prmfir
103300050209     C***                eval      cmzmba = prmfir
103400050209     C***                endif
103500050209     C                   eval      cmznrr = *zeros
103600050209     C                   move      §ctrok        cmznrr
103700050209     C                   eval      cmzlba = vlrfl1
103800050209     C                   call(e)   'TIS711C'
103900050209     C                   parm                    dscmz
104000050209     C                   parm      *blanks       esito
104100050209     C                   if        %error
104200050209     C                             or cmzerr = '1'
104300050209     C                             or esito  = '1'
104400050209     C                   eval      wrkesito = '3'
104500050209     C                   endif
104600050209     C                   endif
104700050209     C*
104800050209     C                   ENDSR
104900050209     C***
105000010530
105100010530
105200010530
105300010530
105400010530      /TITLE Invio dei dati al punto operativo.
105500010530     C     opeini        BEGSR
105600010530     C*
105700010530     C* Inizializzo flag e contatori operativi
105800010530     C                   movel     '0'           flgGiro           1
105900010530     C                   movel     '0'           flgMulti          1
106000010530     C                   movel     '1'           flgStato          1
106100010615     C                   movel     '0'           flgOk             1
106200010530     C                   z-add     *zeros        cntNonEl         10 0
106300010530     C                   z-add     *zeros        depfil            3 0
106400010530     C                   z-add     *zeros        invfil            3 0
106500100719     C*
106600100719     C                   z-add     *zeros        wMultRMN          1 0
106700030407     C*
106800050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
106900030407     C                   if        %subst(vlrppt:1:1) = 'S'
107000030407     C                   seton                                        50
107100030407     C                   else
107200030407     C                   setoff                                       50
107300030407     C                   endif
107400041001     C*
107500050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
107600041001     C                   if        %subst(vlrppt:2:1) = 'S'
107700041001     C                   seton                                        51
107800041001     C                   else
107900041001     C                   setoff                                       51
108000041001     C                   endif
108100100719     C*
108200100719     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
108300100719     C* (naturalmente solo se nn già presente)
108400100719     C                   if        %subst(vlrppt:3:1) = 'S'
108500100719     C                   seton                                        52
108600100719     C                   else
108700100719     C                   setoff                                       52
108800100719     C                   endif
108900100719     C*
109000100719     C* Altri parametri...
109100100719     C                   setoff                                       53
109200100719     C                   if        %subst(vlrppt:4:2) <> *blanks
109300100719     C                   eval      vabctm = %subst(vlrppt:4:2)
109400100719     C                   seton                                        53
109500100719     C                   endif
109600010530     C*
109700100719     C                   setoff                                       54
109800100719     C                   if        %subst(vlrppt:6:1) <> *blanks
109900100719     C                   eval      PiDecChr = %subst(vlrppt:6:1)
110000100719     C                   seton                                        54
110100100719     C                   endif
110200100719     C*
110300100719     C                   setoff                                       55
110400100719     C                   if        %subst(vlrppt:7:1) <> *blanks
110500100719     C                   eval      PiStr=%trim(%subst(vlrppt:7:1))
110600100719     C                   exsr      CHKNUM
110700100719     C                   if        PiInt=*on
110800100719     C                   z-add     PiVal         wMultRMN
110900100719     C                   seton                                        55
111000100719     C                   endif
111100100719     C                   endif
111200100719     C*
111300100719     C                   if        *in55 = *on        and
111400100719     C                             (%subst(vlrppt:8:1) = 'N' or
111500100719     C                              %subst(vlrppt:8:1) = 'A')
111600100719     C                   else
111700100719     C                   setoff                                       55
111800100719     C                   endif
111900100719     C*
112000010530     C                   ENDSR
112100010530     C***
112200100719
112300100719
112400100719
112500100719
112600100719      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
112700100719     C     exeBon        BEGSR
112800100719     C*
112900100719     C* Imposto i parametri x il *pgm d "bonifica"
113000100719     C                   EVAL      ElencoChar = *blanks
113100100719     C                   EVAL      TipoElenco = *blanks
113200100719     C                   EVAL      CharSost   = *blanks
113300100719     C                   EVAL      UpperCase  = '1'
113400100719     C                   EVAL      ChkNull    = '1'
113500100719     C                   EVAL      CharNull   = *blanks
113600100719     C*
113700100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
113800100719     C                   EVAL      TxtInOut   = VABRSD
113900100719     C                   EVAL      EsitoBon   = *blanks
114000100719     C*
114100100719     C                   CALL      'XCHKCHAR'
114200100719     C                   PARM                    TxtInOut
114300100719     C                   PARM                    ElencoChar
114400100719     C                   PARM                    TipoElenco
114500100719     C                   PARM                    CharSost
114600100719     C                   PARM                    UpperCase
114700100719     C                   PARM                    ChkNull
114800100719     C                   PARM                    CharNull
114900100719     C                   PARM                    EsitoBon
115000100719     C*
115100100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
115200100719     C                   IF        EsitoBon = '1'
115300100719     C                   EVAL      VABRSD = TxtInOut
115400100719     C                   ENDIF
115500100719     C*
115600100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
115700100719     C                   EVAL      TxtInOut   = VABIND
115800100719     C                   EVAL      EsitoBon   = *blanks
115900100719     C*
116000100719     C                   CALL      'XCHKCHAR'
116100100719     C                   PARM                    TxtInOut
116200100719     C                   PARM                    ElencoChar
116300100719     C                   PARM                    TipoElenco
116400100719     C                   PARM                    CharSost
116500100719     C                   PARM                    UpperCase
116600100719     C                   PARM                    ChkNull
116700100719     C                   PARM                    CharNull
116800100719     C                   PARM                    EsitoBon
116900100719     C*
117000100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
117100100719     C                   IF        EsitoBon = '1'
117200100719     C                   EVAL      VABIND = TxtInOut
117300100719     C                   ENDIF
117400100719     C*
117500100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
117600100719     C                   EVAL      TxtInOut   = VABLOD
117700100719     C                   EVAL      EsitoBon   = *blanks
117800100719     C*
117900100719     C                   CALL      'XCHKCHAR'
118000100719     C                   PARM                    TxtInOut
118100100719     C                   PARM                    ElencoChar
118200100719     C                   PARM                    TipoElenco
118300100719     C                   PARM                    CharSost
118400100719     C                   PARM                    UpperCase
118500100719     C                   PARM                    ChkNull
118600100719     C                   PARM                    CharNull
118700100719     C                   PARM                    EsitoBon
118800100719     C*
118900100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
119000100719     C                   IF        EsitoBon = '1'
119100100719     C                   EVAL      VABLOD = TxtInOut
119200100719     C                   ENDIF
119300100719     C*
119400100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
119500100719     C                   EVAL      TxtInOut   = VABNOT
119600100719     C                   EVAL      EsitoBon   = *blanks
119700100719     C                   EVAL      UpperCase  = *blanks
119800100719     C*
119900100719     C                   CALL      'XCHKCHAR'
120000100719     C                   PARM                    TxtInOut
120100100719     C                   PARM                    ElencoChar
120200100719     C                   PARM                    TipoElenco
120300100719     C                   PARM                    CharSost
120400100719     C                   PARM                    UpperCase
120500100719     C                   PARM                    ChkNull
120600100719     C                   PARM                    CharNull
120700100719     C                   PARM                    EsitoBon
120800100719     C*
120900100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
121000100719     C                   IF        EsitoBon = '1'
121100100719     C                   EVAL      VABNOT = TxtInOut
121200100719     C                   ENDIF
121300100719     C*
121400100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
121500100719     C                   EVAL      TxtInOut   = VABNT2
121600100719     C                   EVAL      EsitoBon   = *blanks
121700100719     C                   EVAL      UpperCase  = *blanks
121800100719     C*
121900100719     C                   CALL      'XCHKCHAR'
122000100719     C                   PARM                    TxtInOut
122100100719     C                   PARM                    ElencoChar
122200100719     C                   PARM                    TipoElenco
122300100719     C                   PARM                    CharSost
122400100719     C                   PARM                    UpperCase
122500100719     C                   PARM                    ChkNull
122600100719     C                   PARM                    CharNull
122700100719     C                   PARM                    EsitoBon
122800100719     C*
122900100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
123000100719     C                   IF        EsitoBon = '1'
123100100719     C                   EVAL      VABNT2 = TxtInOut
123200100719     C                   ENDIF
123300100719     C*
123400100719     C                   ENDSR
123500100719
123600100719
123700100719
123800100719     C*----------------------------------------------------*
123900100719     C*  CONTROLLO NUMERICITA' CAMPI
124000100719     C*----------------------------------------------------*
124100100719     C     CHKNUM        BEGSR
124200100719     C*
124300100719     C                   IF        PiDecChr = *blanks
124400100719     C  N54              EVAL      PiDecChr = '.'
124500100719     C                   ENDIF
124600100719     C*
124700100719     C                   callp(e)  UBISNUM_Check(PiStr
124800100719     C                                          :PiDecChr
124900100719     C                                          :PiVal
125000100719     C                                          :PiNum
125100100719     C                                          :PiInt)
125200100719     C*
125300100719     C                   IF        %error
125400100719     C                   EVAL      PiInt=*off
125500100719     C                   ENDIF
125600100719     C*
125700100719     C                   ENDSR
125800100719     C***
125900100719
126000010530
126100990910
126200000613     C     *inzsr        BEGSR
126300990910     C*
126400990910     C     *entry        plist
126500990920     C                   parm                    tivlrds
126600990921     C                   parm      wrkesito      esito
126700000724     C                   parm                    prmlit
126800000710     C                   parm                    prmfir
126900010606     C*
127000010606     C* CALCOLA LA DATA CORRENTE
127100100719     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
127200100719     C                   eval      datcor = %dec(%date() : *ISO)
127300000613     C*
127400000613     C                   ENDSR
127500000613     C***
