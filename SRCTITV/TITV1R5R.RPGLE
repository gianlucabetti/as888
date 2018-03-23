000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500060307     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500090930     D curSped         s             15    INZ(*blanks)
002600090930     D depSped         s             15    INZ(*blanks)
002700000830
002800000830     D*------------------
002900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003000000830     D*------------------
003100000830     D WLBDA8          DS                  INZ
003200000830     D  G08DAT                 1      8  0
003300000830     D  G08INV                 9     16  0
003400000830     D  G08ERR                17     17
003500000830     D  G08TGI                18     22  0
003600090930
003700090930     D*------------------
003800090930     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
003900090930     D*------------------
004000090930     D* 10+3
004100090930     D                 DS
004200090930     D  WI15                   1     10
004300090930     D  WD15                  11     13
004400090930     D  WASC15                 1     13
004500090930     D*  6+1
004600090930     D                 DS
004700090930     D  WI09                   1      6
004800090930     D  WD09                   7      7
004900090930     D  WASC09                 1      7
005000090930     D*  2+3
005100090930     D                 DS
005200090930     D  WI07                   1      2
005300090930     D  WD07                   3      5
005400090930     D  WASC07                 1      5
005500090930     D*
005600090114
005700090930     D*---------------------------------------------------------
005800090930     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
005900090930     D*---------------------------------------------------------
006000090930     D                 ds
006100090930     D vhbatb                  1      1
006200090930     D vhbccm                  2      8
006300090930     D vhblnp                  9     11
006400090930     D vhbaas                 12     15
006500090930     D vhbmgs                 16     19
006600090930     D vhbnrs                 20     21
006700090930     D vhbnsp                 22     28
006800090930     D vhbcbo                 29     30
006900090930     D vhblna                 31     33
007000090930     D vhbrsd                 34     68
007100090930     D vhbrd2                 69    103
007200090930     D vhbind                104    138
007300090930     D vhbcad                139    147
007400090930     D vhblod                148    182
007500090930     D vhbprd                183    184
007600090930     D vhbnzd                185    187
007700090930     D vhbgc1                188    189
007800090930     D vhbgc2                190    191
007900090930     D vhbctr                192    194
008000090930     D vhbtsp                195    195
008100090930     D vhbias                196    208
008200090930     D vhbvas                209    211
008300090930     D vhbnas                212    226
008400090930     D vhbncl                227    231
008500090930     D vhbpkb                232    238
008600090930     D vhbvlb                239    243
008700090930     D vhbqft                244    256
008800090930     D vhbcas                257    269
008900090930     D vhbtic                270    271
009000090930     D vhbvca                272    274
009100090930     D vhbgca                275    276
009200090930     D vhbrmn                277    291
009300090930     D vhbrma                292    306
009400090930     D vhbncd                307    313
009500090930     D vhbnca                314    320
009600090930     D vhbxco                321    321
009700090930     D vhbnot                322    356
009800090930     D vhbnt2                357    391
009900090930     D vhbznc                392    393
010000090930     D vhbctm                394    395
010100090930     D vhbffd                396    396
010200090930     D vhbdcr                397    404
010300090930     D vhbtcr                405    405
010400090930     D vhbhcr                406    409
010500090930     D vhbcts                410    411
010600090930     D vhbftm                412    412
010700090930     D vhbvmd                413    425
010800090930     D vhbvad                426    428
010900090930     D vhbgma                429    430
011000090930     D vhbgga                431    432
011100090930     D vhbgva                433    434
011200090930     D vhbtc1                435    435
011300090930     D vhbtc2                436    436
011400090930     D vhbscl                437    437
011500090930     D vhbant                438    446
011600090930     D vhbrmo                447    471
011700090930     D vhbcmo                472    480
011800090930     D vhbnmo                481    483
011900090930     D  X_rec                  1    483    inz
012000090930     D*
012100081217
012200081217     D*------------------
012300081217     D* LINKING A DEFINIZIONI ESTERNE
012400081217     D*------------------
012500081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
012600081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
012700081217
012800081217
012900010201
013000081217
013100090930     C*
013200090930     C* Inizializzo flag e contatori operativi
013300090930     C                   movel     '0'           flgGiro           1
013400090930     C                   movel     '0'           flgMulti          1
013500090930     C                   movel     '1'           flgStato          1
013600090930     C                   movel     '0'           flgOk             1
013700090930     C                   z-add     *zeros        cntNonEl         10 0
013800090930     C                   z-add     *zeros        depfil            3 0
013900090930     C                   z-add     *zeros        invfil            3 0
014000090930     C*
014100000913     C                   reset                   rrnum
014200990921     C                   reset                   esito
014300990921     C                   reset                   wrkesito
014400000613     C*
014500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
014600000613     C*
014700010202     C* Effettuo la chiamata al CLLE preposto
014800090114     C                   call(e)   'TITVVTC'
014900010202     C                   parm                    parccm
015000010202     C                   parm                    parmbr
015100010202     C                   parm      '2'           paropz
015200050201     C*
015300050201     C* Effettuo lancio TISI95 solo x chiusura
015400050201     C                   CLEAR                   TISI95DS
015500050201     C                   EVAL      I95TLA = 'C'
015600050201     C                   CALL      'TISI95R'
015700050201     C                   PARM                    TISI95DS
015800000616     C*
015900000801     C
016000010201     C                   seton                                        LR
016100990908
016200000801
016300910830     C*--------------------------------------------------------
016400090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016500910830     C*--------------------------------------------------------
016600040526     C     RWFILE        BEGSR
016700990910     C*
016800990914     C                   if        not %open(tivin00r)
016900990908     C                   open      tivin00r
017000990914     C                   endif
017100090114     C                   if        not %open(fivabwwr)
017200090114     C                   open      fivabwwr
017300990914     C                   endif
017400070103     C*
017500090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017600020305     C                   exsr      prevat
017700010201     C*
017800010202     C                   if        chkcall = '0'
017900010202     C*
018000090114     C                   if        not %open(fivatwwr)
018100090114     C                   open      fivatwwr
018200010201     C                   endif
018300990910     C*
018400010201     C                   clear                   §CTROKVB          5 0
018500020305     C                   clear                   §CTROKVT          5 0
018600000801     C                   clear                   §CTRMO            5 0
018700000801     C                   clear                   §CTRNO            5 0
018800990910     C*
018900921023     C                   DO        *HIVAL
019000990913     C*
019100990915     C                   READ      tivin00r                               70
019200050627     C                   if        vindta > *blanks
019300000613     C                   add       1             rrnum
019400000801     C*
019500000801     C                   if        *in70 = *off
019600000801     C                             and
019700000801     C                             (vinflg = *blanks
019800000801     C                              or vinflg = '0'
019900000801     C                              or vinflg = '2')
020000000801     C*
020100000801     C                   clear                   vinmsg
020200000801     C                   eval      vinflg = '1'
020300070103     C*
020400070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
020500090930     C                   eval      curSped = %subst(vindta:277:15)
020600090930     C                   if        curSped <> depSped
020700070213     C* Se prima bolla => importo bolla corrente
020800091001     C                   if        §CTROKVB = *zeros
020900090309     C                   EXSR      INZVAR
021000090309     C                   EXSR      DEFCAM
021100070213     C                   exsr      impvab                                       => carico  VAB
021200070213     C                   else
021300070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
021400070213     C                   exsr      wrivab                                       => scarico VAB
021500090309     C                   EXSR      INZVAR
021600090309     C                   EXSR      DEFCAM
021700070213     C                   exsr      impvab                                       => carico  VAB
021800070213     C                   endif
021900090930     C                   exsr      exevate                                      => write VAT-E
022000070103     C* Salvo il raggruppamento spedizione cliente corrente
022100070103     C                   eval      depSped = curSped
022200070213     C*
022300070213     C* Se collo successivo x stessa bolla
022400070213     C                   else
022500070213     C                   exsr      impvab                                       => carico  VAB
022600090930     C                   exsr      exevate                                      => write VAT-E
022700070103     C                   endif
022800000905     C*
022900000905     C                   else
023000000905     C                   eval      vinflg = '1'
023100050628     C                   endif
023200000905     C                   endif
023300000905     C*
023400000905     C  N70              update    tivin000
023500000905     C*
023600991022     C  N70              ENDdo
023700070213     C*
023800070213     C* Scarico testata bolla rimasta in sospesa
023900070213     C                   exsr      wrivab                                       => scarico VAB
024000010202     C*
024100010202     C                   endif
024200990910
024300990910     C* Se non ci sono record con errori ...
024400000710     C                   if        §ctrno = 0
024500990910     C* ... restituisco esito OK.
024600990921     C                   eval      wrkesito = '0'
024700990910     C                   else
024800010201     C                   if        §ctrokvb > 0
024900990921     C                   eval      wrkesito = '1'
025000000710     C                   else
025100000710     C                   eval      wrkesito = '2'
025200990910     C                   endif
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800090114     C                   if        %open(fivabwwr)
025900090114     C                   close     fivabwwr
026000990914     C                   endif
026100090114     C                   if        %open(fivatwwr)
026200090114     C                   close     fivatwwr
026300010201     C                   endif
026400990910     C*
026500010201     C                   if        §ctrokvb > 0
026600000724     C                             and vlrpoi <> *zeros
026700010202     C                   exsr      invio
026800990920     C                   endif
026900990920     C*
027000910830     C                   ENDSR
027100000613     C***
027200010305
027300010305     C*----------------------------------------------------*
027400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027500010305     C*----------------------------------------------------*
027600020305     C     WRIVAB        BEGSR
027700010305     C*
027800060225     C* Quindi scarico il buffer del file d testata
027900090114     C                   write     fivab000                                     => scarico il VAB
028000010305     C*
028100010305     C                   ENDSR
028200990920
028300000801     C*----------------------------------------------------*
028400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028500000801     C*----------------------------------------------------*
028600010201     C     INZVAR        BEGSR
028700000801     C*
028800090114     C                   CLEAR                   FIVAB000
028900090114     C                   CLEAR                   FIVAT000
029000070213     C*
029100040802     C                   Z-ADD     *zeros        Num5_0            5 0
029200040802     C                   MOVEL     '0'           FlgCAS            1
029300000801     C*
029400000801     C                   ENDSR
029500000801     C*----------------------------------------------------*
029600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029700000801     C*----------------------------------------------------*
029800000801     C     DEFCAM        BEGSR
029900000801     C*
030000020619     C* Imposto i valori di default...
030100090930     C                   EVAL      VABCCM = 1050762
030200090930     C                   EVAL      VATCCM = 1050762
030300090930     C                   EVAL      VABLNP = 105
030400090930     C                   EVAL      VATLNP = 105
030500090930     C                   EVAL      VABNRS = 00
030600090930     C                   EVAL      VATNRS = 00
030700090930     C                   EVAL      VABCTR = 000
030800090930     C                   EVAL      VABTSP = 'D'
030900090930     C                   EVAL      VABCBO = '1'
031000090930     C                   EVAL      VABCTM = '7Q'
031100090930     C                   EVAL      VABTC1 = 'A'
031200020619     C* ... e poi verifico se sono stati passati come parametri
031300020619     C                   IF        vlrppt > *blanks
031400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
031500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020619     C                   EXSR      CHKNUM
031700020619     C                   IF        PiInt=*on
031800020619     C                   Z-ADD     PiVal         VABCCM
031900020619     C                   Z-ADD     PiVal         VATCCM
032000020619     C                   ENDIF
032100040506     C                   ENDIF
032200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
032300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032400020619     C                   EXSR      CHKNUM
032500020619     C                   IF        PiInt=*on
032600020619     C                   Z-ADD     PiVal         VABLNP
032700020619     C                   Z-ADD     PiVal         VATLNP
032800040506     C                   ENDIF
032900020619     C                   ENDIF
033000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
033100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033200020619     C                   EXSR      CHKNUM
033300020619     C                   IF        PiInt=*on
033400020619     C                   Z-ADD     PiVal         VABCTR
033500040506     C                   ENDIF
033600020619     C                   ENDIF
033700020619     C                   ENDIF
033800000801     C*
033900000801     C                   ENDSR
034000000801     C*----------------------------------------------------*
034100090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034200000801     C*----------------------------------------------------*
034300040823     C     IMPVAB        BEGSR
034400010305     C*
034500000801     C                   Z-ADD     *zeros        errore            1 0
034600000830     C                   MOVEL     datcor        VABAAS
034700020305     C                   MOVEL     datcor        VATAAS
034800040526     C                   MOVE      datcor        VABMGS
034900040823     C                   MOVE(P)   vlrpoi        VABFGS
035000040823     C                   MOVE(P)   vlrpoi        VATFGS
035100090930     C*
035200090930     C                   eval      x_rec  = vindta
035300090930     C*
035400090930     C                   testb     x'3F'         vhbatb                   02
035500090930     C                   if        *in02
035600090930     C                   seton                                        31
035700090930     C                   else
035800090930     C*
035900090930     C                   SETOFF                                       303132
036000090930     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
036100090930     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
036200090930     C*    OPPURE TUTTI BLANKS
036300090930     C*
036400110126     C*                  TESTN                   VHBLNP               30
036500110126     C* N30              TESTN                   VHBLNP                 30
036600110126     C* N30              TESTN                   VHBLNP                   30
036700110126     C*                  MOVE      VHBLNP        W001A             1
036800110126     C*    *IN30         IFEQ      *OFF
036900110126     C*    W001A         ORLT      '0'
037000110126     C*                  SETON                                        31
037100110126     C*                  eval      vinmsg = %trimr(vinmsg)
037200110126     C*                            + ' ' + 'VHBLNP'
037300110126     C*                  ELSE
037400110126     C*                  MOVE      VHBLNP        VABLNP
037500110126     C*    VABLNP        IFLE      *ZEROS
037600110126     C*                  SETON                                        31
037700110126     C*                  eval      vinmsg = %trimr(vinmsg)
037800110126     C*                            + ' ' + 'VHBLNP'
037900110126     C*                  ENDIF
038000110126     C*                  ENDIF
038100090930     C*
038200090930     C* Effettuo considerazioni x elaborazioni "multi-filiale"
038300090930     C                   eval      depfil = VABLNP
038400091001     C                   eval      invfil = VABLNP
038500090930     C                   exsr      repfil
038600090930     C                   if        depfil = invfil
038700090930     C                   if        vlrpoi = 999
038800090930     C                   MOVE(P)   invfil        VABFGS
038900090930     C                   else
039000090930     C                   MOVE(P)   vlrpoi        VABFGS
039100090930     C                   endif
039200090930     C*
039300110126     C*                  TESTN                   VHBCCM               30
039400110126     C* N30              TESTN                   VHBCCM                 30
039500110126     C* N30              TESTN                   VHBCCM                   30
039600110126     C*                  MOVE      VHBCCM        W001A             1
039700110126     C*    *IN30         IFEQ      *OFF
039800110126     C*    W001A         ORLT      '0'
039900110126     C*                  SETON                                        31
040000110126     C*                  eval      vinmsg = %trimr(vinmsg)
040100110126     C*                            + ' ' + 'VHBCCM'
040200110126     C*                  ELSE
040300110126     C*                  MOVE      VHBCCM        VABCCM
040400110126     C*    VABCCM        IFLE      *ZEROS
040500110126     C*                  SETON                                        31
040600110126     C*                  eval      vinmsg = %trimr(vinmsg)
040700110126     C*                            + ' ' + 'VHBCCM'
040800110126     C*                  ENDIF
040900110126     C*                  ENDIF
041000090930     C*
041100090930     C                   TESTN                   VHBAAS               30
041200090930     C  N30              TESTN                   VHBAAS                 30
041300090930     C  N30              TESTN                   VHBAAS                   30
041400090930     C                   MOVE      VHBAAS        W001A             1
041500090930     C     *IN30         IFEQ      *OFF
041600090930     C     W001A         ORLT      '0'
041700090930     C     W001A         ANDNE     *BLANK
041800090930     C                   SETON                                        32
041900090930     C                   MOVEL     datcor        VABAAS
042000090930     C                   eval      vinmsg = %trimr(vinmsg)
042100090930     C                             + ' ' + 'VHBAAS'
042200090930     C                   ELSE
042300090930     C                   MOVE      VHBAAS        VABAAS
042400090930     C                   ENDIF
042500090930     C*
042600090930     C                   TESTN                   VHBMGS               30
042700090930     C  N30              TESTN                   VHBMGS                 30
042800090930     C  N30              TESTN                   VHBMGS                   30
042900090930     C                   MOVE      VHBMGS        W001A             1
043000090930     C     *IN30         IFEQ      *OFF
043100090930     C     W001A         ORLT      '0'
043200090930     C     W001A         ANDNE     *BLANK
043300090930     C                   SETON                                        32
043400090930     C                   MOVE      '0101'        VABMGS
043500090930     C                   eval      vinmsg = %trimr(vinmsg)
043600090930     C                             + ' ' + 'VHBMGS'
043700090930     C                   ELSE
043800090930     C                   MOVE      VHBMGS        VABMGS
043900090930     C                   ENDIF
044000090930     C*
044100090930     C                   TESTN                   VHBNRS               30
044200090930     C  N30              TESTN                   VHBNRS                 30
044300090930     C  N30              TESTN                   VHBNRS                   30
044400090930     C                   MOVE      VHBNRS        W001A             1
044500090930     C     *IN30         IFEQ      *OFF
044600090930     C     W001A         ORLT      '0'
044700090930     C     W001A         ANDNE     *BLANK
044800090930     C                   SETON                                        32
044900090930     C                   MOVE      *ZERO         VABNRS
045000090930     C                   eval      vinmsg = %trimr(vinmsg)
045100090930     C                             + ' ' + 'VHBNRS'
045200090930     C                   ELSE
045300090930     C                   MOVE      VHBNRS        VABNRS
045400090930     C                   ENDIF
045500090930     C*
045600090930     C                   TESTN                   VHBNSP               30
045700090930     C  N30              TESTN                   VHBNSP                 30
045800090930     C  N30              TESTN                   VHBNSP                   30
045900090930     C                   MOVE      VHBNSP        W001A             1
046000090930     C     *IN30         IFEQ      *OFF
046100090930     C     W001A         ORLT      '0'
046200090930     C     W001A         ANDNE     *BLANK
046300090930     C                   SETON                                        32
046400090930     C                   MOVE      *ZERO         VABNSP
046500090930     C                   eval      vinmsg = %trimr(vinmsg)
046600090930     C                             + ' ' + 'VHBNSP'
046700090930     C                   ELSE
046800090930     C                   MOVE      VHBNSP        VABNSP
046900090930     C                   ENDIF
047000090930     C*
047100090930     C                   TESTN                   VHBLNA               30
047200090930     C  N30              TESTN                   VHBLNA                 30
047300090930     C  N30              TESTN                   VHBLNA                   30
047400090930     C                   MOVE      VHBLNA        W001A             1
047500090930     C     *IN30         IFEQ      *OFF
047600090930     C     W001A         ORLT      '0'
047700090930     C     W001A         ANDNE     *BLANK
047800090930     C                   SETON                                        32
047900090930     C                   MOVE      *ZERO         VABLNA
048000090930     C                   eval      vinmsg = %trimr(vinmsg)
048100090930     C                             + ' ' + 'VHBLNA'
048200090930     C                   ELSE
048300090930     C                   MOVE      VHBLNA        VABLNA
048400090930     C                   ENDIF
048500090930     C*
048600090930     C                   TESTN                   VHBCTR               30
048700090930     C  N30              TESTN                   VHBCTR                 30
048800090930     C  N30              TESTN                   VHBCTR                   30
048900090930     C                   MOVE      VHBCTR        W001A             1
049000090930     C     *IN30         IFEQ      *OFF
049100090930     C     W001A         ORLT      '0'
049200090930     C     W001A         ANDNE     *BLANK
049300090930     C                   SETON                                        32
049400090930     C                   MOVE      *ZERO         VABCTR
049500090930     C                   eval      vinmsg = %trimr(vinmsg)
049600090930     C                             + ' ' + 'VHBCTR'
049700090930     C                   ELSE
049800090930     C                   MOVE      VHBCTR        VABCTR
049900090930     C                   ENDIF
050000090930     C*
050100090930     C                   MOVE      *ZERO         VABIAS
050200090930     C                   MOVE      VHBIAS        WASC15
050300090930     C                   TESTN                   WI15                 30
050400090930     C  N30              TESTN                   WI15                   30
050500090930     C  N30              TESTN                   WI15                     30
050600090930     C                   MOVE      WI15          W001A             1
050700090930     C     *IN30         IFEQ      *OFF
050800090930     C     W001A         ORLT      '0'
050900090930     C     W001A         ANDNE     *BLANK
051000090930     C                   SETON                                        32
051100090930     C                   MOVE      *ZERO         VABIAS
051200090930     C                   eval      vinmsg = %trimr(vinmsg)
051300090930     C                             + ' ' + 'VHBIAS'
051400090930     C                   ELSE
051500090930     C                   MOVEL     WI15          VABIAS
051600090930     C                   TESTN                   WD15                 30
051700090930     C  N30              TESTN                   WD15                   30
051800090930     C  N30              TESTN                   WD15                     30
051900090930     C                   MOVE      WD15          W001A             1
052000090930     C     *IN30         IFEQ      *OFF
052100090930     C     W001A         ORLT      '0'
052200090930     C     W001A         ANDNE     *BLANK
052300090930     C                   SETON                                        32
052400090930     C                   MOVE      *ZERO         VABIAS
052500090930     C                   eval      vinmsg = %trimr(vinmsg)
052600090930     C                             + ' ' + 'VHBIAS'
052700090930     C                   ELSE
052800090930     C                   MOVE      WD15          VABIAS
052900090930     C                   ENDIF
053000090930     C                   ENDIF
053100090930     C*
053200090930     C                   TESTN                   VHBNCL               30
053300090930     C  N30              TESTN                   VHBNCL                 30
053400090930     C  N30              TESTN                   VHBNCL                   30
053500090930     C                   MOVE      VHBNCL        W001A             1
053600090930     C     *IN30         IFEQ      *OFF
053700090930     C     W001A         ORLT      '0'
053800090930     C                   SETON                                        32
053900090930     C                   MOVE      *ZERO         VABNCL
054000090930     C                   eval      vinmsg = %trimr(vinmsg)
054100090930     C                             + ' ' + 'VHBNCL'
054200090930     C                   ELSE
054300090930     C                   MOVE      VHBNCL        VABNCL
054400090930     C                   ENDIF
054500090930     C*
054600090930     C                   MOVE      *ZERO         VABPKB
054700090930     C                   MOVE      VHBPKB        WASC09
054800090930     C                   IF        WI09 = *blanks
054900090930     C                   EVAL      WI09 = *zeros
055000090930     C                   ENDIF
055100090930     C                   TESTN                   WI09                 30
055200090930     C  N30              TESTN                   WI09                   30
055300090930     C  N30              TESTN                   WI09                     30
055400090930     C                   MOVE      WI09          W001A             1
055500090930     C     *IN30         IFEQ      *OFF
055600090930     C     W001A         ORLT      '0'
055700090930     C                   SETON                                        32
055800090930     C                   MOVE      *ZERO         VABPKB
055900090930     C                   eval      vinmsg = %trimr(vinmsg)
056000090930     C                             + ' ' + 'VHBPKB'
056100090930     C                   ELSE
056200090930     C                   MOVEL     WI09          VABPKB
056300090930     C                   TESTN                   WD09                 30
056400090930     C  N30              TESTN                   WD09                     30
056500090930     C                   MOVE      WD09          W001A             1
056600090930     C     *IN30         IFEQ      *OFF
056700090930     C     W001A         ORLT      '0'
056800090930     C     W001A         ANDNE     *BLANK
056900090930     C                   SETON                                        32
057000090930     C                   MOVE      *ZERO         VABPKB
057100090930     C                   eval      vinmsg = %trimr(vinmsg)
057200090930     C                             + ' ' + 'VHBPKB'
057300090930     C                   ELSE
057400090930     C                   MOVE      WD09          VABPKB
057500090930     C                   ENDIF
057600090930     C                   ENDIF
057700090930     C*
057800090930     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
057900090930     C                   MOVE      *ZERO         VABVLB
058000090930     C                   MOVE      VHBVLB        WASC07
058100090930     C                   IF        WI07 = *blanks
058200090930     C                   EVAL      WI07 = *zeros
058300090930     C                   ENDIF
058400090930     C                   TESTN                   WI07                 30
058500090930     C  N30              TESTN                   WI07                   30
058600090930     C  N30              TESTN                   WI07                     30
058700090930     C                   MOVE      WI07          W001A             1
058800090930     C     *IN30         IFEQ      *OFF
058900090930     C     W001A         ORLT      '0'
059000090930     C     W001A         ANDNE     *BLANK
059100090930     C                   SETON                                        32
059200090930     C                   MOVE      *ZERO         VABVLB
059300090930     C                   eval      vinmsg = %trimr(vinmsg)
059400090930     C                             + ' ' + 'VHBVLB'
059500090930     C                   ELSE
059600090930     C                   MOVEL     WI07          VABVLB
059700090930     C                   TESTN                   WD07                 30
059800090930     C  N30              TESTN                   WD07                   30
059900090930     C  N30              TESTN                   WD07                     30
060000090930     C                   MOVE      WD07          W001A             1
060100090930     C     *IN30         IFEQ      *OFF
060200090930     C     W001A         ORLT      '0'
060300090930     C     W001A         ANDNE     *BLANK
060400090930     C                   SETON                                        32
060500090930     C                   MOVE      *ZERO         VABVLB
060600090930     C                   eval      vinmsg = %trimr(vinmsg)
060700090930     C                             + ' ' + 'VHBVLB'
060800090930     C                   ELSE
060900090930     C                   MOVE      WD07          VABVLB
061000090930     C                   ENDIF
061100090930     C                   ENDIF
061200090930     C*
061300090930     C                   MOVE      *ZERO         VABQFT
061400090930     C                   MOVE      VHBQFT        WASC15
061500090930     C                   TESTN                   WI15                 30
061600090930     C  N30              TESTN                   WI15                   30
061700090930     C  N30              TESTN                   WI15                     30
061800090930     C                   MOVE      WI15          W001A             1
061900090930     C     *IN30         IFEQ      *OFF
062000090930     C     W001A         ORLT      '0'
062100090930     C     W001A         ANDNE     *BLANK
062200090930     C                   SETON                                        32
062300090930     C                   MOVE      *ZERO         VABQFT
062400090930     C                   eval      vinmsg = %trimr(vinmsg)
062500090930     C                             + ' ' + 'VHBQFT'
062600090930     C                   ELSE
062700090930     C                   MOVEL     WI15          VABQFT
062800090930     C                   TESTN                   WD15                 30
062900090930     C  N30              TESTN                   WD15                   30
063000090930     C  N30              TESTN                   WD15                     30
063100090930     C                   MOVE      WD15          W001A             1
063200090930     C     *IN30         IFEQ      *OFF
063300090930     C     W001A         ORLT      '0'
063400090930     C     W001A         ANDNE     *BLANK
063500090930     C                   SETON                                        32
063600090930     C                   MOVE      *ZERO         VABQFT
063700090930     C                   eval      vinmsg = %trimr(vinmsg)
063800090930     C                             + ' ' + 'VHBQFT'
063900090930     C                   ELSE
064000090930     C                   MOVE      WD15          VABQFT
064100090930     C                   ENDIF
064200090930     C                   ENDIF
064300090930     C*
064400090930     C                   MOVE      *ZERO         VABCAS
064500090930     C                   MOVE      VHBCAS        WASC15
064600090930     C                   TESTN                   WI15                 30
064700090930     C  N30              TESTN                   WI15                   30
064800090930     C  N30              TESTN                   WI15                     30
064900090930     C                   MOVE      WI15          W001A             1
065000090930     C     *IN30         IFEQ      *OFF
065100090930     C     W001A         ORLT      '0'
065200090930     C     W001A         ANDNE     *BLANK
065300090930     C                   SETON                                        32
065400090930     C                   MOVE      *ZERO         VABCAS
065500090930     C                   eval      vinmsg = %trimr(vinmsg)
065600090930     C                             + ' ' + 'VHBCAS'
065700090930     C                   ELSE
065800090930     C                   MOVEL     WI15          VABCAS
065900090930     C                   TESTN                   WD15                 30
066000090930     C  N30              TESTN                   WD15                   30
066100090930     C  N30              TESTN                   WD15                     30
066200090930     C                   MOVE      WD15          W001A             1
066300090930     C     *IN30         IFEQ      *OFF
066400090930     C     W001A         ORLT      '0'
066500090930     C     W001A         ANDNE     *BLANK
066600090930     C                   SETON                                        32
066700090930     C                   MOVE      *ZERO         VABCAS
066800090930     C                   eval      vinmsg = %trimr(vinmsg)
066900090930     C                             + ' ' + 'VHBCAS'
067000090930     C                   ELSE
067100090930     C                   MOVE      WD15          VABCAS
067200090930     C                   ENDIF
067300090930     C                   ENDIF
067400090930     C*
067500090930     C                   TESTN                   VHBRMN               30
067600090930     C  N30              TESTN                   VHBRMN                 30
067700090930     C  N30              TESTN                   VHBRMN                   30
067800090930     C                   MOVE      VHBRMN        W001A             1
067900090930     C     *IN30         IFEQ      *OFF
068000090930     C     W001A         ORLT      '0'
068100090930     C                   SETON                                        32
068200090930     C                   MOVE      *ZERO         VABRMN
068300090930     C                   eval      vinmsg = %trimr(vinmsg)
068400090930     C                             + ' ' + 'VHBRMN'
068500090930     C                   ELSE
068600090930     C                   MOVE      VHBRMN        VABRMN
068700090930     C                   ENDIF
068800090930     C*
068900090930     C                   TESTN                   VHBNCD               30
069000090930     C  N30              TESTN                   VHBNCD                 30
069100090930     C  N30              TESTN                   VHBNCD                   30
069200090930     C                   MOVE      VHBNCD        W001A             1
069300090930     C     *IN30         IFEQ      *OFF
069400090930     C     W001A         ORLT      '0'
069500090930     C     W001A         ANDNE     *BLANK
069600090930     C                   SETON                                        32
069700090930     C                   MOVE      *ZERO         VABNCD
069800090930     C                   eval      vinmsg = %trimr(vinmsg)
069900090930     C                             + ' ' + 'VHBNCD'
070000090930     C                   ELSE
070100090930     C                   MOVE      VHBNCD        VABNCD
070200090930     C                   ENDIF
070300090930     C*
070400090930     C                   TESTN                   VHBNCA               30
070500090930     C  N30              TESTN                   VHBNCA                 30
070600090930     C  N30              TESTN                   VHBNCA                   30
070700090930     C                   MOVE      VHBNCA        W001A             1
070800090930     C     *IN30         IFEQ      *OFF
070900090930     C     W001A         ORLT      '0'
071000090930     C     W001A         ANDNE     *BLANK
071100090930     C                   SETON                                        32
071200090930     C                   MOVE      *ZERO         VABNCA
071300090930     C                   eval      vinmsg = %trimr(vinmsg)
071400090930     C                             + ' ' + 'VHBNCA'
071500090930     C                   ELSE
071600090930     C                   MOVE      VHBNCA        VABNCA
071700090930     C                   ENDIF
071800090930     C*
071900090930     C                   TESTN                   VHBZNC               30
072000090930     C  N30              TESTN                   VHBZNC                 30
072100090930     C  N30              TESTN                   VHBZNC                   30
072200090930     C                   MOVE      VHBZNC        W001A             1
072300090930     C     *IN30         IFEQ      *OFF
072400090930     C     W001A         ORLT      '0'
072500090930     C     W001A         ANDNE     *BLANK
072600090930     C                   SETON                                        32
072700090930     C                   MOVE      *ZERO         VABZNC
072800090930     C                   eval      vinmsg = %trimr(vinmsg)
072900090930     C                             + ' ' + 'VHBZNC'
073000090930     C                   ELSE
073100090930     C                   MOVE      VHBZNC        VABZNC
073200090930     C                   ENDIF
073300090930     C*
073400090930     C                   TESTN                   VHBDCR               30
073500090930     C  N30              TESTN                   VHBDCR                 30
073600090930     C  N30              TESTN                   VHBDCR                   30
073700090930     C                   MOVE      VHBDCR        W001A             1
073800090930     C     *IN30         IFEQ      *OFF
073900090930     C     W001A         ORLT      '0'
074000090930     C     W001A         ANDNE     *BLANK
074100090930     C                   SETON                                        32
074200090930     C                   MOVE      *ZERO         VABDCR
074300090930     C                   eval      vinmsg = %trimr(vinmsg)
074400090930     C                             + ' ' + 'VHBDCR'
074500090930     C                   ELSE
074600090930     C                   MOVE      VHBDCR        VABDCR
074700090930     C                   ENDIF
074800090930     C*
074900090930     C                   TESTN                   VHBHCR               30
075000090930     C  N30              TESTN                   VHBHCR                 30
075100090930     C  N30              TESTN                   VHBHCR                   30
075200090930     C                   MOVE      VHBHCR        W001A             1
075300090930     C     *IN30         IFEQ      *OFF
075400090930     C     W001A         ORLT      '0'
075500090930     C     W001A         ANDNE     *BLANK
075600090930     C                   SETON                                        32
075700090930     C                   MOVE      *ZERO         VABHCR
075800090930     C                   eval      vinmsg = %trimr(vinmsg)
075900090930     C                             + ' ' + 'VHBHCR'
076000090930     C                   ELSE
076100090930     C                   MOVE      VHBHCR        VABHCR
076200090930     C                   ENDIF
076300090930     C*
076400090930     C                   MOVE      *ZERO         VABVMD
076500090930     C                   MOVE      VHBVMD        WASC15
076600090930     C                   TESTN                   WI15                 30
076700090930     C  N30              TESTN                   WI15                   30
076800090930     C  N30              TESTN                   WI15                     30
076900090930     C                   MOVE      WI15          W001A             1
077000090930     C     *IN30         IFEQ      *OFF
077100090930     C     W001A         ORLT      '0'
077200090930     C     W001A         ANDNE     *BLANK
077300090930     C                   SETON                                        32
077400090930     C                   MOVE      *ZERO         VABVMD
077500090930     C                   eval      vinmsg = %trimr(vinmsg)
077600090930     C                             + ' ' + 'VHBVMD'
077700090930     C                   ELSE
077800090930     C                   MOVEL     WI15          VABVMD
077900090930     C                   TESTN                   WD15                 30
078000090930     C  N30              TESTN                   WD15                   30
078100090930     C  N30              TESTN                   WD15                     30
078200090930     C                   MOVE      WD15          W001A             1
078300090930     C     *IN30         IFEQ      *OFF
078400090930     C     W001A         ORLT      '0'
078500090930     C     W001A         ANDNE     *BLANK
078600090930     C                   SETON                                        32
078700090930     C                   MOVE      *ZERO         VABVMD
078800090930     C                   eval      vinmsg = %trimr(vinmsg)
078900090930     C                             + ' ' + 'VHBVMD'
079000090930     C                   ELSE
079100090930     C                   MOVE      WD15          VABVMD
079200090930     C                   ENDIF
079300090930     C                   ENDIF
079400090930     C*
079500090930     C                   TESTN                   VHBANT               30
079600090930     C  N30              TESTN                   VHBANT                 30
079700090930     C  N30              TESTN                   VHBANT                   30
079800090930     C                   MOVE      VHBANT        W001A             1
079900090930     C     *IN30         IFEQ      *OFF
080000090930     C     W001A         ORLT      '0'
080100090930     C     W001A         ANDNE     *BLANK
080200090930     C                   SETON                                        32
080300090930     C                   MOVE      *ZERO         VABANT
080400090930     C                   eval      vinmsg = %trimr(vinmsg)
080500090930     C                             + ' ' + 'VHBANT'
080600090930     C                   ELSE
080700090930     C                   MOVE      VHBANT        VABANT
080800090930     C                   ENDIF
080900090930     C*
081000090930     C* CAMPI ALFABETICI:
081100090930     C                   MOVE      VHBATB        VABATB
081200090930     C                   MOVE      VHBCBO        VABCBO
081300090930     C                   MOVE      VHBRSD        VABRSD
081400090930     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081500090930     C     '@':'A'       XLATE     VABRSD        VABRSD
081600090930     C* ==
081700090930     C                   MOVE      VHBRD2        VABRD2
081800090930     C                   MOVE      VHBIND        VABIND
081900090930     C                   MOVE      VHBCAD        VABCAD
082000090930     C                   MOVE      VHBLOD        VABLOD
082100090930     C                   MOVE      VHBPRD        VABPRD
082200090930     C                   MOVE      VHBNZD        VABNZD
082300090930     C                   MOVE      VHBGC1        VABGC1
082400090930     C                   MOVE      VHBGC2        VABGC2
082500091105     C***                MOVE      VHBTSP        VABTSP
082600090930     C                   MOVE      VHBVAS        VABVAS
082700090930     C                   MOVE      VHBNAS        VABNAS
082800090930     C                   MOVE      VHBTIC        VABTIC
082900090930     C                   MOVE      VHBVCA        VABVCA
083000090930     C                   MOVE      VHBGCA        VABGCA
083100090930     C                   MOVE      VHBRMA        VABRMA
083200090930     C                   MOVE      VHBXCO        VABXCO
083300090930     C                   MOVE      VHBNOT        VABNOT
083400090930     C                   MOVE      VHBNT2        VABNT2
083500090930     C                   MOVE      VHBCTM        VABCTM
083600090930     C                   MOVE      VHBFFD        VABFFD
083700090930     C                   MOVE      VHBTCR        VABTCR
083800090930     C                   MOVE      VHBCTS        VABCTS
083900090930     C                   MOVE      VHBFTM        VABFTM
084000090930     C                   MOVE      VHBVAD        VABVAD
084100090930     C                   MOVE      VHBGMA        VABGMA
084200090930     C                   MOVE      VHBGGA        VABGGA
084300090930     C                   MOVE      VHBGVA        VABGVA
084400090930     C                   MOVE      VHBTC1        VABTC1
084500090930     C                   MOVE      VHBTC2        VABTC2
084600090930     C                   MOVE      VHBSCL        VABSCL
084700090930     C                   MOVE      VHBRMO        VABRMO
084800090930     C                   MOVE      VHBCMO        VABCMO
084900090930     C                   MOVE      VHBNMO        VABNMO
085000090930     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
085100090930     C* NELLA NAZIONE MITTENTE ORIGINALE
085200090930     C                   IF        %trim(VHBNMO) < 'A'
085300090930     C                   MOVE      *BLANKS       VABNMO
085400090930     C                   ENDIF
085500090930     C*
085600090930     C* Apporto le personalizzazioni cliente
085700110126     C***                EVAL      VABCCM = 1050762
085800110126     C***                EVAL      VATCCM = 1050762
085900110126     C***                EVAL      VABLNP = 105
086000110126     C***                EVAL      VATLNP = 105
086100090930     C                   EVAL      VABNRS = 00
086200090930     C                   EVAL      VATNRS = 00
086300090930     C                   EVAL      VABCTR = 000
086400090930     C                   EVAL      VABTSP = 'D'
086500090930     C                   EVAL      VABCBO = '1'
086600090930     C                   EVAL      VABCTM = '7Q'
086700090930     C                   Z-ADD     VABRMN        VABNSP
086800100215     C                   Z-ADD     VABRMN        VATNSP
086900091105     C                   IF        %subst(vindta:195:1) = '1'
087000091105     C                   EVAL      VABTC1 = 'A'
087100091105     C                   ENDIF
087200090930     C*
087300090930     C* Considerazioni sul contenuto di campi precedentemente valorizzati
087400090930     C                   IF        VABCAS  > *zeros
087500090930     C                   IF        VABCBO = '1'
087600090930     C                   EVAL      VABCBO = '4'
087700090930     C                   ENDIF
087800090930     C                   IF        VABCBO = '2'
087900090930     C                   EVAL      VABCBO = '6'
088000090930     C                   ENDIF
088100090930     C                   ENDIF
088200090930     C*
088300090930     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
088400090930     C                   IF        %trim(VABNZD) = 'I'   OR
088500090930     C                             %trim(VABNZD) = 'IT'  OR
088600090930     C                             %trim(VABNZD) = 'ITA'
088700090930     C                   EVAL      VABNZD = *blanks
088800090930     C                   ENDIF
088900090930     C*
089000090930     C* Reperisco la provincia dal CAP e dalla località
089100090930     C                   IF        VABCAD <> *blanks AND
089200090930     C                             VABPRD  = *blanks AND
089300090930     C                             VABNZD  = *blanks
089400090930     C                   CLEAR                   TISI95DS
089500090930     C                   EVAL      I95TCN = '3'
089600090930     C                   Z-ADD     datcor        I95DAT
089700090930     C                   EVAL      I95CAP = VABCAD
089800090930     C                   EVAL      I95LOC = VABLOD
089900090930     C                   CALL      'TISI95R'
090000090930     C                   PARM                    TISI95DS
090100090930     C                   EVAL      VABPRD = O95PRV
090200090930     C                   ENDIF
090300071121     C*
090400080415     C* Carico l'estensioni A e B del FNVAT
090500090930     C                   exsr      exevata
090600090930     C                   exsr      exevatb
090700020305     C*
090800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
090900011113     C                   EXSR      CHKIMPDIV
091000090930     C*
091100090930     C                   ENDIF
091200090930     C                   ENDIF
091300010202     C*
091400000801     C* Ebbene...
091500091001     C   32              ADD       1             §CTRMO
091600090930     C                   IF        *IN31
091700000801     C                   ADD       1             §CTRNO
091800000801     C                   EVAL      vinflg = '2'
091900000801     C                   ELSE
092000010201     C                   ADD       1             §CTROKVB
092100000801     C                   ENDIF
092200000801     C*
092300000801     C                   ENDSR
092400070102     C*----------------------------------------------------*
092500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
092600070102     C*----------------------------------------------------*
092700070103     C     EXEVATE       BEGSR
092800070102     C*
092900090428     C*
093000090930     C                   exsr      wrivat                                       => scarico VAT
093100070102     C*
093200070102     C                   ENDSR
093300080415     C*----------------------------------------------------*
093400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
093500080415     C*----------------------------------------------------*
093600080415     C     EXEVATA       BEGSR
093700080415     C*
093800090504     C                   EVAL      VATTRC = 'A'
093900090930     C                   EVAL      VATNOT = %trim(%subst(vindta:534:20))
094000080415     C*
094100090922     C                   exsr      wrivat                                       => scarico VAT
094200080415     C*
094300080415     C                   ENDSR
094400071121     C*----------------------------------------------------*
094500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
094600071121     C*----------------------------------------------------*
094700071121     C     EXEVATB       BEGSR
094800071121     C*
094900090504     C                   EVAL      VATTRC = 'B'
095000090930     C                   EVAL      VATNOT = %trim(%subst(vindta:554:20))
095100071121     C*
095200090922     C                   exsr      wrivat                                       => scarico VAT
095300071121     C*
095400071121     C                   ENDSR
095500010201     C*----------------------------------------------------*
095600090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
095700010201     C*----------------------------------------------------*
095800020305     C     WRIVAT        BEGSR
095900050628     C*
096000060223     C* Scrivo solo se valorizzato qualcosa
096100060223     C                   IF        VATNOT <> *blanks
096200090922     C                   WRITE     FIVAT000
096300090922     C                   ADD       1             §ctrokvt
096400060223     C                   ENDIF
096500010201     C*
096600010201     C                   ENDSR
096700010202     C*----------------------------------------------------*
096800090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
096900010202     C*----------------------------------------------------*
097000020305     C     PREVAT        BEGSR
097100010202     C*
097200090114     C* Compongo il nome del membro da dare al FIVATWWR
097300010202     C                   eval      parmbr = vlrhdl
097400010202     C                   movel     'M'           parmbr
097500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
097600010202     C                   eval      paropz = '1'
097700010202     C* Effettuo la chiamata al CLLE preposto
097800090114     C                   call(e)   'TITVVTC'
097900010202     C                   parm                    parccm
098000010202     C                   parm                    parmbr
098100010202     C                   parm                    paropz
098200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
098300010202     C                   if        %error
098400010202     C                   movel     '1'           chkcall
098500010202     C                   else
098600010202     C                   movel     '0'           chkcall
098700010202     C                   endif
098800010202     C*
098900010202     C                   ENDSR
099000000801     C*----------------------------------------------------*
099100090930     C*  CONTROLLO NUMERICITA' CAMPI
099200000801     C*----------------------------------------------------*
099300000801     C     CHKNUM        BEGSR
099400081217     C*
099500081217     C                   IF        PiDecChr = *blanks
099600090922     C                   EVAL      PiDecChr = '.'
099700081217     C                   ENDIF
099800081217     C*
099900081217     C                   callp(e)  UBISNUM_Check(PiStr
100000081217     C                                          :PiDecChr
100100081217     C                                          :PiVal
100200081217     C                                          :PiNum
100300081217     C                                          :PiInt)
100400081217     C*
100500081217     C                   IF        %error
100600081217     C                   EVAL      PiInt=*off
100700081217     C                   ENDIF
100800000801     C*
100900000801     C                   ENDSR
101000000801     C***
101100000801
101200011113
101300011113     C*----------------------------------------------------*
101400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
101500011113     C*----------------------------------------------------*
101600011113     C     CHKIMPDIV     BEGSR
101700011113     C*
101800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
101900011113     C                   Z-ADD     *zeros        wrkDec            9 9
102000011113     C*
102100011113     C* Come prima cosa effettuo considerazioni sulla divisa
102200011113     C                   IF        vabIAS > *zeros
102300011113     C                   IF        vabVAS <> 'EUR'
102400011113     C                   EVAL      vabVAS =  'ITL'
102500011113     C                   ENDIF
102600011113     C                   ENDIF
102700011113     C*
102800011113     C                   IF        vabCAS > *zeros
102900011113     C                   IF        vabVCA <> 'EUR'
103000011113     C                   EVAL      vabVCA =  'ITL'
103100011113     C                   ENDIF
103200011113     C                   ENDIF
103300011113     C*
103400011113     C                   IF        vabVMD > *zeros
103500020305     C                   IF        vabVAD <> 'EUR'
103600011113     C                   EVAL      vabVAD =  'ITL'
103700011113     C                   ENDIF
103800011113     C                   ENDIF
103900011113     C*
104000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
104100011113     C                   Z-ADD     vabIAS        wrkDec
104200011113     C                   IF        wrkDec > *zeros
104300011113     C                   IF        vabVAS = 'ITL'
104400011113     C                   EVAL      vabIAS = *zeros
104500011113     C                   ENDIF
104600011113     C                   ENDIF
104700011113     C*
104800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
104900011113     C                   Z-ADD     vabCAS        wrkDec
105000011113     C                   IF        wrkDec > *zeros
105100011113     C                   IF        vabVCA = 'ITL'
105200011113     C                   EVAL      vabCAS = *zeros
105300011113     C                   ENDIF
105400011113     C                   ENDIF
105500011113     C*
105600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
105700011113     C                   Z-ADD     vabVMD        wrkDec
105800011113     C                   IF        wrkDec > *zeros
105900011113     C                   IF        vabVAD = 'ITL'
106000011113     C                   EVAL      vabVMD = *zeros
106100011113     C                   ENDIF
106200011113     C                   ENDIF
106300011113     C*
106400011113     C                   ENDSR
106500011113     C***
106600011113
106700011113
106800000801
106900000801
107000990920      /TITLE Invio dei dati al punto operativo.
107100010202     C     invio         BEGSR
107200990920     C*
107300090114     C* 1° invio FIVAT
107400010201     C                   reset                   dscmz
107500010201     C                   move      vlrpoi        cmzdst
107600090114     C                   eval      cmzfld = 'FIVATWWR'
107700010201     C                   eval      cmzmbd = vlrhdl
107800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
107900021009     C***                if        prmfir = *blanks
108000090114     C                   eval      cmzfla = 'FIVAT00F'
108100090114     C                   eval      cmzmba = 'FIVAT00F'
108200021009     C***                else
108300021009     C***                eval      cmzfla = prmfir
108400021009     C***                eval      cmzmba = prmfir
108500021009     C***                endif
108600010201     C                   eval      cmznrr = *zeros
108700020305     C                   move      §ctrokvt      cmznrr
108800021018     C                   eval      cmzlba = vlrfl1
108900010201     C                   call(e)   'TIS711C'
109000010201     C                   parm                    dscmz
109100010201     C                   parm      *blanks       esito
109200010205     C                   if        %error
109300010205     C                             or cmzerr = '1'
109400010205     C                             or esito  = '1'
109500010205     C                   eval      wrkesito = '3'
109600010205     C                   else
109700010201     C*
109800090114     C* 2° invio FIVAB
109900010201     C                   reset                   dscmz
110000010201     C                   move      vlrpoi        cmzdst
110100010201     C                   eval      cmzfld = vlrfou
110200010201     C                   eval      cmzmbd = vlrhdl
110300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
110400021009     C***                if        prmfir = *blanks
110500090114     C                   eval      cmzfla = 'FIVAB00F'
110600090114     C                   eval      cmzmba = 'FIVAB00F'
110700021009     C***                else
110800021009     C***                eval      cmzfla = prmfir
110900021009     C***                eval      cmzmba = prmfir
111000021009     C***                endif
111100010201     C                   eval      cmznrr = *zeros
111200010201     C                   move      §ctrokvb      cmznrr
111300021018     C                   eval      cmzlba = vlrfl1
111400010201     C                   call(e)   'TIS711C'
111500010201     C                   parm                    dscmz
111600010201     C                   parm      *blanks       esito
111700010201     C                   if        %error
111800010201     C                             or cmzerr = '1'
111900010201     C                             or esito  = '1'
112000010201     C                   eval      wrkesito = '3'
112100010201     C                   endif
112200010205     C                   endif
112300990920     C*
112400000613     C                   ENDSR
112500000613     C***
112600070411
112700090331
112800090331
112900090331
113000070411     C     *pssr         BEGSR
113100070411     C*
113200070411     C                   if        %open(tivin00r)
113300070411     C                   close     tivin00r
113400070411     C                   endif
113500090114     C                   if        %open(fivabwwr)
113600090114     C                   close     fivabwwr
113700070411     C                   endif
113800090114     C                   if        %open(fivatwwr)
113900090114     C                   close     fivatwwr
114000070411     C                   endif
114100070411     C*
114200070411     C* Effettuo la chiamata al CLLE preposto
114300090114     C                   call(e)   'TITVVTC'
114400070411     C                   parm                    parccm
114500070411     C                   parm                    parmbr
114600070411     C                   parm      '2'           paropz
114700070411     C*
114800070411     C                   eval      wrkesito = '2'
114900070411     C*
115000070411     C                   seton                                        LR
115100070411     C*
115200070411     C                   ENDSR     '*CANCL'
115300070411     C***
115400070411
115500090930
115600090930
115700090930      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
115800090930     C     repfil        BEGSR
115900090930     C*
116000090930     C                   if        invfil = *zeros and
116100090930     C                             depfil > *zeros and
116200090930     C                             (vinflg = *blanks or
116300090930     C                              vinflg = *zeros)
116400090930     C
116500090930     C                   eval      invfil = depfil
116600090930     C                   endif
116700090930     C*
116800090930     C                   if        depfil <> invfil and
116900090930     C                             invfil > *zeros
117000090930     C                   eval      flgMulti = '1'
117100090930     C                   if        vinflg = *blanks
117200090930     C                   add       1             cntNonEl
117300090930     C                   endif
117400090930     C                   endif
117500090930     C*
117600090930     C                   if        vinflg = '2'
117700090930     C                   eval      flgStato = '2'
117800090930     C                   endif
117900090930     C*
118000090930     C                   ENDSR
118100090930     C***
118200090930
118300090930
118400990910
118500000613     C     *inzsr        BEGSR
118600990910     C*
118700990910     C     *entry        plist
118800990920     C                   parm                    tivlrds
118900990921     C                   parm      wrkesito      esito
119000000724     C                   parm                    prmlit
119100000710     C                   parm                    prmfir
119200000613     C*
119300000830     C* CALCOLA LA DATA CORRENTE
119400000830     C                   time                    wn14             14 0
119500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
119600080512     C                   movel     wn14          oracor            6 0          *ORA
119700000830     C                   z-add     wn8           g08dat
119800000830     C                   z-add     *zeros        g08inv
119900000830     C                   movel     '0'           g08err
120000000830     C                   call      'XSRDA8'
120100000830     C                   parm                    wlbda8
120200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
120300000830     C*
120400000613     C                   ENDSR
120500000613     C***
