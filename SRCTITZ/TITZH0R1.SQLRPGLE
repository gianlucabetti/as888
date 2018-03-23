000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400151019      * - ogni volta che viene scritto un rcd di VAT con TRC=I devo anche scrivere un rcd con TRC=S
000500151019      *   e i flag corrispondenti a NN
000600130306      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000021113     FFIVABwwr  O    E             DISK    usropn
001100021113     FFIVATwwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600150812     D depspe          s             10    INZ(*blanks)
002700150812     D curspe          s             10    INZ(*blanks)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003300070530     D Num5_0          s              5  0
003400121011     D w70             s             70
003500151105     D w8              s              8
003600000830
003700020725
003800020725     D*------------------
003900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004000020725     D*------------------
004100070530     D INPUT_DS        DS                  INZ
004200101007     D  VINDTA                     2048
004300101007     D  VINFLG                        1
004400150812     D  VINSPE                       10
004500101007     D  VINRRN                        8  0
004600020725     D*
004700080923     D*------------------
004800080923     D* DS REPERIMENTO NUMERATORE
004900080923     D*------------------
005000080923     D trul33ds      e ds                  inz
005100080923     D*------------------
005200080923     D* DS ARCHITETTURA
005300080923     D*------------------
005400080923     D kpjba         e ds                  inz
005500080923     D*------------------
005600121126     D BarcodeDS       DS           770    INZ
005700121126     D  wBarcode                     14    dim(55)
005800121109     D*------------------
005900121109     D TRUL28DSX     e ds                  inz
006000121109     D*------------------
006100121109     D wX              s              3s 0
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000613     C*
008300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008400000613     C*
008500010202     C* Effettuo la chiamata al CLLE preposto
008600050128     C                   call(e)   'TITVVTC'
008700010202     C                   parm                    parccm
008800010202     C                   parm                    parmbr
008900010202     C                   parm      '2'           paropz
009000070730     C*
009100070730     C* Effettuo lancio TISI95 solo x chiusura
009200070730     C                   CLEAR                   TISI95DS
009300070730     C                   EVAL      I95TLA = 'C'
009400070730     C                   CALL      'TISI95R'
009500070730     C                   PARM                    TISI95DS
009600000616     C*
009700000801     C
009800010201     C                   seton                                        LR
009900000801
010000910830     C*--------------------------------------------------------
010100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010200910830     C*--------------------------------------------------------
010300070530     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800021113     C                   if        not %open(fivabwwr)
010900021113     C                   open      fivabwwr
011000990914     C                   endif
011100070530     C*
011200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011300020305     C                   exsr      prevat
011400010201     C*
011500010202     C                   if        chkcall = '0'
011600010202     C*
011700021113     C                   if        not %open(fivatwwr)
011800021113     C                   open      fivatwwr
011900010201     C                   endif
012000080117     C*
012100080117     C                   EXSR      INZVAR
012200080117     C                   EXSR      DEFCAM
012300990910     C*
012400010201     C                   clear                   §CTROKVB          5 0
012500020305     C                   clear                   §CTROKVT          5 0
012600000801     C                   clear                   §CTRMO            5 0
012700000801     C                   clear                   §CTRNO            5 0
012800990910     C*
012900020725     C/EXEC SQL
013000020725     C+ declare C1 cursor for select
013100150812     C+ vindta, vinflg, substr(vindta, 2, 10) as sped, rrn(tivin00r)
013200060223     C+ from tivin00r
013300060223     C+ order by sped
013400020725     C+ for read only
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800020725     C+ open C1
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200070530     C+ Fetch C1 into :INPUT_DS
014300020725     C/END-EXEC
014400020725     C*
014500020725     C                   dow       sqlcod = *zeros
014600990913     C*
014700020725     C                   if        vindta > *blanks
014800000613     C                   add       1             rrnum
014900000801     C*
015000020725     C                   if        vinflg = *blanks
015100020725     C                             or vinflg = '0'
015200020725     C                             or vinflg = '2'
015300000801     C*
015400020725     C                   clear                   x_vinmsg
015500020725     C                   eval      x_vinflg = '1'
015600010305     C*
015700121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015800151015     C* (uso l'RMN/RMA/NSP)
015900151015     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
016000020305     C                   MOVEL(p)  PiStr         curspe
016100010305     C*
016200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016300080923     C                   if        *in50 = *off
016400071003     C                   exsr      impvab
016500071003     C                   exsr      wrivab
016600150812     C                   exsr      wrivat_b                                     => carico VAT
016700150812     C                   exsr      wrivat_ij                                    => carico VAT
016800071003     C                   else
016900080923     C*
017000071009     C                   if        wDISK = 'D'
017100071009     C                   exsr      impvab
017200071009     C                   exsr      wrivab
017300150812     C                   exsr      wrivat_b                                     => carico VAT
017400150812     C                   exsr      wrivat_ij                                    => carico VAT
017500130306     C***                exsr      wrivat_e                                     => carico VAT
017600071009     C                   else
017700071009     C*
017800010305     C                   if        depspe = *blanks                             => 1° giro
017900010305     C                   eval      depspe = curspe                              => memorizz. spediz
018000080117     C                   clear                   tivinds
018100020305     C                   exsr      impvab
018200150812     C                   exsr      wrivat_b                                     => carico VAT
018300150812     C                   exsr      wrivat_ij                                    => carico VAT
018400130306     C***50              exsr      wrivat_e                                     => carico VAT
018500010305     C                   else
018600020725     C                   if        depspe <> curspe                             => rottura di spediz
018700010305     C                   eval      depspe = curspe                              => memorizz. spediz
018800070928     C                   exsr      wrivab
018900080117     C                   clear                   tivinds
019000020305     C                   exsr      impvab
019100150812     C                   exsr      wrivat_b                                     => carico VAT
019200150812     C                   exsr      wrivat_ij                                    => carico VAT
019300130306     C***50              exsr      wrivat_e                                     => carico VAT
019400020305     C                   else                                                   => x stessa spediz
019500090204     C***                exsr      impvab
019600130723     C***                exsr      wrivat_b                                     => carico VAT
019700150812     C***                exsr      wrivat_ij                                    => carico VAT
019800130306     C***50              exsr      wrivat_e                                     => carico VAT
019900010305     C                   endif
020000010305     C                   endif
020100010305     C                   endif
020200071003     C                   endif
020300071009     C                   endif
020400000905     C*
020500000905     C                   else
020600020725     C                   eval      x_vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900020725     C     VINRRN        chain     tivin000
021000020725     C                   if        %found(tivin00r)
021100020725     C                   eval      y_vinflg = x_vinflg
021200020725     C                   eval      y_vinmsg = x_vinmsg
021300020725     C                   update    tivin000
021400020725     C                   endif
021500020725     C*
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   enddo
022100020725     C*
022200020725     C/EXEC SQL
022300020725     C+ close C1
022400020725     C/END-EXEC
022500000905     C*
022600020305     C* Scarico i VAB rimasti "in sospeso"
022700071009     C                   if        wDISK = 'C'
022800071009     C                   exsr      wrivab
022900071009     C                   endif
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200010305
026300010305     C*----------------------------------------------------*
026400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026500010305     C*----------------------------------------------------*
026600020305     C     WRIVAB        BEGSR
026700070730     C*
026800071003     C* Considerazioni finali
026900071003     C                   if        VABRMA = *blanks
027000071003     C                   movel(P)  VABRMN        VABRMA
027100071003     C                   endif
027200071003     C*
027300021113     C                   write     fivab000                                     => scarico il VAB
027400010305     C*
027500010305     C                   ENDSR
027600990920
027700000801     C*----------------------------------------------------*
027800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900000801     C*----------------------------------------------------*
028000010201     C     INZVAR        BEGSR
028100000801     C*
028200010201     C                   Z-ADD     *zeros        Num5_0
028300151015     C                   EVAL      w70 = *blank
028400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028500020725     C                   MOVEL     '0'           FlgCAS            1
028600000801     C*
028700000801     C                   ENDSR
028800000801     C*----------------------------------------------------*
028900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029000000801     C*----------------------------------------------------*
029100020904     C     DEFCAM        BEGSR
029200080923     C*
029300080923     C                   SETOFF                                       5051
029400000801     C*
029500021113     C                   CLEAR                   FIVAB000
029600021113     C                   CLEAR                   FIVAT000
029700070730     C* Imposto i valori di default...
029800151015     C                   EVAL      VABCCM = 0058105
029900151015     C                   EVAL      VATCCM = 0058105
030000151015     C                   EVAL      VABLNP = 005
030100151015     C                   EVAL      VATLNP = 005
030200130723     C                   EVAL      VABCBO = '1'
030300150508     C                   EVAL      VABCTM = '7Q'
030400070222     C* ... e poi verifico se sono stati passati come parametri
030500070222     C                   IF        vlrppt > *blanks
030600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030800070222     C                   EXSR      CHKNUM
030900070222     C                   IF        PiInt=*on
031000070222     C                   Z-ADD     PiVal         VABCCM
031100070222     C                   Z-ADD     PiVal         VATCCM
031200070222     C                   ENDIF
031300070222     C                   ENDIF
031400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031600070222     C                   EXSR      CHKNUM
031700070222     C                   IF        PiInt=*on
031800070222     C                   Z-ADD     PiVal         VABLNP
031900070222     C                   Z-ADD     PiVal         VATLNP
032000070222     C                   ENDIF
032100070222     C                   ENDIF
032200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032400070222     C                   EXSR      CHKNUM
032500070222     C                   IF        PiInt=*on
032600070222     C                   Z-ADD     PiVal         VABCTR
032700070222     C                   ENDIF
032800070928     C                   ENDIF
032900071009     C                   MOVEL     *blanks       wDISK             1
033000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033100071009     C                   IF        wDISK <> *blanks
033200070928     C                   SETON                                        50
033300070222     C                   ENDIF
033400080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033500080923     C                   SETON                                        51
033600080923     C                   ENDIF
033700130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033800130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033900130507     C                   ENDIF
034000070222     C                   ENDIF
034100071009     C*
034200071009     C   50              EVAL      VABCTM = '7Q'
034300000801     C*
034400000801     C                   ENDSR
034500000801     C*----------------------------------------------------*
034600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034700000801     C*----------------------------------------------------*
034800010201     C     IMPVAB        BEGSR
034900070530     C*
035000070530     C                   SETOFF                                       3132
035100070928     C*
035200070928     C                   EXSR      INZVAR
035300070928     C                   EXSR      DEFCAM
035400070928     C*
035500070928     C                   MOVE(P)   vlrpoi        VABFGS
035600070928     C                   MOVE(P)   vlrpoi        VATFGS
035700130723     C                   MOVEL     datcor        VABAAS
035800070928     C                   MOVE      datcor        VABMGS
035900151015     C                   EVAL      w70=%trim(%subst(vindta:17:40))
036000150508     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036100151015     C     '@':'A'       XLATE     w70           w70
036200070928     C* ==
036300151015     C                   EVAL      VABRSD=%trim(%subst(w70:1:35))
036400151015     C                   EVAL      VABRD2=%trim(%subst(w70:36:35))
036500151015     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
036600151015     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
036700151015     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036800151105     C                   EVAL      VABRMA = %trim(%subst(vindta:187:10))
036900151015     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
037000151015     C                   EVAL      w70=%trim(%subst(vindta:197:80))
037100151015     C                   EVAL      VABNOT=%trim(%subst(w70:1:35))
037200151015     C                   EVAL      VABNT2=%trim(%subst(w70:36:35))
037300151015     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
037400151015     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
037500151015     C                   IF        %subst(vindta:16:1) = 'F'
037600151015     C                   EVAL      VABCTR = 301
037700151015     C                   ELSE
037800151015     C                   EVAL      VABCTR = 300
037900151015     C                   ENDIF
038000150508     C*
038100151015     C* CAP
038200151015     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
038300151015     C                   EXSR      CHKNUM
038400151015     C                   IF        PiInt=*on
038500151015     C                   Z-ADD     PiVal         Num5_0
038600151015     C                   MOVEL(P)  Num5_0        VABCAD
038700151015     C                   ELSE
038800151015     C                   SETON                                        32
038900151015     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039000151015     C                             + ' ' + 'VABCAD'
039100151015     C                   ENDIF
039200151105     C* RMN/NSP
039300151105     C* "normalizzare" le casistiche ad una forma numerica lunga 8, ovvero (le x sono numeri):
039400160401     C* xxxxxxx (es.: 3814425) = quando è un numerico lungo 7 lo trasformiamo in 99xxxxxx => NO !!
039500151105     C* MASxxxxx (es.: MAS58523) = quando inizia con 'MAS' lo trasformiamo in 111xxxxx
039600151105     C* TRAxxxxx (es.: TRA77848) = quando inizia con 'TRA' lo trasformiamo in 222xxxxx
039700160401     C* TKAMxxxx (es.: TKAM4402) = quando inizia con 'TKAM' lo trasformiamo in 8888xxxx
039800160401     C* TAxxxx (es.: TA2176) = quando inizia con 'TA' lo trasformiamo in 9999xxxx
039900151105     C* Quindi del valore risultante teniamo gli ultimi 7 digits a dx per NSP e tutti gli 8 per RMN
040000151105     C                   EVAL      w8 = *blank
040100151105     C                   SELECT
040200151105     C                   WHEN      %len(%trim(%subst(vindta:187:10))) = 7
040300160401     C                   EVAL      w8 = %trim(%subst(vindta:187:10))
040400160401     C***                EVAL      w8 = '99' +
040500160401     C***                           %subst(%trim(%subst(vindta:187:10)) : 2 : 6)
040600151105     C                   WHEN      %subst(%trim(%subst(vindta:187:10)) : 1 : 3)
040700151105     C                             = 'MAS'
040800151105     C                   EVAL      w8 =
040900151105     C                              %replace('111':%trim(%subst(vindta:187:10)))
041000151105     C                   WHEN      %subst(%trim(%subst(vindta:187:10)) : 1 : 3)
041100151105     C                             = 'TRA'
041200151105     C                   EVAL      w8 =
041300151105     C                              %replace('222':%trim(%subst(vindta:187:10)))
041400151105     C                   WHEN      %subst(%trim(%subst(vindta:187:10)) : 1 : 4)
041500151105     C                             = 'TKAM'
041600151105     C                   EVAL      w8 =
041700160401     C                             %replace('8888':%trim(%subst(vindta:187:10)))
041800151105     C                   WHEN      %subst(%trim(%subst(vindta:187:10)) : 1 : 2)
041900151105     C                             = 'TA'
042000151105     C                   EVAL      w8 =
042100160401     C                             %replace('9999':%trim(%subst(vindta:187:10))
042200151105     C                             : 1 : 2)
042300151105     C                   ENDSL
042400151105     C                   EVAL      PiStr=w8
042500121001     C                   EXSR      CHKNUM
042600121001     C                   IF        PiInt=*on
042700121001     C                   Z-ADD     PiVal         VABRMN
042800151015     C                   Z-ADD     PiVal         VABNSP
042900121001     C                   ELSE
043000121001     C                   SETON                                        32
043100121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043200151105     C                             + ' ' + 'VABRMN VABNSP'
043300121001     C                   ENDIF
043400071003     C* NCL
043500151015     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
043600070928     C                   EXSR      CHKNUM
043700070928     C                   IF        PiInt=*on
043800070928     C                   Z-ADD     PiVal         VABNCL
043900070928     C                   ELSE
044000070928     C                   SETON                                        32
044100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200070928     C                             + ' ' + 'VABNCL'
044300070928     C                   ENDIF
044400071003     C* PKB
044500151019     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044600070928     C                   EXSR      CHKNUM
044700151019     C* il dato ha il separatore decimale
044800070928     C                   IF        PiNum=*on
044900150812     C                   EVAL      VABPKB = PiVal
045000070928     C                   ELSE
045100070928     C                   SETON                                        32
045200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045300070928     C                             + ' ' + 'VABPKB'
045400070928     C                   ENDIF
045500150508     C* CAS
045600151015     C                   IF        %trim(%subst(vindta:280:12)) <> *blanks AND
045700151015     C                             %trim(%subst(vindta:280:12)) <> *zeros  AND
045800151019     C* da file di esempio pare che quando vogliono dire 0 lo scrivono così
045900151015     C                             %trim(%subst(vindta:280:12)) <>
046000151019     C                              '0.000000'
046100150508     C                   MOVEL     '1'           FlgCAS
046200151015     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
046300150508     C                   EXSR      CHKNUM
046400150508     C                   IF        PiNum=*on
046500150812     C* il dato ha 2 decimali con   separatore
046600150508     C                   Z-ADD     PiVal         VABCAS
046700150508     C                   ELSE
046800150508     C                   SETON                                        32
046900150508     C                   Z-ADD     *zeros        VABCAS
047000150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100150508     C                             + ' ' + 'VABCAS'
047200150508     C                   ENDIF
047300150508     C                   ENDIF
047400151015     C* IAS
047500151015     C                   IF        %trim(%subst(vindta:295:12)) <> *blanks AND
047600151015     C                             %trim(%subst(vindta:295:12)) <> *zeros  AND
047700151019     C* da file di esempio pare che quando vogliono dire 0 lo scrivono così
047800151015     C                             %trim(%subst(vindta:295:12)) <>
047900151019     C                              '0.000000'
048000151015     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
048100151015     C                   EXSR      CHKNUM
048200151015     C                   IF        PiNum=*on
048300151015     C* il dato ha 2 decimali con   separatore
048400151015     C                   Z-ADD     PiVal         VABIAS
048500151015     C                   ELSE
048600151015     C                   SETON                                        32
048700151015     C                   Z-ADD     *zeros        VABIAS
048800151015     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900151015     C                             + ' ' + 'VABIAS'
049000151015     C                   ENDIF
049100151015     C                   ENDIF
049200070730     C*
049300121109     C* Considerazioni finali su CBO/CAS
049400121106     C*
049500121106      * se c/assegno impostato
049600121106     C                   IF        VABCAS > 0
049700121109     C                   IF        VABCBO = '1'
049800121109     C                   EVAL      VABCBO = '4'
049900121109     C                   ENDIF
050000121109     C                   IF        VABCBO = '2'
050100121109     C                   EVAL      VABCBO = '6'
050200121109     C                   ENDIF
050300121106     C                   ENDIF
050400130306     C*
050500130306     C* Se provincia nn valorizzata la reperisco
050600130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
050700130306     C                   IF        VABPRD =  *blanks AND
050800130306     C                             VABNZD =  *blanks AND
050900130306     C                             VABCAD <> *blanks AND
051000130306     C                             VABLOD <> *blanks
051100130306     C                   CLEAR                   TISI95DS
051200130306     C                   EVAL      I95TCN = '3'
051300130306     C                   Z-ADD     datcor        I95DAT
051400130306     C                   EVAL      I95NAR = VABNZD
051500130306     C                   EVAL      I95CAP = VABCAD
051600130306     C                   EVAL      I95LOC = VABLOD
051700130306     C                   CALL      'TISI95R'
051800130306     C                   PARM                    TISI95DS
051900130306     C                   EVAL      VABPRD = O95PRV
052000130306     C                   ENDIF
052100020305     C*
052200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052300011113     C                   EXSR      CHKIMPDIV
052400010202     C*
052500000801     C* Ebbene...
052600000801     C                   ADD       1             §CTRMO
052700070530     C                   IF        *in31 <> *zeros OR
052800070530     C                             *in32 <> *zeros
052900000801     C                   ADD       1             §CTRNO
053000020725     C                   EVAL      x_vinflg = '2'
053100000801     C                   ELSE
053200010201     C                   ADD       1             §CTROKVB
053300000801     C                   ENDIF
053400000801     C*
053500000801     C                   ENDSR
053600010201     C*----------------------------------------------------*
053700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053800010201     C*----------------------------------------------------*
053900071003     C     WRIVAT_B      BEGSR
054000090204     C*
054100090204     C                   eval      vatAAS = vabAAS
054200090204     C                   eval      vatLNP = vabLNP
054300090204     C                   eval      vatNRS = vabNRS
054400090204     C                   eval      vatNSP = vabNSP
054500010201     C*
054600101007     C* Valorizzo il buffer di scrittura del FIVAT
054700070928     C* - TIPO RECORD "B"
054800151015     C                   if        %subst(vindta:92:15) <> *blanks
054900070928     C                   eval      VATTRC = 'B'
055000151015     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
055100021113     C                   write     FIVAT000
055200020725     C                   endif
055300010201     C*
055400010201     C                   ENDSR
055500150812     C*----------------------------------------------------*
055600150812     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
055700150812     C*----------------------------------------------------*
055800150812     C     WRIVAT_IJ     BEGSR
055900150812     C*
056000150812     C                   eval      vatAAS = vabAAS
056100150812     C                   eval      vatLNP = vabLNP
056200150812     C                   eval      vatNRS = vabNRS
056300150812     C                   eval      vatNSP = vabNSP
056400150812     C*
056500150812     C* Valorizzo il buffer di scrittura del FIVAT
056600150812     C* - TIPO RECORD "I"
056700151015     C                   if        %subst(vindta:413:70) <> *blanks
056800151015     C                   eval      w70 = %trim(%subst(vindta:413:70))
056900150812     C                   eval      VATTRC = 'I'
057000150812     C                   eval      VATNOT = %subst(w70:1:35)
057100150812     C                   write     FIVAT000
057200150812     C                   IF        %subst(w70:36:35) <> *blank
057300150812     C                   eval      VATTRC = 'J'
057400150812     C                   eval      VATNOT = %subst(w70:36:35)
057500150812     C                   write     FIVAT000
057600150812     C                   endif
057700151019     C                   eval      VATTRC = 'S'
057800151019     C                   eval      VATNOT = *blank
057900151019     C                   eval      %subst(VATNOT:17:2) = 'NN'
058000151019     C                   write     FIVAT000
058100150812     C                   endif
058200150812     C*
058300150812     C                   ENDSR
058400071003     C*----------------------------------------------------*
058500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058600071003     C*----------------------------------------------------*
058700071003     C     WRIVAT_E      BEGSR
058800090204     C*
058900090204     C                   eval      vatAAS = vabAAS
059000090204     C                   eval      vatLNP = vabLNP
059100090204     C                   eval      vatNRS = vabNRS
059200090204     C                   eval      vatNSP = vabNSP
059300071003     C*
059400101007     C* Valorizzo il buffer di scrittura del FIVAT
059500071003     C* - TIPO RECORD "E"
059600130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
059700130306     C                   eval      VATTRC = 'E'
059800121109     C*
059900130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
060000071003     C                   write     FIVAT000
060100121109     C*
060200130306     C***                endif
060300071003     C*
060400071003     C                   ENDSR
060500020904
060600020904
060700010202     C*----------------------------------------------------*
060800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060900010202     C*----------------------------------------------------*
061000020305     C     PREVAT        BEGSR
061100010202     C*
061200021113     C* Compongo il nome del membro da dare al FIVATWWR
061300010202     C                   eval      parmbr = vlrhdl
061400010202     C                   movel     'M'           parmbr
061500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061600010202     C                   eval      paropz = '1'
061700010202     C* Effettuo la chiamata al CLLE preposto
061800050128     C                   call(e)   'TITVVTC'
061900010202     C                   parm                    parccm
062000010202     C                   parm                    parmbr
062100010202     C                   parm                    paropz
062200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062300010202     C                   if        %error
062400010202     C                   movel     '1'           chkcall
062500010202     C                   else
062600010202     C                   movel     '0'           chkcall
062700010202     C                   endif
062800010202     C*
062900010202     C                   ENDSR
063000000801     C*----------------------------------------------------*
063100000801     C*  CONTROLLO NUMERICITA' CAMPI
063200000801     C*----------------------------------------------------*
063300000801     C     CHKNUM        BEGSR
063400081113     C*
063500081113     C                   IF        PiDecChr = *blanks
063600151019     C                   EVAL      PiDecChr = '.'
063700081113     C                   ENDIF
063800081113     C*
063900081113     C                   callp(e)  UBISNUM_Check(PiStr
064000081113     C                                          :PiDecChr
064100081113     C                                          :PiVal
064200081113     C                                          :PiNum
064300081113     C                                          :PiInt)
064400081113     C*
064500000801     C                   IF        %error
064600000801     C                   EVAL      PiInt=*off
064700000801     C                   ENDIF
064800000801     C*
064900000801     C                   ENDSR
065000000801     C***
065100000801
065200011113
065300011113     C*----------------------------------------------------*
065400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065500011113     C*----------------------------------------------------*
065600011113     C     CHKIMPDIV     BEGSR
065700011113     C*
065800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065900011113     C                   Z-ADD     *zeros        wrkDec            9 9
066000011113     C*
066100011113     C* Come prima cosa effettuo considerazioni sulla divisa
066200011113     C                   IF        vabIAS > *zeros
066300011113     C                   IF        vabVAS <> 'EUR'
066400011113     C                   EVAL      vabVAS =  'ITL'
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C                   IF        vabCAS > *zeros
066900011113     C                   IF        vabVCA <> 'EUR'
067000011113     C                   EVAL      vabVCA =  'ITL'
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C                   IF        vabVMD > *zeros
067500020305     C                   IF        vabVAD <> 'EUR'
067600011113     C                   EVAL      vabVAD =  'ITL'
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068100011113     C                   Z-ADD     vabIAS        wrkDec
068200011113     C                   IF        wrkDec > *zeros
068300011113     C                   IF        vabVAS = 'ITL'
068400011113     C                   EVAL      vabIAS = *zeros
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068900011113     C                   Z-ADD     vabCAS        wrkDec
069000011113     C                   IF        wrkDec > *zeros
069100011113     C                   IF        vabVCA = 'ITL'
069200011113     C                   EVAL      vabCAS = *zeros
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069700011113     C                   Z-ADD     vabVMD        wrkDec
069800011113     C                   IF        wrkDec > *zeros
069900011113     C                   IF        vabVAD = 'ITL'
070000011113     C                   EVAL      vabVMD = *zeros
070100011113     C                   ENDIF
070200011113     C                   ENDIF
070300011113     C*
070400011113     C                   ENDSR
070500011113     C***
070600011113
070700011113
070800000801
070900000801
071000990920      /TITLE Invio dei dati al punto operativo.
071100010202     C     invio         BEGSR
071200990920     C*
071300021113     C* 1° invio FIVAT
071400010201     C                   reset                   dscmz
071500010201     C                   move      vlrpoi        cmzdst
071600021113     C                   eval      cmzfld = 'FIVATWWR'
071700010201     C                   eval      cmzmbd = vlrhdl
071800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071900021009     C***                if        prmfir = *blanks
072000021113     C                   eval      cmzfla = 'FIVAT00F'
072100021113     C                   eval      cmzmba = 'FIVAT00F'
072200021009     C***                else
072300021009     C***                eval      cmzfla = prmfir
072400021009     C***                eval      cmzmba = prmfir
072500021009     C***                endif
072600010201     C                   eval      cmznrr = *zeros
072700020305     C                   move      §ctrokvt      cmznrr
072800021018     C                   eval      cmzlba = vlrfl1
072900010201     C                   call(e)   'TIS711C'
073000010201     C                   parm                    dscmz
073100010201     C                   parm      *blanks       esito
073200010205     C                   if        %error
073300010205     C                             or cmzerr = '1'
073400010205     C                             or esito  = '1'
073500010205     C                   eval      wrkesito = '3'
073600010205     C                   else
073700010201     C*
073800021113     C* 2° invio FIVAB
073900010201     C                   reset                   dscmz
074000010201     C                   move      vlrpoi        cmzdst
074100010201     C                   eval      cmzfld = vlrfou
074200010201     C                   eval      cmzmbd = vlrhdl
074300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074400021009     C***                if        prmfir = *blanks
074500021113     C                   eval      cmzfla = 'FIVAB00F'
074600021113     C                   eval      cmzmba = 'FIVAB00F'
074700021009     C***                else
074800021009     C***                eval      cmzfla = prmfir
074900021009     C***                eval      cmzmba = prmfir
075000021009     C***                endif
075100010201     C                   eval      cmznrr = *zeros
075200010201     C                   move      §ctrokvb      cmznrr
075300021018     C                   eval      cmzlba = vlrfl1
075400010201     C                   call(e)   'TIS711C'
075500010201     C                   parm                    dscmz
075600010201     C                   parm      *blanks       esito
075700010201     C                   if        %error
075800010201     C                             or cmzerr = '1'
075900010201     C                             or esito  = '1'
076000010201     C                   eval      wrkesito = '3'
076100010201     C                   endif
076200010205     C                   endif
076300990920     C*
076400000613     C                   ENDSR
076500000613     C***
076600070411
076700070411     C     *pssr         BEGSR
076800070411     C*
076900070411     C                   if        %open(tivin00r)
077000070411     C                   close     tivin00r
077100070411     C                   endif
077200070411     C                   if        %open(fivabwwr)
077300070411     C                   close     fivabwwr
077400070411     C                   endif
077500070411     C                   if        %open(fivatwwr)
077600070411     C                   close     fivatwwr
077700070411     C                   endif
077800070411     C*
077900070411     C* Effettuo la chiamata al CLLE preposto
078000070411     C                   call(e)   'TITVVTC'
078100070411     C                   parm                    parccm
078200070411     C                   parm                    parmbr
078300070411     C                   parm      '2'           paropz
078400070411     C*
078500070411     C                   eval      wrkesito = '2'
078600070411     C*
078700070411     C                   seton                                        LR
078800070411     C*
078900070411     C                   ENDSR     '*CANCL'
079000070411     C***
079100070411
079200990910
079300000613     C     *inzsr        BEGSR
079400990910     C*
079500990910     C     *entry        plist
079600990920     C                   parm                    tivlrds
079700990921     C                   parm      wrkesito      esito
079800000724     C                   parm                    prmlit
079900000710     C                   parm                    prmfir
080000000613     C*
080100000830     C* CALCOLA LA DATA CORRENTE
080200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080300100324     C                   eval      datcor = %dec(%date() : *ISO)
080400000830     C*
080500000613     C                   ENDSR
080600000613     C***
