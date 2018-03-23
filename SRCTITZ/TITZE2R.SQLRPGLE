000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200141128      *
000300141128      * PARTICOLARITA':
000400141128      * - il dato di rottura spedizione è RMA
000500141128      * - se la divisa c/assegno è blank viene forzata EUR
000600141128      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800140515
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
002600110622     D depspe          s             10    INZ(*blanks)
002700110622     D curspe          s             10    INZ(*blanks)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003300070530     D Num5_0          s              5  0
003400000830
003500020725
003600020725     D*------------------
003700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003800020725     D*------------------
003900070530     D INPUT_DS        DS                  INZ
004000101007     D  VINDTA                     2048
004100101007     D  VINFLG                        1
004200110524     D  VINSPE                        8
004300101007     D  VINRRN                        8  0
004400020725     D*
004500080923     D*------------------
004600080923     D* DS REPERIMENTO NUMERATORE
004700080923     D*------------------
004800080923     D trul33ds      e ds                  inz
004900080923     D*------------------
005000080923     D* DS ARCHITETTURA
005100080923     D*------------------
005200080923     D kpjba         e ds                  inz
005300080923     D*------------------
005400081113
005500081113     D*------------------
005600081113     D* LINKING A DEFINIZIONI ESTERNE
005700081113     D*------------------
005800100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006000081113
006100990908
006200010201
006300010201
006400080117     C*
006500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006600080117     C
006700080117     C/EXEC SQL
006800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006900080117     C/END-EXEC
007000080117     C
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400000613     C*
007500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007600000613     C*
007700010202     C* Effettuo la chiamata al CLLE preposto
007800050128     C                   call(e)   'TITVVTC'
007900010202     C                   parm                    parccm
008000010202     C                   parm                    parmbr
008100010202     C                   parm      '2'           paropz
008200070730     C*
008300070730     C* Effettuo lancio TISI95 solo x chiusura
008400070730     C                   CLEAR                   TISI95DS
008500070730     C                   EVAL      I95TLA = 'C'
008600070730     C                   CALL      'TISI95R'
008700070730     C                   PARM                    TISI95DS
008800000616     C*
008900000801     C
009000010201     C                   seton                                        LR
009100000801
009200910830     C*--------------------------------------------------------
009300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009400910830     C*--------------------------------------------------------
009500070530     C     RWFILE        BEGSR
009600990910     C*
009700990914     C                   if        not %open(tivin00r)
009800990908     C                   open      tivin00r
009900990914     C                   endif
010000021113     C                   if        not %open(fivabwwr)
010100021113     C                   open      fivabwwr
010200990914     C                   endif
010300070530     C*
010400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010500020305     C                   exsr      prevat
010600010201     C*
010700010202     C                   if        chkcall = '0'
010800010202     C*
010900021113     C                   if        not %open(fivatwwr)
011000021113     C                   open      fivatwwr
011100010201     C                   endif
011200080117     C*
011300080117     C                   EXSR      INZVAR
011400080117     C                   EXSR      DEFCAM
011500990910     C*
011600010201     C                   clear                   §CTROKVB          5 0
011700020305     C                   clear                   §CTROKVT          5 0
011800000801     C                   clear                   §CTRMO            5 0
011900000801     C                   clear                   §CTRNO            5 0
012000990910     C*
012100020725     C/EXEC SQL
012200020725     C+ declare C1 cursor for select
012300110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
012400060223     C+ from tivin00r
012500060223     C+ order by sped
012600020725     C+ for read only
012700020725     C/END-EXEC
012800020725     C
012900020725     C/EXEC SQL
013000020725     C+ open C1
013100020725     C/END-EXEC
013200020725     C
013300020725     C/EXEC SQL
013400070530     C+ Fetch C1 into :INPUT_DS
013500020725     C/END-EXEC
013600020725     C*
013700020725     C                   dow       sqlcod = *zeros
013800990913     C*
013900020725     C                   if        vindta > *blanks
014000000613     C                   add       1             rrnum
014100000801     C*
014200020725     C                   if        vinflg = *blanks
014300020725     C                             or vinflg = '0'
014400020725     C                             or vinflg = '2'
014500000801     C*
014600020725     C                   clear                   x_vinmsg
014700020725     C                   eval      x_vinflg = '1'
014800010305     C*
014900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015000110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015100020305     C                   MOVEL(p)  PiStr         curspe
015200010305     C*
015300071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015400080923     C                   if        *in50 = *off
015500071003     C                   exsr      impvab
015600071003     C                   exsr      wrivab
015700071003     C                   exsr      wrivat_b                                     => carico VAT
015800071003     C                   else
015900080923     C*
016000071009     C                   if        wDISK = 'D'
016100071009     C                   exsr      impvab
016200071009     C                   exsr      wrivab
016300071009     C                   exsr      wrivat_b                                     => carico VAT
016400071009     C                   exsr      wrivat_e                                     => carico VAT
016500071009     C                   else
016600071009     C*
016700010305     C                   if        depspe = *blanks                             => 1° giro
016800010305     C                   eval      depspe = curspe                              => memorizz. spediz
016900080117     C                   clear                   tivinds
017000020305     C                   exsr      impvab
017100071003     C                   exsr      wrivat_b                                     => carico VAT
017200071003     C   50              exsr      wrivat_e                                     => carico VAT
017300010305     C                   else
017400020725     C                   if        depspe <> curspe                             => rottura di spediz
017500010305     C                   eval      depspe = curspe                              => memorizz. spediz
017600070928     C                   exsr      wrivab
017700080117     C                   clear                   tivinds
017800020305     C                   exsr      impvab
017900071003     C                   exsr      wrivat_b                                     => carico VAT
018000071003     C   50              exsr      wrivat_e                                     => carico VAT
018100020305     C                   else                                                   => x stessa spediz
018200090204     C***                exsr      impvab
018300071003     C                   exsr      wrivat_b                                     => carico VAT
018400071003     C   50              exsr      wrivat_e                                     => carico VAT
018500010305     C                   endif
018600010305     C                   endif
018700010305     C                   endif
018800071003     C                   endif
018900071009     C                   endif
019000000905     C*
019100000905     C                   else
019200020725     C                   eval      x_vinflg = '1'
019300000905     C                   endif
019400000905     C*
019500020725     C     VINRRN        chain     tivin000
019600020725     C                   if        %found(tivin00r)
019700020725     C                   eval      y_vinflg = x_vinflg
019800020725     C                   eval      y_vinmsg = x_vinmsg
019900020725     C                   update    tivin000
020000020725     C                   endif
020100020725     C*
020200020725     C/EXEC SQL
020300070530     C+ Fetch C1 into :INPUT_DS
020400020725     C/END-EXEC
020500020725     C*
020600020725     C                   enddo
020700020725     C*
020800020725     C/EXEC SQL
020900020725     C+ close C1
021000020725     C/END-EXEC
021100000905     C*
021200020305     C* Scarico i VAB rimasti "in sospeso"
021300071009     C                   if        wDISK = 'C'
021400071009     C                   exsr      wrivab
021500071009     C                   endif
021600010202     C*
021700010202     C                   endif
021800990910
021900990910     C* Se non ci sono record con errori ...
022000000710     C                   if        §ctrno = 0
022100990910     C* ... restituisco esito OK.
022200990921     C                   eval      wrkesito = '0'
022300990910     C                   else
022400010201     C                   if        §ctrokvb > 0
022500990921     C                   eval      wrkesito = '1'
022600000710     C                   else
022700000710     C                   eval      wrkesito = '2'
022800990910     C                   endif
022900000710     C                   endif
023000990910     C*
023100990914     C                   if        %open(tivin00r)
023200990908     C                   close     tivin00r
023300990914     C                   endif
023400021113     C                   if        %open(fivabwwr)
023500021113     C                   close     fivabwwr
023600990914     C                   endif
023700021113     C                   if        %open(fivatwwr)
023800021113     C                   close     fivatwwr
023900010201     C                   endif
024000990910     C*
024100010201     C                   if        §ctrokvb > 0
024200000724     C                             and vlrpoi <> *zeros
024300010202     C                   exsr      invio
024400990920     C                   endif
024500990920     C*
024600910830     C                   ENDSR
024700000613     C***
024800010305
024900010305     C*----------------------------------------------------*
025000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025100010305     C*----------------------------------------------------*
025200020305     C     WRIVAB        BEGSR
025300070730     C*
025400071003     C* Considerazioni finali
025500071003     C                   if        VABRMA = *blanks
025600071003     C                   movel(P)  VABRMN        VABRMA
025700071003     C                   endif
025800071003     C*
025900021113     C                   write     fivab000                                     => scarico il VAB
026000010305     C*
026100010305     C                   ENDSR
026200990920
026300000801     C*----------------------------------------------------*
026400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026500000801     C*----------------------------------------------------*
026600010201     C     INZVAR        BEGSR
026700000801     C*
026800010201     C                   Z-ADD     *zeros        Num5_0
026900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027000020725     C                   MOVEL     '0'           FlgCAS            1
027100000801     C*
027200000801     C                   ENDSR
027300000801     C*----------------------------------------------------*
027400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027500000801     C*----------------------------------------------------*
027600020904     C     DEFCAM        BEGSR
027700080923     C*
027800080923     C                   SETOFF                                       5051
027900000801     C*
028000021113     C                   CLEAR                   FIVAB000
028100021113     C                   CLEAR                   FIVAT000
028200070730     C* Imposto i valori di default...
028300141128     C                   EVAL      VABCCM = 1192098
028400141128     C                   EVAL      VATCCM = 1192098
028500141128     C                   EVAL      VABLNP = 119
028600141128     C                   EVAL      VATLNP = 119
028700121214     C                   EVAL      VABCTR = 000
028800140403     C                   EVAL      VABCBO = '1'
028900140403     C                   EVAL      VABCTM = '7Q'
029000070222     C* ... e poi verifico se sono stati passati come parametri
029100070222     C                   IF        vlrppt > *blanks
029200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029400070222     C                   EXSR      CHKNUM
029500070222     C                   IF        PiInt=*on
029600070222     C                   Z-ADD     PiVal         VABCCM
029700070222     C                   Z-ADD     PiVal         VATCCM
029800070222     C                   ENDIF
029900070222     C                   ENDIF
030000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030200070222     C                   EXSR      CHKNUM
030300070222     C                   IF        PiInt=*on
030400070222     C                   Z-ADD     PiVal         VABLNP
030500070222     C                   Z-ADD     PiVal         VATLNP
030600070222     C                   ENDIF
030700070222     C                   ENDIF
030800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031000070222     C                   EXSR      CHKNUM
031100070222     C                   IF        PiInt=*on
031200070222     C                   Z-ADD     PiVal         VABCTR
031300070222     C                   ENDIF
031400070928     C                   ENDIF
031500071009     C                   MOVEL     *blanks       wDISK             1
031600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031700071009     C                   IF        wDISK <> *blanks
031800070928     C                   SETON                                        50
031900070222     C                   ENDIF
032000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032100080923     C                   SETON                                        51
032200080923     C                   ENDIF
032300070222     C                   ENDIF
032400071009     C*
032500071009     C   50              EVAL      VABCTM = '7Q'
032600000801     C*
032700000801     C                   ENDSR
032800000801     C*----------------------------------------------------*
032900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033000000801     C*----------------------------------------------------*
033100010201     C     IMPVAB        BEGSR
033200070530     C*
033300070530     C                   SETOFF                                       3132
033400070928     C*
033500070928     C                   EXSR      INZVAR
033600070928     C                   EXSR      DEFCAM
033700070928     C*
033800070928     C                   MOVE(P)   vlrpoi        VABFGS
033900070928     C                   MOVE(P)   vlrpoi        VATFGS
034000070928     C*
034100070928     C                   MOVEL     datcor        VABAAS
034200070928     C                   MOVEL     datcor        VATAAS
034300070928     C                   MOVE      datcor        VABMGS
034400070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034500070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034700070928     C* ==
034800070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
034900070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035000070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035100141128     C***                EVAL      VABRMA=%trim(%subst(vindta:362:15))
035200141128     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035300110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
035400070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035500080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
035600070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035700150921     C                   EVAL      VABNOT=%trim(%subst(vindta:412:35))
035800150921     C                   EVAL      VABNT2=%trim(%subst(vindta:412+35:5))
035900121214     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
036000121214     C                   EVAL      VABCBO='1'
036100121214     C                   ENDIF
036200121214     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
036300121214     C                   EVAL      VABCBO='2'
036400121214     C                   ENDIF
036500140403     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
036600140403     C                   EVAL      VABTSP='E'
036700140403     C                   ENDIF
036800140403     C                   IF        %trim(%subst(vindta:16:1)) = '1'
036900140403     C                   EVAL      VABFFD='S'
037000140403     C                   ENDIF
037100140403     C                   IF        %trim(%subst(vindta:16:1)) = '2'
037200140403     C                   EVAL      VABTC1='A'
037300140403     C                   ENDIF
037400071003     C* CAD
037500070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
037600070928     C                   EXSR      CHKNUM
037700070928     C                   IF        PiInt=*on
037800070928     C                   Z-ADD     PiVal         Num5_0
037900070928     C                   MOVEL     Num5_0        VABCAD
038000070928     C                   ELSE
038100070928     C                   SETON                                        32
038200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038300070928     C                             + ' ' + 'VABCAD'
038400070928     C                   ENDIF
038500080923     C* RMN
038600110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
038700141128     C                   EVAL      PiStr=%trim(%subst(vindta:189:7))
038800141128     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
038900101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
039000080923     C                   EXSR      CHKNUM
039100080923     C                   IF        PiInt=*on
039200080923     C                   Z-ADD     PiVal         VABRMN
039300080923     C                   ELSE
039400080923     C                   SETON                                        32
039500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600080923     C                             + ' ' + 'VABRMN'
039700080923     C                   ENDIF
039800080923     C* NSP
039900080923     C                   IF        *in51 = *on
040000080923     C* NSP => Stacco un numeratore da AZNUM
040100080923     C                   clear                   TRUL33DS
040200080923     C                   eval      I33OPE = *zeros
040300080923     C                   eval      I33CNU = 302
040400080923     C                   eval      I33NUM = 1
040500080923     C                   movel     TRUL33DS      KPJBU
040600080923     C                   call      'TRUL33R'
040700080923     C                   parm                    KPJBA
040800080923     C                   movel     KPJBU         TRUL33DS
040900080923     C                   if        O33ERR = *zeros
041000080923     C                   z-add     O33NRF        VABNSP
041100080923     C                   z-add     O33NRF        VATNSP
041200080923     C                   else
041300080923     C                   SETON                                        31
041400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041500080923     C                             + ' ' + 'VABNSP VATNSP'
041600080923     C                   endif
041700080923     C                   ELSE
041800141128     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041900141128     C                   EVAL      PiStr=%trim(%subst(vindta:189:7))
042000141128     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
042100080923     C                   EXSR      CHKNUM
042200080923     C                   IF        PiInt=*on
042300080923     C                   Z-ADD     PiVal         VABNSP
042400080923     C                   Z-ADD     PiVal         VATNSP
042500080923     C                   ELSE
042600080923     C                   SETON                                        31
042700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042800080923     C                             + ' ' + 'VABNSP VATNSP'
042900080923     C                   ENDIF
043000080923     C                   ENDIF
043100071003     C* NCL
043200070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
043300070928     C                   EXSR      CHKNUM
043400070928     C                   IF        PiInt=*on
043500070928     C                   Z-ADD     PiVal         VABNCL
043600070928     C                   ELSE
043700070928     C                   SETON                                        32
043800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043900070928     C                             + ' ' + 'VABNCL'
044000070928     C                   ENDIF
044100071003     C* PKB
044200110520     C                   IF        %subst(vindta:170:8) <> *blanks
044300070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044400070928     C                   EXSR      CHKNUM
044500070928     C                   IF        PiNum=*on
044600070928     C                   Z-ADD     PiVal         VABPKB
044700070928     C                   ELSE
044800070928     C                   SETON                                        32
044900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045000070928     C                             + ' ' + 'VABPKB'
045100070928     C                   ENDIF
045200110520     C                   ENDIF
045300080923     C* CAS
045400080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
045500080923     C                             AND
045600080923     C                             %trim(%subst(vindta:280:12))<>*zeros
045700080923     C                             AND
045800080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
045900080923     C                             AND
046000080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
046100080923     C                             AND
046200080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
046300070928     C                   MOVEL     '1'           FlgCAS
046400150410     C                   SELECT
046500150410     C                   WHEN      %trim(%subst(vindta:197:80)) =
046600150410     C                             'A/B O POSTALE INTES.SAFTE SPA COME LASCIAT+
046700150410     C                             O DA CLI'
046800150410     C                   EVAL      VABTIC='TM'
046900150410     C                   WHEN      %trim(%subst(vindta:197:80)) =
047000150410     C                             'A/B O POSTALE-A/C A VISTA INT.SAFTE SPA O C+
047100150410     C                             ONTANTI'
047200150410     C                   EVAL      VABTIC='BM'
047300150410     C                   OTHER
047400150410     C                   EVAL      VABTIC=*blank
047500150410     C                   ENDSL
047600070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047700070928     C                   EXSR      CHKNUM
047800070928     C                   IF        PiNum=*on
047900070928     C                   Z-ADD     PiVal         VABCAS
048000070928     C                   ELSE
048100070928     C                   SETON                                        32
048200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300070928     C                             + ' ' + 'VABCAS'
048400070928     C                   ENDIF
048500070928     C                   ENDIF
048600071003     C* IAS
048700080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
048800080923     C                             AND
048900080923     C                             %trim(%subst(vindta:295:12))<>*zeros
049000080923     C                             AND
049100080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
049200080923     C                             AND
049300080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
049400080923     C                             AND
049500080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
049600080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049700080923     C                   EXSR      CHKNUM
049800080923     C                   IF        PiNum=*on
049900080923     C                   Z-ADD     PiVal         VABIAS
050000080923     C                   ELSE
050100080923     C                   SETON                                        32
050200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300080923     C                             + ' ' + 'VABIAS'
050400080923     C                   ENDIF
050500071003     C                   ENDIF
050600070730     C*
050700070730     C* Considerazioni finali su CBO/CAS
050800070730     C                   IF        FlgCAS = '1'
050900070730     C                   IF        VABCBO = '1'
051000070730     C                   EVAL      VABCBO = '4'
051100070730     C                   ENDIF
051200070730     C                   IF        VABCBO = '2'
051300070730     C                   EVAL      VABCBO = '6'
051400070730     C                   ENDIF
051500070730     C                   ENDIF
051600020305     C*
051700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051800011113     C                   EXSR      CHKIMPDIV
051900010202     C*
052000000801     C* Ebbene...
052100000801     C                   ADD       1             §CTRMO
052200070530     C                   IF        *in31 <> *zeros OR
052300070530     C                             *in32 <> *zeros
052400000801     C                   ADD       1             §CTRNO
052500020725     C                   EVAL      x_vinflg = '2'
052600000801     C                   ELSE
052700010201     C                   ADD       1             §CTROKVB
052800000801     C                   ENDIF
052900000801     C*
053000000801     C                   ENDSR
053100010201     C*----------------------------------------------------*
053200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053300010201     C*----------------------------------------------------*
053400071003     C     WRIVAT_B      BEGSR
053500090204     C*
053600090204     C                   eval      vatAAS = vabAAS
053700090204     C                   eval      vatLNP = vabLNP
053800090204     C                   eval      vatNRS = vabNRS
053900090204     C                   eval      vatNSP = vabNSP
054000010201     C*
054100101007     C* Valorizzo il buffer di scrittura del FIVAT
054200070928     C* - TIPO RECORD "B"
054300070928     C                   if        %subst(vindta:92:15) <> *blanks
054400070928     C                   eval      VATTRC = 'B'
054500070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
054600021113     C                   write     FIVAT000
054700020725     C                   endif
054800010201     C*
054900010201     C                   ENDSR
055000071003     C*----------------------------------------------------*
055100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
055200071003     C*----------------------------------------------------*
055300071003     C     WRIVAT_E      BEGSR
055400090204     C*
055500090204     C                   eval      vatAAS = vabAAS
055600090204     C                   eval      vatLNP = vabLNP
055700090204     C                   eval      vatNRS = vabNRS
055800090204     C                   eval      vatNSP = vabNSP
055900071003     C*
056000101007     C* Valorizzo il buffer di scrittura del FIVAT
056100071003     C* - TIPO RECORD "E"
056200141209     C                   eval      VATTRC = 'E'
056300141209     C*
056400071003     C                   if        %subst(vindta:331:26) <> *blanks
056500141209     C*
056600141209     C* sviluppo i CHI SONO partendo da 1 per quanti sono i colli
056700141209     C*
056800141209     C     1             DO        VABNCL        wNumCollo         3 0
056900141209     C*
057000141209     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
057100141209     C                   EVAL      VATNOT = %trim(VATNOT) +
057200141209     C                              %editc(wNumCollo :'X')
057300141209     C*
057400071003     C                   write     FIVAT000
057500141209     C                   ENDDO
057600071003     C                   endif
057700071003     C*
057800071003     C                   ENDSR
057900020904
058000020904
058100010202     C*----------------------------------------------------*
058200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058300010202     C*----------------------------------------------------*
058400020305     C     PREVAT        BEGSR
058500010202     C*
058600021113     C* Compongo il nome del membro da dare al FIVATWWR
058700010202     C                   eval      parmbr = vlrhdl
058800010202     C                   movel     'M'           parmbr
058900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
059000010202     C                   eval      paropz = '1'
059100010202     C* Effettuo la chiamata al CLLE preposto
059200050128     C                   call(e)   'TITVVTC'
059300010202     C                   parm                    parccm
059400010202     C                   parm                    parmbr
059500010202     C                   parm                    paropz
059600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059700010202     C                   if        %error
059800010202     C                   movel     '1'           chkcall
059900010202     C                   else
060000010202     C                   movel     '0'           chkcall
060100010202     C                   endif
060200010202     C*
060300010202     C                   ENDSR
060400000801     C*----------------------------------------------------*
060500000801     C*  CONTROLLO NUMERICITA' CAMPI
060600000801     C*----------------------------------------------------*
060700000801     C     CHKNUM        BEGSR
060800081113     C*
060900081113     C                   IF        PiDecChr = *blanks
061000150116     C                   EVAL      PiDecChr = ','
061100081113     C                   ENDIF
061200081113     C*
061300081113     C                   callp(e)  UBISNUM_Check(PiStr
061400081113     C                                          :PiDecChr
061500081113     C                                          :PiVal
061600081113     C                                          :PiNum
061700081113     C                                          :PiInt)
061800081113     C*
061900000801     C                   IF        %error
062000000801     C                   EVAL      PiInt=*off
062100000801     C                   ENDIF
062200000801     C*
062300000801     C                   ENDSR
062400000801     C***
062500000801
062600011113
062700011113     C*----------------------------------------------------*
062800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062900011113     C*----------------------------------------------------*
063000011113     C     CHKIMPDIV     BEGSR
063100011113     C*
063200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063300011113     C                   Z-ADD     *zeros        wrkDec            9 9
063400011113     C*
063500011113     C* Come prima cosa effettuo considerazioni sulla divisa
063600011113     C                   IF        vabIAS > *zeros
063700011113     C                   IF        vabVAS <> 'EUR'
063800011113     C                   EVAL      vabVAS =  'ITL'
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C                   IF        vabCAS > *zeros
064300141128     C                   IF        vabVCA = *blank
064400141128     C                   EVAL      vabVCA =  'EUR'
064500141128     C                   ENDIF
064600011113     C                   IF        vabVCA <> 'EUR'
064700011113     C                   EVAL      vabVCA =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   IF        vabVMD > *zeros
065200020305     C                   IF        vabVAD <> 'EUR'
065300011113     C                   EVAL      vabVAD =  'ITL'
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065800011113     C                   Z-ADD     vabIAS        wrkDec
065900011113     C                   IF        wrkDec > *zeros
066000011113     C                   IF        vabVAS = 'ITL'
066100011113     C                   EVAL      vabIAS = *zeros
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066600011113     C                   Z-ADD     vabCAS        wrkDec
066700011113     C                   IF        wrkDec > *zeros
066800011113     C                   IF        vabVCA = 'ITL'
066900011113     C                   EVAL      vabCAS = *zeros
067000011113     C                   ENDIF
067100011113     C                   ENDIF
067200011113     C*
067300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067400011113     C                   Z-ADD     vabVMD        wrkDec
067500011113     C                   IF        wrkDec > *zeros
067600011113     C                   IF        vabVAD = 'ITL'
067700011113     C                   EVAL      vabVMD = *zeros
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   ENDSR
068200011113     C***
068300011113
068400011113
068500000801
068600000801
068700990920      /TITLE Invio dei dati al punto operativo.
068800010202     C     invio         BEGSR
068900990920     C*
069000021113     C* 1° invio FIVAT
069100010201     C                   reset                   dscmz
069200010201     C                   move      vlrpoi        cmzdst
069300021113     C                   eval      cmzfld = 'FIVATWWR'
069400010201     C                   eval      cmzmbd = vlrhdl
069500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600021009     C***                if        prmfir = *blanks
069700021113     C                   eval      cmzfla = 'FIVAT00F'
069800021113     C                   eval      cmzmba = 'FIVAT00F'
069900021009     C***                else
070000021009     C***                eval      cmzfla = prmfir
070100021009     C***                eval      cmzmba = prmfir
070200021009     C***                endif
070300010201     C                   eval      cmznrr = *zeros
070400020305     C                   move      §ctrokvt      cmznrr
070500021018     C                   eval      cmzlba = vlrfl1
070600010201     C                   call(e)   'TIS711C'
070700010201     C                   parm                    dscmz
070800010201     C                   parm      *blanks       esito
070900010205     C                   if        %error
071000010205     C                             or cmzerr = '1'
071100010205     C                             or esito  = '1'
071200010205     C                   eval      wrkesito = '3'
071300010205     C                   else
071400010201     C*
071500021113     C* 2° invio FIVAB
071600010201     C                   reset                   dscmz
071700010201     C                   move      vlrpoi        cmzdst
071800010201     C                   eval      cmzfld = vlrfou
071900010201     C                   eval      cmzmbd = vlrhdl
072000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072100021009     C***                if        prmfir = *blanks
072200021113     C                   eval      cmzfla = 'FIVAB00F'
072300021113     C                   eval      cmzmba = 'FIVAB00F'
072400021009     C***                else
072500021009     C***                eval      cmzfla = prmfir
072600021009     C***                eval      cmzmba = prmfir
072700021009     C***                endif
072800010201     C                   eval      cmznrr = *zeros
072900010201     C                   move      §ctrokvb      cmznrr
073000021018     C                   eval      cmzlba = vlrfl1
073100010201     C                   call(e)   'TIS711C'
073200010201     C                   parm                    dscmz
073300010201     C                   parm      *blanks       esito
073400010201     C                   if        %error
073500010201     C                             or cmzerr = '1'
073600010201     C                             or esito  = '1'
073700010201     C                   eval      wrkesito = '3'
073800010201     C                   endif
073900010205     C                   endif
074000990920     C*
074100000613     C                   ENDSR
074200000613     C***
074300070411
074400070411     C     *pssr         BEGSR
074500070411     C*
074600070411     C                   if        %open(tivin00r)
074700070411     C                   close     tivin00r
074800070411     C                   endif
074900070411     C                   if        %open(fivabwwr)
075000070411     C                   close     fivabwwr
075100070411     C                   endif
075200070411     C                   if        %open(fivatwwr)
075300070411     C                   close     fivatwwr
075400070411     C                   endif
075500070411     C*
075600070411     C* Effettuo la chiamata al CLLE preposto
075700070411     C                   call(e)   'TITVVTC'
075800070411     C                   parm                    parccm
075900070411     C                   parm                    parmbr
076000070411     C                   parm      '2'           paropz
076100070411     C*
076200070411     C                   eval      wrkesito = '2'
076300070411     C*
076400070411     C                   seton                                        LR
076500070411     C*
076600070411     C                   ENDSR     '*CANCL'
076700070411     C***
076800070411
076900990910
077000000613     C     *inzsr        BEGSR
077100990910     C*
077200990910     C     *entry        plist
077300990920     C                   parm                    tivlrds
077400990921     C                   parm      wrkesito      esito
077500000724     C                   parm                    prmlit
077600000710     C                   parm                    prmfir
077700000613     C*
077800000830     C* CALCOLA LA DATA CORRENTE
077900100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078000100324     C                   eval      datcor = %dec(%date() : *ISO)
078100000830     C*
078200000613     C                   ENDSR
078300000613     C***
