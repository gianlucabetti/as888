000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130121      *
000300130121      * PARTICOLARITA':
000400130121      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400160610     D depspe          s             20    INZ(*blanks)
002500160610     D curspe          s             20    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200160610     D $X              s              5  0
003300160610     D $Y              s              5  0
003400000830
003500020725
003600020725     D*------------------
003700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003800020725     D*------------------
003900070530     D INPUT_DS        DS                  INZ
004000101007     D  VINDTA                     2048
004100101007     D  VINFLG                        1
004200160610     D  VINSPE                       20
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
005400160610     D cifre           c                   const('0123456789')
005500081113     D*------------------
005600081113     D* LINKING A DEFINIZIONI ESTERNE
005700081113     D*------------------
005800100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006000081113
006100130121     D w70             s             70
006200990908
006300010201
006400010201
006500080117     C*
006600080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006700080117     C
006800080117     C/EXEC SQL
006900080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007000080117     C/END-EXEC
007100080117     C
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500000613     C*
007600070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007700000613     C*
007800010202     C* Effettuo la chiamata al CLLE preposto
007900050128     C                   call(e)   'TITVVTC'
008000010202     C                   parm                    parccm
008100010202     C                   parm                    parmbr
008200010202     C                   parm      '2'           paropz
008300070730     C*
008400070730     C* Effettuo lancio TISI95 solo x chiusura
008500070730     C                   CLEAR                   TISI95DS
008600070730     C                   EVAL      I95TLA = 'C'
008700070730     C                   CALL      'TISI95R'
008800070730     C                   PARM                    TISI95DS
008900000616     C*
009000000801     C
009100010201     C                   seton                                        LR
009200000801
009300910830     C*--------------------------------------------------------
009400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009500910830     C*--------------------------------------------------------
009600070530     C     RWFILE        BEGSR
009700990910     C*
009800990914     C                   if        not %open(tivin00r)
009900990908     C                   open      tivin00r
010000990914     C                   endif
010100021113     C                   if        not %open(fivabwwr)
010200021113     C                   open      fivabwwr
010300990914     C                   endif
010400070530     C*
010500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010600020305     C                   exsr      prevat
010700010201     C*
010800010202     C                   if        chkcall = '0'
010900010202     C*
011000021113     C                   if        not %open(fivatwwr)
011100021113     C                   open      fivatwwr
011200010201     C                   endif
011300080117     C*
011400080117     C                   EXSR      INZVAR
011500080117     C                   EXSR      DEFCAM
011600990910     C*
011700010201     C                   clear                   §CTROKVB          5 0
011800020305     C                   clear                   §CTROKVT          5 0
011900000801     C                   clear                   §CTRMO            5 0
012000000801     C                   clear                   §CTRNO            5 0
012100990910     C*
012200020725     C/EXEC SQL
012300020725     C+ declare C1 cursor for select
012400160610     C+ vindta, vinflg, substr(vindta, 11, 20) as sped, rrn(tivin00r)
012500060223     C+ from tivin00r
012600060223     C+ order by sped
012700020725     C+ for read only
012800020725     C/END-EXEC
012900020725     C
013000020725     C/EXEC SQL
013100020725     C+ open C1
013200020725     C/END-EXEC
013300020725     C
013400020725     C/EXEC SQL
013500070530     C+ Fetch C1 into :INPUT_DS
013600020725     C/END-EXEC
013700020725     C*
013800020725     C                   dow       sqlcod = *zeros
013900990913     C*
014000020725     C                   if        vindta > *blanks
014100000613     C                   add       1             rrnum
014200000801     C*
014300020725     C                   if        vinflg = *blanks
014400020725     C                             or vinflg = '0'
014500020725     C                             or vinflg = '2'
014600000801     C*
014700020725     C                   clear                   x_vinmsg
014800020725     C                   eval      x_vinflg = '1'
014900010305     C*
015000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015100160610     C                   EVAL      PiStr=%trim(%subst(vindta:11:20))
015200020305     C                   MOVEL(p)  PiStr         curspe
015300010305     C*
015400071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015500080923     C                   if        *in50 = *off
015600071003     C                   exsr      impvab
015700071003     C                   exsr      wrivab
015800160610     C                   exsr      wrivat_as                                    => carico VAT
015900160610     C                   exsr      wrivat_ij                                    => carico VAT
016000071003     C                   else
016100080923     C*
016200071009     C                   if        wDISK = 'D'
016300071009     C                   exsr      impvab
016400071009     C                   exsr      wrivab
016500160610     C                   exsr      wrivat_as                                    => carico VAT
016600160610     C                   exsr      wrivat_ij                                    => carico VAT
016700130121     C***                exsr      wrivat_e                                     => carico VAT
016800071009     C                   else
016900071009     C*
017000010305     C                   if        depspe = *blanks                             => 1° giro
017100010305     C                   eval      depspe = curspe                              => memorizz. spediz
017200080117     C                   clear                   tivinds
017300020305     C                   exsr      impvab
017400160610     C                   exsr      wrivat_as                                    => carico VAT
017500160610     C                   exsr      wrivat_ij                                    => carico VAT
017600130121     C***50              exsr      wrivat_e                                     => carico VAT
017700010305     C                   else
017800020725     C                   if        depspe <> curspe                             => rottura di spediz
017900010305     C                   eval      depspe = curspe                              => memorizz. spediz
018000070928     C                   exsr      wrivab
018100080117     C                   clear                   tivinds
018200020305     C                   exsr      impvab
018300160610     C                   exsr      wrivat_as                                    => carico VAT
018400160610     C                   exsr      wrivat_ij                                    => carico VAT
018500130121     C***50              exsr      wrivat_e                                     => carico VAT
018600020305     C                   else                                                   => x stessa spediz
018700090204     C***                exsr      impvab
018800160122     C***                exsr      wrivat_ij                                    => carico VAT
018900130121     C***50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   endif
019100010305     C                   endif
019200010305     C                   endif
019300071003     C                   endif
019400071009     C                   endif
019500000905     C*
019600000905     C                   else
019700020725     C                   eval      x_vinflg = '1'
019800000905     C                   endif
019900000905     C*
020000020725     C     VINRRN        chain     tivin000
020100020725     C                   if        %found(tivin00r)
020200020725     C                   eval      y_vinflg = x_vinflg
020300020725     C                   eval      y_vinmsg = x_vinmsg
020400020725     C                   update    tivin000
020500020725     C                   endif
020600020725     C*
020700020725     C/EXEC SQL
020800070530     C+ Fetch C1 into :INPUT_DS
020900020725     C/END-EXEC
021000020725     C*
021100020725     C                   enddo
021200020725     C*
021300020725     C/EXEC SQL
021400020725     C+ close C1
021500020725     C/END-EXEC
021600000905     C*
021700020305     C* Scarico i VAB rimasti "in sospeso"
021800071009     C                   if        wDISK = 'C'
021900071009     C                   exsr      wrivab
022000071009     C                   endif
022100010202     C*
022200010202     C                   endif
022300990910
022400990910     C* Se non ci sono record con errori ...
022500000710     C                   if        §ctrno = 0
022600990910     C* ... restituisco esito OK.
022700990921     C                   eval      wrkesito = '0'
022800990910     C                   else
022900010201     C                   if        §ctrokvb > 0
023000990921     C                   eval      wrkesito = '1'
023100000710     C                   else
023200000710     C                   eval      wrkesito = '2'
023300990910     C                   endif
023400000710     C                   endif
023500990910     C*
023600990914     C                   if        %open(tivin00r)
023700990908     C                   close     tivin00r
023800990914     C                   endif
023900021113     C                   if        %open(fivabwwr)
024000021113     C                   close     fivabwwr
024100990914     C                   endif
024200021113     C                   if        %open(fivatwwr)
024300021113     C                   close     fivatwwr
024400010201     C                   endif
024500990910     C*
024600010201     C                   if        §ctrokvb > 0
024700000724     C                             and vlrpoi <> *zeros
024800010202     C                   exsr      invio
024900990920     C                   endif
025000990920     C*
025100910830     C                   ENDSR
025200000613     C***
025300010305
025400010305     C*----------------------------------------------------*
025500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025600010305     C*----------------------------------------------------*
025700020305     C     WRIVAB        BEGSR
025800070730     C*
025900071003     C* Considerazioni finali
026000071003     C                   if        VABRMA = *blanks
026100071003     C                   movel(P)  VABRMN        VABRMA
026200071003     C                   endif
026300071003     C*
026400021113     C                   write     fivab000                                     => scarico il VAB
026500010305     C*
026600010305     C                   ENDSR
026700990920
026800000801     C*----------------------------------------------------*
026900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027000000801     C*----------------------------------------------------*
027100010201     C     INZVAR        BEGSR
027200000801     C*
027300010201     C                   Z-ADD     *zeros        Num5_0
027400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027500020725     C                   MOVEL     '0'           FlgCAS            1
027600000801     C*
027700000801     C                   ENDSR
027800000801     C*----------------------------------------------------*
027900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028000000801     C*----------------------------------------------------*
028100020904     C     DEFCAM        BEGSR
028200080923     C*
028300080923     C                   SETOFF                                       5051
028400000801     C*
028500021113     C                   CLEAR                   FIVAB000
028600021113     C                   CLEAR                   FIVAT000
028700070730     C* Imposto i valori di default...
028800160610     C                   EVAL      VABCCM = 0058437
028900160610     C                   EVAL      VATCCM = 0058437
029000160610     C                   EVAL      VABLNP = 005
029100160610     C                   EVAL      VATLNP = 005
029200160610     C                   EVAL      VABCTR = 003
029300130121     C                   EVAL      VABCTM = '7Q'
029400130121     C                   EVAL      VABCBO = '1'
029500070222     C* ... e poi verifico se sono stati passati come parametri
029600070222     C                   IF        vlrppt > *blanks
029700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029900070222     C                   EXSR      CHKNUM
030000070222     C                   IF        PiInt=*on
030100070222     C                   Z-ADD     PiVal         VABCCM
030200070222     C                   Z-ADD     PiVal         VATCCM
030300070222     C                   ENDIF
030400070222     C                   ENDIF
030500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030700070222     C                   EXSR      CHKNUM
030800070222     C                   IF        PiInt=*on
030900070222     C                   Z-ADD     PiVal         VABLNP
031000070222     C                   Z-ADD     PiVal         VATLNP
031100070222     C                   ENDIF
031200070222     C                   ENDIF
031300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABCTR
031800070222     C                   ENDIF
031900070928     C                   ENDIF
032000071009     C                   MOVEL     *blanks       wDISK             1
032100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032200071009     C                   IF        wDISK <> *blanks
032300070928     C                   SETON                                        50
032400070222     C                   ENDIF
032500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032600080923     C                   SETON                                        51
032700080923     C                   ENDIF
032800070222     C                   ENDIF
032900071009     C*
033000071009     C   50              EVAL      VABCTM = '7Q'
033100000801     C*
033200000801     C                   ENDSR
033300000801     C*----------------------------------------------------*
033400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500000801     C*----------------------------------------------------*
033600010201     C     IMPVAB        BEGSR
033700070530     C*
033800070530     C                   SETOFF                                       3132
033900070928     C*
034000070928     C                   EXSR      INZVAR
034100070928     C                   EXSR      DEFCAM
034200070928     C*
034300070928     C                   MOVE(P)   vlrpoi        VABFGS
034400070928     C                   MOVE(P)   vlrpoi        VATFGS
034500070928     C*
034600070928     C                   MOVEL     datcor        VABAAS
034700070928     C                   MOVEL     datcor        VATAAS
034800070928     C                   MOVE      datcor        VABMGS
034900130121     C*
035000130121     C* Reperimento campi ALFA
035100160610     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
035200160610     C                   EVAL      VABRSD=%trim(%subst(vindta:191:30))
035300160610     C                   EVAL      VABIND=%trim(%subst(vindta:221:30))
035400160610     C                   EVAL      VABLOD=%trim(%subst(vindta:251:30))
035500160610     C                   EVAL      VABCAD=%trim(%subst(vindta:286:8))
035600160628     C                   EVAL      VABPRD=%trim(%subst(vindta:281:5))
035700160628     C                   MOVEL     *blanks       wNOTE            70
035800160628     C                   EVAL      wNOTE =%trim(%subst(vindta:358:40))+' '+
035900160628     C                                    %trim(%subst(vindta:398:60))
036000160628     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
036100160628     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
036200160628     C                   EVAL      VABRD2=%trim(%subst(vindta:588:30))
036300160610     C                   SELECT
036400160610     C                   WHEN      %subst(vindta:356:1)='1'
036500160610     C                   EVAL      VABTCR = *blank
036600160610     C                   WHEN      %subst(vindta:356:1)='3'
036700160610     C                   EVAL      VABTCR = 'P'
036800160610     C                   WHEN      %subst(vindta:356:1)='4'
036900160610     C                   EVAL      VABTCR = 'D'
037000160610     C                   ENDSL
037100130121     C*
037200130121     C* Considerazioni sulla ragione sociale del destinatario da indicare
037300130121     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037400130121     C     '@':'A'       XLATE     VABRSD        VABRSD
037500130121     C* ==
037600130121     C*
037700130121     C* Reperimento campi NUMERICI
037800130121     C                   MOVEL     DATCOR        VABAAS
037900130121     C                   MOVE      DATCOR        VABMGS
038000130121     C* NSP
038100160610
038200160624     C* --------------------------------------------------------------------------------------------
038300160624     C* INVECE CHE LA PARTE NUMERICA FINALE DEVO CONSIDERARE TUTTA LA PARTE NUMERICA E CHE LA
038400160624     C* PARTE NON NUMERICA E' L'UNDERSCORE (_)
038500160610     C* devo estrarre la parte numerica finale di un pezzo di stringa della vindta
038600160610     C* per cui trovo la prima pos. <> da blank partendo dal fondo della sottostringa
038700160624     C***                EVAL      $X    =
038800160624     C***                          %checkr(' ':%subst(vindta:11:20))
038900160610     C* poi da lì devo trovare la prima pos non numerica
039000160624     C***                EVAL      $Y    =
039100160624     C***                           %checkr(cifre:%subst(vindta:11:20):$X)
039200160624     C***                EVAL      PiStr=%subst(vindta:$Y+1+10:$X-$Y)
039300160624     C* --------------------------------------------------------------------------------------------
039400160610
039500160624     C                   EVAL      PiStr=
039600160624     C                              %scanrpl('_':'':
039700160624     C                              %trim(%subst(vindta:11:20)))
039800160624
039900130121     C                   EXSR      CHKNUM
040000130121     C                   IF        PiInt=*on
040100130121     C                   Z-ADD     PiVal         VABNSP
040200130121     C                   ELSE
040300130121     C                   SETON                                        31
040400130121     C                   Z-ADD     *zeros        VABNSP
040500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600130121     C                             + ' ' + 'VABNSP'
040700130121     C                   ENDIF
040800130121     C* RMN
040900160610     C                   IF        %subst(vindta:39:20)<>*blank
041000160610     C                   EVAL      PiStr=%trim(%subst(vindta:39:20))
041100130121     C                   EXSR      CHKNUM
041200130121     C                   IF        PiInt=*on
041300130121     C                   Z-ADD     PiVal         VABRMN
041400130121     C                   ELSE
041500130121     C                   SETON                                        31
041600130121     C                   Z-ADD     1             VABRMN
041700130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041800130121     C                             + ' ' + 'VABRMN'
041900130121     C                   ENDIF
042000160610     C                   ELSE
042100160610     C                   Z-ADD     VABNSP        VABRMN
042200160610     C                   ENDIF
042300130121     C* Reperisco la provincia dal CAP e dalla località
042400130121     C                   IF        VABCAD <> *blanks AND
042500130121     C                             VABNZD  = *blanks AND
042600130121     C                             VABPRD  = *blanks
042700130121     C                   CLEAR                   TISI95DS
042800130121     C                   EVAL      I95TCN = '3'
042900130121     C                   Z-ADD     datcor        I95DAT
043000130121     C                   EVAL      I95CAP = VABCAD
043100130121     C                   EVAL      I95LOC = VABLOD
043200130121     C                   EVAL      I95NAR = VABNZD
043300130121     C                   CALL      'TISI95R'
043400130121     C                   PARM                    TISI95DS
043500130121     C                   EVAL      VABPRD = O95PRV
043600130121     C                   ENDIF
043700130121     C* NCL
043800160610     C                   IF        %subst(vindta:306:5)<>'00000' and
043900160610     C                             %subst(vindta:306:5)<>*blank
044000160610     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
044100130121     C                   EXSR      CHKNUM
044200130121     C                   IF        PiInt=*on
044300130121     C                   Z-ADD     PiVal         VABNCL
044400130121     C                   ELSE
044500130121     C                   SETON                                        32
044600130121     C                   Z-ADD     *zeros        VABNCL
044700130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044800130121     C                             + ' ' + 'VABNCL'
044900130121     C                   ENDIF
045000130121     C                   ENDIF
045100160610     C* DCR/HCR
045200160610     C                   IF        %subst(vindta:356:1)<>*blank
045300160610     C                   IF        %subst(vindta:344:8)<>'00000000' and
045400160610     C                             %subst(vindta:344:8)<>*blanks
045500160610     C                   EVAL      PiStr=%trim(%subst(vindta:344:8))
045600160322     C                   EXSR      CHKNUM
045700160322     C                   IF        PiInt=*on
045800160322     C                   Z-ADD     PiVal         VABDCR
045900160322     C                   ELSE
046000160322     C                   SETON                                        32
046100160322     C                   Z-ADD     *zeros        VABDCR
046200160322     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300160322     C                             + ' ' + 'VABDCR'
046400160322     C                   ENDIF
046500160322     C                   ENDIF
046600160610     C                   IF        %subst(vindta:352:4)<>'0000' and
046700160610     C                             %subst(vindta:352:4)<>*blanks
046800160610     C                   EVAL      PiStr=%trim(%subst(vindta:352:4))
046900160610     C                   EXSR      CHKNUM
047000160610     C                   IF        PiInt=*on
047100160610     C                   Z-ADD     PiVal         VABHCR
047200160610     C                   ELSE
047300160610     C                   SETON                                        32
047400160610     C                   Z-ADD     *zeros        VABHCR
047500160610     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600160610     C                             + ' ' + 'VABHCR'
047700160610     C                   ENDIF
047800160610     C                   ENDIF
047900160610     C                   ENDIF
048000130121     C* PKB
048100160610     C                   EVAL      PiStr=%trim(%subst(vindta:299:7))
048200130121     C                   EXSR      CHKNUM
048300130121     C                   IF        PiNum=*on
048400160610     C                   EVAL(H)   VABPKB = PiVal
048500130121     C                   ELSE
048600130121     C                   SETON                                        32
048700130121     C                   Z-ADD     *zeros        VABPKB
048800130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900130121     C                             + ' ' + 'VABPKB'
049000130121     C                   ENDIF
049100130121     C* CAS
049200160610     C                   IF        %subst(vindta:316:13)<>*zeros and
049300160610     C                             %subst(vindta:316:13)<>*blank
049400130121     C                   EVAL      FlgCAS = '1'
049500130121     C                   EVAL      VABVCA = 'EUR'
049600160726     C                   EVAL      VABTIC = 'OM'
049700160726     C                   EVAL      VABRMO = 'MALFATTI & TACCHINI'
049800160610     C                   EVAL      PiStr=%trim(%subst(vindta:316:13))
049900130121     C                   EXSR      CHKNUM
050000130121     C                   IF        PiNum=*on
050100160610     C                   EVAL(H)   VABCAS = PiVal
050200130121     C                   ELSE
050300130121     C                   SETON                                        32
050400130121     C                   Z-ADD     *zeros        VABCAS
050500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050600130121     C                             + ' ' + 'VABCAS'
050700130121     C                   ENDIF
050800130121     C                   ENDIF
050900070730     C*
051000070730     C* Considerazioni finali su CBO/CAS
051100070730     C                   IF        FlgCAS = '1'
051200070730     C                   IF        VABCBO = '1'
051300070730     C                   EVAL      VABCBO = '4'
051400070730     C                   ENDIF
051500070730     C                   IF        VABCBO = '2'
051600070730     C                   EVAL      VABCBO = '6'
051700070730     C                   ENDIF
051800070730     C                   ENDIF
051900020305     C*
052000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052100011113     C                   EXSR      CHKIMPDIV
052200010202     C*
052300000801     C* Ebbene...
052400000801     C                   ADD       1             §CTRMO
052500070530     C                   IF        *in31 <> *zeros OR
052600070530     C                             *in32 <> *zeros
052700000801     C                   ADD       1             §CTRNO
052800020725     C                   EVAL      x_vinflg = '2'
052900000801     C                   ELSE
053000010201     C                   ADD       1             §CTROKVB
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400160610     C*----------------------------------------------------*
053500160610     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A" o "S"
053600160610     C*----------------------------------------------------*
053700160610     C     WRIVAT_AS     BEGSR
053800160610     C*
053900160610     C                   IF        %subst(vindta:458:40) <> *blank
054000160610     C                   eval      vatAAS = vabAAS
054100160610     C                   eval      vatLNP = vabLNP
054200160610     C                   eval      vatNRS = vabNRS
054300160610     C                   eval      vatNSP = vabNSP
054400160610     C*
054500160610     C* - TIPO RECORD "S"
054600160610     C                   eval      VATNOT = *blank
054700160610     C                   eval      VATTRC = 'S'
054800160610     C* controllo se il contenuto della sottostringa è numerico
054900160610     C                   monitor
055000160610     C                   eval      VATNOT = %char(
055100160610     C                             %dec(%trim(%subst(vindta:458:40)) : 16 : 0)
055200160610     C                             )
055300160610     C* se mi dà errore, allora
055400160610     C* - TIPO RECORD "A"
055500160610     C                   on-error
055600160610     C                   eval      VATTRC = 'A'
055700160610     C                   eval      VATNOT = %trim(%subst(vindta:458:40))
055800160610     C                   endmon
055900160610     C                   write     FIVAT000
056000160610     C                   ENDIF
056100160610     C*
056200160610     C                   ENDSR
056300010201     C*----------------------------------------------------*
056400160610     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
056500010201     C*----------------------------------------------------*
056600130121     C     WRIVAT_IJ     BEGSR
056700090204     C*
056800090204     C                   eval      vatAAS = vabAAS
056900090204     C                   eval      vatLNP = vabLNP
057000090204     C                   eval      vatNRS = vabNRS
057100090204     C                   eval      vatNSP = vabNSP
057200010201     C*
057300101007     C* Valorizzo il buffer di scrittura del FIVAT
057400160610     C                   eval      w70 = %trim(%subst(vindta:694:60))
057500130121     C* - TIPO RECORD "I"
057600130121     C                   if        %subst(w70:1:35) <> *blanks
057700130121     C                   eval      VATTRC = 'I'
057800130121     C                   eval      VATNOT = %trim(%subst(w70:1:35))
057900021113     C                   write     FIVAT000
058000020725     C                   endif
058100130121     C* - TIPO RECORD "J"
058200130121     C                   if        %subst(w70:36:35) <> *blanks
058300130121     C                   eval      VATTRC = 'J'
058400130121     C                   eval      VATNOT = %trim(%subst(w70:36:35))
058500130121     C                   write     FIVAT000
058600130121     C                   endif
058700010201     C*
058800010201     C                   ENDSR
058900071003     C*----------------------------------------------------*
059000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059100071003     C*----------------------------------------------------*
059200071003     C     WRIVAT_E      BEGSR
059300090204     C*
059400090204     C                   eval      vatAAS = vabAAS
059500090204     C                   eval      vatLNP = vabLNP
059600090204     C                   eval      vatNRS = vabNRS
059700090204     C                   eval      vatNSP = vabNSP
059800071003     C*
059900101007     C* Valorizzo il buffer di scrittura del FIVAT
060000071003     C* - TIPO RECORD "E"
060100071003     C                   if        %subst(vindta:331:26) <> *blanks
060200071003     C                   eval      VATTRC = 'E'
060300130121     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
060400071003     C                   write     FIVAT000
060500071003     C                   endif
060600071003     C*
060700071003     C                   ENDSR
060800020904
060900020904
061000010202     C*----------------------------------------------------*
061100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061200010202     C*----------------------------------------------------*
061300020305     C     PREVAT        BEGSR
061400010202     C*
061500021113     C* Compongo il nome del membro da dare al FIVATWWR
061600010202     C                   eval      parmbr = vlrhdl
061700010202     C                   movel     'M'           parmbr
061800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061900010202     C                   eval      paropz = '1'
062000010202     C* Effettuo la chiamata al CLLE preposto
062100050128     C                   call(e)   'TITVVTC'
062200010202     C                   parm                    parccm
062300010202     C                   parm                    parmbr
062400010202     C                   parm                    paropz
062500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062600010202     C                   if        %error
062700010202     C                   movel     '1'           chkcall
062800010202     C                   else
062900010202     C                   movel     '0'           chkcall
063000010202     C                   endif
063100010202     C*
063200010202     C                   ENDSR
063300000801     C*----------------------------------------------------*
063400000801     C*  CONTROLLO NUMERICITA' CAMPI
063500000801     C*----------------------------------------------------*
063600000801     C     CHKNUM        BEGSR
063700081113     C*
063800081113     C                   IF        PiDecChr = *blanks
063900121214     C                   EVAL      PiDecChr = ','
064000081113     C                   ENDIF
064100081113     C*
064200081113     C                   callp(e)  UBISNUM_Check(PiStr
064300081113     C                                          :PiDecChr
064400081113     C                                          :PiVal
064500081113     C                                          :PiNum
064600081113     C                                          :PiInt)
064700081113     C*
064800000801     C                   IF        %error
064900000801     C                   EVAL      PiInt=*off
065000000801     C                   ENDIF
065100000801     C*
065200000801     C                   ENDSR
065300000801     C***
065400000801
065500011113
065600011113     C*----------------------------------------------------*
065700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065800011113     C*----------------------------------------------------*
065900011113     C     CHKIMPDIV     BEGSR
066000011113     C*
066100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066200011113     C                   Z-ADD     *zeros        wrkDec            9 9
066300011113     C*
066400011113     C* Come prima cosa effettuo considerazioni sulla divisa
066500011113     C                   IF        vabIAS > *zeros
066600011113     C                   IF        vabVAS <> 'EUR'
066700011113     C                   EVAL      vabVAS =  'ITL'
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C                   IF        vabCAS > *zeros
067200011113     C                   IF        vabVCA <> 'EUR'
067300011113     C                   EVAL      vabVCA =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabVMD > *zeros
067800020305     C                   IF        vabVAD <> 'EUR'
067900011113     C                   EVAL      vabVAD =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068400011113     C                   Z-ADD     vabIAS        wrkDec
068500011113     C                   IF        wrkDec > *zeros
068600011113     C                   IF        vabVAS = 'ITL'
068700011113     C                   EVAL      vabIAS = *zeros
068800011113     C                   ENDIF
068900011113     C                   ENDIF
069000011113     C*
069100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069200011113     C                   Z-ADD     vabCAS        wrkDec
069300011113     C                   IF        wrkDec > *zeros
069400011113     C                   IF        vabVCA = 'ITL'
069500011113     C                   EVAL      vabCAS = *zeros
069600011113     C                   ENDIF
069700011113     C                   ENDIF
069800011113     C*
069900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070000011113     C                   Z-ADD     vabVMD        wrkDec
070100011113     C                   IF        wrkDec > *zeros
070200011113     C                   IF        vabVAD = 'ITL'
070300011113     C                   EVAL      vabVMD = *zeros
070400011113     C                   ENDIF
070500011113     C                   ENDIF
070600011113     C*
070700011113     C                   ENDSR
070800011113     C***
070900011113
071000011113
071100000801
071200000801
071300990920      /TITLE Invio dei dati al punto operativo.
071400010202     C     invio         BEGSR
071500990920     C*
071600021113     C* 1° invio FIVAT
071700010201     C                   reset                   dscmz
071800010201     C                   move      vlrpoi        cmzdst
071900021113     C                   eval      cmzfld = 'FIVATWWR'
072000010201     C                   eval      cmzmbd = vlrhdl
072100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072200021009     C***                if        prmfir = *blanks
072300021113     C                   eval      cmzfla = 'FIVAT00F'
072400021113     C                   eval      cmzmba = 'FIVAT00F'
072500021009     C***                else
072600021009     C***                eval      cmzfla = prmfir
072700021009     C***                eval      cmzmba = prmfir
072800021009     C***                endif
072900010201     C                   eval      cmznrr = *zeros
073000020305     C                   move      §ctrokvt      cmznrr
073100021018     C                   eval      cmzlba = vlrfl1
073200010201     C                   call(e)   'TIS711C'
073300010201     C                   parm                    dscmz
073400010201     C                   parm      *blanks       esito
073500010205     C                   if        %error
073600010205     C                             or cmzerr = '1'
073700010205     C                             or esito  = '1'
073800010205     C                   eval      wrkesito = '3'
073900010205     C                   else
074000010201     C*
074100021113     C* 2° invio FIVAB
074200010201     C                   reset                   dscmz
074300010201     C                   move      vlrpoi        cmzdst
074400010201     C                   eval      cmzfld = vlrfou
074500010201     C                   eval      cmzmbd = vlrhdl
074600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074700021009     C***                if        prmfir = *blanks
074800021113     C                   eval      cmzfla = 'FIVAB00F'
074900021113     C                   eval      cmzmba = 'FIVAB00F'
075000021009     C***                else
075100021009     C***                eval      cmzfla = prmfir
075200021009     C***                eval      cmzmba = prmfir
075300021009     C***                endif
075400010201     C                   eval      cmznrr = *zeros
075500010201     C                   move      §ctrokvb      cmznrr
075600021018     C                   eval      cmzlba = vlrfl1
075700010201     C                   call(e)   'TIS711C'
075800010201     C                   parm                    dscmz
075900010201     C                   parm      *blanks       esito
076000010201     C                   if        %error
076100010201     C                             or cmzerr = '1'
076200010201     C                             or esito  = '1'
076300010201     C                   eval      wrkesito = '3'
076400010201     C                   endif
076500010205     C                   endif
076600990920     C*
076700000613     C                   ENDSR
076800000613     C***
076900070411
077000070411     C     *pssr         BEGSR
077100070411     C*
077200070411     C                   if        %open(tivin00r)
077300070411     C                   close     tivin00r
077400070411     C                   endif
077500070411     C                   if        %open(fivabwwr)
077600070411     C                   close     fivabwwr
077700070411     C                   endif
077800070411     C                   if        %open(fivatwwr)
077900070411     C                   close     fivatwwr
078000070411     C                   endif
078100070411     C*
078200070411     C* Effettuo la chiamata al CLLE preposto
078300070411     C                   call(e)   'TITVVTC'
078400070411     C                   parm                    parccm
078500070411     C                   parm                    parmbr
078600070411     C                   parm      '2'           paropz
078700070411     C*
078800070411     C                   eval      wrkesito = '2'
078900070411     C*
079000070411     C                   seton                                        LR
079100070411     C*
079200070411     C                   ENDSR     '*CANCL'
079300070411     C***
079400070411
079500990910
079600000613     C     *inzsr        BEGSR
079700990910     C*
079800990910     C     *entry        plist
079900990920     C                   parm                    tivlrds
080000990921     C                   parm      wrkesito      esito
080100000724     C                   parm                    prmlit
080200000710     C                   parm                    prmfir
080300000613     C*
080400000830     C* CALCOLA LA DATA CORRENTE
080500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080600100324     C                   eval      datcor = %dec(%date() : *ISO)
080700000830     C*
080800000613     C                   ENDSR
080900000613     C***
