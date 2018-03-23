000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130315     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500111125     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600111125     D fivabds       e ds                  extname(fivab00f)
001700111125     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
001800111125     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400110524     D depspe          s              8    INZ(*blanks)
002500110524     D curspe          s              8    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200000830
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700070530     D INPUT_DS        DS                  INZ
003800101007     D  VINDTA                     2048
003900101007     D  VINFLG                        1
004000110524     D  VINSPE                        8
004100101007     D  VINRRN                        8  0
004200020725     D*
004300080923     D*------------------
004400080923     D* DS REPERIMENTO NUMERATORE
004500080923     D*------------------
004600080923     D trul33ds      e ds                  inz
004700080923     D*------------------
004800080923     D* DS ARCHITETTURA
004900080923     D*------------------
005000080923     D kpjba         e ds                  inz
005100080923     D*------------------
005200081113
005300081113     D*------------------
005400081113     D* LINKING A DEFINIZIONI ESTERNE
005500081113     D*------------------
005600100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005800081113
005900990908
006000010201
006100010201
006200080117     C*
006300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400080117     C
006500080117     C/EXEC SQL
006600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700080117     C/END-EXEC
006800080117     C
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000613     C*
007300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007400000613     C*
007500010202     C* Effettuo la chiamata al CLLE preposto
007600050128     C                   call(e)   'TITVVTC'
007700010202     C                   parm                    parccm
007800010202     C                   parm                    parmbr
007900010202     C                   parm      '2'           paropz
008000070730     C*
008100070730     C* Effettuo lancio TISI95 solo x chiusura
008200070730     C                   CLEAR                   TISI95DS
008300070730     C                   EVAL      I95TLA = 'C'
008400070730     C                   CALL      'TISI95R'
008500070730     C                   PARM                    TISI95DS
008600000616     C*
008700000801     C
008800010201     C                   seton                                        LR
008900990908
009000000801
009100910830     C*--------------------------------------------------------
009200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009300910830     C*--------------------------------------------------------
009400070530     C     RWFILE        BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900021113     C                   if        not %open(fivabwwr)
010000021113     C                   open      fivabwwr
010100990914     C                   endif
010200070530     C*
010300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010400020305     C                   exsr      prevat
010500010201     C*
010600010202     C                   if        chkcall = '0'
010700010202     C*
010800021113     C                   if        not %open(fivatwwr)
010900021113     C                   open      fivatwwr
011000010201     C                   endif
011100080117     C*
011200080117     C                   EXSR      INZVAR
011300080117     C                   EXSR      DEFCAM
011400990910     C*
011500010201     C                   clear                   §CTROKVB          5 0
011600020305     C                   clear                   §CTROKVT          5 0
011700000801     C                   clear                   §CTRMO            5 0
011800000801     C                   clear                   §CTRNO            5 0
011900990910     C*
012000020725     C/EXEC SQL
012100020725     C+ declare C1 cursor for select
012200110524     C+ vindta, vinflg, substr(vindta, 322, 8) as sped, rrn(tivin00r)
012300060223     C+ from tivin00r
012400060223     C+ order by sped
012500020725     C+ for read only
012600020725     C/END-EXEC
012700020725     C
012800020725     C/EXEC SQL
012900020725     C+ open C1
013000020725     C/END-EXEC
013100020725     C
013200020725     C/EXEC SQL
013300070530     C+ Fetch C1 into :INPUT_DS
013400020725     C/END-EXEC
013500020725     C*
013600020725     C                   dow       sqlcod = *zeros
013700990913     C*
013800020725     C                   if        vindta > *blanks
013900000613     C                   add       1             rrnum
014000000801     C*
014100020725     C                   if        vinflg = *blanks
014200020725     C                             or vinflg = '0'
014300020725     C                             or vinflg = '2'
014400000801     C*
014500020725     C                   clear                   x_vinmsg
014600020725     C                   eval      x_vinflg = '1'
014700010305     C*
014800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014900110524     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
015000020305     C                   MOVEL(p)  PiStr         curspe
015100010305     C*
015200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400071003     C                   exsr      impvab
015500111130     C  N31              eval      fivabds_ok = fivabds
015600111130     C  N31              exsr      wrivab
015700071003     C                   exsr      wrivat_b                                     => carico VAT
015800111125     C                   exsr      inzvar
015900071003     C                   else
016000080923     C*
016100071009     C                   if        wDISK = 'D'
016200071009     C                   exsr      impvab
016300111130     C  N31              eval      fivabds_ok = fivabds
016400111130     C  N31              exsr      wrivab
016500071009     C                   exsr      wrivat_b                                     => carico VAT
016600071009     C                   exsr      wrivat_e                                     => carico VAT
016700111125     C                   exsr      inzvar
016800071009     C                   else
016900071009     C*
017000010305     C                   if        depspe = *blanks                             => 1° giro
017100010305     C                   eval      depspe = curspe                              => memorizz. spediz
017200080117     C                   clear                   tivinds
017300020305     C                   exsr      impvab
017400111130     C                   z-add     VABNCL        sav_VABNCL
017500111130     C                   z-add     VABPKB        sav_VABPKB
017600111130     C                   z-add     VABVLB        sav_VABVLB
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800071003     C   50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100111130     C                   z-add     sav_VABNCL    VABNCL
018200111130     C                   z-add     sav_VABPKB    VABPKB
018300111130     C                   z-add     sav_VABVLB    VABVLB
018400111130     C  N31              eval      fivabds_ok = fivabds
018500111125     C  N31              exsr      wrivab
018600111125     C                   exsr      inzvar
018700111125     C                   eval      depspe = curspe                              => memorizz. spediz
018800080117     C                   clear                   tivinds
018900020305     C                   exsr      impvab
019000111130     C                   if        wCntRecSpe = *zeros
019100111130     C                   z-add     VABNCL        sav_VABNCL
019200111130     C                   z-add     VABPKB        sav_VABPKB
019300111130     C                   z-add     VABVLB        sav_VABVLB
019400111130     C                   endif
019500111130     C                   add       1             wCntRecSpe
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800020305     C                   else                                                   => x stessa spediz
019900111130     C                   exsr      impvab
020000111125     C** 51              add       VABNCL        sav_VABNCL
020100111125     C   51              z-add     VABNCL        sav_VABNCL
020200111125     C   51              add       VABPKB        sav_VABPKB
020300111125     C   51              add       VABVLB        sav_VABVLB
020400111125     C   52              z-add     VABNCL        sav_VABNCL
020500111125     C   52              z-add     VABPKB        sav_VABPKB
020600111125     C   52              z-add     VABVLB        sav_VABVLB
020700111125     C   53              add       VABNCL        sav_VABNCL
020800111125     C   53              z-add     VABPKB        sav_VABPKB
020900111125     C   53              z-add     VABVLB        sav_VABVLB
021000111125     C*
021100071003     C                   exsr      wrivat_b                                     => carico VAT
021200071003     C   50              exsr      wrivat_e                                     => carico VAT
021300010305     C                   endif
021400010305     C                   endif
021500010305     C                   endif
021600071003     C                   endif
021700071009     C                   endif
021800000905     C*
021900000905     C                   else
022000020725     C                   eval      x_vinflg = '1'
022100000905     C                   endif
022200000905     C*
022300020725     C     VINRRN        chain     tivin000
022400020725     C                   if        %found(tivin00r)
022500020725     C                   eval      y_vinflg = x_vinflg
022600020725     C                   eval      y_vinmsg = x_vinmsg
022700020725     C                   update    tivin000
022800020725     C                   endif
022900020725     C*
023000020725     C/EXEC SQL
023100070530     C+ Fetch C1 into :INPUT_DS
023200020725     C/END-EXEC
023300020725     C*
023400020725     C                   enddo
023500020725     C*
023600020725     C/EXEC SQL
023700020725     C+ close C1
023800020725     C/END-EXEC
023900000905     C*
024000020305     C* Scarico i VAB rimasti "in sospeso"
024100071009     C                   if        wDISK = 'C'
024200111130     C  N31              eval      fivabds_ok = fivabds
024300111130     C  N31              exsr      wrivab
024400071009     C                   endif
024500010202     C*
024600010202     C                   endif
024700990910
024800990910     C* Se non ci sono record con errori ...
024900000710     C                   if        §ctrno = 0
025000990910     C* ... restituisco esito OK.
025100990921     C                   eval      wrkesito = '0'
025200990910     C                   else
025300010201     C                   if        §ctrokvb > 0
025400990921     C                   eval      wrkesito = '1'
025500000710     C                   else
025600000710     C                   eval      wrkesito = '2'
025700990910     C                   endif
025800000710     C                   endif
025900990910     C*
026000990914     C                   if        %open(tivin00r)
026100990908     C                   close     tivin00r
026200990914     C                   endif
026300021113     C                   if        %open(fivabwwr)
026400021113     C                   close     fivabwwr
026500990914     C                   endif
026600021113     C                   if        %open(fivatwwr)
026700021113     C                   close     fivatwwr
026800010201     C                   endif
026900990910     C*
027000111031     C                   if        §ctrokvb > 0 or
027100111031     C                             §ctrokvt > 0
027200111031     C                             and wPOI <> *zeros
027300010202     C                   exsr      invio
027400990920     C                   endif
027500990920     C*
027600910830     C                   ENDSR
027700000613     C***
027800010305
027900010305     C*----------------------------------------------------*
028000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028100010305     C*----------------------------------------------------*
028200020305     C     WRIVAB        BEGSR
028300070730     C*
028400071003     C* Considerazioni finali
028500111125     C                   if        ok_VABRMA = *blanks
028600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
028700071003     C                   endif
028800071003     C*
028900021113     C                   write     fivab000                                     => scarico il VAB
029000010305     C*
029100010305     C                   ENDSR
029200990920
029300000801     C*----------------------------------------------------*
029400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500000801     C*----------------------------------------------------*
029600010201     C     INZVAR        BEGSR
029700000801     C*
029800010201     C                   Z-ADD     *zeros        Num5_0
029900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030000020725     C                   MOVEL     '0'           FlgCAS            1
030100111125     C*
030200111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
030300111130     C*
030400111125     C                   CLEAR                   FIVABDS
030500111130     C                   CLEAR                   FIVABDS_OK
030600111130     C                   CLEAR                   FIVABDS_SAV
030700111125     C                   CLEAR                   FIVAT000
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031200000801     C*----------------------------------------------------*
031300020904     C     DEFCAM        BEGSR
031400080923     C*
031500111125     C                   SETOFF                                       50
031600111125     C                   SETOFF                                       515253
031700070730     C* Imposto i valori di default...
031800110524     C                   EVAL      VABCCM = 0241160
031900110524     C                   EVAL      VATCCM = 0241160
032000110524     C                   EVAL      VABLNP = 024
032100110524     C                   EVAL      VATLNP = 024
032200101007     C                   EVAL      VABCTR = 000
032300070730     C                   EVAL      VABCBO = '1'
032400110520     C                   EVAL      VABTSP = 'C'
032500070222     C* ... e poi verifico se sono stati passati come parametri
032600070222     C                   IF        vlrppt > *blanks
032700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
032800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032900070222     C                   EXSR      CHKNUM
033000070222     C                   IF        PiInt=*on
033100070222     C                   Z-ADD     PiVal         VABCCM
033200070222     C                   Z-ADD     PiVal         VATCCM
033300070222     C                   ENDIF
033400070222     C                   ENDIF
033500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033700070222     C                   EXSR      CHKNUM
033800070222     C                   IF        PiInt=*on
033900070222     C                   Z-ADD     PiVal         VABLNP
034000070222     C                   Z-ADD     PiVal         VATLNP
034100070222     C                   ENDIF
034200070222     C                   ENDIF
034300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034500070222     C                   EXSR      CHKNUM
034600070222     C                   IF        PiInt=*on
034700070222     C                   Z-ADD     PiVal         VABCTR
034800070222     C                   ENDIF
034900070928     C                   ENDIF
035000071009     C                   MOVEL     *blanks       wDISK             1
035100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035200071009     C                   IF        wDISK <> *blanks
035300070928     C                   SETON                                        50
035400070222     C                   ENDIF
035500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
035600111125     C                   SETON                                        41
035700080923     C                   ENDIF
035800070222     C                   ENDIF
035900071009     C*
036000071009     C   50              EVAL      VABCTM = '7Q'
036100000801     C*
036200000801     C                   ENDSR
036300000801     C*----------------------------------------------------*
036400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036500000801     C*----------------------------------------------------*
036600010201     C     IMPVAB        BEGSR
036700070530     C*
036800070530     C                   SETOFF                                       3132
036900070928     C*
037000070928     C                   EXSR      DEFCAM
037100070928     C*
037200111031     C                   IF        vlrpoi <> 999
037300070928     C                   MOVE(P)   vlrpoi        VABFGS
037400070928     C                   MOVE(P)   vlrpoi        VATFGS
037500111031     C                   Z-ADD     vlrpoi        wPOI              3 0
037600111031     C                   ENDIF
037700070928     C*
037800070928     C                   MOVEL     datcor        VABAAS
037900070928     C                   MOVEL     datcor        VATAAS
038000070928     C                   MOVE      datcor        VABMGS
038100070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
038200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
038400070928     C* ==
038500070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
038600070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
038700070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
038800110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
038900110524     C**+                EVAL      VABRMA=%trim(%subst(vindta:331:10))
039000070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
039100080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
039200140428     C***                EVAL      VABNAS=%trim(%subst(vindta:145:20))
039300140429     C                   EVAL      VABNAS=%trim(%subst(vindta:315:10))
039400070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
039500070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
039600100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
039700100324     C***                EVAL      VABCBO='1'
039800100324     C***                ENDIF
039900100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
040000100324     C***                EVAL      VABCBO='2'
040100100324     C***                ENDIF
040200100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
040300100324     C***                EVAL      VABTSP='E'
040400100324     C***                ENDIF
040500100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
040600100324     C***                EVAL      VABFFD='S'
040700100324     C***                ENDIF
040800100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
040900100324     C***                EVAL      VABTC1='A'
041000100324     C***                ENDIF
041100111028     C* CCM
041200111125     C                   SETOFF                                       51
041300111028     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
041400111028     C                   EXSR      CHKNUM
041500111028     C                   IF        PiInt=*on
041600111028     C                   Z-ADD     PiVal         VABCCM
041700111028     C                   Z-ADD     PiVal         VATCCM
041800111028     C                   ELSE
041900111028     C                   SETON                                        32
042000111028     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042100111028     C                             + ' ' + 'VABCCM VATCCM'
042200111028     C                   ENDIF
042300140922     C                   IF        VABCCM = 0240227 OR
042400140922     C                             VABCCM = 1311537
042500111125     C                   SETON                                        51
042600111125     C                   ENDIF
042700111109     C* CTR
042800111109     C                   IF        VABCCM = 0240227
042900111109     C                   EVAL      VABCTR = 100
043000111109     C                   ENDIF
043100111028     C* LNP
043200160120     C                   EVAL      PiStr=%trim(%subst(vindta:14:3))
043300160120     C                   EXSR      CHKNUM
043400160120     C                   IF        PiInt=*on
043500160120     C                   Z-ADD     PiVal         VABLNP
043600160120     C                   Z-ADD     PiVal         VABFGS
043700160120     C                   Z-ADD     PiVal         VATLNP
043800160120     C                   Z-ADD     PiVal         VATFGS
043900111031     C*
044000160120     C                   IF        vlrpoi = 999
044100160120     C                   Z-ADD     VABFGS        wPOI
044200160120     C                   ENDIF
044300111031     C*
044400160120     C                   ELSE
044500160120     C                   SETON                                        32
044600160120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700160120     C                             + ' ' + 'VABLNP VATLNP'
044800160120     C                   ENDIF
044900071003     C* CAD
045000070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
045100070928     C                   EXSR      CHKNUM
045200070928     C                   IF        PiInt=*on
045300070928     C                   Z-ADD     PiVal         Num5_0
045400070928     C                   MOVEL     Num5_0        VABCAD
045500070928     C                   ELSE
045600070928     C                   SETON                                        32
045700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045800070928     C                             + ' ' + 'VABCAD'
045900070928     C                   ENDIF
046000080923     C* RMN
046100110524     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
046200110524     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
046300110223     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
046400101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
046500080923     C                   EXSR      CHKNUM
046600080923     C                   IF        PiInt=*on
046700080923     C                   Z-ADD     PiVal         VABRMN
046800080923     C                   ELSE
046900080923     C                   SETON                                        32
047000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100080923     C                             + ' ' + 'VABRMN'
047200080923     C                   ENDIF
047300080923     C* NSP
047400111125     C                   IF        *in41 = *on
047500080923     C* NSP => Stacco un numeratore da AZNUM
047600080923     C                   clear                   TRUL33DS
047700080923     C                   eval      I33OPE = *zeros
047800080923     C                   eval      I33CNU = 302
047900080923     C                   eval      I33NUM = 1
048000080923     C                   movel     TRUL33DS      KPJBU
048100080923     C                   call      'TRUL33R'
048200080923     C                   parm                    KPJBA
048300080923     C                   movel     KPJBU         TRUL33DS
048400080923     C                   if        O33ERR = *zeros
048500080923     C                   z-add     O33NRF        VABNSP
048600080923     C                   z-add     O33NRF        VATNSP
048700080923     C                   else
048800080923     C                   SETON                                        31
048900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049000080923     C                             + ' ' + 'VABNSP VATNSP'
049100080923     C                   endif
049200080923     C                   ELSE
049300110525     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
049400110525     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
049500110223     C***                EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
049600080923     C                   EXSR      CHKNUM
049700080923     C                   IF        PiInt=*on
049800080923     C                   Z-ADD     PiVal         VABNSP
049900080923     C                   Z-ADD     PiVal         VATNSP
050000080923     C                   ELSE
050100080923     C                   SETON                                        31
050200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300080923     C                             + ' ' + 'VABNSP VATNSP'
050400080923     C                   ENDIF
050500080923     C                   ENDIF
050600071003     C* NCL
050700070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
050800070928     C                   EXSR      CHKNUM
050900070928     C                   IF        PiInt=*on
051000070928     C                   Z-ADD     PiVal         VABNCL
051100070928     C                   ELSE
051200070928     C                   SETON                                        32
051300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051400070928     C                             + ' ' + 'VABNCL'
051500070928     C                   ENDIF
051600071003     C* PKB
051700110520     C                   IF        %subst(vindta:170:8) <> *blanks
051800070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
051900070928     C                   EXSR      CHKNUM
052000070928     C                   IF        PiNum=*on
052100111125     C                   Z-ADD     PiVal         VABPKB
052200070928     C                   ELSE
052300070928     C                   SETON                                        32
052400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500070928     C                             + ' ' + 'VABPKB'
052600070928     C                   ENDIF
052700110520     C                   ELSE
052800110520     C                   IF        VABCTR = 100
052900110520     C                   EVAL      VABPKB = 18 * VABNCL
053000110520     C                   ELSE
053100110520     C                   EVAL      VABPKB = 10 * VABNCL
053200110520     C                   ENDIF
053300110520     C                   ENDIF
053400080923     C* CAS
053500080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
053600080923     C                             AND
053700080923     C                             %trim(%subst(vindta:280:12))<>*zeros
053800080923     C                             AND
053900080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
054000080923     C                             AND
054100080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
054200080923     C                             AND
054300080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
054400130108     C                             AND
054500130108     C                             %trim(%subst(vindta:280:12))<>'.000'
054600070928     C                   MOVEL     '1'           FlgCAS
054700130315     C                   SELECT
054800130315     C                   WHEN      %trim(%subst(vindta:307:1))='A'
054900130326     C                   EVAL      VABTIC='TM'
055000130315     C                   OTHER
055100130315     C                   EVAL      VABTIC=*blank
055200130315     C                   ENDSL
055300070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
055400070928     C                   EXSR      CHKNUM
055500070928     C                   IF        PiNum=*on
055600070928     C                   Z-ADD     PiVal         VABCAS
055700070928     C                   ELSE
055800070928     C                   SETON                                        32
055900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056000070928     C                             + ' ' + 'VABCAS'
056100070928     C                   ENDIF
056200070928     C                   ENDIF
056300071003     C* IAS
056400080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
056500080923     C                             AND
056600080923     C                             %trim(%subst(vindta:295:12))<>*zeros
056700080923     C                             AND
056800080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
056900080923     C                             AND
057000080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
057100080923     C                             AND
057200080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
057300130108     C                             AND
057400130108     C                             %trim(%subst(vindta:295:12))<>'.000'
057500080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
057600080923     C                   EXSR      CHKNUM
057700080923     C                   IF        PiNum=*on
057800080923     C                   Z-ADD     PiVal         VABIAS
057900080923     C                   ELSE
058000080923     C                   SETON                                        32
058100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058200080923     C                             + ' ' + 'VABIAS'
058300080923     C                   ENDIF
058400071003     C                   ENDIF
058500070730     C*
058600070730     C* Considerazioni finali su CBO/CAS
058700070730     C                   IF        FlgCAS = '1'
058800070730     C                   IF        VABCBO = '1'
058900070730     C                   EVAL      VABCBO = '4'
059000070730     C                   ENDIF
059100070730     C                   IF        VABCBO = '2'
059200070730     C                   EVAL      VABCBO = '6'
059300070730     C                   ENDIF
059400070730     C                   ENDIF
059500020305     C*
059600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
059700011113     C                   EXSR      CHKIMPDIV
059800010202     C*
059900000801     C* Ebbene...
060000000801     C                   ADD       1             §CTRMO
060100070530     C                   IF        *in31 <> *zeros OR
060200070530     C                             *in32 <> *zeros
060300000801     C                   ADD       1             §CTRNO
060400020725     C                   EVAL      x_vinflg = '2'
060500000801     C                   ELSE
060600010201     C                   ADD       1             §CTROKVB
060700000801     C                   ENDIF
060800000801     C*
060900000801     C                   ENDSR
061000010201     C*----------------------------------------------------*
061100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061200010201     C*----------------------------------------------------*
061300071003     C     WRIVAT_B      BEGSR
061400090204     C*
061500090204     C                   eval      vatAAS = vabAAS
061600090204     C                   eval      vatLNP = vabLNP
061700090204     C                   eval      vatNRS = vabNRS
061800090204     C                   eval      vatNSP = vabNSP
061900010201     C*
062000101007     C* Valorizzo il buffer di scrittura del FIVAT
062100070928     C* - TIPO RECORD "B"
062200070928     C                   if        %subst(vindta:92:15) <> *blanks
062300070928     C                   eval      VATTRC = 'B'
062400070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
062500021113     C                   write     FIVAT000
062600111031     C                   add       1             §ctrokvt
062700020725     C                   endif
062800010201     C*
062900010201     C                   ENDSR
063000071003     C*----------------------------------------------------*
063100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063200071003     C*----------------------------------------------------*
063300071003     C     WRIVAT_E      BEGSR
063400090204     C*
063500090204     C                   eval      vatAAS = vabAAS
063600090204     C                   eval      vatLNP = vabLNP
063700090204     C                   eval      vatNRS = vabNRS
063800090204     C                   eval      vatNSP = vabNSP
063900071003     C*
064000101007     C* Valorizzo il buffer di scrittura del FIVAT
064100071003     C* - TIPO RECORD "E"
064200071003     C                   if        %subst(vindta:331:26) <> *blanks
064300071003     C                   eval      VATTRC = 'E'
064400071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
064500071003     C                   write     FIVAT000
064600111031     C                   add       1             §ctrokvt
064700071003     C                   endif
064800071003     C*
064900071003     C                   ENDSR
065000020904
065100020904
065200010202     C*----------------------------------------------------*
065300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065400010202     C*----------------------------------------------------*
065500020305     C     PREVAT        BEGSR
065600010202     C*
065700021113     C* Compongo il nome del membro da dare al FIVATWWR
065800010202     C                   eval      parmbr = vlrhdl
065900010202     C                   movel     'M'           parmbr
066000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066100010202     C                   eval      paropz = '1'
066200010202     C* Effettuo la chiamata al CLLE preposto
066300050128     C                   call(e)   'TITVVTC'
066400010202     C                   parm                    parccm
066500010202     C                   parm                    parmbr
066600010202     C                   parm                    paropz
066700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066800010202     C                   if        %error
066900010202     C                   movel     '1'           chkcall
067000010202     C                   else
067100010202     C                   movel     '0'           chkcall
067200010202     C                   endif
067300010202     C*
067400010202     C                   ENDSR
067500000801     C*----------------------------------------------------*
067600000801     C*  CONTROLLO NUMERICITA' CAMPI
067700000801     C*----------------------------------------------------*
067800000801     C     CHKNUM        BEGSR
067900081113     C*
068000081113     C                   IF        PiDecChr = *blanks
068100110524     C                   EVAL      PiDecChr = '.'
068200081113     C                   ENDIF
068300081113     C*
068400081113     C                   callp(e)  UBISNUM_Check(PiStr
068500081113     C                                          :PiDecChr
068600081113     C                                          :PiVal
068700081113     C                                          :PiNum
068800081113     C                                          :PiInt)
068900081113     C*
069000000801     C                   IF        %error
069100000801     C                   EVAL      PiInt=*off
069200000801     C                   ENDIF
069300000801     C*
069400000801     C                   ENDSR
069500000801     C***
069600000801
069700011113
069800011113     C*----------------------------------------------------*
069900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070000011113     C*----------------------------------------------------*
070100011113     C     CHKIMPDIV     BEGSR
070200011113     C*
070300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070400011113     C                   Z-ADD     *zeros        wrkDec            9 9
070500011113     C*
070600011113     C* Come prima cosa effettuo considerazioni sulla divisa
070700011113     C                   IF        vabIAS > *zeros
070800011113     C                   IF        vabVAS <> 'EUR'
070900011113     C                   EVAL      vabVAS =  'ITL'
071000011113     C                   ENDIF
071100011113     C                   ENDIF
071200011113     C*
071300011113     C                   IF        vabCAS > *zeros
071400130327     C* siccome il cliente passa, a volte, questo dato vuoto per intendere EUR,
071500130327     C* se il c/assegno è valorizzato ma la divisa è vuota, la forzo a EUR
071600130327     C                   IF        vabVCA = *blank
071700130327     C                   EVAL      vabVCA =  'EUR'
071800130327     C                   ENDIF
071900011113     C                   IF        vabVCA <> 'EUR'
072000011113     C                   EVAL      vabVCA =  'ITL'
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C                   IF        vabVMD > *zeros
072500020305     C                   IF        vabVAD <> 'EUR'
072600011113     C                   EVAL      vabVAD =  'ITL'
072700011113     C                   ENDIF
072800011113     C                   ENDIF
072900011113     C*
073000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
073100011113     C                   Z-ADD     vabIAS        wrkDec
073200011113     C                   IF        wrkDec > *zeros
073300011113     C                   IF        vabVAS = 'ITL'
073400011113     C                   EVAL      vabIAS = *zeros
073500011113     C                   ENDIF
073600011113     C                   ENDIF
073700011113     C*
073800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073900011113     C                   Z-ADD     vabCAS        wrkDec
074000011113     C                   IF        wrkDec > *zeros
074100011113     C                   IF        vabVCA = 'ITL'
074200011113     C                   EVAL      vabCAS = *zeros
074300011113     C                   ENDIF
074400011113     C                   ENDIF
074500011113     C*
074600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074700011113     C                   Z-ADD     vabVMD        wrkDec
074800011113     C                   IF        wrkDec > *zeros
074900011113     C                   IF        vabVAD = 'ITL'
075000011113     C                   EVAL      vabVMD = *zeros
075100011113     C                   ENDIF
075200011113     C                   ENDIF
075300011113     C*
075400011113     C                   ENDSR
075500011113     C***
075600011113
075700011113
075800000801
075900000801
076000990920      /TITLE Invio dei dati al punto operativo.
076100010202     C     invio         BEGSR
076200990920     C*
076300021113     C* 1° invio FIVAT
076400010201     C                   reset                   dscmz
076500111031     C                   move      wPOI          cmzdst
076600021113     C                   eval      cmzfld = 'FIVATWWR'
076700010201     C                   eval      cmzmbd = vlrhdl
076800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076900021009     C***                if        prmfir = *blanks
077000021113     C                   eval      cmzfla = 'FIVAT00F'
077100021113     C                   eval      cmzmba = 'FIVAT00F'
077200021009     C***                else
077300021009     C***                eval      cmzfla = prmfir
077400021009     C***                eval      cmzmba = prmfir
077500021009     C***                endif
077600010201     C                   eval      cmznrr = *zeros
077700020305     C                   move      §ctrokvt      cmznrr
077800021018     C                   eval      cmzlba = vlrfl1
077900010201     C                   call(e)   'TIS711C'
078000010201     C                   parm                    dscmz
078100010201     C                   parm      *blanks       esito
078200010205     C                   if        %error
078300010205     C                             or cmzerr = '1'
078400010205     C                             or esito  = '1'
078500010205     C                   eval      wrkesito = '3'
078600010205     C                   else
078700010201     C*
078800021113     C* 2° invio FIVAB
078900010201     C                   reset                   dscmz
079000111031     C                   move      wPOI          cmzdst
079100010201     C                   eval      cmzfld = vlrfou
079200010201     C                   eval      cmzmbd = vlrhdl
079300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079400021009     C***                if        prmfir = *blanks
079500021113     C                   eval      cmzfla = 'FIVAB00F'
079600021113     C                   eval      cmzmba = 'FIVAB00F'
079700021009     C***                else
079800021009     C***                eval      cmzfla = prmfir
079900021009     C***                eval      cmzmba = prmfir
080000021009     C***                endif
080100010201     C                   eval      cmznrr = *zeros
080200010201     C                   move      §ctrokvb      cmznrr
080300021018     C                   eval      cmzlba = vlrfl1
080400010201     C                   call(e)   'TIS711C'
080500010201     C                   parm                    dscmz
080600010201     C                   parm      *blanks       esito
080700010201     C                   if        %error
080800010201     C                             or cmzerr = '1'
080900010201     C                             or esito  = '1'
081000010201     C                   eval      wrkesito = '3'
081100010201     C                   endif
081200010205     C                   endif
081300990920     C*
081400000613     C                   ENDSR
081500000613     C***
081600070411
081700070411     C     *pssr         BEGSR
081800070411     C*
081900070411     C                   if        %open(tivin00r)
082000070411     C                   close     tivin00r
082100070411     C                   endif
082200070411     C                   if        %open(fivabwwr)
082300070411     C                   close     fivabwwr
082400070411     C                   endif
082500070411     C                   if        %open(fivatwwr)
082600070411     C                   close     fivatwwr
082700070411     C                   endif
082800070411     C*
082900070411     C* Effettuo la chiamata al CLLE preposto
083000070411     C                   call(e)   'TITVVTC'
083100070411     C                   parm                    parccm
083200070411     C                   parm                    parmbr
083300070411     C                   parm      '2'           paropz
083400070411     C*
083500070411     C                   eval      wrkesito = '2'
083600070411     C*
083700070411     C                   seton                                        LR
083800070411     C*
083900070411     C                   ENDSR     '*CANCL'
084000070411     C***
084100070411
084200990910
084300000613     C     *inzsr        BEGSR
084400990910     C*
084500990910     C     *entry        plist
084600990920     C                   parm                    tivlrds
084700990921     C                   parm      wrkesito      esito
084800000724     C                   parm                    prmlit
084900000710     C                   parm                    prmfir
085000000613     C*
085100000830     C* CALCOLA LA DATA CORRENTE
085200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085300100324     C                   eval      datcor = %dec(%date() : *ISO)
085400000830     C*
085500000613     C                   ENDSR
085600000613     C***
