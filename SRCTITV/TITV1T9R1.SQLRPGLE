000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021113     FFIVABwwr  O    E             DISK    usropn
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
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100070928     D depspe          s             16    INZ(*blanks)
002200070928     D curspe          s             16    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900020725
003000020725     D*------------------
003100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003200020725     D*------------------
003300070530     D INPUT_DS        DS                  INZ
003400020725     D  VINDTA                 1   2048
003500020725     D  VINFLG              2049   2049
003600020725     D  VINSPE              2050   2059
003700020725     D  VINRRN              2060   2067  0
003800020725     D*
003900081113
004000091223     D*------------------
004100091223     D* DS REPERIMENTO NUMERATORE
004200091223     D*------------------
004300091223     D trul33ds      e ds                  inz
004400091223     D*------------------
004500091223     D* DS ARCHITETTURA
004600091223     D*------------------
004700091223     D kpjba         e ds                  inz
004800091223
004900081113
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005500081113
005600990908
005700010201
005800010201
005900080117     C*
006000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100080117     C
006200080117     C/EXEC SQL
006300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400080117     C/END-EXEC
006500080117     C
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000613     C*
007000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007100000613     C*
007200010202     C* Effettuo la chiamata al CLLE preposto
007300050128     C                   call(e)   'TITVVTC'
007400010202     C                   parm                    parccm
007500010202     C                   parm                    parmbr
007600010202     C                   parm      '2'           paropz
007700070730     C*
007800070730     C* Effettuo lancio TISI95 solo x chiusura
007900070730     C                   CLEAR                   TISI95DS
008000070730     C                   EVAL      I95TLA = 'C'
008100070730     C                   CALL      'TISI95R'
008200070730     C                   PARM                    TISI95DS
008300000616     C*
008400000801     C
008500010201     C                   seton                                        LR
008600990908
008700000801
008800910830     C*--------------------------------------------------------
008900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009000910830     C*--------------------------------------------------------
009100070530     C     RWFILE        BEGSR
009200990910     C*
009300990914     C                   if        not %open(tivin00r)
009400990908     C                   open      tivin00r
009500990914     C                   endif
009600021113     C                   if        not %open(fivabwwr)
009700021113     C                   open      fivabwwr
009800990914     C                   endif
009900070530     C*
010000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010100020305     C                   exsr      prevat
010200010201     C*
010300010202     C                   if        chkcall = '0'
010400010202     C*
010500021113     C                   if        not %open(fivatwwr)
010600021113     C                   open      fivatwwr
010700010201     C                   endif
010800080117     C*
010900080117     C                   EXSR      INZVAR
011000080117     C                   EXSR      DEFCAM
011100990910     C*
011200010201     C                   clear                   §CTROKVB          5 0
011300020305     C                   clear                   §CTROKVT          5 0
011400000801     C                   clear                   §CTRMO            5 0
011500000801     C                   clear                   §CTRNO            5 0
011600990910     C*
011700020725     C/EXEC SQL
011800020725     C+ declare C1 cursor for select
011900130220     C+ vindta, vinflg, substr(vindta, 197, 70) as sped, rrn(tivin00r)
012000060223     C+ from tivin00r
012100060223     C+ order by sped
012200020725     C+ for read only
012300020725     C/END-EXEC
012400020725     C
012500020725     C/EXEC SQL
012600020725     C+ open C1
012700020725     C/END-EXEC
012800020725     C
012900020725     C/EXEC SQL
013000070530     C+ Fetch C1 into :INPUT_DS
013100020725     C/END-EXEC
013200020725     C*
013300020725     C                   dow       sqlcod = *zeros
013400990913     C*
013500020725     C                   if        vindta > *blanks
013600000613     C                   add       1             rrnum
013700000801     C*
013800020725     C                   if        vinflg = *blanks
013900020725     C                             or vinflg = '0'
014000020725     C                             or vinflg = '2'
014100000801     C*
014200020725     C                   clear                   x_vinmsg
014300020725     C                   eval      x_vinflg = '1'
014400010305     C*
014500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014600121219     C***                EVAL      PiStr=%trim(%subst(vindta:315:16))
014700121219     C                   EVAL      PiStr=%trim(%subst(vindta:197:70))
014800020305     C                   MOVEL(p)  PiStr         curspe
014900010305     C*
015000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015100080923     C                   if        *in50 = *off
015200091223     C                   exsr      repNSP
015300071003     C                   exsr      impvab
015400071003     C                   exsr      wrivab
015500071003     C                   exsr      wrivat_b                                     => carico VAT
015600071003     C                   else
015700080923     C*
015800071009     C                   if        wDISK = 'D'
015900091223     C                   exsr      repNSP
016000071009     C                   exsr      impvab
016100071009     C                   exsr      wrivab
016200071009     C                   exsr      wrivat_b                                     => carico VAT
016300071009     C                   exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900091223     C                   exsr      repNSP
017000020305     C                   exsr      impvab
017100071003     C                   exsr      wrivat_b                                     => carico VAT
017200071003     C   50              exsr      wrivat_e                                     => carico VAT
017300010305     C                   else
017400020725     C                   if        depspe <> curspe                             => rottura di spediz
017500010305     C                   eval      depspe = curspe                              => memorizz. spediz
017600070928     C                   exsr      wrivab
017700080117     C                   clear                   tivinds
017800091223     C                   exsr      repNSP
017900020305     C                   exsr      impvab
018000071003     C                   exsr      wrivat_b                                     => carico VAT
018100071003     C   50              exsr      wrivat_e                                     => carico VAT
018200020305     C                   else                                                   => x stessa spediz
018300020305     C                   exsr      impvab
018400071003     C                   exsr      wrivat_b                                     => carico VAT
018500071003     C   50              exsr      wrivat_e                                     => carico VAT
018600010305     C                   endif
018700010305     C                   endif
018800010305     C                   endif
018900071003     C                   endif
019000071009     C                   endif
019100000905     C*
019200000905     C                   else
019300020725     C                   eval      x_vinflg = '1'
019400000905     C                   endif
019500000905     C*
019600020725     C     VINRRN        chain     tivin000
019700020725     C                   if        %found(tivin00r)
019800020725     C                   eval      y_vinflg = x_vinflg
019900020725     C                   eval      y_vinmsg = x_vinmsg
020000020725     C                   update    tivin000
020100020725     C                   endif
020200020725     C*
020300020725     C/EXEC SQL
020400070530     C+ Fetch C1 into :INPUT_DS
020500020725     C/END-EXEC
020600020725     C*
020700020725     C                   enddo
020800020725     C*
020900020725     C/EXEC SQL
021000020725     C+ close C1
021100020725     C/END-EXEC
021200000905     C*
021300020305     C* Scarico i VAB rimasti "in sospeso"
021400071009     C                   if        wDISK = 'C'
021500071009     C                   exsr      wrivab
021600071009     C                   endif
021700010202     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500021113     C                   if        %open(fivabwwr)
023600021113     C                   close     fivabwwr
023700990914     C                   endif
023800021113     C                   if        %open(fivatwwr)
023900021113     C                   close     fivatwwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900010305
025000010305     C*----------------------------------------------------*
025100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025200010305     C*----------------------------------------------------*
025300020305     C     WRIVAB        BEGSR
025400070730     C*
025500071003     C* Considerazioni finali
025600071003     C                   if        VABRMA = *blanks
025700071003     C                   movel(P)  VABRMN        VABRMA
025800071003     C                   endif
025900071003     C*
026000021113     C                   write     fivab000                                     => scarico il VAB
026100010305     C*
026200010305     C                   ENDSR
026300990920
026400000801     C*----------------------------------------------------*
026500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600000801     C*----------------------------------------------------*
026700010201     C     INZVAR        BEGSR
026800000801     C*
026900010201     C                   Z-ADD     *zeros        Num5_0
027000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027100020725     C                   MOVEL     '0'           FlgCAS            1
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700020904     C     DEFCAM        BEGSR
027800080923     C*
027900090310     C                   SETOFF                                       50
028000000801     C*
028100021113     C                   CLEAR                   FIVAB000
028200021113     C                   CLEAR                   FIVAT000
028300070730     C* Imposto i valori di default...
028400091223     C                   EVAL      VABCCM = 0500833
028500091223     C                   EVAL      VATCCM = 0500833
028600091223     C                   EVAL      VABLNP = 050
028700091223     C                   EVAL      VATLNP = 050
028800070928     C                   EVAL      VABCTR = 000
028900070730     C                   EVAL      VABCBO = '1'
029000090310     C                   EVAL      VABCTM = '7Q'
029100070222     C* ... e poi verifico se sono stati passati come parametri
029200070222     C                   IF        vlrppt > *blanks
029300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029500070222     C                   EXSR      CHKNUM
029600070222     C                   IF        PiInt=*on
029700070222     C                   Z-ADD     PiVal         VABCCM
029800070222     C                   Z-ADD     PiVal         VATCCM
029900070222     C                   ENDIF
030000070222     C                   ENDIF
030100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030300070222     C                   EXSR      CHKNUM
030400070222     C                   IF        PiInt=*on
030500070222     C                   Z-ADD     PiVal         VABLNP
030600070222     C                   Z-ADD     PiVal         VATLNP
030700070222     C                   ENDIF
030800070222     C                   ENDIF
030900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031100070222     C                   EXSR      CHKNUM
031200070222     C                   IF        PiInt=*on
031300070222     C                   Z-ADD     PiVal         VABCTR
031400070222     C                   ENDIF
031500070928     C                   ENDIF
031600071009     C                   MOVEL     *blanks       wDISK             1
031700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031800071009     C                   IF        wDISK <> *blanks
031900070928     C                   SETON                                        50
032000070222     C                   ENDIF
032100070222     C                   ENDIF
032200071009     C*
032300071009     C   50              EVAL      VABCTM = '7Q'
032400000801     C*
032500000801     C                   ENDSR
032600091223     C*----------------------------------------------------*
032700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
032800091223     C*----------------------------------------------------*
032900091223     C     REPNSP        BEGSR
033000091223     C*
033100091223     C                   EXSR      INZVAR
033200091223     C                   EXSR      DEFCAM
033300091223     C*
033400091223     C* NSP => Stacco un numeratore da AZNUM
033500091223     C                   clear                   TRUL33DS
033600091223     C                   eval      I33OPE = *zeros
033700091223     C                   eval      I33CNU = 302
033800091223     C                   eval      I33NUM = 1
033900091223     C                   movel     TRUL33DS      KPJBU
034000091223     C                   call      'TRUL33R'
034100091223     C                   parm                    KPJBA
034200091223     C                   movel     KPJBU         TRUL33DS
034300091223     C                   if        O33ERR = *zeros
034400091223     C                   z-add     O33NRF        VABNSP
034500091223     C                   z-add     O33NRF        VATNSP
034600091223     C                   else
034700091223     C                   SETON                                        31
034800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
034900091223     C                             + ' ' + 'VABNSP VATNSP'
035000091223     C                   endif
035100091223     C*
035200091223     C                   ENDSR
035300000801     C*----------------------------------------------------*
035400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035500000801     C*----------------------------------------------------*
035600010201     C     IMPVAB        BEGSR
035700070530     C*
035800070530     C                   SETOFF                                       3132
035900070928     C*
036000070928     C                   MOVE(P)   vlrpoi        VABFGS
036100070928     C                   MOVE(P)   vlrpoi        VATFGS
036200070928     C*
036300070928     C                   MOVEL     datcor        VABAAS
036400070928     C                   MOVEL     datcor        VATAAS
036500070928     C                   MOVE      datcor        VABMGS
036600090218     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
036700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036900070928     C* ==
037000090218     C                   EVAL      VABRD2=%trim(%subst(vindta:52:5))
037100070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
037200070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
037300070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
037400090218     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
037500070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
037600080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
037700121219     C***                EVAL      VABNOT=%trim(%subst(vindta:197:35))
037800130103     C                   EVAL      VABNOT=%trim(%subst(vindta:197+35:80-35))
037900130103     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
038000080923     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
038100080226     C                   EVAL      VABCBO='1'
038200080226     C                   ENDIF
038300080923     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
038400080226     C                   EVAL      VABCBO='2'
038500080226     C                   ENDIF
038600080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
038700080226     C                   EVAL      VABTSP='E'
038800080226     C                   ENDIF
038900080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
039000080226     C                   EVAL      VABFFD='S'
039100080226     C                   ENDIF
039200081113     C                   IF        %trim(%subst(vindta:16:1)) = '2'
039300080328     C                   EVAL      VABTC1='A'
039400080226     C                   ENDIF
039500071003     C* CAD
039600070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
039700070928     C                   EXSR      CHKNUM
039800070928     C                   IF        PiInt=*on
039900070928     C                   Z-ADD     PiVal         Num5_0
040000070928     C                   MOVEL     Num5_0        VABCAD
040100070928     C                   ELSE
040200070928     C                   SETON                                        32
040300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400070928     C                             + ' ' + 'VABCAD'
040500070928     C                   ENDIF
040600080923     C* RMN
040700091223     C                   EVAL      PiStr=%trim(%subst(vindta:197:70))
040800080923     C                   EXSR      CHKNUM
040900080923     C                   IF        PiInt=*on
041000080923     C                   Z-ADD     PiVal         VABRMN
041100080923     C                   ELSE
041200080923     C                   SETON                                        32
041300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041400080923     C                             + ' ' + 'VABRMN'
041500080923     C                   ENDIF
041600091223     C* NCL
041700070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
041800070928     C                   EXSR      CHKNUM
041900070928     C                   IF        PiInt=*on
042000091223     C                   ADD       PiVal         VABNCL
042100070928     C                   ELSE
042200070928     C                   SETON                                        32
042300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400070928     C                             + ' ' + 'VABNCL'
042500070928     C                   ENDIF
042600091223     C* PKB
042700070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
042800070928     C                   EXSR      CHKNUM
042900070928     C                   IF        PiNum=*on
043000091223     C                   ADD       PiVal         VABPKB
043100070928     C                   ELSE
043200070928     C                   SETON                                        32
043300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400070928     C                             + ' ' + 'VABPKB'
043500070928     C                   ENDIF
043600091223     C* VLB
043700121107     C***                IF        %trim(%subst(vindta:308:7))<>*blanks
043800121107     C***                          AND
043900121107     C***                          %trim(%subst(vindta:308:7))<>*zeros
044000121107     C***                EVAL      PiStr=%trim(%subst(vindta:308:7))
044100121107     C***                EXSR      CHKNUM
044200121107     C***                IF        PiNum=*on
044300121107     C***                ADD       PiVal         VABVLB
044400121107     C***                ELSE
044500121107     C***                SETON                                        32
044600121107     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
044700121107     C***                          + ' ' + 'VABVLB'
044800121107     C***                ENDIF
044900121107     C***                ENDIF
045000080923     C* CAS
045100091223     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
045200080923     C                             AND
045300080923     C                             %trim(%subst(vindta:280:12))<>*zeros
045400080923     C                             AND
045500080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
045600080923     C                             AND
045700080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
045800080923     C                             AND
045900080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
046000070928     C                   MOVEL     '1'           FlgCAS
046100090223     C                   EVAL      VABTIC = 'BM'
046200070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
046300070928     C                   EXSR      CHKNUM
046400070928     C                   IF        PiNum=*on
046500070928     C                   Z-ADD     PiVal         VABCAS
046600070928     C                   ELSE
046700070928     C                   SETON                                        32
046800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900070928     C                             + ' ' + 'VABCAS'
047000070928     C                   ENDIF
047100070928     C                   ENDIF
047200071003     C* IAS
047300080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
047400080923     C                             AND
047500080923     C                             %trim(%subst(vindta:295:12))<>*zeros
047600080923     C                             AND
047700080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
047800080923     C                             AND
047900080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
048000080923     C                             AND
048100080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
048200080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
048300080923     C                   EXSR      CHKNUM
048400080923     C                   IF        PiNum=*on
048500080923     C                   Z-ADD     PiVal         VABIAS
048600080923     C                   ELSE
048700080923     C                   SETON                                        32
048800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900080923     C                             + ' ' + 'VABIAS'
049000080923     C                   ENDIF
049100071003     C                   ENDIF
049200070730     C*
049300070730     C* Considerazioni finali su CBO/CAS
049400070730     C                   IF        FlgCAS = '1'
049500070730     C                   IF        VABCBO = '1'
049600070730     C                   EVAL      VABCBO = '4'
049700070730     C                   ENDIF
049800070730     C                   IF        VABCBO = '2'
049900070730     C                   EVAL      VABCBO = '6'
050000070730     C                   ENDIF
050100070730     C                   ENDIF
050200020305     C*
050300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050400011113     C                   EXSR      CHKIMPDIV
050500010202     C*
050600000801     C* Ebbene...
050700000801     C                   ADD       1             §CTRMO
050800070530     C                   IF        *in31 <> *zeros OR
050900070530     C                             *in32 <> *zeros
051000000801     C                   ADD       1             §CTRNO
051100020725     C                   EVAL      x_vinflg = '2'
051200000801     C                   ELSE
051300010201     C                   ADD       1             §CTROKVB
051400000801     C                   ENDIF
051500000801     C*
051600000801     C                   ENDSR
051700010201     C*----------------------------------------------------*
051800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
051900010201     C*----------------------------------------------------*
052000071003     C     WRIVAT_B      BEGSR
052100010201     C*
052200021113     C* Valorizzo l buffer di scrittura del FIVAT
052300070928     C* - TIPO RECORD "B"
052400070928     C                   if        %subst(vindta:92:15) <> *blanks
052500070928     C                   eval      VATTRC = 'B'
052600070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
052700021113     C                   write     FIVAT000
052800020725     C                   endif
052900010201     C*
053000010201     C                   ENDSR
053100071003     C*----------------------------------------------------*
053200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
053300071003     C*----------------------------------------------------*
053400071003     C     WRIVAT_E      BEGSR
053500071003     C*
053600071003     C* Valorizzo l buffer di scrittura del FIVAT
053700071003     C* - TIPO RECORD "E"
053800071003     C                   if        %subst(vindta:331:26) <> *blanks
053900071003     C                   eval      VATTRC = 'E'
054000071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
054100071003     C                   write     FIVAT000
054200071003     C                   endif
054300071003     C*
054400071003     C                   ENDSR
054500020904
054600020904
054700010202     C*----------------------------------------------------*
054800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054900010202     C*----------------------------------------------------*
055000020305     C     PREVAT        BEGSR
055100010202     C*
055200021113     C* Compongo il nome del membro da dare al FIVATWWR
055300010202     C                   eval      parmbr = vlrhdl
055400010202     C                   movel     'M'           parmbr
055500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
055600010202     C                   eval      paropz = '1'
055700010202     C* Effettuo la chiamata al CLLE preposto
055800050128     C                   call(e)   'TITVVTC'
055900010202     C                   parm                    parccm
056000010202     C                   parm                    parmbr
056100010202     C                   parm                    paropz
056200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056300010202     C                   if        %error
056400010202     C                   movel     '1'           chkcall
056500010202     C                   else
056600010202     C                   movel     '0'           chkcall
056700010202     C                   endif
056800010202     C*
056900010202     C                   ENDSR
057000000801     C*----------------------------------------------------*
057100000801     C*  CONTROLLO NUMERICITA' CAMPI
057200000801     C*----------------------------------------------------*
057300000801     C     CHKNUM        BEGSR
057400081113     C*
057500081113     C                   IF        PiDecChr = *blanks
057600091223     C                   EVAL      PiDecChr = ','
057700081113     C                   ENDIF
057800091223     C*
057900081113     C                   callp(e)  UBISNUM_Check(PiStr
058000081113     C                                          :PiDecChr
058100081113     C                                          :PiVal
058200081113     C                                          :PiNum
058300081113     C                                          :PiInt)
058400081113     C*
058500000801     C                   IF        %error
058600000801     C                   EVAL      PiInt=*off
058700000801     C                   ENDIF
058800000801     C*
058900000801     C                   ENDSR
059000000801     C***
059100000801
059200011113
059300011113     C*----------------------------------------------------*
059400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059500011113     C*----------------------------------------------------*
059600011113     C     CHKIMPDIV     BEGSR
059700011113     C*
059800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059900011113     C                   Z-ADD     *zeros        wrkDec            9 9
060000011113     C*
060100011113     C* Come prima cosa effettuo considerazioni sulla divisa
060200011113     C                   IF        vabIAS > *zeros
060300011113     C                   IF        vabVAS <> 'EUR'
060400011113     C                   EVAL      vabVAS =  'ITL'
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C                   IF        vabCAS > *zeros
060900011113     C                   IF        vabVCA <> 'EUR'
061000011113     C                   EVAL      vabVCA =  'ITL'
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C                   IF        vabVMD > *zeros
061500020305     C                   IF        vabVAD <> 'EUR'
061600011113     C                   EVAL      vabVAD =  'ITL'
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062100011113     C                   Z-ADD     vabIAS        wrkDec
062200011113     C                   IF        wrkDec > *zeros
062300011113     C                   IF        vabVAS = 'ITL'
062400011113     C                   EVAL      vabIAS = *zeros
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062900011113     C                   Z-ADD     vabCAS        wrkDec
063000011113     C                   IF        wrkDec > *zeros
063100011113     C                   IF        vabVCA = 'ITL'
063200011113     C                   EVAL      vabCAS = *zeros
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063700011113     C                   Z-ADD     vabVMD        wrkDec
063800011113     C                   IF        wrkDec > *zeros
063900011113     C                   IF        vabVAD = 'ITL'
064000011113     C                   EVAL      vabVMD = *zeros
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C                   ENDSR
064500011113     C***
064600011113
064700011113
064800000801
064900000801
065000990920      /TITLE Invio dei dati al punto operativo.
065100010202     C     invio         BEGSR
065200990920     C*
065300021113     C* 1° invio FIVAT
065400010201     C                   reset                   dscmz
065500010201     C                   move      vlrpoi        cmzdst
065600021113     C                   eval      cmzfld = 'FIVATWWR'
065700010201     C                   eval      cmzmbd = vlrhdl
065800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065900021009     C***                if        prmfir = *blanks
066000021113     C                   eval      cmzfla = 'FIVAT00F'
066100021113     C                   eval      cmzmba = 'FIVAT00F'
066200021009     C***                else
066300021009     C***                eval      cmzfla = prmfir
066400021009     C***                eval      cmzmba = prmfir
066500021009     C***                endif
066600010201     C                   eval      cmznrr = *zeros
066700020305     C                   move      §ctrokvt      cmznrr
066800021018     C                   eval      cmzlba = vlrfl1
066900010201     C                   call(e)   'TIS711C'
067000010201     C                   parm                    dscmz
067100010201     C                   parm      *blanks       esito
067200010205     C                   if        %error
067300010205     C                             or cmzerr = '1'
067400010205     C                             or esito  = '1'
067500010205     C                   eval      wrkesito = '3'
067600010205     C                   else
067700010201     C*
067800021113     C* 2° invio FIVAB
067900010201     C                   reset                   dscmz
068000010201     C                   move      vlrpoi        cmzdst
068100010201     C                   eval      cmzfld = vlrfou
068200010201     C                   eval      cmzmbd = vlrhdl
068300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068400021009     C***                if        prmfir = *blanks
068500021113     C                   eval      cmzfla = 'FIVAB00F'
068600021113     C                   eval      cmzmba = 'FIVAB00F'
068700021009     C***                else
068800021009     C***                eval      cmzfla = prmfir
068900021009     C***                eval      cmzmba = prmfir
069000021009     C***                endif
069100010201     C                   eval      cmznrr = *zeros
069200010201     C                   move      §ctrokvb      cmznrr
069300021018     C                   eval      cmzlba = vlrfl1
069400010201     C                   call(e)   'TIS711C'
069500010201     C                   parm                    dscmz
069600010201     C                   parm      *blanks       esito
069700010201     C                   if        %error
069800010201     C                             or cmzerr = '1'
069900010201     C                             or esito  = '1'
070000010201     C                   eval      wrkesito = '3'
070100010201     C                   endif
070200010205     C                   endif
070300990920     C*
070400000613     C                   ENDSR
070500000613     C***
070600070411
070700070411     C     *pssr         BEGSR
070800070411     C*
070900070411     C                   if        %open(tivin00r)
071000070411     C                   close     tivin00r
071100070411     C                   endif
071200070411     C                   if        %open(fivabwwr)
071300070411     C                   close     fivabwwr
071400070411     C                   endif
071500070411     C                   if        %open(fivatwwr)
071600070411     C                   close     fivatwwr
071700070411     C                   endif
071800070411     C*
071900070411     C* Effettuo la chiamata al CLLE preposto
072000070411     C                   call(e)   'TITVVTC'
072100070411     C                   parm                    parccm
072200070411     C                   parm                    parmbr
072300070411     C                   parm      '2'           paropz
072400070411     C*
072500070411     C                   eval      wrkesito = '2'
072600070411     C*
072700070411     C                   seton                                        LR
072800070411     C*
072900070411     C                   ENDSR     '*CANCL'
073000070411     C***
073100070411
073200990910
073300000613     C     *inzsr        BEGSR
073400990910     C*
073500990910     C     *entry        plist
073600990920     C                   parm                    tivlrds
073700990921     C                   parm      wrkesito      esito
073800000724     C                   parm                    prmlit
073900000710     C                   parm                    prmfir
074000000613     C*
074100000830     C* CALCOLA LA DATA CORRENTE
074200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074300091223     C                   eval      datcor = %dec(%date() : *ISO)
074400000830     C*
074500000613     C                   ENDSR
074600000613     C***
