000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130116     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002100090203     D depspe          s             10    INZ(*blanks)
002200090203     D curspe          s             10    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800070530     D Num5_0          s              5  0
002900000830
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500101007     D  VINDTA                     2048
003600101007     D  VINFLG                        1
003700101007     D  VINSPE                       10
003800101007     D  VINRRN                        8  0
003900020725     D*
004000080923     D*------------------
004100080923     D* DS REPERIMENTO NUMERATORE
004200080923     D*------------------
004300080923     D trul33ds      e ds                  inz
004400080923     D*------------------
004500080923     D* DS ARCHITETTURA
004600080923     D*------------------
004700080923     D kpjba         e ds                  inz
004800080923     D*------------------
004900081113
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
011900100324     C+ vindta, vinflg, substr(vindta, 331, 10) as sped, rrn(tivin00r)
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
014600090203     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
014700020305     C                   MOVEL(p)  PiStr         curspe
014800010305     C*
014900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015000080923     C                   if        *in50 = *off
015100071003     C                   exsr      impvab
015200071003     C                   exsr      wrivab
015300071003     C                   exsr      wrivat_b                                     => carico VAT
015400071003     C                   else
015500080923     C*
015600071009     C                   if        wDISK = 'D'
015700071009     C                   exsr      impvab
015800071009     C                   exsr      wrivab
015900071009     C                   exsr      wrivat_b                                     => carico VAT
016000071009     C                   exsr      wrivat_e                                     => carico VAT
016100071009     C                   else
016200071009     C*
016300010305     C                   if        depspe = *blanks                             => 1° giro
016400010305     C                   eval      depspe = curspe                              => memorizz. spediz
016500080117     C                   clear                   tivinds
016600020305     C                   exsr      impvab
016700071003     C                   exsr      wrivat_b                                     => carico VAT
016800071003     C   50              exsr      wrivat_e                                     => carico VAT
016900010305     C                   else
017000020725     C                   if        depspe <> curspe                             => rottura di spediz
017100010305     C                   eval      depspe = curspe                              => memorizz. spediz
017200070928     C                   exsr      wrivab
017300080117     C                   clear                   tivinds
017400020305     C                   exsr      impvab
017500071003     C                   exsr      wrivat_b                                     => carico VAT
017600071003     C   50              exsr      wrivat_e                                     => carico VAT
017700020305     C                   else                                                   => x stessa spediz
017800090204     C***                exsr      impvab
017900071003     C                   exsr      wrivat_b                                     => carico VAT
018000071003     C   50              exsr      wrivat_e                                     => carico VAT
018100010305     C                   endif
018200010305     C                   endif
018300010305     C                   endif
018400071003     C                   endif
018500071009     C                   endif
018600000905     C*
018700000905     C                   else
018800020725     C                   eval      x_vinflg = '1'
018900000905     C                   endif
019000000905     C*
019100020725     C     VINRRN        chain     tivin000
019200020725     C                   if        %found(tivin00r)
019300020725     C                   eval      y_vinflg = x_vinflg
019400020725     C                   eval      y_vinmsg = x_vinmsg
019500020725     C                   update    tivin000
019600020725     C                   endif
019700020725     C*
019800020725     C/EXEC SQL
019900070530     C+ Fetch C1 into :INPUT_DS
020000020725     C/END-EXEC
020100020725     C*
020200020725     C                   enddo
020300020725     C*
020400020725     C/EXEC SQL
020500020725     C+ close C1
020600020725     C/END-EXEC
020700000905     C*
020800020305     C* Scarico i VAB rimasti "in sospeso"
020900071009     C                   if        wDISK = 'C'
021000071009     C                   exsr      wrivab
021100071009     C                   endif
021200010202     C*
021300010202     C                   endif
021400990910
021500990910     C* Se non ci sono record con errori ...
021600000710     C                   if        §ctrno = 0
021700990910     C* ... restituisco esito OK.
021800990921     C                   eval      wrkesito = '0'
021900990910     C                   else
022000010201     C                   if        §ctrokvb > 0
022100990921     C                   eval      wrkesito = '1'
022200000710     C                   else
022300000710     C                   eval      wrkesito = '2'
022400990910     C                   endif
022500000710     C                   endif
022600990910     C*
022700990914     C                   if        %open(tivin00r)
022800990908     C                   close     tivin00r
022900990914     C                   endif
023000021113     C                   if        %open(fivabwwr)
023100021113     C                   close     fivabwwr
023200990914     C                   endif
023300021113     C                   if        %open(fivatwwr)
023400021113     C                   close     fivatwwr
023500010201     C                   endif
023600990910     C*
023700010201     C                   if        §ctrokvb > 0
023800000724     C                             and vlrpoi <> *zeros
023900010202     C                   exsr      invio
024000990920     C                   endif
024100990920     C*
024200910830     C                   ENDSR
024300000613     C***
024400010305
024500010305     C*----------------------------------------------------*
024600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024700010305     C*----------------------------------------------------*
024800020305     C     WRIVAB        BEGSR
024900070730     C*
025000071003     C* Considerazioni finali
025100071003     C                   if        VABRMA = *blanks
025200071003     C                   movel(P)  VABRMN        VABRMA
025300071003     C                   endif
025400071003     C*
025500021113     C                   write     fivab000                                     => scarico il VAB
025600010305     C*
025700010305     C                   ENDSR
025800990920
025900000801     C*----------------------------------------------------*
026000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026100000801     C*----------------------------------------------------*
026200010201     C     INZVAR        BEGSR
026300000801     C*
026400010201     C                   Z-ADD     *zeros        Num5_0
026500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026600020725     C                   MOVEL     '0'           FlgCAS            1
026700000801     C*
026800000801     C                   ENDSR
026900000801     C*----------------------------------------------------*
027000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027100000801     C*----------------------------------------------------*
027200020904     C     DEFCAM        BEGSR
027300080923     C*
027400080923     C                   SETOFF                                       5051
027500000801     C*
027600021113     C                   CLEAR                   FIVAB000
027700021113     C                   CLEAR                   FIVAT000
027800070730     C* Imposto i valori di default...
027900101103     C                   EVAL      VABCCM = 1120372
028000101103     C                   EVAL      VATCCM = 1120372
028100101103     C                   EVAL      VABLNP = 112
028200101103     C                   EVAL      VATLNP = 112
028300101007     C                   EVAL      VABCTR = 000
028400070730     C                   EVAL      VABCBO = '1'
028500070222     C* ... e poi verifico se sono stati passati come parametri
028600070222     C                   IF        vlrppt > *blanks
028700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
028800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028900070222     C                   EXSR      CHKNUM
029000070222     C                   IF        PiInt=*on
029100070222     C                   Z-ADD     PiVal         VABCCM
029200070222     C                   Z-ADD     PiVal         VATCCM
029300070222     C                   ENDIF
029400070222     C                   ENDIF
029500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
029600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029700070222     C                   EXSR      CHKNUM
029800070222     C                   IF        PiInt=*on
029900070222     C                   Z-ADD     PiVal         VABLNP
030000070222     C                   Z-ADD     PiVal         VATLNP
030100070222     C                   ENDIF
030200070222     C                   ENDIF
030300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030500070222     C                   EXSR      CHKNUM
030600070222     C                   IF        PiInt=*on
030700070222     C                   Z-ADD     PiVal         VABCTR
030800070222     C                   ENDIF
030900070928     C                   ENDIF
031000071009     C                   MOVEL     *blanks       wDISK             1
031100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031200071009     C                   IF        wDISK <> *blanks
031300070928     C                   SETON                                        50
031400070222     C                   ENDIF
031500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
031600080923     C                   SETON                                        51
031700080923     C                   ENDIF
031800070222     C                   ENDIF
031900071009     C*
032000071009     C   50              EVAL      VABCTM = '7Q'
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032500000801     C*----------------------------------------------------*
032600010201     C     IMPVAB        BEGSR
032700070530     C*
032800070530     C                   SETOFF                                       3132
032900070928     C*
033000070928     C                   EXSR      INZVAR
033100070928     C                   EXSR      DEFCAM
033200070928     C*
033300070928     C                   MOVE(P)   vlrpoi        VABFGS
033400070928     C                   MOVE(P)   vlrpoi        VATFGS
033500070928     C*
033600070928     C                   MOVEL     datcor        VABAAS
033700070928     C                   MOVEL     datcor        VATAAS
033800070928     C                   MOVE      datcor        VABMGS
033900070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034200070928     C* ==
034300070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
034400070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034500070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
034600101007     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
034700101007     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
034800070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
034900080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
035000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
035300100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
035400100324     C***                EVAL      VABCBO='1'
035500100324     C***                ENDIF
035600100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
035700100324     C***                EVAL      VABCBO='2'
035800100324     C***                ENDIF
035900100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
036000100324     C***                EVAL      VABTSP='E'
036100100324     C***                ENDIF
036200100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
036300100324     C***                EVAL      VABFFD='S'
036400100324     C***                ENDIF
036500100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
036600100324     C***                EVAL      VABTC1='A'
036700100324     C***                ENDIF
036800071003     C* CAD
036900070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
037000070928     C                   EXSR      CHKNUM
037100070928     C                   IF        PiInt=*on
037200070928     C                   Z-ADD     PiVal         Num5_0
037300070928     C                   MOVEL     Num5_0        VABCAD
037400070928     C                   ELSE
037500070928     C                   SETON                                        32
037600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037700070928     C                             + ' ' + 'VABCAD'
037800070928     C                   ENDIF
037900080923     C* RMN
038000110223     C                   EVAL      PiStr=%trim(%subst(vindta:190:6))
038100110223     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
038200101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
038300080923     C                   EXSR      CHKNUM
038400080923     C                   IF        PiInt=*on
038500080923     C                   Z-ADD     PiVal         VABRMN
038600080923     C                   ELSE
038700080923     C                   SETON                                        32
038800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038900080923     C                             + ' ' + 'VABRMN'
039000080923     C                   ENDIF
039100080923     C* NSP
039200080923     C                   IF        *in51 = *on
039300080923     C* NSP => Stacco un numeratore da AZNUM
039400080923     C                   clear                   TRUL33DS
039500080923     C                   eval      I33OPE = *zeros
039600080923     C                   eval      I33CNU = 302
039700080923     C                   eval      I33NUM = 1
039800080923     C                   movel     TRUL33DS      KPJBU
039900080923     C                   call      'TRUL33R'
040000080923     C                   parm                    KPJBA
040100080923     C                   movel     KPJBU         TRUL33DS
040200080923     C                   if        O33ERR = *zeros
040300080923     C                   z-add     O33NRF        VABNSP
040400080923     C                   z-add     O33NRF        VATNSP
040500080923     C                   else
040600080923     C                   SETON                                        31
040700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800080923     C                             + ' ' + 'VABNSP VATNSP'
040900080923     C                   endif
041000080923     C                   ELSE
041100110223     C                   EVAL      PiStr=%trim(%subst(vindta:190:6))
041200110223     C***                EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
041300080923     C                   EXSR      CHKNUM
041400080923     C                   IF        PiInt=*on
041500080923     C                   Z-ADD     PiVal         VABNSP
041600080923     C                   Z-ADD     PiVal         VATNSP
041700080923     C                   ELSE
041800080923     C                   SETON                                        31
041900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000080923     C                             + ' ' + 'VABNSP VATNSP'
042100080923     C                   ENDIF
042200080923     C                   ENDIF
042300071003     C* NCL
042400070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
042500070928     C                   EXSR      CHKNUM
042600070928     C                   IF        PiInt=*on
042700070928     C                   Z-ADD     PiVal         VABNCL
042800070928     C                   ELSE
042900070928     C                   SETON                                        32
043000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100070928     C                             + ' ' + 'VABNCL'
043200070928     C                   ENDIF
043300161130     C* NOT_E
043400161130     C* Forzatura particolare concordata anche se è una zozzeria
043500161130     C                   IF        VABNCL = 1
043600161130     C  N50              EXSR      WRIVAT_E
043700161130     C                   ENDIF
043800071003     C* PKB
043900070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044000070928     C                   EXSR      CHKNUM
044100070928     C                   IF        PiNum=*on
044200070928     C                   Z-ADD     PiVal         VABPKB
044300070928     C                   ELSE
044400070928     C                   SETON                                        32
044500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600070928     C                             + ' ' + 'VABPKB'
044700070928     C                   ENDIF
044800080923     C* CAS
044900080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
045000080923     C                             AND
045100080923     C                             %trim(%subst(vindta:280:12))<>*zeros
045200080923     C                             AND
045300080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
045400080923     C                             AND
045500080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
045600080923     C                             AND
045700080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
045800070928     C                   MOVEL     '1'           FlgCAS
045900070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
046000070928     C                   EXSR      CHKNUM
046100070928     C                   IF        PiNum=*on
046200070928     C                   Z-ADD     PiVal         VABCAS
046300070928     C                   ELSE
046400070928     C                   SETON                                        32
046500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046600070928     C                             + ' ' + 'VABCAS'
046700070928     C                   ENDIF
046800070928     C                   ENDIF
046900071003     C* IAS
047000080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
047100080923     C                             AND
047200080923     C                             %trim(%subst(vindta:295:12))<>*zeros
047300080923     C                             AND
047400080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
047500080923     C                             AND
047600080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
047700080923     C                             AND
047800080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
047900080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
048000080923     C                   EXSR      CHKNUM
048100080923     C                   IF        PiNum=*on
048200080923     C                   Z-ADD     PiVal         VABIAS
048300080923     C                   ELSE
048400080923     C                   SETON                                        32
048500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048600080923     C                             + ' ' + 'VABIAS'
048700080923     C                   ENDIF
048800071003     C                   ENDIF
048900070730     C*
049000070730     C* Considerazioni finali su CBO/CAS
049100070730     C                   IF        FlgCAS = '1'
049200070730     C                   IF        VABCBO = '1'
049300070730     C                   EVAL      VABCBO = '4'
049400070730     C                   ENDIF
049500070730     C                   IF        VABCBO = '2'
049600070730     C                   EVAL      VABCBO = '6'
049700070730     C                   ENDIF
049800140221     C* se VABCAS > 0 importi il VABTIC dalla posizione 380 per 2 bytes
049900140221     C                   EVAL      VABTIC=%trim(%subst(vindta:380:2))
050000070730     C                   ENDIF
050100020305     C*
050200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050300011113     C                   EXSR      CHKIMPDIV
050400010202     C*
050500000801     C* Ebbene...
050600000801     C                   ADD       1             §CTRMO
050700070530     C                   IF        *in31 <> *zeros OR
050800070530     C                             *in32 <> *zeros
050900000801     C                   ADD       1             §CTRNO
051000020725     C                   EVAL      x_vinflg = '2'
051100000801     C                   ELSE
051200010201     C                   ADD       1             §CTROKVB
051300000801     C                   ENDIF
051400000801     C*
051500000801     C                   ENDSR
051600010201     C*----------------------------------------------------*
051700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
051800010201     C*----------------------------------------------------*
051900071003     C     WRIVAT_B      BEGSR
052000090204     C*
052100090204     C                   eval      vatAAS = vabAAS
052200090204     C                   eval      vatLNP = vabLNP
052300090204     C                   eval      vatNRS = vabNRS
052400090204     C                   eval      vatNSP = vabNSP
052500010201     C*
052600101007     C* Valorizzo il buffer di scrittura del FIVAT
052700070928     C* - TIPO RECORD "B"
052800070928     C                   if        %subst(vindta:92:15) <> *blanks
052900070928     C                   eval      VATTRC = 'B'
053000070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
053100021113     C                   write     FIVAT000
053200020725     C                   endif
053300010201     C*
053400010201     C                   ENDSR
053500071003     C*----------------------------------------------------*
053600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
053700071003     C*----------------------------------------------------*
053800071003     C     WRIVAT_E      BEGSR
053900090204     C*
054000090204     C                   eval      vatAAS = vabAAS
054100090204     C                   eval      vatLNP = vabLNP
054200090204     C                   eval      vatNRS = vabNRS
054300090204     C                   eval      vatNSP = vabNSP
054400071003     C*
054500101007     C* Valorizzo il buffer di scrittura del FIVAT
054600071003     C* - TIPO RECORD "E"
054700071003     C                   if        %subst(vindta:331:26) <> *blanks
054800071003     C                   eval      VATTRC = 'E'
054900071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
055000071003     C                   write     FIVAT000
055100071003     C                   endif
055200071003     C*
055300071003     C                   ENDSR
055400020904
055500020904
055600010202     C*----------------------------------------------------*
055700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055800010202     C*----------------------------------------------------*
055900020305     C     PREVAT        BEGSR
056000010202     C*
056100021113     C* Compongo il nome del membro da dare al FIVATWWR
056200010202     C                   eval      parmbr = vlrhdl
056300010202     C                   movel     'M'           parmbr
056400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
056500010202     C                   eval      paropz = '1'
056600010202     C* Effettuo la chiamata al CLLE preposto
056700050128     C                   call(e)   'TITVVTC'
056800010202     C                   parm                    parccm
056900010202     C                   parm                    parmbr
057000010202     C                   parm                    paropz
057100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057200010202     C                   if        %error
057300010202     C                   movel     '1'           chkcall
057400010202     C                   else
057500010202     C                   movel     '0'           chkcall
057600010202     C                   endif
057700010202     C*
057800010202     C                   ENDSR
057900000801     C*----------------------------------------------------*
058000000801     C*  CONTROLLO NUMERICITA' CAMPI
058100000801     C*----------------------------------------------------*
058200000801     C     CHKNUM        BEGSR
058300081113     C*
058400081113     C                   IF        PiDecChr = *blanks
058500110223     C                   EVAL      PiDecChr = ','
058600081113     C                   ENDIF
058700081113     C*
058800081113     C                   callp(e)  UBISNUM_Check(PiStr
058900081113     C                                          :PiDecChr
059000081113     C                                          :PiVal
059100081113     C                                          :PiNum
059200081113     C                                          :PiInt)
059300081113     C*
059400000801     C                   IF        %error
059500000801     C                   EVAL      PiInt=*off
059600000801     C                   ENDIF
059700000801     C*
059800000801     C                   ENDSR
059900000801     C***
060000000801
060100011113
060200011113     C*----------------------------------------------------*
060300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060400011113     C*----------------------------------------------------*
060500011113     C     CHKIMPDIV     BEGSR
060600011113     C*
060700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060800011113     C                   Z-ADD     *zeros        wrkDec            9 9
060900011113     C*
061000011113     C* Come prima cosa effettuo considerazioni sulla divisa
061100011113     C                   IF        vabIAS > *zeros
061200011113     C                   IF        vabVAS <> 'EUR'
061300011113     C                   EVAL      vabVAS =  'ITL'
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C                   IF        vabCAS > *zeros
061800011113     C                   IF        vabVCA <> 'EUR'
061900011113     C                   EVAL      vabVCA =  'ITL'
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   IF        vabVMD > *zeros
062400020305     C                   IF        vabVAD <> 'EUR'
062500011113     C                   EVAL      vabVAD =  'ITL'
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063000011113     C                   Z-ADD     vabIAS        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVAS = 'ITL'
063300011113     C                   EVAL      vabIAS = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063800011113     C                   Z-ADD     vabCAS        wrkDec
063900011113     C                   IF        wrkDec > *zeros
064000011113     C                   IF        vabVCA = 'ITL'
064100011113     C                   EVAL      vabCAS = *zeros
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064600011113     C                   Z-ADD     vabVMD        wrkDec
064700011113     C                   IF        wrkDec > *zeros
064800011113     C                   IF        vabVAD = 'ITL'
064900011113     C                   EVAL      vabVMD = *zeros
065000011113     C                   ENDIF
065100011113     C                   ENDIF
065200011113     C*
065300011113     C                   ENDSR
065400011113     C***
065500011113
065600011113
065700000801
065800000801
065900990920      /TITLE Invio dei dati al punto operativo.
066000010202     C     invio         BEGSR
066100990920     C*
066200021113     C* 1° invio FIVAT
066300010201     C                   reset                   dscmz
066400010201     C                   move      vlrpoi        cmzdst
066500021113     C                   eval      cmzfld = 'FIVATWWR'
066600010201     C                   eval      cmzmbd = vlrhdl
066700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066800021009     C***                if        prmfir = *blanks
066900021113     C                   eval      cmzfla = 'FIVAT00F'
067000021113     C                   eval      cmzmba = 'FIVAT00F'
067100021009     C***                else
067200021009     C***                eval      cmzfla = prmfir
067300021009     C***                eval      cmzmba = prmfir
067400021009     C***                endif
067500010201     C                   eval      cmznrr = *zeros
067600020305     C                   move      §ctrokvt      cmznrr
067700021018     C                   eval      cmzlba = vlrfl1
067800010201     C                   call(e)   'TIS711C'
067900010201     C                   parm                    dscmz
068000010201     C                   parm      *blanks       esito
068100010205     C                   if        %error
068200010205     C                             or cmzerr = '1'
068300010205     C                             or esito  = '1'
068400010205     C                   eval      wrkesito = '3'
068500010205     C                   else
068600010201     C*
068700021113     C* 2° invio FIVAB
068800010201     C                   reset                   dscmz
068900010201     C                   move      vlrpoi        cmzdst
069000010201     C                   eval      cmzfld = vlrfou
069100010201     C                   eval      cmzmbd = vlrhdl
069200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069300021009     C***                if        prmfir = *blanks
069400021113     C                   eval      cmzfla = 'FIVAB00F'
069500021113     C                   eval      cmzmba = 'FIVAB00F'
069600021009     C***                else
069700021009     C***                eval      cmzfla = prmfir
069800021009     C***                eval      cmzmba = prmfir
069900021009     C***                endif
070000010201     C                   eval      cmznrr = *zeros
070100010201     C                   move      §ctrokvb      cmznrr
070200021018     C                   eval      cmzlba = vlrfl1
070300010201     C                   call(e)   'TIS711C'
070400010201     C                   parm                    dscmz
070500010201     C                   parm      *blanks       esito
070600010201     C                   if        %error
070700010201     C                             or cmzerr = '1'
070800010201     C                             or esito  = '1'
070900010201     C                   eval      wrkesito = '3'
071000010201     C                   endif
071100010205     C                   endif
071200990920     C*
071300000613     C                   ENDSR
071400000613     C***
071500070411
071600070411     C     *pssr         BEGSR
071700070411     C*
071800070411     C                   if        %open(tivin00r)
071900070411     C                   close     tivin00r
072000070411     C                   endif
072100070411     C                   if        %open(fivabwwr)
072200070411     C                   close     fivabwwr
072300070411     C                   endif
072400070411     C                   if        %open(fivatwwr)
072500070411     C                   close     fivatwwr
072600070411     C                   endif
072700070411     C*
072800070411     C* Effettuo la chiamata al CLLE preposto
072900070411     C                   call(e)   'TITVVTC'
073000070411     C                   parm                    parccm
073100070411     C                   parm                    parmbr
073200070411     C                   parm      '2'           paropz
073300070411     C*
073400070411     C                   eval      wrkesito = '2'
073500070411     C*
073600070411     C                   seton                                        LR
073700070411     C*
073800070411     C                   ENDSR     '*CANCL'
073900070411     C***
074000070411
074100990910
074200000613     C     *inzsr        BEGSR
074300990910     C*
074400990910     C     *entry        plist
074500990920     C                   parm                    tivlrds
074600990921     C                   parm      wrkesito      esito
074700000724     C                   parm                    prmlit
074800000710     C                   parm                    prmfir
074900000613     C*
075000000830     C* CALCOLA LA DATA CORRENTE
075100100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075200100324     C                   eval      datcor = %dec(%date() : *ISO)
075300000830     C*
075400000613     C                   ENDSR
075500000613     C***
