000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120705     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002100110622     D depspe          s             10    INZ(*blanks)
002200110622     D curspe          s             10    INZ(*blanks)
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
003700110524     D  VINSPE                        8
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
008600000801
008700910830     C*--------------------------------------------------------
008800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008900910830     C*--------------------------------------------------------
009000070530     C     RWFILE        BEGSR
009100990910     C*
009200990914     C                   if        not %open(tivin00r)
009300990908     C                   open      tivin00r
009400990914     C                   endif
009500021113     C                   if        not %open(fivabwwr)
009600021113     C                   open      fivabwwr
009700990914     C                   endif
009800070530     C*
009900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010000020305     C                   exsr      prevat
010100010201     C*
010200010202     C                   if        chkcall = '0'
010300010202     C*
010400021113     C                   if        not %open(fivatwwr)
010500021113     C                   open      fivatwwr
010600010201     C                   endif
010700080117     C*
010800080117     C                   EXSR      INZVAR
010900080117     C                   EXSR      DEFCAM
011000990910     C*
011100010201     C                   clear                   §CTROKVB          5 0
011200020305     C                   clear                   §CTROKVT          5 0
011300000801     C                   clear                   §CTRMO            5 0
011400000801     C                   clear                   §CTRNO            5 0
011500990910     C*
011600020725     C/EXEC SQL
011700020725     C+ declare C1 cursor for select
011800110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
011900060223     C+ from tivin00r
012000060223     C+ order by sped
012100020725     C+ for read only
012200020725     C/END-EXEC
012300020725     C
012400020725     C/EXEC SQL
012500020725     C+ open C1
012600020725     C/END-EXEC
012700020725     C
012800020725     C/EXEC SQL
012900070530     C+ Fetch C1 into :INPUT_DS
013000020725     C/END-EXEC
013100020725     C*
013200020725     C                   dow       sqlcod = *zeros
013300990913     C*
013400020725     C                   if        vindta > *blanks
013500000613     C                   add       1             rrnum
013600000801     C*
013700020725     C                   if        vinflg = *blanks
013800020725     C                             or vinflg = '0'
013900020725     C                             or vinflg = '2'
014000000801     C*
014100020725     C                   clear                   x_vinmsg
014200020725     C                   eval      x_vinflg = '1'
014300010305     C*
014400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014500110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
014600020305     C                   MOVEL(p)  PiStr         curspe
014700010305     C*
014800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014900080923     C                   if        *in50 = *off
015000071003     C                   exsr      impvab
015100071003     C                   exsr      wrivab
015200071003     C                   exsr      wrivat_b                                     => carico VAT
015300071003     C                   else
015400080923     C*
015500071009     C                   if        wDISK = 'D'
015600071009     C                   exsr      impvab
015700071009     C                   exsr      wrivab
015800071009     C                   exsr      wrivat_b                                     => carico VAT
015900071009     C                   exsr      wrivat_e                                     => carico VAT
016000071009     C                   else
016100071009     C*
016200010305     C                   if        depspe = *blanks                             => 1° giro
016300010305     C                   eval      depspe = curspe                              => memorizz. spediz
016400080117     C                   clear                   tivinds
016500020305     C                   exsr      impvab
016600071003     C                   exsr      wrivat_b                                     => carico VAT
016700071003     C   50              exsr      wrivat_e                                     => carico VAT
016800010305     C                   else
016900020725     C                   if        depspe <> curspe                             => rottura di spediz
017000010305     C                   eval      depspe = curspe                              => memorizz. spediz
017100070928     C                   exsr      wrivab
017200080117     C                   clear                   tivinds
017300020305     C                   exsr      impvab
017400071003     C                   exsr      wrivat_b                                     => carico VAT
017500071003     C   50              exsr      wrivat_e                                     => carico VAT
017600020305     C                   else                                                   => x stessa spediz
017700090204     C***                exsr      impvab
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   endif
018100010305     C                   endif
018200010305     C                   endif
018300071003     C                   endif
018400071009     C                   endif
018500000905     C*
018600000905     C                   else
018700020725     C                   eval      x_vinflg = '1'
018800000905     C                   endif
018900000905     C*
019000020725     C     VINRRN        chain     tivin000
019100020725     C                   if        %found(tivin00r)
019200020725     C                   eval      y_vinflg = x_vinflg
019300020725     C                   eval      y_vinmsg = x_vinmsg
019400020725     C                   update    tivin000
019500020725     C                   endif
019600020725     C*
019700020725     C/EXEC SQL
019800070530     C+ Fetch C1 into :INPUT_DS
019900020725     C/END-EXEC
020000020725     C*
020100020725     C                   enddo
020200020725     C*
020300020725     C/EXEC SQL
020400020725     C+ close C1
020500020725     C/END-EXEC
020600000905     C*
020700020305     C* Scarico i VAB rimasti "in sospeso"
020800071009     C                   if        wDISK = 'C'
020900071009     C                   exsr      wrivab
021000071009     C                   endif
021100010202     C*
021200010202     C                   endif
021300990910
021400990910     C* Se non ci sono record con errori ...
021500000710     C                   if        §ctrno = 0
021600990910     C* ... restituisco esito OK.
021700990921     C                   eval      wrkesito = '0'
021800990910     C                   else
021900010201     C                   if        §ctrokvb > 0
022000990921     C                   eval      wrkesito = '1'
022100000710     C                   else
022200000710     C                   eval      wrkesito = '2'
022300990910     C                   endif
022400000710     C                   endif
022500990910     C*
022600990914     C                   if        %open(tivin00r)
022700990908     C                   close     tivin00r
022800990914     C                   endif
022900021113     C                   if        %open(fivabwwr)
023000021113     C                   close     fivabwwr
023100990914     C                   endif
023200021113     C                   if        %open(fivatwwr)
023300021113     C                   close     fivatwwr
023400010201     C                   endif
023500990910     C*
023600010201     C                   if        §ctrokvb > 0
023700000724     C                             and vlrpoi <> *zeros
023800010202     C                   exsr      invio
023900990920     C                   endif
024000990920     C*
024100910830     C                   ENDSR
024200000613     C***
024300010305
024400010305     C*----------------------------------------------------*
024500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024600010305     C*----------------------------------------------------*
024700020305     C     WRIVAB        BEGSR
024800070730     C*
024900071003     C* Considerazioni finali
025000071003     C                   if        VABRMA = *blanks
025100071003     C                   movel(P)  VABRMN        VABRMA
025200071003     C                   endif
025300071003     C*
025400021113     C                   write     fivab000                                     => scarico il VAB
025500010305     C*
025600010305     C                   ENDSR
025700990920
025800000801     C*----------------------------------------------------*
025900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026000000801     C*----------------------------------------------------*
026100010201     C     INZVAR        BEGSR
026200000801     C*
026300010201     C                   Z-ADD     *zeros        Num5_0
026400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026500020725     C                   MOVEL     '0'           FlgCAS            1
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027000000801     C*----------------------------------------------------*
027100020904     C     DEFCAM        BEGSR
027200080923     C*
027300080923     C                   SETOFF                                       5051
027400000801     C*
027500021113     C                   CLEAR                   FIVAB000
027600021113     C                   CLEAR                   FIVAT000
027700070730     C* Imposto i valori di default...
027800110622     C                   EVAL      VABCCM = 0182711
027900110622     C                   EVAL      VATCCM = 0182711
028000110613     C                   EVAL      VABLNP = 018
028100110613     C                   EVAL      VATLNP = 018
028200110622     C                   EVAL      VABCTR = 100
028300070730     C                   EVAL      VABCBO = '1'
028400110520     C                   EVAL      VABTSP = 'C'
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
034600110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
034700110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
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
038000110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
038100110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
038200110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
038300101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
038400080923     C                   EXSR      CHKNUM
038500080923     C                   IF        PiInt=*on
038600080923     C                   Z-ADD     PiVal         VABRMN
038700080923     C                   ELSE
038800080923     C                   SETON                                        32
038900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039000080923     C                             + ' ' + 'VABRMN'
039100080923     C                   ENDIF
039200080923     C* NSP
039300080923     C                   IF        *in51 = *on
039400080923     C* NSP => Stacco un numeratore da AZNUM
039500080923     C                   clear                   TRUL33DS
039600080923     C                   eval      I33OPE = *zeros
039700080923     C                   eval      I33CNU = 302
039800080923     C                   eval      I33NUM = 1
039900080923     C                   movel     TRUL33DS      KPJBU
040000080923     C                   call      'TRUL33R'
040100080923     C                   parm                    KPJBA
040200080923     C                   movel     KPJBU         TRUL33DS
040300080923     C                   if        O33ERR = *zeros
040400080923     C                   z-add     O33NRF        VABNSP
040500080923     C                   z-add     O33NRF        VATNSP
040600080923     C                   else
040700080923     C                   SETON                                        31
040800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040900080923     C                             + ' ' + 'VABNSP VATNSP'
041000080923     C                   endif
041100080923     C                   ELSE
041200110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041300110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
041400110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
041500080923     C                   EXSR      CHKNUM
041600080923     C                   IF        PiInt=*on
041700080923     C                   Z-ADD     PiVal         VABNSP
041800080923     C                   Z-ADD     PiVal         VATNSP
041900080923     C                   ELSE
042000080923     C                   SETON                                        31
042100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200080923     C                             + ' ' + 'VABNSP VATNSP'
042300080923     C                   ENDIF
042400080923     C                   ENDIF
042500071003     C* NCL
042600070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
042700070928     C                   EXSR      CHKNUM
042800070928     C                   IF        PiInt=*on
042900070928     C                   Z-ADD     PiVal         VABNCL
043000070928     C                   ELSE
043100070928     C                   SETON                                        32
043200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043300070928     C                             + ' ' + 'VABNCL'
043400070928     C                   ENDIF
043500071003     C* PKB
043600110520     C                   IF        %subst(vindta:170:8) <> *blanks
043700070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
043800070928     C                   EXSR      CHKNUM
043900070928     C                   IF        PiNum=*on
044000070928     C                   Z-ADD     PiVal         VABPKB
044100070928     C                   ELSE
044200070928     C                   SETON                                        32
044300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400070928     C                             + ' ' + 'VABPKB'
044500070928     C                   ENDIF
044600110520     C                   ELSE
044700110520     C                   IF        VABCTR = 100
044800110520     C                   EVAL      VABPKB = 18 * VABNCL
044900110520     C                   ELSE
045000110520     C                   EVAL      VABPKB = 10 * VABNCL
045100110520     C                   ENDIF
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
046400070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
046500070928     C                   EXSR      CHKNUM
046600070928     C                   IF        PiNum=*on
046700070928     C                   Z-ADD     PiVal         VABCAS
046800070928     C                   ELSE
046900070928     C                   SETON                                        32
047000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100070928     C                             + ' ' + 'VABCAS'
047200070928     C                   ENDIF
047300070928     C                   ENDIF
047400071003     C* IAS
047500080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
047600080923     C                             AND
047700080923     C                             %trim(%subst(vindta:295:12))<>*zeros
047800080923     C                             AND
047900080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
048000080923     C                             AND
048100080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
048200080923     C                             AND
048300080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
048400080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
048500080923     C                   EXSR      CHKNUM
048600080923     C                   IF        PiNum=*on
048700080923     C                   Z-ADD     PiVal         VABIAS
048800080923     C                   ELSE
048900080923     C                   SETON                                        32
049000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100080923     C                             + ' ' + 'VABIAS'
049200080923     C                   ENDIF
049300071003     C                   ENDIF
049400070730     C*
049500070730     C* Considerazioni finali su CBO/CAS
049600070730     C                   IF        FlgCAS = '1'
049700070730     C                   IF        VABCBO = '1'
049800070730     C                   EVAL      VABCBO = '4'
049900070730     C                   ENDIF
050000070730     C                   IF        VABCBO = '2'
050100070730     C                   EVAL      VABCBO = '6'
050200070730     C                   ENDIF
050300070730     C                   ENDIF
050400020305     C*
050500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050600011113     C                   EXSR      CHKIMPDIV
050700010202     C*
050800000801     C* Ebbene...
050900000801     C                   ADD       1             §CTRMO
051000070530     C                   IF        *in31 <> *zeros OR
051100070530     C                             *in32 <> *zeros
051200000801     C                   ADD       1             §CTRNO
051300020725     C                   EVAL      x_vinflg = '2'
051400000801     C                   ELSE
051500010201     C                   ADD       1             §CTROKVB
051600000801     C                   ENDIF
051700000801     C*
051800000801     C                   ENDSR
051900010201     C*----------------------------------------------------*
052000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052100010201     C*----------------------------------------------------*
052200071003     C     WRIVAT_B      BEGSR
052300090204     C*
052400090204     C                   eval      vatAAS = vabAAS
052500090204     C                   eval      vatLNP = vabLNP
052600090204     C                   eval      vatNRS = vabNRS
052700090204     C                   eval      vatNSP = vabNSP
052800010201     C*
052900101007     C* Valorizzo il buffer di scrittura del FIVAT
053000070928     C* - TIPO RECORD "B"
053100070928     C                   if        %subst(vindta:92:15) <> *blanks
053200070928     C                   eval      VATTRC = 'B'
053300070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
053400021113     C                   write     FIVAT000
053500020725     C                   endif
053600010201     C*
053700010201     C                   ENDSR
053800071003     C*----------------------------------------------------*
053900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054000071003     C*----------------------------------------------------*
054100071003     C     WRIVAT_E      BEGSR
054200090204     C*
054300090204     C                   eval      vatAAS = vabAAS
054400090204     C                   eval      vatLNP = vabLNP
054500090204     C                   eval      vatNRS = vabNRS
054600090204     C                   eval      vatNSP = vabNSP
054700071003     C*
054800101007     C* Valorizzo il buffer di scrittura del FIVAT
054900071003     C* - TIPO RECORD "E"
055000071003     C                   if        %subst(vindta:331:26) <> *blanks
055100071003     C                   eval      VATTRC = 'E'
055200130212     C* il cliente ci passa il barcode numerico con gli 0 a sx ma non li vuole
055300130212     C                   eval      VATNOT =
055400130212     C                              %trim(%char(%int(%subst(vindta:331:26))))
055500071003     C                   write     FIVAT000
055600071003     C                   endif
055700071003     C*
055800071003     C                   ENDSR
055900020904
056000020904
056100010202     C*----------------------------------------------------*
056200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056300010202     C*----------------------------------------------------*
056400020305     C     PREVAT        BEGSR
056500010202     C*
056600021113     C* Compongo il nome del membro da dare al FIVATWWR
056700010202     C                   eval      parmbr = vlrhdl
056800010202     C                   movel     'M'           parmbr
056900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057000010202     C                   eval      paropz = '1'
057100010202     C* Effettuo la chiamata al CLLE preposto
057200050128     C                   call(e)   'TITVVTC'
057300010202     C                   parm                    parccm
057400010202     C                   parm                    parmbr
057500010202     C                   parm                    paropz
057600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057700010202     C                   if        %error
057800010202     C                   movel     '1'           chkcall
057900010202     C                   else
058000010202     C                   movel     '0'           chkcall
058100010202     C                   endif
058200010202     C*
058300010202     C                   ENDSR
058400000801     C*----------------------------------------------------*
058500000801     C*  CONTROLLO NUMERICITA' CAMPI
058600000801     C*----------------------------------------------------*
058700000801     C     CHKNUM        BEGSR
058800081113     C*
058900081113     C                   IF        PiDecChr = *blanks
059000110524     C                   EVAL      PiDecChr = '.'
059100081113     C                   ENDIF
059200081113     C*
059300081113     C                   callp(e)  UBISNUM_Check(PiStr
059400081113     C                                          :PiDecChr
059500081113     C                                          :PiVal
059600081113     C                                          :PiNum
059700081113     C                                          :PiInt)
059800081113     C*
059900000801     C                   IF        %error
060000000801     C                   EVAL      PiInt=*off
060100000801     C                   ENDIF
060200000801     C*
060300000801     C                   ENDSR
060400000801     C***
060500000801
060600011113
060700011113     C*----------------------------------------------------*
060800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060900011113     C*----------------------------------------------------*
061000011113     C     CHKIMPDIV     BEGSR
061100011113     C*
061200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061300011113     C                   Z-ADD     *zeros        wrkDec            9 9
061400011113     C*
061500011113     C* Come prima cosa effettuo considerazioni sulla divisa
061600011113     C                   IF        vabIAS > *zeros
061700011113     C                   IF        vabVAS <> 'EUR'
061800011113     C                   EVAL      vabVAS =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C                   IF        vabCAS > *zeros
062300011113     C                   IF        vabVCA <> 'EUR'
062400011113     C                   EVAL      vabVCA =  'ITL'
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   IF        vabVMD > *zeros
062900020305     C                   IF        vabVAD <> 'EUR'
063000011113     C                   EVAL      vabVAD =  'ITL'
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063500011113     C                   Z-ADD     vabIAS        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVAS = 'ITL'
063800011113     C                   EVAL      vabIAS = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064300011113     C                   Z-ADD     vabCAS        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVCA = 'ITL'
064600011113     C                   EVAL      vabCAS = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065100011113     C                   Z-ADD     vabVMD        wrkDec
065200011113     C                   IF        wrkDec > *zeros
065300011113     C                   IF        vabVAD = 'ITL'
065400011113     C                   EVAL      vabVMD = *zeros
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C                   ENDSR
065900011113     C***
066000011113
066100011113
066200000801
066300000801
066400990920      /TITLE Invio dei dati al punto operativo.
066500010202     C     invio         BEGSR
066600990920     C*
066700021113     C* 1° invio FIVAT
066800010201     C                   reset                   dscmz
066900010201     C                   move      vlrpoi        cmzdst
067000021113     C                   eval      cmzfld = 'FIVATWWR'
067100010201     C                   eval      cmzmbd = vlrhdl
067200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067300021009     C***                if        prmfir = *blanks
067400021113     C                   eval      cmzfla = 'FIVAT00F'
067500021113     C                   eval      cmzmba = 'FIVAT00F'
067600021009     C***                else
067700021009     C***                eval      cmzfla = prmfir
067800021009     C***                eval      cmzmba = prmfir
067900021009     C***                endif
068000010201     C                   eval      cmznrr = *zeros
068100020305     C                   move      §ctrokvt      cmznrr
068200021018     C                   eval      cmzlba = vlrfl1
068300010201     C                   call(e)   'TIS711C'
068400010201     C                   parm                    dscmz
068500010201     C                   parm      *blanks       esito
068600010205     C                   if        %error
068700010205     C                             or cmzerr = '1'
068800010205     C                             or esito  = '1'
068900010205     C                   eval      wrkesito = '3'
069000010205     C                   else
069100010201     C*
069200021113     C* 2° invio FIVAB
069300010201     C                   reset                   dscmz
069400010201     C                   move      vlrpoi        cmzdst
069500010201     C                   eval      cmzfld = vlrfou
069600010201     C                   eval      cmzmbd = vlrhdl
069700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069800021009     C***                if        prmfir = *blanks
069900021113     C                   eval      cmzfla = 'FIVAB00F'
070000021113     C                   eval      cmzmba = 'FIVAB00F'
070100021009     C***                else
070200021009     C***                eval      cmzfla = prmfir
070300021009     C***                eval      cmzmba = prmfir
070400021009     C***                endif
070500010201     C                   eval      cmznrr = *zeros
070600010201     C                   move      §ctrokvb      cmznrr
070700021018     C                   eval      cmzlba = vlrfl1
070800010201     C                   call(e)   'TIS711C'
070900010201     C                   parm                    dscmz
071000010201     C                   parm      *blanks       esito
071100010201     C                   if        %error
071200010201     C                             or cmzerr = '1'
071300010201     C                             or esito  = '1'
071400010201     C                   eval      wrkesito = '3'
071500010201     C                   endif
071600010205     C                   endif
071700990920     C*
071800000613     C                   ENDSR
071900000613     C***
072000070411
072100070411     C     *pssr         BEGSR
072200070411     C*
072300070411     C                   if        %open(tivin00r)
072400070411     C                   close     tivin00r
072500070411     C                   endif
072600070411     C                   if        %open(fivabwwr)
072700070411     C                   close     fivabwwr
072800070411     C                   endif
072900070411     C                   if        %open(fivatwwr)
073000070411     C                   close     fivatwwr
073100070411     C                   endif
073200070411     C*
073300070411     C* Effettuo la chiamata al CLLE preposto
073400070411     C                   call(e)   'TITVVTC'
073500070411     C                   parm                    parccm
073600070411     C                   parm                    parmbr
073700070411     C                   parm      '2'           paropz
073800070411     C*
073900070411     C                   eval      wrkesito = '2'
074000070411     C*
074100070411     C                   seton                                        LR
074200070411     C*
074300070411     C                   ENDSR     '*CANCL'
074400070411     C***
074500070411
074600990910
074700000613     C     *inzsr        BEGSR
074800990910     C*
074900990910     C     *entry        plist
075000990920     C                   parm                    tivlrds
075100990921     C                   parm      wrkesito      esito
075200000724     C                   parm                    prmlit
075300000710     C                   parm                    prmfir
075400000613     C*
075500000830     C* CALCOLA LA DATA CORRENTE
075600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075700100324     C                   eval      datcor = %dec(%date() : *ISO)
075800000830     C*
075900000613     C                   ENDSR
076000000613     C***
