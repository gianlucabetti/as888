000100120619      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500120619     FEDIVABwr  O    E             DISK    usropn
000600120619     FEDIVATwr  O    E             DISK    usropn
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
002100120619     D depspe          s             10    INZ(*blanks)
002200120619     D curspe          s             10    INZ(*blanks)
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
005600120619     D*------------------
005700120619     D* DS CALCOLO CHECK-DIGITS
005800120619     D*------------------
005900120619     D TRUL28DSX     e ds                  inz
006000990908
006100010201
006200010201
006300080117     C*
006400080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006500080117     C
006600080117     C/EXEC SQL
006700080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006800080117     C/END-EXEC
006900080117     C
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300000613     C*
007400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007500000613     C*
007600010202     C* Effettuo la chiamata al CLLE preposto
007700120619     C                   call(e)   'TITVEVTC'
007800010202     C                   parm                    parccm
007900010202     C                   parm                    parmbr
008000010202     C                   parm      '2'           paropz
008100070730     C*
008200070730     C* Effettuo lancio TISI95 solo x chiusura
008300070730     C                   CLEAR                   TISI95DS
008400070730     C                   EVAL      I95TLA = 'C'
008500070730     C                   CALL      'TISI95R'
008600070730     C                   PARM                    TISI95DS
008700000616     C*
008800000801     C
008900010201     C                   seton                                        LR
009000120619
009100910830     C*--------------------------------------------------------
009200120619     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009300910830     C*--------------------------------------------------------
009400070530     C     RWFILE        BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900120619     C                   if        not %open(edivabwr)
010000120619     C                   open      edivabwr
010100990914     C                   endif
010200070530     C*
010300120619     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010400020305     C                   exsr      prevat
010500010201     C*
010600010202     C                   if        chkcall = '0'
010700010202     C*
010800120619     C                   if        not %open(edivatwr)
010900120619     C                   open      edivatwr
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
012200120619     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
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
015500071003     C                   exsr      wrivab
015600071003     C                   exsr      wrivat_b                                     => carico VAT
015700071003     C                   else
015800080923     C*
015900071009     C                   if        wDISK = 'D'
016000071009     C                   exsr      impvab
016100071009     C                   exsr      wrivab
016200071009     C                   exsr      wrivat_b                                     => carico VAT
016300071009     C                   exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900020305     C                   exsr      impvab
017000071003     C                   exsr      wrivat_b                                     => carico VAT
017100071003     C   50              exsr      wrivat_e                                     => carico VAT
017200010305     C                   else
017300020725     C                   if        depspe <> curspe                             => rottura di spediz
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500070928     C                   exsr      wrivab
017600080117     C                   clear                   tivinds
017700020305     C                   exsr      impvab
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000020305     C                   else                                                   => x stessa spediz
018100090204     C***                exsr      impvab
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400010305     C                   endif
018500010305     C                   endif
018600010305     C                   endif
018700071003     C                   endif
018800071009     C                   endif
018900000905     C*
019000000905     C                   else
019100020725     C                   eval      x_vinflg = '1'
019200000905     C                   endif
019300000905     C*
019400020725     C     VINRRN        chain     tivin000
019500020725     C                   if        %found(tivin00r)
019600020725     C                   eval      y_vinflg = x_vinflg
019700020725     C                   eval      y_vinmsg = x_vinmsg
019800020725     C                   update    tivin000
019900020725     C                   endif
020000020725     C*
020100020725     C/EXEC SQL
020200070530     C+ Fetch C1 into :INPUT_DS
020300020725     C/END-EXEC
020400020725     C*
020500020725     C                   enddo
020600020725     C*
020700020725     C/EXEC SQL
020800020725     C+ close C1
020900020725     C/END-EXEC
021000000905     C*
021100020305     C* Scarico i VAB rimasti "in sospeso"
021200071009     C                   if        wDISK = 'C'
021300071009     C                   exsr      wrivab
021400071009     C                   endif
021500010202     C*
021600010202     C                   endif
021700990910
021800990910     C* Se non ci sono record con errori ...
021900000710     C                   if        §ctrno = 0
022000990910     C* ... restituisco esito OK.
022100990921     C                   eval      wrkesito = '0'
022200990910     C                   else
022300010201     C                   if        §ctrokvb > 0
022400990921     C                   eval      wrkesito = '1'
022500000710     C                   else
022600000710     C                   eval      wrkesito = '2'
022700990910     C                   endif
022800000710     C                   endif
022900990910     C*
023000990914     C                   if        %open(tivin00r)
023100990908     C                   close     tivin00r
023200990914     C                   endif
023300120619     C                   if        %open(edivabwr)
023400120619     C                   close     edivabwr
023500120619     C                   endif
023600120619     C                   if        %open(edivatwr)
023700120619     C                   close     edivatwr
023800120619     C                   endif
023900990910     C*
024000010201     C                   if        §ctrokvb > 0
024100000724     C                             and vlrpoi <> *zeros
024200010202     C                   exsr      invio
024300990920     C                   endif
024400990920     C*
024500910830     C                   ENDSR
024600000613     C***
024700010305
024800010305     C*----------------------------------------------------*
024900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025000010305     C*----------------------------------------------------*
025100020305     C     WRIVAB        BEGSR
025200070730     C*
025300071003     C* Considerazioni finali
025400071003     C                   if        VABRMA = *blanks
025500071003     C                   movel(P)  VABRMN        VABRMA
025600071003     C                   endif
025700071003     C*
025800120619     C                   write     edivab00                                     => scarico il VAB
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700010201     C                   Z-ADD     *zeros        Num5_0
026800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026900020725     C                   MOVEL     '0'           FlgCAS            1
027000000801     C*
027100000801     C                   ENDSR
027200000801     C*----------------------------------------------------*
027300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027400000801     C*----------------------------------------------------*
027500020904     C     DEFCAM        BEGSR
027600080923     C*
027700080923     C                   SETOFF                                       5051
027800000801     C*
027900120619     C                   CLEAR                   EDIVAB00
028000120619     C                   CLEAR                   EDIVAT00
028100070730     C* Imposto i valori di default...
028200120619     C                   EVAL      VABCCM = 0661788
028300120619     C                   EVAL      VATCCM = 0661788
028400120619     C                   EVAL      VABLNP = 066
028500120619     C                   EVAL      VATLNP = 066
028600120619     C                   EVAL      VABCTR = 300
028700070730     C                   EVAL      VABCBO = '1'
028800110520     C                   EVAL      VABTSP = 'C'
028900070222     C* ... e poi verifico se sono stati passati come parametri
029000070222     C                   IF        vlrppt > *blanks
029100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029300070222     C                   EXSR      CHKNUM
029400070222     C                   IF        PiInt=*on
029500070222     C                   Z-ADD     PiVal         VABCCM
029600070222     C                   Z-ADD     PiVal         VATCCM
029700070222     C                   ENDIF
029800070222     C                   ENDIF
029900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030100070222     C                   EXSR      CHKNUM
030200070222     C                   IF        PiInt=*on
030300070222     C                   Z-ADD     PiVal         VABLNP
030400070222     C                   Z-ADD     PiVal         VATLNP
030500070222     C                   ENDIF
030600070222     C                   ENDIF
030700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030900070222     C                   EXSR      CHKNUM
031000070222     C                   IF        PiInt=*on
031100070222     C                   Z-ADD     PiVal         VABCTR
031200070222     C                   ENDIF
031300070928     C                   ENDIF
031400071009     C                   MOVEL     *blanks       wDISK             1
031500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031600071009     C                   IF        wDISK <> *blanks
031700070928     C                   SETON                                        50
031800070222     C                   ENDIF
031900080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032000080923     C                   SETON                                        51
032100080923     C                   ENDIF
032200070222     C                   ENDIF
032300071009     C*
032400071009     C   50              EVAL      VABCTM = '7Q'
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800120619     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
032900000801     C*----------------------------------------------------*
033000010201     C     IMPVAB        BEGSR
033100070530     C*
033200070530     C                   SETOFF                                       3132
033300070928     C*
033400070928     C                   EXSR      INZVAR
033500070928     C                   EXSR      DEFCAM
033600070928     C*
033700070928     C                   MOVE(P)   vlrpoi        VABFGS
033800070928     C                   MOVE(P)   vlrpoi        VATFGS
033900070928     C*
034000070928     C                   MOVEL     datcor        VABAAS
034100070928     C                   MOVEL     datcor        VATAAS
034200070928     C                   MOVE      datcor        VABMGS
034300070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034400070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034500070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034600070928     C* ==
034700070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
034800070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034900070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035000110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035100110524     C**+                EVAL      VABRMA=%trim(%subst(vindta:331:10))
035200070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035300080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
035400070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035500070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035600070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
035700100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
035800100324     C***                EVAL      VABCBO='1'
035900100324     C***                ENDIF
036000100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
036100100324     C***                EVAL      VABCBO='2'
036200100324     C***                ENDIF
036300100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
036400100324     C***                EVAL      VABTSP='E'
036500100324     C***                ENDIF
036600100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
036700100324     C***                EVAL      VABFFD='S'
036800100324     C***                ENDIF
036900100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
037000100324     C***                EVAL      VABTC1='A'
037100100324     C***                ENDIF
037200071003     C* CAD
037300070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
037400070928     C                   EXSR      CHKNUM
037500070928     C                   IF        PiInt=*on
037600070928     C                   Z-ADD     PiVal         Num5_0
037700070928     C                   MOVEL     Num5_0        VABCAD
037800070928     C                   ELSE
037900070928     C                   SETON                                        32
038000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038100070928     C                             + ' ' + 'VABCAD'
038200070928     C                   ENDIF
038300080923     C* RMN
038400110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
038500120619     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
038600120619     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
038700101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
038800080923     C                   EXSR      CHKNUM
038900080923     C                   IF        PiInt=*on
039000080923     C                   Z-ADD     PiVal         VABRMN
039100080923     C                   ELSE
039200080923     C                   SETON                                        32
039300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039400080923     C                             + ' ' + 'VABRMN'
039500080923     C                   ENDIF
039600080923     C* NSP
039700080923     C                   IF        *in51 = *on
039800080923     C* NSP => Stacco un numeratore da AZNUM
039900080923     C                   clear                   TRUL33DS
040000080923     C                   eval      I33OPE = *zeros
040100080923     C                   eval      I33CNU = 302
040200080923     C                   eval      I33NUM = 1
040300080923     C                   movel     TRUL33DS      KPJBU
040400080923     C                   call      'TRUL33R'
040500080923     C                   parm                    KPJBA
040600080923     C                   movel     KPJBU         TRUL33DS
040700080923     C                   if        O33ERR = *zeros
040800080923     C                   z-add     O33NRF        VABNSP
040900080923     C                   z-add     O33NRF        VATNSP
041000080923     C                   else
041100080923     C                   SETON                                        31
041200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300080923     C                             + ' ' + 'VABNSP VATNSP'
041400080923     C                   endif
041500080923     C                   ELSE
041600110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041700120619     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
041800120619     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
041900110223     C***                EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
042000080923     C                   EXSR      CHKNUM
042100080923     C                   IF        PiInt=*on
042200080923     C                   Z-ADD     PiVal         VABNSP
042300080923     C                   Z-ADD     PiVal         VATNSP
042400080923     C                   ELSE
042500080923     C                   SETON                                        31
042600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700080923     C                             + ' ' + 'VABNSP VATNSP'
042800080923     C                   ENDIF
042900080923     C                   ENDIF
043000071003     C* NCL
043100070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
043200070928     C                   EXSR      CHKNUM
043300070928     C                   IF        PiInt=*on
043400070928     C                   Z-ADD     PiVal         VABNCL
043500070928     C                   ELSE
043600070928     C                   SETON                                        32
043700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043800070928     C                             + ' ' + 'VABNCL'
043900070928     C                   ENDIF
044000071003     C* PKB
044100110520     C                   IF        %subst(vindta:170:8) <> *blanks
044200070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044300070928     C                   EXSR      CHKNUM
044400070928     C                   IF        PiNum=*on
044500070928     C                   Z-ADD     PiVal         VABPKB
044600070928     C                   ELSE
044700070928     C                   SETON                                        32
044800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044900070928     C                             + ' ' + 'VABPKB'
045000070928     C                   ENDIF
045100110520     C                   ELSE
045200110520     C                   IF        VABCTR = 100
045300110520     C                   EVAL      VABPKB = 18 * VABNCL
045400110520     C                   ELSE
045500110520     C                   EVAL      VABPKB = 10 * VABNCL
045600110520     C                   ENDIF
045700110520     C                   ENDIF
045800080923     C* CAS
045900080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
046000080923     C                             AND
046100080923     C                             %trim(%subst(vindta:280:12))<>*zeros
046200080923     C                             AND
046300080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
046400080923     C                             AND
046500080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
046600080923     C                             AND
046700080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
046800070928     C                   MOVEL     '1'           FlgCAS
046900070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047000070928     C                   EXSR      CHKNUM
047100070928     C                   IF        PiNum=*on
047200070928     C                   Z-ADD     PiVal         VABCAS
047300070928     C                   ELSE
047400070928     C                   SETON                                        32
047500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600070928     C                             + ' ' + 'VABCAS'
047700070928     C                   ENDIF
047800070928     C                   ENDIF
047900071003     C* IAS
048000080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
048100080923     C                             AND
048200080923     C                             %trim(%subst(vindta:295:12))<>*zeros
048300080923     C                             AND
048400080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
048500080923     C                             AND
048600080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
048700080923     C                             AND
048800080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
048900080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049000080923     C                   EXSR      CHKNUM
049100080923     C                   IF        PiNum=*on
049200080923     C                   Z-ADD     PiVal         VABIAS
049300080923     C                   ELSE
049400080923     C                   SETON                                        32
049500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600080923     C                             + ' ' + 'VABIAS'
049700080923     C                   ENDIF
049800071003     C                   ENDIF
049900070730     C*
050000070730     C* Considerazioni finali su CBO/CAS
050100070730     C                   IF        FlgCAS = '1'
050200070730     C                   IF        VABCBO = '1'
050300070730     C                   EVAL      VABCBO = '4'
050400070730     C                   ENDIF
050500070730     C                   IF        VABCBO = '2'
050600070730     C                   EVAL      VABCBO = '6'
050700070730     C                   ENDIF
050800070730     C                   ENDIF
050900020305     C*
051000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051100011113     C                   EXSR      CHKIMPDIV
051200010202     C*
051300000801     C* Ebbene...
051400000801     C                   ADD       1             §CTRMO
051500070530     C                   IF        *in31 <> *zeros OR
051600070530     C                             *in32 <> *zeros
051700000801     C                   ADD       1             §CTRNO
051800020725     C                   EVAL      x_vinflg = '2'
051900000801     C                   ELSE
052000010201     C                   ADD       1             §CTROKVB
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400010201     C*----------------------------------------------------*
052500120619     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
052600010201     C*----------------------------------------------------*
052700071003     C     WRIVAT_B      BEGSR
052800090204     C*
052900090204     C                   eval      vatAAS = vabAAS
053000090204     C                   eval      vatLNP = vabLNP
053100090204     C                   eval      vatNRS = vabNRS
053200090204     C                   eval      vatNSP = vabNSP
053300010201     C*
053400120619     C* Valorizzo il buffer di scrittura del EDIVAT
053500070928     C* - TIPO RECORD "B"
053600070928     C                   if        %subst(vindta:92:15) <> *blanks
053700070928     C                   eval      VATTRC = 'B'
053800070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
053900120619     C                   write     EDIVAT00
054000020725     C                   endif
054100010201     C*
054200010201     C                   ENDSR
054300071003     C*----------------------------------------------------*
054400120619     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
054500071003     C*----------------------------------------------------*
054600071003     C     WRIVAT_E      BEGSR
054700090204     C*
054800090204     C                   eval      vatAAS = vabAAS
054900090204     C                   eval      vatLNP = vabLNP
055000090204     C                   eval      vatNRS = vabNRS
055100090204     C                   eval      vatNSP = vabNSP
055200071003     C*
055300120619     C* Valorizzo il buffer di scrittura del EDIVAT
055400071003     C* - TIPO RECORD "E"
055500071003     C                   if        %subst(vindta:331:26) <> *blanks
055600071003     C                   eval      VATTRC = 'E'
055700120619     C*
055800120619     C*
055900120619     C                   clear                   TRUL28DSX
056000120619     C                   eval      I284STD = 'E10'
056100120619     C                   eval      I284COD = %trim(%subst(vindta:331:26))
056200120619     C                   call      'TRUL28R4'
056300120619     C                   parm                    TRUL28DSX
056400120619     C* Se tutto ok scrivo il "chi sono" corrente
056500120619     C                   if        O284err = *blanks
056600120619     C                   eval      VATNOT = %trim(%subst(vindta:331:26)) +
056700120619     C                                      %trim(O284CkD)
056800120619     C                   write     EDIVAT00
056900071003     C                   endif
057000120619     C                   endif
057100071003     C*
057200071003     C                   ENDSR
057300020904
057400020904
057500010202     C*----------------------------------------------------*
057600120619     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
057700010202     C*----------------------------------------------------*
057800020305     C     PREVAT        BEGSR
057900010202     C*
058000120619     C* Compongo il nome del membro da dare al EDIVATWR
058100010202     C                   eval      parmbr = vlrhdl
058200010202     C                   movel     'M'           parmbr
058300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
058400010202     C                   eval      paropz = '1'
058500010202     C* Effettuo la chiamata al CLLE preposto
058600120619     C                   call(e)   'TITVEVTC'
058700010202     C                   parm                    parccm
058800010202     C                   parm                    parmbr
058900010202     C                   parm                    paropz
059000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059100010202     C                   if        %error
059200010202     C                   movel     '1'           chkcall
059300010202     C                   else
059400010202     C                   movel     '0'           chkcall
059500010202     C                   endif
059600010202     C*
059700010202     C                   ENDSR
059800000801     C*----------------------------------------------------*
059900000801     C*  CONTROLLO NUMERICITA' CAMPI
060000000801     C*----------------------------------------------------*
060100000801     C     CHKNUM        BEGSR
060200081113     C*
060300081113     C                   IF        PiDecChr = *blanks
060400110524     C                   EVAL      PiDecChr = '.'
060500081113     C                   ENDIF
060600081113     C*
060700081113     C                   callp(e)  UBISNUM_Check(PiStr
060800081113     C                                          :PiDecChr
060900081113     C                                          :PiVal
061000081113     C                                          :PiNum
061100081113     C                                          :PiInt)
061200081113     C*
061300000801     C                   IF        %error
061400000801     C                   EVAL      PiInt=*off
061500000801     C                   ENDIF
061600000801     C*
061700000801     C                   ENDSR
061800000801     C***
061900000801
062000011113
062100011113     C*----------------------------------------------------*
062200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062300011113     C*----------------------------------------------------*
062400011113     C     CHKIMPDIV     BEGSR
062500011113     C*
062600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062700011113     C                   Z-ADD     *zeros        wrkDec            9 9
062800011113     C*
062900011113     C* Come prima cosa effettuo considerazioni sulla divisa
063000011113     C                   IF        vabIAS > *zeros
063100011113     C                   IF        vabVAS <> 'EUR'
063200011113     C                   EVAL      vabVAS =  'ITL'
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C                   IF        vabCAS > *zeros
063700011113     C                   IF        vabVCA <> 'EUR'
063800011113     C                   EVAL      vabVCA =  'ITL'
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C                   IF        vabVMD > *zeros
064300020305     C                   IF        vabVAD <> 'EUR'
064400011113     C                   EVAL      vabVAD =  'ITL'
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064900011113     C                   Z-ADD     vabIAS        wrkDec
065000011113     C                   IF        wrkDec > *zeros
065100011113     C                   IF        vabVAS = 'ITL'
065200011113     C                   EVAL      vabIAS = *zeros
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065700011113     C                   Z-ADD     vabCAS        wrkDec
065800011113     C                   IF        wrkDec > *zeros
065900011113     C                   IF        vabVCA = 'ITL'
066000011113     C                   EVAL      vabCAS = *zeros
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066500011113     C                   Z-ADD     vabVMD        wrkDec
066600011113     C                   IF        wrkDec > *zeros
066700011113     C                   IF        vabVAD = 'ITL'
066800011113     C                   EVAL      vabVMD = *zeros
066900011113     C                   ENDIF
067000011113     C                   ENDIF
067100011113     C*
067200011113     C                   ENDSR
067300011113     C***
067400011113
067500011113
067600000801
067700000801
067800990920      /TITLE Invio dei dati al punto operativo.
067900010202     C     invio         BEGSR
068000990920     C*
068100120619     C* 1° invio EDIVAT
068200010201     C                   reset                   dscmz
068300010201     C                   move      vlrpoi        cmzdst
068400120619     C                   eval      cmzfld = 'EDIVATWR'
068500010201     C                   eval      cmzmbd = vlrhdl
068600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068700021009     C***                if        prmfir = *blanks
068800120619     C                   eval      cmzfla = 'EDIVAT0F'
068900120619     C                   eval      cmzmba = 'EDIVAT0F'
069000021009     C***                else
069100021009     C***                eval      cmzfla = prmfir
069200021009     C***                eval      cmzmba = prmfir
069300021009     C***                endif
069400010201     C                   eval      cmznrr = *zeros
069500020305     C                   move      §ctrokvt      cmznrr
069600021018     C                   eval      cmzlba = vlrfl1
069700010201     C                   call(e)   'TIS711C'
069800010201     C                   parm                    dscmz
069900010201     C                   parm      *blanks       esito
070000010205     C                   if        %error
070100010205     C                             or cmzerr = '1'
070200010205     C                             or esito  = '1'
070300010205     C                   eval      wrkesito = '3'
070400010205     C                   else
070500010201     C*
070600120619     C* 2° invio EDIVAB
070700010201     C                   reset                   dscmz
070800010201     C                   move      vlrpoi        cmzdst
070900010201     C                   eval      cmzfld = vlrfou
071000010201     C                   eval      cmzmbd = vlrhdl
071100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071200021009     C***                if        prmfir = *blanks
071300120619     C                   eval      cmzfla = 'EDIVAT0F'
071400120619     C                   eval      cmzmba = 'EDIVAT0F'
071500021009     C***                else
071600021009     C***                eval      cmzfla = prmfir
071700021009     C***                eval      cmzmba = prmfir
071800021009     C***                endif
071900010201     C                   eval      cmznrr = *zeros
072000010201     C                   move      §ctrokvb      cmznrr
072100021018     C                   eval      cmzlba = vlrfl1
072200010201     C                   call(e)   'TIS711C'
072300010201     C                   parm                    dscmz
072400010201     C                   parm      *blanks       esito
072500010201     C                   if        %error
072600010201     C                             or cmzerr = '1'
072700010201     C                             or esito  = '1'
072800010201     C                   eval      wrkesito = '3'
072900010201     C                   endif
073000010205     C                   endif
073100990920     C*
073200000613     C                   ENDSR
073300000613     C***
073400070411
073500070411     C     *pssr         BEGSR
073600070411     C*
073700070411     C                   if        %open(tivin00r)
073800070411     C                   close     tivin00r
073900070411     C                   endif
074000120619     C                   if        %open(edivabwr)
074100120619     C                   close     edivabwr
074200120619     C                   endif
074300120619     C                   if        %open(edivatwr)
074400120619     C                   close     edivatwr
074500120619     C                   endif
074600070411     C*
074700070411     C* Effettuo la chiamata al CLLE preposto
074800120619     C                   call(e)   'TITVEVTC'
074900070411     C                   parm                    parccm
075000070411     C                   parm                    parmbr
075100070411     C                   parm      '2'           paropz
075200070411     C*
075300070411     C                   eval      wrkesito = '2'
075400070411     C*
075500070411     C                   seton                                        LR
075600070411     C*
075700070411     C                   ENDSR     '*CANCL'
075800070411     C***
075900070411
076000990910
076100000613     C     *inzsr        BEGSR
076200990910     C*
076300990910     C     *entry        plist
076400990920     C                   parm                    tivlrds
076500990921     C                   parm      wrkesito      esito
076600000724     C                   parm                    prmlit
076700000710     C                   parm                    prmfir
076800000613     C*
076900000830     C* CALCOLA LA DATA CORRENTE
077000100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077100100324     C                   eval      datcor = %dec(%date() : *ISO)
077200000830     C*
077300000613     C                   ENDSR
077400000613     C***
