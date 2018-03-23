000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002900171213     D w70             s             70
003000000830
003100020725
003200020725     D*------------------
003300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003400020725     D*------------------
003500070530     D INPUT_DS        DS                  INZ
003600101007     D  VINDTA                     2048
003700101007     D  VINFLG                        1
003800110524     D  VINSPE                        8
003900101007     D  VINRRN                        8  0
004000020725     D*
004100080923     D*------------------
004200080923     D* DS REPERIMENTO NUMERATORE
004300080923     D*------------------
004400080923     D trul33ds      e ds                  inz
004500080923     D*------------------
004600080923     D* DS ARCHITETTURA
004700080923     D*------------------
004800080923     D kpjba         e ds                  inz
004900080923     D*------------------
005000081113
005100081113     D*------------------
005200081113     D* LINKING A DEFINIZIONI ESTERNE
005300081113     D*------------------
005400100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600081113
005700990908
005800010201
005900010201
006000080117     C*
006100080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006200080117     C
006300080117     C/EXEC SQL
006400080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006500080117     C/END-EXEC
006600080117     C
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000613     C*
007100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400050128     C                   call(e)   'TITVVTC'
007500010202     C                   parm                    parccm
007600010202     C                   parm                    parmbr
007700010202     C                   parm      '2'           paropz
007800070730     C*
007900070730     C* Effettuo lancio TISI95 solo x chiusura
008000070730     C                   CLEAR                   TISI95DS
008100070730     C                   EVAL      I95TLA = 'C'
008200070730     C                   CALL      'TISI95R'
008300070730     C                   PARM                    TISI95DS
008400000616     C*
008500000801     C
008600010201     C                   seton                                        LR
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
011900110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
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
014600110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
014700020305     C                   MOVEL(p)  PiStr         curspe
014800010305     C*
014900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015000080923     C                   if        *in50 = *off
015100071003     C                   exsr      impvab
015200071003     C                   exsr      wrivab
015300071003     C                   exsr      wrivat_b                                     => carico VAT
015400171213     C                   exsr      wrivat_ij                                    => carico VAT
015500171213     C                   exsr      wrivat_s                                     => carico VAT
015600071003     C                   else
015700080923     C*
015800071009     C                   if        wDISK = 'D'
015900071009     C                   exsr      impvab
016000071009     C                   exsr      wrivab
016100071009     C                   exsr      wrivat_b                                     => carico VAT
016200171213     C                   exsr      wrivat_ij                                    => carico VAT
016300171213     C                   exsr      wrivat_s                                     => carico VAT
016400071009     C                   exsr      wrivat_e                                     => carico VAT
016500071009     C                   else
016600071009     C*
016700010305     C                   if        depspe = *blanks                             => 1° giro
016800010305     C                   eval      depspe = curspe                              => memorizz. spediz
016900080117     C                   clear                   tivinds
017000020305     C                   exsr      impvab
017100071003     C                   exsr      wrivat_b                                     => carico VAT
017200171213     C                   exsr      wrivat_ij                                    => carico VAT
017300171213     C                   exsr      wrivat_s                                     => carico VAT
017400071003     C   50              exsr      wrivat_e                                     => carico VAT
017500010305     C                   else
017600020725     C                   if        depspe <> curspe                             => rottura di spediz
017700010305     C                   eval      depspe = curspe                              => memorizz. spediz
017800070928     C                   exsr      wrivab
017900080117     C                   clear                   tivinds
018000020305     C                   exsr      impvab
018100071003     C                   exsr      wrivat_b                                     => carico VAT
018200171213     C                   exsr      wrivat_ij                                    => carico VAT
018300171213     C                   exsr      wrivat_s                                     => carico VAT
018400071003     C   50              exsr      wrivat_e                                     => carico VAT
018500020305     C                   else                                                   => x stessa spediz
018600090204     C***                exsr      impvab
018700071003     C                   exsr      wrivat_b                                     => carico VAT
018800171213     C                   exsr      wrivat_ij                                    => carico VAT
018900171213     C                   exsr      wrivat_s                                     => carico VAT
019000071003     C   50              exsr      wrivat_e                                     => carico VAT
019100010305     C                   endif
019200010305     C                   endif
019300010305     C                   endif
019400071003     C                   endif
019500071009     C                   endif
019600000905     C*
019700000905     C                   else
019800020725     C                   eval      x_vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100020725     C     VINRRN        chain     tivin000
020200020725     C                   if        %found(tivin00r)
020300020725     C                   eval      y_vinflg = x_vinflg
020400020725     C                   eval      y_vinmsg = x_vinmsg
020500020725     C                   update    tivin000
020600020725     C                   endif
020700020725     C*
020800020725     C/EXEC SQL
020900070530     C+ Fetch C1 into :INPUT_DS
021000020725     C/END-EXEC
021100020725     C*
021200020725     C                   enddo
021300020725     C*
021400020725     C/EXEC SQL
021500020725     C+ close C1
021600020725     C/END-EXEC
021700000905     C*
021800020305     C* Scarico i VAB rimasti "in sospeso"
021900071009     C                   if        wDISK = 'C'
022000071009     C                   exsr      wrivab
022100071009     C                   endif
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000021113     C                   if        %open(fivabwwr)
024100021113     C                   close     fivabwwr
024200990914     C                   endif
024300021113     C                   if        %open(fivatwwr)
024400021113     C                   close     fivatwwr
024500010201     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800000724     C                             and vlrpoi <> *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400010305
025500010305     C*----------------------------------------------------*
025600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025700010305     C*----------------------------------------------------*
025800020305     C     WRIVAB        BEGSR
025900070730     C*
026000071003     C* Considerazioni finali
026100071003     C                   if        VABRMA = *blanks
026200071003     C                   movel(P)  VABRMN        VABRMA
026300071003     C                   endif
026400071003     C*
026500021113     C                   write     fivab000                                     => scarico il VAB
026600010305     C*
026700010305     C                   ENDSR
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400010201     C                   Z-ADD     *zeros        Num5_0
027500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027600020725     C                   MOVEL     '0'           FlgCAS            1
027700171213     C                   CLEAR                   w70
027800180122     C                   CLEAR                   PiDecChr
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028300000801     C*----------------------------------------------------*
028400020904     C     DEFCAM        BEGSR
028500080923     C*
028600080923     C                   SETOFF                                       5051
028700000801     C*
028800021113     C                   CLEAR                   FIVAB000
028900021113     C                   CLEAR                   FIVAT000
029000070730     C* Imposto i valori di default...
029100121214     C                   EVAL      VABCCM = 0250055
029200121214     C                   EVAL      VATCCM = 0250055
029300121214     C                   EVAL      VABLNP = 025
029400121214     C                   EVAL      VATLNP = 025
029500121214     C                   EVAL      VABCTR = 000
029600070222     C* ... e poi verifico se sono stati passati come parametri
029700070222     C                   IF        vlrppt > *blanks
029800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030000070222     C                   EXSR      CHKNUM
030100070222     C                   IF        PiInt=*on
030200070222     C                   Z-ADD     PiVal         VABCCM
030300070222     C                   Z-ADD     PiVal         VATCCM
030400070222     C                   ENDIF
030500070222     C                   ENDIF
030600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030800070222     C                   EXSR      CHKNUM
030900070222     C                   IF        PiInt=*on
031000070222     C                   Z-ADD     PiVal         VABLNP
031100070222     C                   Z-ADD     PiVal         VATLNP
031200070222     C                   ENDIF
031300070222     C                   ENDIF
031400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031600070222     C                   EXSR      CHKNUM
031700070222     C                   IF        PiInt=*on
031800070222     C                   Z-ADD     PiVal         VABCTR
031900070222     C                   ENDIF
032000070928     C                   ENDIF
032100071009     C                   MOVEL     *blanks       wDISK             1
032200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032300071009     C                   IF        wDISK <> *blanks
032400070928     C                   SETON                                        50
032500070222     C                   ENDIF
032600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032700080923     C                   SETON                                        51
032800080923     C                   ENDIF
032900070222     C                   ENDIF
033000071009     C*
033100071009     C   50              EVAL      VABCTM = '7Q'
033200000801     C*
033300000801     C                   ENDSR
033400000801     C*----------------------------------------------------*
033500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600000801     C*----------------------------------------------------*
033700010201     C     IMPVAB        BEGSR
033800070530     C*
033900070530     C                   SETOFF                                       3132
034000070928     C*
034100070928     C                   EXSR      INZVAR
034200070928     C                   EXSR      DEFCAM
034300070928     C*
034400070928     C                   MOVE(P)   vlrpoi        VABFGS
034500070928     C                   MOVE(P)   vlrpoi        VATFGS
034600070928     C*
034700070928     C                   MOVEL     datcor        VABAAS
034800070928     C                   MOVEL     datcor        VATAAS
034900070928     C                   MOVE      datcor        VABMGS
035000070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
035100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035300070928     C* ==
035400160118     C                   EVAL      VABRD2=%trim(%subst(vindta:412:35))
035500160118     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035600070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035700070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035800110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035900110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
036000080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
036100070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
036200070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
036300070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
036400121214     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
036500121214     C                   EVAL      VABCBO='1'
036600121214     C                   ENDIF
036700121214     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
036800121214     C                   EVAL      VABCBO='2'
036900121214     C                   ENDIF
037000100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
037100100324     C***                EVAL      VABTSP='E'
037200100324     C***                ENDIF
037300100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
037400100324     C***                EVAL      VABFFD='S'
037500100324     C***                ENDIF
037600100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
037700100324     C***                EVAL      VABTC1='A'
037800100324     C***                ENDIF
037900071003     C* CAD
038000070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
038100070928     C                   EXSR      CHKNUM
038200070928     C                   IF        PiInt=*on
038300070928     C                   Z-ADD     PiVal         Num5_0
038400070928     C                   MOVEL     Num5_0        VABCAD
038500070928     C                   ELSE
038600070928     C                   SETON                                        32
038700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038800070928     C                             + ' ' + 'VABCAD'
038900070928     C                   ENDIF
039000080923     C* RMN
039100110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
039200110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
039300110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
039400101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
039500080923     C                   EXSR      CHKNUM
039600080923     C                   IF        PiInt=*on
039700080923     C                   Z-ADD     PiVal         VABRMN
039800080923     C                   ELSE
039900080923     C                   SETON                                        32
040000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100080923     C                             + ' ' + 'VABRMN'
040200080923     C                   ENDIF
040300080923     C* NSP
040400080923     C                   IF        *in51 = *on
040500080923     C* NSP => Stacco un numeratore da AZNUM
040600080923     C                   clear                   TRUL33DS
040700080923     C                   eval      I33OPE = *zeros
040800080923     C                   eval      I33CNU = 302
040900080923     C                   eval      I33NUM = 1
041000080923     C                   movel     TRUL33DS      KPJBU
041100080923     C                   call      'TRUL33R'
041200080923     C                   parm                    KPJBA
041300080923     C                   movel     KPJBU         TRUL33DS
041400080923     C                   if        O33ERR = *zeros
041500080923     C                   z-add     O33NRF        VABNSP
041600080923     C                   z-add     O33NRF        VATNSP
041700080923     C                   else
041800080923     C                   SETON                                        31
041900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000080923     C                             + ' ' + 'VABNSP VATNSP'
042100080923     C                   endif
042200080923     C                   ELSE
042300110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
042400110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
042500110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
042600080923     C                   EXSR      CHKNUM
042700080923     C                   IF        PiInt=*on
042800080923     C                   Z-ADD     PiVal         VABNSP
042900080923     C                   Z-ADD     PiVal         VATNSP
043000080923     C                   ELSE
043100080923     C                   SETON                                        31
043200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043300080923     C                             + ' ' + 'VABNSP VATNSP'
043400080923     C                   ENDIF
043500080923     C                   ENDIF
043600071003     C* NCL
043700070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
043800070928     C                   EXSR      CHKNUM
043900070928     C                   IF        PiInt=*on
044000070928     C                   Z-ADD     PiVal         VABNCL
044100070928     C                   ELSE
044200070928     C                   SETON                                        32
044300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400070928     C                             + ' ' + 'VABNCL'
044500070928     C                   ENDIF
044600071003     C* PKB
044700110520     C                   IF        %subst(vindta:170:8) <> *blanks
044800070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044900070928     C                   EXSR      CHKNUM
045000070928     C                   IF        PiNum=*on
045100070928     C                   Z-ADD     PiVal         VABPKB
045200070928     C                   ELSE
045300070928     C                   SETON                                        32
045400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045500070928     C                             + ' ' + 'VABPKB'
045600070928     C                   ENDIF
045700110520     C                   ELSE
045800110520     C                   IF        VABCTR = 100
045900110520     C                   EVAL      VABPKB = 18 * VABNCL
046000110520     C                   ELSE
046100110520     C                   EVAL      VABPKB = 10 * VABNCL
046200110520     C                   ENDIF
046300110520     C                   ENDIF
046400080923     C* CAS
046500080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
046600080923     C                             AND
046700080923     C                             %trim(%subst(vindta:280:12))<>*zeros
046800080923     C                             AND
046900180122     C                             %trim(%subst(vindta:280:12))<>'00000000.000'
047000070928     C                   MOVEL     '1'           FlgCAS
047100070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047200180122     C                   EVAL      PiDecChr = '.'
047300070928     C                   EXSR      CHKNUM
047400070928     C                   IF        PiNum=*on
047500070928     C                   Z-ADD     PiVal         VABCAS
047600180122     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
047700180122     C                   EVAL      VABTIC=%trim(%subst(vindta:568:2))
047800070928     C                   ELSE
047900070928     C                   SETON                                        32
048000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100070928     C                             + ' ' + 'VABCAS'
048200070928     C                   ENDIF
048300070928     C                   ENDIF
048400071003     C* IAS
048500080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
048600080923     C                             AND
048700080923     C                             %trim(%subst(vindta:295:12))<>*zeros
048800080923     C                             AND
048900080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
049000080923     C                             AND
049100080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
049200080923     C                             AND
049300080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
049400080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049500080923     C                   EXSR      CHKNUM
049600080923     C                   IF        PiNum=*on
049700080923     C                   Z-ADD     PiVal         VABIAS
049800080923     C                   ELSE
049900080923     C                   SETON                                        32
050000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050100080923     C                             + ' ' + 'VABIAS'
050200080923     C                   ENDIF
050300071003     C                   ENDIF
050400070730     C*
050500070730     C* Considerazioni finali su CBO/CAS
050600070730     C                   IF        FlgCAS = '1'
050700070730     C                   IF        VABCBO = '1'
050800070730     C                   EVAL      VABCBO = '4'
050900070730     C                   ENDIF
051000070730     C                   IF        VABCBO = '2'
051100070730     C                   EVAL      VABCBO = '6'
051200070730     C                   ENDIF
051300070730     C                   ENDIF
051400020305     C*
051500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051600011113     C                   EXSR      CHKIMPDIV
051700010202     C*
051800000801     C* Ebbene...
051900000801     C                   ADD       1             §CTRMO
052000070530     C                   IF        *in31 <> *zeros OR
052100070530     C                             *in32 <> *zeros
052200000801     C                   ADD       1             §CTRNO
052300020725     C                   EVAL      x_vinflg = '2'
052400000801     C                   ELSE
052500010201     C                   ADD       1             §CTROKVB
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900010201     C*----------------------------------------------------*
053000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
053100010201     C*----------------------------------------------------*
053200071003     C     WRIVAT_B      BEGSR
053300090204     C*
053400090204     C                   eval      vatAAS = vabAAS
053500090204     C                   eval      vatLNP = vabLNP
053600090204     C                   eval      vatNRS = vabNRS
053700090204     C                   eval      vatNSP = vabNSP
053800010201     C*
053900101007     C* Valorizzo il buffer di scrittura del FIVAT
054000070928     C* - TIPO RECORD "B"
054100070928     C                   if        %subst(vindta:92:15) <> *blanks
054200070928     C                   eval      VATTRC = 'B'
054300070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
054400021113     C                   write     FIVAT000
054500020725     C                   endif
054600010201     C*
054700010201     C                   ENDSR
054800171213     C*----------------------------------------------------*
054900171213     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"/"J"
055000171213     C*----------------------------------------------------*
055100171213     C     WRIVAT_IJ     BEGSR
055200171213     C*
055300171213     C                   eval      vatAAS = vabAAS
055400171213     C                   eval      vatLNP = vabLNP
055500171213     C                   eval      vatNRS = vabNRS
055600171213     C                   eval      vatNSP = vabNSP
055700171213     C*
055800171213     C* Valorizzo il buffer di scrittura del FIVAT
055900171213     C* - TIPO RECORD "I"
056000171213     C                   if        %subst(vindta:482:70) <> *blanks
056100171213     C                   eval      w70 = %trim(%subst(vindta:482:70))
056200171213     C                   eval      VATTRC = 'I'
056300171213     C                   eval      VATNOT = %trim(%subst(w70:1:35))
056400171213     C                   write     FIVAT000
056500171213     C*
056600171213     C* - TIPO RECORD "J"
056700171213     C                   if        %subst(w70:36:35) <> *blank
056800171213     C                   eval      VATTRC = 'J'
056900171213     C                   eval      VATNOT = %trim(%subst(w70:36:35))
057000171213     C                   write     FIVAT000
057100171213     C                   endif
057200171213     C*
057300171213     C                   endif
057400171213     C*
057500171213     C                   ENDSR
057600171213     C*----------------------------------------------------*
057700171213     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
057800171213     C*----------------------------------------------------*
057900171213     C     WRIVAT_S      BEGSR
058000171213     C*
058100171213     C                   eval      vatAAS = vabAAS
058200171213     C                   eval      vatLNP = vabLNP
058300171213     C                   eval      vatNRS = vabNRS
058400171213     C                   eval      vatNSP = vabNSP
058500171213     C*
058600171213     C* Valorizzo il buffer di scrittura del FIVAT
058700171213     C* - TIPO RECORD "S"
058800171213     C                   eval      VATTRC = 'S'
058900171213     C                   eval      VATNOT = %subst(vindta:552:16)
059000171213     C* imposto i flag perché potrebbe essere che non passano il cellulare ma solo l'email
059100171213     C                   EVAL      %subst(VATNOT:17:2) = 'SS'
059200171213     C                   write     FIVAT000
059300171213     C*
059400171213     C                   ENDSR
059500071003     C*----------------------------------------------------*
059600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059700071003     C*----------------------------------------------------*
059800071003     C     WRIVAT_E      BEGSR
059900090204     C*
060000090204     C                   eval      vatAAS = vabAAS
060100090204     C                   eval      vatLNP = vabLNP
060200090204     C                   eval      vatNRS = vabNRS
060300090204     C                   eval      vatNSP = vabNSP
060400071003     C*
060500101007     C* Valorizzo il buffer di scrittura del FIVAT
060600071003     C* - TIPO RECORD "E"
060700071003     C                   if        %subst(vindta:331:26) <> *blanks
060800071003     C                   eval      VATTRC = 'E'
060900071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
061000071003     C                   write     FIVAT000
061100071003     C                   endif
061200071003     C*
061300071003     C                   ENDSR
061400020904
061500020904
061600010202     C*----------------------------------------------------*
061700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061800010202     C*----------------------------------------------------*
061900020305     C     PREVAT        BEGSR
062000010202     C*
062100021113     C* Compongo il nome del membro da dare al FIVATWWR
062200010202     C                   eval      parmbr = vlrhdl
062300010202     C                   movel     'M'           parmbr
062400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062500010202     C                   eval      paropz = '1'
062600010202     C* Effettuo la chiamata al CLLE preposto
062700050128     C                   call(e)   'TITVVTC'
062800010202     C                   parm                    parccm
062900010202     C                   parm                    parmbr
063000010202     C                   parm                    paropz
063100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063200010202     C                   if        %error
063300010202     C                   movel     '1'           chkcall
063400010202     C                   else
063500010202     C                   movel     '0'           chkcall
063600010202     C                   endif
063700010202     C*
063800010202     C                   ENDSR
063900000801     C*----------------------------------------------------*
064000000801     C*  CONTROLLO NUMERICITA' CAMPI
064100000801     C*----------------------------------------------------*
064200000801     C     CHKNUM        BEGSR
064300081113     C*
064400081113     C                   IF        PiDecChr = *blanks
064500121214     C                   EVAL      PiDecChr = ','
064600081113     C                   ENDIF
064700081113     C*
064800081113     C                   callp(e)  UBISNUM_Check(PiStr
064900081113     C                                          :PiDecChr
065000081113     C                                          :PiVal
065100081113     C                                          :PiNum
065200081113     C                                          :PiInt)
065300081113     C*
065400000801     C                   IF        %error
065500000801     C                   EVAL      PiInt=*off
065600000801     C                   ENDIF
065700000801     C*
065800000801     C                   ENDSR
065900000801     C***
066000000801
066100011113
066200011113     C*----------------------------------------------------*
066300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066400011113     C*----------------------------------------------------*
066500011113     C     CHKIMPDIV     BEGSR
066600011113     C*
066700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066800011113     C                   Z-ADD     *zeros        wrkDec            9 9
066900011113     C*
067000011113     C* Come prima cosa effettuo considerazioni sulla divisa
067100011113     C                   IF        vabIAS > *zeros
067200011113     C                   IF        vabVAS <> 'EUR'
067300011113     C                   EVAL      vabVAS =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabCAS > *zeros
067800011113     C                   IF        vabVCA <> 'EUR'
067900011113     C                   EVAL      vabVCA =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C                   IF        vabVMD > *zeros
068400020305     C                   IF        vabVAD <> 'EUR'
068500011113     C                   EVAL      vabVAD =  'ITL'
068600011113     C                   ENDIF
068700011113     C                   ENDIF
068800011113     C*
068900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069000011113     C                   Z-ADD     vabIAS        wrkDec
069100011113     C                   IF        wrkDec > *zeros
069200011113     C                   IF        vabVAS = 'ITL'
069300011113     C                   EVAL      vabIAS = *zeros
069400011113     C                   ENDIF
069500011113     C                   ENDIF
069600011113     C*
069700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069800011113     C                   Z-ADD     vabCAS        wrkDec
069900011113     C                   IF        wrkDec > *zeros
070000011113     C                   IF        vabVCA = 'ITL'
070100011113     C                   EVAL      vabCAS = *zeros
070200011113     C                   ENDIF
070300011113     C                   ENDIF
070400011113     C*
070500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070600011113     C                   Z-ADD     vabVMD        wrkDec
070700011113     C                   IF        wrkDec > *zeros
070800011113     C                   IF        vabVAD = 'ITL'
070900011113     C                   EVAL      vabVMD = *zeros
071000011113     C                   ENDIF
071100011113     C                   ENDIF
071200011113     C*
071300011113     C                   ENDSR
071400011113     C***
071500011113
071600011113
071700000801
071800000801
071900990920      /TITLE Invio dei dati al punto operativo.
072000010202     C     invio         BEGSR
072100990920     C*
072200021113     C* 1° invio FIVAT
072300010201     C                   reset                   dscmz
072400010201     C                   move      vlrpoi        cmzdst
072500021113     C                   eval      cmzfld = 'FIVATWWR'
072600010201     C                   eval      cmzmbd = vlrhdl
072700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072800021009     C***                if        prmfir = *blanks
072900021113     C                   eval      cmzfla = 'FIVAT00F'
073000021113     C                   eval      cmzmba = 'FIVAT00F'
073100021009     C***                else
073200021009     C***                eval      cmzfla = prmfir
073300021009     C***                eval      cmzmba = prmfir
073400021009     C***                endif
073500010201     C                   eval      cmznrr = *zeros
073600020305     C                   move      §ctrokvt      cmznrr
073700021018     C                   eval      cmzlba = vlrfl1
073800010201     C                   call(e)   'TIS711C'
073900010201     C                   parm                    dscmz
074000010201     C                   parm      *blanks       esito
074100010205     C                   if        %error
074200010205     C                             or cmzerr = '1'
074300010205     C                             or esito  = '1'
074400010205     C                   eval      wrkesito = '3'
074500010205     C                   else
074600010201     C*
074700021113     C* 2° invio FIVAB
074800010201     C                   reset                   dscmz
074900010201     C                   move      vlrpoi        cmzdst
075000010201     C                   eval      cmzfld = vlrfou
075100010201     C                   eval      cmzmbd = vlrhdl
075200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075300021009     C***                if        prmfir = *blanks
075400021113     C                   eval      cmzfla = 'FIVAB00F'
075500021113     C                   eval      cmzmba = 'FIVAB00F'
075600021009     C***                else
075700021009     C***                eval      cmzfla = prmfir
075800021009     C***                eval      cmzmba = prmfir
075900021009     C***                endif
076000010201     C                   eval      cmznrr = *zeros
076100010201     C                   move      §ctrokvb      cmznrr
076200021018     C                   eval      cmzlba = vlrfl1
076300010201     C                   call(e)   'TIS711C'
076400010201     C                   parm                    dscmz
076500010201     C                   parm      *blanks       esito
076600010201     C                   if        %error
076700010201     C                             or cmzerr = '1'
076800010201     C                             or esito  = '1'
076900010201     C                   eval      wrkesito = '3'
077000010201     C                   endif
077100010205     C                   endif
077200990920     C*
077300000613     C                   ENDSR
077400000613     C***
077500070411
077600070411     C     *pssr         BEGSR
077700070411     C*
077800070411     C                   if        %open(tivin00r)
077900070411     C                   close     tivin00r
078000070411     C                   endif
078100070411     C                   if        %open(fivabwwr)
078200070411     C                   close     fivabwwr
078300070411     C                   endif
078400070411     C                   if        %open(fivatwwr)
078500070411     C                   close     fivatwwr
078600070411     C                   endif
078700070411     C*
078800070411     C* Effettuo la chiamata al CLLE preposto
078900070411     C                   call(e)   'TITVVTC'
079000070411     C                   parm                    parccm
079100070411     C                   parm                    parmbr
079200070411     C                   parm      '2'           paropz
079300070411     C*
079400070411     C                   eval      wrkesito = '2'
079500070411     C*
079600070411     C                   seton                                        LR
079700070411     C*
079800070411     C                   ENDSR     '*CANCL'
079900070411     C***
080000070411
080100990910
080200000613     C     *inzsr        BEGSR
080300990910     C*
080400990910     C     *entry        plist
080500990920     C                   parm                    tivlrds
080600990921     C                   parm      wrkesito      esito
080700000724     C                   parm                    prmlit
080800000710     C                   parm                    prmfir
080900000613     C*
081000000830     C* CALCOLA LA DATA CORRENTE
081100100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081200100324     C                   eval      datcor = %dec(%date() : *ISO)
081300000830     C*
081400000613     C                   ENDSR
081500000613     C***
