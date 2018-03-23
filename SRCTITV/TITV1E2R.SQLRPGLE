000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
002800070530     D Num5_0          s              5  0
002900000830
003000000830     D*------------------
003100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200000830     D*------------------
003300000830     D WLBDA8          DS                  INZ
003400000830     D  G08DAT                 1      8  0
003500000830     D  G08INV                 9     16  0
003600000830     D  G08ERR                17     17
003700000830     D  G08TGI                18     22  0
003800000830     D*
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400020725     D  VINDTA                 1   2048
004500020725     D  VINFLG              2049   2049
004600020725     D  VINSPE              2050   2059
004700020725     D  VINRRN              2060   2067  0
004800020725     D*
004900990908
005000010201
005100010201
005200080117     C*
005300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005400080117     C
005500080117     C/EXEC SQL
005600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005700080117     C/END-EXEC
005800080117     C
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000613     C*
006300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
006400000613     C*
006500010202     C* Effettuo la chiamata al CLLE preposto
006600050128     C                   call(e)   'TITVVTC'
006700010202     C                   parm                    parccm
006800010202     C                   parm                    parmbr
006900010202     C                   parm      '2'           paropz
007000070730     C*
007100070730     C* Effettuo lancio TISI95 solo x chiusura
007200070730     C                   CLEAR                   TISI95DS
007300070730     C                   EVAL      I95TLA = 'C'
007400070730     C                   CALL      'TISI95R'
007500070730     C                   PARM                    TISI95DS
007600000616     C*
007700000801     C
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008300910830     C*--------------------------------------------------------
008400070530     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900021113     C                   if        not %open(fivabwwr)
009000021113     C                   open      fivabwwr
009100990914     C                   endif
009200070530     C*
009300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009400020305     C                   exsr      prevat
009500010201     C*
009600010202     C                   if        chkcall = '0'
009700010202     C*
009800021113     C                   if        not %open(fivatwwr)
009900021113     C                   open      fivatwwr
010000010201     C                   endif
010100080117     C*
010200080117     C                   EXSR      INZVAR
010300080117     C                   EXSR      DEFCAM
010400990910     C*
010500010201     C                   clear                   §CTROKVB          5 0
010600020305     C                   clear                   §CTROKVT          5 0
010700000801     C                   clear                   §CTRMO            5 0
010800000801     C                   clear                   §CTRNO            5 0
010900990910     C*
011000020725     C/EXEC SQL
011100020725     C+ declare C1 cursor for select
011200070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
011300060223     C+ from tivin00r
011400060223     C+ order by sped
011500020725     C+ for read only
011600020725     C/END-EXEC
011700020725     C
011800020725     C/EXEC SQL
011900020725     C+ open C1
012000020725     C/END-EXEC
012100020725     C
012200020725     C/EXEC SQL
012300070530     C+ Fetch C1 into :INPUT_DS
012400020725     C/END-EXEC
012500020725     C*
012600020725     C                   dow       sqlcod = *zeros
012700990913     C*
012800020725     C                   if        vindta > *blanks
012900000613     C                   add       1             rrnum
013000000801     C*
013100020725     C                   if        vinflg = *blanks
013200020725     C                             or vinflg = '0'
013300020725     C                             or vinflg = '2'
013400000801     C*
013500020725     C                   clear                   x_vinmsg
013600020725     C                   eval      x_vinflg = '1'
013700010305     C*
013800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
013900070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
014000020305     C                   MOVEL(p)  PiStr         curspe
014100010305     C*
014200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014300071003     C                   if        *in50 = *off
014400071003     C                   exsr      impvab
014500071003     C                   exsr      wrivab
014600071003     C                   exsr      wrivat_b                                     => carico VAT
014700071003     C                   else
014800071003     C*
014900071009     C                   if        wDISK = 'D'
015000071009     C                   exsr      impvab
015100071009     C                   exsr      wrivab
015200071009     C                   exsr      wrivat_b                                     => carico VAT
015300071009     C                   exsr      wrivat_e                                     => carico VAT
015400071009     C                   else
015500071009     C*
015600010305     C                   if        depspe = *blanks                             => 1° giro
015700010305     C                   eval      depspe = curspe                              => memorizz. spediz
015800080117     C                   clear                   tivinds
015900020305     C                   exsr      impvab
016000071003     C                   exsr      wrivat_b                                     => carico VAT
016100071003     C   50              exsr      wrivat_e                                     => carico VAT
016200010305     C                   else
016300020725     C                   if        depspe <> curspe                             => rottura di spediz
016400010305     C                   eval      depspe = curspe                              => memorizz. spediz
016500070928     C                   exsr      wrivab
016600080117     C                   clear                   tivinds
016700020305     C                   exsr      impvab
016800071003     C                   exsr      wrivat_b                                     => carico VAT
016900071003     C   50              exsr      wrivat_e                                     => carico VAT
017000020305     C                   else                                                   => x stessa spediz
017100020305     C                   exsr      impvab
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300071003     C   50              exsr      wrivat_e                                     => carico VAT
017400010305     C                   endif
017500010305     C                   endif
017600010305     C                   endif
017700071003     C                   endif
017800071009     C                   endif
017900000905     C*
018000000905     C                   else
018100020725     C                   eval      x_vinflg = '1'
018200000905     C                   endif
018300000905     C*
018400020725     C     VINRRN        chain     tivin000
018500020725     C                   if        %found(tivin00r)
018600020725     C                   eval      y_vinflg = x_vinflg
018700020725     C                   eval      y_vinmsg = x_vinmsg
018800020725     C                   update    tivin000
018900020725     C                   endif
019000020725     C*
019100020725     C/EXEC SQL
019200070530     C+ Fetch C1 into :INPUT_DS
019300020725     C/END-EXEC
019400020725     C*
019500020725     C                   enddo
019600020725     C*
019700020725     C/EXEC SQL
019800020725     C+ close C1
019900020725     C/END-EXEC
020000000905     C*
020100020305     C* Scarico i VAB rimasti "in sospeso"
020200071009     C                   if        wDISK = 'C'
020300071009     C                   exsr      wrivab
020400071009     C                   endif
020500010202     C*
020600010202     C                   endif
020700990910
020800990910     C* Se non ci sono record con errori ...
020900000710     C                   if        §ctrno = 0
021000990910     C* ... restituisco esito OK.
021100990921     C                   eval      wrkesito = '0'
021200990910     C                   else
021300010201     C                   if        §ctrokvb > 0
021400990921     C                   eval      wrkesito = '1'
021500000710     C                   else
021600000710     C                   eval      wrkesito = '2'
021700990910     C                   endif
021800000710     C                   endif
021900990910     C*
022000990914     C                   if        %open(tivin00r)
022100990908     C                   close     tivin00r
022200990914     C                   endif
022300021113     C                   if        %open(fivabwwr)
022400021113     C                   close     fivabwwr
022500990914     C                   endif
022600021113     C                   if        %open(fivatwwr)
022700021113     C                   close     fivatwwr
022800010201     C                   endif
022900990910     C*
023000010201     C                   if        §ctrokvb > 0
023100000724     C                             and vlrpoi <> *zeros
023200010202     C                   exsr      invio
023300990920     C                   endif
023400990920     C*
023500910830     C                   ENDSR
023600000613     C***
023700010305
023800010305     C*----------------------------------------------------*
023900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024000010305     C*----------------------------------------------------*
024100020305     C     WRIVAB        BEGSR
024200070730     C*
024300071003     C* Considerazioni finali
024400071003     C                   if        VABRMA = *blanks
024500071003     C                   movel(P)  VABRMN        VABRMA
024600071003     C                   endif
024700071003     C*
024800021113     C                   write     fivab000                                     => scarico il VAB
024900010305     C*
025000010305     C                   ENDSR
025100990920
025200000801     C*----------------------------------------------------*
025300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025400000801     C*----------------------------------------------------*
025500010201     C     INZVAR        BEGSR
025600000801     C*
025700010201     C                   Z-ADD     *zeros        Num5_0
025800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
025900020725     C                   MOVEL     '0'           FlgCAS            1
026000000801     C*
026100000801     C                   ENDSR
026200000801     C*----------------------------------------------------*
026300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026400000801     C*----------------------------------------------------*
026500020904     C     DEFCAM        BEGSR
026600000801     C*
026700021113     C                   CLEAR                   FIVAB000
026800021113     C                   CLEAR                   FIVAT000
026900070730     C* Imposto i valori di default...
027000080226     C                   EVAL      VABCCM = 0013836
027100080226     C                   EVAL      VATCCM = 0013836
027200080226     C                   EVAL      VABLNP = 001
027300080226     C                   EVAL      VATLNP = 001
027400070928     C                   EVAL      VABCTR = 000
027500070730     C                   EVAL      VABCBO = '1'
027600070222     C* ... e poi verifico se sono stati passati come parametri
027700070222     C                   IF        vlrppt > *blanks
027800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
027900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028000070222     C                   EXSR      CHKNUM
028100070222     C                   IF        PiInt=*on
028200070222     C                   Z-ADD     PiVal         VABCCM
028300070222     C                   Z-ADD     PiVal         VATCCM
028400070222     C                   ENDIF
028500070222     C                   ENDIF
028600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
028700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028800070222     C                   EXSR      CHKNUM
028900070222     C                   IF        PiInt=*on
029000070222     C                   Z-ADD     PiVal         VABLNP
029100070222     C                   Z-ADD     PiVal         VATLNP
029200070222     C                   ENDIF
029300070222     C                   ENDIF
029400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABCTR
029900070222     C                   ENDIF
030000070928     C                   ENDIF
030100071009     C                   MOVEL     *blanks       wDISK             1
030200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
030300071009     C                   IF        wDISK <> *blanks
030400070928     C                   SETON                                        50
030500070928     C                   ELSE
030600070928     C                   SETOFF                                       50
030700070222     C                   ENDIF
030800070222     C                   ENDIF
030900071009     C*
031000071009     C   50              EVAL      VABCTM = '7Q'
031100000801     C*
031200000801     C                   ENDSR
031300000801     C*----------------------------------------------------*
031400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031500000801     C*----------------------------------------------------*
031600010201     C     IMPVAB        BEGSR
031700070530     C*
031800070530     C                   SETOFF                                       3132
031900070928     C*
032000070928     C                   EXSR      INZVAR
032100070928     C                   EXSR      DEFCAM
032200070928     C*
032300070928     C                   MOVE(P)   vlrpoi        VABFGS
032400070928     C                   MOVE(P)   vlrpoi        VATFGS
032500070928     C*
032600070928     C                   MOVEL     datcor        VABAAS
032700070928     C                   MOVEL     datcor        VATAAS
032800070928     C                   MOVE      datcor        VABMGS
032900070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
033000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
033200070928     C* ==
033300070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
033400070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
033500070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
033600070928     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
033700070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
033800080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
033900070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
034000081027     C                   EVAL      VABNOT=%trim(%subst(vindta:93:15))
034100081027     C                   EVAL      VABNT2=%trim(%subst(vindta:145:35))
034200080226     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
034300080226     C                   EVAL      VABCBO='1'
034400080226     C                   ENDIF
034500080226     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
034600080226     C                   EVAL      VABCBO='2'
034700080226     C                   ENDIF
034800080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
034900080226     C                   EVAL      VABTSP='E'
035000080226     C                   ENDIF
035100080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
035200080226     C                   EVAL      VABFFD='S'
035300080226     C                   ENDIF
035400081027     C                   IF        %trim(%subst(vindta:13:1)) = 'A'
035500080328     C                   EVAL      VABTC1='A'
035600080226     C                   ENDIF
035700080829     C                   IF        %trim(%subst(vindta:197:1)) = *blanks
035800080328     C                   EVAL      VABTIC=*blanks
035900080328     C                   ENDIF
036000080829     C                   IF        %trim(%subst(vindta:197:1)) = 'B'
036100080328     C                   EVAL      VABTIC='TM'
036200080328     C                   ENDIF
036300080925     C                   IF        %trim(%subst(vindta:197:1)) = 'V'
036400080925     C                   EVAL      VABTIC='BM'
036500080925     C                   ENDIF
036600071003     C* CAD
036700070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
036800070928     C                   EXSR      CHKNUM
036900070928     C                   IF        PiInt=*on
037000070928     C                   Z-ADD     PiVal         Num5_0
037100070928     C                   MOVEL     Num5_0        VABCAD
037200070928     C                   ELSE
037300070928     C                   SETON                                        32
037400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037500070928     C                             + ' ' + 'VABCAD'
037600070928     C                   ENDIF
037700071003     C* NSP/RMN
037800080117     C                   EVAL      PiStr=%trim(%subst(vindta:187+1:10-1))
037900070928     C                   EXSR      CHKNUM
038000070928     C                   IF        PiInt=*on
038100070928     C                   Z-ADD     PiVal         VABRMN
038200070928     C                   Z-ADD     PiVal         VABNSP
038300070928     C                   Z-ADD     PiVal         VATNSP
038400070928     C                   ELSE
038500070928     C                   SETON                                        31
038600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038700070928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038800070928     C                   ENDIF
038900071003     C* NCL
039000070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
039100070928     C                   EXSR      CHKNUM
039200070928     C                   IF        PiInt=*on
039300070928     C                   Z-ADD     PiVal         VABNCL
039400070928     C                   ELSE
039500070928     C                   SETON                                        32
039600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700070928     C                             + ' ' + 'VABNCL'
039800070928     C                   ENDIF
039900071003     C* PKB
040000070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
040100070928     C                   EXSR      CHKNUM
040200070928     C                   IF        PiNum=*on
040300070928     C                   Z-ADD     PiVal         VABPKB
040400070928     C                   ELSE
040500070928     C                   SETON                                        32
040600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040700070928     C                             + ' ' + 'VABPKB'
040800070928     C                   ENDIF
040900071003     C* CAS
041000070928     C                   IF        %trim(%subst(vindta:280:12))<>*blanks AND
041100070928     C                             %trim(%subst(vindta:280:12))<>*zeros  AND
041200080229     C                             %trim(%subst(vindta:280:12))<>'0,00'  AND
041300080229     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
041400070928     C                   MOVEL     '1'           FlgCAS
041500070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
041600070928     C                   EXSR      CHKNUM
041700070928     C                   IF        PiNum=*on
041800070928     C                   Z-ADD     PiVal         VABCAS
041900070928     C                   ELSE
042000070928     C                   SETON                                        32
042100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200070928     C                             + ' ' + 'VABCAS'
042300070928     C                   ENDIF
042400070928     C                   ENDIF
042500071003     C* IAS
042600071003     C                   IF        %trim(%subst(vindta:295:12))<>*blanks AND
042700071003     C                             %trim(%subst(vindta:295:12))<>*zeros  AND
042800080229     C                             %trim(%subst(vindta:295:12))<>'0,00'  AND
042900080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
043000080229     C***                EVAL      PiStr=%trim(%subst(vindta:295:12))
043100080229     C***                EXSR      CHKNUM
043200080229     C***                IF        PiNum=*on
043300080229     C***                Z-ADD     PiVal         VABIAS
043400080229     C***                ELSE
043500080229     C***                SETON                                        32
043600080229     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
043700080229     C***                          + ' ' + 'VABIAS'
043800080229     C***                ENDIF
043900071003     C                   ENDIF
044000070730     C*
044100070730     C* Considerazioni finali su CBO/CAS
044200070730     C                   IF        FlgCAS = '1'
044300070730     C                   IF        VABCBO = '1'
044400070730     C                   EVAL      VABCBO = '4'
044500070730     C                   ENDIF
044600070730     C                   IF        VABCBO = '2'
044700070730     C                   EVAL      VABCBO = '6'
044800070730     C                   ENDIF
044900070730     C                   ENDIF
045000020305     C*
045100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045200011113     C                   EXSR      CHKIMPDIV
045300010202     C*
045400000801     C* Ebbene...
045500000801     C                   ADD       1             §CTRMO
045600070530     C                   IF        *in31 <> *zeros OR
045700070530     C                             *in32 <> *zeros
045800000801     C                   ADD       1             §CTRNO
045900020725     C                   EVAL      x_vinflg = '2'
046000000801     C                   ELSE
046100010201     C                   ADD       1             §CTROKVB
046200000801     C                   ENDIF
046300000801     C*
046400000801     C                   ENDSR
046500010201     C*----------------------------------------------------*
046600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
046700010201     C*----------------------------------------------------*
046800071003     C     WRIVAT_B      BEGSR
046900010201     C*
047000021113     C* Valorizzo l buffer di scrittura del FIVAT
047100070928     C* - TIPO RECORD "B"
047200070928     C                   if        %subst(vindta:92:15) <> *blanks
047300070928     C                   eval      VATTRC = 'B'
047400070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
047500021113     C                   write     FIVAT000
047600020725     C                   endif
047700010201     C*
047800010201     C                   ENDSR
047900071003     C*----------------------------------------------------*
048000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
048100071003     C*----------------------------------------------------*
048200071003     C     WRIVAT_E      BEGSR
048300071003     C*
048400071003     C* Valorizzo l buffer di scrittura del FIVAT
048500071003     C* - TIPO RECORD "E"
048600071003     C                   if        %subst(vindta:331:26) <> *blanks
048700071003     C                   eval      VATTRC = 'E'
048800071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
048900071003     C                   write     FIVAT000
049000071003     C                   endif
049100071003     C*
049200071003     C                   ENDSR
049300020904
049400020904
049500010202     C*----------------------------------------------------*
049600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000021113     C* Compongo il nome del membro da dare al FIVATWWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600050128     C                   call(e)   'TITVVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100000801     C     CHKNUM        BEGSR
052200000801     C*
052300000801     C                   call(e)   'ISNUMERIC'
052400000801     C                   PARM                    PiStr            30
052500080226     C                   PARM      ','           PiDecChr          1
052600000801     C                   PARM      *ZEROS        PiVal            30 9
052700000801     C                   PARM      '0'           PiInt             1
052800000801     C                   PARM      '0'           PiNum             1
052900000801     C                   IF        %error
053000000801     C                   EVAL      PiInt=*off
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400000801     C***
053500000801
053600011113
053700011113     C*----------------------------------------------------*
053800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053900011113     C*----------------------------------------------------*
054000011113     C     CHKIMPDIV     BEGSR
054100011113     C*
054200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054300011113     C                   Z-ADD     *zeros        wrkDec            9 9
054400011113     C*
054500011113     C* Come prima cosa effettuo considerazioni sulla divisa
054600011113     C                   IF        vabIAS > *zeros
054700011113     C                   IF        vabVAS <> 'EUR'
054800011113     C                   EVAL      vabVAS =  'ITL'
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   IF        vabCAS > *zeros
055300011113     C                   IF        vabVCA <> 'EUR'
055400011113     C                   EVAL      vabVCA =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   IF        vabVMD > *zeros
055900020305     C                   IF        vabVAD <> 'EUR'
056000011113     C                   EVAL      vabVAD =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056500011113     C                   Z-ADD     vabIAS        wrkDec
056600011113     C                   IF        wrkDec > *zeros
056700011113     C                   IF        vabVAS = 'ITL'
056800011113     C                   EVAL      vabIAS = *zeros
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057300011113     C                   Z-ADD     vabCAS        wrkDec
057400011113     C                   IF        wrkDec > *zeros
057500011113     C                   IF        vabVCA = 'ITL'
057600011113     C                   EVAL      vabCAS = *zeros
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058100011113     C                   Z-ADD     vabVMD        wrkDec
058200011113     C                   IF        wrkDec > *zeros
058300011113     C                   IF        vabVAD = 'ITL'
058400011113     C                   EVAL      vabVMD = *zeros
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C                   ENDSR
058900011113     C***
059000011113
059100011113
059200000801
059300000801
059400990920      /TITLE Invio dei dati al punto operativo.
059500010202     C     invio         BEGSR
059600990920     C*
059700021113     C* 1° invio FIVAT
059800010201     C                   reset                   dscmz
059900010201     C                   move      vlrpoi        cmzdst
060000021113     C                   eval      cmzfld = 'FIVATWWR'
060100010201     C                   eval      cmzmbd = vlrhdl
060200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060300021009     C***                if        prmfir = *blanks
060400021113     C                   eval      cmzfla = 'FIVAT00F'
060500021113     C                   eval      cmzmba = 'FIVAT00F'
060600021009     C***                else
060700021009     C***                eval      cmzfla = prmfir
060800021009     C***                eval      cmzmba = prmfir
060900021009     C***                endif
061000010201     C                   eval      cmznrr = *zeros
061100020305     C                   move      §ctrokvt      cmznrr
061200021018     C                   eval      cmzlba = vlrfl1
061300010201     C                   call(e)   'TIS711C'
061400010201     C                   parm                    dscmz
061500010201     C                   parm      *blanks       esito
061600010205     C                   if        %error
061700010205     C                             or cmzerr = '1'
061800010205     C                             or esito  = '1'
061900010205     C                   eval      wrkesito = '3'
062000010205     C                   else
062100010201     C*
062200021113     C* 2° invio FIVAB
062300010201     C                   reset                   dscmz
062400010201     C                   move      vlrpoi        cmzdst
062500010201     C                   eval      cmzfld = vlrfou
062600010201     C                   eval      cmzmbd = vlrhdl
062700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062800021009     C***                if        prmfir = *blanks
062900021113     C                   eval      cmzfla = 'FIVAB00F'
063000021113     C                   eval      cmzmba = 'FIVAB00F'
063100021009     C***                else
063200021009     C***                eval      cmzfla = prmfir
063300021009     C***                eval      cmzmba = prmfir
063400021009     C***                endif
063500010201     C                   eval      cmznrr = *zeros
063600010201     C                   move      §ctrokvb      cmznrr
063700021018     C                   eval      cmzlba = vlrfl1
063800010201     C                   call(e)   'TIS711C'
063900010201     C                   parm                    dscmz
064000010201     C                   parm      *blanks       esito
064100010201     C                   if        %error
064200010201     C                             or cmzerr = '1'
064300010201     C                             or esito  = '1'
064400010201     C                   eval      wrkesito = '3'
064500010201     C                   endif
064600010205     C                   endif
064700990920     C*
064800000613     C                   ENDSR
064900000613     C***
065000070411
065100070411     C     *pssr         BEGSR
065200070411     C*
065300070411     C                   if        %open(tivin00r)
065400070411     C                   close     tivin00r
065500070411     C                   endif
065600070411     C                   if        %open(fivabwwr)
065700070411     C                   close     fivabwwr
065800070411     C                   endif
065900070411     C                   if        %open(fivatwwr)
066000070411     C                   close     fivatwwr
066100070411     C                   endif
066200070411     C*
066300070411     C* Effettuo la chiamata al CLLE preposto
066400070411     C                   call(e)   'TITVVTC'
066500070411     C                   parm                    parccm
066600070411     C                   parm                    parmbr
066700070411     C                   parm      '2'           paropz
066800070411     C*
066900070411     C                   eval      wrkesito = '2'
067000070411     C*
067100070411     C                   seton                                        LR
067200070411     C*
067300070411     C                   ENDSR     '*CANCL'
067400070411     C***
067500070411
067600990910
067700000613     C     *inzsr        BEGSR
067800990910     C*
067900990910     C     *entry        plist
068000990920     C                   parm                    tivlrds
068100990921     C                   parm      wrkesito      esito
068200000724     C                   parm                    prmlit
068300000710     C                   parm                    prmfir
068400000613     C*
068500000830     C* CALCOLA LA DATA CORRENTE
068600000830     C                   time                    wn14             14 0
068700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068800000830     C                   z-add     wn8           g08dat
068900000830     C                   z-add     *zeros        g08inv
069000000830     C                   movel     '0'           g08err
069100000830     C                   call      'XSRDA8'
069200000830     C                   parm                    wlbda8
069300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069400000830     C*
069500000613     C                   ENDSR
069600000613     C***
