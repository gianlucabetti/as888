000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002100100122     D depspe          s             15    INZ(*blanks)
002200100122     D curspe          s             15    INZ(*blanks)
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
011900100122     C+ vindta, vinflg, substr(vindta, 80, 15) as sped, rrn(tivin00r)
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
014600100122     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
014700020305     C                   MOVEL(p)  PiStr         curspe
014800010305     C*
014900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015000080923     C                   if        *in50 = *off
015100091223     C                   exsr      repNSP
015200071003     C                   exsr      impvab
015300071003     C                   exsr      wrivab
015400071003     C                   exsr      wrivat_b                                     => carico VAT
015500071003     C                   else
015600080923     C*
015700071009     C                   if        wDISK = 'D'
015800091223     C                   exsr      repNSP
015900071009     C                   exsr      impvab
016000071009     C                   exsr      wrivab
016100071009     C                   exsr      wrivat_b                                     => carico VAT
016200071009     C                   exsr      wrivat_e                                     => carico VAT
016300071009     C                   else
016400071009     C*
016500010305     C                   if        depspe = *blanks                             => 1° giro
016600010305     C                   eval      depspe = curspe                              => memorizz. spediz
016700080117     C                   clear                   tivinds
016800091223     C                   exsr      repNSP
016900020305     C                   exsr      impvab
017000071003     C                   exsr      wrivat_b                                     => carico VAT
017100071003     C   50              exsr      wrivat_e                                     => carico VAT
017200010305     C                   else
017300020725     C                   if        depspe <> curspe                             => rottura di spediz
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500070928     C                   exsr      wrivab
017600080117     C                   clear                   tivinds
017700091223     C                   exsr      repNSP
017800020305     C                   exsr      impvab
017900071003     C                   exsr      wrivat_b                                     => carico VAT
018000071003     C   50              exsr      wrivat_e                                     => carico VAT
018100020305     C                   else                                                   => x stessa spediz
018200020305     C                   exsr      impvab
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
027800090310     C                   SETOFF                                       50
027900000801     C*
028000021113     C                   CLEAR                   FIVAB000
028100021113     C                   CLEAR                   FIVAT000
028200070730     C* Imposto i valori di default...
028300100122     C                   EVAL      VABCCM = 0990766
028400100122     C                   EVAL      VATCCM = 0990766
028500100122     C                   EVAL      VABLNP = 099
028600100122     C                   EVAL      VATLNP = 099
028700100122     C                   EVAL      VABCTR = 100
028800070730     C                   EVAL      VABCBO = '1'
028900100122     C                   EVAL      VABCTM = *blanks
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
032000070222     C                   ENDIF
032100071009     C*
032200071009     C   50              EVAL      VABCTM = '7Q'
032300000801     C*
032400000801     C                   ENDSR
032500091223     C*----------------------------------------------------*
032600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
032700091223     C*----------------------------------------------------*
032800091223     C     REPNSP        BEGSR
032900091223     C*
033000091223     C                   EXSR      INZVAR
033100091223     C                   EXSR      DEFCAM
033200091223     C*
033300091223     C* NSP => Stacco un numeratore da AZNUM
033400091223     C                   clear                   TRUL33DS
033500091223     C                   eval      I33OPE = *zeros
033600091223     C                   eval      I33CNU = 302
033700091223     C                   eval      I33NUM = 1
033800091223     C                   movel     TRUL33DS      KPJBU
033900091223     C                   call      'TRUL33R'
034000091223     C                   parm                    KPJBA
034100091223     C                   movel     KPJBU         TRUL33DS
034200091223     C                   if        O33ERR = *zeros
034300091223     C                   z-add     O33NRF        VABNSP
034400091223     C                   z-add     O33NRF        VATNSP
034500091223     C                   else
034600091223     C                   SETON                                        31
034700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
034800091223     C                             + ' ' + 'VABNSP VATNSP'
034900091223     C                   endif
035000091223     C*
035100091223     C                   ENDSR
035200000801     C*----------------------------------------------------*
035300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035400000801     C*----------------------------------------------------*
035500010201     C     IMPVAB        BEGSR
035600070530     C*
035700070530     C                   SETOFF                                       3132
035800070928     C*
035900070928     C                   MOVE(P)   vlrpoi        VABFGS
036000070928     C                   MOVE(P)   vlrpoi        VATFGS
036100070928     C*
036200070928     C                   MOVEL     datcor        VABAAS
036300070928     C                   MOVEL     datcor        VATAAS
036400070928     C                   MOVE      datcor        VABMGS
036500100122     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
036600070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036700070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036800070928     C* ==
036900100122     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
037000100122     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
037100100122     C                   EVAL      VABLOD=%trim(%subst(vindta:261:30))
037200100122     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
037300100122     C                   EVAL      VABRMA=%trim(%subst(vindta:95:15))
037400100122     C                   EVAL      VABVCA=%trim(%subst(vindta:392:4))
037500100122     C                   EVAL      VABNOT=%trim(%subst(vindta:464:35))
037600100122     C                   EVAL      VABNT2=%trim(%subst(vindta:464+35:60-35))
037700100122     C* DCR
037800100122     C                   IF        %trim(%subst(vindta:46:8))<>*blanks
037900100122     C                             AND
038000100122     C                             %trim(%subst(vindta:46:8))<>*zeros
038100100122     C                   EVAL      PiStr=%trim(%subst(vindta:46:8))
038200100122     C                   EXSR      CHKNUM
038300100122     C                   IF        PiInt=*on
038400100122     C                   ADD       PiVal         VABDCR
038500100122     C                   ELSE
038600100122     C                   SETON                                        32
038700100122     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038800100122     C                             + ' ' + 'VABDCR'
038900100122     C                   ENDIF
039000100122     C*
039100100122     C                   ELSE
039200100122     C*
039300100122     C                   IF        %trim(%subst(vindta:54:8))<>*blanks
039400100122     C                             AND
039500100122     C                             %trim(%subst(vindta:54:8))<>*zeros
039600100122     C                   EVAL      PiStr=%trim(%subst(vindta:54:8))
039700100122     C                   EXSR      CHKNUM
039800100122     C                   IF        PiInt=*on
039900100122     C                   EVAL      VABTCR = 'P'
040000100122     C                   ADD       PiVal         VABDCR
040100100122     C                   ELSE
040200100122     C                   SETON                                        32
040300100122     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400100122     C                             + ' ' + 'VABDCR'
040500100122     C                   ENDIF
040600100122     C                   ENDIF
040700100122     C                   ENDIF
040800071003     C* CAD
040900100122     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
041000070928     C                   EXSR      CHKNUM
041100070928     C                   IF        PiInt=*on
041200070928     C                   Z-ADD     PiVal         Num5_0
041300070928     C                   MOVEL     Num5_0        VABCAD
041400070928     C                   ELSE
041500070928     C                   SETON                                        32
041600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041700070928     C                             + ' ' + 'VABCAD'
041800070928     C                   ENDIF
041900080923     C* RMN
042000100122     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
042100080923     C                   EXSR      CHKNUM
042200080923     C                   IF        PiInt=*on
042300080923     C                   Z-ADD     PiVal         VABRMN
042400080923     C                   ELSE
042500080923     C                   SETON                                        32
042600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700080923     C                             + ' ' + 'VABRMN'
042800080923     C                   ENDIF
042900091223     C* NCL
043000100122     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
043100070928     C                   EXSR      CHKNUM
043200070928     C                   IF        PiInt=*on
043300091223     C                   ADD       PiVal         VABNCL
043400070928     C                   ELSE
043500070928     C                   SETON                                        32
043600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043700070928     C                             + ' ' + 'VABNCL'
043800070928     C                   ENDIF
043900100122     C* PKB
044000100201     C***                IF        %trim(%subst(vindta:360:7))<>*blanks
044100100201     C***                          AND
044200100201     C***                          %trim(%subst(vindta:360:7))<>*zeros
044300100201     C***                EVAL      PiStr=%trim(%subst(vindta:360:7))
044400100201     C***                EXSR      CHKNUM
044500100201     C***                IF        PiNum=*on
044600100201     C***                EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
044700100201     C***                ADD       PiVal         VABPKB
044800100201     C                   EVAL      VABPKB = VABPKB + (8 * VABNCL)
044900100201     C***                ELSE
045000100201     C***                SETON                                        32
045100100201     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
045200100201     C***                          + ' ' + 'VABPKB'
045300100201     C***                ENDIF
045400100201     C***                ENDIF
045500091223     C* VLB
045600100122     C                   IF        %trim(%subst(vindta:385:7))<>*blanks
045700091223     C                             AND
045800100122     C                             %trim(%subst(vindta:385:7))<>*zeros
045900100122     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
046000091223     C                   EXSR      CHKNUM
046100091223     C                   IF        PiNum=*on
046200100122     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
046300091223     C                   ADD       PiVal         VABVLB
046400091223     C                   ELSE
046500091223     C                   SETON                                        32
046600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700091223     C                             + ' ' + 'VABVLB'
046800091223     C                   ENDIF
046900091223     C                   ENDIF
047000080923     C* CAS
047100100122     C                   IF        %trim(%subst(vindta:396:15))<>*blanks
047200080923     C                             AND
047300100122     C                             %trim(%subst(vindta:396:15))<>*zeros
047400080923     C                             AND
047500100122     C                             %trim(%subst(vindta:396:15))<>'0,00'
047600080923     C                             AND
047700100122     C                             %trim(%subst(vindta:396:15))<>
047800100122     C                                  '00000000000,000'
047900080923     C                             AND
048000100122     C                             %trim(%subst(vindta:396:15))<>
048100100122     C                                  '00000000000.000'
048200070928     C                   MOVEL     '1'           FlgCAS
048300100122     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
048400070928     C                   EXSR      CHKNUM
048500070928     C                   IF        PiNum=*on
048600100122     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
048700070928     C                   Z-ADD     PiVal         VABCAS
048800070928     C                   ELSE
048900070928     C                   SETON                                        32
049000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100070928     C                             + ' ' + 'VABCAS'
049200070928     C                   ENDIF
049300070928     C                   ENDIF
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
052300010201     C*
052400021113     C* Valorizzo l buffer di scrittura del FIVAT
052500070928     C* - TIPO RECORD "B"
052600070928     C                   if        %subst(vindta:92:15) <> *blanks
052700070928     C                   eval      VATTRC = 'B'
052800070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
052900021113     C                   write     FIVAT000
053000020725     C                   endif
053100010201     C*
053200010201     C                   ENDSR
053300071003     C*----------------------------------------------------*
053400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
053500071003     C*----------------------------------------------------*
053600071003     C     WRIVAT_E      BEGSR
053700071003     C*
053800071003     C* Valorizzo l buffer di scrittura del FIVAT
053900071003     C* - TIPO RECORD "E"
054000071003     C                   if        %subst(vindta:331:26) <> *blanks
054100071003     C                   eval      VATTRC = 'E'
054200071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
054300071003     C                   write     FIVAT000
054400071003     C                   endif
054500071003     C*
054600071003     C                   ENDSR
054700020904
054800020904
054900010202     C*----------------------------------------------------*
055000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
055100010202     C*----------------------------------------------------*
055200020305     C     PREVAT        BEGSR
055300010202     C*
055400021113     C* Compongo il nome del membro da dare al FIVATWWR
055500010202     C                   eval      parmbr = vlrhdl
055600010202     C                   movel     'M'           parmbr
055700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
055800010202     C                   eval      paropz = '1'
055900010202     C* Effettuo la chiamata al CLLE preposto
056000050128     C                   call(e)   'TITVVTC'
056100010202     C                   parm                    parccm
056200010202     C                   parm                    parmbr
056300010202     C                   parm                    paropz
056400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056500010202     C                   if        %error
056600010202     C                   movel     '1'           chkcall
056700010202     C                   else
056800010202     C                   movel     '0'           chkcall
056900010202     C                   endif
057000010202     C*
057100010202     C                   ENDSR
057200000801     C*----------------------------------------------------*
057300000801     C*  CONTROLLO NUMERICITA' CAMPI
057400000801     C*----------------------------------------------------*
057500000801     C     CHKNUM        BEGSR
057600081113     C*
057700081113     C                   IF        PiDecChr = *blanks
057800091223     C                   EVAL      PiDecChr = ','
057900081113     C                   ENDIF
058000091223     C*
058100081113     C                   callp(e)  UBISNUM_Check(PiStr
058200081113     C                                          :PiDecChr
058300081113     C                                          :PiVal
058400081113     C                                          :PiNum
058500081113     C                                          :PiInt)
058600081113     C*
058700000801     C                   IF        %error
058800000801     C                   EVAL      PiInt=*off
058900000801     C                   ENDIF
059000000801     C*
059100000801     C                   ENDSR
059200000801     C***
059300000801
059400011113
059500011113     C*----------------------------------------------------*
059600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059700011113     C*----------------------------------------------------*
059800011113     C     CHKIMPDIV     BEGSR
059900011113     C*
060000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060100011113     C                   Z-ADD     *zeros        wrkDec            9 9
060200011113     C*
060300011113     C* Come prima cosa effettuo considerazioni sulla divisa
060400011113     C                   IF        vabIAS > *zeros
060500011113     C                   IF        vabVAS <> 'EUR'
060600011113     C                   EVAL      vabVAS =  'ITL'
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C                   IF        vabCAS > *zeros
061100011113     C                   IF        vabVCA <> 'EUR'
061200011113     C                   EVAL      vabVCA =  'ITL'
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C                   IF        vabVMD > *zeros
061700020305     C                   IF        vabVAD <> 'EUR'
061800011113     C                   EVAL      vabVAD =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062300011113     C                   Z-ADD     vabIAS        wrkDec
062400011113     C                   IF        wrkDec > *zeros
062500011113     C                   IF        vabVAS = 'ITL'
062600011113     C                   EVAL      vabIAS = *zeros
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063100011113     C                   Z-ADD     vabCAS        wrkDec
063200011113     C                   IF        wrkDec > *zeros
063300011113     C                   IF        vabVCA = 'ITL'
063400011113     C                   EVAL      vabCAS = *zeros
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063900011113     C                   Z-ADD     vabVMD        wrkDec
064000011113     C                   IF        wrkDec > *zeros
064100011113     C                   IF        vabVAD = 'ITL'
064200011113     C                   EVAL      vabVMD = *zeros
064300011113     C                   ENDIF
064400011113     C                   ENDIF
064500011113     C*
064600011113     C                   ENDSR
064700011113     C***
064800011113
064900011113
065000000801
065100000801
065200990920      /TITLE Invio dei dati al punto operativo.
065300010202     C     invio         BEGSR
065400990920     C*
065500021113     C* 1° invio FIVAT
065600010201     C                   reset                   dscmz
065700010201     C                   move      vlrpoi        cmzdst
065800021113     C                   eval      cmzfld = 'FIVATWWR'
065900010201     C                   eval      cmzmbd = vlrhdl
066000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066100021009     C***                if        prmfir = *blanks
066200021113     C                   eval      cmzfla = 'FIVAT00F'
066300021113     C                   eval      cmzmba = 'FIVAT00F'
066400021009     C***                else
066500021009     C***                eval      cmzfla = prmfir
066600021009     C***                eval      cmzmba = prmfir
066700021009     C***                endif
066800010201     C                   eval      cmznrr = *zeros
066900020305     C                   move      §ctrokvt      cmznrr
067000021018     C                   eval      cmzlba = vlrfl1
067100010201     C                   call(e)   'TIS711C'
067200010201     C                   parm                    dscmz
067300010201     C                   parm      *blanks       esito
067400010205     C                   if        %error
067500010205     C                             or cmzerr = '1'
067600010205     C                             or esito  = '1'
067700010205     C                   eval      wrkesito = '3'
067800010205     C                   else
067900010201     C*
068000021113     C* 2° invio FIVAB
068100010201     C                   reset                   dscmz
068200010201     C                   move      vlrpoi        cmzdst
068300010201     C                   eval      cmzfld = vlrfou
068400010201     C                   eval      cmzmbd = vlrhdl
068500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068600021009     C***                if        prmfir = *blanks
068700021113     C                   eval      cmzfla = 'FIVAB00F'
068800021113     C                   eval      cmzmba = 'FIVAB00F'
068900021009     C***                else
069000021009     C***                eval      cmzfla = prmfir
069100021009     C***                eval      cmzmba = prmfir
069200021009     C***                endif
069300010201     C                   eval      cmznrr = *zeros
069400010201     C                   move      §ctrokvb      cmznrr
069500021018     C                   eval      cmzlba = vlrfl1
069600010201     C                   call(e)   'TIS711C'
069700010201     C                   parm                    dscmz
069800010201     C                   parm      *blanks       esito
069900010201     C                   if        %error
070000010201     C                             or cmzerr = '1'
070100010201     C                             or esito  = '1'
070200010201     C                   eval      wrkesito = '3'
070300010201     C                   endif
070400010205     C                   endif
070500990920     C*
070600000613     C                   ENDSR
070700000613     C***
070800070411
070900070411     C     *pssr         BEGSR
071000070411     C*
071100070411     C                   if        %open(tivin00r)
071200070411     C                   close     tivin00r
071300070411     C                   endif
071400070411     C                   if        %open(fivabwwr)
071500070411     C                   close     fivabwwr
071600070411     C                   endif
071700070411     C                   if        %open(fivatwwr)
071800070411     C                   close     fivatwwr
071900070411     C                   endif
072000070411     C*
072100070411     C* Effettuo la chiamata al CLLE preposto
072200070411     C                   call(e)   'TITVVTC'
072300070411     C                   parm                    parccm
072400070411     C                   parm                    parmbr
072500070411     C                   parm      '2'           paropz
072600070411     C*
072700070411     C                   eval      wrkesito = '2'
072800070411     C*
072900070411     C                   seton                                        LR
073000070411     C*
073100070411     C                   ENDSR     '*CANCL'
073200070411     C***
073300070411
073400990910
073500000613     C     *inzsr        BEGSR
073600990910     C*
073700990910     C     *entry        plist
073800990920     C                   parm                    tivlrds
073900990921     C                   parm      wrkesito      esito
074000000724     C                   parm                    prmlit
074100000710     C                   parm                    prmfir
074200000613     C*
074300000830     C* CALCOLA LA DATA CORRENTE
074400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074500091223     C                   eval      datcor = %dec(%date() : *ISO)
074600000830     C*
074700000613     C                   ENDSR
074800000613     C***
