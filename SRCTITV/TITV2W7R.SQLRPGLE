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
002100130121     D depspe          s              7    INZ(*blanks)
002200130121     D curspe          s              7    INZ(*blanks)
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
003700130121     D  VINSPE                        7
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
005600130121     D w70             s             70
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
011900130121     C+ vindta, vinflg, substr(vindta, 83, 7) as sped, rrn(tivin00r)
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
014600130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
014700020305     C                   MOVEL(p)  PiStr         curspe
014800010305     C*
014900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015000080923     C                   if        *in50 = *off
015100071003     C                   exsr      impvab
015200071003     C                   exsr      wrivab
015300170220     C                   exsr      wrivat_ij                                    => carico VAT
015400170220     C                   exsr      wrivat_s                                     => carico VAT
015500071003     C                   else
015600080923     C*
015700071009     C                   if        wDISK = 'D'
015800071009     C                   exsr      impvab
015900071009     C                   exsr      wrivab
016000170220     C                   exsr      wrivat_ij                                    => carico VAT
016100170220     C                   exsr      wrivat_s                                     => carico VAT
016200130121     C***                exsr      wrivat_e                                     => carico VAT
016300071009     C                   else
016400071009     C*
016500010305     C                   if        depspe = *blanks                             => 1° giro
016600010305     C                   eval      depspe = curspe                              => memorizz. spediz
016700080117     C                   clear                   tivinds
016800020305     C                   exsr      impvab
016900170220     C                   exsr      wrivat_ij                                    => carico VAT
017000170220     C                   exsr      wrivat_s                                     => carico VAT
017100130121     C***50              exsr      wrivat_e                                     => carico VAT
017200010305     C                   else
017300020725     C                   if        depspe <> curspe                             => rottura di spediz
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500070928     C                   exsr      wrivab
017600080117     C                   clear                   tivinds
017700020305     C                   exsr      impvab
017800170220     C                   exsr      wrivat_ij                                    => carico VAT
017900170220     C                   exsr      wrivat_s                                     => carico VAT
018000130121     C***50              exsr      wrivat_e                                     => carico VAT
018100020305     C                   else                                                   => x stessa spediz
018200090204     C***                exsr      impvab
018300161219     C***                exsr      wrivat_ij                                    => carico VAT
018400170220     C***                exsr      wrivat_s                                     => carico VAT
018500130121     C***50              exsr      wrivat_e                                     => carico VAT
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
027900080923     C                   SETOFF                                       5051
028000000801     C*
028100021113     C                   CLEAR                   FIVAB000
028200021113     C                   CLEAR                   FIVAT000
028300070730     C* Imposto i valori di default...
028400161219     C                   EVAL      VABCCM = 0012811
028500161219     C                   EVAL      VATCCM = 0012811
028600161219     C                   EVAL      VABLNP = 001
028700161219     C                   EVAL      VATLNP = 001
028800161219     C                   EVAL      VABCTR = 000
028900130121     C                   EVAL      VABCTM = '7Q'
029000130121     C                   EVAL      VABCBO = '1'
029100161219     C                   EVAL      VABTSP = 'D'
029200070222     C* ... e poi verifico se sono stati passati come parametri
029300070222     C                   IF        vlrppt > *blanks
029400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABCCM
029900070222     C                   Z-ADD     PiVal         VATCCM
030000070222     C                   ENDIF
030100070222     C                   ENDIF
030200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABLNP
030700070222     C                   Z-ADD     PiVal         VATLNP
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABCTR
031500070222     C                   ENDIF
031600070928     C                   ENDIF
031700071009     C                   MOVEL     *blanks       wDISK             1
031800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031900071009     C                   IF        wDISK <> *blanks
032000070928     C                   SETON                                        50
032100070222     C                   ENDIF
032200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032300080923     C                   SETON                                        51
032400080923     C                   ENDIF
032500070222     C                   ENDIF
032600071009     C*
032700071009     C   50              EVAL      VABCTM = '7Q'
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200000801     C*----------------------------------------------------*
033300010201     C     IMPVAB        BEGSR
033400070530     C*
033500070530     C                   SETOFF                                       3132
033600070928     C*
033700070928     C                   EXSR      INZVAR
033800070928     C                   EXSR      DEFCAM
033900070928     C*
034000070928     C                   MOVE(P)   vlrpoi        VABFGS
034100070928     C                   MOVE(P)   vlrpoi        VATFGS
034200070928     C*
034300070928     C                   MOVEL     datcor        VABAAS
034400161219     C                   MOVEL     datcor        VATAAS
034500070928     C                   MOVE      datcor        VABMGS
034600130121     C*
034700130121     C* Reperimento campi ALFA
034800161219     C                   EVAL      VABRMO=%trim(%subst(vindta:114:30))
034900161219     C                   EVAL      VABCMO=%trim(%subst(vindta:209:5))
035000130121     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
035100130121     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
035200130121     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
035300130121     C                   EVAL      VABCAD=%trim(%subst(vindta:319:8))
035400130121     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
035500130121     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
035600130121     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
035700130121     C*
035800130121     C* Considerazioni sulla ragione sociale del destinatario da indicare
035900130121     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036000130121     C     '@':'A'       XLATE     VABRSD        VABRSD
036100130121     C* ==
036200130121     C*
036300130121     C* GC1
036400130121     C                   IF        %subst(vindta:478:3) = 'LUN'
036500130121     C                   EVAL      VABGC1 = '1'
036600130121     C                   ENDIF
036700130121     C                   IF        %subst(vindta:478:3) = 'MAR'
036800130121     C                   EVAL      VABGC1 = '2'
036900130121     C                   ENDIF
037000130121     C                   IF        %subst(vindta:478:3) = 'MER'
037100130121     C                   EVAL      VABGC1 = '3'
037200130121     C                   ENDIF
037300130121     C                   IF        %subst(vindta:478:3) = 'GIO'
037400130121     C                   EVAL      VABGC1 = '4'
037500130121     C                   ENDIF
037600130121     C                   IF        %subst(vindta:478:3) = 'VEN'
037700130121     C                   EVAL      VABGC1 = '5'
037800130121     C                   ENDIF
037900130121     C                   IF        %subst(vindta:478:3) = 'SAB'
038000130121     C                   EVAL      VABGC1 = '6'
038100130121     C                   ENDIF
038200130121     C                   IF        %subst(vindta:478:3) = 'DOM'
038300130121     C                   EVAL      VABGC1 = '7'
038400130121     C                   ENDIF
038500130121     C* GC1
038600130121     C                   IF        %subst(vindta:481:1) = '1'
038700130121     C                   EVAL      %subst(VABGC1:2:1) = 'M'
038800130121     C                   ENDIF
038900130121     C                   IF        %subst(vindta:481:1) = '2'
039000130121     C                   EVAL      %subst(VABGC1:2:1) = 'P'
039100130121     C                   ENDIF
039200130121     C* CBO
039300130121     C                   IF        %subst(vindta:2:1) = '1'
039400130121     C                   EVAL      VABCBO = '1'
039500130121     C                   ENDIF
039600130121     C                   IF        %subst(vindta:2:1) = '6'
039700130121     C                   EVAL      VABCBO = '2'
039800130121     C                   ENDIF
039900130121     C*
040000130121     C* Reperimento campi NUMERICI
040100130121     C                   MOVEL     DATCOR        VABAAS
040200130121     C                   MOVE      DATCOR        VABMGS
040300130121     C* NSP
040400130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
040500130121     C                   EXSR      CHKNUM
040600130121     C                   IF        PiInt=*on
040700130121     C                   Z-ADD     PiVal         VABNSP
040800130121     C                   ELSE
040900130121     C                   SETON                                        31
041000130121     C                   Z-ADD     *zeros        VABNSP
041100130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041200130121     C                             + ' ' + 'VABNSP'
041300130121     C                   ENDIF
041400130121     C* RMN
041500170220     C***                EVAL      PiStr=%trim(%subst(vindta:41:17))
041600170220     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
041700130121     C                   EXSR      CHKNUM
041800130121     C                   IF        PiInt=*on
041900130121     C                   Z-ADD     PiVal         VABRMN
042000130121     C                   ELSE
042100161222     C                   SETON                                        32
042200130121     C                   Z-ADD     1             VABRMN
042300130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400130121     C                             + ' ' + 'VABRMN'
042500130121     C                   ENDIF
042600130121     C* CAD
042700130121     C***                EVAL      PiStr=%trim(%subst(vindta:319:8))
042800130121     C***                EXSR      CHKNUM
042900130121     C***                IF        PiInt=*on
043000130121     C***                Z-ADD     PiVal         Num5_0
043100130121     C***                MOVEL(p)  Num5_0        VABCAD
043200130121     C***                ELSE
043300130121     C***                SETON                                        32
043400130121     C***                EVAL      VABCAD = *zeros
043500130121     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
043600130121     C***                          + ' ' + 'VABCAD'
043700130121     C***                ENDIF
043800130121     C* Reperisco la provincia dal CAP e dalla località
043900130121     C                   IF        VABCAD <> *blanks AND
044000130121     C                             VABNZD  = *blanks AND
044100130121     C                             VABPRD  = *blanks
044200130121     C                   CLEAR                   TISI95DS
044300130121     C                   EVAL      I95TCN = '3'
044400130121     C                   Z-ADD     datcor        I95DAT
044500130121     C                   EVAL      I95CAP = VABCAD
044600130121     C                   EVAL      I95LOC = VABLOD
044700130121     C                   EVAL      I95NAR = VABNZD
044800130121     C                   CALL      'TISI95R'
044900130121     C                   PARM                    TISI95DS
045000130121     C                   EVAL      VABPRD = O95PRV
045100130121     C                   ENDIF
045200130121     C* DCR/TC1
045300130121     C***                IF        %subst(vindta:490:1) = '1'
045400130121     C* DCR
045500130121     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
045600130121     C                   EXSR      CHKNUM
045700130121     C                   IF        PiInt=*on
045800130121     C                   Z-ADD     PiVal         VABDCR
045900130121     C                   ELSE
046000130121     C                   SETON                                        32
046100130121     C                   Z-ADD     *zeros        VABDCR
046200130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300130121     C                             + ' ' + 'VABDCR'
046400130121     C                   ENDIF
046500130121     C***                ELSE
046600130121     C* TC1
046700130121     C                   IF        %subst(vindta:490:1) = '2'
046800130121     C                   EVAL      VABTC1 = 'A'
046900130121     C                   ENDIF
047000130121     C***                ENDIF
047100130121     C* NCL
047200161219     C                   IF        %subst(vindta:352:5)<>'00000'
047300161219     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
047400130121     C                   EXSR      CHKNUM
047500130121     C                   IF        PiInt=*on
047600130121     C                   Z-ADD     PiVal         VABNCL
047700130121     C                   ELSE
047800130121     C                   SETON                                        32
047900130121     C                   Z-ADD     *zeros        VABNCL
048000130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100130121     C                             + ' ' + 'VABNCL'
048200130121     C                   ENDIF
048300130121     C                   ELSE
048400130121     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
048500130121     C                   EXSR      CHKNUM
048600130121     C                   IF        PiInt=*on
048700130121     C                   Z-ADD     PiVal         VABNCL
048800130121     C                   ELSE
048900130121     C                   SETON                                        32
049000130121     C                   Z-ADD     *zeros        VABNCL
049100130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200130121     C                             + ' ' + 'VABNCL'
049300130121     C                   ENDIF
049400130121     C                   ENDIF
049500130121     C* PKB
049600130121     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
049700130121     C                   EXSR      CHKNUM
049800130121     C                   IF        PiNum=*on
049900161219     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
050000130121     C                   Z-ADD(H)  PiVal         VABPKB
050100130121     C                   ELSE
050200130121     C                   SETON                                        32
050300130121     C                   Z-ADD     *zeros        VABPKB
050400130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050500130121     C                             + ' ' + 'VABPKB'
050600130121     C                   ENDIF
050700161219     C* VLB
050800170801     C***                EVAL      PiStr=%trim(%subst(vindta:369:5))
050900170801     C***                EXSR      CHKNUM
051000170801     C***                IF        PiNum=*on
051100170801     C***                EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
051200170801     C***                Z-ADD(H)  PiVal         VABVLB
051300170801     C***                ELSE
051400170801     C***                SETON                                        32
051500170801     C***                Z-ADD     *zeros        VABVLB
051600170801     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
051700170801     C***                          + ' ' + 'VABVLB'
051800170801     C***                ENDIF
051900130121     C* CAS
052000161219     C                   IF        %subst(vindta:388:13)<>'0000000000000' and
052100161219     C                             %subst(vindta:388:13)<>*blank          and
052200161219     C                             %subst(vindta:388:13)<>'0000000000,00'
052300130121     C                   EVAL      FlgCAS = '1'
052400130121     C                   EVAL      VABVCA = 'EUR'
052500180124     C                   SELECT
052600180125     C                   WHEN      %subst(vindta:649:1)='1'
052700180125     C                   EVAL      VABTIC = 'OM'
052800180124     C                   WHEN      VABCCM = 2750637 AND
052900171005     C                             %scan('ESSSE':VABRMO) > *zeros
053000171005     C                   EVAL      VABTIC = 'TO'
053100180124     C                   ENDSL
053200161219     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
053300130121     C                   EXSR      CHKNUM
053400130121     C                   IF        PiNum=*on
053500161219     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
053600130121     C                   Z-ADD(H)  PiVal         VABCAS
053700130121     C                   ELSE
053800130121     C                   SETON                                        32
053900130121     C                   Z-ADD     *zeros        VABCAS
054000130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054100130121     C                             + ' ' + 'VABCAS'
054200130121     C                   ENDIF
054300130121     C                   ENDIF
054400130121     C* IAS
054500130121     C                   IF        %subst(vindta:558:13)<>'0000000000000'
054600130121     C                   EVAL      VABVAS = 'EUR'
054700130121     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
054800130121     C                   EXSR      CHKNUM
054900130121     C                   IF        PiNum=*on
055000130121     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
055100130121     C                   Z-ADD(H)  PiVal         VABIAS
055200130121     C                   ELSE
055300130121     C                   SETON                                        32
055400130121     C                   Z-ADD     *zeros        VABIAS
055500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600130121     C                             + ' ' + 'VABIAS'
055700130121     C                   ENDIF
055800130121     C                   ENDIF
055900070730     C*
056000070730     C* Considerazioni finali su CBO/CAS
056100070730     C                   IF        FlgCAS = '1'
056200070730     C                   IF        VABCBO = '1'
056300070730     C                   EVAL      VABCBO = '4'
056400070730     C                   ENDIF
056500070730     C                   IF        VABCBO = '2'
056600070730     C                   EVAL      VABCBO = '6'
056700070730     C                   ENDIF
056800070730     C                   ENDIF
056900020305     C*
057000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057100011113     C                   EXSR      CHKIMPDIV
057200010202     C*
057300000801     C* Ebbene...
057400000801     C                   ADD       1             §CTRMO
057500070530     C                   IF        *in31 <> *zeros OR
057600070530     C                             *in32 <> *zeros
057700000801     C                   ADD       1             §CTRNO
057800020725     C                   EVAL      x_vinflg = '2'
057900000801     C                   ELSE
058000010201     C                   ADD       1             §CTROKVB
058100000801     C                   ENDIF
058200000801     C*
058300000801     C                   ENDSR
058400010201     C*----------------------------------------------------*
058500170220     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I/J"
058600010201     C*----------------------------------------------------*
058700130121     C     WRIVAT_IJ     BEGSR
058800090204     C*
058900161219     C                   eval      vatAAS = vabAAS
059000161219     C                   eval      vatLNP = vabLNP
059100161219     C                   eval      vatNRS = vabNRS
059200161219     C                   eval      vatNSP = vabNSP
059300010201     C*
059400101007     C* Valorizzo il buffer di scrittura del FIVAT
059500170220     C***                eval      w70 = %trim(%subst(vindta:574:70))
059600170220     C                   eval      w70 = %trim(%subst(vindta:753:70))
059700130121     C* - TIPO RECORD "I"
059800161219     C                   if        %subst(w70:1:35) <> *blanks
059900161219     C                   eval      VATTRC = 'I'
060000161219     C                   eval      VATNOT = %trim(%subst(w70:1:35))
060100161219     C                   write     FIVAT000
060200161219     C                   endif
060300130121     C* - TIPO RECORD "J"
060400161219     C                   if        %subst(w70:36:35) <> *blanks
060500161219     C                   eval      VATTRC = 'J'
060600161219     C                   eval      VATNOT = %trim(%subst(w70:36:35))
060700161219     C                   write     FIVAT000
060800161219     C                   endif
060900010201     C*
061000010201     C                   ENDSR
061100170220     C*----------------------------------------------------*
061200170220     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
061300170220     C*----------------------------------------------------*
061400170220     C     WRIVAT_S      BEGSR
061500170220     C*
061600170220     C                   eval      vatAAS = vabAAS
061700170220     C                   eval      vatLNP = vabLNP
061800170220     C                   eval      vatNRS = vabNRS
061900170220     C                   eval      vatNSP = vabNSP
062000170220     C*
062100170220     C* Valorizzo il buffer di scrittura del FIVAT
062200170220     C* - TIPO RECORD "S"
062300170220     C                   if        %subst(vindta:823:16) <> *blanks
062400170220     C                   eval      VATTRC = 'S'
062500170220     C                   eval      VATNOT = %trim(%subst(vindta:823:16))
062600170220     C                   write     FIVAT000
062700170220     C                   endif
062800170220     C*
062900170220     C                   ENDSR
063000071003     C*----------------------------------------------------*
063100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063200071003     C*----------------------------------------------------*
063300071003     C     WRIVAT_E      BEGSR
063400090204     C*
063500161219     C                   eval      vatAAS = vabAAS
063600161219     C                   eval      vatLNP = vabLNP
063700161219     C                   eval      vatNRS = vabNRS
063800161219     C                   eval      vatNSP = vabNSP
063900071003     C*
064000101007     C* Valorizzo il buffer di scrittura del FIVAT
064100071003     C* - TIPO RECORD "E"
064200161219     C***                if        %subst(vindta:331:26) <> *blanks
064300161219     C***                eval      VATTRC = 'E'
064400130121     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
064500161219     C***                write     FIVAT000
064600161219     C***                endif
064700071003     C*
064800071003     C                   ENDSR
064900020904
065000020904
065100010202     C*----------------------------------------------------*
065200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065300010202     C*----------------------------------------------------*
065400020305     C     PREVAT        BEGSR
065500010202     C*
065600021113     C* Compongo il nome del membro da dare al FIVATWWR
065700010202     C                   eval      parmbr = vlrhdl
065800010202     C                   movel     'M'           parmbr
065900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066000010202     C                   eval      paropz = '1'
066100010202     C* Effettuo la chiamata al CLLE preposto
066200050128     C                   call(e)   'TITVVTC'
066300010202     C                   parm                    parccm
066400010202     C                   parm                    parmbr
066500010202     C                   parm                    paropz
066600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066700010202     C                   if        %error
066800010202     C                   movel     '1'           chkcall
066900010202     C                   else
067000010202     C                   movel     '0'           chkcall
067100010202     C                   endif
067200010202     C*
067300010202     C                   ENDSR
067400000801     C*----------------------------------------------------*
067500000801     C*  CONTROLLO NUMERICITA' CAMPI
067600000801     C*----------------------------------------------------*
067700000801     C     CHKNUM        BEGSR
067800081113     C*
067900081113     C                   IF        PiDecChr = *blanks
068000121214     C                   EVAL      PiDecChr = ','
068100081113     C                   ENDIF
068200081113     C*
068300081113     C                   callp(e)  UBISNUM_Check(PiStr
068400081113     C                                          :PiDecChr
068500081113     C                                          :PiVal
068600081113     C                                          :PiNum
068700081113     C                                          :PiInt)
068800081113     C*
068900000801     C                   IF        %error
069000000801     C                   EVAL      PiInt=*off
069100000801     C                   ENDIF
069200000801     C*
069300000801     C                   ENDSR
069400000801     C***
069500000801
069600011113
069700011113     C*----------------------------------------------------*
069800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069900011113     C*----------------------------------------------------*
070000011113     C     CHKIMPDIV     BEGSR
070100011113     C*
070200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070300011113     C                   Z-ADD     *zeros        wrkDec            9 9
070400011113     C*
070500011113     C* Come prima cosa effettuo considerazioni sulla divisa
070600011113     C                   IF        vabIAS > *zeros
070700011113     C                   IF        vabVAS <> 'EUR'
070800011113     C                   EVAL      vabVAS =  'ITL'
070900011113     C                   ENDIF
071000011113     C                   ENDIF
071100011113     C*
071200011113     C                   IF        vabCAS > *zeros
071300011113     C                   IF        vabVCA <> 'EUR'
071400011113     C                   EVAL      vabVCA =  'ITL'
071500011113     C                   ENDIF
071600011113     C                   ENDIF
071700011113     C*
071800011113     C                   IF        vabVMD > *zeros
071900020305     C                   IF        vabVAD <> 'EUR'
072000011113     C                   EVAL      vabVAD =  'ITL'
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072500011113     C                   Z-ADD     vabIAS        wrkDec
072600011113     C                   IF        wrkDec > *zeros
072700011113     C                   IF        vabVAS = 'ITL'
072800011113     C                   EVAL      vabIAS = *zeros
072900011113     C                   ENDIF
073000011113     C                   ENDIF
073100011113     C*
073200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073300011113     C                   Z-ADD     vabCAS        wrkDec
073400011113     C                   IF        wrkDec > *zeros
073500011113     C                   IF        vabVCA = 'ITL'
073600011113     C                   EVAL      vabCAS = *zeros
073700011113     C                   ENDIF
073800011113     C                   ENDIF
073900011113     C*
074000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074100011113     C                   Z-ADD     vabVMD        wrkDec
074200011113     C                   IF        wrkDec > *zeros
074300011113     C                   IF        vabVAD = 'ITL'
074400011113     C                   EVAL      vabVMD = *zeros
074500011113     C                   ENDIF
074600011113     C                   ENDIF
074700011113     C*
074800011113     C                   ENDSR
074900011113     C***
075000011113
075100011113
075200000801
075300000801
075400990920      /TITLE Invio dei dati al punto operativo.
075500010202     C     invio         BEGSR
075600990920     C*
075700021113     C* 1° invio FIVAT
075800010201     C                   reset                   dscmz
075900010201     C                   move      vlrpoi        cmzdst
076000021113     C                   eval      cmzfld = 'FIVATWWR'
076100010201     C                   eval      cmzmbd = vlrhdl
076200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076300021009     C***                if        prmfir = *blanks
076400021113     C                   eval      cmzfla = 'FIVAT00F'
076500021113     C                   eval      cmzmba = 'FIVAT00F'
076600021009     C***                else
076700021009     C***                eval      cmzfla = prmfir
076800021009     C***                eval      cmzmba = prmfir
076900021009     C***                endif
077000010201     C                   eval      cmznrr = *zeros
077100020305     C                   move      §ctrokvt      cmznrr
077200021018     C                   eval      cmzlba = vlrfl1
077300010201     C                   call(e)   'TIS711C'
077400010201     C                   parm                    dscmz
077500010201     C                   parm      *blanks       esito
077600010205     C                   if        %error
077700010205     C                             or cmzerr = '1'
077800010205     C                             or esito  = '1'
077900010205     C                   eval      wrkesito = '3'
078000010205     C                   else
078100010201     C*
078200021113     C* 2° invio FIVAB
078300010201     C                   reset                   dscmz
078400010201     C                   move      vlrpoi        cmzdst
078500010201     C                   eval      cmzfld = vlrfou
078600010201     C                   eval      cmzmbd = vlrhdl
078700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078800021009     C***                if        prmfir = *blanks
078900021113     C                   eval      cmzfla = 'FIVAB00F'
079000021113     C                   eval      cmzmba = 'FIVAB00F'
079100021009     C***                else
079200021009     C***                eval      cmzfla = prmfir
079300021009     C***                eval      cmzmba = prmfir
079400021009     C***                endif
079500010201     C                   eval      cmznrr = *zeros
079600010201     C                   move      §ctrokvb      cmznrr
079700021018     C                   eval      cmzlba = vlrfl1
079800010201     C                   call(e)   'TIS711C'
079900010201     C                   parm                    dscmz
080000010201     C                   parm      *blanks       esito
080100010201     C                   if        %error
080200010201     C                             or cmzerr = '1'
080300010201     C                             or esito  = '1'
080400010201     C                   eval      wrkesito = '3'
080500010201     C                   endif
080600010205     C                   endif
080700990920     C*
080800000613     C                   ENDSR
080900000613     C***
081000070411
081100070411     C     *pssr         BEGSR
081200070411     C*
081300070411     C                   if        %open(tivin00r)
081400070411     C                   close     tivin00r
081500070411     C                   endif
081600070411     C                   if        %open(fivabwwr)
081700070411     C                   close     fivabwwr
081800070411     C                   endif
081900070411     C                   if        %open(fivatwwr)
082000070411     C                   close     fivatwwr
082100070411     C                   endif
082200070411     C*
082300070411     C* Effettuo la chiamata al CLLE preposto
082400070411     C                   call(e)   'TITVVTC'
082500070411     C                   parm                    parccm
082600070411     C                   parm                    parmbr
082700070411     C                   parm      '2'           paropz
082800070411     C*
082900070411     C                   eval      wrkesito = '2'
083000070411     C*
083100070411     C                   seton                                        LR
083200070411     C*
083300070411     C                   ENDSR     '*CANCL'
083400070411     C***
083500070411
083600990910
083700000613     C     *inzsr        BEGSR
083800990910     C*
083900990910     C     *entry        plist
084000990920     C                   parm                    tivlrds
084100990921     C                   parm      wrkesito      esito
084200000724     C                   parm                    prmlit
084300000710     C                   parm                    prmfir
084400000613     C*
084500000830     C* CALCOLA LA DATA CORRENTE
084600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084700100324     C                   eval      datcor = %dec(%date() : *ISO)
084800000830     C*
084900000613     C                   ENDSR
085000000613     C***
