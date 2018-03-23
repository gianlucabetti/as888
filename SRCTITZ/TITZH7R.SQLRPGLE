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
002400130121     D depspe          s              7    INZ(*blanks)
002500130121     D curspe          s              7    INZ(*blanks)
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
004000130121     D  VINSPE                        7
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
005900130121     D w70             s             70
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
007700050128     C                   call(e)   'TITVVTC'
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
012200130121     C+ vindta, vinflg, substr(vindta, 83, 7) as sped, rrn(tivin00r)
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
014900130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
015000020305     C                   MOVEL(p)  PiStr         curspe
015100010305     C*
015200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400071003     C                   exsr      impvab
015500071003     C                   exsr      wrivab
015600160122     C***                exsr      wrivat_ij                                    => carico VAT
015700071003     C                   else
015800080923     C*
015900071009     C                   if        wDISK = 'D'
016000071009     C                   exsr      impvab
016100071009     C                   exsr      wrivab
016200160122     C***                exsr      wrivat_ij                                    => carico VAT
016300130121     C***                exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900020305     C                   exsr      impvab
017000160122     C***                exsr      wrivat_ij                                    => carico VAT
017100130121     C***50              exsr      wrivat_e                                     => carico VAT
017200010305     C                   else
017300020725     C                   if        depspe <> curspe                             => rottura di spediz
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500070928     C                   exsr      wrivab
017600080117     C                   clear                   tivinds
017700020305     C                   exsr      impvab
017800160122     C***                exsr      wrivat_ij                                    => carico VAT
017900130121     C***50              exsr      wrivat_e                                     => carico VAT
018000020305     C                   else                                                   => x stessa spediz
018100090204     C***                exsr      impvab
018200160122     C***                exsr      wrivat_ij                                    => carico VAT
018300130121     C***50              exsr      wrivat_e                                     => carico VAT
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
023300021113     C                   if        %open(fivabwwr)
023400021113     C                   close     fivabwwr
023500990914     C                   endif
023600021113     C                   if        %open(fivatwwr)
023700021113     C                   close     fivatwwr
023800010201     C                   endif
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
025800021113     C                   write     fivab000                                     => scarico il VAB
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
027900021113     C                   CLEAR                   FIVAB000
028000021113     C                   CLEAR                   FIVAT000
028100070730     C* Imposto i valori di default...
028200160122     C                   EVAL      VABCCM = 2022737
028300160122     C                   EVAL      VATCCM = 2022737
028400160122     C                   EVAL      VABLNP = 202
028500160122     C                   EVAL      VATLNP = 202
028600160122     C                   EVAL      VABCTR = 300
028700130121     C                   EVAL      VABCTM = '7Q'
028800130121     C                   EVAL      VABCBO = '1'
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
032800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
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
034300130121     C*
034400130121     C* Reperimento campi ALFA
034500130121     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
034600130121     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
034700130121     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
034800130121     C                   EVAL      VABCAD=%trim(%subst(vindta:319:8))
034900130121     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
035000160511     C***                EVAL      VABRMA=%trim(%subst(vindta:51:20))
035100170522     C***                EVAL      VABRMA=%trim(%subst(vindta:42:9))
035200170522     C                   EVAL      VABRMA=%trim(%subst(vindta:60:11))
035300160122     C***                EVAL      VABNOT=%trim(%subst(vindta:491:35))
035400160222     C                   EVAL      VABRMO=%trim(%subst(vindta:114:30))
035500130121     C*
035600130121     C* Considerazioni sulla ragione sociale del destinatario da indicare
035700130121     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035800130121     C     '@':'A'       XLATE     VABRSD        VABRSD
035900130121     C* ==
036000130121     C*
036100130121     C* Reperimento campi NUMERICI
036200130121     C                   MOVEL     DATCOR        VABAAS
036300130121     C                   MOVE      DATCOR        VABMGS
036400130121     C* NSP
036500130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
036600130121     C                   EXSR      CHKNUM
036700130121     C                   IF        PiInt=*on
036800130121     C                   Z-ADD     PiVal         VABNSP
036900130121     C                   ELSE
037000130121     C                   SETON                                        31
037100130121     C                   Z-ADD     *zeros        VABNSP
037200130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037300130121     C                             + ' ' + 'VABNSP'
037400130121     C                   ENDIF
037500130121     C* RMN
037600160511     C***                EVAL      PiStr=%trim(%subst(vindta:21:8))
037700170522     C***                EVAL      PiStr=%trim(%subst(vindta:64:7))
037800170522     C                   EVAL      PiStr=%trim(%subst(vindta:42:9))
037900130121     C                   EXSR      CHKNUM
038000130121     C                   IF        PiInt=*on
038100130121     C                   Z-ADD     PiVal         VABRMN
038200130121     C                   ELSE
038300130121     C                   SETON                                        31
038400130121     C                   Z-ADD     1             VABRMN
038500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038600130121     C                             + ' ' + 'VABRMN'
038700130121     C                   ENDIF
038800130121     C* Reperisco la provincia dal CAP e dalla località
038900130121     C                   IF        VABCAD <> *blanks AND
039000130121     C                             VABNZD  = *blanks AND
039100130121     C                             VABPRD  = *blanks
039200130121     C                   CLEAR                   TISI95DS
039300130121     C                   EVAL      I95TCN = '3'
039400130121     C                   Z-ADD     datcor        I95DAT
039500130121     C                   EVAL      I95CAP = VABCAD
039600130121     C                   EVAL      I95LOC = VABLOD
039700130121     C                   EVAL      I95NAR = VABNZD
039800130121     C                   CALL      'TISI95R'
039900130121     C                   PARM                    TISI95DS
040000130121     C                   EVAL      VABPRD = O95PRV
040100130121     C                   ENDIF
040200130121     C* NCL
040300160122     C                   IF        %subst(vindta:364:5)<>'00000' and
040400160122     C                             %subst(vindta:364:5)<>*blank
040500160122     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
040600130121     C                   EXSR      CHKNUM
040700130121     C                   IF        PiInt=*on
040800130121     C                   Z-ADD     PiVal         VABNCL
040900130121     C                   ELSE
041000130121     C                   SETON                                        32
041100130121     C                   Z-ADD     *zeros        VABNCL
041200130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300130121     C                             + ' ' + 'VABNCL'
041400130121     C                   ENDIF
041500130121     C                   ENDIF
041600160322     C* DCR
041700160322     C                   IF        %subst(vindta:482:8)<>'00000000' and
041800160322     C                             %subst(vindta:482:8)<>*blanks
041900160322     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
042000160322     C                   EXSR      CHKNUM
042100160322     C                   IF        PiInt=*on
042200160322     C                   Z-ADD     PiVal         VABDCR
042300160322     C                   ELSE
042400160322     C                   SETON                                        32
042500160322     C                   Z-ADD     *zeros        VABDCR
042600160322     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700160322     C                             + ' ' + 'VABDCR'
042800160322     C                   ENDIF
042900160322     C                   ENDIF
043000130121     C* PKB
043100130121     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
043200130121     C                   EXSR      CHKNUM
043300130121     C                   IF        PiNum=*on
043400160122     C                   EVAL(H)   VABPKB = PiVal/100
043500130121     C                   ELSE
043600130121     C                   SETON                                        32
043700130121     C                   Z-ADD     *zeros        VABPKB
043800130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043900130121     C                             + ' ' + 'VABPKB'
044000130121     C                   ENDIF
044100160525     C* VLB
044200160525     C                   EVAL      PiStr=%trim(%subst(vindta:369:5))
044300160525     C                   EXSR      CHKNUM
044400160525     C                   IF        PiNum=*on
044500160525     C                   EVAL(H)   VABVLB = PiVal/100
044600160525     C                   ELSE
044700160525     C                   SETON                                        32
044800160525     C                   Z-ADD     *zeros        VABVLB
044900160525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045000160525     C                             + ' ' + 'VABVLB'
045100160525     C                   ENDIF
045200130121     C* CAS
045300160122     C                   IF        %subst(vindta:388:13)<>*zeros and
045400160122     C                             %subst(vindta:388:13)<>*blank
045500130121     C                   EVAL      FlgCAS = '1'
045600130121     C                   EVAL      VABVCA = 'EUR'
045700160222     C                   EVAL      VABTIC = 'OM'
045800160317     C* solo per questo cod. cliente, il tipo incasso è TO se c'è contrassegno
045900160317     C                   IF        VABCCM = 962301
046000160317     C                   EVAL      VABTIC = 'TO'
046100160317     C                   ENDIF
046200160122     C                   EVAL      PiStr=%trim(%subst(vindta:388:13))
046300130121     C                   EXSR      CHKNUM
046400130121     C                   IF        PiNum=*on
046500160509     C                   EVAL(H)   VABCAS = PiVal/1000
046600130121     C                   ELSE
046700130121     C                   SETON                                        32
046800130121     C                   Z-ADD     *zeros        VABCAS
046900130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000130121     C                             + ' ' + 'VABCAS'
047100130121     C                   ENDIF
047200130121     C                   ENDIF
047300070730     C*
047400070730     C* Considerazioni finali su CBO/CAS
047500070730     C                   IF        FlgCAS = '1'
047600070730     C                   IF        VABCBO = '1'
047700070730     C                   EVAL      VABCBO = '4'
047800070730     C                   ENDIF
047900070730     C                   IF        VABCBO = '2'
048000070730     C                   EVAL      VABCBO = '6'
048100070730     C                   ENDIF
048200070730     C                   ENDIF
048300020305     C*
048400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048500011113     C                   EXSR      CHKIMPDIV
048600010202     C*
048700000801     C* Ebbene...
048800000801     C                   ADD       1             §CTRMO
048900070530     C                   IF        *in31 <> *zeros OR
049000070530     C                             *in32 <> *zeros
049100000801     C                   ADD       1             §CTRNO
049200020725     C                   EVAL      x_vinflg = '2'
049300000801     C                   ELSE
049400010201     C                   ADD       1             §CTROKVB
049500000801     C                   ENDIF
049600000801     C*
049700000801     C                   ENDSR
049800010201     C*----------------------------------------------------*
049900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
050000010201     C*----------------------------------------------------*
050100130121     C     WRIVAT_IJ     BEGSR
050200090204     C*
050300090204     C                   eval      vatAAS = vabAAS
050400090204     C                   eval      vatLNP = vabLNP
050500090204     C                   eval      vatNRS = vabNRS
050600090204     C                   eval      vatNSP = vabNSP
050700010201     C*
050800101007     C* Valorizzo il buffer di scrittura del FIVAT
050900130121     C                   eval      w70 = %trim(%subst(vindta:574:70))
051000130121     C* - TIPO RECORD "I"
051100130121     C                   if        %subst(w70:1:35) <> *blanks
051200130121     C                   eval      VATTRC = 'I'
051300130121     C                   eval      VATNOT = %trim(%subst(w70:1:35))
051400021113     C                   write     FIVAT000
051500020725     C                   endif
051600130121     C* - TIPO RECORD "J"
051700130121     C                   if        %subst(w70:36:35) <> *blanks
051800130121     C                   eval      VATTRC = 'J'
051900130121     C                   eval      VATNOT = %trim(%subst(w70:36:35))
052000130121     C                   write     FIVAT000
052100130121     C                   endif
052200010201     C*
052300010201     C                   ENDSR
052400071003     C*----------------------------------------------------*
052500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
052600071003     C*----------------------------------------------------*
052700071003     C     WRIVAT_E      BEGSR
052800090204     C*
052900090204     C                   eval      vatAAS = vabAAS
053000090204     C                   eval      vatLNP = vabLNP
053100090204     C                   eval      vatNRS = vabNRS
053200090204     C                   eval      vatNSP = vabNSP
053300071003     C*
053400101007     C* Valorizzo il buffer di scrittura del FIVAT
053500071003     C* - TIPO RECORD "E"
053600071003     C                   if        %subst(vindta:331:26) <> *blanks
053700071003     C                   eval      VATTRC = 'E'
053800130121     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
053900071003     C                   write     FIVAT000
054000071003     C                   endif
054100071003     C*
054200071003     C                   ENDSR
054300020904
054400020904
054500010202     C*----------------------------------------------------*
054600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054700010202     C*----------------------------------------------------*
054800020305     C     PREVAT        BEGSR
054900010202     C*
055000021113     C* Compongo il nome del membro da dare al FIVATWWR
055100010202     C                   eval      parmbr = vlrhdl
055200010202     C                   movel     'M'           parmbr
055300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
055400010202     C                   eval      paropz = '1'
055500010202     C* Effettuo la chiamata al CLLE preposto
055600050128     C                   call(e)   'TITVVTC'
055700010202     C                   parm                    parccm
055800010202     C                   parm                    parmbr
055900010202     C                   parm                    paropz
056000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056100010202     C                   if        %error
056200010202     C                   movel     '1'           chkcall
056300010202     C                   else
056400010202     C                   movel     '0'           chkcall
056500010202     C                   endif
056600010202     C*
056700010202     C                   ENDSR
056800000801     C*----------------------------------------------------*
056900000801     C*  CONTROLLO NUMERICITA' CAMPI
057000000801     C*----------------------------------------------------*
057100000801     C     CHKNUM        BEGSR
057200081113     C*
057300081113     C                   IF        PiDecChr = *blanks
057400121214     C                   EVAL      PiDecChr = ','
057500081113     C                   ENDIF
057600081113     C*
057700081113     C                   callp(e)  UBISNUM_Check(PiStr
057800081113     C                                          :PiDecChr
057900081113     C                                          :PiVal
058000081113     C                                          :PiNum
058100081113     C                                          :PiInt)
058200081113     C*
058300000801     C                   IF        %error
058400000801     C                   EVAL      PiInt=*off
058500000801     C                   ENDIF
058600000801     C*
058700000801     C                   ENDSR
058800000801     C***
058900000801
059000011113
059100011113     C*----------------------------------------------------*
059200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059300011113     C*----------------------------------------------------*
059400011113     C     CHKIMPDIV     BEGSR
059500011113     C*
059600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059700011113     C                   Z-ADD     *zeros        wrkDec            9 9
059800011113     C*
059900011113     C* Come prima cosa effettuo considerazioni sulla divisa
060000011113     C                   IF        vabIAS > *zeros
060100011113     C                   IF        vabVAS <> 'EUR'
060200011113     C                   EVAL      vabVAS =  'ITL'
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C                   IF        vabCAS > *zeros
060700011113     C                   IF        vabVCA <> 'EUR'
060800011113     C                   EVAL      vabVCA =  'ITL'
060900011113     C                   ENDIF
061000011113     C                   ENDIF
061100011113     C*
061200011113     C                   IF        vabVMD > *zeros
061300020305     C                   IF        vabVAD <> 'EUR'
061400011113     C                   EVAL      vabVAD =  'ITL'
061500011113     C                   ENDIF
061600011113     C                   ENDIF
061700011113     C*
061800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061900011113     C                   Z-ADD     vabIAS        wrkDec
062000011113     C                   IF        wrkDec > *zeros
062100011113     C                   IF        vabVAS = 'ITL'
062200011113     C                   EVAL      vabIAS = *zeros
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062700011113     C                   Z-ADD     vabCAS        wrkDec
062800011113     C                   IF        wrkDec > *zeros
062900011113     C                   IF        vabVCA = 'ITL'
063000011113     C                   EVAL      vabCAS = *zeros
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063500011113     C                   Z-ADD     vabVMD        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVAD = 'ITL'
063800011113     C                   EVAL      vabVMD = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C                   ENDSR
064300011113     C***
064400011113
064500011113
064600000801
064700000801
064800990920      /TITLE Invio dei dati al punto operativo.
064900010202     C     invio         BEGSR
065000990920     C*
065100021113     C* 1° invio FIVAT
065200010201     C                   reset                   dscmz
065300010201     C                   move      vlrpoi        cmzdst
065400021113     C                   eval      cmzfld = 'FIVATWWR'
065500010201     C                   eval      cmzmbd = vlrhdl
065600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065700021009     C***                if        prmfir = *blanks
065800021113     C                   eval      cmzfla = 'FIVAT00F'
065900021113     C                   eval      cmzmba = 'FIVAT00F'
066000021009     C***                else
066100021009     C***                eval      cmzfla = prmfir
066200021009     C***                eval      cmzmba = prmfir
066300021009     C***                endif
066400010201     C                   eval      cmznrr = *zeros
066500020305     C                   move      §ctrokvt      cmznrr
066600021018     C                   eval      cmzlba = vlrfl1
066700010201     C                   call(e)   'TIS711C'
066800010201     C                   parm                    dscmz
066900010201     C                   parm      *blanks       esito
067000010205     C                   if        %error
067100010205     C                             or cmzerr = '1'
067200010205     C                             or esito  = '1'
067300010205     C                   eval      wrkesito = '3'
067400010205     C                   else
067500010201     C*
067600021113     C* 2° invio FIVAB
067700010201     C                   reset                   dscmz
067800010201     C                   move      vlrpoi        cmzdst
067900010201     C                   eval      cmzfld = vlrfou
068000010201     C                   eval      cmzmbd = vlrhdl
068100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068200021009     C***                if        prmfir = *blanks
068300021113     C                   eval      cmzfla = 'FIVAB00F'
068400021113     C                   eval      cmzmba = 'FIVAB00F'
068500021009     C***                else
068600021009     C***                eval      cmzfla = prmfir
068700021009     C***                eval      cmzmba = prmfir
068800021009     C***                endif
068900010201     C                   eval      cmznrr = *zeros
069000010201     C                   move      §ctrokvb      cmznrr
069100021018     C                   eval      cmzlba = vlrfl1
069200010201     C                   call(e)   'TIS711C'
069300010201     C                   parm                    dscmz
069400010201     C                   parm      *blanks       esito
069500010201     C                   if        %error
069600010201     C                             or cmzerr = '1'
069700010201     C                             or esito  = '1'
069800010201     C                   eval      wrkesito = '3'
069900010201     C                   endif
070000010205     C                   endif
070100990920     C*
070200000613     C                   ENDSR
070300000613     C***
070400070411
070500070411     C     *pssr         BEGSR
070600070411     C*
070700070411     C                   if        %open(tivin00r)
070800070411     C                   close     tivin00r
070900070411     C                   endif
071000070411     C                   if        %open(fivabwwr)
071100070411     C                   close     fivabwwr
071200070411     C                   endif
071300070411     C                   if        %open(fivatwwr)
071400070411     C                   close     fivatwwr
071500070411     C                   endif
071600070411     C*
071700070411     C* Effettuo la chiamata al CLLE preposto
071800070411     C                   call(e)   'TITVVTC'
071900070411     C                   parm                    parccm
072000070411     C                   parm                    parmbr
072100070411     C                   parm      '2'           paropz
072200070411     C*
072300070411     C                   eval      wrkesito = '2'
072400070411     C*
072500070411     C                   seton                                        LR
072600070411     C*
072700070411     C                   ENDSR     '*CANCL'
072800070411     C***
072900070411
073000990910
073100000613     C     *inzsr        BEGSR
073200990910     C*
073300990910     C     *entry        plist
073400990920     C                   parm                    tivlrds
073500990921     C                   parm      wrkesito      esito
073600000724     C                   parm                    prmlit
073700000710     C                   parm                    prmfir
073800000613     C*
073900000830     C* CALCOLA LA DATA CORRENTE
074000100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074100100324     C                   eval      datcor = %dec(%date() : *ISO)
074200000830     C*
074300000613     C                   ENDSR
074400000613     C***
