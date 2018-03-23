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
002400171120     D depspe          s              5    INZ(*blanks)
002500171120     D curspe          s              5    INZ(*blanks)
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
004000171120     D  VINSPE                        5
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
012200171120     C+ vindta, vinflg, substr(vindta, 1, 5) as sped, rrn(tivin00r)
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
014900171120     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
015000020305     C                   MOVEL(p)  PiStr         curspe
015100010305     C*
015200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400171120     C                   exsr      repNSP
015500071003     C                   exsr      impvab
015600071003     C                   exsr      wrivab
015700160122     C***                exsr      wrivat_ij                                    => carico VAT
015800071003     C                   else
015900080923     C*
016000071009     C                   if        wDISK = 'D'
016100171120     C                   exsr      repNSP
016200071009     C                   exsr      impvab
016300071009     C                   exsr      wrivab
016400160122     C***                exsr      wrivat_ij                                    => carico VAT
016500130121     C***                exsr      wrivat_e                                     => carico VAT
016600071009     C                   else
016700071009     C*
016800010305     C                   if        depspe = *blanks                             => 1° giro
016900010305     C                   eval      depspe = curspe                              => memorizz. spediz
017000080117     C                   clear                   tivinds
017100171120     C                   exsr      repNSP
017200020305     C                   exsr      impvab
017300160122     C***                exsr      wrivat_ij                                    => carico VAT
017400130121     C***50              exsr      wrivat_e                                     => carico VAT
017500010305     C                   else
017600020725     C                   if        depspe <> curspe                             => rottura di spediz
017700010305     C                   eval      depspe = curspe                              => memorizz. spediz
017800070928     C                   exsr      wrivab
017900080117     C                   clear                   tivinds
018000171120     C                   exsr      repNSP
018100020305     C                   exsr      impvab
018200160122     C***                exsr      wrivat_ij                                    => carico VAT
018300130121     C***50              exsr      wrivat_e                                     => carico VAT
018400020305     C                   else                                                   => x stessa spediz
018500090204     C***                exsr      impvab
018600160122     C***                exsr      wrivat_ij                                    => carico VAT
018700130121     C***50              exsr      wrivat_e                                     => carico VAT
018800010305     C                   endif
018900010305     C                   endif
019000010305     C                   endif
019100071003     C                   endif
019200071009     C                   endif
019300000905     C*
019400000905     C                   else
019500020725     C                   eval      x_vinflg = '1'
019600000905     C                   endif
019700000905     C*
019800020725     C     VINRRN        chain     tivin000
019900020725     C                   if        %found(tivin00r)
020000020725     C                   eval      y_vinflg = x_vinflg
020100020725     C                   eval      y_vinmsg = x_vinmsg
020200020725     C                   update    tivin000
020300020725     C                   endif
020400020725     C*
020500020725     C/EXEC SQL
020600070530     C+ Fetch C1 into :INPUT_DS
020700020725     C/END-EXEC
020800020725     C*
020900020725     C                   enddo
021000020725     C*
021100020725     C/EXEC SQL
021200020725     C+ close C1
021300020725     C/END-EXEC
021400000905     C*
021500020305     C* Scarico i VAB rimasti "in sospeso"
021600071009     C                   if        wDISK = 'C'
021700071009     C                   exsr      wrivab
021800071009     C                   endif
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021113     C                   if        %open(fivabwwr)
023800021113     C                   close     fivabwwr
023900990914     C                   endif
024000021113     C                   if        %open(fivatwwr)
024100021113     C                   close     fivatwwr
024200010201     C                   endif
024300990910     C*
024400010201     C                   if        §ctrokvb > 0
024500000724     C                             and vlrpoi <> *zeros
024600010202     C                   exsr      invio
024700990920     C                   endif
024800990920     C*
024900910830     C                   ENDSR
025000000613     C***
025100010305
025200010305     C*----------------------------------------------------*
025300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025400010305     C*----------------------------------------------------*
025500020305     C     WRIVAB        BEGSR
025600070730     C*
025700071003     C* Considerazioni finali
025800071003     C                   if        VABRMA = *blanks
025900071003     C                   movel(P)  VABRMN        VABRMA
026000071003     C                   endif
026100071003     C*
026200021113     C                   write     fivab000                                     => scarico il VAB
026300010305     C*
026400010305     C                   ENDSR
026500990920
026600000801     C*----------------------------------------------------*
026700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026800000801     C*----------------------------------------------------*
026900010201     C     INZVAR        BEGSR
027000000801     C*
027100010201     C                   Z-ADD     *zeros        Num5_0
027200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027300020725     C                   MOVEL     '0'           FlgCAS            1
027400000801     C*
027500000801     C                   ENDSR
027600000801     C*----------------------------------------------------*
027700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027800000801     C*----------------------------------------------------*
027900020904     C     DEFCAM        BEGSR
028000080923     C*
028100080923     C                   SETOFF                                       5051
028200000801     C*
028300021113     C                   CLEAR                   FIVAB000
028400021113     C                   CLEAR                   FIVAT000
028500070730     C* Imposto i valori di default...
028600171120     C                   EVAL      VABCCM = 0962301
028700171120     C                   EVAL      VATCCM = 0962301
028800171120     C                   EVAL      VABLNP = 096
028900171120     C                   EVAL      VATLNP = 096
029000171120     C                   EVAL      VABCTR = 000
029100130121     C                   EVAL      VABCBO = '1'
029200171120     C                   EVAL      VABTSP = 'C'
029300171121     C                   EVAL      VABCTM = '7'
029400070222     C* ... e poi verifico se sono stati passati come parametri
029500070222     C                   IF        vlrppt > *blanks
029600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029800070222     C                   EXSR      CHKNUM
029900070222     C                   IF        PiInt=*on
030000070222     C                   Z-ADD     PiVal         VABCCM
030100070222     C                   Z-ADD     PiVal         VATCCM
030200070222     C                   ENDIF
030300070222     C                   ENDIF
030400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABLNP
030900070222     C                   Z-ADD     PiVal         VATLNP
031000070222     C                   ENDIF
031100070222     C                   ENDIF
031200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABCTR
031700070222     C                   ENDIF
031800070928     C                   ENDIF
031900071009     C                   MOVEL     *blanks       wDISK             1
032000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032100071009     C                   IF        wDISK <> *blanks
032200070928     C                   SETON                                        50
032300070222     C                   ENDIF
032400080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032500080923     C                   SETON                                        51
032600080923     C                   ENDIF
032700070222     C                   ENDIF
032800071009     C*
032900071009     C   50              EVAL      VABCTM = '7Q'
033000000801     C*
033100000801     C                   ENDSR
033200171120     C*----------------------------------------------------*
033300171120     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033400171120     C*----------------------------------------------------*
033500171120     C     REPNSP        BEGSR
033600171120     C*
033700171120     C                   EXSR      INZVAR
033800171120     C                   EXSR      DEFCAM
033900171120     C*
034000171120     C                   SETON                                        60
034100171120     C*
034200171120     C* NSP => Stacco un numeratore da AZNUM
034300171120     C                   clear                   TRUL33DS
034400171120     C                   eval      I33OPE = *zeros
034500171120     C                   eval      I33CNU = 302
034600171120     C                   eval      I33NUM = 1
034700171120     C                   movel     TRUL33DS      KPJBU
034800171120     C                   call      'TRUL33R'
034900171120     C                   parm                    KPJBA
035000171120     C                   movel     KPJBU         TRUL33DS
035100171120     C                   if        O33ERR = *zeros
035200171120     C                   z-add     O33NRF        VABNSP
035300171120     C                   z-add     O33NRF        VATNSP
035400171120     C                   else
035500171120     C                   SETON                                        31
035600171120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035700171120     C                             + ' ' + 'VABNSP VATNSP'
035800171120     C                   endif
035900171120     C*
036000171120     C                   ENDSR
036100000801     C*----------------------------------------------------*
036200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036300000801     C*----------------------------------------------------*
036400010201     C     IMPVAB        BEGSR
036500070530     C*
036600070530     C                   SETOFF                                       3132
036700070928     C*
036800070928     C                   MOVE(P)   vlrpoi        VABFGS
036900070928     C                   MOVE(P)   vlrpoi        VATFGS
037000070928     C*
037100070928     C                   MOVEL     datcor        VABAAS
037200070928     C                   MOVEL     datcor        VATAAS
037300070928     C                   MOVE      datcor        VABMGS
037400130121     C*
037500130121     C* Reperimento campi ALFA
037600171120     C                   EVAL      VABRSD=%trim(%subst(vindta:242:35))
037700171120     C                   EVAL      VABIND=%trim(%subst(vindta:277:35))
037800171120     C                   EVAL      VABLOD=%trim(%subst(vindta:312:25))
037900171120     C                   EVAL      VABCAD=%trim(%subst(vindta:337:5))
038000171120     C                   EVAL      VABPRD=%trim(%subst(vindta:342:2))
038100171120     C                   EVAL      VABRMA=%trim(%subst(vindta:65:10))
038200171120     C                   EVAL      VABRMO=%trim(%subst(vindta:109:35))
038300130121     C*
038400130121     C* Considerazioni sulla ragione sociale del destinatario da indicare
038500130121     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038600130121     C     '@':'A'       XLATE     VABRSD        VABRSD
038700130121     C* ==
038800130121     C*
038900130121     C* Reperimento campi NUMERICI
039000130121     C                   MOVEL     DATCOR        VABAAS
039100130121     C                   MOVE      DATCOR        VABMGS
039200130121     C* RMN
039300171120     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
039400130121     C                   EXSR      CHKNUM
039500130121     C                   IF        PiInt=*on
039600130121     C                   Z-ADD     PiVal         VABRMN
039700130121     C                   ELSE
039800130121     C                   SETON                                        31
039900130121     C                   Z-ADD     1             VABRMN
040000130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100130121     C                             + ' ' + 'VABRMN'
040200130121     C                   ENDIF
040300130121     C* Reperisco la provincia dal CAP e dalla località
040400130121     C                   IF        VABCAD <> *blanks AND
040500130121     C                             VABNZD  = *blanks AND
040600130121     C                             VABPRD  = *blanks
040700130121     C                   CLEAR                   TISI95DS
040800130121     C                   EVAL      I95TCN = '3'
040900130121     C                   Z-ADD     datcor        I95DAT
041000130121     C                   EVAL      I95CAP = VABCAD
041100130121     C                   EVAL      I95LOC = VABLOD
041200130121     C                   EVAL      I95NAR = VABNZD
041300130121     C                   CALL      'TISI95R'
041400130121     C                   PARM                    TISI95DS
041500130121     C                   EVAL      VABPRD = O95PRV
041600130121     C                   ENDIF
041700130121     C* NCL
041800171120     C                   EVAL      PiStr=%trim(%subst(vindta:368:5))
041900130121     C                   EXSR      CHKNUM
042000130121     C                   IF        PiInt=*on
042100130121     C                   Z-ADD     PiVal         VABNCL
042200130121     C                   ELSE
042300130121     C                   SETON                                        32
042400130121     C                   Z-ADD     *zeros        VABNCL
042500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042600130121     C                             + ' ' + 'VABNCL'
042700130121     C                   ENDIF
042800160322     C* DCR
042900171120     C                   IF        %subst(vindta:29:8)<>'00000000' and
043000171120     C                             %subst(vindta:29:8)<>*blanks
043100171120     C                   EVAL      PiStr=%trim(%subst(vindta:29:8))
043200160322     C                   EXSR      CHKNUM
043300160322     C                   IF        PiInt=*on
043400160322     C                   Z-ADD     PiVal         VABDCR
043500160322     C                   ELSE
043600160322     C                   SETON                                        32
043700160322     C                   Z-ADD     *zeros        VABDCR
043800160322     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043900160322     C                             + ' ' + 'VABDCR'
044000160322     C                   ENDIF
044100160322     C                   ENDIF
044200130121     C* PKB
044300171120     C                   EVAL      PiStr=%trim(%subst(vindta:426:9))
044400130121     C                   EXSR      CHKNUM
044500130121     C                   IF        PiNum=*on
044600160122     C                   EVAL(H)   VABPKB = PiVal/100
044700130121     C                   ELSE
044800130121     C                   SETON                                        32
044900130121     C                   Z-ADD     *zeros        VABPKB
045000130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100130121     C                             + ' ' + 'VABPKB'
045200130121     C                   ENDIF
045300160525     C* VLB
045400171120     C                   EVAL      PiStr=%trim(%subst(vindta:419:7))
045500160525     C                   EXSR      CHKNUM
045600160525     C                   IF        PiNum=*on
045700171120     C                   EVAL(H)   VABVLB = PiVal/1000
045800160525     C                   ELSE
045900160525     C                   SETON                                        32
046000160525     C                   Z-ADD     *zeros        VABVLB
046100160525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200160525     C                             + ' ' + 'VABVLB'
046300160525     C                   ENDIF
046400130121     C* CAS
046500171120     C                   IF        %subst(vindta:470:9)<>*zeros and
046600171120     C                             %subst(vindta:470:9)<>*blank
046700130121     C                   EVAL      FlgCAS = '1'
046800130121     C                   EVAL      VABVCA = 'EUR'
046900171120     C                   EVAL      VABTIC = 'TO'
047000171120     C                   EVAL      PiStr=%trim(%subst(vindta:470:9))
047100130121     C                   EXSR      CHKNUM
047200130121     C                   IF        PiNum=*on
047300171120     C                   EVAL(H)   VABCAS = PiVal/100
047400130121     C                   ELSE
047500130121     C                   SETON                                        32
047600130121     C                   Z-ADD     *zeros        VABCAS
047700130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047800130121     C                             + ' ' + 'VABCAS'
047900130121     C                   ENDIF
048000130121     C                   ENDIF
048100171124     C*
048200171124     C* Imposto cliente e tariffa dalle note del cliente
048300171128     C                   IF        %trim(%subst(vindta:1079:60))='GROUPAGE'
048400171124     C                   EVAL      VABCCM = 0962302
048500171124     C                   EVAL      VATCCM = 0962302
048600171124     C                   EVAL      VABLNP = 096
048700171124     C                   EVAL      VATLNP = 096
048800171124     C                   EVAL      VABCTR = 001
048900171124     C                   ELSE
049000171124     C                   EVAL      VABCCM = 0962301
049100171124     C                   EVAL      VATCCM = 0962301
049200171124     C                   EVAL      VABLNP = 096
049300171124     C                   EVAL      VATLNP = 096
049400171124     C                   EVAL      VABCTR = 300
049500171124     C                   ENDIF
049600070730     C*
049700070730     C* Considerazioni finali su CBO/CAS
049800070730     C                   IF        FlgCAS = '1'
049900070730     C                   IF        VABCBO = '1'
050000070730     C                   EVAL      VABCBO = '4'
050100070730     C                   ENDIF
050200070730     C                   IF        VABCBO = '2'
050300070730     C                   EVAL      VABCBO = '6'
050400070730     C                   ENDIF
050500070730     C                   ENDIF
050600020305     C*
050700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050800011113     C                   EXSR      CHKIMPDIV
050900010202     C*
051000000801     C* Ebbene...
051100000801     C                   ADD       1             §CTRMO
051200070530     C                   IF        *in31 <> *zeros OR
051300070530     C                             *in32 <> *zeros
051400000801     C                   ADD       1             §CTRNO
051500020725     C                   EVAL      x_vinflg = '2'
051600000801     C                   ELSE
051700010201     C                   ADD       1             §CTROKVB
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100010201     C*----------------------------------------------------*
052200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052300010201     C*----------------------------------------------------*
052400130121     C     WRIVAT_IJ     BEGSR
052500090204     C*
052600090204     C                   eval      vatAAS = vabAAS
052700090204     C                   eval      vatLNP = vabLNP
052800090204     C                   eval      vatNRS = vabNRS
052900090204     C                   eval      vatNSP = vabNSP
053000010201     C*
053100101007     C* Valorizzo il buffer di scrittura del FIVAT
053200171120     C***                eval      w70 = %trim(%subst(vindta:574:70))
053300130121     C* - TIPO RECORD "I"
053400130121     C                   if        %subst(w70:1:35) <> *blanks
053500130121     C                   eval      VATTRC = 'I'
053600130121     C                   eval      VATNOT = %trim(%subst(w70:1:35))
053700021113     C                   write     FIVAT000
053800020725     C                   endif
053900130121     C* - TIPO RECORD "J"
054000130121     C                   if        %subst(w70:36:35) <> *blanks
054100130121     C                   eval      VATTRC = 'J'
054200130121     C                   eval      VATNOT = %trim(%subst(w70:36:35))
054300130121     C                   write     FIVAT000
054400130121     C                   endif
054500010201     C*
054600010201     C                   ENDSR
054700071003     C*----------------------------------------------------*
054800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054900071003     C*----------------------------------------------------*
055000071003     C     WRIVAT_E      BEGSR
055100090204     C*
055200090204     C                   eval      vatAAS = vabAAS
055300090204     C                   eval      vatLNP = vabLNP
055400090204     C                   eval      vatNRS = vabNRS
055500090204     C                   eval      vatNSP = vabNSP
055600071003     C*
055700101007     C* Valorizzo il buffer di scrittura del FIVAT
055800071003     C* - TIPO RECORD "E"
055900171120     C***                if        %subst(vindta:331:26) <> *blanks
056000071003     C                   eval      VATTRC = 'E'
056100130121     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
056200071003     C                   write     FIVAT000
056300171120     C***                endif
056400071003     C*
056500071003     C                   ENDSR
056600020904
056700020904
056800010202     C*----------------------------------------------------*
056900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057000010202     C*----------------------------------------------------*
057100020305     C     PREVAT        BEGSR
057200010202     C*
057300021113     C* Compongo il nome del membro da dare al FIVATWWR
057400010202     C                   eval      parmbr = vlrhdl
057500010202     C                   movel     'M'           parmbr
057600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057700010202     C                   eval      paropz = '1'
057800010202     C* Effettuo la chiamata al CLLE preposto
057900050128     C                   call(e)   'TITVVTC'
058000010202     C                   parm                    parccm
058100010202     C                   parm                    parmbr
058200010202     C                   parm                    paropz
058300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058400010202     C                   if        %error
058500010202     C                   movel     '1'           chkcall
058600010202     C                   else
058700010202     C                   movel     '0'           chkcall
058800010202     C                   endif
058900010202     C*
059000010202     C                   ENDSR
059100000801     C*----------------------------------------------------*
059200000801     C*  CONTROLLO NUMERICITA' CAMPI
059300000801     C*----------------------------------------------------*
059400000801     C     CHKNUM        BEGSR
059500081113     C*
059600081113     C                   IF        PiDecChr = *blanks
059700121214     C                   EVAL      PiDecChr = ','
059800081113     C                   ENDIF
059900081113     C*
060000081113     C                   callp(e)  UBISNUM_Check(PiStr
060100081113     C                                          :PiDecChr
060200081113     C                                          :PiVal
060300081113     C                                          :PiNum
060400081113     C                                          :PiInt)
060500081113     C*
060600000801     C                   IF        %error
060700000801     C                   EVAL      PiInt=*off
060800000801     C                   ENDIF
060900000801     C*
061000000801     C                   ENDSR
061100000801     C***
061200000801
061300011113
061400011113     C*----------------------------------------------------*
061500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061600011113     C*----------------------------------------------------*
061700011113     C     CHKIMPDIV     BEGSR
061800011113     C*
061900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062000011113     C                   Z-ADD     *zeros        wrkDec            9 9
062100011113     C*
062200011113     C* Come prima cosa effettuo considerazioni sulla divisa
062300011113     C                   IF        vabIAS > *zeros
062400011113     C                   IF        vabVAS <> 'EUR'
062500011113     C                   EVAL      vabVAS =  'ITL'
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C                   IF        vabCAS > *zeros
063000011113     C                   IF        vabVCA <> 'EUR'
063100011113     C                   EVAL      vabVCA =  'ITL'
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C                   IF        vabVMD > *zeros
063600020305     C                   IF        vabVAD <> 'EUR'
063700011113     C                   EVAL      vabVAD =  'ITL'
063800011113     C                   ENDIF
063900011113     C                   ENDIF
064000011113     C*
064100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064200011113     C                   Z-ADD     vabIAS        wrkDec
064300011113     C                   IF        wrkDec > *zeros
064400011113     C                   IF        vabVAS = 'ITL'
064500011113     C                   EVAL      vabIAS = *zeros
064600011113     C                   ENDIF
064700011113     C                   ENDIF
064800011113     C*
064900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065000011113     C                   Z-ADD     vabCAS        wrkDec
065100011113     C                   IF        wrkDec > *zeros
065200011113     C                   IF        vabVCA = 'ITL'
065300011113     C                   EVAL      vabCAS = *zeros
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065800011113     C                   Z-ADD     vabVMD        wrkDec
065900011113     C                   IF        wrkDec > *zeros
066000011113     C                   IF        vabVAD = 'ITL'
066100011113     C                   EVAL      vabVMD = *zeros
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C                   ENDSR
066600011113     C***
066700011113
066800011113
066900000801
067000000801
067100990920      /TITLE Invio dei dati al punto operativo.
067200010202     C     invio         BEGSR
067300990920     C*
067400021113     C* 1° invio FIVAT
067500010201     C                   reset                   dscmz
067600010201     C                   move      vlrpoi        cmzdst
067700021113     C                   eval      cmzfld = 'FIVATWWR'
067800010201     C                   eval      cmzmbd = vlrhdl
067900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068000021009     C***                if        prmfir = *blanks
068100021113     C                   eval      cmzfla = 'FIVAT00F'
068200021113     C                   eval      cmzmba = 'FIVAT00F'
068300021009     C***                else
068400021009     C***                eval      cmzfla = prmfir
068500021009     C***                eval      cmzmba = prmfir
068600021009     C***                endif
068700010201     C                   eval      cmznrr = *zeros
068800020305     C                   move      §ctrokvt      cmznrr
068900021018     C                   eval      cmzlba = vlrfl1
069000010201     C                   call(e)   'TIS711C'
069100010201     C                   parm                    dscmz
069200010201     C                   parm      *blanks       esito
069300010205     C                   if        %error
069400010205     C                             or cmzerr = '1'
069500010205     C                             or esito  = '1'
069600010205     C                   eval      wrkesito = '3'
069700010205     C                   else
069800010201     C*
069900021113     C* 2° invio FIVAB
070000010201     C                   reset                   dscmz
070100010201     C                   move      vlrpoi        cmzdst
070200010201     C                   eval      cmzfld = vlrfou
070300010201     C                   eval      cmzmbd = vlrhdl
070400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070500021009     C***                if        prmfir = *blanks
070600021113     C                   eval      cmzfla = 'FIVAB00F'
070700021113     C                   eval      cmzmba = 'FIVAB00F'
070800021009     C***                else
070900021009     C***                eval      cmzfla = prmfir
071000021009     C***                eval      cmzmba = prmfir
071100021009     C***                endif
071200010201     C                   eval      cmznrr = *zeros
071300010201     C                   move      §ctrokvb      cmznrr
071400021018     C                   eval      cmzlba = vlrfl1
071500010201     C                   call(e)   'TIS711C'
071600010201     C                   parm                    dscmz
071700010201     C                   parm      *blanks       esito
071800010201     C                   if        %error
071900010201     C                             or cmzerr = '1'
072000010201     C                             or esito  = '1'
072100010201     C                   eval      wrkesito = '3'
072200010201     C                   endif
072300010205     C                   endif
072400990920     C*
072500000613     C                   ENDSR
072600000613     C***
072700070411
072800070411     C     *pssr         BEGSR
072900070411     C*
073000070411     C                   if        %open(tivin00r)
073100070411     C                   close     tivin00r
073200070411     C                   endif
073300070411     C                   if        %open(fivabwwr)
073400070411     C                   close     fivabwwr
073500070411     C                   endif
073600070411     C                   if        %open(fivatwwr)
073700070411     C                   close     fivatwwr
073800070411     C                   endif
073900070411     C*
074000070411     C* Effettuo la chiamata al CLLE preposto
074100070411     C                   call(e)   'TITVVTC'
074200070411     C                   parm                    parccm
074300070411     C                   parm                    parmbr
074400070411     C                   parm      '2'           paropz
074500070411     C*
074600070411     C                   eval      wrkesito = '2'
074700070411     C*
074800070411     C                   seton                                        LR
074900070411     C*
075000070411     C                   ENDSR     '*CANCL'
075100070411     C***
075200070411
075300990910
075400000613     C     *inzsr        BEGSR
075500990910     C*
075600990910     C     *entry        plist
075700990920     C                   parm                    tivlrds
075800990921     C                   parm      wrkesito      esito
075900000724     C                   parm                    prmlit
076000000710     C                   parm                    prmfir
076100000613     C*
076200000830     C* CALCOLA LA DATA CORRENTE
076300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076400100324     C                   eval      datcor = %dec(%date() : *ISO)
076500000830     C*
076600000613     C                   ENDSR
076700000613     C***
