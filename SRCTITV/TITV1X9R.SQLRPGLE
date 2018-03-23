000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500100412     FFIVABwwr  O    E             DISK    usropn
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
002100100525     D depspe          s              5    INZ(*blanks)
002200100525     D curspe          s              5    INZ(*blanks)
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
004300100309     D trul33ds      e ds                  inz
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
007000100525     C                   EXSR      DEFCAM
007100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400100525     C  N51              call(e)   'TITVVTC'
007500100525     C                   parm                    parccm
007600100525     C                   parm                    parmbr
007700100525     C                   parm      '2'           paropz
007800100525     C   51              call(e)   'TITVVBC'
007900100525     C                   parm                    parccm
008000100525     C                   parm                    parmbr
008100100525     C                   parm      '2'           paropz
008200070730     C*
008300070730     C* Effettuo lancio TISI95 solo x chiusura
008400070730     C                   CLEAR                   TISI95DS
008500070730     C                   EVAL      I95TLA = 'C'
008600070730     C                   CALL      'TISI95R'
008700070730     C                   PARM                    TISI95DS
008800000616     C*
008900000801     C
009000010201     C                   seton                                        LR
009100990908
009200000801
009300910830     C*--------------------------------------------------------
009400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009500910830     C*--------------------------------------------------------
009600070530     C     RWFILE        BEGSR
009700990910     C*
009800990914     C                   if        not %open(tivin00r)
009900990908     C                   open      tivin00r
010000990914     C                   endif
010100100525     C*
010200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010300100525     C                   exsr      prevasx
010400010201     C*
010500010202     C                   if        chkcall = '0'
010600100525     C*
010700100525     C                   if        not %open(fivabwwr)
010800100525     C                   open      fivabwwr
010900100525     C                   endif
011000010202     C*
011100021113     C                   if        not %open(fivatwwr)
011200021113     C                   open      fivatwwr
011300010201     C                   endif
011400080117     C*
011500080117     C                   EXSR      INZVAR
011600080117     C                   EXSR      DEFCAM
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200990910     C*
012300020725     C/EXEC SQL
012400020725     C+ declare C1 cursor for select
012500100525     C+ vindta, vinflg, substr(vindta, 1, 5) as sped, rrn(tivin00r)
012600060223     C+ from tivin00r
012700060223     C+ order by sped
012800020725     C+ for read only
012900020725     C/END-EXEC
013000020725     C
013100020725     C/EXEC SQL
013200020725     C+ open C1
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600070530     C+ Fetch C1 into :INPUT_DS
013700020725     C/END-EXEC
013800020725     C*
013900020725     C                   dow       sqlcod = *zeros
014000990913     C*
014100100310     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400020725     C                   if        vinflg = *blanks
014500020725     C                             or vinflg = '0'
014600020725     C                             or vinflg = '2'
014700000801     C*
014800020725     C                   clear                   x_vinmsg
014900020725     C                   eval      x_vinflg = '1'
015000010305     C*
015100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200100525     C                   EVAL      curspe=%trim(%subst(vindta:1:5))
015300010305     C*
015400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015500080923     C                   if        *in50 = *off
015600071003     C                   exsr      impvab
015700100412     C                   exsr      wrivab
015800100127     C                   exsr      wrivat_a                                     => carico VAT
015900071003     C                   exsr      wrivat_b                                     => carico VAT
016000100525     C   51              exsr      wrivat_e                                     => carico VAT
016100071003     C                   else
016200080923     C*
016300071009     C                   if        wDISK = 'D'
016400091223     C                   exsr      repNSP
016500071009     C                   exsr      impvab
016600100412     C                   exsr      wrivab
016700100127     C                   exsr      wrivat_a                                     => carico VAT
016800071009     C                   exsr      wrivat_b                                     => carico VAT
016900071009     C                   exsr      wrivat_e                                     => carico VAT
017000071009     C                   else
017100071009     C*
017200010305     C                   if        depspe = *blanks                             => 1° giro
017300010305     C                   eval      depspe = curspe                              => memorizz. spediz
017400080117     C                   clear                   tivinds
017500091223     C                   exsr      repNSP
017600020305     C                   exsr      impvab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300100412     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500091223     C                   exsr      repNSP
018600020305     C                   exsr      impvab
018700100127     C                   exsr      wrivat_a                                     => carico VAT
018800071003     C                   exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000020305     C                   else                                                   => x stessa spediz
019100100401     C*
019200100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019300100412     C                   if        wDUPREC <> 'N'
019400020305     C                   exsr      impvab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800100401     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100010305     C                   endif
020200071003     C                   endif
020300071009     C                   endif
020400000905     C*
020500000905     C                   else
020600020725     C                   eval      x_vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900020725     C     VINRRN        chain     tivin000
021000020725     C                   if        %found(tivin00r)
021100020725     C                   eval      y_vinflg = x_vinflg
021200020725     C                   eval      y_vinmsg = x_vinmsg
021300020725     C                   update    tivin000
021400020725     C                   endif
021500020725     C*
021600020725     C/EXEC SQL
021700070530     C+ Fetch C1 into :INPUT_DS
021800020725     C/END-EXEC
021900020725     C*
022000020725     C                   enddo
022100020725     C*
022200020725     C/EXEC SQL
022300020725     C+ close C1
022400020725     C/END-EXEC
022500000905     C*
022600020305     C* Scarico i VAB rimasti "in sospeso"
022700071009     C                   if        wDISK = 'C'
022800100412     C                   exsr      wrivab
022900071009     C                   endif
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800100525     C                   if        §ctrokvb > 0 OR
023900100525     C                             §ctrokvt > 0
024000990921     C                   eval      wrkesito = '1'
024100000710     C                   else
024200000710     C                   eval      wrkesito = '2'
024300990910     C                   endif
024400000710     C                   endif
024500990910     C*
024600990914     C                   if        %open(tivin00r)
024700990908     C                   close     tivin00r
024800990914     C                   endif
024900021113     C                   if        %open(fivabwwr)
025000021113     C                   close     fivabwwr
025100990914     C                   endif
025200021113     C                   if        %open(fivatwwr)
025300021113     C                   close     fivatwwr
025400010201     C                   endif
025500990910     C*
025600100525     C                   if        §ctrokvb > 0 OR
025700100525     C                             §ctrokvt > 0
025800000724     C                             and vlrpoi <> *zeros
025900010202     C                   exsr      invio
026000990920     C                   endif
026100990920     C*
026200910830     C                   ENDSR
026300000613     C***
026400100412
026500100412
026600010305
026700010305     C*----------------------------------------------------*
026800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026900010305     C*----------------------------------------------------*
027000020305     C     WRIVAB        BEGSR
027100100412     C*
027200100412     C* Gestisco l'eventuale rottura x numero spedizione
027300100412     C                   if        VABRMA = *blanks
027400100412     C                   movel(P)  VABRMN        VABRMA
027500100412     C                   endif
027600100412     C*
027700100525     C                   if        *in51 = *off and *in31 = *off
027800100525     C                   write     fivab000                                     => scarico il VAB
027900100525     C                   endif
028000010305     C*
028100010305     C                   ENDSR
028200990920
028300000801     C*----------------------------------------------------*
028400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028500000801     C*----------------------------------------------------*
028600010201     C     INZVAR        BEGSR
028700000801     C*
028800010201     C                   Z-ADD     *zeros        Num5_0
028900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029000020725     C                   MOVEL     '0'           FlgCAS            1
029100000801     C*
029200000801     C                   ENDSR
029300000801     C*----------------------------------------------------*
029400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029500000801     C*----------------------------------------------------*
029600020904     C     DEFCAM        BEGSR
029700080923     C*
029800100525     C                   SETOFF                                       5051
029900000801     C*
030000021113     C                   CLEAR                   FIVAB000
030100021113     C                   CLEAR                   FIVAT000
030200070730     C* Imposto i valori di default...
030300100525     C                   EVAL      VABCCM = 0771079
030400100525     C                   EVAL      VATCCM = 0771079
030500100525     C                   EVAL      VABLNP = 077
030600100525     C                   EVAL      VATLNP = 077
030700100503     C                   EVAL      VABCTR = 000
030800070730     C                   EVAL      VABCBO = '1'
030900070222     C* ... e poi verifico se sono stati passati come parametri
031000070222     C                   IF        vlrppt > *blanks
031100100525     C*
031200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABCCM
031700070222     C                   Z-ADD     PiVal         VATCCM
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABLNP
032500070222     C                   Z-ADD     PiVal         VATLNP
032600070222     C                   ENDIF
032700070222     C                   ENDIF
032800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000070222     C                   EXSR      CHKNUM
033100070222     C                   IF        PiInt=*on
033200070222     C                   Z-ADD     PiVal         VABCTR
033300070222     C                   ENDIF
033400070928     C                   ENDIF
033500071009     C                   MOVEL     *blanks       wDISK             1
033600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033700071009     C                   IF        wDISK <> *blanks
033800070928     C                   SETON                                        50
033900070222     C                   ENDIF
034000100525     C                   IF        wDISK  = 'T'
034100100525     C                   SETOFF                                       50
034200100525     C                   SETON                                        51
034300100525     C                   ENDIF
034400100401     C                   MOVEL     *blanks       wDUPREC           1
034500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
034600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
034700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
034800070222     C                   ENDIF
034900100525     C*
035000100525     C                   ENDIF
035100071009     C*
035200071009     C   50              EVAL      VABCTM = '7Q'
035300000801     C*
035400000801     C                   ENDSR
035500091223     C*----------------------------------------------------*
035600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035700091223     C*----------------------------------------------------*
035800091223     C     REPNSP        BEGSR
035900091223     C*
036000091223     C                   EXSR      INZVAR
036100091223     C                   EXSR      DEFCAM
036200091223     C*
036300091223     C* NSP => Stacco un numeratore da AZNUM
036400091223     C                   clear                   TRUL33DS
036500091223     C                   eval      I33OPE = *zeros
036600091223     C                   eval      I33CNU = 302
036700091223     C                   eval      I33NUM = 1
036800091223     C                   movel     TRUL33DS      KPJBU
036900091223     C                   call      'TRUL33R'
037000091223     C                   parm                    KPJBA
037100091223     C                   movel     KPJBU         TRUL33DS
037200091223     C                   if        O33ERR = *zeros
037300091223     C                   z-add     O33NRF        VABNSP
037400091223     C                   z-add     O33NRF        VATNSP
037500091223     C                   else
037600091223     C                   SETON                                        31
037700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037800091223     C                             + ' ' + 'VABNSP VATNSP'
037900091223     C                   endif
038000091223     C*
038100091223     C                   ENDSR
038200000801     C*----------------------------------------------------*
038300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
038400000801     C*----------------------------------------------------*
038500010201     C     IMPVAB        BEGSR
038600070530     C*
038700070530     C                   SETOFF                                       3132
038800070928     C*
038900070928     C                   MOVE(P)   vlrpoi        VABFGS
039000070928     C                   MOVE(P)   vlrpoi        VATFGS
039100070928     C*
039200070928     C                   MOVEL     datcor        VABAAS
039300070928     C                   MOVEL     datcor        VATAAS
039400070928     C                   MOVE      datcor        VABMGS
039500100525     C                   EVAL      VABRSD=%trim(%subst(vindta:14:50))
039600100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
039700100503     C     '@':'A'       XLATE     VABRSD        VABRSD
039800100503     C* ==
039900100525     C                   EVAL      VABIND=%trim(%subst(vindta:64:50))
040000100525     C                   EVAL      VABLOD=%trim(%subst(vindta:119:50))
040100100525     C                   EVAL      VABPRD=%trim(%subst(vindta:169:2))
040200100525     C                   EVAL      VABRMA=%trim(%subst(vindta:1:5))
040300100525     C                   MOVEL     *blanks       wNOTE           240
040400100525     C                   EVAL      wNOTE =%trim(%subst(vindta:201:240))
040500100525     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
040600100525     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
040700100520     C*
040800100503     C* CAD
040900100525     C                   EVAL      PiStr=%trim(%subst(vindta:114:5))
041000100503     C                   EXSR      CHKNUM
041100100503     C                   IF        PiInt=*on
041200100503     C                   Z-ADD     PiVal         Num5_0
041300100503     C                   MOVEL     Num5_0        VABCAD
041400100503     C                   ELSE
041500100503     C                   SETON                                        32
041600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041700100503     C                             + ' ' + 'VABCAD'
041800100503     C                   ENDIF
041900100525     C* NSP
042000100525     C                   IF        *in50 = *off
042100100525     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
042200100525     C                   EXSR      CHKNUM
042300100525     C                   IF        PiInt=*on
042400100525     C                   Z-ADD     PiVal         VABNSP
042500100525     C                   Z-ADD     PiVal         VATNSP
042600100525     C                   ELSE
042700100525     C                   SETON                                        31
042800100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042900100525     C                             + ' ' + 'VABNSP VATNSP'
043000100525     C                   ENDIF
043100100525     C                   ENDIF
043200100521     C* RMN
043300100525     C                   EVAL      PiStr=%trim(%subst(vindta:1:5))
043400100503     C                   EXSR      CHKNUM
043500100503     C                   IF        PiInt=*on
043600100503     C                   Z-ADD     PiVal         VABRMN
043700100503     C                   ELSE
043800100525     C                   SETON                                        32
043900100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044000100503     C                             + ' ' + 'VABRMN'
044100100503     C                   ENDIF
044200100503     C* NCL
044300100525     C                   EVAL      PiStr=%trim(%subst(vindta:171:6))
044400100503     C                   EXSR      CHKNUM
044500100503     C                   IF        PiInt=*on
044600100503     C                   Z-ADD     PiVal         VABNCL
044700100503     C                   ELSE
044800100503     C                   SETON                                        32
044900100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045000100503     C                             + ' ' + 'VABNCL'
045100100503     C                   ENDIF
045200100503     C* PKB
045300100525     C                   EVAL      PiStr=%trim(%subst(vindta:177:8))
045400100503     C                   EXSR      CHKNUM
045500100503     C                   IF        PiNum=*on
045600100525     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
045700100503     C                   Z-ADD     PiVal         VABPKB
045800100503     C                   ELSE
045900100503     C                   SETON                                        32
046000100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100100503     C                             + ' ' + 'VABPKB'
046200100503     C                   ENDIF
046300100525     C* VLB
046400100525     C                   EVAL      PiStr=%trim(%subst(vindta:185:8))
046500100525     C                   EXSR      CHKNUM
046600100525     C                   IF        PiNum=*on
046700100525     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
046800100525     C                   Z-ADD     PiVal         VABVLB
046900100525     C                   ELSE
047000100525     C                   SETON                                        32
047100100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047200100525     C                             + ' ' + 'VABVLB'
047300100525     C                   ENDIF
047400100310     C*
047500100310     C* Se provincia nn valorizzata la reperisco
047600100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
047700100525     C                   IF        VABNZD  = *blanks AND
047800100525     C                             VABPRD  = *blanks AND
047900100525     C                             VABCAD <> *blanks
048000100310     C                   CLEAR                   TISI95DS
048100100310     C                   EVAL      I95TCN = '3'
048200100310     C                   Z-ADD     datcor        I95DAT
048300100310     C                   EVAL      I95NAR = VABNZD
048400100310     C                   EVAL      I95CAP = VABCAD
048500100310     C                   EVAL      I95LOC = VABLOD
048600100310     C                   CALL      'TISI95R'
048700100310     C                   PARM                    TISI95DS
048800100310     C                   EVAL      VABPRD = O95PRV
048900100310     C                   ENDIF
049000070730     C*
049100070730     C* Considerazioni finali su CBO/CAS
049200070730     C                   IF        FlgCAS = '1'
049300070730     C                   IF        VABCBO = '1'
049400070730     C                   EVAL      VABCBO = '4'
049500070730     C                   ENDIF
049600070730     C                   IF        VABCBO = '2'
049700070730     C                   EVAL      VABCBO = '6'
049800070730     C                   ENDIF
049900070730     C                   ENDIF
050000010202     C*
050100000801     C* Ebbene...
050200000801     C                   ADD       1             §CTRMO
050300070530     C                   IF        *in31 <> *zeros OR
050400070530     C                             *in32 <> *zeros
050500000801     C                   ADD       1             §CTRNO
050600020725     C                   EVAL      x_vinflg = '2'
050700000801     C                   ELSE
050800010201     C                   ADD       1             §CTROKVB
050900000801     C                   ENDIF
051000000801     C*
051100000801     C                   ENDSR
051200100127     C*----------------------------------------------------*
051300100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
051400100127     C*----------------------------------------------------*
051500100127     C     WRIVAT_A      BEGSR
051600100127     C*
051700100127     C* Valorizzo l buffer di scrittura del FIVAT
051800100127     C* - TIPO RECORD "A"
051900100412     C***                eval      VATTRC = 'A'
052000100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
052100100412     C***                exsr      wrivat
052200100127     C*
052300100127     C                   ENDSR
052400010201     C*----------------------------------------------------*
052500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052600010201     C*----------------------------------------------------*
052700071003     C     WRIVAT_B      BEGSR
052800010201     C*
052900021113     C* Valorizzo l buffer di scrittura del FIVAT
053000070928     C* - TIPO RECORD "B"
053100100412     C***                eval      VATTRC = 'B'
053200100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
053300100412     C***                exsr      wrivat
053400010201     C*
053500010201     C                   ENDSR
053600071003     C*----------------------------------------------------*
053700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
053800071003     C*----------------------------------------------------*
053900071003     C     WRIVAT_E      BEGSR
054000071003     C*
054100071003     C* Valorizzo l buffer di scrittura del FIVAT
054200071003     C* - TIPO RECORD "E"
054300100310     C                   eval      VATTRC = 'E'
054400100419     C*
054500100419     C* X ogni collo indicato in testata
054600100419     C                   z-add     1             wColli            5 0
054700100419     C                   dow       wColli <= VABNCL
054800100419     C*
054900100419     C* Valorizzo il buffer di scrittura del FIVAT
055000100419     C                   move(p)   wColli        wColliAlfa        2
055100100503     C***                eval      VATNOT = %trim(%subst(vindta:331:26))+
055200100503     C***                                   wColliAlfa
055300100419     C                   exsr      wrivat                                       => scarico VAT
055400100419     C*
055500100419     C                   add       1             wColli
055600100419     C                   enddo
055700100419     C*
055800071003     C*
055900071003     C                   ENDSR
056000100127     C*----------------------------------------------------*
056100100127     C*  SCARICO BUFFER FIVAT
056200100127     C*----------------------------------------------------*
056300100127     C     WRIVAT        BEGSR
056400100127     C*
056500100127     C                   if        vatNOT <> *blanks
056600100127     C                   ADD       1             §CTROKVT
056700100127     C                   write     FIVAT000
056800100127     C                   endif
056900100127     C*
057000100127     C                   ENDSR
057100020904
057200020904
057300100525     C*----------------------------------------------------*
057400100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
057500100525     C*----------------------------------------------------*
057600100525     C     PREVASX       BEGSR
057700010202     C*
057800021113     C* Compongo il nome del membro da dare al FIVATWWR
057900010202     C                   eval      parmbr = vlrhdl
058000010202     C                   movel     'M'           parmbr
058100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
058200010202     C                   eval      paropz = '1'
058300100525     C*
058400100525     C* Effettuo la chiamata al CLLE preposto
058500100525     C  N51              call(e)   'TITVVTC'
058600100525     C                   parm                    parccm
058700100525     C                   parm                    parmbr
058800100525     C                   parm                    paropz
058900100525     C   51              call(e)   'TITVVBC'
059000100525     C                   parm                    parccm
059100100525     C                   parm                    parmbr
059200100525     C                   parm                    paropz
059300100525     C*
059400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059500010202     C                   if        %error
059600010202     C                   movel     '1'           chkcall
059700010202     C                   else
059800010202     C                   movel     '0'           chkcall
059900010202     C                   endif
060000010202     C*
060100010202     C                   ENDSR
060200000801     C*----------------------------------------------------*
060300000801     C*  CONTROLLO NUMERICITA' CAMPI
060400000801     C*----------------------------------------------------*
060500000801     C     CHKNUM        BEGSR
060600081113     C*
060700081113     C                   IF        PiDecChr = *blanks
060800100503     C                   EVAL      PiDecChr = '.'
060900081113     C                   ENDIF
061000091223     C*
061100081113     C                   callp(e)  UBISNUM_Check(PiStr
061200081113     C                                          :PiDecChr
061300081113     C                                          :PiVal
061400081113     C                                          :PiNum
061500081113     C                                          :PiInt)
061600081113     C*
061700000801     C                   IF        %error
061800000801     C                   EVAL      PiInt=*off
061900000801     C                   ENDIF
062000000801     C*
062100000801     C                   ENDSR
062200000801     C***
062300011113
062400011113
062500000801
062600000801
062700990920      /TITLE Invio dei dati al punto operativo.
062800010202     C     invio         BEGSR
062900990920     C*
063000021113     C* 1° invio FIVAT
063100010201     C                   reset                   dscmz
063200010201     C                   move      vlrpoi        cmzdst
063300021113     C                   eval      cmzfld = 'FIVATWWR'
063400010201     C                   eval      cmzmbd = vlrhdl
063500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063600021009     C***                if        prmfir = *blanks
063700021113     C                   eval      cmzfla = 'FIVAT00F'
063800021113     C                   eval      cmzmba = 'FIVAT00F'
063900021009     C***                else
064000021009     C***                eval      cmzfla = prmfir
064100021009     C***                eval      cmzmba = prmfir
064200021009     C***                endif
064300010201     C                   eval      cmznrr = *zeros
064400020305     C                   move      §ctrokvt      cmznrr
064500021018     C                   eval      cmzlba = vlrfl1
064600010201     C                   call(e)   'TIS711C'
064700010201     C                   parm                    dscmz
064800010201     C                   parm      *blanks       esito
064900010205     C                   if        %error
065000010205     C                             or cmzerr = '1'
065100010205     C                             or esito  = '1'
065200010205     C                   eval      wrkesito = '3'
065300010205     C                   else
065400010201     C*
065500021113     C* 2° invio FIVAB
065600100525     C                   if        *in51 = *off
065700010201     C                   reset                   dscmz
065800010201     C                   move      vlrpoi        cmzdst
065900010201     C                   eval      cmzfld = vlrfou
066000010201     C                   eval      cmzmbd = vlrhdl
066100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066200021009     C***                if        prmfir = *blanks
066300021113     C                   eval      cmzfla = 'FIVAB00F'
066400021113     C                   eval      cmzmba = 'FIVAB00F'
066500021009     C***                else
066600021009     C***                eval      cmzfla = prmfir
066700021009     C***                eval      cmzmba = prmfir
066800021009     C***                endif
066900010201     C                   eval      cmznrr = *zeros
067000010201     C                   move      §ctrokvb      cmznrr
067100021018     C                   eval      cmzlba = vlrfl1
067200010201     C                   call(e)   'TIS711C'
067300010201     C                   parm                    dscmz
067400010201     C                   parm      *blanks       esito
067500010201     C                   if        %error
067600010201     C                             or cmzerr = '1'
067700010201     C                             or esito  = '1'
067800010201     C                   eval      wrkesito = '3'
067900010201     C                   endif
068000100525     C                   endif
068100010205     C                   endif
068200990920     C*
068300000613     C                   ENDSR
068400000613     C***
068500070411
068600070411     C     *pssr         BEGSR
068700070411     C*
068800070411     C                   if        %open(tivin00r)
068900070411     C                   close     tivin00r
069000070411     C                   endif
069100070411     C                   if        %open(fivabwwr)
069200070411     C                   close     fivabwwr
069300070411     C                   endif
069400070411     C                   if        %open(fivatwwr)
069500070411     C                   close     fivatwwr
069600070411     C                   endif
069700070411     C*
069800070411     C* Effettuo la chiamata al CLLE preposto
069900100525     C  N51              call(e)   'TITVVTC'
070000100525     C                   parm                    parccm
070100100525     C                   parm                    parmbr
070200100525     C                   parm      '2'           paropz
070300100525     C   51              call(e)   'TITVVBC'
070400100525     C                   parm                    parccm
070500100525     C                   parm                    parmbr
070600100525     C                   parm      '2'           paropz
070700070411     C*
070800070411     C                   eval      wrkesito = '2'
070900070411     C*
071000070411     C                   seton                                        LR
071100070411     C*
071200070411     C                   ENDSR     '*CANCL'
071300070411     C***
071400070411
071500990910
071600000613     C     *inzsr        BEGSR
071700990910     C*
071800990910     C     *entry        plist
071900990920     C                   parm                    tivlrds
072000990921     C                   parm      wrkesito      esito
072100000724     C                   parm                    prmlit
072200000710     C                   parm                    prmfir
072300000613     C*
072400000830     C* CALCOLA LA DATA CORRENTE
072500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072600091223     C                   eval      datcor = %dec(%date() : *ISO)
072700000830     C*
072800000613     C                   ENDSR
072900000613     C***
