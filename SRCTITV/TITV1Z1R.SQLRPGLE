000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130620     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002100100728     D depspe          s              6    INZ(*blanks)
002200100728     D curspe          s              6    INZ(*blanks)
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
003400100728     D  VINDTA                     2048
003500100728     D  VINFLG                        1
003600100728     D  VINSPE                        6
003700100728     D  VINRRN                        8  0
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
007000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
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
009100000801
009200910830     C*--------------------------------------------------------
009300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009400910830     C*--------------------------------------------------------
009500070530     C     RWFILE        BEGSR
009600990910     C*
009700990914     C                   if        not %open(tivin00r)
009800990908     C                   open      tivin00r
009900990914     C                   endif
010000070530     C*
010100100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010200100525     C                   exsr      prevasx
010300010201     C*
010400010202     C                   if        chkcall = '0'
010500010202     C*
010600100525     C                   if        not %open(fivabwwr)
010700100525     C                   open      fivabwwr
010800100525     C                   endif
010900100525     C*
011000021113     C                   if        not %open(fivatwwr)
011100021113     C                   open      fivatwwr
011200010201     C                   endif
011300080117     C*
011400080117     C                   EXSR      INZVAR
011500080117     C                   EXSR      DEFCAM
011600990910     C*
011700010201     C                   clear                   §CTROKVB          5 0
011800020305     C                   clear                   §CTROKVT          5 0
011900000801     C                   clear                   §CTRMO            5 0
012000000801     C                   clear                   §CTRNO            5 0
012100990910     C*
012200020725     C/EXEC SQL
012300100726     C+ declare C1 cursor for
012400100726     C+ select vindta, vinflg, substr(vindta, 200, 6) as sped, rrn(tivin00r)
012500060223     C+ from tivin00r
012600100728     C+ where substr(vindta, 1, 4) = '0000'
012700100726     C+ union
012800100726     C+ select vindta, vinflg, substr(vindta, 5, 6) as sped, rrn(tivin00r)
012900100726     C+ from tivin00r
013000100726     C+ where substr(vindta, 1, 4) = 'NVLI'
013100100726     C+ union
013200100726     C+ select vindta, vinflg, substr(vindta, 23, 6) as sped, rrn(tivin00r)
013300100726     C+ from tivin00r
013400100726     C+ where substr(vindta, 1, 4) = '2010'
013500060223     C+ order by sped
013600020725     C+ for read only
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000020725     C+ open C1
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400070530     C+ Fetch C1 into :INPUT_DS
014500020725     C/END-EXEC
014600020725     C*
014700020725     C                   dow       sqlcod = *zeros
014800990913     C*
014900100310     C                   if        vindta > *blanks
015000000613     C                   add       1             rrnum
015100000801     C*
015200020725     C                   if        vinflg = *blanks
015300020725     C                             or vinflg = '0'
015400020725     C                             or vinflg = '2'
015500000801     C*
015600020725     C                   clear                   x_vinmsg
015700020725     C                   eval      x_vinflg = '1'
015800100728     C*
015900100728     C                   z-add     1             wGiro             1 0
016000010305     C*
016100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016200100728     C                   EVAL      curspe=VINSPE
016300010305     C*
016400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016500080923     C                   if        *in50 = *off
016600071003     C                   exsr      impvab
016700100525     C                   exsr      wrivab
016800100127     C                   exsr      wrivat_a                                     => carico VAT
016900071003     C                   exsr      wrivat_b                                     => carico VAT
017000100525     C   51              exsr      wrivat_e                                     => carico VAT
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400091223     C                   exsr      repNSP
017500071009     C                   exsr      impvab
017600100412     C                   exsr      wrivab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071009     C                   exsr      wrivat_b                                     => carico VAT
017900071009     C                   exsr      wrivat_e                                     => carico VAT
018000071009     C                   else
018100071009     C*
018200010305     C                   if        depspe = *blanks                             => 1° giro
018300010305     C                   eval      depspe = curspe                              => memorizz. spediz
018400080117     C                   clear                   tivinds
018500100728     C***                exsr      repNSP
018600020305     C                   exsr      impvab
018700100127     C                   exsr      wrivat_a                                     => carico VAT
018800071003     C                   exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200010305     C                   eval      depspe = curspe                              => memorizz. spediz
019300100412     C                   exsr      wrivab
019400080117     C                   clear                   tivinds
019500100728     C***                exsr      repNSP
019600020305     C                   exsr      impvab
019700100127     C                   exsr      wrivat_a                                     => carico VAT
019800071003     C                   exsr      wrivat_b                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000020305     C                   else                                                   => x stessa spediz
020100100401     C*
020200100728     C* Se nn richiesta esclusione spedizioni "duplicate"
020300100728     C                   select
020400100728     C                   when      wDUPREC = *blanks
020500020305     C                   exsr      impvab
020600100127     C                   exsr      wrivat_a                                     => carico VAT
020700071003     C                   exsr      wrivat_b                                     => carico VAT
020800071003     C   50              exsr      wrivat_e                                     => carico VAT
020900100728     C                   when      wDUPREC = 'C'
021000100728     C   50              exsr      wrivat_e                                     => carico VAT
021100100728     C                   endsl
021200010305     C                   endif
021300010305     C                   endif
021400010305     C                   endif
021500071003     C                   endif
021600071009     C                   endif
021700000905     C*
021800000905     C                   else
021900020725     C                   eval      x_vinflg = '1'
022000000905     C                   endif
022100000905     C*
022200020725     C     VINRRN        chain     tivin000
022300020725     C                   if        %found(tivin00r)
022400020725     C                   eval      y_vinflg = x_vinflg
022500020725     C                   eval      y_vinmsg = x_vinmsg
022600020725     C                   update    tivin000
022700020725     C                   endif
022800020725     C*
022900020725     C/EXEC SQL
023000070530     C+ Fetch C1 into :INPUT_DS
023100020725     C/END-EXEC
023200020725     C*
023300020725     C                   enddo
023400020725     C*
023500020725     C/EXEC SQL
023600020725     C+ close C1
023700020725     C/END-EXEC
023800000905     C*
023900020305     C* Scarico i VAB rimasti "in sospeso"
024000071009     C                   if        wDISK = 'C'
024100100412     C                   exsr      wrivab
024200071009     C                   endif
024300010202     C*
024400010202     C                   endif
024500990910
024600990910     C* Se non ci sono record con errori ...
024700000710     C                   if        §ctrno = 0
024800990910     C* ... restituisco esito OK.
024900990921     C                   eval      wrkesito = '0'
025000990910     C                   else
025100100525     C                   if        §ctrokvb > 0 OR
025200100525     C                             §ctrokvt > 0
025300990921     C                   eval      wrkesito = '1'
025400000710     C                   else
025500000710     C                   eval      wrkesito = '2'
025600990910     C                   endif
025700000710     C                   endif
025800990910     C*
025900990914     C                   if        %open(tivin00r)
026000990908     C                   close     tivin00r
026100990914     C                   endif
026200021113     C                   if        %open(fivabwwr)
026300021113     C                   close     fivabwwr
026400990914     C                   endif
026500021113     C                   if        %open(fivatwwr)
026600021113     C                   close     fivatwwr
026700010201     C                   endif
026800990910     C*
026900100525     C                   if        §ctrokvb > 0 OR
027000100525     C                             §ctrokvt > 0
027100000724     C                             and vlrpoi <> *zeros
027200010202     C                   exsr      invio
027300990920     C                   endif
027400990920     C*
027500910830     C                   ENDSR
027600000613     C***
027700100412
027800100412
027900010305
028000010305     C*----------------------------------------------------*
028100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028200010305     C*----------------------------------------------------*
028300020305     C     WRIVAB        BEGSR
028400100412     C*
028500100412     C* Gestisco l'eventuale rottura x numero spedizione
028600100412     C                   if        VABRMA = *blanks
028700100412     C                   movel(P)  VABRMN        VABRMA
028800100412     C                   endif
028900100412     C*
029000100525     C                   if        *in51 = *off and *in31 = *off
029100100525     C                   write     fivab000                                     => scarico il VAB
029200100525     C                   endif
029300010305     C*
029400010305     C                   ENDSR
029500990920
029600000801     C*----------------------------------------------------*
029700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029800000801     C*----------------------------------------------------*
029900010201     C     INZVAR        BEGSR
030000000801     C*
030100010201     C                   Z-ADD     *zeros        Num5_0
030200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030300020725     C                   MOVEL     '0'           FlgCAS            1
030400000801     C*
030500000801     C                   ENDSR
030600000801     C*----------------------------------------------------*
030700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030800000801     C*----------------------------------------------------*
030900020904     C     DEFCAM        BEGSR
031000080923     C*
031100100525     C                   SETOFF                                       5051
031200000801     C*
031300021113     C                   CLEAR                   FIVAB000
031400021113     C                   CLEAR                   FIVAT000
031500070730     C* Imposto i valori di default...
031600100728     C                   EVAL      VABCCM = 0015079
031700100728     C                   EVAL      VATCCM = 0015079
031800100728     C                   EVAL      VABLNP = 001
031900100728     C                   EVAL      VATLNP = 001
032000100525     C                   EVAL      VABCTR = 000
032100100525     C                   EVAL      VABCBO = '1'
032200070222     C* ... e poi verifico se sono stati passati come parametri
032300070222     C                   IF        vlrppt > *blanks
032400100525     C*
032500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
032600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032700070222     C                   EXSR      CHKNUM
032800070222     C                   IF        PiInt=*on
032900070222     C                   Z-ADD     PiVal         VABCCM
033000070222     C                   Z-ADD     PiVal         VATCCM
033100070222     C                   ENDIF
033200070222     C                   ENDIF
033300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033500070222     C                   EXSR      CHKNUM
033600070222     C                   IF        PiInt=*on
033700070222     C                   Z-ADD     PiVal         VABLNP
033800070222     C                   Z-ADD     PiVal         VATLNP
033900070222     C                   ENDIF
034000070222     C                   ENDIF
034100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABCTR
034600070222     C                   ENDIF
034700070928     C                   ENDIF
034800071009     C                   MOVEL     *blanks       wDISK             1
034900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035000071009     C                   IF        wDISK <> *blanks
035100070928     C                   SETON                                        50
035200070222     C                   ENDIF
035300100525     C                   IF        wDISK  = 'T'
035400100525     C                   SETOFF                                       50
035500100525     C                   SETON                                        51
035600100525     C                   ENDIF
035700100401     C                   MOVEL     *blanks       wDUPREC           1
035800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
035900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036100100525     C                   ENDIF
036200100525     C*
036300070222     C                   ENDIF
036400071009     C*
036500071009     C   50              EVAL      VABCTM = '7Q'
036600000801     C*
036700000801     C                   ENDSR
036800091223     C*----------------------------------------------------*
036900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037000091223     C*----------------------------------------------------*
037100091223     C     REPNSP        BEGSR
037200091223     C*
037300091223     C                   EXSR      INZVAR
037400091223     C                   EXSR      DEFCAM
037500091223     C*
037600091223     C* NSP => Stacco un numeratore da AZNUM
037700091223     C                   clear                   TRUL33DS
037800091223     C                   eval      I33OPE = *zeros
037900091223     C                   eval      I33CNU = 302
038000091223     C                   eval      I33NUM = 1
038100091223     C                   movel     TRUL33DS      KPJBU
038200091223     C                   call      'TRUL33R'
038300091223     C                   parm                    KPJBA
038400091223     C                   movel     KPJBU         TRUL33DS
038500091223     C                   if        O33ERR = *zeros
038600091223     C                   z-add     O33NRF        VABNSP
038700091223     C                   z-add     O33NRF        VATNSP
038800091223     C                   else
038900091223     C                   SETON                                        31
039000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100091223     C                             + ' ' + 'VABNSP VATNSP'
039200091223     C                   endif
039300091223     C*
039400091223     C                   ENDSR
039500000801     C*----------------------------------------------------*
039600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
039700000801     C*----------------------------------------------------*
039800010201     C     IMPVAB        BEGSR
039900070530     C*
040000070530     C                   SETOFF                                       3132
040100100728     C*
040200100728     C* Gestione tipo record '0000'
040300100728     C                   IF        %subst(vindta:1:4) = '0000'
040400100728     C*
040500100728     C* Se nn primo giro => scarico il buffer precedente
040600100728     C                   if        wGiro = 1
040700100728     C                   eval      wGiro = 2
040800100728     C                   else
040900100728     C                   WRITE     FIVAB000
041000100728     C                   endif
041100100728     C*
041200100728     C* ......inizializzazioni iniziali e formati record file Bartolini
041300100728     C                   EXSR      INZVAR
041400100728     C                   EXSR      DEFCAM
041500100728     C*
041600100728     C                   MOVE(P)   vlrpoi        VABFGS
041700100728     C                   MOVE(P)   vlrpoi        VATFGS
041800100728     C*
041900100728     C                   MOVEL     datcor        VABAAS
042000100728     C                   MOVEL     datcor        VATAAS
042100100728     C                   MOVE      datcor        VABMGS
042200100728     C*
042300100728     C                   EVAL      VABRSD=%trim(%subst(vindta:6:30))
042400100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042500100503     C     '@':'A'       XLATE     VABRSD        VABRSD
042600100503     C* ==
042700100728     C                   EVAL      VABIND=%trim(%subst(vindta:36:30))
042800100728     C                   EVAL      VABLOD=%trim(%subst(vindta:66:20))
042900100728     C                   EVAL      VABPRD=%trim(%subst(vindta:91:2))
043000100728     C                   EVAL      VABRMA=%trim(%subst(vindta:200:6))
043100100520     C*
043200100525     C* NSP
043300100525     C                   IF        *in50 = *off
043400100728     C                   EVAL      PiStr=VINSPE
043500100525     C                   EXSR      CHKNUM
043600100525     C                   IF        PiInt=*on
043700100525     C                   Z-ADD     PiVal         VABNSP
043800100525     C                   Z-ADD     PiVal         VATNSP
043900100525     C                   ELSE
044000100525     C                   SETON                                        31
044100100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200100525     C                             + ' ' + 'VABNSP VATNSP'
044300100525     C                   ENDIF
044400100728     C*
044500100728     C                   ELSE
044600100728     C*
044700100728     C* NSP => Stacco un numeratore da AZNUM
044800100728     C                   clear                   TRUL33DS
044900100728     C                   eval      I33OPE = *zeros
045000100728     C                   eval      I33CNU = 302
045100100728     C                   eval      I33NUM = 1
045200100728     C                   movel     TRUL33DS      KPJBU
045300100728     C                   call      'TRUL33R'
045400100728     C                   parm                    KPJBA
045500100728     C                   movel     KPJBU         TRUL33DS
045600100728     C                   if        O33ERR = *zeros
045700100728     C                   z-add     O33NRF        VABNSP
045800100728     C                   z-add     O33NRF        VATNSP
045900100728     C                   else
046000100728     C                   SETON                                        31
046100100728     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200100728     C                             + ' ' + 'VABNSP VATNSP'
046300100728     C                   endif
046400100728     C*
046500100525     C                   ENDIF
046600100525     C* RMN
046700100728     C                   EVAL      PiStr=%trim(%subst(vindta:200:6))
046800100629     C                   EXSR      CHKNUM
046900100629     C                   IF        PiInt=*on
047000100629     C                   Z-ADD     PiVal         VABRMN
047100100629     C                   ELSE
047200100629     C                   SETON                                        32
047300100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400100629     C                             + ' ' + 'VABRMN'
047500100629     C                   ENDIF
047600100503     C* CAD
047700100728     C                   EVAL      PiStr=%trim(%subst(vindta:86:5))
047800100629     C                   EXSR      CHKNUM
047900100629     C                   IF        PiInt=*on
048000100629     C                   Z-ADD     PiVal         Num5_0
048100100629     C                   MOVEL     Num5_0        VABCAD
048200100629     C                   ELSE
048300100629     C                   SETON                                        32
048400100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048500100629     C                             + ' ' + 'VABCAD'
048600100629     C                   ENDIF
048700100503     C* NCL
048800100728     C                   EVAL      PiStr=%trim(%subst(vindta:129:5))
048900100629     C                   EXSR      CHKNUM
049000100629     C                   IF        PiInt=*on
049100100629     C                   Z-ADD     PiVal         VABNCL
049200100629     C                   ELSE
049300100629     C                   SETON                                        32
049400100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500100629     C                             + ' ' + 'VABNCL'
049600100629     C                   ENDIF
049700100503     C* PKB
049800100728     C                   EVAL      PiStr=%trim(%subst(vindta:134:4))
049900100629     C                   EXSR      CHKNUM
050000100629     C                   IF        PiNum=*on
050100100629     C                   Z-ADD     PiVal         VABPKB
050200100629     C                   ELSE
050300100629     C                   SETON                                        32
050400100629     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050500100629     C                             + ' ' + 'VABPKB'
050600100629     C                   ENDIF
050700100310     C*
050800100310     C* Se provincia nn valorizzata la reperisco
050900100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
051000100629     C                   IF        VABNZD  = *blanks AND
051100100629     C                             VABPRD  = *blanks AND
051200100629     C                             VABCAD <> *blanks
051300100629     C                   CLEAR                   TISI95DS
051400100629     C                   EVAL      I95TCN = '3'
051500100629     C                   Z-ADD     datcor        I95DAT
051600100629     C                   EVAL      I95NAR = VABNZD
051700100629     C                   EVAL      I95CAP = VABCAD
051800100629     C                   EVAL      I95LOC = VABLOD
051900100629     C                   CALL      'TISI95R'
052000100629     C                   PARM                    TISI95DS
052100100629     C                   EVAL      VABPRD = O95PRV
052200100629     C                   ENDIF
052300070730     C*
052400070730     C* Considerazioni finali su CBO/CAS
052500100629     C                   IF        FlgCAS = '1'
052600100629     C                   IF        VABCBO = '1'
052700100629     C                   EVAL      VABCBO = '4'
052800100629     C                   ENDIF
052900100629     C                   IF        VABCBO = '2'
053000100629     C                   EVAL      VABCBO = '6'
053100100629     C                   ENDIF
053200100629     C                   ENDIF
053300100728     C*
053400100728     C                   ENDIF
053500100728     C*
053600100728     C* Gestione tipo record 'NVLI'
053700100728     C                   IF        %subst(vindta:1:4) = 'NVLI'
053800100728     C*
053900100728     C                   EVAL      VABNOT=%trim(%subst(vindta:11:35))
054000100728     C                   EVAL      VABNT2=%trim(%subst(vindta:11+35:35))
054100100728     C*
054200100728     C                   ENDIF
054300010202     C*
054400000801     C* Ebbene...
054500000801     C                   ADD       1             §CTRMO
054600070530     C                   IF        *in31 <> *zeros OR
054700070530     C                             *in32 <> *zeros
054800000801     C                   ADD       1             §CTRNO
054900020725     C                   EVAL      x_vinflg = '2'
055000000801     C                   ELSE
055100010201     C                   ADD       1             §CTROKVB
055200000801     C                   ENDIF
055300000801     C*
055400000801     C                   ENDSR
055500100127     C*----------------------------------------------------*
055600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
055700100127     C*----------------------------------------------------*
055800100127     C     WRIVAT_A      BEGSR
055900100127     C*
056000100127     C* Valorizzo l buffer di scrittura del FIVAT
056100100127     C* - TIPO RECORD "A"
056200100412     C***                eval      VATTRC = 'A'
056300100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
056400100412     C***                exsr      wrivat
056500100127     C*
056600100127     C                   ENDSR
056700010201     C*----------------------------------------------------*
056800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056900010201     C*----------------------------------------------------*
057000071003     C     WRIVAT_B      BEGSR
057100010201     C*
057200021113     C* Valorizzo l buffer di scrittura del FIVAT
057300070928     C* - TIPO RECORD "B"
057400100412     C***                eval      VATTRC = 'B'
057500100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
057600100412     C***                exsr      wrivat
057700010201     C*
057800010201     C                   ENDSR
057900071003     C*----------------------------------------------------*
058000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058100071003     C*----------------------------------------------------*
058200071003     C     WRIVAT_E      BEGSR
058300071003     C*
058400071003     C* Valorizzo l buffer di scrittura del FIVAT
058500071003     C* - TIPO RECORD "E"
058600100728     C*
058700100728     C* Gestione tipo record '2010'
058800100728     C                   IF        %subst(vindta:1:4) = '2010'
058900100728     C*
059000100728     C                   eval      VATTRC = 'E'
059100100728     C                   eval      VATNOT = %trim(%subst(vindta:29:9))
059200100728     C                   exsr      wrivat
059300100728     C*
059400100728     C                   ENDIF
059500071003     C*
059600071003     C                   ENDSR
059700100127     C*----------------------------------------------------*
059800100127     C*  SCARICO BUFFER FIVAT
059900100127     C*----------------------------------------------------*
060000100127     C     WRIVAT        BEGSR
060100100127     C*
060200100127     C                   if        vatNOT <> *blanks
060300100127     C                   ADD       1             §CTROKVT
060400100127     C                   write     FIVAT000
060500100127     C                   endif
060600100127     C*
060700100127     C                   ENDSR
060800020904
060900020904
061000010202     C*----------------------------------------------------*
061100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
061200010202     C*----------------------------------------------------*
061300100525     C     PREVASX       BEGSR
061400100525     C*
061500021113     C* Compongo il nome del membro da dare al FIVATWWR
061600010202     C                   eval      parmbr = vlrhdl
061700010202     C                   movel     'M'           parmbr
061800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061900010202     C                   eval      paropz = '1'
062000100525     C*
062100010202     C* Effettuo la chiamata al CLLE preposto
062200100525     C  N51              call(e)   'TITVVTC'
062300010202     C                   parm                    parccm
062400010202     C                   parm                    parmbr
062500010202     C                   parm                    paropz
062600100525     C   51              call(e)   'TITVVBC'
062700100525     C                   parm                    parccm
062800100525     C                   parm                    parmbr
062900100525     C                   parm                    paropz
063000100525     C*
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
064500100503     C                   EVAL      PiDecChr = '.'
064600081113     C                   ENDIF
064700091223     C*
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
069300100525     C                   if        *in51 = *off
069400010201     C                   reset                   dscmz
069500010201     C                   move      vlrpoi        cmzdst
069600010201     C                   eval      cmzfld = vlrfou
069700010201     C                   eval      cmzmbd = vlrhdl
069800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069900021009     C***                if        prmfir = *blanks
070000021113     C                   eval      cmzfla = 'FIVAB00F'
070100021113     C                   eval      cmzmba = 'FIVAB00F'
070200021009     C***                else
070300021009     C***                eval      cmzfla = prmfir
070400021009     C***                eval      cmzmba = prmfir
070500021009     C***                endif
070600010201     C                   eval      cmznrr = *zeros
070700010201     C                   move      §ctrokvb      cmznrr
070800021018     C                   eval      cmzlba = vlrfl1
070900010201     C                   call(e)   'TIS711C'
071000010201     C                   parm                    dscmz
071100010201     C                   parm      *blanks       esito
071200010201     C                   if        %error
071300010201     C                             or cmzerr = '1'
071400010201     C                             or esito  = '1'
071500010201     C                   eval      wrkesito = '3'
071600010201     C                   endif
071700100525     C                   endif
071800010205     C                   endif
071900990920     C*
072000000613     C                   ENDSR
072100000613     C***
072200070411
072300070411     C     *pssr         BEGSR
072400070411     C*
072500070411     C                   if        %open(tivin00r)
072600070411     C                   close     tivin00r
072700070411     C                   endif
072800070411     C                   if        %open(fivabwwr)
072900070411     C                   close     fivabwwr
073000070411     C                   endif
073100070411     C                   if        %open(fivatwwr)
073200070411     C                   close     fivatwwr
073300070411     C                   endif
073400070411     C*
073500070411     C* Effettuo la chiamata al CLLE preposto
073600100525     C  N51              call(e)   'TITVVTC'
073700100525     C                   parm                    parccm
073800100525     C                   parm                    parmbr
073900100525     C                   parm      '2'           paropz
074000100525     C   51              call(e)   'TITVVBC'
074100100525     C                   parm                    parccm
074200100525     C                   parm                    parmbr
074300100525     C                   parm      '2'           paropz
074400070411     C*
074500070411     C                   eval      wrkesito = '2'
074600070411     C*
074700070411     C                   seton                                        LR
074800070411     C*
074900070411     C                   ENDSR     '*CANCL'
075000070411     C***
075100070411
075200990910
075300000613     C     *inzsr        BEGSR
075400990910     C*
075500990910     C     *entry        plist
075600990920     C                   parm                    tivlrds
075700990921     C                   parm      wrkesito      esito
075800000724     C                   parm                    prmlit
075900000710     C                   parm                    prmfir
076000000613     C*
076100000830     C* CALCOLA LA DATA CORRENTE
076200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076300091223     C                   eval      datcor = %dec(%date() : *ISO)
076400000830     C*
076500000613     C                   ENDSR
076600000613     C***
