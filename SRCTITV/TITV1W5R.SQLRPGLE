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
002100100412     D depspe          s             15    INZ(*blanks)
002200100412     D curspe          s             15    INZ(*blanks)
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
011900100412     C+ vindta, vinflg, substr(vindta, 7, 15) as sped, rrn(tivin00r)
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
013500100310     C                   if        vindta > *blanks
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
014600100412     C                   EVAL      curspe=%trim(%subst(vindta:7:15))
014700010305     C*
014800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014900080923     C                   if        *in50 = *off
015000091223     C                   exsr      repNSP
015100071003     C                   exsr      impvab
015200100412     C                   exsr      wrivab
015300100127     C                   exsr      wrivat_a                                     => carico VAT
015400071003     C                   exsr      wrivat_b                                     => carico VAT
015500071003     C                   else
015600080923     C*
015700071009     C                   if        wDISK = 'D'
015800091223     C                   exsr      repNSP
015900071009     C                   exsr      impvab
016000100412     C                   exsr      wrivab
016100100127     C                   exsr      wrivat_a                                     => carico VAT
016200071009     C                   exsr      wrivat_b                                     => carico VAT
016300071009     C                   exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900091223     C                   exsr      repNSP
017000020305     C                   exsr      impvab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300071003     C   50              exsr      wrivat_e                                     => carico VAT
017400010305     C                   else
017500020725     C                   if        depspe <> curspe                             => rottura di spediz
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700100412     C                   exsr      wrivab
017800080117     C                   clear                   tivinds
017900091223     C                   exsr      repNSP
018000020305     C                   exsr      impvab
018100100127     C                   exsr      wrivat_a                                     => carico VAT
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400020305     C                   else                                                   => x stessa spediz
018500100401     C*
018600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
018700100412     C                   if        wDUPREC <> 'N'
018800020305     C                   exsr      impvab
018900100127     C                   exsr      wrivat_a                                     => carico VAT
019000071003     C                   exsr      wrivat_b                                     => carico VAT
019100071003     C   50              exsr      wrivat_e                                     => carico VAT
019200100401     C                   endif
019300010305     C                   endif
019400010305     C                   endif
019500010305     C                   endif
019600071003     C                   endif
019700071009     C                   endif
019800000905     C*
019900000905     C                   else
020000020725     C                   eval      x_vinflg = '1'
020100000905     C                   endif
020200000905     C*
020300020725     C     VINRRN        chain     tivin000
020400020725     C                   if        %found(tivin00r)
020500020725     C                   eval      y_vinflg = x_vinflg
020600020725     C                   eval      y_vinmsg = x_vinmsg
020700020725     C                   update    tivin000
020800020725     C                   endif
020900020725     C*
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   enddo
021500020725     C*
021600020725     C/EXEC SQL
021700020725     C+ close C1
021800020725     C/END-EXEC
021900000905     C*
022000020305     C* Scarico i VAB rimasti "in sospeso"
022100071009     C                   if        wDISK = 'C'
022200100412     C                   exsr      wrivab
022300071009     C                   endif
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021113     C                   if        %open(fivabwwr)
024300021113     C                   close     fivabwwr
024400990914     C                   endif
024500021113     C                   if        %open(fivatwwr)
024600021113     C                   close     fivatwwr
024700010201     C                   endif
024800990910     C*
024900010201     C                   if        §ctrokvb > 0
025000000724     C                             and vlrpoi <> *zeros
025100010202     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600100412
025700100412
025800010305
025900010305     C*----------------------------------------------------*
026000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026100010305     C*----------------------------------------------------*
026200020305     C     WRIVAB        BEGSR
026300100412     C*
026400100412     C* Gestisco l'eventuale rottura x numero spedizione
026500100412     C                   if        VABRMA = *blanks
026600100412     C                   movel(P)  VABRMN        VABRMA
026700100412     C                   endif
026800100412     C*
026900100412     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800010201     C                   Z-ADD     *zeros        Num5_0
027900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028000020725     C                   MOVEL     '0'           FlgCAS            1
028100000801     C*
028200000801     C                   ENDSR
028300000801     C*----------------------------------------------------*
028400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028500000801     C*----------------------------------------------------*
028600020904     C     DEFCAM        BEGSR
028700080923     C*
028800090310     C                   SETOFF                                       50
028900000801     C*
029000021113     C                   CLEAR                   FIVAB000
029100021113     C                   CLEAR                   FIVAT000
029200070730     C* Imposto i valori di default...
029300100412     C                   EVAL      VABCCM = 0504682
029400100412     C                   EVAL      VATCCM = 0504682
029500100412     C                   EVAL      VABLNP = 050
029600100412     C                   EVAL      VATLNP = 050
029700100412     C                   EVAL      VABCTR = 002
029800070730     C                   EVAL      VABCBO = '1'
029900100309     C                   EVAL      VABVCA = 'EUR'
030000070222     C* ... e poi verifico se sono stati passati come parametri
030100070222     C                   IF        vlrppt > *blanks
030200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABCCM
030700070222     C                   Z-ADD     PiVal         VATCCM
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABLNP
031500070222     C                   Z-ADD     PiVal         VATLNP
031600070222     C                   ENDIF
031700070222     C                   ENDIF
031800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000070222     C                   EXSR      CHKNUM
032100070222     C                   IF        PiInt=*on
032200070222     C                   Z-ADD     PiVal         VABCTR
032300070222     C                   ENDIF
032400070928     C                   ENDIF
032500071009     C                   MOVEL     *blanks       wDISK             1
032600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032700071009     C                   IF        wDISK <> *blanks
032800070928     C                   SETON                                        50
032900070222     C                   ENDIF
033000100401     C                   MOVEL     *blanks       wDUPREC           1
033100100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
033200070222     C                   ENDIF
033300071009     C*
033400071009     C   50              EVAL      VABCTM = '7Q'
033500000801     C*
033600000801     C                   ENDSR
033700091223     C*----------------------------------------------------*
033800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033900091223     C*----------------------------------------------------*
034000091223     C     REPNSP        BEGSR
034100091223     C*
034200091223     C                   EXSR      INZVAR
034300091223     C                   EXSR      DEFCAM
034400091223     C*
034500091223     C* NSP => Stacco un numeratore da AZNUM
034600091223     C                   clear                   TRUL33DS
034700091223     C                   eval      I33OPE = *zeros
034800091223     C                   eval      I33CNU = 302
034900091223     C                   eval      I33NUM = 1
035000091223     C                   movel     TRUL33DS      KPJBU
035100091223     C                   call      'TRUL33R'
035200091223     C                   parm                    KPJBA
035300091223     C                   movel     KPJBU         TRUL33DS
035400091223     C                   if        O33ERR = *zeros
035500091223     C                   z-add     O33NRF        VABNSP
035600091223     C                   z-add     O33NRF        VATNSP
035700091223     C                   else
035800091223     C                   SETON                                        31
035900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036000091223     C                             + ' ' + 'VABNSP VATNSP'
036100091223     C                   endif
036200091223     C*
036300091223     C                   ENDSR
036400000801     C*----------------------------------------------------*
036500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036600000801     C*----------------------------------------------------*
036700010201     C     IMPVAB        BEGSR
036800070530     C*
036900070530     C                   SETOFF                                       3132
037000070928     C*
037100070928     C                   MOVE(P)   vlrpoi        VABFGS
037200070928     C                   MOVE(P)   vlrpoi        VATFGS
037300070928     C*
037400070928     C                   MOVEL     datcor        VABAAS
037500070928     C                   MOVEL     datcor        VATAAS
037600070928     C                   MOVE      datcor        VABMGS
037700100412     C                   EVAL      VABRSD=%trim(%subst(vindta:36:40))
037800070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037900070928     C     '@':'A'       XLATE     VABRSD        VABRSD
038000070928     C* ==
038100100412     C                   EVAL      VABRD2=%trim(%subst(vindta:76:44))
038200100412     C                   EVAL      VABIND=%trim(%subst(vindta:120:40))
038300100412     C                   EVAL      VABNOT=%trim(%subst(vindta:160:40))
038400100412     C                   EVAL      VABLOD=%trim(%subst(vindta:210:40))
038500100412     C                   EVAL      VABCAD=%trim(%subst(vindta:200:10))
038600100412     C                   EVAL      VABRMA=%trim(%subst(vindta:345:7))
038700100309     C* NCL
038800100412     C                   EVAL      PiStr=%trim(%subst(vindta:253:5))
038900100309     C                   EXSR      CHKNUM
039000100309     C                   IF        PiInt=*on
039100100309     C                   Z-ADD     PiVal         VABNCL
039200100309     C                   ELSE
039300100309     C                   SETON                                        32
039400100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039500100309     C                             + ' ' + 'VABNCL'
039600100309     C                   ENDIF
039700100309     C* PKB
039800100412     C                   EVAL      PiStr=%trim(%subst(vindta:266:7))
039900100309     C                   EXSR      CHKNUM
040000100309     C                   IF        PiNum=*on
040100100412     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
040200100412     C                   ADD(H)    PiVal         VABPKB
040300100309     C                   ELSE
040400100309     C                   SETON                                        32
040500100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600100309     C                             + ' ' + 'VABPKB'
040700100309     C                   ENDIF
040800100412     C* VLB
040900100412     C                   EVAL      PiStr=%trim(%subst(vindta:282:5))
041000100412     C                   EXSR      CHKNUM
041100100412     C                   IF        PiNum=*on
041200100412     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
041300100412     C                   ADD(H)    PiVal         VABVLB
041400100412     C                   ELSE
041500100412     C                   SETON                                        32
041600100412     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041700100412     C                             + ' ' + 'VABVLB'
041800100412     C                   ENDIF
041900100412     C* RMN
042000100412     C                   EVAL      PiStr=%trim(%subst(vindta:7:15))
042100080923     C                   EXSR      CHKNUM
042200080923     C                   IF        PiInt=*on
042300080923     C                   Z-ADD     PiVal         VABRMN
042400080923     C                   ELSE
042500080923     C                   SETON                                        32
042600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700080923     C                             + ' ' + 'VABRMN'
042800080923     C                   ENDIF
042900100310     C*
043000100310     C* Se provincia nn valorizzata la reperisco
043100100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
043200100310     C                   IF        VABNZD = *blanks AND
043300100310     C                             VABPRD = *blanks
043400100310     C                   CLEAR                   TISI95DS
043500100310     C                   EVAL      I95TCN = '3'
043600100310     C                   Z-ADD     datcor        I95DAT
043700100310     C                   EVAL      I95NAR = VABNZD
043800100310     C                   EVAL      I95CAP = VABCAD
043900100310     C                   EVAL      I95LOC = VABLOD
044000100310     C                   CALL      'TISI95R'
044100100310     C                   PARM                    TISI95DS
044200100310     C                   EVAL      VABPRD = O95PRV
044300100310     C                   ENDIF
044400070730     C*
044500070730     C* Considerazioni finali su CBO/CAS
044600070730     C                   IF        FlgCAS = '1'
044700070730     C                   IF        VABCBO = '1'
044800070730     C                   EVAL      VABCBO = '4'
044900070730     C                   ENDIF
045000070730     C                   IF        VABCBO = '2'
045100070730     C                   EVAL      VABCBO = '6'
045200070730     C                   ENDIF
045300070730     C                   ENDIF
045400020305     C*
045500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045600011113     C                   EXSR      CHKIMPDIV
045700010202     C*
045800000801     C* Ebbene...
045900000801     C                   ADD       1             §CTRMO
046000070530     C                   IF        *in31 <> *zeros OR
046100070530     C                             *in32 <> *zeros
046200000801     C                   ADD       1             §CTRNO
046300020725     C                   EVAL      x_vinflg = '2'
046400000801     C                   ELSE
046500010201     C                   ADD       1             §CTROKVB
046600000801     C                   ENDIF
046700000801     C*
046800000801     C                   ENDSR
046900100127     C*----------------------------------------------------*
047000100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
047100100127     C*----------------------------------------------------*
047200100127     C     WRIVAT_A      BEGSR
047300100127     C*
047400100127     C* Valorizzo l buffer di scrittura del FIVAT
047500100127     C* - TIPO RECORD "A"
047600100412     C***                eval      VATTRC = 'A'
047700100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
047800100412     C***                exsr      wrivat
047900100127     C*
048000100127     C                   ENDSR
048100010201     C*----------------------------------------------------*
048200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
048300010201     C*----------------------------------------------------*
048400071003     C     WRIVAT_B      BEGSR
048500010201     C*
048600021113     C* Valorizzo l buffer di scrittura del FIVAT
048700070928     C* - TIPO RECORD "B"
048800100412     C***                eval      VATTRC = 'B'
048900100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
049000100412     C***                exsr      wrivat
049100010201     C*
049200010201     C                   ENDSR
049300071003     C*----------------------------------------------------*
049400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
049500071003     C*----------------------------------------------------*
049600071003     C     WRIVAT_E      BEGSR
049700071003     C*
049800071003     C* Valorizzo l buffer di scrittura del FIVAT
049900071003     C* - TIPO RECORD "E"
050000100310     C                   eval      VATTRC = 'E'
050100100412     C                   eval      VATNOT = %trim(%subst(vindta:258:7))
050200100310     C                   exsr      wrivat                                       => scarico VAT
050300071003     C*
050400071003     C                   ENDSR
050500100127     C*----------------------------------------------------*
050600100127     C*  SCARICO BUFFER FIVAT
050700100127     C*----------------------------------------------------*
050800100127     C     WRIVAT        BEGSR
050900100127     C*
051000100127     C                   if        vatNOT <> *blanks
051100100127     C                   ADD       1             §CTROKVT
051200100127     C                   write     FIVAT000
051300100127     C                   endif
051400100127     C*
051500100127     C                   ENDSR
051600020904
051700020904
051800010202     C*----------------------------------------------------*
051900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052000010202     C*----------------------------------------------------*
052100020305     C     PREVAT        BEGSR
052200010202     C*
052300021113     C* Compongo il nome del membro da dare al FIVATWWR
052400010202     C                   eval      parmbr = vlrhdl
052500010202     C                   movel     'M'           parmbr
052600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
052700010202     C                   eval      paropz = '1'
052800010202     C* Effettuo la chiamata al CLLE preposto
052900050128     C                   call(e)   'TITVVTC'
053000010202     C                   parm                    parccm
053100010202     C                   parm                    parmbr
053200010202     C                   parm                    paropz
053300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053400010202     C                   if        %error
053500010202     C                   movel     '1'           chkcall
053600010202     C                   else
053700010202     C                   movel     '0'           chkcall
053800010202     C                   endif
053900010202     C*
054000010202     C                   ENDSR
054100000801     C*----------------------------------------------------*
054200000801     C*  CONTROLLO NUMERICITA' CAMPI
054300000801     C*----------------------------------------------------*
054400000801     C     CHKNUM        BEGSR
054500081113     C*
054600081113     C                   IF        PiDecChr = *blanks
054700091223     C                   EVAL      PiDecChr = ','
054800081113     C                   ENDIF
054900091223     C*
055000081113     C                   callp(e)  UBISNUM_Check(PiStr
055100081113     C                                          :PiDecChr
055200081113     C                                          :PiVal
055300081113     C                                          :PiNum
055400081113     C                                          :PiInt)
055500081113     C*
055600000801     C                   IF        %error
055700000801     C                   EVAL      PiInt=*off
055800000801     C                   ENDIF
055900000801     C*
056000000801     C                   ENDSR
056100000801     C***
056200000801
056300011113
056400011113     C*----------------------------------------------------*
056500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056600011113     C*----------------------------------------------------*
056700011113     C     CHKIMPDIV     BEGSR
056800011113     C*
056900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057000011113     C                   Z-ADD     *zeros        wrkDec            9 9
057100011113     C*
057200011113     C* Come prima cosa effettuo considerazioni sulla divisa
057300011113     C                   IF        vabIAS > *zeros
057400011113     C                   IF        vabVAS <> 'EUR'
057500011113     C                   EVAL      vabVAS =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   IF        vabCAS > *zeros
058000011113     C                   IF        vabVCA <> 'EUR'
058100011113     C                   EVAL      vabVCA =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   IF        vabVMD > *zeros
058600020305     C                   IF        vabVAD <> 'EUR'
058700011113     C                   EVAL      vabVAD =  'ITL'
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059200011113     C                   Z-ADD     vabIAS        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVAS = 'ITL'
059500011113     C                   EVAL      vabIAS = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060000011113     C                   Z-ADD     vabCAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVCA = 'ITL'
060300011113     C                   EVAL      vabCAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060800011113     C                   Z-ADD     vabVMD        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVAD = 'ITL'
061100011113     C                   EVAL      vabVMD = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   ENDSR
061600011113     C***
061700011113
061800011113
061900000801
062000000801
062100990920      /TITLE Invio dei dati al punto operativo.
062200010202     C     invio         BEGSR
062300990920     C*
062400021113     C* 1° invio FIVAT
062500010201     C                   reset                   dscmz
062600010201     C                   move      vlrpoi        cmzdst
062700021113     C                   eval      cmzfld = 'FIVATWWR'
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021113     C                   eval      cmzfla = 'FIVAT00F'
063200021113     C                   eval      cmzmba = 'FIVAT00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800020305     C                   move      §ctrokvt      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010205     C                   if        %error
064400010205     C                             or cmzerr = '1'
064500010205     C                             or esito  = '1'
064600010205     C                   eval      wrkesito = '3'
064700010205     C                   else
064800010201     C*
064900021113     C* 2° invio FIVAB
065000010201     C                   reset                   dscmz
065100010201     C                   move      vlrpoi        cmzdst
065200010201     C                   eval      cmzfld = vlrfou
065300010201     C                   eval      cmzmbd = vlrhdl
065400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065500021009     C***                if        prmfir = *blanks
065600021113     C                   eval      cmzfla = 'FIVAB00F'
065700021113     C                   eval      cmzmba = 'FIVAB00F'
065800021009     C***                else
065900021009     C***                eval      cmzfla = prmfir
066000021009     C***                eval      cmzmba = prmfir
066100021009     C***                endif
066200010201     C                   eval      cmznrr = *zeros
066300010201     C                   move      §ctrokvb      cmznrr
066400021018     C                   eval      cmzlba = vlrfl1
066500010201     C                   call(e)   'TIS711C'
066600010201     C                   parm                    dscmz
066700010201     C                   parm      *blanks       esito
066800010201     C                   if        %error
066900010201     C                             or cmzerr = '1'
067000010201     C                             or esito  = '1'
067100010201     C                   eval      wrkesito = '3'
067200010201     C                   endif
067300010205     C                   endif
067400990920     C*
067500000613     C                   ENDSR
067600000613     C***
067700070411
067800070411     C     *pssr         BEGSR
067900070411     C*
068000070411     C                   if        %open(tivin00r)
068100070411     C                   close     tivin00r
068200070411     C                   endif
068300070411     C                   if        %open(fivabwwr)
068400070411     C                   close     fivabwwr
068500070411     C                   endif
068600070411     C                   if        %open(fivatwwr)
068700070411     C                   close     fivatwwr
068800070411     C                   endif
068900070411     C*
069000070411     C* Effettuo la chiamata al CLLE preposto
069100070411     C                   call(e)   'TITVVTC'
069200070411     C                   parm                    parccm
069300070411     C                   parm                    parmbr
069400070411     C                   parm      '2'           paropz
069500070411     C*
069600070411     C                   eval      wrkesito = '2'
069700070411     C*
069800070411     C                   seton                                        LR
069900070411     C*
070000070411     C                   ENDSR     '*CANCL'
070100070411     C***
070200070411
070300990910
070400000613     C     *inzsr        BEGSR
070500990910     C*
070600990910     C     *entry        plist
070700990920     C                   parm                    tivlrds
070800990921     C                   parm      wrkesito      esito
070900000724     C                   parm                    prmlit
071000000710     C                   parm                    prmfir
071100000613     C*
071200000830     C* CALCOLA LA DATA CORRENTE
071300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071400091223     C                   eval      datcor = %dec(%date() : *ISO)
071500000830     C*
071600000613     C                   ENDSR
071700000613     C***
