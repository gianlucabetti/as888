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
002100110523     D depspe          s              8    INZ(*blanks)
002200110523     D curspe          s              8    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900150319     D w70             s             70
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500100729     D  VINDTA                     2048
003600100729     D  VINFLG                        1
003700110523     D  VINSPE                        8
003800100729     D  VINRRN                        8  0
003900020725     D*
004000081113
004100091223     D*------------------
004200091223     D* DS REPERIMENTO NUMERATORE
004300091223     D*------------------
004400100309     D trul33ds      e ds                  inz
004500091223     D*------------------
004600091223     D* DS ARCHITETTURA
004700091223     D*------------------
004800091223     D kpjba         e ds                  inz
004900091223
005000101119
005100101119     D*-------------------
005200101119     D* COSTANTI
005300101119     D*-------------------
005400101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005500101119     D                                     èéù')
005600101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005700101119     D                                     EEU')
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006300090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400081113
006500990908
006600010201
006700010201
006800080117     C*
006900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000080117     C
007100080117     C/EXEC SQL
007200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300080117     C/END-EXEC
007400080117     C
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000613     C*
007900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300100525     C  N51              call(e)   'TITVVTC'
008400100525     C                   parm                    parccm
008500100525     C                   parm                    parmbr
008600100525     C                   parm      '2'           paropz
008700100525     C   51              call(e)   'TITVVBC'
008800100525     C                   parm                    parccm
008900100525     C                   parm                    parmbr
009000100525     C                   parm      '2'           paropz
009100070730     C*
009200070730     C* Effettuo lancio TISI95 solo x chiusura
009300070730     C                   CLEAR                   TISI95DS
009400070730     C                   EVAL      I95TLA = 'C'
009500070730     C                   CALL      'TISI95R'
009600070730     C                   PARM                    TISI95DS
009700000616     C*
009800000801     C
009900010201     C                   seton                                        LR
010000990908
010100000801
010200910830     C*--------------------------------------------------------
010300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010400910830     C*--------------------------------------------------------
010500070530     C     RWFILE        BEGSR
010600990910     C*
010700990914     C                   if        not %open(tivin00r)
010800990908     C                   open      tivin00r
010900990914     C                   endif
011000070530     C*
011100100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011200100525     C                   exsr      prevasx
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600100525     C                   if        not %open(fivabwwr)
011700100525     C                   open      fivabwwr
011800100525     C                   endif
011900100525     C*
012000021113     C                   if        not %open(fivatwwr)
012100021113     C                   open      fivatwwr
012200010201     C                   endif
012300080117     C*
012400080117     C                   EXSR      INZVAR
012500080117     C                   EXSR      DEFCAM
012600990910     C*
012700010201     C                   clear                   §CTROKVB          5 0
012800020305     C                   clear                   §CTROKVT          5 0
012900000801     C                   clear                   §CTRMO            5 0
013000000801     C                   clear                   §CTRNO            5 0
013100990910     C*
013200020725     C/EXEC SQL
013300020725     C+ declare C1 cursor for select
013400110523     C+ vindta, vinflg, substr(vindta, 51, 8) as sped, rrn(tivin00r)
013500060223     C+ from tivin00r
013600110523     C+ order by sped, substr(vindta, 51, 8) desc
013700020725     C+ for read only
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100020725     C+ open C1
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500070530     C+ Fetch C1 into :INPUT_DS
014600020725     C/END-EXEC
014700020725     C*
014800020725     C                   dow       sqlcod = *zeros
014900990913     C*
015000100310     C                   if        vindta > *blanks
015100000613     C                   add       1             rrnum
015200000801     C*
015300020725     C                   if        vinflg = *blanks
015400020725     C                             or vinflg = '0'
015500020725     C                             or vinflg = '2'
015600000801     C*
015700020725     C                   clear                   x_vinmsg
015800020725     C                   eval      x_vinflg = '1'
015900101119     C*
016000101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016100101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016200010305     C*
016300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016400110523     C                   EVAL      curspe=%trim(%subst(vindta:51:8))
016500010305     C*
016600100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800101119     C                   exsr      inzvar
016900101119     C                   exsr      defcam
017000071003     C                   exsr      impvab
017100100525     C                   exsr      wrivab
017200100127     C                   exsr      wrivat_a                                     => carico VAT
017300071003     C                   exsr      wrivat_b                                     => carico VAT
017400150319     C                   exsr      wrivat_i                                     => carico VAT
017500150319     C                   exsr      wrivat_s                                     => carico VAT
017600100525     C   51              exsr      wrivat_e                                     => carico VAT
017700071003     C                   else
017800080923     C*
017900071009     C                   if        wDISK = 'D'
018000091223     C                   exsr      repNSP
018100071009     C                   exsr      impvab
018200100412     C                   exsr      wrivab
018300100127     C                   exsr      wrivat_a                                     => carico VAT
018400071009     C                   exsr      wrivat_b                                     => carico VAT
018500150319     C                   exsr      wrivat_i                                     => carico VAT
018600150319     C                   exsr      wrivat_s                                     => carico VAT
018700071009     C                   exsr      wrivat_e                                     => carico VAT
018800071009     C                   else
018900071009     C*
019000010305     C                   if        depspe = *blanks                             => 1° giro
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200080117     C                   clear                   tivinds
019300091223     C                   exsr      repNSP
019400020305     C                   exsr      impvab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700150319     C                   exsr      wrivat_i                                     => carico VAT
019800150319     C                   exsr      wrivat_s                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000010305     C                   else
020100020725     C                   if        depspe <> curspe                             => rottura di spediz
020200010305     C                   eval      depspe = curspe                              => memorizz. spediz
020300100412     C                   exsr      wrivab
020400080117     C                   clear                   tivinds
020500091223     C                   exsr      repNSP
020600020305     C                   exsr      impvab
020700100127     C                   exsr      wrivat_a                                     => carico VAT
020800071003     C                   exsr      wrivat_b                                     => carico VAT
020900150319     C                   exsr      wrivat_i                                     => carico VAT
021000150319     C                   exsr      wrivat_s                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200020305     C                   else                                                   => x stessa spediz
021300100401     C*
021400100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021500100928     C                   select
021600100928     C                   when      wDUPREC = ' '
021700020305     C                   exsr      impvab
021800100127     C                   exsr      wrivat_a                                     => carico VAT
021900071003     C                   exsr      wrivat_b                                     => carico VAT
022000150319     C                   exsr      wrivat_i                                     => carico VAT
022100150319     C                   exsr      wrivat_s                                     => carico VAT
022200071003     C   50              exsr      wrivat_e                                     => carico VAT
022300100928     C                   when      wDUPREC = 'C'
022400100928     C   50              exsr      wrivat_e                                     => carico VAT
022500100928     C                   endsl
022600010305     C                   endif
022700010305     C                   endif
022800010305     C                   endif
022900071003     C                   endif
023000110201     C*
023100071009     C                   endif
023200000905     C*
023300000905     C                   else
023400020725     C                   eval      x_vinflg = '1'
023500000905     C                   endif
023600000905     C*
023700020725     C     VINRRN        chain     tivin000
023800020725     C                   if        %found(tivin00r)
023900020725     C                   eval      y_vinflg = x_vinflg
024000020725     C                   eval      y_vinmsg = x_vinmsg
024100020725     C                   update    tivin000
024200020725     C                   endif
024300020725     C*
024400020725     C/EXEC SQL
024500070530     C+ Fetch C1 into :INPUT_DS
024600020725     C/END-EXEC
024700020725     C*
024800020725     C                   enddo
024900020725     C*
025000020725     C/EXEC SQL
025100020725     C+ close C1
025200020725     C/END-EXEC
025300000905     C*
025400020305     C* Scarico i VAB rimasti "in sospeso"
025500071009     C                   if        wDISK = 'C'
025600100412     C                   exsr      wrivab
025700071009     C                   endif
025800010202     C*
025900010202     C                   endif
026000990910
026100990910     C* Se non ci sono record con errori ...
026200000710     C                   if        §ctrno = 0
026300990910     C* ... restituisco esito OK.
026400990921     C                   eval      wrkesito = '0'
026500990910     C                   else
026600100525     C                   if        §ctrokvb > 0 OR
026700100525     C                             §ctrokvt > 0
026800990921     C                   eval      wrkesito = '1'
026900000710     C                   else
027000000710     C                   eval      wrkesito = '2'
027100990910     C                   endif
027200000710     C                   endif
027300990910     C*
027400990914     C                   if        %open(tivin00r)
027500990908     C                   close     tivin00r
027600990914     C                   endif
027700021113     C                   if        %open(fivabwwr)
027800021113     C                   close     fivabwwr
027900990914     C                   endif
028000021113     C                   if        %open(fivatwwr)
028100021113     C                   close     fivatwwr
028200010201     C                   endif
028300990910     C*
028400100525     C                   if        §ctrokvb > 0 OR
028500100525     C                             §ctrokvt > 0
028600000724     C                             and vlrpoi <> *zeros
028700010202     C                   exsr      invio
028800990920     C                   endif
028900990920     C*
029000910830     C                   ENDSR
029100000613     C***
029200100412
029300100412
029400010305
029500010305     C*----------------------------------------------------*
029600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029700010305     C*----------------------------------------------------*
029800020305     C     WRIVAB        BEGSR
029900100412     C*
030000100412     C* Gestisco l'eventuale rottura x numero spedizione
030100100412     C                   if        VABRMA = *blanks
030200100412     C                   movel(P)  VABRMN        VABRMA
030300100412     C                   endif
030400100412     C*
030500100525     C                   if        *in51 = *off and *in31 = *off
030600100525     C                   write     fivab000                                     => scarico il VAB
030700100525     C                   endif
030800010305     C*
030900010305     C                   ENDSR
031000990920
031100000801     C*----------------------------------------------------*
031200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031300000801     C*----------------------------------------------------*
031400010201     C     INZVAR        BEGSR
031500000801     C*
031600010201     C                   Z-ADD     *zeros        Num5_0
031700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031800020725     C                   MOVEL     '0'           FlgCAS            1
031900000801     C*
032000000801     C                   ENDSR
032100000801     C*----------------------------------------------------*
032200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032300000801     C*----------------------------------------------------*
032400020904     C     DEFCAM        BEGSR
032500080923     C*
032600100928     C                   SETOFF                                       505160
032700000801     C*
032800021113     C                   CLEAR                   FIVAB000
032900021113     C                   CLEAR                   FIVAT000
033000070730     C* Imposto i valori di default...
033100110523     C                   EVAL      VABCCM = 1411141
033200110523     C                   EVAL      VATCCM = 1411141
033300110523     C                   EVAL      VABLNP = 141
033400110523     C                   EVAL      VATLNP = 141
033500110128     C                   EVAL      VABCTR = 000
033600100525     C                   EVAL      VABCBO = '1'
033700070222     C* ... e poi verifico se sono stati passati come parametri
033800070222     C                   IF        vlrppt > *blanks
033900100525     C*
034000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034200070222     C                   EXSR      CHKNUM
034300070222     C                   IF        PiInt=*on
034400070222     C                   Z-ADD     PiVal         VABCCM
034500070222     C                   Z-ADD     PiVal         VATCCM
034600070222     C                   ENDIF
034700070222     C                   ENDIF
034800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035000070222     C                   EXSR      CHKNUM
035100070222     C                   IF        PiInt=*on
035200070222     C                   Z-ADD     PiVal         VABLNP
035300070222     C                   Z-ADD     PiVal         VATLNP
035400070222     C                   ENDIF
035500070222     C                   ENDIF
035600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035800070222     C                   EXSR      CHKNUM
035900070222     C                   IF        PiInt=*on
036000070222     C                   Z-ADD     PiVal         VABCTR
036100070222     C                   ENDIF
036200070928     C                   ENDIF
036300071009     C                   MOVEL     *blanks       wDISK             1
036400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036500071009     C                   IF        wDISK <> *blanks
036600070928     C                   SETON                                        50
036700070222     C                   ENDIF
036800100525     C                   IF        wDISK  = 'T'
036900100525     C                   SETOFF                                       50
037000100525     C                   SETON                                        51
037100100525     C                   ENDIF
037200100401     C                   MOVEL     *blanks       wDUPREC           1
037300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037600100525     C                   ENDIF
037700100525     C*
037800070222     C                   ENDIF
037900071009     C*
038000071009     C   50              EVAL      VABCTM = '7Q'
038100000801     C*
038200000801     C                   ENDSR
038300091223     C*----------------------------------------------------*
038400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038500091223     C*----------------------------------------------------*
038600091223     C     REPNSP        BEGSR
038700091223     C*
038800091223     C                   EXSR      INZVAR
038900091223     C                   EXSR      DEFCAM
039000100928     C*
039100100928     C                   SETON                                        60
039200091223     C*
039300091223     C* NSP => Stacco un numeratore da AZNUM
039400091223     C                   clear                   TRUL33DS
039500091223     C                   eval      I33OPE = *zeros
039600091223     C                   eval      I33CNU = 302
039700091223     C                   eval      I33NUM = 1
039800091223     C                   movel     TRUL33DS      KPJBU
039900091223     C                   call      'TRUL33R'
040000091223     C                   parm                    KPJBA
040100091223     C                   movel     KPJBU         TRUL33DS
040200091223     C                   if        O33ERR = *zeros
040300091223     C                   z-add     O33NRF        VABNSP
040400091223     C                   z-add     O33NRF        VATNSP
040500091223     C                   else
040600091223     C                   SETON                                        31
040700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800091223     C                             + ' ' + 'VABNSP VATNSP'
040900091223     C                   endif
041000091223     C*
041100091223     C                   ENDSR
041200000801     C*----------------------------------------------------*
041300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041400000801     C*----------------------------------------------------*
041500100730     C     IMPVAB        BEGSR
041600070530     C*
041700070530     C                   SETOFF                                       3132
041800070928     C*
041900070928     C                   MOVE(P)   vlrpoi        VABFGS
042000070928     C                   MOVE(P)   vlrpoi        VATFGS
042100070928     C*
042200070928     C                   MOVEL     datcor        VABAAS
042300070928     C                   MOVEL     datcor        VATAAS
042400070928     C                   MOVE      datcor        VABMGS
042500100729     C*
042600100729     C* Reperimento campi ALFA
042700110523     C                   EVAL      VABRSD=%trim(%subst(vindta:76:40))
042800110523     C                   EVAL      VABRMA=%trim(%subst(vindta:51:8))
042900110523     C                   EVAL      VABIND=%trim(%subst(vindta:116:30))
043000110523     C                   EVAL      VABLOD=%trim(%subst(vindta:151:30))
043100110523     C                   EVAL      VABPRD=%trim(%subst(vindta:181:2))
043200110523     C                   EVAL      VABCAD=%trim(%subst(vindta:146:5))
043300110523     C                   MOVEL     *blanks       wNOTE           560
043400110523     C                   EVAL      wNOTE = %trim(%subst(vindta:266:70)) +
043500110523     C                                     %trim(%subst(vindta:336:70)) +
043600110523     C                                     %trim(%subst(vindta:406:70)) +
043700110523     C                                     %trim(%subst(vindta:476:70)) +
043800110523     C                                     %trim(%subst(vindta:546:70)) +
043900110523     C                                     %trim(%subst(vindta:616:70)) +
044000110523     C                                     %trim(%subst(vindta:686:70)) +
044100110523     C                                     %trim(%subst(vindta:756:70))
044200110523     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
044300110523     C                   EVAL      VABNT2=%trim(%subst(wNOTE:35+1:35))
044400110523     C* CBO
044500110523     C                   SELECT
044600110523     C                   WHEN      %subst(vindta:207:1) = 'E' OR
044700110523     C                             %subst(vindta:207:1) = 'F'
044800110523     C                   EVAL      VABCBO = '2'
044900110523     C                   WHEN      %subst(vindta:207:1) = 'C' OR
045000110523     C                             %subst(vindta:207:1) = 'D'
045100110523     C                   EVAL      VABCBO = '1'
045200110523     C                   ENDSL
045300150304     C* TC1
045400110523     C                   IF        %scan('AVVIS':wNOTE)  > 1 OR
045500110523     C                             %scan('PRENOT':wNOTE) > 1
045600110523     C                   EVAL      VABTC1 = 'A'
045700110523     C                   ENDIF
045800150304     C* TC2
045900150304     C                   IF        %trim(%subst(vindta:826:1))='A'
046000150304     C                   EVAL      VABTC2 = 'A'
046100150304     C                   ENDIF
046200150304     C* TIC
046300150304     C                   EVAL      VABTIC=%trim(%subst(vindta:827:2))
046400100729     C*
046500100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
046600100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046700100729     C     '@':'A'       XLATE     VABRSD        VABRSD
046800100729     C* ==
046900100729     C*
047000100729     C* Reperimento campi NUMERICI
047100100729     C                   MOVEL     DATCOR        VABAAS
047200100729     C                   MOVE      DATCOR        VABMGS
047300110408     C*
047400110127     C* NSP / RMN
047500101119     C                   IF        *IN51 = *off
047600110523     C                   EVAL      PiStr=%subst(vindta:51:8)
047700100730     C                   EXSR      CHKNUM
047800100730     C                   IF        PiInt=*on
047900100730     C                   Z-ADD     PiVal         VABRMN
048000100928     C  N60              Z-ADD     PiVal         VABNSP
048100100928     C  N60              Z-ADD     PiVal         VATNSP
048200100730     C                   ELSE
048300100730     C                   SETON                                        32
048400100730     C                   Z-ADD     1             VABRMN
048500100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048600101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
048700100730     C                   ENDIF
048800101119     C*
048900101119     C                   ELSE
049000101119     C*
049100110523     C                   EVAL      PiStr=%subst(vindta:51:8)
049200101119     C                   EXSR      CHKNUM
049300101119     C                   IF        PiInt=*on
049400101119     C  N60              Z-ADD     PiVal         VATNSP
049500101119     C                   ELSE
049600101119     C                   SETON                                        32
049700101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800101119     C                             + ' ' + 'VATNSP'
049900101119     C                   ENDIF
050000101119     C                   ENDIF
050100110201     C* NCL
050200110523     C                   EVAL      PiStr=%trim(%subst(vindta:208:6))
050300100928     C                   EXSR      CHKNUM
050400100928     C                   IF        PiInt=*on
050500110325     C                   Z-ADD     PiVal         VABNCL
050600100928     C                   ELSE
050700101119     C  N51              SETON                                        32
050800101119     C  N51              Z-ADD     *zeros        VABNCL
050900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051000100928     C                             + ' ' + 'VABNCL'
051100100928     C                   ENDIF
051200100729     C* PKB
051300110523     C                   IF        %trim(%subst(vindta:214:13)) <> *all'0' AND
051400110523     C                             %trim(%subst(vindta:214:13)) <> *blanks AND
051500110523     C                             %trim(%subst(vindta:214:13)) <> ',0000'
051600110523     C                   EVAL      PiStr=%trim(%subst(vindta:214:13))
051700100729     C                   EXSR      CHKNUM
051800100729     C                   IF        PiNum=*on
051900110325     C                   Z-ADD(H)  PiVal         VABPKB
052000100729     C                   ELSE
052100101119     C  N51              SETON                                        32
052200101119     C  N51              Z-ADD     *zeros        VABPKB
052300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052400100729     C                             + ' ' + 'VABPKB'
052500100729     C                   ENDIF
052600110325     C                   ENDIF
052700110523     C* CAS
052800110713     C                   IF        %trim(%subst(vindta:251:15)) <> *all'0' AND
052900110713     C                             %trim(%subst(vindta:251:15)) <> *blanks AND
053000110713     C                             %trim(%subst(vindta:251:15)) <> ',00'
053100110523     C                   EVAL      FlgCAS = '1'
053200110714     C                   EVAL      VABVCA='EUR'
053300150304     C***                EVAL      VABTIC='CM'
053400110713     C                   EVAL      PiStr='0'+%trim(%subst(vindta:251:15))
053500110523     C                   EXSR      CHKNUM
053600110523     C                   IF        PiNum=*on
053700110523     C                   Z-ADD(H)  PiVal         VABCAS
053800110523     C                   ELSE
053900110523     C  N51              SETON                                        32
054000110523     C  N51              Z-ADD     *zeros        VABCAS
054100110523     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054200110523     C                             + ' ' + 'VABCAS'
054300110523     C                   ENDIF
054400110523     C                   ENDIF
054500100729     C*
054600100729     C* Se provincia nn valorizzata la reperisco
054700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
054800100729     C                   IF        VABNZD  = *blanks AND
054900100729     C                             VABPRD  = *blanks AND
055000100729     C                             VABCAD <> *blanks
055100100729     C                   CLEAR                   TISI95DS
055200100729     C                   EVAL      I95TCN = '3'
055300100729     C                   Z-ADD     datcor        I95DAT
055400100729     C                   EVAL      I95NAR = VABNZD
055500100729     C                   EVAL      I95CAP = VABCAD
055600100729     C                   EVAL      I95LOC = VABLOD
055700100729     C                   CALL      'TISI95R'
055800100729     C                   PARM                    TISI95DS
055900100729     C                   EVAL      VABPRD = O95PRV
056000100729     C                   ENDIF
056100100729     C*
056200100729     C* Considerazioni finali su CBO/CAS
056300100729     C                   IF        FlgCAS = '1'
056400100729     C                   IF        VABCBO = '1'
056500100729     C                   EVAL      VABCBO = '4'
056600100729     C                   ENDIF
056700100729     C                   IF        VABCBO = '2'
056800100729     C                   EVAL      VABCBO = '6'
056900100729     C                   ENDIF
057000100729     C                   ENDIF
057100010202     C*
057200000801     C* Ebbene...
057300000801     C                   ADD       1             §CTRMO
057400070530     C                   IF        *in31 <> *zeros OR
057500070530     C                             *in32 <> *zeros
057600000801     C                   ADD       1             §CTRNO
057700020725     C                   EVAL      x_vinflg = '2'
057800000801     C                   ELSE
057900010201     C                   ADD       1             §CTROKVB
058000000801     C                   ENDIF
058100000801     C*
058200000801     C                   ENDSR
058300100127     C*----------------------------------------------------*
058400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
058500100127     C*----------------------------------------------------*
058600100127     C     WRIVAT_A      BEGSR
058700100127     C*
058800100127     C* Valorizzo l buffer di scrittura del FIVAT
058900100127     C* - TIPO RECORD "A"
059000101119     C***                eval      VATTRC = 'A'
059100101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
059200101119     C***                exsr      wrivat
059300100127     C*
059400100127     C                   ENDSR
059500010201     C*----------------------------------------------------*
059600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059700010201     C*----------------------------------------------------*
059800071003     C     WRIVAT_B      BEGSR
059900010201     C*
060000021113     C* Valorizzo l buffer di scrittura del FIVAT
060100070928     C* - TIPO RECORD "B"
060200110325     C                   eval      VATTRC = 'B'
060300110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
060400110325     C                   exsr      wrivat
060500010201     C*
060600010201     C                   ENDSR
060700150319     C*----------------------------------------------------*
060800150319     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
060900150319     C*----------------------------------------------------*
061000150319     C     WRIVAT_I      BEGSR
061100150319     C*
061200150319     C* Valorizzo il buffer di scrittura del FIVAT
061300150319     C* - TIPO RECORD "I"
061400150319     C                   EVAL      w70=%trim(%subst(vindta:849:70))
061500150319     C                   if        %subst(w70:1:35) <> *blanks
061600150319     C                   eval      VATTRC = 'I'
061700150319     C                   eval      VATNOT = %trim(%subst(w70:1:35))
061800150319     C                   write     FIVAT000
061900150319     C                   endif
062000150319     C                   if        %subst(w70:36:35) <> *blanks
062100150319     C                   eval      VATTRC = 'J'
062200150319     C                   eval      VATNOT = %trim(%subst(w70:36:35))
062300150319     C                   write     FIVAT000
062400150319     C                   endif
062500150319     C*
062600150319     C                   ENDSR
062700150319     C*----------------------------------------------------*
062800150319     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
062900150319     C*----------------------------------------------------*
063000150319     C     WRIVAT_S      BEGSR
063100150319     C*
063200150319     C* Valorizzo il buffer di scrittura del FIVAT
063300150319     C* - TIPO RECORD "S"
063400150319     C                   IF        %trim(%subst(vindta:834:15)) <> *blank
063500150319     C                   eval      VATTRC = 'S'
063600150319     C                   eval      VATNOT = %trim(%subst(vindta:834:15))
063700150319     C                   EVAL      %subst(VATNOT:17:2) = '  '
063800150319     C                   write     FIVAT000
063900150319     C                   endif
064000150319     C*
064100150319     C                   ENDSR
064200071003     C*----------------------------------------------------*
064300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
064400071003     C*----------------------------------------------------*
064500071003     C     WRIVAT_E      BEGSR
064600071003     C*
064700071003     C* Valorizzo l buffer di scrittura del FIVAT
064800071003     C* - TIPO RECORD "E"
064900100729     C*
065000100928     C                   eval      VATTRC = 'E'
065100110325     C                   eval      VATNOT = %trim(%subst(vindta:430:35))
065200100928     C                   exsr      wrivat
065300071003     C*
065400071003     C                   ENDSR
065500100127     C*----------------------------------------------------*
065600100127     C*  SCARICO BUFFER FIVAT
065700100127     C*----------------------------------------------------*
065800100127     C     WRIVAT        BEGSR
065900100127     C*
066000100127     C                   if        vatNOT <> *blanks
066100100127     C                   ADD       1             §CTROKVT
066200100127     C                   write     FIVAT000
066300100127     C                   endif
066400100127     C*
066500100127     C                   ENDSR
066600020904
066700020904
066800010202     C*----------------------------------------------------*
066900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
067000010202     C*----------------------------------------------------*
067100100525     C     PREVASX       BEGSR
067200100525     C*
067300021113     C* Compongo il nome del membro da dare al FIVATWWR
067400010202     C                   eval      parmbr = vlrhdl
067500010202     C                   movel     'M'           parmbr
067600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
067700010202     C                   eval      paropz = '1'
067800100525     C*
067900010202     C* Effettuo la chiamata al CLLE preposto
068000100525     C  N51              call(e)   'TITVVTC'
068100010202     C                   parm                    parccm
068200010202     C                   parm                    parmbr
068300010202     C                   parm                    paropz
068400100525     C   51              call(e)   'TITVVBC'
068500100525     C                   parm                    parccm
068600100525     C                   parm                    parmbr
068700100525     C                   parm                    paropz
068800100525     C*
068900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069000010202     C                   if        %error
069100010202     C                   movel     '1'           chkcall
069200010202     C                   else
069300010202     C                   movel     '0'           chkcall
069400010202     C                   endif
069500010202     C*
069600010202     C                   ENDSR
069700000801     C*----------------------------------------------------*
069800000801     C*  CONTROLLO NUMERICITA' CAMPI
069900000801     C*----------------------------------------------------*
070000000801     C     CHKNUM        BEGSR
070100081113     C*
070200081113     C                   IF        PiDecChr = *blanks
070300110714     C                   EVAL      PiDecChr = ','
070400081113     C                   ENDIF
070500091223     C*
070600081113     C                   callp(e)  UBISNUM_Check(PiStr
070700081113     C                                          :PiDecChr
070800081113     C                                          :PiVal
070900081113     C                                          :PiNum
071000081113     C                                          :PiInt)
071100081113     C*
071200000801     C                   IF        %error
071300000801     C                   EVAL      PiInt=*off
071400000801     C                   ENDIF
071500000801     C*
071600000801     C                   ENDSR
071700000801     C***
071800011113
071900011113
072000000801
072100000801
072200990920      /TITLE Invio dei dati al punto operativo.
072300010202     C     invio         BEGSR
072400990920     C*
072500021113     C* 1° invio FIVAT
072600010201     C                   reset                   dscmz
072700110411     C                   move      vatfgs        cmzdst
072800021113     C                   eval      cmzfld = 'FIVATWWR'
072900010201     C                   eval      cmzmbd = vlrhdl
073000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073100021009     C***                if        prmfir = *blanks
073200021113     C                   eval      cmzfla = 'FIVAT00F'
073300021113     C                   eval      cmzmba = 'FIVAT00F'
073400021009     C***                else
073500021009     C***                eval      cmzfla = prmfir
073600021009     C***                eval      cmzmba = prmfir
073700021009     C***                endif
073800010201     C                   eval      cmznrr = *zeros
073900020305     C                   move      §ctrokvt      cmznrr
074000021018     C                   eval      cmzlba = vlrfl1
074100010201     C                   call(e)   'TIS711C'
074200010201     C                   parm                    dscmz
074300010201     C                   parm      *blanks       esito
074400010205     C                   if        %error
074500010205     C                             or cmzerr = '1'
074600010205     C                             or esito  = '1'
074700010205     C                   eval      wrkesito = '3'
074800010205     C                   else
074900010201     C*
075000021113     C* 2° invio FIVAB
075100100525     C                   if        *in51 = *off
075200010201     C                   reset                   dscmz
075300110411     C                   move      vabfgs        cmzdst
075400010201     C                   eval      cmzfld = vlrfou
075500010201     C                   eval      cmzmbd = vlrhdl
075600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075700021009     C***                if        prmfir = *blanks
075800021113     C                   eval      cmzfla = 'FIVAB00F'
075900021113     C                   eval      cmzmba = 'FIVAB00F'
076000021009     C***                else
076100021009     C***                eval      cmzfla = prmfir
076200021009     C***                eval      cmzmba = prmfir
076300021009     C***                endif
076400010201     C                   eval      cmznrr = *zeros
076500010201     C                   move      §ctrokvb      cmznrr
076600021018     C                   eval      cmzlba = vlrfl1
076700010201     C                   call(e)   'TIS711C'
076800010201     C                   parm                    dscmz
076900010201     C                   parm      *blanks       esito
077000010201     C                   if        %error
077100010201     C                             or cmzerr = '1'
077200010201     C                             or esito  = '1'
077300010201     C                   eval      wrkesito = '3'
077400010201     C                   endif
077500100525     C                   endif
077600010205     C                   endif
077700990920     C*
077800000613     C                   ENDSR
077900000613     C***
078000070411
078100070411     C     *pssr         BEGSR
078200070411     C*
078300070411     C                   if        %open(tivin00r)
078400070411     C                   close     tivin00r
078500070411     C                   endif
078600070411     C                   if        %open(fivabwwr)
078700070411     C                   close     fivabwwr
078800070411     C                   endif
078900070411     C                   if        %open(fivatwwr)
079000070411     C                   close     fivatwwr
079100070411     C                   endif
079200070411     C*
079300070411     C* Effettuo la chiamata al CLLE preposto
079400100525     C  N51              call(e)   'TITVVTC'
079500100525     C                   parm                    parccm
079600100525     C                   parm                    parmbr
079700100525     C                   parm      '2'           paropz
079800100525     C   51              call(e)   'TITVVBC'
079900100525     C                   parm                    parccm
080000100525     C                   parm                    parmbr
080100100525     C                   parm      '2'           paropz
080200070411     C*
080300070411     C                   eval      wrkesito = '2'
080400070411     C*
080500070411     C                   seton                                        LR
080600070411     C*
080700070411     C                   ENDSR     '*CANCL'
080800070411     C***
080900070411
081000990910
081100000613     C     *inzsr        BEGSR
081200990910     C*
081300990910     C     *entry        plist
081400990920     C                   parm                    tivlrds
081500990921     C                   parm      wrkesito      esito
081600000724     C                   parm                    prmlit
081700000710     C                   parm                    prmfir
081800000613     C*
081900000830     C* CALCOLA LA DATA CORRENTE
082000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082100091223     C                   eval      datcor = %dec(%date() : *ISO)
082200000830     C*
082300000613     C                   ENDSR
082400000613     C***
