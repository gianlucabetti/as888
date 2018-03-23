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
002100110408     D depspe          s             10    INZ(*blanks)
002200110408     D curspe          s             10    INZ(*blanks)
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
003400100729     D  VINDTA                     2048
003500100729     D  VINFLG                        1
003600110408     D  VINSPE                       10
003700100729     D  VINRRN                        8  0
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
004900101119
005000101119     D*-------------------
005100101119     D* COSTANTI
005200101119     D*-------------------
005300101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005400101119     D                                     èéù')
005500101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005600101119     D                                     EEU')
005700081113
005800081113     D*------------------
005900081113     D* LINKING A DEFINIZIONI ESTERNE
006000081113     D*------------------
006100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006200090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113
006400990908
006500010201
006600010201
006700080117     C*
006800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006900080117     C
007000080117     C/EXEC SQL
007100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007200080117     C/END-EXEC
007300080117     C
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000613     C*
007800100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008000000613     C*
008100010202     C* Effettuo la chiamata al CLLE preposto
008200100525     C  N51              call(e)   'TITVVTC'
008300100525     C                   parm                    parccm
008400100525     C                   parm                    parmbr
008500100525     C                   parm      '2'           paropz
008600100525     C   51              call(e)   'TITVVBC'
008700100525     C                   parm                    parccm
008800100525     C                   parm                    parmbr
008900100525     C                   parm      '2'           paropz
009000070730     C*
009100070730     C* Effettuo lancio TISI95 solo x chiusura
009200070730     C                   CLEAR                   TISI95DS
009300070730     C                   EVAL      I95TLA = 'C'
009400070730     C                   CALL      'TISI95R'
009500070730     C                   PARM                    TISI95DS
009600000616     C*
009700000801     C
009800010201     C                   seton                                        LR
009900990908
010000000801
010100910830     C*--------------------------------------------------------
010200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010300910830     C*--------------------------------------------------------
010400070530     C     RWFILE        BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900070530     C*
011000100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011100100525     C                   exsr      prevasx
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500100525     C                   if        not %open(fivabwwr)
011600100525     C                   open      fivabwwr
011700100525     C                   endif
011800100525     C*
011900021113     C                   if        not %open(fivatwwr)
012000021113     C                   open      fivatwwr
012100010201     C                   endif
012200080117     C*
012300080117     C                   EXSR      INZVAR
012400080117     C                   EXSR      DEFCAM
012500990910     C*
012600010201     C                   clear                   §CTROKVB          5 0
012700020305     C                   clear                   §CTROKVT          5 0
012800000801     C                   clear                   §CTRMO            5 0
012900000801     C                   clear                   §CTRNO            5 0
013000990910     C*
013100020725     C/EXEC SQL
013200020725     C+ declare C1 cursor for select
013300110408     C+ vindta, vinflg, substr(vindta, 1, 10) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110408     C+ order by sped, substr(vindta, 1, 10) desc
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
015800101119     C*
015900101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016000101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016100010305     C*
016200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016300110408     C                   EVAL      curspe=%trim(%subst(vindta:1:10))
016400010305     C*
016500100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016600080923     C                   if        *in50 = *off
016700101119     C                   exsr      inzvar
016800101119     C                   exsr      defcam
016900071003     C                   exsr      impvab
017000100525     C                   exsr      wrivab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300100525     C   51              exsr      wrivat_e                                     => carico VAT
017400071003     C                   else
017500080923     C*
017600071009     C                   if        wDISK = 'D'
017700091223     C                   exsr      repNSP
017800071009     C                   exsr      impvab
017900100412     C                   exsr      wrivab
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071009     C                   exsr      wrivat_b                                     => carico VAT
018200071009     C                   exsr      wrivat_e                                     => carico VAT
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800091223     C                   exsr      repNSP
018900020305     C                   exsr      impvab
019000100127     C                   exsr      wrivat_a                                     => carico VAT
019100071003     C                   exsr      wrivat_b                                     => carico VAT
019200071003     C   50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   else
019400020725     C                   if        depspe <> curspe                             => rottura di spediz
019500010305     C                   eval      depspe = curspe                              => memorizz. spediz
019600100412     C                   exsr      wrivab
019700080117     C                   clear                   tivinds
019800091223     C                   exsr      repNSP
019900020305     C                   exsr      impvab
020000100127     C                   exsr      wrivat_a                                     => carico VAT
020100071003     C                   exsr      wrivat_b                                     => carico VAT
020200071003     C   50              exsr      wrivat_e                                     => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400100401     C*
020500100401     C* Se nn richiesta esclusione spedizioni "duplicate"
020600100928     C                   select
020700100928     C                   when      wDUPREC = ' '
020800020305     C                   exsr      impvab
020900100127     C                   exsr      wrivat_a                                     => carico VAT
021000071003     C                   exsr      wrivat_b                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200100928     C                   when      wDUPREC = 'C'
021300100928     C   50              exsr      wrivat_e                                     => carico VAT
021400100928     C                   endsl
021500010305     C                   endif
021600010305     C                   endif
021700010305     C                   endif
021800071003     C                   endif
021900110201     C*
022000071009     C                   endif
022100000905     C*
022200000905     C                   else
022300020725     C                   eval      x_vinflg = '1'
022400000905     C                   endif
022500000905     C*
022600020725     C     VINRRN        chain     tivin000
022700020725     C                   if        %found(tivin00r)
022800020725     C                   eval      y_vinflg = x_vinflg
022900020725     C                   eval      y_vinmsg = x_vinmsg
023000020725     C                   update    tivin000
023100020725     C                   endif
023200020725     C*
023300020725     C/EXEC SQL
023400070530     C+ Fetch C1 into :INPUT_DS
023500020725     C/END-EXEC
023600020725     C*
023700020725     C                   enddo
023800020725     C*
023900020725     C/EXEC SQL
024000020725     C+ close C1
024100020725     C/END-EXEC
024200000905     C*
024300020305     C* Scarico i VAB rimasti "in sospeso"
024400071009     C                   if        wDISK = 'C'
024500100412     C                   exsr      wrivab
024600071009     C                   endif
024700010202     C*
024800010202     C                   endif
024900990910
025000990910     C* Se non ci sono record con errori ...
025100000710     C                   if        §ctrno = 0
025200990910     C* ... restituisco esito OK.
025300990921     C                   eval      wrkesito = '0'
025400990910     C                   else
025500100525     C                   if        §ctrokvb > 0 OR
025600100525     C                             §ctrokvt > 0
025700990921     C                   eval      wrkesito = '1'
025800000710     C                   else
025900000710     C                   eval      wrkesito = '2'
026000990910     C                   endif
026100000710     C                   endif
026200990910     C*
026300990914     C                   if        %open(tivin00r)
026400990908     C                   close     tivin00r
026500990914     C                   endif
026600021113     C                   if        %open(fivabwwr)
026700021113     C                   close     fivabwwr
026800990914     C                   endif
026900021113     C                   if        %open(fivatwwr)
027000021113     C                   close     fivatwwr
027100010201     C                   endif
027200990910     C*
027300100525     C                   if        §ctrokvb > 0 OR
027400100525     C                             §ctrokvt > 0
027500000724     C                             and vlrpoi <> *zeros
027600150130     C                   exsr      invio
027700990920     C                   endif
027800990920     C*
027900910830     C                   ENDSR
028000000613     C***
028100100412
028200100412
028300010305
028400010305     C*----------------------------------------------------*
028500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028600010305     C*----------------------------------------------------*
028700020305     C     WRIVAB        BEGSR
028800100412     C*
028900100412     C* Gestisco l'eventuale rottura x numero spedizione
029000100412     C                   if        VABRMA = *blanks
029100100412     C                   movel(P)  VABRMN        VABRMA
029200100412     C                   endif
029300100412     C*
029400100525     C                   if        *in51 = *off and *in31 = *off
029500100525     C                   write     fivab000                                     => scarico il VAB
029600100525     C                   endif
029700010305     C*
029800010305     C                   ENDSR
029900990920
030000000801     C*----------------------------------------------------*
030100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200000801     C*----------------------------------------------------*
030300010201     C     INZVAR        BEGSR
030400000801     C*
030500010201     C                   Z-ADD     *zeros        Num5_0
030600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030700020725     C                   MOVEL     '0'           FlgCAS            1
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031200000801     C*----------------------------------------------------*
031300020904     C     DEFCAM        BEGSR
031400080923     C*
031500100928     C                   SETOFF                                       505160
031600000801     C*
031700021113     C                   CLEAR                   FIVAB000
031800021113     C                   CLEAR                   FIVAT000
031900070730     C* Imposto i valori di default...
032000110408     C                   EVAL      VABCCM = 2022829
032100110408     C                   EVAL      VATCCM = 2022829
032200110408     C                   EVAL      VABLNP = 202
032300110408     C                   EVAL      VATLNP = 202
032400110128     C                   EVAL      VABCTR = 000
032500110408     C                   EVAL      VABCTM = '7A'
032600100525     C                   EVAL      VABCBO = '1'
032700070222     C* ... e poi verifico se sono stati passati come parametri
032800070222     C                   IF        vlrppt > *blanks
032900100525     C*
033000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033200070222     C                   EXSR      CHKNUM
033300070222     C                   IF        PiInt=*on
033400070222     C                   Z-ADD     PiVal         VABCCM
033500070222     C                   Z-ADD     PiVal         VATCCM
033600070222     C                   ENDIF
033700070222     C                   ENDIF
033800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034000070222     C                   EXSR      CHKNUM
034100070222     C                   IF        PiInt=*on
034200070222     C                   Z-ADD     PiVal         VABLNP
034300070222     C                   Z-ADD     PiVal         VATLNP
034400070222     C                   ENDIF
034500070222     C                   ENDIF
034600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABCTR
035100070222     C                   ENDIF
035200070928     C                   ENDIF
035300071009     C                   MOVEL     *blanks       wDISK             1
035400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035500071009     C                   IF        wDISK <> *blanks
035600070928     C                   SETON                                        50
035700070222     C                   ENDIF
035800100525     C                   IF        wDISK  = 'T'
035900100525     C                   SETOFF                                       50
036000100525     C                   SETON                                        51
036100100525     C                   ENDIF
036200100401     C                   MOVEL     *blanks       wDUPREC           1
036300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036600100525     C                   ENDIF
036700100525     C*
036800070222     C                   ENDIF
036900071009     C*
037000071009     C   50              EVAL      VABCTM = '7Q'
037100000801     C*
037200000801     C                   ENDSR
037300091223     C*----------------------------------------------------*
037400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037500091223     C*----------------------------------------------------*
037600091223     C     REPNSP        BEGSR
037700091223     C*
037800091223     C                   EXSR      INZVAR
037900091223     C                   EXSR      DEFCAM
038000100928     C*
038100100928     C                   SETON                                        60
038200091223     C*
038300091223     C* NSP => Stacco un numeratore da AZNUM
038400091223     C                   clear                   TRUL33DS
038500091223     C                   eval      I33OPE = *zeros
038600091223     C                   eval      I33CNU = 302
038700091223     C                   eval      I33NUM = 1
038800091223     C                   movel     TRUL33DS      KPJBU
038900091223     C                   call      'TRUL33R'
039000091223     C                   parm                    KPJBA
039100091223     C                   movel     KPJBU         TRUL33DS
039200091223     C                   if        O33ERR = *zeros
039300091223     C                   z-add     O33NRF        VABNSP
039400091223     C                   z-add     O33NRF        VATNSP
039500091223     C                   else
039600091223     C                   SETON                                        31
039700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039800091223     C                             + ' ' + 'VABNSP VATNSP'
039900091223     C                   endif
040000091223     C*
040100091223     C                   ENDSR
040200000801     C*----------------------------------------------------*
040300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040400000801     C*----------------------------------------------------*
040500100730     C     IMPVAB        BEGSR
040600070530     C*
040700070530     C                   SETOFF                                       3132
040800070928     C*
040900070928     C                   MOVE(P)   vlrpoi        VABFGS
041000070928     C                   MOVE(P)   vlrpoi        VATFGS
041100070928     C*
041200070928     C                   MOVEL     datcor        VABAAS
041300070928     C                   MOVEL     datcor        VATAAS
041400070928     C                   MOVE      datcor        VABMGS
041500100729     C*
041600100729     C* Reperimento campi ALFA
041700110408     C                   EVAL      VABRSD=%trim(%subst(vindta:19:35))
041800110408     C                   EVAL      VABRD2=%trim(%subst(vindta:19+35:40-35))
041900110408     C                   EVAL      VABRMA=%trim(%subst(vindta:1:10))
042000110408     C                   EVAL      VABIND=%trim(%subst(vindta:59:40))
042100110408     C                   EVAL      VABLOD=%trim(%subst(vindta:109:35))
042200110408     C                   EVAL      VABPRD=%trim(%subst(vindta:144:2))
042300110408     C                   EVAL      VABCAD=%trim(%subst(vindta:99:5))
042400110408     C                   EVAL      VABNOT=%trim(%subst(vindta:178:35))
042500110408     C                   EVAL      VABNT2=%trim(%subst(vindta:178+35:75-35))
042600110503     C                   EVAL      VABTSP=%trim(%subst(vindta:262:1))
042700100729     C*
042800100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
042900100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043000100729     C     '@':'A'       XLATE     VABRSD        VABRSD
043100100729     C* ==
043200100729     C*
043300100729     C* Reperimento campi NUMERICI
043400100729     C                   MOVEL     DATCOR        VABAAS
043500100729     C                   MOVE      DATCOR        VABMGS
043600110408     C* CCM
043700110408     C                   EVAL      PiStr=%trim(%subst(vindta:247:7))
043800110408     C                   EXSR      CHKNUM
043900110408     C                   IF        PiInt=*on
044000110408     C                   Z-ADD     PiVal         VABCCM
044100110408     C                   Z-ADD     PiVal         VATCCM
044200110408     C                   ELSE
044300110408     C  N51              SETON                                        32
044400110408     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
044500110408     C                             + ' ' + 'VABCCM VATCCM'
044600110408     C                   ENDIF
044700110408     C*
044800110408     C                   SELECT
044900110408     C                   WHEN      VABCCM = 2022829
045000110408     C                   EVAL      VABLNP = 202
045100110408     C                   EVAL      VABFGS = 202
045200110411     C                   EVAL      VATFGS = 202
045300110408     C                   EVAL      VABCTR = 000
045400110408     C                   WHEN      VABCCM = 1660846
045500110408     C                   EVAL      VABLNP = 166
045600110408     C                   EVAL      VABFGS = 166
045700110411     C                   EVAL      VATFGS = 166
045800110408     C                   EVAL      VABCTR = 000
045900110408     C                   WHEN      VABCCM = 1162029
046000110408     C                   EVAL      VABLNP = 116
046100110408     C                   EVAL      VABFGS = 116
046200110411     C                   EVAL      VATFGS = 116
046300110408     C                   EVAL      VABCTR = 000
046400150119     C                   WHEN      VABCCM = 0057864
046500150119     C                   EVAL      VABLNP = 005
046600150119     C                   EVAL      VABFGS = 005
046700150119     C                   EVAL      VATFGS = 005
046800150119     C                   EVAL      VABCTR = 000
046900150119     C                   WHEN      VABCCM = 0057865
047000150119     C                   EVAL      VABLNP = 005
047100150119     C                   EVAL      VABFGS = 005
047200150119     C                   EVAL      VATFGS = 005
047300150119     C                   EVAL      VABCTR = 000
047400110408     C                   ENDSL
047500110408     C*
047600110127     C* NSP / RMN
047700101119     C                   IF        *IN51 = *off
047800110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
047900110412     C                                   %subst(vindta:6:5)
048000100730     C                   EXSR      CHKNUM
048100100730     C                   IF        PiInt=*on
048200100730     C                   Z-ADD     PiVal         VABRMN
048300100928     C  N60              Z-ADD     PiVal         VABNSP
048400100928     C  N60              Z-ADD     PiVal         VATNSP
048500100730     C                   ELSE
048600100730     C                   SETON                                        32
048700100730     C                   Z-ADD     1             VABRMN
048800100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
049000100730     C                   ENDIF
049100101119     C*
049200101119     C                   ELSE
049300101119     C*
049400110412     C                   EVAL      PiStr=%subst(vindta:1:2) +
049500110412     C                                   %subst(vindta:6:5)
049600101119     C                   EXSR      CHKNUM
049700101119     C                   IF        PiInt=*on
049800101119     C  N60              Z-ADD     PiVal         VATNSP
049900101119     C                   ELSE
050000101119     C                   SETON                                        32
050100101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200101119     C                             + ' ' + 'VATNSP'
050300101119     C                   ENDIF
050400101119     C                   ENDIF
050500110201     C* NCL
050600110408     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
050700100928     C                   EXSR      CHKNUM
050800100928     C                   IF        PiInt=*on
050900110325     C                   Z-ADD     PiVal         VABNCL
051000100928     C                   ELSE
051100101119     C  N51              SETON                                        32
051200101119     C  N51              Z-ADD     *zeros        VABNCL
051300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051400100928     C                             + ' ' + 'VABNCL'
051500100928     C                   ENDIF
051600100729     C* PKB
051700110408     C                   IF        %subst(vindta:155:8) <> *all'0' AND
051800110408     C                             %subst(vindta:155:8) <> *blanks
051900110408     C                   EVAL      PiStr=%trim(%subst(vindta:155:8))
052000100729     C                   EXSR      CHKNUM
052100100729     C                   IF        PiNum=*on
052200110325     C                   Z-ADD(H)  PiVal         VABPKB
052300100729     C                   ELSE
052400101119     C  N51              SETON                                        32
052500101119     C  N51              Z-ADD     *zeros        VABPKB
052600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052700100729     C                             + ' ' + 'VABPKB'
052800100729     C                   ENDIF
052900110325     C                   ENDIF
053000110325     C* VLB
053100110408     C                   IF        %subst(vindta:165:8) <> *all'0' AND
053200110408     C                             %subst(vindta:165:8) <> *blanks
053300110408     C                   EVAL      PiStr=%trim(%subst(vindta:165:8))
053400110325     C                   EXSR      CHKNUM
053500110325     C                   IF        PiNum=*on
053600110325     C                   Z-ADD(H)  PiVal         VABVLB
053700110325     C                   ELSE
053800110325     C  N51              SETON                                        32
053900110325     C  N51              Z-ADD     *zeros        VABVLB
054000110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054100110325     C                             + ' ' + 'VABVLB'
054200110325     C                   ENDIF
054300110325     C                   ENDIF
054400100729     C*
054500100729     C* Se provincia nn valorizzata la reperisco
054600100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
054700100729     C                   IF        VABNZD  = *blanks AND
054800100729     C                             VABPRD  = *blanks AND
054900100729     C                             VABCAD <> *blanks
055000100729     C                   CLEAR                   TISI95DS
055100100729     C                   EVAL      I95TCN = '3'
055200100729     C                   Z-ADD     datcor        I95DAT
055300100729     C                   EVAL      I95NAR = VABNZD
055400100729     C                   EVAL      I95CAP = VABCAD
055500100729     C                   EVAL      I95LOC = VABLOD
055600100729     C                   CALL      'TISI95R'
055700100729     C                   PARM                    TISI95DS
055800100729     C                   EVAL      VABPRD = O95PRV
055900100729     C                   ENDIF
056000110503     C* DCR
056100110503     C                   IF        %subst(vindta:254:8) <> *all'0' AND
056200110503     C                             %subst(vindta:254:8) <> *blanks
056300110503     C                   EVAL      PiStr=%trim(%subst(vindta:254:8))
056400110503     C                   EXSR      CHKNUM
056500110503     C                   IF        PiInt=*on
056600110503     C                   Z-ADD(H)  PiVal         VABDCR
056700110503     C                   ELSE
056800110503     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056900110503     C                             + ' ' + 'VABDCR'
057000110503     C                   ENDIF
057100110503     C                   ENDIF
057200100729     C*
057300100729     C* Considerazioni finali su CBO/CAS
057400100729     C                   IF        FlgCAS = '1'
057500100729     C                   IF        VABCBO = '1'
057600100729     C                   EVAL      VABCBO = '4'
057700100729     C                   ENDIF
057800100729     C                   IF        VABCBO = '2'
057900100729     C                   EVAL      VABCBO = '6'
058000100729     C                   ENDIF
058100100729     C                   ENDIF
058200010202     C*
058300000801     C* Ebbene...
058400000801     C                   ADD       1             §CTRMO
058500070530     C                   IF        *in31 <> *zeros OR
058600070530     C                             *in32 <> *zeros
058700000801     C                   ADD       1             §CTRNO
058800020725     C                   EVAL      x_vinflg = '2'
058900000801     C                   ELSE
059000010201     C                   ADD       1             §CTROKVB
059100000801     C                   ENDIF
059200000801     C*
059300000801     C                   ENDSR
059400100127     C*----------------------------------------------------*
059500100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059600100127     C*----------------------------------------------------*
059700100127     C     WRIVAT_A      BEGSR
059800100127     C*
059900100127     C* Valorizzo l buffer di scrittura del FIVAT
060000100127     C* - TIPO RECORD "A"
060100101119     C***                eval      VATTRC = 'A'
060200101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
060300101119     C***                exsr      wrivat
060400100127     C*
060500100127     C                   ENDSR
060600010201     C*----------------------------------------------------*
060700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060800010201     C*----------------------------------------------------*
060900071003     C     WRIVAT_B      BEGSR
061000010201     C*
061100021113     C* Valorizzo l buffer di scrittura del FIVAT
061200070928     C* - TIPO RECORD "B"
061300110325     C                   eval      VATTRC = 'B'
061400110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
061500110325     C                   exsr      wrivat
061600010201     C*
061700010201     C                   ENDSR
061800071003     C*----------------------------------------------------*
061900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062000071003     C*----------------------------------------------------*
062100071003     C     WRIVAT_E      BEGSR
062200071003     C*
062300071003     C* Valorizzo l buffer di scrittura del FIVAT
062400071003     C* - TIPO RECORD "E"
062500100729     C*
062600100928     C                   eval      VATTRC = 'E'
062700110325     C                   eval      VATNOT = %trim(%subst(vindta:430:35))
062800100928     C                   exsr      wrivat
062900071003     C*
063000071003     C                   ENDSR
063100100127     C*----------------------------------------------------*
063200100127     C*  SCARICO BUFFER FIVAT
063300100127     C*----------------------------------------------------*
063400100127     C     WRIVAT        BEGSR
063500100127     C*
063600100127     C                   if        vatNOT <> *blanks
063700100127     C                   ADD       1             §CTROKVT
063800100127     C                   write     FIVAT000
063900100127     C                   endif
064000100127     C*
064100100127     C                   ENDSR
064200020904
064300020904
064400010202     C*----------------------------------------------------*
064500100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
064600010202     C*----------------------------------------------------*
064700100525     C     PREVASX       BEGSR
064800100525     C*
064900021113     C* Compongo il nome del membro da dare al FIVATWWR
065000010202     C                   eval      parmbr = vlrhdl
065100010202     C                   movel     'M'           parmbr
065200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065300010202     C                   eval      paropz = '1'
065400100525     C*
065500010202     C* Effettuo la chiamata al CLLE preposto
065600100525     C  N51              call(e)   'TITVVTC'
065700010202     C                   parm                    parccm
065800010202     C                   parm                    parmbr
065900010202     C                   parm                    paropz
066000100525     C   51              call(e)   'TITVVBC'
066100100525     C                   parm                    parccm
066200100525     C                   parm                    parmbr
066300100525     C                   parm                    paropz
066400100525     C*
066500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066600010202     C                   if        %error
066700010202     C                   movel     '1'           chkcall
066800010202     C                   else
066900010202     C                   movel     '0'           chkcall
067000010202     C                   endif
067100010202     C*
067200010202     C                   ENDSR
067300000801     C*----------------------------------------------------*
067400000801     C*  CONTROLLO NUMERICITA' CAMPI
067500000801     C*----------------------------------------------------*
067600000801     C     CHKNUM        BEGSR
067700081113     C*
067800081113     C                   IF        PiDecChr = *blanks
067900110408     C                   EVAL      PiDecChr = '.'
068000081113     C                   ENDIF
068100091223     C*
068200081113     C                   callp(e)  UBISNUM_Check(PiStr
068300081113     C                                          :PiDecChr
068400081113     C                                          :PiVal
068500081113     C                                          :PiNum
068600081113     C                                          :PiInt)
068700081113     C*
068800000801     C                   IF        %error
068900000801     C                   EVAL      PiInt=*off
069000000801     C                   ENDIF
069100000801     C*
069200000801     C                   ENDSR
069300000801     C***
069400011113
069500011113
069600000801
069700000801
069800990920      /TITLE Invio dei dati al punto operativo.
069900010202     C     invio         BEGSR
070000990920     C*
070100021113     C* 1° invio FIVAT
070200010201     C                   reset                   dscmz
070300150130     C                   if        vlrpoi = 999
070400110411     C                   move      vatfgs        cmzdst
070500150130     C                   else
070600150130     C                   move      vlrpoi        cmzdst
070700150130     C                   endif
070800021113     C                   eval      cmzfld = 'FIVATWWR'
070900010201     C                   eval      cmzmbd = vlrhdl
071000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071100021009     C***                if        prmfir = *blanks
071200021113     C                   eval      cmzfla = 'FIVAT00F'
071300021113     C                   eval      cmzmba = 'FIVAT00F'
071400021009     C***                else
071500021009     C***                eval      cmzfla = prmfir
071600021009     C***                eval      cmzmba = prmfir
071700021009     C***                endif
071800010201     C                   eval      cmznrr = *zeros
071900020305     C                   move      §ctrokvt      cmznrr
072000021018     C                   eval      cmzlba = vlrfl1
072100010201     C                   call(e)   'TIS711C'
072200010201     C                   parm                    dscmz
072300010201     C                   parm      *blanks       esito
072400010205     C                   if        %error
072500010205     C                             or cmzerr = '1'
072600010205     C                             or esito  = '1'
072700010205     C                   eval      wrkesito = '3'
072800010205     C                   else
072900010201     C*
073000021113     C* 2° invio FIVAB
073100100525     C                   if        *in51 = *off
073200010201     C                   reset                   dscmz
073300150130     C                   if        vlrpoi = 999
073400150130     C                   move      vabfgs        cmzdst
073500150130     C                   else
073600150130     C                   move      vlrpoi        cmzdst
073700150130     C                   endif
073800010201     C                   eval      cmzfld = vlrfou
073900010201     C                   eval      cmzmbd = vlrhdl
074000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074100021009     C***                if        prmfir = *blanks
074200021113     C                   eval      cmzfla = 'FIVAB00F'
074300021113     C                   eval      cmzmba = 'FIVAB00F'
074400021009     C***                else
074500021009     C***                eval      cmzfla = prmfir
074600021009     C***                eval      cmzmba = prmfir
074700021009     C***                endif
074800010201     C                   eval      cmznrr = *zeros
074900010201     C                   move      §ctrokvb      cmznrr
075000021018     C                   eval      cmzlba = vlrfl1
075100010201     C                   call(e)   'TIS711C'
075200010201     C                   parm                    dscmz
075300010201     C                   parm      *blanks       esito
075400010201     C                   if        %error
075500010201     C                             or cmzerr = '1'
075600010201     C                             or esito  = '1'
075700010201     C                   eval      wrkesito = '3'
075800010201     C                   endif
075900100525     C                   endif
076000010205     C                   endif
076100990920     C*
076200000613     C                   ENDSR
076300000613     C***
076400070411
076500070411     C     *pssr         BEGSR
076600070411     C*
076700070411     C                   if        %open(tivin00r)
076800070411     C                   close     tivin00r
076900070411     C                   endif
077000070411     C                   if        %open(fivabwwr)
077100070411     C                   close     fivabwwr
077200070411     C                   endif
077300070411     C                   if        %open(fivatwwr)
077400070411     C                   close     fivatwwr
077500070411     C                   endif
077600070411     C*
077700070411     C* Effettuo la chiamata al CLLE preposto
077800100525     C  N51              call(e)   'TITVVTC'
077900100525     C                   parm                    parccm
078000100525     C                   parm                    parmbr
078100100525     C                   parm      '2'           paropz
078200100525     C   51              call(e)   'TITVVBC'
078300100525     C                   parm                    parccm
078400100525     C                   parm                    parmbr
078500100525     C                   parm      '2'           paropz
078600070411     C*
078700070411     C                   eval      wrkesito = '2'
078800070411     C*
078900070411     C                   seton                                        LR
079000070411     C*
079100070411     C                   ENDSR     '*CANCL'
079200070411     C***
079300070411
079400990910
079500000613     C     *inzsr        BEGSR
079600990910     C*
079700990910     C     *entry        plist
079800990920     C                   parm                    tivlrds
079900990921     C                   parm      wrkesito      esito
080000000724     C                   parm                    prmlit
080100000710     C                   parm                    prmfir
080200000613     C*
080300000830     C* CALCOLA LA DATA CORRENTE
080400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080500091223     C                   eval      datcor = %dec(%date() : *ISO)
080600000830     C*
080700000613     C                   ENDSR
080800000613     C***
