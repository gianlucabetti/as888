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
002100111110     D depspe          s              6    INZ(*blanks)
002200111110     D curspe          s              6    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900180108     D w70             s             70
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500100729     D  VINDTA                     2048
003600100729     D  VINFLG                        1
003700111110     D  VINSPE                        6
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
013400111110     C+ vindta, vinflg, substr(vindta, 5, 6) as sped, rrn(tivin00r)
013500060223     C+ from tivin00r
013600111020     C+ order by sped
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
016400111110     C                   EVAL      curspe=%trim(%subst(vindta:5:6))
016500010305     C*
016600100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800101119     C                   exsr      inzvar
016900101119     C                   exsr      defcam
017000071003     C                   exsr      impvab
017100100525     C                   exsr      wrivab
017200100127     C                   exsr      wrivat_a                                     => carico VAT
017300071003     C                   exsr      wrivat_b                                     => carico VAT
017400180108     C                   exsr      wrivat_s                                     => carico VAT
017500100525     C   51              exsr      wrivat_e                                     => carico VAT
017600180108     C                   exsr      wrivat_ij                                    => carico VAT
017700071003     C                   else
017800080923     C*
017900071009     C                   if        wDISK = 'D'
018000091223     C                   exsr      repNSP
018100071009     C                   exsr      impvab
018200100412     C                   exsr      wrivab
018300100127     C                   exsr      wrivat_a                                     => carico VAT
018400071009     C                   exsr      wrivat_b                                     => carico VAT
018500180108     C                   exsr      wrivat_s                                     => carico VAT
018600071009     C                   exsr      wrivat_e                                     => carico VAT
018700180108     C                   exsr      wrivat_ij                                    => carico VAT
018800071009     C                   else
018900071009     C*
019000010305     C                   if        depspe = *blanks                             => 1° giro
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200080117     C                   clear                   tivinds
019300091223     C                   exsr      repNSP
019400020305     C                   exsr      impvab
019500100127     C                   exsr      wrivat_a                                     => carico VAT
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700180108     C                   exsr      wrivat_s                                     => carico VAT
019800071003     C   50              exsr      wrivat_e                                     => carico VAT
019900180108     C                   exsr      wrivat_ij                                    => carico VAT
020000010305     C                   else
020100020725     C                   if        depspe <> curspe                             => rottura di spediz
020200010305     C                   eval      depspe = curspe                              => memorizz. spediz
020300100412     C                   exsr      wrivab
020400080117     C                   clear                   tivinds
020500091223     C                   exsr      repNSP
020600020305     C                   exsr      impvab
020700100127     C                   exsr      wrivat_a                                     => carico VAT
020800071003     C                   exsr      wrivat_b                                     => carico VAT
020900180108     C                   exsr      wrivat_s                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100180108     C                   exsr      wrivat_ij                                    => carico VAT
021200020305     C                   else                                                   => x stessa spediz
021300100401     C*
021400100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021500100928     C                   select
021600100928     C                   when      wDUPREC = ' '
021700020305     C                   exsr      impvab
021800100127     C                   exsr      wrivat_a                                     => carico VAT
021900071003     C                   exsr      wrivat_b                                     => carico VAT
022000180108     C                   exsr      wrivat_s                                     => carico VAT
022100071003     C   50              exsr      wrivat_e                                     => carico VAT
022200180108     C                   exsr      wrivat_ij                                    => carico VAT
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
033100111110     C                   EVAL      VABCCM = 0056194
033200111110     C                   EVAL      VATCCM = 0056194
033300111110     C                   EVAL      VABLNP = 005
033400111110     C                   EVAL      VATLNP = 005
033500111110     C                   EVAL      VABCTR = 000
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
042700111128     C                   EVAL      VABRSD=%trim(%subst(vindta:312:35))
042800111129     C                   EVAL      VABRD2=%trim(%subst(vindta:347:50))
042900111129     C                   EVAL      VABIND=%trim(%subst(vindta:397:35))
043000111129     C                   EVAL      VABLOD=%trim(%subst(vindta:492:35))
043100111129     C                   EVAL      VABCAD=%trim(%subst(vindta:482:5))
043200111129     C                   EVAL      VABPRD=%trim(%subst(vindta:527:2))
043300111110     C                   EVAL      VABRMA=%trim(%subst(vindta:5:6))
043400110801     C*
043500110801     C                   SELECT
043600111129     C                   WHEN      %subst(vindta:248:2) = '01'
043700111110     C                   EVAL      VABCBO='2'
043800111129     C                   WHEN      %subst(vindta:248:2) = '02'
043900111110     C                   EVAL      VABCBO='1'
044000110801     C                   ENDSL
044100100729     C*
044200100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044300100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044400100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044500100729     C* ==
044600100729     C*
044700100729     C* Reperimento campi NUMERICI
044800100729     C                   MOVEL     DATCOR        VABAAS
044900100729     C                   MOVE      DATCOR        VABMGS
045000110408     C*
045100111020     C* NSP
045200101119     C                   IF        *IN51 = *off
045300111110     C                   EVAL      PiStr=%subst(vindta:5:6)
045400100730     C                   EXSR      CHKNUM
045500100730     C                   IF        PiInt=*on
045600100928     C  N60              Z-ADD     PiVal         VABNSP
045700100928     C  N60              Z-ADD     PiVal         VATNSP
045800100730     C                   ELSE
045900100730     C                   SETON                                        32
046000100730     C                   Z-ADD     1             VABRMN
046100100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200111020     C                             + ' ' + 'VABNSP VATNSP'
046300100730     C                   ENDIF
046400101119     C*
046500101119     C                   ELSE
046600101119     C*
046700111110     C                   EVAL      PiStr=%subst(vindta:5:6)
046800101119     C                   EXSR      CHKNUM
046900101119     C                   IF        PiInt=*on
047000101119     C  N60              Z-ADD     PiVal         VATNSP
047100101119     C                   ELSE
047200101119     C                   SETON                                        32
047300101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400101119     C                             + ' ' + 'VATNSP'
047500101119     C                   ENDIF
047600101119     C                   ENDIF
047700111020     C* RMN
047800111110     C                   EVAL      PiStr=%subst(vindta:5:6)
047900111020     C                   EXSR      CHKNUM
048000111020     C                   IF        PiInt=*on
048100111020     C                   Z-ADD     PiVal         VABRMN
048200111020     C                   ELSE
048300111020     C  N51              SETON                                        32
048400111020     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048500111020     C                             + ' ' + 'VABRMN'
048600111020     C                   ENDIF
048700110801     C* NCL
048800111129     C                   EVAL      PiStr=%subst(vindta:305:7)
048900110801     C                   EXSR      CHKNUM
049000110801     C                   IF        PiInt=*on
049100110801     C                   ADD       PiVal         VABNCL
049200110801     C                   ELSE
049300110801     C  N51              SETON                                        32
049400111020     C  N51              Z-ADD     *zeros        VABNCL
049500110801     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049600110801     C                             + ' ' + 'VABNCL'
049700110801     C                   ENDIF
049800100729     C* PKB
049900111129     C                   EVAL      PiStr=%trim(%subst(vindta:287:9))
050000100729     C                   EXSR      CHKNUM
050100100729     C                   IF        PiNum=*on
050200111110     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
050300110627     C                   ADD(H)    PiVal         VABPKB
050400100729     C                   ELSE
050500101119     C  N51              SETON                                        32
050600101119     C  N51              Z-ADD     *zeros        VABPKB
050700101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050800100729     C                             + ' ' + 'VABPKB'
050900100729     C                   ENDIF
051000100729     C*
051100100729     C* Se provincia nn valorizzata la reperisco
051200100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
051300100729     C                   IF        VABNZD  = *blanks AND
051400100729     C                             VABPRD  = *blanks AND
051500100729     C                             VABCAD <> *blanks
051600100729     C                   CLEAR                   TISI95DS
051700100729     C                   EVAL      I95TCN = '3'
051800100729     C                   Z-ADD     datcor        I95DAT
051900100729     C                   EVAL      I95NAR = VABNZD
052000100729     C                   EVAL      I95CAP = VABCAD
052100100729     C                   EVAL      I95LOC = VABLOD
052200100729     C                   CALL      'TISI95R'
052300100729     C                   PARM                    TISI95DS
052400100729     C                   EVAL      VABPRD = O95PRV
052500100729     C                   ENDIF
052600100729     C*
052700100729     C* Considerazioni finali su CBO/CAS
052800100729     C                   IF        FlgCAS = '1'
052900100729     C                   IF        VABCBO = '1'
053000100729     C                   EVAL      VABCBO = '4'
053100100729     C                   ENDIF
053200100729     C                   IF        VABCBO = '2'
053300100729     C                   EVAL      VABCBO = '6'
053400100729     C                   ENDIF
053500100729     C                   ENDIF
053600010202     C*
053700000801     C* Ebbene...
053800000801     C                   ADD       1             §CTRMO
053900070530     C                   IF        *in31 <> *zeros OR
054000070530     C                             *in32 <> *zeros
054100000801     C                   ADD       1             §CTRNO
054200020725     C                   EVAL      x_vinflg = '2'
054300000801     C                   ELSE
054400010201     C                   ADD       1             §CTROKVB
054500000801     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800100127     C*----------------------------------------------------*
054900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
055000100127     C*----------------------------------------------------*
055100100127     C     WRIVAT_A      BEGSR
055200100127     C*
055300100127     C* Valorizzo l buffer di scrittura del FIVAT
055400100127     C* - TIPO RECORD "A"
055500101119     C***                eval      VATTRC = 'A'
055600101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
055700101119     C***                exsr      wrivat
055800100127     C*
055900100127     C                   ENDSR
056000010201     C*----------------------------------------------------*
056100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056200010201     C*----------------------------------------------------*
056300071003     C     WRIVAT_B      BEGSR
056400010201     C*
056500021113     C* Valorizzo l buffer di scrittura del FIVAT
056600070928     C* - TIPO RECORD "B"
056700110627     C***                eval      VATTRC = 'B'
056800110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
056900110627     C***                exsr      wrivat
057000010201     C*
057100010201     C                   ENDSR
057200180108     C*----------------------------------------------------*
057300180108     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
057400180108     C*----------------------------------------------------*
057500180108     C     WRIVAT_IJ     BEGSR
057600180108     C*
057700180108     C* Valorizzo l buffer di scrittura del FIVAT
057800180108     C* - TIPO RECORD "I"
057900180108     C                   if        %subst(vindta:529:70) <> *blanks
058000180108     C                   eval      w70 = %trim(%subst(vindta:529:70))
058100180108     C                   eval      VATTRC = 'I'
058200180108     C                   eval      VATNOT = %subst(w70:1:35)
058300180108     C                   write     FIVAT000
058400180108     C                   IF        %subst(w70:36:35) <> *blank
058500180108     C                   eval      VATTRC = 'J'
058600180108     C                   eval      VATNOT = %subst(w70:36:35)
058700180108     C                   write     FIVAT000
058800180108     C                   endif
058900180108     C                   endif
059000180108     C*
059100180108     C                   ENDSR
059200180108     C*----------------------------------------------------*
059300180108     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
059400180108     C*----------------------------------------------------*
059500180108     C     WRIVAT_S      BEGSR
059600180108     C*
059700180108     C* Valorizzo l buffer di scrittura del FIVAT
059800180108     C* - TIPO RECORD "S"
059900180108     C                   eval      VATTRC = 'S'
060000180108     C                   eval      VATNOT = %trim(%subst(vindta:599:16))
060100180108     C                   EVAL      %subst(VATNOT:17:2) = 'SS'
060200180108     C                   exsr      wrivat
060300180108     C*
060400180108     C                   ENDSR
060500071003     C*----------------------------------------------------*
060600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
060700071003     C*----------------------------------------------------*
060800071003     C     WRIVAT_E      BEGSR
060900071003     C*
061000071003     C* Valorizzo l buffer di scrittura del FIVAT
061100071003     C* - TIPO RECORD "E"
061200100729     C*
061300100928     C                   eval      VATTRC = 'E'
061400111110     C                   eval      VATNOT = %trim(%subst(vindta:5:6))
061500111110     C***                exsr      wrivat
061600111110     C*
061700111110     C* Sviluppati i "CHI SONO" in relazione al numero colli
061800111110     C*
061900111110     C* Inizializzo progressivo collo su totale colli della spedizione
062000121221     C                   Z-ADD     1             wNumColloDA       5 0
062100111110     C                   MOVEL     *blanks       wNumColloCor      2
062200111110     C                   MOVEL     *blanks       wWhoIs            6
062300111110     C                   eval      wWhoIs = %trim(%subst(vindta:5:6))
062400111110     C*
062500111110     C                   DOW       wNumColloDA <= VABNCL
062600121221     C***                MOVEL(P)  wNumColloDA   wNumColloCor
062700121221     C                   MOVE(P)   wNumColloDA   wNumColloCor
062800111110     C                   EVAL      VATNOT =
062900111110     C                                %subst(%char(%dec(%date() : *ISO)):3:2) +
063000111110     C                                wNumColloCor + wWhoIs
063100111110     C                   exsr      wrivat
063200111110     C                   add       1             §CTROKVT
063300111110     C*
063400111110     C                   EVAL      wNumColloDA = wNumColloDA + 1
063500111110     C                   ENDDO
063600071003     C*
063700071003     C                   ENDSR
063800100127     C*----------------------------------------------------*
063900100127     C*  SCARICO BUFFER FIVAT
064000100127     C*----------------------------------------------------*
064100100127     C     WRIVAT        BEGSR
064200100127     C*
064300100127     C                   if        vatNOT <> *blanks
064400100127     C                   ADD       1             §CTROKVT
064500100127     C                   write     FIVAT000
064600100127     C                   endif
064700100127     C*
064800100127     C                   ENDSR
064900020904
065000020904
065100010202     C*----------------------------------------------------*
065200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
065300010202     C*----------------------------------------------------*
065400100525     C     PREVASX       BEGSR
065500100525     C*
065600021113     C* Compongo il nome del membro da dare al FIVATWWR
065700010202     C                   eval      parmbr = vlrhdl
065800010202     C                   movel     'M'           parmbr
065900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066000010202     C                   eval      paropz = '1'
066100100525     C*
066200010202     C* Effettuo la chiamata al CLLE preposto
066300100525     C  N51              call(e)   'TITVVTC'
066400010202     C                   parm                    parccm
066500010202     C                   parm                    parmbr
066600010202     C                   parm                    paropz
066700100525     C   51              call(e)   'TITVVBC'
066800100525     C                   parm                    parccm
066900100525     C                   parm                    parmbr
067000100525     C                   parm                    paropz
067100100525     C*
067200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067300010202     C                   if        %error
067400010202     C                   movel     '1'           chkcall
067500010202     C                   else
067600010202     C                   movel     '0'           chkcall
067700010202     C                   endif
067800010202     C*
067900010202     C                   ENDSR
068000000801     C*----------------------------------------------------*
068100000801     C*  CONTROLLO NUMERICITA' CAMPI
068200000801     C*----------------------------------------------------*
068300000801     C     CHKNUM        BEGSR
068400081113     C*
068500081113     C                   IF        PiDecChr = *blanks
068600110408     C                   EVAL      PiDecChr = '.'
068700081113     C                   ENDIF
068800091223     C*
068900081113     C                   callp(e)  UBISNUM_Check(PiStr
069000081113     C                                          :PiDecChr
069100081113     C                                          :PiVal
069200081113     C                                          :PiNum
069300081113     C                                          :PiInt)
069400081113     C*
069500000801     C                   IF        %error
069600000801     C                   EVAL      PiInt=*off
069700000801     C                   ENDIF
069800000801     C*
069900000801     C                   ENDSR
070000000801     C***
070100011113
070200011113
070300000801
070400000801
070500990920      /TITLE Invio dei dati al punto operativo.
070600010202     C     invio         BEGSR
070700990920     C*
070800021113     C* 1° invio FIVAT
070900010201     C                   reset                   dscmz
071000110411     C                   move      vatfgs        cmzdst
071100021113     C                   eval      cmzfld = 'FIVATWWR'
071200010201     C                   eval      cmzmbd = vlrhdl
071300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071400021009     C***                if        prmfir = *blanks
071500021113     C                   eval      cmzfla = 'FIVAT00F'
071600021113     C                   eval      cmzmba = 'FIVAT00F'
071700021009     C***                else
071800021009     C***                eval      cmzfla = prmfir
071900021009     C***                eval      cmzmba = prmfir
072000021009     C***                endif
072100010201     C                   eval      cmznrr = *zeros
072200020305     C                   move      §ctrokvt      cmznrr
072300021018     C                   eval      cmzlba = vlrfl1
072400010201     C                   call(e)   'TIS711C'
072500010201     C                   parm                    dscmz
072600010201     C                   parm      *blanks       esito
072700010205     C                   if        %error
072800010205     C                             or cmzerr = '1'
072900010205     C                             or esito  = '1'
073000010205     C                   eval      wrkesito = '3'
073100010205     C                   else
073200010201     C*
073300021113     C* 2° invio FIVAB
073400100525     C                   if        *in51 = *off
073500010201     C                   reset                   dscmz
073600110411     C                   move      vabfgs        cmzdst
073700010201     C                   eval      cmzfld = vlrfou
073800010201     C                   eval      cmzmbd = vlrhdl
073900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074000021009     C***                if        prmfir = *blanks
074100021113     C                   eval      cmzfla = 'FIVAB00F'
074200021113     C                   eval      cmzmba = 'FIVAB00F'
074300021009     C***                else
074400021009     C***                eval      cmzfla = prmfir
074500021009     C***                eval      cmzmba = prmfir
074600021009     C***                endif
074700010201     C                   eval      cmznrr = *zeros
074800010201     C                   move      §ctrokvb      cmznrr
074900021018     C                   eval      cmzlba = vlrfl1
075000010201     C                   call(e)   'TIS711C'
075100010201     C                   parm                    dscmz
075200010201     C                   parm      *blanks       esito
075300010201     C                   if        %error
075400010201     C                             or cmzerr = '1'
075500010201     C                             or esito  = '1'
075600010201     C                   eval      wrkesito = '3'
075700010201     C                   endif
075800100525     C                   endif
075900010205     C                   endif
076000990920     C*
076100000613     C                   ENDSR
076200000613     C***
076300070411
076400110627     C     *pssr         BEGSR
076500070411     C*
076600110627     C                   if        %open(tivin00r)
076700110627     C                   close     tivin00r
076800110627     C                   endif
076900110627     C                   if        %open(fivabwwr)
077000110627     C                   close     fivabwwr
077100110627     C                   endif
077200110627     C                   if        %open(fivatwwr)
077300110627     C                   close     fivatwwr
077400110627     C                   endif
077500110627     C*
077600110627     C* Effettuo la chiamata al CLLE preposto
077700110627     C  N51              call(e)   'TITVVTC'
077800110627     C                   parm                    parccm
077900110627     C                   parm                    parmbr
078000110627     C                   parm      '2'           paropz
078100110627     C   51              call(e)   'TITVVBC'
078200110627     C                   parm                    parccm
078300110627     C                   parm                    parmbr
078400110627     C                   parm      '2'           paropz
078500110627     C*
078600110627     C                   eval      wrkesito = '2'
078700110627     C*
078800110627     C                   seton                                        LR
078900110627     C*
079000110627     C                   ENDSR     '*CANCL'
079100070411     C***
079200070411
079300990910
079400000613     C     *inzsr        BEGSR
079500990910     C*
079600990910     C     *entry        plist
079700990920     C                   parm                    tivlrds
079800990921     C                   parm      wrkesito      esito
079900000724     C                   parm                    prmlit
080000000710     C                   parm                    prmfir
080100000613     C*
080200000830     C* CALCOLA LA DATA CORRENTE
080300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080400091223     C                   eval      datcor = %dec(%date() : *ISO)
080500000830     C*
080600000613     C                   ENDSR
080700000613     C***
