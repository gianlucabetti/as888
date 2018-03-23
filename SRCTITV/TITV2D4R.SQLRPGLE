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
002100110407     D depspe          s             15    INZ(*blanks)
002200110407     D curspe          s             15    INZ(*blanks)
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
003600110407     D  VINSPE                       15
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
013300110407     C+ vindta, vinflg, substr(vindta, 25, 15) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110407     C+ order by sped, substr(vindta, 25, 15) desc
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
016300110407     C                   EVAL      curspe=%trim(%subst(vindta:25:15))
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
027600010202     C                   exsr      invio
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
032000110325     C                   EVAL      VABCCM = 0494115
032100110325     C                   EVAL      VATCCM = 0494115
032200110325     C                   EVAL      VABLNP = 049
032300110325     C                   EVAL      VATLNP = 049
032400110128     C                   EVAL      VABCTR = 000
032500100525     C                   EVAL      VABCBO = '1'
032600070222     C* ... e poi verifico se sono stati passati come parametri
032700070222     C                   IF        vlrppt > *blanks
032800100525     C*
032900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033100070222     C                   EXSR      CHKNUM
033200070222     C                   IF        PiInt=*on
033300070222     C                   Z-ADD     PiVal         VABCCM
033400070222     C                   Z-ADD     PiVal         VATCCM
033500070222     C                   ENDIF
033600070222     C                   ENDIF
033700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033900070222     C                   EXSR      CHKNUM
034000070222     C                   IF        PiInt=*on
034100070222     C                   Z-ADD     PiVal         VABLNP
034200070222     C                   Z-ADD     PiVal         VATLNP
034300070222     C                   ENDIF
034400070222     C                   ENDIF
034500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034700070222     C                   EXSR      CHKNUM
034800070222     C                   IF        PiInt=*on
034900120419     C                   Z-ADD     PiVal         VABCTR
035000070222     C                   ENDIF
035100070928     C                   ENDIF
035200071009     C                   MOVEL     *blanks       wDISK             1
035300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035400071009     C                   IF        wDISK <> *blanks
035500070928     C                   SETON                                        50
035600070222     C                   ENDIF
035700100525     C                   IF        wDISK  = 'T'
035800100525     C                   SETOFF                                       50
035900100525     C                   SETON                                        51
036000100525     C                   ENDIF
036100100401     C                   MOVEL     *blanks       wDUPREC           1
036200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036500100525     C                   ENDIF
036600120419     C                   IF        %subst(vlrppt:18:3) <> *blanks
036700120419     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:3))
036800120419     C                   EXSR      CHKNUM
036900120419     C                   IF        PiInt=*on
037000120419     C                   Z-ADD     PiVal         wCTR_EEX          3 0
037100120419     C                   ENDIF
037200120419     C                   ENDIF
037300120419     C                   IF        %subst(vlrppt:21:3) <> *blanks
037400120419     C                   EVAL      PiStr=%trim(%subst(vlrppt:21:3))
037500120419     C                   EXSR      CHKNUM
037600120419     C                   IF        PiInt=*on
037700120419     C                   Z-ADD     PiVal         wCTR_DPD          3 0
037800120419     C                   ENDIF
037900120419     C                   ENDIF
038000100525     C*
038100070222     C                   ENDIF
038200071009     C*
038300071009     C   50              EVAL      VABCTM = '7Q'
038400000801     C*
038500000801     C                   ENDSR
038600091223     C*----------------------------------------------------*
038700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038800091223     C*----------------------------------------------------*
038900091223     C     REPNSP        BEGSR
039000091223     C*
039100091223     C                   EXSR      INZVAR
039200091223     C                   EXSR      DEFCAM
039300100928     C*
039400100928     C                   SETON                                        60
039500091223     C*
039600091223     C* NSP => Stacco un numeratore da AZNUM
039700091223     C                   clear                   TRUL33DS
039800091223     C                   eval      I33OPE = *zeros
039900091223     C                   eval      I33CNU = 302
040000091223     C                   eval      I33NUM = 1
040100091223     C                   movel     TRUL33DS      KPJBU
040200091223     C                   call      'TRUL33R'
040300091223     C                   parm                    KPJBA
040400091223     C                   movel     KPJBU         TRUL33DS
040500091223     C                   if        O33ERR = *zeros
040600091223     C                   z-add     O33NRF        VABNSP
040700091223     C                   z-add     O33NRF        VATNSP
040800091223     C                   else
040900091223     C                   SETON                                        31
041000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041100091223     C                             + ' ' + 'VABNSP VATNSP'
041200091223     C                   endif
041300091223     C*
041400091223     C                   ENDSR
041500000801     C*----------------------------------------------------*
041600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041700000801     C*----------------------------------------------------*
041800100730     C     IMPVAB        BEGSR
041900070530     C*
042000070530     C                   SETOFF                                       3132
042100070928     C*
042200070928     C                   MOVE(P)   vlrpoi        VABFGS
042300070928     C                   MOVE(P)   vlrpoi        VATFGS
042400070928     C*
042500070928     C                   MOVEL     datcor        VABAAS
042600070928     C                   MOVEL     datcor        VATAAS
042700070928     C                   MOVE      datcor        VABMGS
042800100729     C*
042900100729     C* Reperimento campi ALFA
043000110325     C                   EVAL      VABRSD=%trim(%subst(vindta:236:40))
043100110325     C                   EVAL      VABIND=%trim(%subst(vindta:276:60))
043200110325     C                   EVAL      VABLOD=%trim(%subst(vindta:345:30))
043300110325     C                   EVAL      VABPRD=%trim(%subst(vindta:375:2))
043400110325     C                   EVAL      VABCAD=%trim(%subst(vindta:336:9))
043500110929     C                   EVAL      VABNT2='Cliente: ' +
043600110929     C                                    %trim(%subst(vindta:470:10))
043700110331     C* CBO
043800110127     C                   SELECT
043900110325     C                   WHEN      %subst(vindta:60:2) = '02'
044000110325     C                   EVAL      VABCBO = '2'
044100110127     C                   ENDSL
044200110929     C* TSP
044300110929     C                   SELECT
044400110929     C                   WHEN      %subst(vindta:390:3) = '1'
044500110930     C                   EVAL      VABTSP = 'C'
044600110929     C                   WHEN      %subst(vindta:390:3) = '2'
044700110930     C                   EVAL      VABTSP = 'H'
044800110929     C                   WHEN      %subst(vindta:390:3) = '3'
044900110930     C                   EVAL      VABTSP = 'E'
045000110929     C                   ENDSL
045100110331     C* FFD
045200110331     C                   SELECT
045300110331     C                   WHEN      %subst(vindta:429:1) = '1'
045400110331     C                   EVAL      VABFFD = 'S'
045500110331     C                   ENDSL
045600110127     C*
045700110325     C                   EVAL      VABRMA=%trim(%subst(vindta:25:15))
045800100729     C*
045900100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
046000100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100100729     C     '@':'A'       XLATE     VABRSD        VABRSD
046200100729     C* ==
046300100729     C*
046400100729     C* Reperimento campi NUMERICI
046500100729     C                   MOVEL     DATCOR        VABAAS
046600100729     C                   MOVE      DATCOR        VABMGS
046700110127     C* NSP / RMN
046800101119     C                   IF        *IN51 = *off
046900110325     C                   EVAL      PiStr=%trim(%subst(vindta:50:10))
047000100730     C                   EXSR      CHKNUM
047100100730     C                   IF        PiInt=*on
047200100730     C                   Z-ADD     PiVal         VABRMN
047300100928     C  N60              Z-ADD     PiVal         VABNSP
047400100928     C  N60              Z-ADD     PiVal         VATNSP
047500100730     C                   ELSE
047600100730     C                   SETON                                        32
047700100730     C                   Z-ADD     1             VABRMN
047800100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047900101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
048000100730     C                   ENDIF
048100101119     C*
048200101119     C                   ELSE
048300101119     C*
048400110407     C                   EVAL      PiStr=%trim(%subst(vindta:50:10))
048500101119     C                   EXSR      CHKNUM
048600101119     C                   IF        PiInt=*on
048700101119     C  N60              Z-ADD     PiVal         VATNSP
048800101119     C                   ELSE
048900101119     C                   SETON                                        32
049000101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100101119     C                             + ' ' + 'VATNSP'
049200101119     C                   ENDIF
049300101119     C                   ENDIF
049400110201     C* NCL
049500110325     C                   EVAL      PiStr=%trim(%subst(vindta:409:5))
049600100928     C                   EXSR      CHKNUM
049700100928     C                   IF        PiInt=*on
049800110325     C                   Z-ADD     PiVal         VABNCL
049900100928     C                   ELSE
050000101119     C  N51              SETON                                        32
050100101119     C  N51              Z-ADD     *zeros        VABNCL
050200101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050300100928     C                             + ' ' + 'VABNCL'
050400100928     C                   ENDIF
050500100729     C* PKB
050600110325     C                   IF        %subst(vindta:414:8) <> *all'0' AND
050700110325     C                             %subst(vindta:414:8) <> *blanks
050800110325     C                   EVAL      PiStr=%trim(%subst(vindta:414:8))
050900100729     C                   EXSR      CHKNUM
051000100729     C                   IF        PiNum=*on
051100110325     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
051200110325     C                   Z-ADD(H)  PiVal         VABPKB
051300100729     C                   ELSE
051400101119     C  N51              SETON                                        32
051500101119     C  N51              Z-ADD     *zeros        VABPKB
051600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051700100729     C                             + ' ' + 'VABPKB'
051800100729     C                   ENDIF
051900110325     C                   ENDIF
052000110325     C* VLB
052100110325     C                   IF        %subst(vindta:422:7) <> *all'0' AND
052200110325     C                             %subst(vindta:422:7) <> *blanks
052300110325     C                   EVAL      PiStr=%trim(%subst(vindta:422:7))
052400110325     C                   EXSR      CHKNUM
052500110325     C                   IF        PiNum=*on
052600110325     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
052700110325     C                   Z-ADD(H)  PiVal         VABVLB
052800110325     C                   ELSE
052900110325     C  N51              SETON                                        32
053000110325     C  N51              Z-ADD     *zeros        VABVLB
053100110325     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053200110325     C                             + ' ' + 'VABVLB'
053300110325     C                   ENDIF
053400110325     C                   ENDIF
053500100729     C* CAS
053600110325     C                   IF        %subst(vindta:396:13) <> *all'0' AND
053700110325     C                             %subst(vindta:396:13) <> *blanks
053800100729     C                   EVAL      FlgCAS = '1'
053900110127     C                   EVAL      VABVCA = 'EUR'
054000110928     C                   EVAL      VABTIC = 'BM'
054100110325     C                   EVAL      PiStr=%trim(%subst(vindta:396:13))
054200100729     C                   EXSR      CHKNUM
054300100729     C                   IF        PiNum=*on
054400110325     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
054500100928     C                   Z-ADD(H)  PiVal         VABCAS
054600100729     C                   ELSE
054700101119     C  N51              SETON                                        32
054800101119     C  N51              Z-ADD     *zeros        VABCAS
054900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055000100729     C                             + ' ' + 'VABCAS'
055100100729     C                   ENDIF
055200101119     C                   ELSE
055300101119     C                   EVAL      FlgCAS = '0'
055400100729     C                   ENDIF
055500100729     C*
055600100729     C* Se provincia nn valorizzata la reperisco
055700100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
055800100729     C                   IF        VABNZD  = *blanks AND
055900100729     C                             VABPRD  = *blanks AND
056000100729     C                             VABCAD <> *blanks
056100100729     C                   CLEAR                   TISI95DS
056200100729     C                   EVAL      I95TCN = '3'
056300100729     C                   Z-ADD     datcor        I95DAT
056400100729     C                   EVAL      I95NAR = VABNZD
056500100729     C                   EVAL      I95CAP = VABCAD
056600100729     C                   EVAL      I95LOC = VABLOD
056700100729     C                   CALL      'TISI95R'
056800100729     C                   PARM                    TISI95DS
056900100729     C                   EVAL      VABPRD = O95PRV
057000100729     C                   ENDIF
057100100729     C*
057200100729     C* Considerazioni finali su CBO/CAS
057300100729     C                   IF        FlgCAS = '1'
057400100729     C                   IF        VABCBO = '1'
057500100729     C                   EVAL      VABCBO = '4'
057600100729     C                   ENDIF
057700100729     C                   IF        VABCBO = '2'
057800100729     C                   EVAL      VABCBO = '6'
057900100729     C                   ENDIF
058000100729     C                   ENDIF
058100010202     C*
058200000801     C* Ebbene...
058300000801     C                   ADD       1             §CTRMO
058400070530     C                   IF        *in31 <> *zeros OR
058500070530     C                             *in32 <> *zeros
058600000801     C                   ADD       1             §CTRNO
058700020725     C                   EVAL      x_vinflg = '2'
058800000801     C                   ELSE
058900010201     C                   ADD       1             §CTROKVB
059000000801     C                   ENDIF
059100000801     C*
059200000801     C                   ENDSR
059300100127     C*----------------------------------------------------*
059400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059500100127     C*----------------------------------------------------*
059600100127     C     WRIVAT_A      BEGSR
059700100127     C*
059800100127     C* Valorizzo l buffer di scrittura del FIVAT
059900100127     C* - TIPO RECORD "A"
060000101119     C***                eval      VATTRC = 'A'
060100101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
060200101119     C***                exsr      wrivat
060300100127     C*
060400100127     C                   ENDSR
060500010201     C*----------------------------------------------------*
060600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060700010201     C*----------------------------------------------------*
060800071003     C     WRIVAT_B      BEGSR
060900010201     C*
061000021113     C* Valorizzo l buffer di scrittura del FIVAT
061100070928     C* - TIPO RECORD "B"
061200110325     C                   eval      VATTRC = 'B'
061300110325     C                   eval      VATNOT = %trim(%subst(vindta:206:15))
061400110325     C                   exsr      wrivat
061500010201     C*
061600010201     C                   ENDSR
061700071003     C*----------------------------------------------------*
061800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
061900071003     C*----------------------------------------------------*
062000071003     C     WRIVAT_E      BEGSR
062100071003     C*
062200071003     C* Valorizzo l buffer di scrittura del FIVAT
062300071003     C* - TIPO RECORD "E"
062400100729     C*
062500100928     C                   eval      VATTRC = 'E'
062600110325     C                   eval      VATNOT = %trim(%subst(vindta:430:35))
062700100928     C                   exsr      wrivat
062800071003     C*
062900071003     C                   ENDSR
063000100127     C*----------------------------------------------------*
063100100127     C*  SCARICO BUFFER FIVAT
063200100127     C*----------------------------------------------------*
063300100127     C     WRIVAT        BEGSR
063400100127     C*
063500100127     C                   if        vatNOT <> *blanks
063600100127     C                   ADD       1             §CTROKVT
063700100127     C                   write     FIVAT000
063800100127     C                   endif
063900100127     C*
064000100127     C                   ENDSR
064100020904
064200020904
064300010202     C*----------------------------------------------------*
064400100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
064500010202     C*----------------------------------------------------*
064600100525     C     PREVASX       BEGSR
064700100525     C*
064800021113     C* Compongo il nome del membro da dare al FIVATWWR
064900010202     C                   eval      parmbr = vlrhdl
065000010202     C                   movel     'M'           parmbr
065100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065200010202     C                   eval      paropz = '1'
065300100525     C*
065400010202     C* Effettuo la chiamata al CLLE preposto
065500100525     C  N51              call(e)   'TITVVTC'
065600010202     C                   parm                    parccm
065700010202     C                   parm                    parmbr
065800010202     C                   parm                    paropz
065900100525     C   51              call(e)   'TITVVBC'
066000100525     C                   parm                    parccm
066100100525     C                   parm                    parmbr
066200100525     C                   parm                    paropz
066300100525     C*
066400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066500010202     C                   if        %error
066600010202     C                   movel     '1'           chkcall
066700010202     C                   else
066800010202     C                   movel     '0'           chkcall
066900010202     C                   endif
067000010202     C*
067100010202     C                   ENDSR
067200000801     C*----------------------------------------------------*
067300000801     C*  CONTROLLO NUMERICITA' CAMPI
067400000801     C*----------------------------------------------------*
067500000801     C     CHKNUM        BEGSR
067600081113     C*
067700081113     C                   IF        PiDecChr = *blanks
067800110201     C                   EVAL      PiDecChr = ','
067900081113     C                   ENDIF
068000091223     C*
068100081113     C                   callp(e)  UBISNUM_Check(PiStr
068200081113     C                                          :PiDecChr
068300081113     C                                          :PiVal
068400081113     C                                          :PiNum
068500081113     C                                          :PiInt)
068600081113     C*
068700000801     C                   IF        %error
068800000801     C                   EVAL      PiInt=*off
068900000801     C                   ENDIF
069000000801     C*
069100000801     C                   ENDSR
069200000801     C***
069300011113
069400011113
069500000801
069600000801
069700990920      /TITLE Invio dei dati al punto operativo.
069800010202     C     invio         BEGSR
069900990920     C*
070000021113     C* 1° invio FIVAT
070100010201     C                   reset                   dscmz
070200010201     C                   move      vlrpoi        cmzdst
070300021113     C                   eval      cmzfld = 'FIVATWWR'
070400010201     C                   eval      cmzmbd = vlrhdl
070500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070600021009     C***                if        prmfir = *blanks
070700021113     C                   eval      cmzfla = 'FIVAT00F'
070800021113     C                   eval      cmzmba = 'FIVAT00F'
070900021009     C***                else
071000021009     C***                eval      cmzfla = prmfir
071100021009     C***                eval      cmzmba = prmfir
071200021009     C***                endif
071300010201     C                   eval      cmznrr = *zeros
071400020305     C                   move      §ctrokvt      cmznrr
071500021018     C                   eval      cmzlba = vlrfl1
071600010201     C                   call(e)   'TIS711C'
071700010201     C                   parm                    dscmz
071800010201     C                   parm      *blanks       esito
071900010205     C                   if        %error
072000010205     C                             or cmzerr = '1'
072100010205     C                             or esito  = '1'
072200010205     C                   eval      wrkesito = '3'
072300010205     C                   else
072400010201     C*
072500021113     C* 2° invio FIVAB
072600100525     C                   if        *in51 = *off
072700010201     C                   reset                   dscmz
072800010201     C                   move      vlrpoi        cmzdst
072900010201     C                   eval      cmzfld = vlrfou
073000010201     C                   eval      cmzmbd = vlrhdl
073100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073200021009     C***                if        prmfir = *blanks
073300021113     C                   eval      cmzfla = 'FIVAB00F'
073400021113     C                   eval      cmzmba = 'FIVAB00F'
073500021009     C***                else
073600021009     C***                eval      cmzfla = prmfir
073700021009     C***                eval      cmzmba = prmfir
073800021009     C***                endif
073900010201     C                   eval      cmznrr = *zeros
074000010201     C                   move      §ctrokvb      cmznrr
074100021018     C                   eval      cmzlba = vlrfl1
074200010201     C                   call(e)   'TIS711C'
074300010201     C                   parm                    dscmz
074400010201     C                   parm      *blanks       esito
074500010201     C                   if        %error
074600010201     C                             or cmzerr = '1'
074700010201     C                             or esito  = '1'
074800010201     C                   eval      wrkesito = '3'
074900010201     C                   endif
075000100525     C                   endif
075100010205     C                   endif
075200990920     C*
075300000613     C                   ENDSR
075400000613     C***
075500070411
075600070411     C     *pssr         BEGSR
075700070411     C*
075800070411     C                   if        %open(tivin00r)
075900070411     C                   close     tivin00r
076000070411     C                   endif
076100070411     C                   if        %open(fivabwwr)
076200070411     C                   close     fivabwwr
076300070411     C                   endif
076400070411     C                   if        %open(fivatwwr)
076500070411     C                   close     fivatwwr
076600070411     C                   endif
076700070411     C*
076800070411     C* Effettuo la chiamata al CLLE preposto
076900100525     C  N51              call(e)   'TITVVTC'
077000100525     C                   parm                    parccm
077100100525     C                   parm                    parmbr
077200100525     C                   parm      '2'           paropz
077300100525     C   51              call(e)   'TITVVBC'
077400100525     C                   parm                    parccm
077500100525     C                   parm                    parmbr
077600100525     C                   parm      '2'           paropz
077700070411     C*
077800070411     C                   eval      wrkesito = '2'
077900070411     C*
078000070411     C                   seton                                        LR
078100070411     C*
078200070411     C                   ENDSR     '*CANCL'
078300070411     C***
078400070411
078500990910
078600000613     C     *inzsr        BEGSR
078700990910     C*
078800990910     C     *entry        plist
078900990920     C                   parm                    tivlrds
079000990921     C                   parm      wrkesito      esito
079100000724     C                   parm                    prmlit
079200000710     C                   parm                    prmfir
079300000613     C*
079400000830     C* CALCOLA LA DATA CORRENTE
079500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079600091223     C                   eval      datcor = %dec(%date() : *ISO)
079700000830     C*
079800000613     C                   ENDSR
079900000613     C***
