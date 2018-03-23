000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
002100110127     D depspe          s              5    INZ(*blanks)
002200110127     D curspe          s              5    INZ(*blanks)
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
003600100729     D  VINSPE                        8
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
013300110127     C+ vindta, vinflg, substr(vindta, 320, 5) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110127     C+ order by sped, substr(vindta, 320, 5) desc
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
016300110127     C                   EVAL      curspe=%trim(%subst(vindta:320:5))
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
021900071009     C                   endif
022000000905     C*
022100000905     C                   else
022200020725     C                   eval      x_vinflg = '1'
022300000905     C                   endif
022400000905     C*
022500020725     C     VINRRN        chain     tivin000
022600020725     C                   if        %found(tivin00r)
022700020725     C                   eval      y_vinflg = x_vinflg
022800020725     C                   eval      y_vinmsg = x_vinmsg
022900020725     C                   update    tivin000
023000020725     C                   endif
023100020725     C*
023200020725     C/EXEC SQL
023300070530     C+ Fetch C1 into :INPUT_DS
023400020725     C/END-EXEC
023500020725     C*
023600020725     C                   enddo
023700020725     C*
023800020725     C/EXEC SQL
023900020725     C+ close C1
024000020725     C/END-EXEC
024100000905     C*
024200020305     C* Scarico i VAB rimasti "in sospeso"
024300071009     C                   if        wDISK = 'C'
024400100412     C                   exsr      wrivab
024500071009     C                   endif
024600010202     C*
024700010202     C                   endif
024800990910
024900990910     C* Se non ci sono record con errori ...
025000000710     C                   if        §ctrno = 0
025100990910     C* ... restituisco esito OK.
025200990921     C                   eval      wrkesito = '0'
025300990910     C                   else
025400100525     C                   if        §ctrokvb > 0 OR
025500100525     C                             §ctrokvt > 0
025600990921     C                   eval      wrkesito = '1'
025700000710     C                   else
025800000710     C                   eval      wrkesito = '2'
025900990910     C                   endif
026000000710     C                   endif
026100990910     C*
026200990914     C                   if        %open(tivin00r)
026300990908     C                   close     tivin00r
026400990914     C                   endif
026500021113     C                   if        %open(fivabwwr)
026600021113     C                   close     fivabwwr
026700990914     C                   endif
026800021113     C                   if        %open(fivatwwr)
026900021113     C                   close     fivatwwr
027000010201     C                   endif
027100990910     C*
027200100525     C                   if        §ctrokvb > 0 OR
027300100525     C                             §ctrokvt > 0
027400000724     C                             and vlrpoi <> *zeros
027500010202     C                   exsr      invio
027600990920     C                   endif
027700990920     C*
027800910830     C                   ENDSR
027900000613     C***
028000100412
028100100412
028200010305
028300010305     C*----------------------------------------------------*
028400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028500010305     C*----------------------------------------------------*
028600020305     C     WRIVAB        BEGSR
028700100412     C*
028800100412     C* Gestisco l'eventuale rottura x numero spedizione
028900100412     C                   if        VABRMA = *blanks
029000100412     C                   movel(P)  VABRMN        VABRMA
029100100412     C                   endif
029200100412     C*
029300100525     C                   if        *in51 = *off and *in31 = *off
029400100525     C                   write     fivab000                                     => scarico il VAB
029500100525     C                   endif
029600010305     C*
029700010305     C                   ENDSR
029800990920
029900000801     C*----------------------------------------------------*
030000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030100000801     C*----------------------------------------------------*
030200010201     C     INZVAR        BEGSR
030300000801     C*
030400010201     C                   Z-ADD     *zeros        Num5_0
030500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030600020725     C                   MOVEL     '0'           FlgCAS            1
030700000801     C*
030800000801     C                   ENDSR
030900000801     C*----------------------------------------------------*
031000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031100000801     C*----------------------------------------------------*
031200020904     C     DEFCAM        BEGSR
031300080923     C*
031400100928     C                   SETOFF                                       505160
031500000801     C*
031600021113     C                   CLEAR                   FIVAB000
031700021113     C                   CLEAR                   FIVAT000
031800070730     C* Imposto i valori di default...
031900110127     C                   EVAL      VABCCM = 0011786
032000110127     C                   EVAL      VATCCM = 0011786
032100110127     C                   EVAL      VABLNP = 001
032200110127     C                   EVAL      VATLNP = 001
032300110127     C                   EVAL      VABCTR = 100
032400100928     C                   EVAL      VABCTM = '7Q'
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
034900070222     C                   Z-ADD     PiVal         VABCTR
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
036600100525     C*
036700070222     C                   ENDIF
036800071009     C*
036900071009     C   50              EVAL      VABCTM = '7Q'
037000000801     C*
037100000801     C                   ENDSR
037200091223     C*----------------------------------------------------*
037300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037400091223     C*----------------------------------------------------*
037500091223     C     REPNSP        BEGSR
037600091223     C*
037700091223     C                   EXSR      INZVAR
037800091223     C                   EXSR      DEFCAM
037900100928     C*
038000100928     C                   SETON                                        60
038100091223     C*
038200091223     C* NSP => Stacco un numeratore da AZNUM
038300091223     C                   clear                   TRUL33DS
038400091223     C                   eval      I33OPE = *zeros
038500091223     C                   eval      I33CNU = 302
038600091223     C                   eval      I33NUM = 1
038700091223     C                   movel     TRUL33DS      KPJBU
038800091223     C                   call      'TRUL33R'
038900091223     C                   parm                    KPJBA
039000091223     C                   movel     KPJBU         TRUL33DS
039100091223     C                   if        O33ERR = *zeros
039200091223     C                   z-add     O33NRF        VABNSP
039300091223     C                   z-add     O33NRF        VATNSP
039400091223     C                   else
039500091223     C                   SETON                                        31
039600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700091223     C                             + ' ' + 'VABNSP VATNSP'
039800091223     C                   endif
039900091223     C*
040000091223     C                   ENDSR
040100000801     C*----------------------------------------------------*
040200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040300000801     C*----------------------------------------------------*
040400100730     C     IMPVAB        BEGSR
040500070530     C*
040600070530     C                   SETOFF                                       3132
040700070928     C*
040800070928     C                   MOVE(P)   vlrpoi        VABFGS
040900070928     C                   MOVE(P)   vlrpoi        VATFGS
041000070928     C*
041100070928     C                   MOVEL     datcor        VABAAS
041200070928     C                   MOVEL     datcor        VATAAS
041300070928     C                   MOVE      datcor        VABMGS
041400100729     C*
041500100729     C* Reperimento campi ALFA
041600110127     C                   EVAL      VABRSD=%trim(%subst(vindta:33:25))
041700110127     C                   EVAL      VABRD2=%trim(%subst(vindta:58:15))
041800110127     C                   EVAL      VABIND=%trim(%subst(vindta:73:50))
041900110127     C                   EVAL      VABLOD=%trim(%subst(vindta:123:40))
042000110127     C                   EVAL      VABPRD=%trim(%subst(vindta:168:2))
042100110127     C                   EVAL      VABCAD=%trim(%subst(vindta:163:5))
042200101117     C                   EVAL      VABNZD=%trim(%subst(vindta:674:3))
042300101117     C                   IF        VABNZD='I'   or
042400101117     C                             VABNZD='IT'  or
042500101117     C                             VABNZD='ITA'
042600101117     C                   EVAL      VABNZD=*blanks
042700101117     C                   ENDIF
042800110127     C*
042900110127     C                   SELECT
043000110127     C                   WHEN      %subst(vindta:199:1) = 'C'
043100110127     C                   EVAL      VABTIC = 'CM'
043200110127     C                   WHEN      %subst(vindta:199:1) = 'D'
043300110127     C                   EVAL      VABTIC = 'TM'
043400110127     C                   WHEN      %subst(vindta:199:1) = 'V'
043500110127     C                   EVAL      VABTIC = 'BM'
043600110127     C                   ENDSL
043700110127     C*
043800110127     C                   EVAL      VABRMA=%trim(%subst(vindta:317:9))
043900110127     C                   EVAL      VABNOT=%trim(%subst(vindta:201:13))
044000100729     C*
044100100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044200100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044300100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044400100729     C* ==
044500100729     C*
044600100729     C* Reperimento campi NUMERICI
044700100729     C                   MOVEL     DATCOR        VABAAS
044800100729     C                   MOVE      DATCOR        VABMGS
044900110127     C* NSP / RMN
045000101119     C                   IF        *IN51 = *off
045100110127     C                   EVAL      PiStr=%trim(%subst(vindta:320:5))
045200100730     C                   EXSR      CHKNUM
045300100730     C                   IF        PiInt=*on
045400100730     C                   Z-ADD     PiVal         VABRMN
045500100928     C  N60              Z-ADD     PiVal         VABNSP
045600100928     C  N60              Z-ADD     PiVal         VATNSP
045700100730     C                   ELSE
045800100730     C                   SETON                                        32
045900100730     C                   Z-ADD     1             VABRMN
046000100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
046200100730     C                   ENDIF
046300101119     C*
046400101119     C                   ELSE
046500101119     C*
046600110127     C                   EVAL      PiStr=%trim(%subst(vindta:320:5))
046700101119     C                   EXSR      CHKNUM
046800101119     C                   IF        PiInt=*on
046900101119     C  N60              Z-ADD     PiVal         VATNSP
047000101119     C                   ELSE
047100101119     C                   SETON                                        32
047200101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047300101119     C                             + ' ' + 'VATNSP'
047400101119     C                   ENDIF
047500101119     C                   ENDIF
047600100729     C* NCL
047700110127     C                   EVAL      PiStr=%trim(%subst(vindta:171:3))
047800100928     C                   EXSR      CHKNUM
047900100928     C                   IF        PiInt=*on
048000100928     C                   Z-ADD     PiVal         VABNCL
048100100928     C                   ELSE
048200101119     C  N51              SETON                                        32
048300101119     C  N51              Z-ADD     *zeros        VABNCL
048400101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048500100928     C                             + ' ' + 'VABNCL'
048600100928     C                   ENDIF
048700100729     C* PKB
048800110127     C                   EVAL      PiStr=%trim(%subst(vindta:174:4))
048900100729     C                   EXSR      CHKNUM
049000100729     C                   IF        PiNum=*on
049100100729     C                   Z-ADD(H)  PiVal         VABPKB
049200100729     C                   ELSE
049300101119     C  N51              SETON                                        32
049400101119     C  N51              Z-ADD     *zeros        VABPKB
049500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049600100729     C                             + ' ' + 'VABPKB'
049700100729     C                   ENDIF
049800101117     C* VLB
049900110127     C                   IF        %subst(vindta:178:5) <> '00.00'
050000110127     C                   EVAL      PiStr=%trim(%subst(vindta:178:5))
050100101117     C                   EXSR      CHKNUM
050200101117     C                   IF        PiNum=*on
050300101117     C                   Z-ADD(H)  PiVal         VABVLB
050400101117     C                   ELSE
050500101119     C  N51              SETON                                        32
050600101119     C  N51              Z-ADD     *zeros        VABVLB
050700101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050800101117     C                             + ' ' + 'VABVLB'
050900101117     C                   ENDIF
051000101119     C                   ENDIF
051100100729     C* CAS
051200110127     C                   IF        %subst(vindta:183:8)<> *blanks    AND
051300110127     C                             %subst(vindta:183:8)<> *zeros     AND
051400110127     C                             %subst(vindta:183:8)<> '00000.00'
051500100729     C                   EVAL      FlgCAS = '1'
051600110127     C                   EVAL      VABVCA = 'EUR'
051700110127     C                   EVAL      PiStr=%trim(%subst(vindta:183:8))
051800100729     C                   EXSR      CHKNUM
051900100729     C                   IF        PiNum=*on
052000100928     C                   Z-ADD(H)  PiVal         VABCAS
052100100729     C                   ELSE
052200101119     C  N51              SETON                                        32
052300101119     C  N51              Z-ADD     *zeros        VABCAS
052400101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052500100729     C                             + ' ' + 'VABCAS'
052600100729     C                   ENDIF
052700101119     C                   ELSE
052800101119     C                   EVAL      FlgCAS = '0'
052900100729     C                   ENDIF
053000100729     C*
053100100729     C* Se provincia nn valorizzata la reperisco
053200100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
053300100729     C                   IF        VABNZD  = *blanks AND
053400100729     C                             VABPRD  = *blanks AND
053500100729     C                             VABCAD <> *blanks
053600100729     C                   CLEAR                   TISI95DS
053700100729     C                   EVAL      I95TCN = '3'
053800100729     C                   Z-ADD     datcor        I95DAT
053900100729     C                   EVAL      I95NAR = VABNZD
054000100729     C                   EVAL      I95CAP = VABCAD
054100100729     C                   EVAL      I95LOC = VABLOD
054200100729     C                   CALL      'TISI95R'
054300100729     C                   PARM                    TISI95DS
054400100729     C                   EVAL      VABPRD = O95PRV
054500100729     C                   ENDIF
054600100729     C*
054700100729     C* Considerazioni finali su CBO/CAS
054800100729     C                   IF        FlgCAS = '1'
054900100729     C                   IF        VABCBO = '1'
055000100729     C                   EVAL      VABCBO = '4'
055100100729     C                   ENDIF
055200100729     C                   IF        VABCBO = '2'
055300100729     C                   EVAL      VABCBO = '6'
055400100729     C                   ENDIF
055500100729     C                   ENDIF
055600010202     C*
055700000801     C* Ebbene...
055800000801     C                   ADD       1             §CTRMO
055900070530     C                   IF        *in31 <> *zeros OR
056000070530     C                             *in32 <> *zeros
056100000801     C                   ADD       1             §CTRNO
056200020725     C                   EVAL      x_vinflg = '2'
056300000801     C                   ELSE
056400010201     C                   ADD       1             §CTROKVB
056500000801     C                   ENDIF
056600000801     C*
056700000801     C                   ENDSR
056800100127     C*----------------------------------------------------*
056900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057000100127     C*----------------------------------------------------*
057100100127     C     WRIVAT_A      BEGSR
057200100127     C*
057300100127     C* Valorizzo l buffer di scrittura del FIVAT
057400100127     C* - TIPO RECORD "A"
057500101119     C***                eval      VATTRC = 'A'
057600101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
057700101119     C***                exsr      wrivat
057800100127     C*
057900100127     C                   ENDSR
058000010201     C*----------------------------------------------------*
058100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058200010201     C*----------------------------------------------------*
058300071003     C     WRIVAT_B      BEGSR
058400010201     C*
058500021113     C* Valorizzo l buffer di scrittura del FIVAT
058600070928     C* - TIPO RECORD "B"
058700101119     C***                eval      VATTRC = 'B'
058800101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
058900101119     C***                exsr      wrivat
059000010201     C*
059100010201     C                   ENDSR
059200071003     C*----------------------------------------------------*
059300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059400071003     C*----------------------------------------------------*
059500071003     C     WRIVAT_E      BEGSR
059600071003     C*
059700071003     C* Valorizzo l buffer di scrittura del FIVAT
059800071003     C* - TIPO RECORD "E"
059900100729     C*
060000100928     C                   eval      VATTRC = 'E'
060100120404     C                   eval      VATNOT = %trim(%subst(vindta:459:8))
060200120404     C***                eval      VATNOT = %trim(%subst(vindta:477:8))
060300100928     C                   exsr      wrivat
060400071003     C*
060500071003     C                   ENDSR
060600100127     C*----------------------------------------------------*
060700100127     C*  SCARICO BUFFER FIVAT
060800100127     C*----------------------------------------------------*
060900100127     C     WRIVAT        BEGSR
061000100127     C*
061100100127     C                   if        vatNOT <> *blanks
061200100127     C                   ADD       1             §CTROKVT
061300100127     C                   write     FIVAT000
061400100127     C                   endif
061500100127     C*
061600100127     C                   ENDSR
061700020904
061800020904
061900010202     C*----------------------------------------------------*
062000100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
062100010202     C*----------------------------------------------------*
062200100525     C     PREVASX       BEGSR
062300100525     C*
062400021113     C* Compongo il nome del membro da dare al FIVATWWR
062500010202     C                   eval      parmbr = vlrhdl
062600010202     C                   movel     'M'           parmbr
062700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062800010202     C                   eval      paropz = '1'
062900100525     C*
063000010202     C* Effettuo la chiamata al CLLE preposto
063100100525     C  N51              call(e)   'TITVVTC'
063200010202     C                   parm                    parccm
063300010202     C                   parm                    parmbr
063400010202     C                   parm                    paropz
063500100525     C   51              call(e)   'TITVVBC'
063600100525     C                   parm                    parccm
063700100525     C                   parm                    parmbr
063800100525     C                   parm                    paropz
063900100525     C*
064000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064100010202     C                   if        %error
064200010202     C                   movel     '1'           chkcall
064300010202     C                   else
064400010202     C                   movel     '0'           chkcall
064500010202     C                   endif
064600010202     C*
064700010202     C                   ENDSR
064800000801     C*----------------------------------------------------*
064900000801     C*  CONTROLLO NUMERICITA' CAMPI
065000000801     C*----------------------------------------------------*
065100000801     C     CHKNUM        BEGSR
065200081113     C*
065300081113     C                   IF        PiDecChr = *blanks
065400100503     C                   EVAL      PiDecChr = '.'
065500081113     C                   ENDIF
065600091223     C*
065700081113     C                   callp(e)  UBISNUM_Check(PiStr
065800081113     C                                          :PiDecChr
065900081113     C                                          :PiVal
066000081113     C                                          :PiNum
066100081113     C                                          :PiInt)
066200081113     C*
066300000801     C                   IF        %error
066400000801     C                   EVAL      PiInt=*off
066500000801     C                   ENDIF
066600000801     C*
066700000801     C                   ENDSR
066800000801     C***
066900011113
067000011113
067100000801
067200000801
067300990920      /TITLE Invio dei dati al punto operativo.
067400010202     C     invio         BEGSR
067500990920     C*
067600021113     C* 1° invio FIVAT
067700010201     C                   reset                   dscmz
067800010201     C                   move      vlrpoi        cmzdst
067900021113     C                   eval      cmzfld = 'FIVATWWR'
068000010201     C                   eval      cmzmbd = vlrhdl
068100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068200021009     C***                if        prmfir = *blanks
068300021113     C                   eval      cmzfla = 'FIVAT00F'
068400021113     C                   eval      cmzmba = 'FIVAT00F'
068500021009     C***                else
068600021009     C***                eval      cmzfla = prmfir
068700021009     C***                eval      cmzmba = prmfir
068800021009     C***                endif
068900010201     C                   eval      cmznrr = *zeros
069000020305     C                   move      §ctrokvt      cmznrr
069100021018     C                   eval      cmzlba = vlrfl1
069200010201     C                   call(e)   'TIS711C'
069300010201     C                   parm                    dscmz
069400010201     C                   parm      *blanks       esito
069500010205     C                   if        %error
069600010205     C                             or cmzerr = '1'
069700010205     C                             or esito  = '1'
069800010205     C                   eval      wrkesito = '3'
069900010205     C                   else
070000010201     C*
070100021113     C* 2° invio FIVAB
070200100525     C                   if        *in51 = *off
070300010201     C                   reset                   dscmz
070400010201     C                   move      vlrpoi        cmzdst
070500010201     C                   eval      cmzfld = vlrfou
070600010201     C                   eval      cmzmbd = vlrhdl
070700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070800021009     C***                if        prmfir = *blanks
070900021113     C                   eval      cmzfla = 'FIVAB00F'
071000021113     C                   eval      cmzmba = 'FIVAB00F'
071100021009     C***                else
071200021009     C***                eval      cmzfla = prmfir
071300021009     C***                eval      cmzmba = prmfir
071400021009     C***                endif
071500010201     C                   eval      cmznrr = *zeros
071600010201     C                   move      §ctrokvb      cmznrr
071700021018     C                   eval      cmzlba = vlrfl1
071800010201     C                   call(e)   'TIS711C'
071900010201     C                   parm                    dscmz
072000010201     C                   parm      *blanks       esito
072100010201     C                   if        %error
072200010201     C                             or cmzerr = '1'
072300010201     C                             or esito  = '1'
072400010201     C                   eval      wrkesito = '3'
072500010201     C                   endif
072600100525     C                   endif
072700010205     C                   endif
072800990920     C*
072900000613     C                   ENDSR
073000000613     C***
073100070411
073200070411     C     *pssr         BEGSR
073300070411     C*
073400070411     C                   if        %open(tivin00r)
073500070411     C                   close     tivin00r
073600070411     C                   endif
073700070411     C                   if        %open(fivabwwr)
073800070411     C                   close     fivabwwr
073900070411     C                   endif
074000070411     C                   if        %open(fivatwwr)
074100070411     C                   close     fivatwwr
074200070411     C                   endif
074300070411     C*
074400070411     C* Effettuo la chiamata al CLLE preposto
074500100525     C  N51              call(e)   'TITVVTC'
074600100525     C                   parm                    parccm
074700100525     C                   parm                    parmbr
074800100525     C                   parm      '2'           paropz
074900100525     C   51              call(e)   'TITVVBC'
075000100525     C                   parm                    parccm
075100100525     C                   parm                    parmbr
075200100525     C                   parm      '2'           paropz
075300070411     C*
075400070411     C                   eval      wrkesito = '2'
075500070411     C*
075600070411     C                   seton                                        LR
075700070411     C*
075800070411     C                   ENDSR     '*CANCL'
075900070411     C***
076000070411
076100990910
076200000613     C     *inzsr        BEGSR
076300990910     C*
076400990910     C     *entry        plist
076500990920     C                   parm                    tivlrds
076600990921     C                   parm      wrkesito      esito
076700000724     C                   parm                    prmlit
076800000710     C                   parm                    prmfir
076900000613     C*
077000000830     C* CALCOLA LA DATA CORRENTE
077100091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077200091223     C                   eval      datcor = %dec(%date() : *ISO)
077300000830     C*
077400000613     C                   ENDSR
