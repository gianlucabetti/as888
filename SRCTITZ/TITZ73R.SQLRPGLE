000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130319     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002100110201     D depspe          s             10    INZ(*blanks)
002200110201     D curspe          s             10    INZ(*blanks)
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
003600120423     D  VINSPE                       10
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
009900000801
010000910830     C*--------------------------------------------------------
010100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010200910830     C*--------------------------------------------------------
010300070530     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800070530     C*
010900100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011000100525     C                   exsr      prevasx
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400100525     C                   if        not %open(fivabwwr)
011500100525     C                   open      fivabwwr
011600100525     C                   endif
011700100525     C*
011800021113     C                   if        not %open(fivatwwr)
011900021113     C                   open      fivatwwr
012000010201     C                   endif
012100080117     C*
012200080117     C                   EXSR      INZVAR
012300080117     C                   EXSR      DEFCAM
012400990910     C*
012500010201     C                   clear                   §CTROKVB          5 0
012600020305     C                   clear                   §CTROKVT          5 0
012700000801     C                   clear                   §CTRMO            5 0
012800000801     C                   clear                   §CTRNO            5 0
012900990910     C*
013000020725     C/EXEC SQL
013100020725     C+ declare C1 cursor for select
013200110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013300060223     C+ from tivin00r
013400110201     C+ order by sped, substr(vindta, 108, 10) desc
013500020725     C+ for read only
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900020725     C+ open C1
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300070530     C+ Fetch C1 into :INPUT_DS
014400020725     C/END-EXEC
014500020725     C*
014600020725     C                   dow       sqlcod = *zeros
014700990913     C*
014800100310     C                   if        vindta > *blanks
014900000613     C                   add       1             rrnum
015000000801     C*
015100020725     C                   if        vinflg = *blanks
015200020725     C                             or vinflg = '0'
015300020725     C                             or vinflg = '2'
015400000801     C*
015500020725     C                   clear                   x_vinmsg
015600020725     C                   eval      x_vinflg = '1'
015700101119     C*
015800101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015900101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016000110201     C*
016100110201     C* Skippo il record finale 'EOF'
016200110201     C                   IF        %trim(vindta) = 'EOF'
016300110201     C                   eval      x_vinflg = '1'
016400110201     C                   ELSE
016500010305     C*
016600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016700110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
016800010305     C*
016900100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017000080923     C                   if        *in50 = *off
017100101119     C                   exsr      inzvar
017200101119     C                   exsr      defcam
017300071003     C                   exsr      impvab
017400100525     C                   exsr      wrivab
017500100127     C                   exsr      wrivat_a                                     => carico VAT
017600071003     C                   exsr      wrivat_b                                     => carico VAT
017700100525     C   51              exsr      wrivat_e                                     => carico VAT
017800071003     C                   else
017900080923     C*
018000071009     C                   if        wDISK = 'D'
018100091223     C                   exsr      repNSP
018200071009     C                   exsr      impvab
018300100412     C                   exsr      wrivab
018400100127     C                   exsr      wrivat_a                                     => carico VAT
018500071009     C                   exsr      wrivat_b                                     => carico VAT
018600071009     C                   exsr      wrivat_e                                     => carico VAT
018700071009     C                   else
018800071009     C*
018900010305     C                   if        depspe = *blanks                             => 1° giro
019000010305     C                   eval      depspe = curspe                              => memorizz. spediz
019100080117     C                   clear                   tivinds
019200091223     C                   exsr      repNSP
019300020305     C                   exsr      impvab
019400100127     C                   exsr      wrivat_a                                     => carico VAT
019500071003     C                   exsr      wrivat_b                                     => carico VAT
019600071003     C   50              exsr      wrivat_e                                     => carico VAT
019700010305     C                   else
019800020725     C                   if        depspe <> curspe                             => rottura di spediz
019900010305     C                   eval      depspe = curspe                              => memorizz. spediz
020000100412     C                   exsr      wrivab
020100080117     C                   clear                   tivinds
020200091223     C                   exsr      repNSP
020300020305     C                   exsr      impvab
020400100127     C                   exsr      wrivat_a                                     => carico VAT
020500071003     C                   exsr      wrivat_b                                     => carico VAT
020600071003     C   50              exsr      wrivat_e                                     => carico VAT
020700020305     C                   else                                                   => x stessa spediz
020800100401     C*
020900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021000100928     C                   select
021100100928     C                   when      wDUPREC = ' '
021200020305     C                   exsr      impvab
021300100127     C                   exsr      wrivat_a                                     => carico VAT
021400071003     C                   exsr      wrivat_b                                     => carico VAT
021500071003     C   50              exsr      wrivat_e                                     => carico VAT
021600100928     C                   when      wDUPREC = 'C'
021700100928     C   50              exsr      wrivat_e                                     => carico VAT
021800100928     C                   endsl
021900010305     C                   endif
022000010305     C                   endif
022100010305     C                   endif
022200071003     C                   endif
022300110201     C*
022400110201     C                   endif
022500071009     C                   endif
022600000905     C*
022700000905     C                   else
022800020725     C                   eval      x_vinflg = '1'
022900000905     C                   endif
023000000905     C*
023100020725     C     VINRRN        chain     tivin000
023200020725     C                   if        %found(tivin00r)
023300020725     C                   eval      y_vinflg = x_vinflg
023400020725     C                   eval      y_vinmsg = x_vinmsg
023500020725     C                   update    tivin000
023600020725     C                   endif
023700020725     C*
023800020725     C/EXEC SQL
023900070530     C+ Fetch C1 into :INPUT_DS
024000020725     C/END-EXEC
024100020725     C*
024200020725     C                   enddo
024300020725     C*
024400020725     C/EXEC SQL
024500020725     C+ close C1
024600020725     C/END-EXEC
024700000905     C*
024800020305     C* Scarico i VAB rimasti "in sospeso"
024900071009     C                   if        wDISK = 'C'
025000100412     C                   exsr      wrivab
025100071009     C                   endif
025200010202     C*
025300010202     C                   endif
025400990910
025500990910     C* Se non ci sono record con errori ...
025600000710     C                   if        §ctrno = 0
025700990910     C* ... restituisco esito OK.
025800990921     C                   eval      wrkesito = '0'
025900990910     C                   else
026000100525     C                   if        §ctrokvb > 0 OR
026100100525     C                             §ctrokvt > 0
026200990921     C                   eval      wrkesito = '1'
026300000710     C                   else
026400000710     C                   eval      wrkesito = '2'
026500990910     C                   endif
026600000710     C                   endif
026700990910     C*
026800990914     C                   if        %open(tivin00r)
026900990908     C                   close     tivin00r
027000990914     C                   endif
027100021113     C                   if        %open(fivabwwr)
027200021113     C                   close     fivabwwr
027300990914     C                   endif
027400021113     C                   if        %open(fivatwwr)
027500021113     C                   close     fivatwwr
027600010201     C                   endif
027700990910     C*
027800100525     C                   if        §ctrokvb > 0 OR
027900100525     C                             §ctrokvt > 0
028000000724     C                             and vlrpoi <> *zeros
028100010202     C                   exsr      invio
028200990920     C                   endif
028300990920     C*
028400910830     C                   ENDSR
028500000613     C***
028600100412
028700100412
028800010305
028900010305     C*----------------------------------------------------*
029000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029100010305     C*----------------------------------------------------*
029200020305     C     WRIVAB        BEGSR
029300100412     C*
029400100412     C* Gestisco l'eventuale rottura x numero spedizione
029500100412     C                   if        VABRMA = *blanks
029600100412     C                   movel(P)  VABRMN        VABRMA
029700100412     C                   endif
029800100412     C*
029900100525     C                   if        *in51 = *off and *in31 = *off
030000100525     C                   write     fivab000                                     => scarico il VAB
030100100525     C                   endif
030200010305     C*
030300010305     C                   ENDSR
030400990920
030500000801     C*----------------------------------------------------*
030600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700000801     C*----------------------------------------------------*
030800010201     C     INZVAR        BEGSR
030900000801     C*
031000010201     C                   Z-ADD     *zeros        Num5_0
031100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031200020725     C                   MOVEL     '0'           FlgCAS            1
031300000801     C*
031400000801     C                   ENDSR
031500000801     C*----------------------------------------------------*
031600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031700000801     C*----------------------------------------------------*
031800020904     C     DEFCAM        BEGSR
031900080923     C*
032000100928     C                   SETOFF                                       505160
032100000801     C*
032200021113     C                   CLEAR                   FIVAB000
032300021113     C                   CLEAR                   FIVAT000
032400070730     C* Imposto i valori di default...
032500130319     C                   EVAL      VABCCM = 1171020
032600130319     C                   EVAL      VATCCM = 1171020
032700130319     C                   EVAL      VABLNP = 117
032800130319     C                   EVAL      VATLNP = 117
032900110128     C                   EVAL      VABCTR = 000
033000100525     C                   EVAL      VABCBO = '1'
033100070222     C* ... e poi verifico se sono stati passati come parametri
033200070222     C                   IF        vlrppt > *blanks
033300100525     C*
033400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033600070222     C                   EXSR      CHKNUM
033700070222     C                   IF        PiInt=*on
033800070222     C                   Z-ADD     PiVal         VABCCM
033900070222     C                   Z-ADD     PiVal         VATCCM
034000070222     C                   ENDIF
034100070222     C                   ENDIF
034200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034400070222     C                   EXSR      CHKNUM
034500070222     C                   IF        PiInt=*on
034600070222     C                   Z-ADD     PiVal         VABLNP
034700070222     C                   Z-ADD     PiVal         VATLNP
034800070222     C                   ENDIF
034900070222     C                   ENDIF
035000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035200070222     C                   EXSR      CHKNUM
035300070222     C                   IF        PiInt=*on
035400070222     C                   Z-ADD     PiVal         VABCTR
035500070222     C                   ENDIF
035600070928     C                   ENDIF
035700071009     C                   MOVEL     *blanks       wDISK             1
035800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035900071009     C                   IF        wDISK <> *blanks
036000070928     C                   SETON                                        50
036100070222     C                   ENDIF
036200100525     C                   IF        wDISK  = 'T'
036300100525     C                   SETOFF                                       50
036400100525     C                   SETON                                        51
036500100525     C                   ENDIF
036600100401     C                   MOVEL     *blanks       wDUPREC           1
036700100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036800100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036900100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037000100525     C                   ENDIF
037100100525     C*
037200070222     C                   ENDIF
037300071009     C*
037400071009     C   50              EVAL      VABCTM = '7Q'
037500000801     C*
037600000801     C                   ENDSR
037700091223     C*----------------------------------------------------*
037800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037900091223     C*----------------------------------------------------*
038000091223     C     REPNSP        BEGSR
038100091223     C*
038200091223     C                   EXSR      INZVAR
038300091223     C                   EXSR      DEFCAM
038400100928     C*
038500100928     C                   SETON                                        60
038600091223     C*
038700091223     C* NSP => Stacco un numeratore da AZNUM
038800091223     C                   clear                   TRUL33DS
038900091223     C                   eval      I33OPE = *zeros
039000091223     C                   eval      I33CNU = 302
039100091223     C                   eval      I33NUM = 1
039200091223     C                   movel     TRUL33DS      KPJBU
039300091223     C                   call      'TRUL33R'
039400091223     C                   parm                    KPJBA
039500091223     C                   movel     KPJBU         TRUL33DS
039600091223     C                   if        O33ERR = *zeros
039700091223     C                   z-add     O33NRF        VABNSP
039800091223     C                   z-add     O33NRF        VATNSP
039900091223     C                   else
040000091223     C                   SETON                                        31
040100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040200091223     C                             + ' ' + 'VABNSP VATNSP'
040300091223     C                   endif
040400091223     C*
040500091223     C                   ENDSR
040600000801     C*----------------------------------------------------*
040700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040800000801     C*----------------------------------------------------*
040900100730     C     IMPVAB        BEGSR
041000070530     C*
041100070530     C                   SETOFF                                       3132
041200070928     C*
041300070928     C                   MOVE(P)   vlrpoi        VABFGS
041400070928     C                   MOVE(P)   vlrpoi        VATFGS
041500070928     C*
041600070928     C                   MOVEL     datcor        VABAAS
041700070928     C                   MOVEL     datcor        VATAAS
041800070928     C                   MOVE      datcor        VABMGS
041900100729     C*
042000100729     C* Reperimento campi ALFA
042100110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
042200110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
042300110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
042400110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
042500110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
042600110127     C*
042700110127     C                   SELECT
042800110201     C                   WHEN      %subst(vindta:207:1) = 'F'
042900110201     C                   EVAL      VABCBO = '1'
043000110201     C                   WHEN      %subst(vindta:207:1) = 'A'
043100110201     C                   EVAL      VABCBO = '2'
043200110127     C                   WHEN      %subst(vindta:199:1) = 'V'
043300110127     C                   EVAL      VABTIC = 'BM'
043400110127     C                   ENDSL
043500110127     C*
043600110201     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
043700110201     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
043800110201     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
043900100729     C*
044000100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044100100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044200100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044300100729     C* ==
044400100729     C*
044500100729     C* Reperimento campi NUMERICI
044600100729     C                   MOVEL     DATCOR        VABAAS
044700100729     C                   MOVE      DATCOR        VABMGS
044800110127     C* NSP / RMN
044900101119     C                   IF        *IN51 = *off
045000120423     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
045100100730     C                   EXSR      CHKNUM
045200100730     C                   IF        PiInt=*on
045300100730     C                   Z-ADD     PiVal         VABRMN
045400100928     C  N60              Z-ADD     PiVal         VABNSP
045500100928     C  N60              Z-ADD     PiVal         VATNSP
045600100730     C                   ELSE
045700100730     C                   SETON                                        32
045800100730     C                   Z-ADD     1             VABRMN
045900100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046000101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
046100100730     C                   ENDIF
046200101119     C*
046300101119     C                   ELSE
046400101119     C*
046500120423     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
046600101119     C                   EXSR      CHKNUM
046700101119     C                   IF        PiInt=*on
046800101119     C  N60              Z-ADD     PiVal         VATNSP
046900101119     C                   ELSE
047000101119     C                   SETON                                        32
047100101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047200101119     C                             + ' ' + 'VATNSP'
047300101119     C                   ENDIF
047400101119     C                   ENDIF
047500110201     C* NCL
047600110201     C                   z-add     *zeros        wCOLLI            5 0
047700110201     C                   z-add     *zeros        wBANCALI          5 0
047800110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
047900100928     C                   EXSR      CHKNUM
048000100928     C                   IF        PiInt=*on
048100110201     C                   Z-ADD     PiVal         wCOLLI
048200100928     C                   ELSE
048300101119     C  N51              SETON                                        32
048400101119     C  N51              Z-ADD     *zeros        VABNCL
048500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048600100928     C                             + ' ' + 'VABNCL'
048700100928     C                   ENDIF
048800110201     C*
048900110201     C                   EVAL      PiStr=%trim(%subst(vindta:129:2))
049000110201     C                   EXSR      CHKNUM
049100110201     C                   IF        PiInt=*on
049200110201     C                   Z-ADD     PiVal         wBANCALI
049300110201     C                   ELSE
049400110201     C  N51              SETON                                        32
049500110201     C  N51              Z-ADD     *zeros        VABNCL
049600110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049700110201     C                             + ' ' + 'VABNCL'
049800110201     C                   ENDIF
049900110201     C*
050000110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
050100100729     C* PKB
050200110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
050300100729     C                   EXSR      CHKNUM
050400100729     C                   IF        PiNum=*on
050500110201     C                   Z-ADD     PiVal         VABPKB
050600100729     C                   ELSE
050700101119     C  N51              SETON                                        32
050800101119     C  N51              Z-ADD     *zeros        VABPKB
050900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051000100729     C                             + ' ' + 'VABPKB'
051100100729     C                   ENDIF
051200110201     C* VLB
051300110201     C*                  IF        %subst(vindta:234:11) <> '000000000,0'
051400110201     C*                  EVAL      PiStr=%trim(%subst(vindta:234:11))
051500110201     C*                  EXSR      CHKNUM
051600110201     C*                  IF        PiNum=*on
051700110201     C*                  Z-ADD(H)  PiVal         VABVLB
051800110201     C*                  ELSE
051900110201     C* N51              SETON                                        32
052000110201     C* N51              Z-ADD     *zeros        VABVLB
052100110201     C* N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052200110201     C*                            + ' ' + 'VABVLB'
052300110201     C*                  ENDIF
052400110201     C*                  ENDIF
052500100729     C* CAS
052600110201     C                   IF        %subst(vindta:137:10)<> *blanks    AND
052700110201     C                             %subst(vindta:137:10)<> *zeros     AND
052800110201     C                             %subst(vindta:137:10)<> '0000000,00'
052900100729     C                   EVAL      FlgCAS = '1'
053000110127     C                   EVAL      VABVCA = 'EUR'
053100110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
053200100729     C                   EXSR      CHKNUM
053300100729     C                   IF        PiNum=*on
053400100928     C                   Z-ADD(H)  PiVal         VABCAS
053500100729     C                   ELSE
053600101119     C  N51              SETON                                        32
053700101119     C  N51              Z-ADD     *zeros        VABCAS
053800101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053900100729     C                             + ' ' + 'VABCAS'
054000100729     C                   ENDIF
054100101119     C                   ELSE
054200101119     C                   EVAL      FlgCAS = '0'
054300100729     C                   ENDIF
054400110201     C* IAS
054500110201     C                   IF        %subst(vindta:223:11)<> *blanks    AND
054600110201     C                             %subst(vindta:223:11)<> *zeros     AND
054700110201     C                             %subst(vindta:223:11)<> '00000000,00'
054800110201     C                   EVAL      VABVAS = 'EUR'
054900110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
055000110201     C                   EXSR      CHKNUM
055100110201     C                   IF        PiNum=*on
055200110201     C                   Z-ADD(H)  PiVal         VABIAS
055300110201     C                   ELSE
055400110201     C  N51              SETON                                        32
055500110201     C  N51              Z-ADD     *zeros        VABIAS
055600110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055700110201     C                             + ' ' + 'VABIAS'
055800110201     C                   ENDIF
055900110201     C                   ENDIF
056000100729     C*
056100100729     C* Se provincia nn valorizzata la reperisco
056200100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
056300100729     C                   IF        VABNZD  = *blanks AND
056400100729     C                             VABPRD  = *blanks AND
056500100729     C                             VABCAD <> *blanks
056600100729     C                   CLEAR                   TISI95DS
056700100729     C                   EVAL      I95TCN = '3'
056800100729     C                   Z-ADD     datcor        I95DAT
056900100729     C                   EVAL      I95NAR = VABNZD
057000100729     C                   EVAL      I95CAP = VABCAD
057100100729     C                   EVAL      I95LOC = VABLOD
057200100729     C                   CALL      'TISI95R'
057300100729     C                   PARM                    TISI95DS
057400100729     C                   EVAL      VABPRD = O95PRV
057500100729     C                   ENDIF
057600100729     C*
057700100729     C* Considerazioni finali su CBO/CAS
057800100729     C                   IF        FlgCAS = '1'
057900100729     C                   IF        VABCBO = '1'
058000100729     C                   EVAL      VABCBO = '4'
058100100729     C                   ENDIF
058200100729     C                   IF        VABCBO = '2'
058300100729     C                   EVAL      VABCBO = '6'
058400100729     C                   ENDIF
058500100729     C                   ENDIF
058600010202     C*
058700000801     C* Ebbene...
058800000801     C                   ADD       1             §CTRMO
058900070530     C                   IF        *in31 <> *zeros OR
059000070530     C                             *in32 <> *zeros
059100000801     C                   ADD       1             §CTRNO
059200020725     C                   EVAL      x_vinflg = '2'
059300000801     C                   ELSE
059400010201     C                   ADD       1             §CTROKVB
059500000801     C                   ENDIF
059600000801     C*
059700000801     C                   ENDSR
059800100127     C*----------------------------------------------------*
059900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
060000100127     C*----------------------------------------------------*
060100100127     C     WRIVAT_A      BEGSR
060200100127     C*
060300100127     C* Valorizzo l buffer di scrittura del FIVAT
060400100127     C* - TIPO RECORD "A"
060500101119     C***                eval      VATTRC = 'A'
060600101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
060700101119     C***                exsr      wrivat
060800100127     C*
060900100127     C                   ENDSR
061000010201     C*----------------------------------------------------*
061100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061200010201     C*----------------------------------------------------*
061300071003     C     WRIVAT_B      BEGSR
061400010201     C*
061500021113     C* Valorizzo l buffer di scrittura del FIVAT
061600070928     C* - TIPO RECORD "B"
061700101119     C***                eval      VATTRC = 'B'
061800101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
061900101119     C***                exsr      wrivat
062000010201     C*
062100010201     C                   ENDSR
062200071003     C*----------------------------------------------------*
062300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062400071003     C*----------------------------------------------------*
062500071003     C     WRIVAT_E      BEGSR
062600071003     C*
062700071003     C* Valorizzo l buffer di scrittura del FIVAT
062800071003     C* - TIPO RECORD "E"
062900100729     C*
063000130320     C***                eval      VATTRC = 'E'
063100130320     C***                eval      VATNOT = %trim(%subst(vindta:459:8))
063200130320     C***                exsr      wrivat
063300071003     C*
063400071003     C                   ENDSR
063500100127     C*----------------------------------------------------*
063600100127     C*  SCARICO BUFFER FIVAT
063700100127     C*----------------------------------------------------*
063800100127     C     WRIVAT        BEGSR
063900100127     C*
064000100127     C                   if        vatNOT <> *blanks
064100100127     C                   ADD       1             §CTROKVT
064200100127     C                   write     FIVAT000
064300100127     C                   endif
064400100127     C*
064500100127     C                   ENDSR
064600020904
064700020904
064800010202     C*----------------------------------------------------*
064900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
065000010202     C*----------------------------------------------------*
065100100525     C     PREVASX       BEGSR
065200100525     C*
065300021113     C* Compongo il nome del membro da dare al FIVATWWR
065400010202     C                   eval      parmbr = vlrhdl
065500010202     C                   movel     'M'           parmbr
065600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065700010202     C                   eval      paropz = '1'
065800100525     C*
065900010202     C* Effettuo la chiamata al CLLE preposto
066000100525     C  N51              call(e)   'TITVVTC'
066100010202     C                   parm                    parccm
066200010202     C                   parm                    parmbr
066300010202     C                   parm                    paropz
066400100525     C   51              call(e)   'TITVVBC'
066500100525     C                   parm                    parccm
066600100525     C                   parm                    parmbr
066700100525     C                   parm                    paropz
066800100525     C*
066900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067000010202     C                   if        %error
067100010202     C                   movel     '1'           chkcall
067200010202     C                   else
067300010202     C                   movel     '0'           chkcall
067400010202     C                   endif
067500010202     C*
067600010202     C                   ENDSR
067700000801     C*----------------------------------------------------*
067800000801     C*  CONTROLLO NUMERICITA' CAMPI
067900000801     C*----------------------------------------------------*
068000000801     C     CHKNUM        BEGSR
068100081113     C*
068200081113     C                   IF        PiDecChr = *blanks
068300110201     C                   EVAL      PiDecChr = ','
068400081113     C                   ENDIF
068500091223     C*
068600081113     C                   callp(e)  UBISNUM_Check(PiStr
068700081113     C                                          :PiDecChr
068800081113     C                                          :PiVal
068900081113     C                                          :PiNum
069000081113     C                                          :PiInt)
069100081113     C*
069200000801     C                   IF        %error
069300000801     C                   EVAL      PiInt=*off
069400000801     C                   ENDIF
069500000801     C*
069600000801     C                   ENDSR
069700000801     C***
069800011113
069900011113
070000000801
070100000801
070200990920      /TITLE Invio dei dati al punto operativo.
070300010202     C     invio         BEGSR
070400990920     C*
070500021113     C* 1° invio FIVAT
070600010201     C                   reset                   dscmz
070700010201     C                   move      vlrpoi        cmzdst
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
073300010201     C                   move      vlrpoi        cmzdst
073400010201     C                   eval      cmzfld = vlrfou
073500010201     C                   eval      cmzmbd = vlrhdl
073600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073700021009     C***                if        prmfir = *blanks
073800021113     C                   eval      cmzfla = 'FIVAB00F'
073900021113     C                   eval      cmzmba = 'FIVAB00F'
074000021009     C***                else
074100021009     C***                eval      cmzfla = prmfir
074200021009     C***                eval      cmzmba = prmfir
074300021009     C***                endif
074400010201     C                   eval      cmznrr = *zeros
074500010201     C                   move      §ctrokvb      cmznrr
074600021018     C                   eval      cmzlba = vlrfl1
074700010201     C                   call(e)   'TIS711C'
074800010201     C                   parm                    dscmz
074900010201     C                   parm      *blanks       esito
075000010201     C                   if        %error
075100010201     C                             or cmzerr = '1'
075200010201     C                             or esito  = '1'
075300010201     C                   eval      wrkesito = '3'
075400010201     C                   endif
075500100525     C                   endif
075600010205     C                   endif
075700990920     C*
075800000613     C                   ENDSR
075900000613     C***
076000070411
076100070411     C     *pssr         BEGSR
076200070411     C*
076300070411     C                   if        %open(tivin00r)
076400070411     C                   close     tivin00r
076500070411     C                   endif
076600070411     C                   if        %open(fivabwwr)
076700070411     C                   close     fivabwwr
076800070411     C                   endif
076900070411     C                   if        %open(fivatwwr)
077000070411     C                   close     fivatwwr
077100070411     C                   endif
077200070411     C*
077300070411     C* Effettuo la chiamata al CLLE preposto
077400100525     C  N51              call(e)   'TITVVTC'
077500100525     C                   parm                    parccm
077600100525     C                   parm                    parmbr
077700100525     C                   parm      '2'           paropz
077800100525     C   51              call(e)   'TITVVBC'
077900100525     C                   parm                    parccm
078000100525     C                   parm                    parmbr
078100100525     C                   parm      '2'           paropz
078200070411     C*
078300070411     C                   eval      wrkesito = '2'
078400070411     C*
078500070411     C                   seton                                        LR
078600070411     C*
078700070411     C                   ENDSR     '*CANCL'
078800070411     C***
078900070411
079000990910
079100000613     C     *inzsr        BEGSR
079200990910     C*
079300990910     C     *entry        plist
079400990920     C                   parm                    tivlrds
079500990921     C                   parm      wrkesito      esito
079600000724     C                   parm                    prmlit
079700000710     C                   parm                    prmfir
079800000613     C*
079900000830     C* CALCOLA LA DATA CORRENTE
080000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080100091223     C                   eval      datcor = %dec(%date() : *ISO)
080200000830     C*
080300000613     C                   ENDSR
080400000613     C***
