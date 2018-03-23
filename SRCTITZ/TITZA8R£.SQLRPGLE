000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140203      *
000300140203      * PARTICOLARITA':
000400140203      *
000500140203     H dftactgrp(*no) actgrp(*caller) bnddir('TRUL')
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800100412     FFIVABwwr  O    E             DISK    usropn
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
002400110201     D depspe          s             10    INZ(*blanks)
002500110201     D curspe          s             10    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100091223     D Num5_0          s              5  0
003200130911     D IdColloIn       s             15  0
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700070530     D INPUT_DS        DS                  INZ
003800100729     D  VINDTA                     2048
003900100729     D  VINFLG                        1
004000120423     D  VINSPE                       10
004100100729     D  VINRRN                        8  0
004200020725     D*
004300081113
004400091223     D*------------------
004500091223     D* DS REPERIMENTO NUMERATORE
004600091223     D*------------------
004700100309     D trul33ds      e ds                  inz
004800091223     D*------------------
004900091223     D* DS ARCHITETTURA
005000091223     D*------------------
005100091223     D kpjba         e ds                  inz
005200091223
005300101119
005400101119     D*-------------------
005500101119     D* COSTANTI
005600101119     D*-------------------
005700101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005800101119     D                                     èéù')
005900101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006000101119     D                                     EEU')
006100081113
006200081113     D*------------------
006300081113     D* LINKING A DEFINIZIONI ESTERNE
006400081113     D*------------------
006500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700081113
006800990908
006900010201
007000010201
007100080117     C*
007200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300080117     C
007400080117     C/EXEC SQL
007500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600080117     C/END-EXEC
007700080117     C
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000613     C*
008200100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008400000613     C*
008500010202     C* Effettuo la chiamata al CLLE preposto
008600100525     C  N51              call(e)   'TITVVTC'
008700100525     C                   parm                    parccm
008800100525     C                   parm                    parmbr
008900100525     C                   parm      '2'           paropz
009000100525     C   51              call(e)   'TITVVBC'
009100100525     C                   parm                    parccm
009200100525     C                   parm                    parmbr
009300100525     C                   parm      '2'           paropz
009400070730     C*
009500070730     C* Effettuo lancio TISI95 solo x chiusura
009600070730     C                   CLEAR                   TISI95DS
009700070730     C                   EVAL      I95TLA = 'C'
009800070730     C                   CALL      'TISI95R'
009900070730     C                   PARM                    TISI95DS
010000000616     C*
010100000801     C
010200010201     C                   seton                                        LR
010300000801
010400910830     C*--------------------------------------------------------
010500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010600910830     C*--------------------------------------------------------
010700070530     C     RWFILE        BEGSR
010800990910     C*
010900990914     C                   if        not %open(tivin00r)
011000990908     C                   open      tivin00r
011100990914     C                   endif
011200070530     C*
011300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011400100525     C                   exsr      prevasx
011500010201     C*
011600010202     C                   if        chkcall = '0'
011700010202     C*
011800100525     C                   if        not %open(fivabwwr)
011900100525     C                   open      fivabwwr
012000100525     C                   endif
012100100525     C*
012200021113     C                   if        not %open(fivatwwr)
012300021113     C                   open      fivatwwr
012400010201     C                   endif
012500080117     C*
012600080117     C                   EXSR      INZVAR
012700080117     C                   EXSR      DEFCAM
012800990910     C*
012900010201     C                   clear                   §CTROKVB          5 0
013000020305     C                   clear                   §CTROKVT          5 0
013100000801     C                   clear                   §CTRMO            5 0
013200000801     C                   clear                   §CTRNO            5 0
013300990910     C*
013400020725     C/EXEC SQL
013500020725     C+ declare C1 cursor for select
013600110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013700060223     C+ from tivin00r
013800110201     C+ order by sped, substr(vindta, 108, 10) desc
013900020725     C+ for read only
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300020725     C+ open C1
014400020725     C/END-EXEC
014500020725     C
014600020725     C/EXEC SQL
014700070530     C+ Fetch C1 into :INPUT_DS
014800020725     C/END-EXEC
014900020725     C*
015000020725     C                   dow       sqlcod = *zeros
015100990913     C*
015200100310     C                   if        vindta > *blanks
015300000613     C                   add       1             rrnum
015400000801     C*
015500020725     C                   if        vinflg = *blanks
015600020725     C                             or vinflg = '0'
015700020725     C                             or vinflg = '2'
015800000801     C*
015900020725     C                   clear                   x_vinmsg
016000020725     C                   eval      x_vinflg = '1'
016100101119     C*
016200101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016300101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016400110201     C*
016500110201     C* Skippo il record finale 'EOF'
016600110201     C                   IF        %trim(vindta) = 'EOF'
016700110201     C                   eval      x_vinflg = '1'
016800110201     C                   ELSE
016900010305     C*
017000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017100110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
017200010305     C*
017300100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017400080923     C                   if        *in50 = *off
017500101119     C                   exsr      inzvar
017600101119     C                   exsr      defcam
017700071003     C                   exsr      impvab
017800100525     C                   exsr      wrivab
017900100127     C                   exsr      wrivat_a                                     => carico VAT
018000071003     C                   exsr      wrivat_b                                     => carico VAT
018100100525     C   51              exsr      wrivat_e                                     => carico VAT
018200071003     C                   else
018300080923     C*
018400071009     C                   if        wDISK = 'D'
018500091223     C                   exsr      repNSP
018600071009     C                   exsr      impvab
018700100412     C                   exsr      wrivab
018800100127     C                   exsr      wrivat_a                                     => carico VAT
018900071009     C                   exsr      wrivat_b                                     => carico VAT
019000071009     C                   exsr      wrivat_e                                     => carico VAT
019100071009     C                   else
019200071009     C*
019300010305     C                   if        depspe = *blanks                             => 1° giro
019400010305     C                   eval      depspe = curspe                              => memorizz. spediz
019500080117     C                   clear                   tivinds
019600091223     C                   exsr      repNSP
019700020305     C                   exsr      impvab
019800100127     C                   exsr      wrivat_a                                     => carico VAT
019900071003     C                   exsr      wrivat_b                                     => carico VAT
020000071003     C   50              exsr      wrivat_e                                     => carico VAT
020100010305     C                   else
020200020725     C                   if        depspe <> curspe                             => rottura di spediz
020300010305     C                   eval      depspe = curspe                              => memorizz. spediz
020400100412     C                   exsr      wrivab
020500080117     C                   clear                   tivinds
020600091223     C                   exsr      repNSP
020700020305     C                   exsr      impvab
020800100127     C                   exsr      wrivat_a                                     => carico VAT
020900071003     C                   exsr      wrivat_b                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100020305     C                   else                                                   => x stessa spediz
021200100401     C*
021300100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021400100928     C                   select
021500100928     C                   when      wDUPREC = ' '
021600020305     C                   exsr      impvab
021700100127     C                   exsr      wrivat_a                                     => carico VAT
021800071003     C                   exsr      wrivat_b                                     => carico VAT
021900071003     C   50              exsr      wrivat_e                                     => carico VAT
022000100928     C                   when      wDUPREC = 'C'
022100100928     C   50              exsr      wrivat_e                                     => carico VAT
022200100928     C                   endsl
022300010305     C                   endif
022400010305     C                   endif
022500010305     C                   endif
022600071003     C                   endif
022700110201     C*
022800110201     C                   endif
022900071009     C                   endif
023000000905     C*
023100000905     C                   else
023200020725     C                   eval      x_vinflg = '1'
023300000905     C                   endif
023400000905     C*
023500020725     C     VINRRN        chain     tivin000
023600020725     C                   if        %found(tivin00r)
023700020725     C                   eval      y_vinflg = x_vinflg
023800020725     C                   eval      y_vinmsg = x_vinmsg
023900020725     C                   update    tivin000
024000020725     C                   endif
024100020725     C*
024200020725     C/EXEC SQL
024300070530     C+ Fetch C1 into :INPUT_DS
024400020725     C/END-EXEC
024500020725     C*
024600020725     C                   enddo
024700020725     C*
024800020725     C/EXEC SQL
024900020725     C+ close C1
025000020725     C/END-EXEC
025100000905     C*
025200020305     C* Scarico i VAB rimasti "in sospeso"
025300071009     C                   if        wDISK = 'C'
025400100412     C                   exsr      wrivab
025500071009     C                   endif
025600010202     C*
025700010202     C                   endif
025800990910
025900990910     C* Se non ci sono record con errori ...
026000000710     C                   if        §ctrno = 0
026100990910     C* ... restituisco esito OK.
026200990921     C                   eval      wrkesito = '0'
026300990910     C                   else
026400100525     C                   if        §ctrokvb > 0 OR
026500100525     C                             §ctrokvt > 0
026600990921     C                   eval      wrkesito = '1'
026700000710     C                   else
026800000710     C                   eval      wrkesito = '2'
026900990910     C                   endif
027000000710     C                   endif
027100990910     C*
027200990914     C                   if        %open(tivin00r)
027300990908     C                   close     tivin00r
027400990914     C                   endif
027500021113     C                   if        %open(fivabwwr)
027600021113     C                   close     fivabwwr
027700990914     C                   endif
027800021113     C                   if        %open(fivatwwr)
027900021113     C                   close     fivatwwr
028000010201     C                   endif
028100990910     C*
028200100525     C                   if        §ctrokvb > 0 OR
028300100525     C                             §ctrokvt > 0
028400000724     C                             and vlrpoi <> *zeros
028500010202     C                   exsr      invio
028600990920     C                   endif
028700990920     C*
028800910830     C                   ENDSR
028900000613     C***
029000100412
029100100412
029200010305
029300010305     C*----------------------------------------------------*
029400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029500010305     C*----------------------------------------------------*
029600020305     C     WRIVAB        BEGSR
029700100412     C*
029800100412     C* Gestisco l'eventuale rottura x numero spedizione
029900100412     C                   if        VABRMA = *blanks
030000100412     C                   movel(P)  VABRMN        VABRMA
030100100412     C                   endif
030200100412     C*
030300100525     C                   if        *in51 = *off and *in31 = *off
030400100525     C                   write     fivab000                                     => scarico il VAB
030500100525     C                   endif
030600010305     C*
030700010305     C                   ENDSR
030800990920
030900000801     C*----------------------------------------------------*
031000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031100000801     C*----------------------------------------------------*
031200010201     C     INZVAR        BEGSR
031300000801     C*
031400010201     C                   Z-ADD     *zeros        Num5_0
031500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031600020725     C                   MOVEL     '0'           FlgCAS            1
031700000801     C*
031800000801     C                   ENDSR
031900000801     C*----------------------------------------------------*
032000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032100000801     C*----------------------------------------------------*
032200020904     C     DEFCAM        BEGSR
032300080923     C*
032400100928     C                   SETOFF                                       505160
032500000801     C*
032600021113     C                   CLEAR                   FIVAB000
032700021113     C                   CLEAR                   FIVAT000
032800070730     C* Imposto i valori di default...
032900140128     C                   EVAL      VABCCM = 0436322
033000140128     C                   EVAL      VATCCM = 0436322
033100140128     C                   EVAL      VABLNP = 043
033200140128     C                   EVAL      VATLNP = 043
033300110128     C                   EVAL      VABCTR = 000
033400100525     C                   EVAL      VABCBO = '1'
033500070222     C* ... e poi verifico se sono stati passati come parametri
033600070222     C                   IF        vlrppt > *blanks
033700100525     C*
033800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034000070222     C                   EXSR      CHKNUM
034100070222     C                   IF        PiInt=*on
034200070222     C                   Z-ADD     PiVal         VABCCM
034300070222     C                   Z-ADD     PiVal         VATCCM
034400070222     C                   ENDIF
034500070222     C                   ENDIF
034600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABLNP
035100070222     C                   Z-ADD     PiVal         VATLNP
035200070222     C                   ENDIF
035300070222     C                   ENDIF
035400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035600070222     C                   EXSR      CHKNUM
035700070222     C                   IF        PiInt=*on
035800070222     C                   Z-ADD     PiVal         VABCTR
035900070222     C                   ENDIF
036000070928     C                   ENDIF
036100071009     C                   MOVEL     *blanks       wDISK             1
036200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036300071009     C                   IF        wDISK <> *blanks
036400070928     C                   SETON                                        50
036500070222     C                   ENDIF
036600100525     C                   IF        wDISK  = 'T'
036700100525     C                   SETOFF                                       50
036800100525     C                   SETON                                        51
036900100525     C                   ENDIF
037000100401     C                   MOVEL     *blanks       wDUPREC           1
037100100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037200100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037300100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037400100525     C                   ENDIF
037500100525     C*
037600070222     C                   ENDIF
037700071009     C*
037800071009     C   50              EVAL      VABCTM = '7Q'
037900000801     C*
038000000801     C                   ENDSR
038100091223     C*----------------------------------------------------*
038200091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038300091223     C*----------------------------------------------------*
038400091223     C     REPNSP        BEGSR
038500091223     C*
038600091223     C                   EXSR      INZVAR
038700091223     C                   EXSR      DEFCAM
038800100928     C*
038900100928     C                   SETON                                        60
039000091223     C*
039100091223     C* NSP => Stacco un numeratore da AZNUM
039200091223     C                   clear                   TRUL33DS
039300091223     C                   eval      I33OPE = *zeros
039400091223     C                   eval      I33CNU = 302
039500091223     C                   eval      I33NUM = 1
039600091223     C                   movel     TRUL33DS      KPJBU
039700091223     C                   call      'TRUL33R'
039800091223     C                   parm                    KPJBA
039900091223     C                   movel     KPJBU         TRUL33DS
040000091223     C                   if        O33ERR = *zeros
040100091223     C                   z-add     O33NRF        VABNSP
040200091223     C                   z-add     O33NRF        VATNSP
040300091223     C                   else
040400091223     C                   SETON                                        31
040500091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600091223     C                             + ' ' + 'VABNSP VATNSP'
040700091223     C                   endif
040800091223     C*
040900091223     C                   ENDSR
041000000801     C*----------------------------------------------------*
041100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041200000801     C*----------------------------------------------------*
041300100730     C     IMPVAB        BEGSR
041400070530     C*
041500070530     C                   SETOFF                                       3132
041600070928     C*
041700070928     C                   MOVE(P)   vlrpoi        VABFGS
041800070928     C                   MOVE(P)   vlrpoi        VATFGS
041900070928     C*
042000070928     C                   MOVEL     datcor        VABAAS
042100070928     C                   MOVEL     datcor        VATAAS
042200070928     C                   MOVE      datcor        VABMGS
042300100729     C*
042400100729     C* Reperimento campi ALFA
042500110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
042600110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
042700110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
042800110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
042900110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
043000110127     C*
043100110127     C                   SELECT
043200110201     C                   WHEN      %subst(vindta:207:1) = 'F'
043300110201     C                   EVAL      VABCBO = '1'
043400110201     C                   WHEN      %subst(vindta:207:1) = 'A'
043500110201     C                   EVAL      VABCBO = '2'
043600110127     C                   WHEN      %subst(vindta:199:1) = 'V'
043700110127     C                   EVAL      VABTIC = 'BM'
043800110127     C                   ENDSL
043900110127     C*
044000130911     C* per RMA tolgo gli 0 non significativi
044100130911     C                   EVAL      VABRMA=%trim(%editc(%dec(
044200130911     C                                     %trim(%subst(vindta:108:10))
044300130911     C                                    :15:0):'4'))
044400130911     C*
044500110201     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
044600110201     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
044700100729     C*
044800100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044900100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045000100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045100100729     C* ==
045200100729     C*
045300100729     C* Reperimento campi NUMERICI
045400100729     C                   MOVEL     DATCOR        VABAAS
045500100729     C                   MOVE      DATCOR        VABMGS
045600110127     C* NSP / RMN
045700101119     C                   IF        *IN51 = *off
045800130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
045900100730     C                   EXSR      CHKNUM
046000100730     C                   IF        PiInt=*on
046100100730     C                   Z-ADD     PiVal         VABRMN
046200100928     C  N60              Z-ADD     PiVal         VABNSP
046300100928     C  N60              Z-ADD     PiVal         VATNSP
046400100730     C                   ELSE
046500100730     C                   SETON                                        32
046600100730     C                   Z-ADD     1             VABRMN
046700100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
046900100730     C                   ENDIF
047000101119     C*
047100101119     C                   ELSE
047200101119     C*
047300130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
047400101119     C                   EXSR      CHKNUM
047500101119     C                   IF        PiInt=*on
047600101119     C  N60              Z-ADD     PiVal         VATNSP
047700101119     C                   ELSE
047800101119     C                   SETON                                        32
047900101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000101119     C                             + ' ' + 'VATNSP'
048100101119     C                   ENDIF
048200101119     C                   ENDIF
048300110201     C* NCL
048400110201     C                   z-add     *zeros        wCOLLI            5 0
048500110201     C                   z-add     *zeros        wBANCALI          5 0
048600110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
048700100928     C                   EXSR      CHKNUM
048800100928     C                   IF        PiInt=*on
048900110201     C                   Z-ADD     PiVal         wCOLLI
049000100928     C                   ELSE
049100101119     C  N51              SETON                                        32
049200101119     C  N51              Z-ADD     *zeros        VABNCL
049300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049400100928     C                             + ' ' + 'VABNCL'
049500100928     C                   ENDIF
049600110201     C*
049700131017     C* aggiungo i bancali (se ci sono)
049800131017     C                   IF        %subst(vindta:129:2) <> *blank
049900131017     C                   EVAL      PiStr=%trim(%subst(vindta:129:2))
050000110201     C                   EXSR      CHKNUM
050100110201     C                   IF        PiInt=*on
050200110201     C                   Z-ADD     PiVal         wBANCALI
050300110201     C                   ELSE
050400110201     C  N51              SETON                                        32
050500110201     C  N51              Z-ADD     *zeros        VABNCL
050600110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050700110201     C                             + ' ' + 'VABNCL'
050800110201     C                   ENDIF
050900131017     C                   ENDIF
051000110201     C*
051100110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
051200100729     C* PKB
051300110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
051400100729     C                   EXSR      CHKNUM
051500100729     C                   IF        PiNum=*on
051600110201     C                   Z-ADD     PiVal         VABPKB
051700100729     C                   ELSE
051800101119     C  N51              SETON                                        32
051900101119     C  N51              Z-ADD     *zeros        VABPKB
052000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052100100729     C                             + ' ' + 'VABPKB'
052200100729     C                   ENDIF
052300110201     C* VLB
052400110201     C*                  IF        %subst(vindta:234:11) <> '000000000,0'
052500110201     C*                  EVAL      PiStr=%trim(%subst(vindta:234:11))
052600110201     C*                  EXSR      CHKNUM
052700110201     C*                  IF        PiNum=*on
052800110201     C*                  Z-ADD(H)  PiVal         VABVLB
052900110201     C*                  ELSE
053000110201     C* N51              SETON                                        32
053100110201     C* N51              Z-ADD     *zeros        VABVLB
053200110201     C* N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053300110201     C*                            + ' ' + 'VABVLB'
053400110201     C*                  ENDIF
053500110201     C*                  ENDIF
053600100729     C* CAS
053700110201     C                   IF        %subst(vindta:137:10)<> *blanks    AND
053800110201     C                             %subst(vindta:137:10)<> *zeros     AND
053900110201     C                             %subst(vindta:137:10)<> '0000000,00'
054000100729     C                   EVAL      FlgCAS = '1'
054100110127     C                   EVAL      VABVCA = 'EUR'
054200110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
054300100729     C                   EXSR      CHKNUM
054400100729     C                   IF        PiNum=*on
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
055500110201     C* IAS
055600110201     C                   IF        %subst(vindta:223:11)<> *blanks    AND
055700110201     C                             %subst(vindta:223:11)<> *zeros     AND
055800110201     C                             %subst(vindta:223:11)<> '00000000,00'
055900110201     C                   EVAL      VABVAS = 'EUR'
056000110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
056100110201     C                   EXSR      CHKNUM
056200110201     C                   IF        PiNum=*on
056300110201     C                   Z-ADD(H)  PiVal         VABIAS
056400110201     C                   ELSE
056500110201     C  N51              SETON                                        32
056600110201     C  N51              Z-ADD     *zeros        VABIAS
056700110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056800110201     C                             + ' ' + 'VABIAS'
056900110201     C                   ENDIF
057000110201     C                   ENDIF
057100100729     C*
057200100729     C* Se provincia nn valorizzata la reperisco
057300100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
057400100729     C                   IF        VABNZD  = *blanks AND
057500100729     C                             VABPRD  = *blanks AND
057600100729     C                             VABCAD <> *blanks
057700100729     C                   CLEAR                   TISI95DS
057800100729     C                   EVAL      I95TCN = '3'
057900100729     C                   Z-ADD     datcor        I95DAT
058000100729     C                   EVAL      I95NAR = VABNZD
058100100729     C                   EVAL      I95CAP = VABCAD
058200100729     C                   EVAL      I95LOC = VABLOD
058300100729     C                   CALL      'TISI95R'
058400100729     C                   PARM                    TISI95DS
058500100729     C                   EVAL      VABPRD = O95PRV
058600100729     C                   ENDIF
058700100729     C*
058800100729     C* Considerazioni finali su CBO/CAS
058900100729     C                   IF        FlgCAS = '1'
059000100729     C                   IF        VABCBO = '1'
059100100729     C                   EVAL      VABCBO = '4'
059200100729     C                   ENDIF
059300100729     C                   IF        VABCBO = '2'
059400100729     C                   EVAL      VABCBO = '6'
059500100729     C                   ENDIF
059600100729     C                   ENDIF
059700010202     C*
059800000801     C* Ebbene...
059900000801     C                   ADD       1             §CTRMO
060000070530     C                   IF        *in31 <> *zeros OR
060100070530     C                             *in32 <> *zeros
060200000801     C                   ADD       1             §CTRNO
060300020725     C                   EVAL      x_vinflg = '2'
060400000801     C                   ELSE
060500010201     C                   ADD       1             §CTROKVB
060600000801     C                   ENDIF
060700000801     C*
060800000801     C                   ENDSR
060900100127     C*----------------------------------------------------*
061000100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061100100127     C*----------------------------------------------------*
061200100127     C     WRIVAT_A      BEGSR
061300100127     C*
061400100127     C* Valorizzo l buffer di scrittura del FIVAT
061500100127     C* - TIPO RECORD "A"
061600101119     C***                eval      VATTRC = 'A'
061700101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
061800101119     C***                exsr      wrivat
061900100127     C*
062000100127     C                   ENDSR
062100010201     C*----------------------------------------------------*
062200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
062300010201     C*----------------------------------------------------*
062400071003     C     WRIVAT_B      BEGSR
062500010201     C*
062600021113     C* Valorizzo l buffer di scrittura del FIVAT
062700070928     C* - TIPO RECORD "B"
062800101119     C***                eval      VATTRC = 'B'
062900101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
063000101119     C***                exsr      wrivat
063100010201     C*
063200010201     C                   ENDSR
063300071003     C*----------------------------------------------------*
063400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063500071003     C*----------------------------------------------------*
063600071003     C     WRIVAT_E      BEGSR
063700071003     C*
063800071003     C* Valorizzo l buffer di scrittura del FIVAT
063900071003     C* - TIPO RECORD "E"
064000100729     C*
064100130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
064200130911     C*
064300130911     C*
064400100928     C                   eval      VATTRC = 'E'
064500140515     C                   eval      VATNOT = %trim(%subst(vindta:257:10))
064600140515     C                   exsr      wrivat
064700071003     C*
064800071003     C                   ENDSR
064900100127     C*----------------------------------------------------*
065000100127     C*  SCARICO BUFFER FIVAT
065100100127     C*----------------------------------------------------*
065200100127     C     WRIVAT        BEGSR
065300100127     C*
065400100127     C                   if        vatNOT <> *blanks
065500100127     C                   ADD       1             §CTROKVT
065600100127     C                   write     FIVAT000
065700100127     C                   endif
065800100127     C*
065900100127     C                   ENDSR
066000020904
066100020904
066200010202     C*----------------------------------------------------*
066300100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
066400010202     C*----------------------------------------------------*
066500100525     C     PREVASX       BEGSR
066600100525     C*
066700021113     C* Compongo il nome del membro da dare al FIVATWWR
066800010202     C                   eval      parmbr = vlrhdl
066900010202     C                   movel     'M'           parmbr
067000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
067100010202     C                   eval      paropz = '1'
067200100525     C*
067300010202     C* Effettuo la chiamata al CLLE preposto
067400100525     C  N51              call(e)   'TITVVTC'
067500010202     C                   parm                    parccm
067600010202     C                   parm                    parmbr
067700010202     C                   parm                    paropz
067800100525     C   51              call(e)   'TITVVBC'
067900100525     C                   parm                    parccm
068000100525     C                   parm                    parmbr
068100100525     C                   parm                    paropz
068200100525     C*
068300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068400010202     C                   if        %error
068500010202     C                   movel     '1'           chkcall
068600010202     C                   else
068700010202     C                   movel     '0'           chkcall
068800010202     C                   endif
068900010202     C*
069000010202     C                   ENDSR
069100000801     C*----------------------------------------------------*
069200000801     C*  CONTROLLO NUMERICITA' CAMPI
069300000801     C*----------------------------------------------------*
069400000801     C     CHKNUM        BEGSR
069500081113     C*
069600081113     C                   IF        PiDecChr = *blanks
069700110201     C                   EVAL      PiDecChr = ','
069800081113     C                   ENDIF
069900091223     C*
070000081113     C                   callp(e)  UBISNUM_Check(PiStr
070100081113     C                                          :PiDecChr
070200081113     C                                          :PiVal
070300081113     C                                          :PiNum
070400081113     C                                          :PiInt)
070500081113     C*
070600000801     C                   IF        %error
070700000801     C                   EVAL      PiInt=*off
070800000801     C                   ENDIF
070900000801     C*
071000000801     C                   ENDSR
071100000801     C***
071200011113
071300011113
071400000801
071500000801
071600990920      /TITLE Invio dei dati al punto operativo.
071700010202     C     invio         BEGSR
071800990920     C*
071900021113     C* 1° invio FIVAT
072000010201     C                   reset                   dscmz
072100010201     C                   move      vlrpoi        cmzdst
072200021113     C                   eval      cmzfld = 'FIVATWWR'
072300010201     C                   eval      cmzmbd = vlrhdl
072400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072500021009     C***                if        prmfir = *blanks
072600021113     C                   eval      cmzfla = 'FIVAT00F'
072700021113     C                   eval      cmzmba = 'FIVAT00F'
072800021009     C***                else
072900021009     C***                eval      cmzfla = prmfir
073000021009     C***                eval      cmzmba = prmfir
073100021009     C***                endif
073200010201     C                   eval      cmznrr = *zeros
073300020305     C                   move      §ctrokvt      cmznrr
073400021018     C                   eval      cmzlba = vlrfl1
073500010201     C                   call(e)   'TIS711C'
073600010201     C                   parm                    dscmz
073700010201     C                   parm      *blanks       esito
073800010205     C                   if        %error
073900010205     C                             or cmzerr = '1'
074000010205     C                             or esito  = '1'
074100010205     C                   eval      wrkesito = '3'
074200010205     C                   else
074300010201     C*
074400021113     C* 2° invio FIVAB
074500100525     C                   if        *in51 = *off
074600010201     C                   reset                   dscmz
074700010201     C                   move      vlrpoi        cmzdst
074800010201     C                   eval      cmzfld = vlrfou
074900010201     C                   eval      cmzmbd = vlrhdl
075000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075100021009     C***                if        prmfir = *blanks
075200021113     C                   eval      cmzfla = 'FIVAB00F'
075300021113     C                   eval      cmzmba = 'FIVAB00F'
075400021009     C***                else
075500021009     C***                eval      cmzfla = prmfir
075600021009     C***                eval      cmzmba = prmfir
075700021009     C***                endif
075800010201     C                   eval      cmznrr = *zeros
075900010201     C                   move      §ctrokvb      cmznrr
076000021018     C                   eval      cmzlba = vlrfl1
076100010201     C                   call(e)   'TIS711C'
076200010201     C                   parm                    dscmz
076300010201     C                   parm      *blanks       esito
076400010201     C                   if        %error
076500010201     C                             or cmzerr = '1'
076600010201     C                             or esito  = '1'
076700010201     C                   eval      wrkesito = '3'
076800010201     C                   endif
076900100525     C                   endif
077000010205     C                   endif
077100990920     C*
077200000613     C                   ENDSR
077300000613     C***
077400070411
077500070411     C     *pssr         BEGSR
077600070411     C*
077700070411     C                   if        %open(tivin00r)
077800070411     C                   close     tivin00r
077900070411     C                   endif
078000070411     C                   if        %open(fivabwwr)
078100070411     C                   close     fivabwwr
078200070411     C                   endif
078300070411     C                   if        %open(fivatwwr)
078400070411     C                   close     fivatwwr
078500070411     C                   endif
078600070411     C*
078700070411     C* Effettuo la chiamata al CLLE preposto
078800100525     C  N51              call(e)   'TITVVTC'
078900100525     C                   parm                    parccm
079000100525     C                   parm                    parmbr
079100100525     C                   parm      '2'           paropz
079200100525     C   51              call(e)   'TITVVBC'
079300100525     C                   parm                    parccm
079400100525     C                   parm                    parmbr
079500100525     C                   parm      '2'           paropz
079600070411     C*
079700070411     C                   eval      wrkesito = '2'
079800070411     C*
079900070411     C                   seton                                        LR
080000070411     C*
080100070411     C                   ENDSR     '*CANCL'
080200070411     C***
080300070411
080400990910
080500000613     C     *inzsr        BEGSR
080600990910     C*
080700990910     C     *entry        plist
080800990920     C                   parm                    tivlrds
080900990921     C                   parm      wrkesito      esito
081000000724     C                   parm                    prmlit
081100000710     C                   parm                    prmfir
081200000613     C*
081300000830     C* CALCOLA LA DATA CORRENTE
081400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081500091223     C                   eval      datcor = %dec(%date() : *ISO)
081600000830     C*
081700000613     C                   ENDSR
081800000613     C***
