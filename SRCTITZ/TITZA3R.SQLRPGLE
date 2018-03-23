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
002100110201     D depspe          s             10    INZ(*blanks)
002200110201     D curspe          s             10    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900130911     D IdColloIn       s             15  0
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500100729     D  VINDTA                     2048
003600100729     D  VINFLG                        1
003700120423     D  VINSPE                       10
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
013300110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500110201     C+ order by sped, substr(vindta, 108, 10) desc
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
016100110201     C*
016200110201     C* Skippo il record finale 'EOF'
016300110201     C                   IF        %trim(vindta) = 'EOF'
016400110201     C                   eval      x_vinflg = '1'
016500110201     C                   ELSE
016600010305     C*
016700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016800110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
016900010305     C*
017000100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017100080923     C                   if        *in50 = *off
017200101119     C                   exsr      inzvar
017300101119     C                   exsr      defcam
017400071003     C                   exsr      impvab
017500100525     C                   exsr      wrivab
017600100127     C                   exsr      wrivat_a                                     => carico VAT
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800100525     C   51              exsr      wrivat_e                                     => carico VAT
017900071003     C                   else
018000080923     C*
018100071009     C                   if        wDISK = 'D'
018200091223     C                   exsr      repNSP
018300071009     C                   exsr      impvab
018400100412     C                   exsr      wrivab
018500100127     C                   exsr      wrivat_a                                     => carico VAT
018600071009     C                   exsr      wrivat_b                                     => carico VAT
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
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   else
019900020725     C                   if        depspe <> curspe                             => rottura di spediz
020000010305     C                   eval      depspe = curspe                              => memorizz. spediz
020100100412     C                   exsr      wrivab
020200080117     C                   clear                   tivinds
020300091223     C                   exsr      repNSP
020400020305     C                   exsr      impvab
020500100127     C                   exsr      wrivat_a                                     => carico VAT
020600071003     C                   exsr      wrivat_b                                     => carico VAT
020700071003     C   50              exsr      wrivat_e                                     => carico VAT
020800020305     C                   else                                                   => x stessa spediz
020900100401     C*
021000100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021100100928     C                   select
021200100928     C                   when      wDUPREC = ' '
021300020305     C                   exsr      impvab
021400100127     C                   exsr      wrivat_a                                     => carico VAT
021500071003     C                   exsr      wrivat_b                                     => carico VAT
021600071003     C   50              exsr      wrivat_e                                     => carico VAT
021700100928     C                   when      wDUPREC = 'C'
021800100928     C   50              exsr      wrivat_e                                     => carico VAT
021900100928     C                   endsl
022000010305     C                   endif
022100010305     C                   endif
022200010305     C                   endif
022300071003     C                   endif
022400110201     C*
022500110201     C                   endif
022600071009     C                   endif
022700000905     C*
022800000905     C                   else
022900020725     C                   eval      x_vinflg = '1'
023000000905     C                   endif
023100000905     C*
023200020725     C     VINRRN        chain     tivin000
023300020725     C                   if        %found(tivin00r)
023400020725     C                   eval      y_vinflg = x_vinflg
023500020725     C                   eval      y_vinmsg = x_vinmsg
023600020725     C                   update    tivin000
023700020725     C                   endif
023800020725     C*
023900020725     C/EXEC SQL
024000070530     C+ Fetch C1 into :INPUT_DS
024100020725     C/END-EXEC
024200020725     C*
024300020725     C                   enddo
024400020725     C*
024500020725     C/EXEC SQL
024600020725     C+ close C1
024700020725     C/END-EXEC
024800000905     C*
024900020305     C* Scarico i VAB rimasti "in sospeso"
025000071009     C                   if        wDISK = 'C'
025100100412     C                   exsr      wrivab
025200071009     C                   endif
025300010202     C*
025400010202     C                   endif
025500990910
025600990910     C* Se non ci sono record con errori ...
025700000710     C                   if        §ctrno = 0
025800990910     C* ... restituisco esito OK.
025900990921     C                   eval      wrkesito = '0'
026000990910     C                   else
026100100525     C                   if        §ctrokvb > 0 OR
026200100525     C                             §ctrokvt > 0
026300990921     C                   eval      wrkesito = '1'
026400000710     C                   else
026500000710     C                   eval      wrkesito = '2'
026600990910     C                   endif
026700000710     C                   endif
026800990910     C*
026900990914     C                   if        %open(tivin00r)
027000990908     C                   close     tivin00r
027100990914     C                   endif
027200021113     C                   if        %open(fivabwwr)
027300021113     C                   close     fivabwwr
027400990914     C                   endif
027500021113     C                   if        %open(fivatwwr)
027600021113     C                   close     fivatwwr
027700010201     C                   endif
027800990910     C*
027900100525     C                   if        §ctrokvb > 0 OR
028000100525     C                             §ctrokvt > 0
028100000724     C                             and vlrpoi <> *zeros
028200010202     C                   exsr      invio
028300990920     C                   endif
028400990920     C*
028500910830     C                   ENDSR
028600000613     C***
028700100412
028800100412
028900010305
029000010305     C*----------------------------------------------------*
029100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029200010305     C*----------------------------------------------------*
029300020305     C     WRIVAB        BEGSR
029400100412     C*
029500100412     C* Gestisco l'eventuale rottura x numero spedizione
029600100412     C                   if        VABRMA = *blanks
029700100412     C                   movel(P)  VABRMN        VABRMA
029800100412     C                   endif
029900100412     C*
030000100525     C                   if        *in51 = *off and *in31 = *off
030100100525     C                   write     fivab000                                     => scarico il VAB
030200100525     C                   endif
030300010305     C*
030400010305     C                   ENDSR
030500990920
030600000801     C*----------------------------------------------------*
030700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030800000801     C*----------------------------------------------------*
030900010201     C     INZVAR        BEGSR
031000000801     C*
031100010201     C                   Z-ADD     *zeros        Num5_0
031200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031300020725     C                   MOVEL     '0'           FlgCAS            1
031400000801     C*
031500000801     C                   ENDSR
031600000801     C*----------------------------------------------------*
031700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031800000801     C*----------------------------------------------------*
031900020904     C     DEFCAM        BEGSR
032000080923     C*
032100100928     C                   SETOFF                                       505160
032200000801     C*
032300021113     C                   CLEAR                   FIVAB000
032400021113     C                   CLEAR                   FIVAT000
032500070730     C* Imposto i valori di default...
032600131017     C                   EVAL      VABCCM = 0201114
032700131017     C                   EVAL      VATCCM = 0201114
032800130911     C                   EVAL      VABLNP = 020
032900130911     C                   EVAL      VATLNP = 020
033000110128     C                   EVAL      VABCTR = 000
033100100525     C                   EVAL      VABCBO = '1'
033200070222     C* ... e poi verifico se sono stati passati come parametri
033300070222     C                   IF        vlrppt > *blanks
033400100525     C*
033500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033700070222     C                   EXSR      CHKNUM
033800070222     C                   IF        PiInt=*on
033900070222     C                   Z-ADD     PiVal         VABCCM
034000070222     C                   Z-ADD     PiVal         VATCCM
034100070222     C                   ENDIF
034200070222     C                   ENDIF
034300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034500070222     C                   EXSR      CHKNUM
034600070222     C                   IF        PiInt=*on
034700070222     C                   Z-ADD     PiVal         VABLNP
034800070222     C                   Z-ADD     PiVal         VATLNP
034900070222     C                   ENDIF
035000070222     C                   ENDIF
035100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035300070222     C                   EXSR      CHKNUM
035400070222     C                   IF        PiInt=*on
035500070222     C                   Z-ADD     PiVal         VABCTR
035600070222     C                   ENDIF
035700070928     C                   ENDIF
035800071009     C                   MOVEL     *blanks       wDISK             1
035900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036000071009     C                   IF        wDISK <> *blanks
036100070928     C                   SETON                                        50
036200070222     C                   ENDIF
036300100525     C                   IF        wDISK  = 'T'
036400100525     C                   SETOFF                                       50
036500100525     C                   SETON                                        51
036600100525     C                   ENDIF
036700100401     C                   MOVEL     *blanks       wDUPREC           1
036800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037100100525     C                   ENDIF
037200100525     C*
037300070222     C                   ENDIF
037400071009     C*
037500071009     C   50              EVAL      VABCTM = '7Q'
037600000801     C*
037700000801     C                   ENDSR
037800091223     C*----------------------------------------------------*
037900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038000091223     C*----------------------------------------------------*
038100091223     C     REPNSP        BEGSR
038200091223     C*
038300091223     C                   EXSR      INZVAR
038400091223     C                   EXSR      DEFCAM
038500100928     C*
038600100928     C                   SETON                                        60
038700091223     C*
038800091223     C* NSP => Stacco un numeratore da AZNUM
038900091223     C                   clear                   TRUL33DS
039000091223     C                   eval      I33OPE = *zeros
039100091223     C                   eval      I33CNU = 302
039200091223     C                   eval      I33NUM = 1
039300091223     C                   movel     TRUL33DS      KPJBU
039400091223     C                   call      'TRUL33R'
039500091223     C                   parm                    KPJBA
039600091223     C                   movel     KPJBU         TRUL33DS
039700091223     C                   if        O33ERR = *zeros
039800091223     C                   z-add     O33NRF        VABNSP
039900091223     C                   z-add     O33NRF        VATNSP
040000091223     C                   else
040100091223     C                   SETON                                        31
040200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040300091223     C                             + ' ' + 'VABNSP VATNSP'
040400091223     C                   endif
040500091223     C*
040600091223     C                   ENDSR
040700000801     C*----------------------------------------------------*
040800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040900000801     C*----------------------------------------------------*
041000100730     C     IMPVAB        BEGSR
041100070530     C*
041200070530     C                   SETOFF                                       3132
041300070928     C*
041400070928     C                   MOVE(P)   vlrpoi        VABFGS
041500070928     C                   MOVE(P)   vlrpoi        VATFGS
041600070928     C*
041700070928     C                   MOVEL     datcor        VABAAS
041800070928     C                   MOVEL     datcor        VATAAS
041900070928     C                   MOVE      datcor        VABMGS
042000100729     C*
042100100729     C* Reperimento campi ALFA
042200110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
042300110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
042400110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
042500110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
042600110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
042700110127     C*
042800110127     C                   SELECT
042900110201     C                   WHEN      %subst(vindta:207:1) = 'F'
043000110201     C                   EVAL      VABCBO = '1'
043100110201     C                   WHEN      %subst(vindta:207:1) = 'A'
043200110201     C                   EVAL      VABCBO = '2'
043300110127     C                   WHEN      %subst(vindta:199:1) = 'V'
043400110127     C                   EVAL      VABTIC = 'BM'
043500110127     C                   ENDSL
043600110127     C*
043700130911     C* per RMA tolgo gli 0 non significativi
043800130911     C                   EVAL      VABRMA=%trim(%editc(%dec(
043900130911     C                                     %trim(%subst(vindta:108:10))
044000130911     C                                    :15:0):'4'))
044100130911     C*
044200110201     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
044300110201     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
044400100729     C*
044500100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
044600100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044700100729     C     '@':'A'       XLATE     VABRSD        VABRSD
044800100729     C* ==
044900100729     C*
045000100729     C* Reperimento campi NUMERICI
045100100729     C                   MOVEL     DATCOR        VABAAS
045200100729     C                   MOVE      DATCOR        VABMGS
045300110127     C* NSP / RMN
045400101119     C                   IF        *IN51 = *off
045500130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
045600100730     C                   EXSR      CHKNUM
045700100730     C                   IF        PiInt=*on
045800100730     C                   Z-ADD     PiVal         VABRMN
045900100928     C  N60              Z-ADD     PiVal         VABNSP
046000100928     C  N60              Z-ADD     PiVal         VATNSP
046100100730     C                   ELSE
046200100730     C                   SETON                                        32
046300100730     C                   Z-ADD     1             VABRMN
046400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046500101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
046600100730     C                   ENDIF
046700101119     C*
046800101119     C                   ELSE
046900101119     C*
047000130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
047100101119     C                   EXSR      CHKNUM
047200101119     C                   IF        PiInt=*on
047300101119     C  N60              Z-ADD     PiVal         VATNSP
047400101119     C                   ELSE
047500101119     C                   SETON                                        32
047600101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700101119     C                             + ' ' + 'VATNSP'
047800101119     C                   ENDIF
047900101119     C                   ENDIF
048000110201     C* NCL
048100110201     C                   z-add     *zeros        wCOLLI            5 0
048200110201     C                   z-add     *zeros        wBANCALI          5 0
048300110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
048400100928     C                   EXSR      CHKNUM
048500100928     C                   IF        PiInt=*on
048600110201     C                   Z-ADD     PiVal         wCOLLI
048700100928     C                   ELSE
048800101119     C  N51              SETON                                        32
048900101119     C  N51              Z-ADD     *zeros        VABNCL
049000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049100100928     C                             + ' ' + 'VABNCL'
049200100928     C                   ENDIF
049300110201     C*
049400131017     C* aggiungo i bancali (se ci sono)
049500131017     C                   IF        %subst(vindta:129:2) <> *blank
049600131017     C                   EVAL      PiStr=%trim(%subst(vindta:129:2))
049700110201     C                   EXSR      CHKNUM
049800110201     C                   IF        PiInt=*on
049900110201     C                   Z-ADD     PiVal         wBANCALI
050000110201     C                   ELSE
050100110201     C  N51              SETON                                        32
050200110201     C  N51              Z-ADD     *zeros        VABNCL
050300110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050400110201     C                             + ' ' + 'VABNCL'
050500110201     C                   ENDIF
050600131017     C                   ENDIF
050700110201     C*
050800110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
050900100729     C* PKB
051000110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
051100100729     C                   EXSR      CHKNUM
051200100729     C                   IF        PiNum=*on
051300110201     C                   Z-ADD     PiVal         VABPKB
051400100729     C                   ELSE
051500101119     C  N51              SETON                                        32
051600101119     C  N51              Z-ADD     *zeros        VABPKB
051700101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051800100729     C                             + ' ' + 'VABPKB'
051900100729     C                   ENDIF
052000110201     C* VLB
052100110201     C*                  IF        %subst(vindta:234:11) <> '000000000,0'
052200110201     C*                  EVAL      PiStr=%trim(%subst(vindta:234:11))
052300110201     C*                  EXSR      CHKNUM
052400110201     C*                  IF        PiNum=*on
052500110201     C*                  Z-ADD(H)  PiVal         VABVLB
052600110201     C*                  ELSE
052700110201     C* N51              SETON                                        32
052800110201     C* N51              Z-ADD     *zeros        VABVLB
052900110201     C* N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053000110201     C*                            + ' ' + 'VABVLB'
053100110201     C*                  ENDIF
053200110201     C*                  ENDIF
053300100729     C* CAS
053400110201     C                   IF        %subst(vindta:137:10)<> *blanks    AND
053500110201     C                             %subst(vindta:137:10)<> *zeros     AND
053600110201     C                             %subst(vindta:137:10)<> '0000000,00'
053700100729     C                   EVAL      FlgCAS = '1'
053800110127     C                   EVAL      VABVCA = 'EUR'
053900110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
054000100729     C                   EXSR      CHKNUM
054100100729     C                   IF        PiNum=*on
054200100928     C                   Z-ADD(H)  PiVal         VABCAS
054300100729     C                   ELSE
054400101119     C  N51              SETON                                        32
054500101119     C  N51              Z-ADD     *zeros        VABCAS
054600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
054700100729     C                             + ' ' + 'VABCAS'
054800100729     C                   ENDIF
054900101119     C                   ELSE
055000101119     C                   EVAL      FlgCAS = '0'
055100100729     C                   ENDIF
055200110201     C* IAS
055300110201     C                   IF        %subst(vindta:223:11)<> *blanks    AND
055400110201     C                             %subst(vindta:223:11)<> *zeros     AND
055500110201     C                             %subst(vindta:223:11)<> '00000000,00'
055600110201     C                   EVAL      VABVAS = 'EUR'
055700110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
055800110201     C                   EXSR      CHKNUM
055900110201     C                   IF        PiNum=*on
056000110201     C                   Z-ADD(H)  PiVal         VABIAS
056100110201     C                   ELSE
056200110201     C  N51              SETON                                        32
056300110201     C  N51              Z-ADD     *zeros        VABIAS
056400110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056500110201     C                             + ' ' + 'VABIAS'
056600110201     C                   ENDIF
056700110201     C                   ENDIF
056800100729     C*
056900100729     C* Se provincia nn valorizzata la reperisco
057000100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
057100100729     C                   IF        VABNZD  = *blanks AND
057200100729     C                             VABPRD  = *blanks AND
057300100729     C                             VABCAD <> *blanks
057400100729     C                   CLEAR                   TISI95DS
057500100729     C                   EVAL      I95TCN = '3'
057600100729     C                   Z-ADD     datcor        I95DAT
057700100729     C                   EVAL      I95NAR = VABNZD
057800100729     C                   EVAL      I95CAP = VABCAD
057900100729     C                   EVAL      I95LOC = VABLOD
058000100729     C                   CALL      'TISI95R'
058100100729     C                   PARM                    TISI95DS
058200100729     C                   EVAL      VABPRD = O95PRV
058300100729     C                   ENDIF
058400100729     C*
058500100729     C* Considerazioni finali su CBO/CAS
058600100729     C                   IF        FlgCAS = '1'
058700100729     C                   IF        VABCBO = '1'
058800100729     C                   EVAL      VABCBO = '4'
058900100729     C                   ENDIF
059000100729     C                   IF        VABCBO = '2'
059100100729     C                   EVAL      VABCBO = '6'
059200100729     C                   ENDIF
059300100729     C                   ENDIF
059400010202     C*
059500000801     C* Ebbene...
059600000801     C                   ADD       1             §CTRMO
059700070530     C                   IF        *in31 <> *zeros OR
059800070530     C                             *in32 <> *zeros
059900000801     C                   ADD       1             §CTRNO
060000020725     C                   EVAL      x_vinflg = '2'
060100000801     C                   ELSE
060200010201     C                   ADD       1             §CTROKVB
060300000801     C                   ENDIF
060400000801     C*
060500000801     C                   ENDSR
060600100127     C*----------------------------------------------------*
060700100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
060800100127     C*----------------------------------------------------*
060900100127     C     WRIVAT_A      BEGSR
061000100127     C*
061100100127     C* Valorizzo l buffer di scrittura del FIVAT
061200100127     C* - TIPO RECORD "A"
061300101119     C***                eval      VATTRC = 'A'
061400101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
061500101119     C***                exsr      wrivat
061600100127     C*
061700100127     C                   ENDSR
061800010201     C*----------------------------------------------------*
061900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
062000010201     C*----------------------------------------------------*
062100071003     C     WRIVAT_B      BEGSR
062200010201     C*
062300021113     C* Valorizzo l buffer di scrittura del FIVAT
062400070928     C* - TIPO RECORD "B"
062500101119     C***                eval      VATTRC = 'B'
062600101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
062700101119     C***                exsr      wrivat
062800010201     C*
062900010201     C                   ENDSR
063000071003     C*----------------------------------------------------*
063100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063200071003     C*----------------------------------------------------*
063300071003     C     WRIVAT_E      BEGSR
063400071003     C*
063500071003     C* Valorizzo l buffer di scrittura del FIVAT
063600071003     C* - TIPO RECORD "E"
063700100729     C*
063800130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
063900130911     C*
064000130911     C*
064100100928     C                   eval      VATTRC = 'E'
064200130911     C***                eval      VATNOT = %trim(%subst(vindta:459:8))
064300130911     C                   eval      IdColloIn = %dec((%subst(vindta:897:15))
064400130911     C                                          : 15 : 0)
064500130911     C*
064600130911     C     1             DO        VABNCL
064700130911     C                   eval      IdColloIn = IdColloIn + 1
064800130911     C                   EVAL      VATNOT = %trim(%editc(IdColloIn-1:'4'))
064900130911     C*
065000100928     C                   exsr      wrivat
065100130911     C                   ENDDO
065200071003     C*
065300071003     C                   ENDSR
065400100127     C*----------------------------------------------------*
065500100127     C*  SCARICO BUFFER FIVAT
065600100127     C*----------------------------------------------------*
065700100127     C     WRIVAT        BEGSR
065800100127     C*
065900100127     C                   if        vatNOT <> *blanks
066000100127     C                   ADD       1             §CTROKVT
066100100127     C                   write     FIVAT000
066200100127     C                   endif
066300100127     C*
066400100127     C                   ENDSR
066500020904
066600020904
066700010202     C*----------------------------------------------------*
066800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
066900010202     C*----------------------------------------------------*
067000100525     C     PREVASX       BEGSR
067100100525     C*
067200021113     C* Compongo il nome del membro da dare al FIVATWWR
067300010202     C                   eval      parmbr = vlrhdl
067400010202     C                   movel     'M'           parmbr
067500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
067600010202     C                   eval      paropz = '1'
067700100525     C*
067800010202     C* Effettuo la chiamata al CLLE preposto
067900100525     C  N51              call(e)   'TITVVTC'
068000010202     C                   parm                    parccm
068100010202     C                   parm                    parmbr
068200010202     C                   parm                    paropz
068300100525     C   51              call(e)   'TITVVBC'
068400100525     C                   parm                    parccm
068500100525     C                   parm                    parmbr
068600100525     C                   parm                    paropz
068700100525     C*
068800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068900010202     C                   if        %error
069000010202     C                   movel     '1'           chkcall
069100010202     C                   else
069200010202     C                   movel     '0'           chkcall
069300010202     C                   endif
069400010202     C*
069500010202     C                   ENDSR
069600000801     C*----------------------------------------------------*
069700000801     C*  CONTROLLO NUMERICITA' CAMPI
069800000801     C*----------------------------------------------------*
069900000801     C     CHKNUM        BEGSR
070000081113     C*
070100081113     C                   IF        PiDecChr = *blanks
070200110201     C                   EVAL      PiDecChr = ','
070300081113     C                   ENDIF
070400091223     C*
070500081113     C                   callp(e)  UBISNUM_Check(PiStr
070600081113     C                                          :PiDecChr
070700081113     C                                          :PiVal
070800081113     C                                          :PiNum
070900081113     C                                          :PiInt)
071000081113     C*
071100000801     C                   IF        %error
071200000801     C                   EVAL      PiInt=*off
071300000801     C                   ENDIF
071400000801     C*
071500000801     C                   ENDSR
071600000801     C***
071700011113
071800011113
071900000801
072000000801
072100990920      /TITLE Invio dei dati al punto operativo.
072200010202     C     invio         BEGSR
072300990920     C*
072400021113     C* 1° invio FIVAT
072500010201     C                   reset                   dscmz
072600010201     C                   move      vlrpoi        cmzdst
072700021113     C                   eval      cmzfld = 'FIVATWWR'
072800010201     C                   eval      cmzmbd = vlrhdl
072900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073000021009     C***                if        prmfir = *blanks
073100021113     C                   eval      cmzfla = 'FIVAT00F'
073200021113     C                   eval      cmzmba = 'FIVAT00F'
073300021009     C***                else
073400021009     C***                eval      cmzfla = prmfir
073500021009     C***                eval      cmzmba = prmfir
073600021009     C***                endif
073700010201     C                   eval      cmznrr = *zeros
073800020305     C                   move      §ctrokvt      cmznrr
073900021018     C                   eval      cmzlba = vlrfl1
074000010201     C                   call(e)   'TIS711C'
074100010201     C                   parm                    dscmz
074200010201     C                   parm      *blanks       esito
074300010205     C                   if        %error
074400010205     C                             or cmzerr = '1'
074500010205     C                             or esito  = '1'
074600010205     C                   eval      wrkesito = '3'
074700010205     C                   else
074800010201     C*
074900021113     C* 2° invio FIVAB
075000100525     C                   if        *in51 = *off
075100010201     C                   reset                   dscmz
075200010201     C                   move      vlrpoi        cmzdst
075300010201     C                   eval      cmzfld = vlrfou
075400010201     C                   eval      cmzmbd = vlrhdl
075500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075600021009     C***                if        prmfir = *blanks
075700021113     C                   eval      cmzfla = 'FIVAB00F'
075800021113     C                   eval      cmzmba = 'FIVAB00F'
075900021009     C***                else
076000021009     C***                eval      cmzfla = prmfir
076100021009     C***                eval      cmzmba = prmfir
076200021009     C***                endif
076300010201     C                   eval      cmznrr = *zeros
076400010201     C                   move      §ctrokvb      cmznrr
076500021018     C                   eval      cmzlba = vlrfl1
076600010201     C                   call(e)   'TIS711C'
076700010201     C                   parm                    dscmz
076800010201     C                   parm      *blanks       esito
076900010201     C                   if        %error
077000010201     C                             or cmzerr = '1'
077100010201     C                             or esito  = '1'
077200010201     C                   eval      wrkesito = '3'
077300010201     C                   endif
077400100525     C                   endif
077500010205     C                   endif
077600990920     C*
077700000613     C                   ENDSR
077800000613     C***
077900070411
078000070411     C     *pssr         BEGSR
078100070411     C*
078200070411     C                   if        %open(tivin00r)
078300070411     C                   close     tivin00r
078400070411     C                   endif
078500070411     C                   if        %open(fivabwwr)
078600070411     C                   close     fivabwwr
078700070411     C                   endif
078800070411     C                   if        %open(fivatwwr)
078900070411     C                   close     fivatwwr
079000070411     C                   endif
079100070411     C*
079200070411     C* Effettuo la chiamata al CLLE preposto
079300100525     C  N51              call(e)   'TITVVTC'
079400100525     C                   parm                    parccm
079500100525     C                   parm                    parmbr
079600100525     C                   parm      '2'           paropz
079700100525     C   51              call(e)   'TITVVBC'
079800100525     C                   parm                    parccm
079900100525     C                   parm                    parmbr
080000100525     C                   parm      '2'           paropz
080100070411     C*
080200070411     C                   eval      wrkesito = '2'
080300070411     C*
080400070411     C                   seton                                        LR
080500070411     C*
080600070411     C                   ENDSR     '*CANCL'
080700070411     C***
080800070411
080900990910
081000000613     C     *inzsr        BEGSR
081100990910     C*
081200990910     C     *entry        plist
081300990920     C                   parm                    tivlrds
081400990921     C                   parm      wrkesito      esito
081500000724     C                   parm                    prmlit
081600000710     C                   parm                    prmfir
081700000613     C*
081800000830     C* CALCOLA LA DATA CORRENTE
081900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082000091223     C                   eval      datcor = %dec(%date() : *ISO)
082100000830     C*
082200000613     C                   ENDSR
082300000613     C***
