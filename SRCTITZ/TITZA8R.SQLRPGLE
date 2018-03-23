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
003300161102     D w70             s             70
003400020725
003500020725     D*------------------
003600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003700020725     D*------------------
003800070530     D INPUT_DS        DS                  INZ
003900100729     D  VINDTA                     2048
004000100729     D  VINFLG                        1
004100120423     D  VINSPE                       10
004200100729     D  VINRRN                        8  0
004300020725     D*
004400081113
004500091223     D*------------------
004600091223     D* DS REPERIMENTO NUMERATORE
004700091223     D*------------------
004800100309     D trul33ds      e ds                  inz
004900091223     D*------------------
005000091223     D* DS ARCHITETTURA
005100091223     D*------------------
005200091223     D kpjba         e ds                  inz
005300091223
005400101119
005500101119     D*-------------------
005600101119     D* COSTANTI
005700101119     D*-------------------
005800101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005900101119     D                                     èéù')
006000101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006100101119     D                                     EEU')
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000613     C*
008300100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700100525     C  N51              call(e)   'TITVVTC'
008800100525     C                   parm                    parccm
008900100525     C                   parm                    parmbr
009000100525     C                   parm      '2'           paropz
009100100525     C   51              call(e)   'TITVVBC'
009200100525     C                   parm                    parccm
009300100525     C                   parm                    parmbr
009400100525     C                   parm      '2'           paropz
009500070730     C*
009600070730     C* Effettuo lancio TISI95 solo x chiusura
009700070730     C                   CLEAR                   TISI95DS
009800070730     C                   EVAL      I95TLA = 'C'
009900070730     C                   CALL      'TISI95R'
010000070730     C                   PARM                    TISI95DS
010100000616     C*
010200000801     C
010300010201     C                   seton                                        LR
010400000801
010500910830     C*--------------------------------------------------------
010600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010700910830     C*--------------------------------------------------------
010800070530     C     RWFILE        BEGSR
010900990910     C*
011000990914     C                   if        not %open(tivin00r)
011100990908     C                   open      tivin00r
011200990914     C                   endif
011300070530     C*
011400100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011500100525     C                   exsr      prevasx
011600010201     C*
011700010202     C                   if        chkcall = '0'
011800010202     C*
011900100525     C                   if        not %open(fivabwwr)
012000100525     C                   open      fivabwwr
012100100525     C                   endif
012200100525     C*
012300021113     C                   if        not %open(fivatwwr)
012400021113     C                   open      fivatwwr
012500010201     C                   endif
012600080117     C*
012700080117     C                   EXSR      INZVAR
012800080117     C                   EXSR      DEFCAM
012900990910     C*
013000010201     C                   clear                   §CTROKVB          5 0
013100020305     C                   clear                   §CTROKVT          5 0
013200000801     C                   clear                   §CTRMO            5 0
013300000801     C                   clear                   §CTRNO            5 0
013400990910     C*
013500020725     C/EXEC SQL
013600020725     C+ declare C1 cursor for select
013700110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013800060223     C+ from tivin00r
013900110201     C+ order by sped, substr(vindta, 108, 10) desc
014000020725     C+ for read only
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400020725     C+ open C1
014500020725     C/END-EXEC
014600020725     C
014700020725     C/EXEC SQL
014800070530     C+ Fetch C1 into :INPUT_DS
014900020725     C/END-EXEC
015000020725     C*
015100020725     C                   dow       sqlcod = *zeros
015200990913     C*
015300100310     C                   if        vindta > *blanks
015400000613     C                   add       1             rrnum
015500000801     C*
015600020725     C                   if        vinflg = *blanks
015700020725     C                             or vinflg = '0'
015800020725     C                             or vinflg = '2'
015900000801     C*
016000020725     C                   clear                   x_vinmsg
016100020725     C                   eval      x_vinflg = '1'
016200101119     C*
016300101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016400101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016500110201     C*
016600110201     C* Skippo il record finale 'EOF'
016700110201     C                   IF        %trim(vindta) = 'EOF'
016800110201     C                   eval      x_vinflg = '1'
016900110201     C                   ELSE
017000010305     C*
017100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017200110201     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
017300010305     C*
017400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017500080923     C                   if        *in50 = *off
017600101119     C                   exsr      inzvar
017700101119     C                   exsr      defcam
017800071003     C                   exsr      impvab
017900100525     C                   exsr      wrivab
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071003     C                   exsr      wrivat_b                                     => carico VAT
018200100525     C   51              exsr      wrivat_e                                     => carico VAT
018300161102     C                   exsr      wrivat_ij                                    => carico VAT
018400071003     C                   else
018500080923     C*
018600071009     C                   if        wDISK = 'D'
018700091223     C                   exsr      repNSP
018800071009     C                   exsr      impvab
018900100412     C                   exsr      wrivab
019000100127     C                   exsr      wrivat_a                                     => carico VAT
019100071009     C                   exsr      wrivat_b                                     => carico VAT
019200071009     C                   exsr      wrivat_e                                     => carico VAT
019300161102     C                   exsr      wrivat_ij                                    => carico VAT
019400071009     C                   else
019500071009     C*
019600010305     C                   if        depspe = *blanks                             => 1° giro
019700010305     C                   eval      depspe = curspe                              => memorizz. spediz
019800080117     C                   clear                   tivinds
019900091223     C                   exsr      repNSP
020000020305     C                   exsr      impvab
020100100127     C                   exsr      wrivat_a                                     => carico VAT
020200071003     C                   exsr      wrivat_b                                     => carico VAT
020300071003     C   50              exsr      wrivat_e                                     => carico VAT
020400161102     C                   exsr      wrivat_ij                                    => carico VAT
020500010305     C                   else
020600020725     C                   if        depspe <> curspe                             => rottura di spediz
020700010305     C                   eval      depspe = curspe                              => memorizz. spediz
020800100412     C                   exsr      wrivab
020900080117     C                   clear                   tivinds
021000091223     C                   exsr      repNSP
021100020305     C                   exsr      impvab
021200100127     C                   exsr      wrivat_a                                     => carico VAT
021300071003     C                   exsr      wrivat_b                                     => carico VAT
021400071003     C   50              exsr      wrivat_e                                     => carico VAT
021500161102     C                   exsr      wrivat_ij                                    => carico VAT
021600020305     C                   else                                                   => x stessa spediz
021700100401     C*
021800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021900100928     C                   select
022000100928     C                   when      wDUPREC = ' '
022100020305     C                   exsr      impvab
022200100127     C                   exsr      wrivat_a                                     => carico VAT
022300071003     C                   exsr      wrivat_b                                     => carico VAT
022400071003     C   50              exsr      wrivat_e                                     => carico VAT
022500161102     C                   exsr      wrivat_ij                                    => carico VAT
022600100928     C                   when      wDUPREC = 'C'
022700100928     C   50              exsr      wrivat_e                                     => carico VAT
022800100928     C                   endsl
022900010305     C                   endif
023000010305     C                   endif
023100010305     C                   endif
023200071003     C                   endif
023300110201     C*
023400110201     C                   endif
023500071009     C                   endif
023600000905     C*
023700000905     C                   else
023800020725     C                   eval      x_vinflg = '1'
023900000905     C                   endif
024000000905     C*
024100020725     C     VINRRN        chain     tivin000
024200020725     C                   if        %found(tivin00r)
024300020725     C                   eval      y_vinflg = x_vinflg
024400020725     C                   eval      y_vinmsg = x_vinmsg
024500020725     C                   update    tivin000
024600020725     C                   endif
024700020725     C*
024800020725     C/EXEC SQL
024900070530     C+ Fetch C1 into :INPUT_DS
025000020725     C/END-EXEC
025100020725     C*
025200020725     C                   enddo
025300020725     C*
025400020725     C/EXEC SQL
025500020725     C+ close C1
025600020725     C/END-EXEC
025700000905     C*
025800020305     C* Scarico i VAB rimasti "in sospeso"
025900071009     C                   if        wDISK = 'C'
026000100412     C                   exsr      wrivab
026100071009     C                   endif
026200010202     C*
026300010202     C                   endif
026400990910
026500990910     C* Se non ci sono record con errori ...
026600000710     C                   if        §ctrno = 0
026700990910     C* ... restituisco esito OK.
026800990921     C                   eval      wrkesito = '0'
026900990910     C                   else
027000100525     C                   if        §ctrokvb > 0 OR
027100100525     C                             §ctrokvt > 0
027200990921     C                   eval      wrkesito = '1'
027300000710     C                   else
027400000710     C                   eval      wrkesito = '2'
027500990910     C                   endif
027600000710     C                   endif
027700990910     C*
027800990914     C                   if        %open(tivin00r)
027900990908     C                   close     tivin00r
028000990914     C                   endif
028100021113     C                   if        %open(fivabwwr)
028200021113     C                   close     fivabwwr
028300990914     C                   endif
028400021113     C                   if        %open(fivatwwr)
028500021113     C                   close     fivatwwr
028600010201     C                   endif
028700990910     C*
028800100525     C                   if        §ctrokvb > 0 OR
028900100525     C                             §ctrokvt > 0
029000000724     C                             and vlrpoi <> *zeros
029100010202     C                   exsr      invio
029200990920     C                   endif
029300990920     C*
029400910830     C                   ENDSR
029500000613     C***
029600100412
029700100412
029800010305
029900010305     C*----------------------------------------------------*
030000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030100010305     C*----------------------------------------------------*
030200020305     C     WRIVAB        BEGSR
030300100412     C*
030400100412     C* Gestisco l'eventuale rottura x numero spedizione
030500100412     C                   if        VABRMA = *blanks
030600100412     C                   movel(P)  VABRMN        VABRMA
030700100412     C                   endif
030800100412     C*
030900100525     C                   if        *in51 = *off and *in31 = *off
031000100525     C                   write     fivab000                                     => scarico il VAB
031100100525     C                   endif
031200010305     C*
031300010305     C                   ENDSR
031400990920
031500000801     C*----------------------------------------------------*
031600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031700000801     C*----------------------------------------------------*
031800010201     C     INZVAR        BEGSR
031900000801     C*
032000010201     C                   Z-ADD     *zeros        Num5_0
032100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032200020725     C                   MOVEL     '0'           FlgCAS            1
032300000801     C*
032400000801     C                   ENDSR
032500000801     C*----------------------------------------------------*
032600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032700000801     C*----------------------------------------------------*
032800020904     C     DEFCAM        BEGSR
032900080923     C*
033000100928     C                   SETOFF                                       505160
033100000801     C*
033200021113     C                   CLEAR                   FIVAB000
033300021113     C                   CLEAR                   FIVAT000
033400070730     C* Imposto i valori di default...
033500140128     C                   EVAL      VABCCM = 0436322
033600140128     C                   EVAL      VATCCM = 0436322
033700140128     C                   EVAL      VABLNP = 043
033800140128     C                   EVAL      VATLNP = 043
033900110128     C                   EVAL      VABCTR = 000
034000100525     C                   EVAL      VABCBO = '1'
034100070222     C* ... e poi verifico se sono stati passati come parametri
034200070222     C                   IF        vlrppt > *blanks
034300100525     C*
034400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABCCM
034900070222     C                   Z-ADD     PiVal         VATCCM
035000070222     C                   ENDIF
035100070222     C                   ENDIF
035200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABLNP
035700070222     C                   Z-ADD     PiVal         VATLNP
035800070222     C                   ENDIF
035900070222     C                   ENDIF
036000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCTR
036500070222     C                   ENDIF
036600070928     C                   ENDIF
036700071009     C                   MOVEL     *blanks       wDISK             1
036800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036900071009     C                   IF        wDISK <> *blanks
037000070928     C                   SETON                                        50
037100070222     C                   ENDIF
037200100525     C                   IF        wDISK  = 'T'
037300100525     C                   SETOFF                                       50
037400100525     C                   SETON                                        51
037500100525     C                   ENDIF
037600100401     C                   MOVEL     *blanks       wDUPREC           1
037700100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037800100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037900100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038000100525     C                   ENDIF
038100100525     C*
038200070222     C                   ENDIF
038300071009     C*
038400071009     C   50              EVAL      VABCTM = '7Q'
038500000801     C*
038600000801     C                   ENDSR
038700091223     C*----------------------------------------------------*
038800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038900091223     C*----------------------------------------------------*
039000091223     C     REPNSP        BEGSR
039100091223     C*
039200091223     C                   EXSR      INZVAR
039300091223     C                   EXSR      DEFCAM
039400100928     C*
039500100928     C                   SETON                                        60
039600091223     C*
039700091223     C* NSP => Stacco un numeratore da AZNUM
039800091223     C                   clear                   TRUL33DS
039900091223     C                   eval      I33OPE = *zeros
040000091223     C                   eval      I33CNU = 302
040100091223     C                   eval      I33NUM = 1
040200091223     C                   movel     TRUL33DS      KPJBU
040300091223     C                   call      'TRUL33R'
040400091223     C                   parm                    KPJBA
040500091223     C                   movel     KPJBU         TRUL33DS
040600091223     C                   if        O33ERR = *zeros
040700091223     C                   z-add     O33NRF        VABNSP
040800091223     C                   z-add     O33NRF        VATNSP
040900091223     C                   else
041000091223     C                   SETON                                        31
041100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041200091223     C                             + ' ' + 'VABNSP VATNSP'
041300091223     C                   endif
041400091223     C*
041500091223     C                   ENDSR
041600000801     C*----------------------------------------------------*
041700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041800000801     C*----------------------------------------------------*
041900100730     C     IMPVAB        BEGSR
042000070530     C*
042100070530     C                   SETOFF                                       3132
042200070928     C*
042300070928     C                   MOVE(P)   vlrpoi        VABFGS
042400070928     C                   MOVE(P)   vlrpoi        VATFGS
042500070928     C*
042600070928     C                   MOVEL     datcor        VABAAS
042700070928     C                   MOVEL     datcor        VATAAS
042800070928     C                   MOVE      datcor        VABMGS
042900100729     C*
043000100729     C* Reperimento campi ALFA
043100110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
043200110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043300110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043400110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
043500110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
043600110127     C*
043700110127     C                   SELECT
043800110201     C                   WHEN      %subst(vindta:207:1) = 'F'
043900110201     C                   EVAL      VABCBO = '1'
044000110201     C                   WHEN      %subst(vindta:207:1) = 'A'
044100110201     C                   EVAL      VABCBO = '2'
044200110127     C                   WHEN      %subst(vindta:199:1) = 'V'
044300110127     C                   EVAL      VABTIC = 'BM'
044400110127     C                   ENDSL
044500110127     C*
044600130911     C* per RMA tolgo gli 0 non significativi
044700130911     C                   EVAL      VABRMA=%trim(%editc(%dec(
044800130911     C                                     %trim(%subst(vindta:108:10))
044900130911     C                                    :15:0):'4'))
045000130911     C*
045100110201     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
045200110201     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
045300100729     C*
045400100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
045500100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045600100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045700100729     C* ==
045800100729     C*
045900100729     C* Reperimento campi NUMERICI
046000100729     C                   MOVEL     DATCOR        VABAAS
046100100729     C                   MOVE      DATCOR        VABMGS
046200110127     C* NSP / RMN
046300101119     C                   IF        *IN51 = *off
046400130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
046500100730     C                   EXSR      CHKNUM
046600100730     C                   IF        PiInt=*on
046700100730     C                   Z-ADD     PiVal         VABRMN
046800100928     C  N60              Z-ADD     PiVal         VABNSP
046900100928     C  N60              Z-ADD     PiVal         VATNSP
047000100730     C                   ELSE
047100100730     C                   SETON                                        32
047200100730     C                   Z-ADD     1             VABRMN
047300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
047500100730     C                   ENDIF
047600101119     C*
047700101119     C                   ELSE
047800101119     C*
047900130911     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
048000101119     C                   EXSR      CHKNUM
048100101119     C                   IF        PiInt=*on
048200101119     C  N60              Z-ADD     PiVal         VATNSP
048300101119     C                   ELSE
048400101119     C                   SETON                                        32
048500101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048600101119     C                             + ' ' + 'VATNSP'
048700101119     C                   ENDIF
048800101119     C                   ENDIF
048900110201     C* NCL
049000110201     C                   z-add     *zeros        wCOLLI            5 0
049100110201     C                   z-add     *zeros        wBANCALI          5 0
049200110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
049300100928     C                   EXSR      CHKNUM
049400100928     C                   IF        PiInt=*on
049500110201     C                   Z-ADD     PiVal         wCOLLI
049600100928     C                   ELSE
049700101119     C  N51              SETON                                        32
049800101119     C  N51              Z-ADD     *zeros        VABNCL
049900101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050000100928     C                             + ' ' + 'VABNCL'
050100100928     C                   ENDIF
050200110201     C*
050300131017     C* aggiungo i bancali (se ci sono)
050400131017     C                   IF        %subst(vindta:129:2) <> *blank
050500131017     C                   EVAL      PiStr=%trim(%subst(vindta:129:2))
050600110201     C                   EXSR      CHKNUM
050700110201     C                   IF        PiInt=*on
050800110201     C                   Z-ADD     PiVal         wBANCALI
050900110201     C                   ELSE
051000110201     C  N51              SETON                                        32
051100110201     C  N51              Z-ADD     *zeros        VABNCL
051200110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051300110201     C                             + ' ' + 'VABNCL'
051400110201     C                   ENDIF
051500131017     C                   ENDIF
051600110201     C*
051700110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
051800100729     C* PKB
051900110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
052000100729     C                   EXSR      CHKNUM
052100100729     C                   IF        PiNum=*on
052200110201     C                   Z-ADD     PiVal         VABPKB
052300100729     C                   ELSE
052400101119     C  N51              SETON                                        32
052500101119     C  N51              Z-ADD     *zeros        VABPKB
052600101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052700100729     C                             + ' ' + 'VABPKB'
052800100729     C                   ENDIF
052900110201     C* VLB
053000110201     C*                  IF        %subst(vindta:234:11) <> '000000000,0'
053100110201     C*                  EVAL      PiStr=%trim(%subst(vindta:234:11))
053200110201     C*                  EXSR      CHKNUM
053300110201     C*                  IF        PiNum=*on
053400110201     C*                  Z-ADD(H)  PiVal         VABVLB
053500110201     C*                  ELSE
053600110201     C* N51              SETON                                        32
053700110201     C* N51              Z-ADD     *zeros        VABVLB
053800110201     C* N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053900110201     C*                            + ' ' + 'VABVLB'
054000110201     C*                  ENDIF
054100110201     C*                  ENDIF
054200100729     C* CAS
054300110201     C                   IF        %subst(vindta:137:10)<> *blanks    AND
054400110201     C                             %subst(vindta:137:10)<> *zeros     AND
054500110201     C                             %subst(vindta:137:10)<> '0000000,00'
054600100729     C                   EVAL      FlgCAS = '1'
054700110127     C                   EVAL      VABVCA = 'EUR'
054800110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
054900100729     C                   EXSR      CHKNUM
055000100729     C                   IF        PiNum=*on
055100100928     C                   Z-ADD(H)  PiVal         VABCAS
055200100729     C                   ELSE
055300101119     C  N51              SETON                                        32
055400101119     C  N51              Z-ADD     *zeros        VABCAS
055500101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055600100729     C                             + ' ' + 'VABCAS'
055700100729     C                   ENDIF
055800101119     C                   ELSE
055900101119     C                   EVAL      FlgCAS = '0'
056000100729     C                   ENDIF
056100110201     C* IAS
056200110201     C                   IF        %subst(vindta:223:11)<> *blanks    AND
056300110201     C                             %subst(vindta:223:11)<> *zeros     AND
056400110201     C                             %subst(vindta:223:11)<> '00000000,00'
056500110201     C                   EVAL      VABVAS = 'EUR'
056600110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
056700110201     C                   EXSR      CHKNUM
056800110201     C                   IF        PiNum=*on
056900110201     C                   Z-ADD(H)  PiVal         VABIAS
057000110201     C                   ELSE
057100110201     C  N51              SETON                                        32
057200110201     C  N51              Z-ADD     *zeros        VABIAS
057300110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057400110201     C                             + ' ' + 'VABIAS'
057500110201     C                   ENDIF
057600110201     C                   ENDIF
057700100729     C*
057800100729     C* Se provincia nn valorizzata la reperisco
057900100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
058000100729     C                   IF        VABNZD  = *blanks AND
058100100729     C                             VABPRD  = *blanks AND
058200100729     C                             VABCAD <> *blanks
058300100729     C                   CLEAR                   TISI95DS
058400100729     C                   EVAL      I95TCN = '3'
058500100729     C                   Z-ADD     datcor        I95DAT
058600100729     C                   EVAL      I95NAR = VABNZD
058700100729     C                   EVAL      I95CAP = VABCAD
058800100729     C                   EVAL      I95LOC = VABLOD
058900100729     C                   CALL      'TISI95R'
059000100729     C                   PARM                    TISI95DS
059100100729     C                   EVAL      VABPRD = O95PRV
059200100729     C                   ENDIF
059300100729     C*
059400100729     C* Considerazioni finali su CBO/CAS
059500100729     C                   IF        FlgCAS = '1'
059600100729     C                   IF        VABCBO = '1'
059700100729     C                   EVAL      VABCBO = '4'
059800100729     C                   ENDIF
059900100729     C                   IF        VABCBO = '2'
060000100729     C                   EVAL      VABCBO = '6'
060100100729     C                   ENDIF
060200100729     C                   ENDIF
060300010202     C*
060400000801     C* Ebbene...
060500000801     C                   ADD       1             §CTRMO
060600070530     C                   IF        *in31 <> *zeros OR
060700070530     C                             *in32 <> *zeros
060800000801     C                   ADD       1             §CTRNO
060900020725     C                   EVAL      x_vinflg = '2'
061000000801     C                   ELSE
061100010201     C                   ADD       1             §CTROKVB
061200000801     C                   ENDIF
061300000801     C*
061400000801     C                   ENDSR
061500100127     C*----------------------------------------------------*
061600100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061700100127     C*----------------------------------------------------*
061800100127     C     WRIVAT_A      BEGSR
061900100127     C*
062000100127     C* Valorizzo l buffer di scrittura del FIVAT
062100100127     C* - TIPO RECORD "A"
062200101119     C***                eval      VATTRC = 'A'
062300101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
062400101119     C***                exsr      wrivat
062500100127     C*
062600100127     C                   ENDSR
062700010201     C*----------------------------------------------------*
062800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
062900010201     C*----------------------------------------------------*
063000071003     C     WRIVAT_B      BEGSR
063100010201     C*
063200021113     C* Valorizzo l buffer di scrittura del FIVAT
063300070928     C* - TIPO RECORD "B"
063400101119     C***                eval      VATTRC = 'B'
063500101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
063600101119     C***                exsr      wrivat
063700010201     C*
063800010201     C                   ENDSR
063900161102     C*----------------------------------------------------*
064000161102     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
064100161102     C*----------------------------------------------------*
064200161102     C     WRIVAT_IJ     BEGSR
064300161102     C*
064400161102     C* Valorizzo l buffer di scrittura del FIVAT
064500161102     C* - TIPO RECORD "I"
064600161102     C                   if        %subst(vindta:968:70) <> *blanks
064700161102     C                   eval      w70 = %trim(%subst(vindta:968:70))
064800161102     C                   eval      VATTRC = 'I'
064900161102     C                   eval      VATNOT = %subst(w70:1:35)
065000161102     C                   write     FIVAT000
065100161102     C                   IF        %subst(w70:36:35) <> *blank
065200161102     C                   eval      VATTRC = 'J'
065300161102     C                   eval      VATNOT = %subst(w70:36:35)
065400161102     C                   write     FIVAT000
065500161102     C                   endif
065600161102     C                   endif
065700161102     C*
065800161102     C                   ENDSR
065900071003     C*----------------------------------------------------*
066000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
066100071003     C*----------------------------------------------------*
066200071003     C     WRIVAT_E      BEGSR
066300071003     C*
066400071003     C* Valorizzo l buffer di scrittura del FIVAT
066500071003     C* - TIPO RECORD "E"
066600100729     C*
066700130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
066800130911     C*
066900130911     C*
067000100928     C                   eval      VATTRC = 'E'
067100140515     C                   eval      VATNOT = %trim(%subst(vindta:257:10))
067200140515     C                   exsr      wrivat
067300071003     C*
067400071003     C                   ENDSR
067500100127     C*----------------------------------------------------*
067600100127     C*  SCARICO BUFFER FIVAT
067700100127     C*----------------------------------------------------*
067800100127     C     WRIVAT        BEGSR
067900100127     C*
068000100127     C                   if        vatNOT <> *blanks
068100100127     C                   ADD       1             §CTROKVT
068200100127     C                   write     FIVAT000
068300100127     C                   endif
068400100127     C*
068500100127     C                   ENDSR
068600020904
068700020904
068800010202     C*----------------------------------------------------*
068900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
069000010202     C*----------------------------------------------------*
069100100525     C     PREVASX       BEGSR
069200100525     C*
069300021113     C* Compongo il nome del membro da dare al FIVATWWR
069400010202     C                   eval      parmbr = vlrhdl
069500010202     C                   movel     'M'           parmbr
069600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069700010202     C                   eval      paropz = '1'
069800100525     C*
069900010202     C* Effettuo la chiamata al CLLE preposto
070000100525     C  N51              call(e)   'TITVVTC'
070100010202     C                   parm                    parccm
070200010202     C                   parm                    parmbr
070300010202     C                   parm                    paropz
070400100525     C   51              call(e)   'TITVVBC'
070500100525     C                   parm                    parccm
070600100525     C                   parm                    parmbr
070700100525     C                   parm                    paropz
070800100525     C*
070900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071000010202     C                   if        %error
071100010202     C                   movel     '1'           chkcall
071200010202     C                   else
071300010202     C                   movel     '0'           chkcall
071400010202     C                   endif
071500010202     C*
071600010202     C                   ENDSR
071700000801     C*----------------------------------------------------*
071800000801     C*  CONTROLLO NUMERICITA' CAMPI
071900000801     C*----------------------------------------------------*
072000000801     C     CHKNUM        BEGSR
072100081113     C*
072200081113     C                   IF        PiDecChr = *blanks
072300110201     C                   EVAL      PiDecChr = ','
072400081113     C                   ENDIF
072500091223     C*
072600081113     C                   callp(e)  UBISNUM_Check(PiStr
072700081113     C                                          :PiDecChr
072800081113     C                                          :PiVal
072900081113     C                                          :PiNum
073000081113     C                                          :PiInt)
073100081113     C*
073200000801     C                   IF        %error
073300000801     C                   EVAL      PiInt=*off
073400000801     C                   ENDIF
073500000801     C*
073600000801     C                   ENDSR
073700000801     C***
073800011113
073900011113
074000000801
074100000801
074200990920      /TITLE Invio dei dati al punto operativo.
074300010202     C     invio         BEGSR
074400990920     C*
074500021113     C* 1° invio FIVAT
074600010201     C                   reset                   dscmz
074700010201     C                   move      vlrpoi        cmzdst
074800021113     C                   eval      cmzfld = 'FIVATWWR'
074900010201     C                   eval      cmzmbd = vlrhdl
075000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075100021009     C***                if        prmfir = *blanks
075200021113     C                   eval      cmzfla = 'FIVAT00F'
075300021113     C                   eval      cmzmba = 'FIVAT00F'
075400021009     C***                else
075500021009     C***                eval      cmzfla = prmfir
075600021009     C***                eval      cmzmba = prmfir
075700021009     C***                endif
075800010201     C                   eval      cmznrr = *zeros
075900020305     C                   move      §ctrokvt      cmznrr
076000021018     C                   eval      cmzlba = vlrfl1
076100010201     C                   call(e)   'TIS711C'
076200010201     C                   parm                    dscmz
076300010201     C                   parm      *blanks       esito
076400010205     C                   if        %error
076500010205     C                             or cmzerr = '1'
076600010205     C                             or esito  = '1'
076700010205     C                   eval      wrkesito = '3'
076800010205     C                   else
076900010201     C*
077000021113     C* 2° invio FIVAB
077100100525     C                   if        *in51 = *off
077200010201     C                   reset                   dscmz
077300010201     C                   move      vlrpoi        cmzdst
077400010201     C                   eval      cmzfld = vlrfou
077500010201     C                   eval      cmzmbd = vlrhdl
077600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077700021009     C***                if        prmfir = *blanks
077800021113     C                   eval      cmzfla = 'FIVAB00F'
077900021113     C                   eval      cmzmba = 'FIVAB00F'
078000021009     C***                else
078100021009     C***                eval      cmzfla = prmfir
078200021009     C***                eval      cmzmba = prmfir
078300021009     C***                endif
078400010201     C                   eval      cmznrr = *zeros
078500010201     C                   move      §ctrokvb      cmznrr
078600021018     C                   eval      cmzlba = vlrfl1
078700010201     C                   call(e)   'TIS711C'
078800010201     C                   parm                    dscmz
078900010201     C                   parm      *blanks       esito
079000010201     C                   if        %error
079100010201     C                             or cmzerr = '1'
079200010201     C                             or esito  = '1'
079300010201     C                   eval      wrkesito = '3'
079400010201     C                   endif
079500100525     C                   endif
079600010205     C                   endif
079700990920     C*
079800000613     C                   ENDSR
079900000613     C***
080000070411
080100070411     C     *pssr         BEGSR
080200070411     C*
080300070411     C                   if        %open(tivin00r)
080400070411     C                   close     tivin00r
080500070411     C                   endif
080600070411     C                   if        %open(fivabwwr)
080700070411     C                   close     fivabwwr
080800070411     C                   endif
080900070411     C                   if        %open(fivatwwr)
081000070411     C                   close     fivatwwr
081100070411     C                   endif
081200070411     C*
081300070411     C* Effettuo la chiamata al CLLE preposto
081400100525     C  N51              call(e)   'TITVVTC'
081500100525     C                   parm                    parccm
081600100525     C                   parm                    parmbr
081700100525     C                   parm      '2'           paropz
081800100525     C   51              call(e)   'TITVVBC'
081900100525     C                   parm                    parccm
082000100525     C                   parm                    parmbr
082100100525     C                   parm      '2'           paropz
082200070411     C*
082300070411     C                   eval      wrkesito = '2'
082400070411     C*
082500070411     C                   seton                                        LR
082600070411     C*
082700070411     C                   ENDSR     '*CANCL'
082800070411     C***
082900070411
083000990910
083100000613     C     *inzsr        BEGSR
083200990910     C*
083300990910     C     *entry        plist
083400990920     C                   parm                    tivlrds
083500990921     C                   parm      wrkesito      esito
083600000724     C                   parm                    prmlit
083700000710     C                   parm                    prmfir
083800000613     C*
083900000830     C* CALCOLA LA DATA CORRENTE
084000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084100091223     C                   eval      datcor = %dec(%date() : *ISO)
084200000830     C*
084300000613     C                   ENDSR
084400000613     C***
