000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400130116      * PARTICOLARITA':
000500130207      * Valorizzare il campo VABTIC sempre a BM a prescindere dal importo contrassegno
000600150407      * 07/04/15 in caso di importo contrassegno valorizzato, VABTIC = *blank
000700130128      * se il peso risultante è 0 , lo imposto a 0,1
000800130116
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000100412     FFIVABwwr  O    E             DISK    usropn
001100021113     FFIVATwwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600000621     D recko           s            150    INZ(*blanks)
002700011113     D depcmd          s            150    INZ(*blanks)
002800111031     D depspe          s             25    INZ(*blanks)
002900111031     D curspe          s             25    INZ(*blanks)
003000010202     D parccm          s              8    INZ(*blanks)
003100010202     D parmbr          s             10    INZ(*blanks)
003200010202     D paropz          s              1    INZ(*blanks)
003300010202     D chkcall         s              1    INZ(*blanks)
003400080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003500091223     D Num5_0          s              5  0
003600020725
003700020725     D*------------------
003800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003900020725     D*------------------
004000070530     D INPUT_DS        DS                  INZ
004100100729     D  VINDTA                     2048
004200100729     D  VINFLG                        1
004300111031     D  VINSPE                       25
004400100729     D  VINRRN                        8  0
004500020725     D*
004600081113
004700091223     D*------------------
004800091223     D* DS REPERIMENTO NUMERATORE
004900091223     D*------------------
005000100309     D trul33ds      e ds                  inz
005100091223     D*------------------
005200091223     D* DS ARCHITETTURA
005300091223     D*------------------
005400091223     D kpjba         e ds                  inz
005500091223
005600101119
005700101119     D*-------------------
005800101119     D* COSTANTI
005900101119     D*-------------------
006000101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006100101119     D                                     èéù')
006200101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006300101119     D                                     EEU')
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007000081113
007100990908
007200010201
007300010201
007400080117     C*
007500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600080117     C
007700080117     C/EXEC SQL
007800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900080117     C/END-EXEC
008000080117     C
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400000613     C*
008500100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008600070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008700000613     C*
008800010202     C* Effettuo la chiamata al CLLE preposto
008900100525     C  N51              call(e)   'TITVVTC'
009000100525     C                   parm                    parccm
009100100525     C                   parm                    parmbr
009200100525     C                   parm      '2'           paropz
009300100525     C   51              call(e)   'TITVVBC'
009400100525     C                   parm                    parccm
009500100525     C                   parm                    parmbr
009600100525     C                   parm      '2'           paropz
009700070730     C*
009800070730     C* Effettuo lancio TISI95 solo x chiusura
009900070730     C                   CLEAR                   TISI95DS
010000070730     C                   EVAL      I95TLA = 'C'
010100070730     C                   CALL      'TISI95R'
010200070730     C                   PARM                    TISI95DS
010300000616     C*
010400000801     C
010500010201     C                   seton                                        LR
010600990908
010700000801
010800910830     C*--------------------------------------------------------
010900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
011000910830     C*--------------------------------------------------------
011100070530     C     RWFILE        BEGSR
011200990910     C*
011300990914     C                   if        not %open(tivin00r)
011400990908     C                   open      tivin00r
011500990914     C                   endif
011600070530     C*
011700100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011800100525     C                   exsr      prevasx
011900010201     C*
012000010202     C                   if        chkcall = '0'
012100010202     C*
012200100525     C                   if        not %open(fivabwwr)
012300100525     C                   open      fivabwwr
012400100525     C                   endif
012500100525     C*
012600021113     C                   if        not %open(fivatwwr)
012700021113     C                   open      fivatwwr
012800010201     C                   endif
012900080117     C*
013000080117     C                   EXSR      INZVAR
013100080117     C                   EXSR      DEFCAM
013200990910     C*
013300010201     C                   clear                   §CTROKVB          5 0
013400020305     C                   clear                   §CTROKVT          5 0
013500000801     C                   clear                   §CTRMO            5 0
013600000801     C                   clear                   §CTRNO            5 0
013700990910     C*
013800020725     C/EXEC SQL
013900020725     C+ declare C1 cursor for select
014000111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
014100060223     C+ from tivin00r
014200111020     C+ order by sped
014300020725     C+ for read only
014400020725     C/END-EXEC
014500020725     C
014600020725     C/EXEC SQL
014700020725     C+ open C1
014800020725     C/END-EXEC
014900020725     C
015000020725     C/EXEC SQL
015100070530     C+ Fetch C1 into :INPUT_DS
015200020725     C/END-EXEC
015300020725     C*
015400020725     C                   dow       sqlcod = *zeros
015500990913     C*
015600100310     C                   if        vindta > *blanks
015700000613     C                   add       1             rrnum
015800000801     C*
015900020725     C                   if        vinflg = *blanks
016000020725     C                             or vinflg = '0'
016100020725     C                             or vinflg = '2'
016200000801     C*
016300020725     C                   clear                   x_vinmsg
016400020725     C                   eval      x_vinflg = '1'
016500101119     C*
016600101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016700101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016800010305     C*
016900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017000111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
017100010305     C*
017200100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017300080923     C                   if        *in50 = *off
017400101119     C                   exsr      inzvar
017500101119     C                   exsr      defcam
017600071003     C                   exsr      impvab
017700100525     C                   exsr      wrivab
017800111031     C                   exsr      exevata                                      => carico VAT
017900111031     C                   exsr      exevatb                                      => carico VAT
018000111031     C   51              exsr      exevate                                      => carico VAT
018100120911     C***                exsr      exevati                                      => carico VAT
018200071003     C                   else
018300080923     C*
018400071009     C                   if        wDISK = 'D'
018500091223     C                   exsr      repNSP
018600071009     C                   exsr      impvab
018700100412     C                   exsr      wrivab
018800111031     C                   exsr      exevata                                      => carico VAT
018900111031     C                   exsr      exevatb                                      => carico VAT
019000111031     C                   exsr      exevate                                      => carico VAT
019100120911     C***                exsr      exevati                                      => carico VAT
019200071009     C                   else
019300071009     C*
019400010305     C                   if        depspe = *blanks                             => 1° giro
019500010305     C                   eval      depspe = curspe                              => memorizz. spediz
019600080117     C                   clear                   tivinds
019700091223     C                   exsr      repNSP
019800020305     C                   exsr      impvab
019900111031     C                   exsr      exevata                                      => carico VAT
020000111031     C                   exsr      exevatb                                      => carico VAT
020100111031     C   50              exsr      exevate                                      => carico VAT
020200120911     C***                exsr      exevati                                      => carico VAT
020300010305     C                   else
020400020725     C                   if        depspe <> curspe                             => rottura di spediz
020500010305     C                   eval      depspe = curspe                              => memorizz. spediz
020600100412     C                   exsr      wrivab
020700080117     C                   clear                   tivinds
020800091223     C                   exsr      repNSP
020900020305     C                   exsr      impvab
021000111031     C                   exsr      exevata                                      => carico VAT
021100111031     C                   exsr      exevatb                                      => carico VAT
021200111031     C   50              exsr      exevate                                      => carico VAT
021300120911     C***                exsr      exevati                                      => carico VAT
021400020305     C                   else                                                   => x stessa spediz
021500100401     C*
021600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021700100928     C                   select
021800100928     C                   when      wDUPREC = ' '
021900020305     C                   exsr      impvab
022000111031     C                   exsr      exevata                                      => carico VAT
022100111031     C                   exsr      exevatb                                      => carico VAT
022200111031     C   50              exsr      exevate                                      => carico VAT
022300120911     C***                exsr      exevati                                      => carico VAT
022400100928     C                   when      wDUPREC = 'C'
022500111031     C   50              exsr      exevate                                      => carico VAT
022600100928     C                   endsl
022700010305     C                   endif
022800010305     C                   endif
022900010305     C                   endif
023000071003     C                   endif
023100110201     C*
023200071009     C                   endif
023300000905     C*
023400000905     C                   else
023500020725     C                   eval      x_vinflg = '1'
023600000905     C                   endif
023700000905     C*
023800020725     C     VINRRN        chain     tivin000
023900020725     C                   if        %found(tivin00r)
024000020725     C                   eval      y_vinflg = x_vinflg
024100020725     C                   eval      y_vinmsg = x_vinmsg
024200020725     C                   update    tivin000
024300020725     C                   endif
024400020725     C*
024500020725     C/EXEC SQL
024600070530     C+ Fetch C1 into :INPUT_DS
024700020725     C/END-EXEC
024800020725     C*
024900020725     C                   enddo
025000020725     C*
025100020725     C/EXEC SQL
025200020725     C+ close C1
025300020725     C/END-EXEC
025400000905     C*
025500020305     C* Scarico i VAB rimasti "in sospeso"
025600071009     C                   if        wDISK = 'C'
025700100412     C                   exsr      wrivab
025800071009     C                   endif
025900010202     C*
026000010202     C                   endif
026100990910
026200990910     C* Se non ci sono record con errori ...
026300000710     C                   if        §ctrno = 0
026400990910     C* ... restituisco esito OK.
026500990921     C                   eval      wrkesito = '0'
026600990910     C                   else
026700100525     C                   if        §ctrokvb > 0 OR
026800100525     C                             §ctrokvt > 0
026900990921     C                   eval      wrkesito = '1'
027000000710     C                   else
027100000710     C                   eval      wrkesito = '2'
027200990910     C                   endif
027300000710     C                   endif
027400990910     C*
027500990914     C                   if        %open(tivin00r)
027600990908     C                   close     tivin00r
027700990914     C                   endif
027800021113     C                   if        %open(fivabwwr)
027900021113     C                   close     fivabwwr
028000990914     C                   endif
028100021113     C                   if        %open(fivatwwr)
028200021113     C                   close     fivatwwr
028300010201     C                   endif
028400990910     C*
028500100525     C                   if        §ctrokvb > 0 OR
028600100525     C                             §ctrokvt > 0
028700000724     C                             and vlrpoi <> *zeros
028800010202     C                   exsr      invio
028900990920     C                   endif
029000990920     C*
029100910830     C                   ENDSR
029200000613     C***
029300100412
029400100412
029500010305
029600010305     C*----------------------------------------------------*
029700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029800010305     C*----------------------------------------------------*
029900020305     C     WRIVAB        BEGSR
030000100412     C*
030100100412     C* Gestisco l'eventuale rottura x numero spedizione
030200100412     C                   if        VABRMA = *blanks
030300100412     C                   movel(P)  VABRMN        VABRMA
030400100412     C                   endif
030500100412     C*
030600100525     C                   if        *in51 = *off and *in31 = *off
030700100525     C                   write     fivab000                                     => scarico il VAB
030800100525     C                   endif
030900010305     C*
031000010305     C                   ENDSR
031100990920
031200000801     C*----------------------------------------------------*
031300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031400000801     C*----------------------------------------------------*
031500010201     C     INZVAR        BEGSR
031600000801     C*
031700010201     C                   Z-ADD     *zeros        Num5_0
031800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031900020725     C                   MOVEL     '0'           FlgCAS            1
032000000801     C*
032100000801     C                   ENDSR
032200000801     C*----------------------------------------------------*
032300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032400000801     C*----------------------------------------------------*
032500020904     C     DEFCAM        BEGSR
032600080923     C*
032700100928     C                   SETOFF                                       505160
032800000801     C*
032900021113     C                   CLEAR                   FIVAB000
033000021113     C                   CLEAR                   FIVAT000
033100070730     C* Imposto i valori di default...
033200121109     C                   EVAL      VABCCM = 1661771
033300121109     C                   EVAL      VATCCM = 1661771
033400121109     C                   EVAL      VABLNP = 166
033500121109     C                   EVAL      VATLNP = 166
033600120910     C                   EVAL      VABCTR = 000
033700100525     C                   EVAL      VABCBO = '1'
033800070222     C* ... e poi verifico se sono stati passati come parametri
033900070222     C                   IF        vlrppt > *blanks
034000100525     C*
034100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABCCM
034600070222     C                   Z-ADD     PiVal         VATCCM
034700070222     C                   ENDIF
034800070222     C                   ENDIF
034900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABLNP
035400070222     C                   Z-ADD     PiVal         VATLNP
035500070222     C                   ENDIF
035600070222     C                   ENDIF
035700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABCTR
036200070222     C                   ENDIF
036300070928     C                   ENDIF
036400071009     C                   MOVEL     *blanks       wDISK             1
036500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036600071009     C                   IF        wDISK <> *blanks
036700070928     C                   SETON                                        50
036800070222     C                   ENDIF
036900100525     C                   IF        wDISK  = 'T'
037000100525     C                   SETOFF                                       50
037100100525     C                   SETON                                        51
037200100525     C                   ENDIF
037300100401     C                   MOVEL     *blanks       wDUPREC           1
037400100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037500100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037600100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037700100525     C                   ENDIF
037800111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
037900111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
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
041500111031     C*----------------------------------------------------*
041600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041700111031     C*----------------------------------------------------*
041800111031     C     IMPVAB        BEGSR
041900111031     C*
042000111031     C                   Z-ADD     *zeros        errore            1 0
042100111031     C                   MOVEL     datcor        VABAAS
042200111031     C                   MOVEL     datcor        VATAAS
042300111031     C                   MOVE      datcor        VABMGS
042400111031     C                   MOVE(P)   vlrpoi        VABFGS
042500111031     C                   MOVE(P)   vlrpoi        VATFGS
042600111031     C*
042700120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042800111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042900111031     C     '@':'A'       XLATE     VABRSD        VABRSD
043000111031     C* ==
043100111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
043200111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043300111031     C                                    %trim(%subst(vindta:341:30))+' '+
043400111031     C                                    %trim(%subst(vindta:371:5)))
043500111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043600111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043700111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
043800121217     C***                EVAL      VABRMA=%trim(%subst(vindta:46:25))
043900121217     C                   EVAL      VABRMA=%trim(%subst(vindta:376:15))
044000111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
044100111031     C                   IF        %subst(vindta:43:1) = 'N'
044200111031     C                   EVAL      VABCBO='1'
044300111031     C                   ENDIF
044400111031     C                   IF        %subst(vindta:43:1) = 'P'
044500111031     C                   EVAL      VABCBO='2'
044600111031     C                   ENDIF
044700130116     C***                IF        %subst(vindta:511:3) = 'ABM'
044800130116     C***                EVAL      VABTIC='BM'
044900130116     C***                ENDIF
045000130116     C***                IF        %subst(vindta:511:3) = 'ABS'
045100130116     C***                EVAL      VABTIC='B'
045200130116     C***                ENDIF
045300130116     C***                IF        %subst(vindta:511:3) = 'CON'
045400130116     C***                EVAL      VABTIC=*blanks
045500130116     C***                ENDIF
045600130116     C***                IF        %subst(vindta:511:3) = 'ACM'
045700130116     C***                EVAL      VABTIC='CM'
045800130116     C***                ENDIF
045900130116     C***                IF        %subst(vindta:511:3) = 'ACS'
046000130116     C***                EVAL      VABTIC='CB'
046100130116     C***                ENDIF
046200130116     C***                IF        %subst(vindta:511:3) = 'VAR'
046300130116     C***                EVAL      VABTIC=*blanks
046400130116     C***                ENDIF
046500111031     C* CAP
046600111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
046700111031     C                   EXSR      CHKNUM
046800111031     C                   IF        PiInt=*on
046900111031     C                   Z-ADD     PiVal         Num5_0
047000111031     C                   MOVEL(P)  Num5_0        VABCAD
047100111031     C                   ELSE
047200111031     C                   ADD       1             errore
047300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400111031     C                             + ' ' + 'VABCAD'
047500111031     C                   ENDIF
047600111031     C* Reperisco la provincia dal CAP e dalla località
047700111031     C                   IF        VABCAD <> *blanks AND
047800111031     C                             VABPRD  = *blanks
047900111031     C                   CLEAR                   TISI95DS
048000111031     C                   EVAL      I95TCN = '3'
048100111031     C                   Z-ADD     datcor        I95DAT
048200111031     C                   EVAL      I95CAP = VABCAD
048300111031     C                   EVAL      I95LOC = VABLOD
048400111031     C                   EVAL      I95NAR = VABNZD
048500111031     C                   CALL      'TISI95R'
048600111031     C                   PARM                    TISI95DS
048700111031     C                   EVAL      VABPRD = O95PRV
048800111031     C                   ENDIF
048900111031     C* NCL
049000111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
049100111031     C                   EXSR      CHKNUM
049200111031     C                   IF        PiInt=*on
049300111031     C                   SELECT
049400111031     C                   WHEN      wNCL = *blanks
049500111031     C                   Z-ADD     PiVal         VABNCL
049600111031     C                   WHEN      wNCL = 'A'
049700111031     C                   ADD       PiVal         VABNCL
049800111031     C                   WHEN      wNCL = '1'
049900111031     C                   ADD       1             VABNCL
050000111031     C                   ENDSL
050100111031     C                   ELSE
050200111031     C                   ADD       1             errore
050300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400111031     C                             + ' ' + 'VABNCL'
050500111031     C                   ENDIF
050600111031     C* PKB
050700120403     C* il peso è in grammi
050800111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
050900111031     C                   EXSR      CHKNUM
051000111031     C                   IF        PiNum=*on
051100120403     C                   EVAL      PiVal=PiVal/1000
051200111031     C                   Z-ADD     PiVal         VABPKB
051300130128     C* se il peso risultante è 0
051400130128     C                   IF        VABPKB = 0
051500130128     C* lo imposto a 0,1
051600130128     C                   EVAL      VABPKB = 0,1
051700130128     C                   ENDIF
051800111031     C                   ELSE
051900111031     C                   ADD       1             errore
052000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052100111031     C                             + ' ' + 'VABPKB'
052200111031     C                   ENDIF
052300111031     C* RMN
052400121109     C                   EVAL      PiStr=%trim(%subst(vindta:46:14))
052500120403     C*** elenco delle posizioni utilizzate nel tempo
052600121109     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
052700121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
052800120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
052900120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
053000111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
053100111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
053200111031     C                   EXSR      CHKNUM
053300111031     C                   IF        PiInt=*on
053400111031     C                   Z-ADD     PiVal         VABRMN
053500111031     C                   ELSE
053600111031     C                   ADD       1             errore
053700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053800111031     C                             + ' ' + 'VABRMN'
053900111031     C                   ENDIF
054000111031     C* CAS
054100111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
054200111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
054300111031     C                             %trim(%subst(vindta:502:9))<>*zeros
054400111031     C                   EVAL      VABVCA = 'EUR'
054500111031     C                   EVAL      FlgCAS = '1'
054600111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
054700111031     C                   EXSR      CHKNUM
054800111031     C                   IF        PiNum=*on
054900111031     C                   Z-ADD     PiVal         VABCAS
055000111031     C                   ELSE
055100111031     C                   ADD       1             errore
055200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055300111031     C                             + ' ' + 'VABCAS'
055400111031     C                   ENDIF
055500111031     C                   ENDIF
055600111031     C* IAS
055700111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
055800111031     C                   EVAL      VABVAS = 'EUR'
055900111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
056000111031     C                   EXSR      CHKNUM
056100111031     C                   IF        PiNum=*on
056200111031     C                   Z-ADD     PiVal         VABIAS
056300111031     C                   ELSE
056400111031     C                   ADD       1             errore
056500111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056600111031     C                             + ' ' + 'VABIAS'
056700111031     C                   ENDIF
056800111031     C                   ENDIF
056900111031     C*
057000111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
057100111031     C                   IF        FlgCAS <> '0'
057200111031     C                   IF        VABCBO = '1'
057300111031     C                   EVAL      VABCBO = '4'
057400111031     C                   ELSE
057500111031     C                   EVAL      VABCBO = '6'
057600111031     C                   ENDIF
057700111031     C                   ENDIF
057800111031     C*
057900111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058000111031     C                   EXSR      CHKIMPDIV
058100111031     C*
058200111031     C* Ebbene...
058300111031     C                   ADD       1             §CTRMO
058400111031     C                   IF        errore <> *zeros
058500111031     C                   ADD       1             §CTRNO
058600111031     C                   EVAL      recko = vindta
058700111031     C                   EVAL      vinflg = '2'
058800111031     C                   ELSE
058900111031     C                   ADD       1             §CTROKVB
059000111031     C                   ENDIF
059100111031     C*
059200111031     C                   ENDSR
059300111031
059400111031
059500111031     C*----------------------------------------------------*
059600111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059700111031     C*----------------------------------------------------*
059800111031     C     CHKIMPDIV     BEGSR
059900111031     C*
060000111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060100111031     C                   Z-ADD     *zeros        wrkDec            9 9
060200111031     C*
060300111031     C* Come prima cosa effettuo considerazioni sulla divisa
060400111031     C                   IF        vabIAS > *zeros
060500111031     C                   IF        vabVAS <> 'EUR'
060600111031     C                   EVAL      vabVAS =  'ITL'
060700111031     C                   ENDIF
060800111031     C                   ENDIF
060900111031     C*
061000111031     C                   IF        vabCAS > *zeros
061100111031     C                   IF        vabVCA <> 'EUR'
061200111031     C                   EVAL      vabVCA =  'ITL'
061300111031     C                   ENDIF
061400130207     C***                SELECT
061500130207     C***                WHEN      VABCAS < 500
061600130207     C***                EVAL      VABTIC = *blank
061700130207     C***                WHEN      VABCAS >= 500
061800150407     C                   EVAL      VABTIC = *blank
061900130207     C***                ENDSL
062000111031     C                   ENDIF
062100111031     C*
062200111031     C                   IF        vabVMD > *zeros
062300111031     C                   IF        vabVAD <> 'EUR'
062400111031     C                   EVAL      vabVAD =  'ITL'
062500111031     C                   ENDIF
062600111031     C                   ENDIF
062700111031     C*
062800111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062900111031     C                   Z-ADD     vabIAS        wrkDec
063000111031     C                   IF        wrkDec > *zeros
063100111031     C                   IF        vabVAS = 'ITL'
063200111031     C                   EVAL      vabIAS = *zeros
063300111031     C                   ENDIF
063400111031     C                   ENDIF
063500111031     C*
063600111031     C* Stabilisco se il contrasegno ha decimali valorizzati
063700111031     C                   Z-ADD     vabCAS        wrkDec
063800111031     C                   IF        wrkDec > *zeros
063900111031     C                   IF        vabVCA = 'ITL'
064000111031     C                   EVAL      vabCAS = *zeros
064100111031     C                   ENDIF
064200111031     C                   ENDIF
064300111031     C*
064400111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064500111031     C                   Z-ADD     vabVMD        wrkDec
064600111031     C                   IF        wrkDec > *zeros
064700111031     C                   IF        vabVAD = 'ITL'
064800111031     C                   EVAL      vabVMD = *zeros
064900111031     C                   ENDIF
065000111031     C                   ENDIF
065100111031     C*
065200111031     C                   ENDSR
065300111031     C***
065400111031
065500111031
065600111031     C*----------------------------------------------------*
065700111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
065800111031     C*----------------------------------------------------*
065900111031     C     EXEVATA       BEGSR
066000111031     C*
066100111031     C***                EVAL      VATTRC='A'
066200111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
066300111031     C***                exsr      wrivat                                       => scarico VAT
066400111031     C*
066500111031     C                   ENDSR
066600111031     C*----------------------------------------------------*
066700111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
066800111031     C*----------------------------------------------------*
066900111031     C     EXEVATB       BEGSR
067000111031     C*
067100121217     C***                EVAL      VATTRC='B'
067200121217     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
067300121217     C***                exsr      wrivat                                       => scarico VAT
067400111031     C*
067500111031     C                   ENDSR
067600111031     C*----------------------------------------------------*
067700111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
067800111031     C*----------------------------------------------------*
067900111031     C     EXEVATE       BEGSR
068000111031     C*
068100111031     C                   EVAL      VATTRC='E'
068200111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
068300111031     C                   exsr      wrivat                                       => scarico VAT
068400111031     C*
068500111031     C                   ENDSR
068600120402     C*----------------------------------------------------*
068700120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
068800120402     C*----------------------------------------------------*
068900120402     C     EXEVATI       BEGSR
069000120402     C*
069100120402     C                   EVAL      VATTRC='I'
069200120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
069300120402     C                   exsr      wrivat                                       => scarico VAT
069400120402     C*
069500120402     C                   EVAL      VATTRC='J'
069600120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
069700120402     C                   exsr      wrivat                                       => scarico VAT
069800120402     C*
069900120402     C                   ENDSR
070000111031     C*----------------------------------------------------*
070100111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
070200111031     C*----------------------------------------------------*
070300111031     C     WRIVAT        BEGSR
070400111031     C*
070500111031     C* Scrivo solo se valorizzato qualcosa
070600111031     C                   IF        VATNOT <> *blanks
070700111031     C                   WRITE     FIVAT000
070800111031     C                   ENDIF
070900111031     C*
071000111031     C                   ENDSR
071100020904
071200020904
071300010202     C*----------------------------------------------------*
071400100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071500010202     C*----------------------------------------------------*
071600100525     C     PREVASX       BEGSR
071700100525     C*
071800021113     C* Compongo il nome del membro da dare al FIVATWWR
071900010202     C                   eval      parmbr = vlrhdl
072000010202     C                   movel     'M'           parmbr
072100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072200010202     C                   eval      paropz = '1'
072300100525     C*
072400010202     C* Effettuo la chiamata al CLLE preposto
072500100525     C  N51              call(e)   'TITVVTC'
072600010202     C                   parm                    parccm
072700010202     C                   parm                    parmbr
072800010202     C                   parm                    paropz
072900100525     C   51              call(e)   'TITVVBC'
073000100525     C                   parm                    parccm
073100100525     C                   parm                    parmbr
073200100525     C                   parm                    paropz
073300100525     C*
073400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073500010202     C                   if        %error
073600010202     C                   movel     '1'           chkcall
073700010202     C                   else
073800010202     C                   movel     '0'           chkcall
073900010202     C                   endif
074000010202     C*
074100010202     C                   ENDSR
074200000801     C*----------------------------------------------------*
074300000801     C*  CONTROLLO NUMERICITA' CAMPI
074400000801     C*----------------------------------------------------*
074500000801     C     CHKNUM        BEGSR
074600081113     C*
074700081113     C                   IF        PiDecChr = *blanks
074800110408     C                   EVAL      PiDecChr = '.'
074900081113     C                   ENDIF
075000091223     C*
075100081113     C                   callp(e)  UBISNUM_Check(PiStr
075200081113     C                                          :PiDecChr
075300081113     C                                          :PiVal
075400081113     C                                          :PiNum
075500081113     C                                          :PiInt)
075600081113     C*
075700000801     C                   IF        %error
075800000801     C                   EVAL      PiInt=*off
075900000801     C                   ENDIF
076000000801     C*
076100000801     C                   ENDSR
076200000801     C***
076300011113
076400011113
076500000801
076600000801
076700990920      /TITLE Invio dei dati al punto operativo.
076800010202     C     invio         BEGSR
076900990920     C*
077000021113     C* 1° invio FIVAT
077100010201     C                   reset                   dscmz
077200110411     C                   move      vatfgs        cmzdst
077300021113     C                   eval      cmzfld = 'FIVATWWR'
077400010201     C                   eval      cmzmbd = vlrhdl
077500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077600021009     C***                if        prmfir = *blanks
077700021113     C                   eval      cmzfla = 'FIVAT00F'
077800021113     C                   eval      cmzmba = 'FIVAT00F'
077900021009     C***                else
078000021009     C***                eval      cmzfla = prmfir
078100021009     C***                eval      cmzmba = prmfir
078200021009     C***                endif
078300010201     C                   eval      cmznrr = *zeros
078400020305     C                   move      §ctrokvt      cmznrr
078500021018     C                   eval      cmzlba = vlrfl1
078600010201     C                   call(e)   'TIS711C'
078700010201     C                   parm                    dscmz
078800010201     C                   parm      *blanks       esito
078900010205     C                   if        %error
079000010205     C                             or cmzerr = '1'
079100010205     C                             or esito  = '1'
079200010205     C                   eval      wrkesito = '3'
079300010205     C                   else
079400010201     C*
079500021113     C* 2° invio FIVAB
079600100525     C                   if        *in51 = *off
079700010201     C                   reset                   dscmz
079800110411     C                   move      vabfgs        cmzdst
079900010201     C                   eval      cmzfld = vlrfou
080000010201     C                   eval      cmzmbd = vlrhdl
080100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080200021009     C***                if        prmfir = *blanks
080300021113     C                   eval      cmzfla = 'FIVAB00F'
080400021113     C                   eval      cmzmba = 'FIVAB00F'
080500021009     C***                else
080600021009     C***                eval      cmzfla = prmfir
080700021009     C***                eval      cmzmba = prmfir
080800021009     C***                endif
080900010201     C                   eval      cmznrr = *zeros
081000010201     C                   move      §ctrokvb      cmznrr
081100021018     C                   eval      cmzlba = vlrfl1
081200010201     C                   call(e)   'TIS711C'
081300010201     C                   parm                    dscmz
081400010201     C                   parm      *blanks       esito
081500010201     C                   if        %error
081600010201     C                             or cmzerr = '1'
081700010201     C                             or esito  = '1'
081800010201     C                   eval      wrkesito = '3'
081900010201     C                   endif
082000100525     C                   endif
082100010205     C                   endif
082200990920     C*
082300000613     C                   ENDSR
082400000613     C***
082500070411
082600110627     C     *pssr         BEGSR
082700070411     C*
082800110627     C                   if        %open(tivin00r)
082900110627     C                   close     tivin00r
083000110627     C                   endif
083100110627     C                   if        %open(fivabwwr)
083200110627     C                   close     fivabwwr
083300110627     C                   endif
083400110627     C                   if        %open(fivatwwr)
083500110627     C                   close     fivatwwr
083600110627     C                   endif
083700110627     C*
083800110627     C* Effettuo la chiamata al CLLE preposto
083900110627     C  N51              call(e)   'TITVVTC'
084000110627     C                   parm                    parccm
084100110627     C                   parm                    parmbr
084200110627     C                   parm      '2'           paropz
084300110627     C   51              call(e)   'TITVVBC'
084400110627     C                   parm                    parccm
084500110627     C                   parm                    parmbr
084600110627     C                   parm      '2'           paropz
084700110627     C*
084800110627     C                   eval      wrkesito = '2'
084900110627     C*
085000110627     C                   seton                                        LR
085100110627     C*
085200110627     C                   ENDSR     '*CANCL'
085300070411     C***
085400070411
085500990910
085600000613     C     *inzsr        BEGSR
085700990910     C*
085800990910     C     *entry        plist
085900990920     C                   parm                    tivlrds
086000990921     C                   parm      wrkesito      esito
086100000724     C                   parm                    prmlit
086200000710     C                   parm                    prmfir
086300000613     C*
086400000830     C* CALCOLA LA DATA CORRENTE
086500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
086600091223     C                   eval      datcor = %dec(%date() : *ISO)
086700000830     C*
086800000613     C                   ENDSR
086900000613     C***
