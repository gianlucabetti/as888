000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400130116      * PARTICOLARITA':
000500130207      * Valorizzare il campo VABTIC sempre a BM a prescindere dal importo contrassegno
000600130128      * se il peso risultante è 0 , lo imposto a 0,1
000700130116
000800020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000900100412     FFIVABwwr  O    E             DISK    usropn
001000021113     FFIVATwwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900070730     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500000621     D recko           s            150    INZ(*blanks)
002600011113     D depcmd          s            150    INZ(*blanks)
002700111031     D depspe          s             25    INZ(*blanks)
002800111031     D curspe          s             25    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400091223     D Num5_0          s              5  0
003500020725
003600020725     D*------------------
003700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003800020725     D*------------------
003900070530     D INPUT_DS        DS                  INZ
004000100729     D  VINDTA                     2048
004100100729     D  VINFLG                        1
004200111031     D  VINSPE                       25
004300100729     D  VINRRN                        8  0
004400020725     D*
004500081113
004600091223     D*------------------
004700091223     D* DS REPERIMENTO NUMERATORE
004800091223     D*------------------
004900100309     D trul33ds      e ds                  inz
005000091223     D*------------------
005100091223     D* DS ARCHITETTURA
005200091223     D*------------------
005300091223     D kpjba         e ds                  inz
005400091223
005500101119
005600101119     D*-------------------
005700101119     D* COSTANTI
005800101119     D*-------------------
005900101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006000101119     D                                     èéù')
006100101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006200101119     D                                     EEU')
006300081113
006400081113     D*------------------
006500081113     D* LINKING A DEFINIZIONI ESTERNE
006600081113     D*------------------
006700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006800090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006900081113
007000990908
007100010201
007200010201
007300080117     C*
007400080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007500080117     C
007600080117     C/EXEC SQL
007700080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007800080117     C/END-EXEC
007900080117     C
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008600000613     C*
008700010202     C* Effettuo la chiamata al CLLE preposto
008800100525     C  N51              call(e)   'TITVVTC'
008900100525     C                   parm                    parccm
009000100525     C                   parm                    parmbr
009100100525     C                   parm      '2'           paropz
009200100525     C   51              call(e)   'TITVVBC'
009300100525     C                   parm                    parccm
009400100525     C                   parm                    parmbr
009500100525     C                   parm      '2'           paropz
009600070730     C*
009700070730     C* Effettuo lancio TISI95 solo x chiusura
009800070730     C                   CLEAR                   TISI95DS
009900070730     C                   EVAL      I95TLA = 'C'
010000070730     C                   CALL      'TISI95R'
010100070730     C                   PARM                    TISI95DS
010200000616     C*
010300000801     C
010400010201     C                   seton                                        LR
010500990908
010600000801
010700910830     C*--------------------------------------------------------
010800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010900910830     C*--------------------------------------------------------
011000070530     C     RWFILE        BEGSR
011100990910     C*
011200990914     C                   if        not %open(tivin00r)
011300990908     C                   open      tivin00r
011400990914     C                   endif
011500070530     C*
011600100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011700100525     C                   exsr      prevasx
011800010201     C*
011900010202     C                   if        chkcall = '0'
012000010202     C*
012100100525     C                   if        not %open(fivabwwr)
012200100525     C                   open      fivabwwr
012300100525     C                   endif
012400100525     C*
012500021113     C                   if        not %open(fivatwwr)
012600021113     C                   open      fivatwwr
012700010201     C                   endif
012800080117     C*
012900080117     C                   EXSR      INZVAR
013000080117     C                   EXSR      DEFCAM
013100990910     C*
013200010201     C                   clear                   §CTROKVB          5 0
013300020305     C                   clear                   §CTROKVT          5 0
013400000801     C                   clear                   §CTRMO            5 0
013500000801     C                   clear                   §CTRNO            5 0
013600990910     C*
013700020725     C/EXEC SQL
013800020725     C+ declare C1 cursor for select
013900111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
014000060223     C+ from tivin00r
014100111020     C+ order by sped
014200020725     C+ for read only
014300020725     C/END-EXEC
014400020725     C
014500020725     C/EXEC SQL
014600020725     C+ open C1
014700020725     C/END-EXEC
014800020725     C
014900020725     C/EXEC SQL
015000070530     C+ Fetch C1 into :INPUT_DS
015100020725     C/END-EXEC
015200020725     C*
015300020725     C                   dow       sqlcod = *zeros
015400990913     C*
015500100310     C                   if        vindta > *blanks
015600000613     C                   add       1             rrnum
015700000801     C*
015800020725     C                   if        vinflg = *blanks
015900020725     C                             or vinflg = '0'
016000020725     C                             or vinflg = '2'
016100000801     C*
016200020725     C                   clear                   x_vinmsg
016300020725     C                   eval      x_vinflg = '1'
016400101119     C*
016500101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016600101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016700010305     C*
016800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016900111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
017000010305     C*
017100100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017200080923     C                   if        *in50 = *off
017300101119     C                   exsr      inzvar
017400101119     C                   exsr      defcam
017500071003     C                   exsr      impvab
017600100525     C                   exsr      wrivab
017700111031     C                   exsr      exevata                                      => carico VAT
017800111031     C                   exsr      exevatb                                      => carico VAT
017900111031     C   51              exsr      exevate                                      => carico VAT
018000120911     C***                exsr      exevati                                      => carico VAT
018100071003     C                   else
018200080923     C*
018300071009     C                   if        wDISK = 'D'
018400091223     C                   exsr      repNSP
018500071009     C                   exsr      impvab
018600100412     C                   exsr      wrivab
018700111031     C                   exsr      exevata                                      => carico VAT
018800111031     C                   exsr      exevatb                                      => carico VAT
018900111031     C                   exsr      exevate                                      => carico VAT
019000120911     C***                exsr      exevati                                      => carico VAT
019100071009     C                   else
019200071009     C*
019300010305     C                   if        depspe = *blanks                             => 1° giro
019400010305     C                   eval      depspe = curspe                              => memorizz. spediz
019500080117     C                   clear                   tivinds
019600091223     C                   exsr      repNSP
019700020305     C                   exsr      impvab
019800111031     C                   exsr      exevata                                      => carico VAT
019900111031     C                   exsr      exevatb                                      => carico VAT
020000111031     C   50              exsr      exevate                                      => carico VAT
020100120911     C***                exsr      exevati                                      => carico VAT
020200010305     C                   else
020300020725     C                   if        depspe <> curspe                             => rottura di spediz
020400010305     C                   eval      depspe = curspe                              => memorizz. spediz
020500100412     C                   exsr      wrivab
020600080117     C                   clear                   tivinds
020700091223     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900111031     C                   exsr      exevata                                      => carico VAT
021000111031     C                   exsr      exevatb                                      => carico VAT
021100111031     C   50              exsr      exevate                                      => carico VAT
021200120911     C***                exsr      exevati                                      => carico VAT
021300020305     C                   else                                                   => x stessa spediz
021400100401     C*
021500100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021600100928     C                   select
021700100928     C                   when      wDUPREC = ' '
021800020305     C                   exsr      impvab
021900111031     C                   exsr      exevata                                      => carico VAT
022000111031     C                   exsr      exevatb                                      => carico VAT
022100111031     C   50              exsr      exevate                                      => carico VAT
022200120911     C***                exsr      exevati                                      => carico VAT
022300100928     C                   when      wDUPREC = 'C'
022400111031     C   50              exsr      exevate                                      => carico VAT
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
033100121109     C                   EVAL      VABCCM = 1661771
033200121109     C                   EVAL      VATCCM = 1661771
033300121109     C                   EVAL      VABLNP = 166
033400121109     C                   EVAL      VATLNP = 166
033500120910     C                   EVAL      VABCTR = 000
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
037700111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
037800111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
037900100525     C*
038000070222     C                   ENDIF
038100071009     C*
038200071009     C   50              EVAL      VABCTM = '7Q'
038300000801     C*
038400000801     C                   ENDSR
038500091223     C*----------------------------------------------------*
038600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038700091223     C*----------------------------------------------------*
038800091223     C     REPNSP        BEGSR
038900091223     C*
039000091223     C                   EXSR      INZVAR
039100091223     C                   EXSR      DEFCAM
039200100928     C*
039300100928     C                   SETON                                        60
039400091223     C*
039500091223     C* NSP => Stacco un numeratore da AZNUM
039600091223     C                   clear                   TRUL33DS
039700091223     C                   eval      I33OPE = *zeros
039800091223     C                   eval      I33CNU = 302
039900091223     C                   eval      I33NUM = 1
040000091223     C                   movel     TRUL33DS      KPJBU
040100091223     C                   call      'TRUL33R'
040200091223     C                   parm                    KPJBA
040300091223     C                   movel     KPJBU         TRUL33DS
040400091223     C                   if        O33ERR = *zeros
040500091223     C                   z-add     O33NRF        VABNSP
040600091223     C                   z-add     O33NRF        VATNSP
040700091223     C                   else
040800091223     C                   SETON                                        31
040900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041000091223     C                             + ' ' + 'VABNSP VATNSP'
041100091223     C                   endif
041200091223     C*
041300091223     C                   ENDSR
041400111031     C*----------------------------------------------------*
041500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041600111031     C*----------------------------------------------------*
041700111031     C     IMPVAB        BEGSR
041800111031     C*
041900111031     C                   Z-ADD     *zeros        errore            1 0
042000111031     C                   MOVEL     datcor        VABAAS
042100111031     C                   MOVEL     datcor        VATAAS
042200111031     C                   MOVE      datcor        VABMGS
042300111031     C                   MOVE(P)   vlrpoi        VABFGS
042400111031     C                   MOVE(P)   vlrpoi        VATFGS
042500111031     C*
042600120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042700111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042800111031     C     '@':'A'       XLATE     VABRSD        VABRSD
042900111031     C* ==
043000111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
043100111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043200111031     C                                    %trim(%subst(vindta:341:30))+' '+
043300111031     C                                    %trim(%subst(vindta:371:5)))
043400111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043500111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043600111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
043700121217     C***                EVAL      VABRMA=%trim(%subst(vindta:46:25))
043800121217     C                   EVAL      VABRMA=%trim(%subst(vindta:376:15))
043900111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
044000111031     C                   IF        %subst(vindta:43:1) = 'N'
044100111031     C                   EVAL      VABCBO='1'
044200111031     C                   ENDIF
044300111031     C                   IF        %subst(vindta:43:1) = 'P'
044400111031     C                   EVAL      VABCBO='2'
044500111031     C                   ENDIF
044600130116     C***                IF        %subst(vindta:511:3) = 'ABM'
044700130116     C***                EVAL      VABTIC='BM'
044800130116     C***                ENDIF
044900130116     C***                IF        %subst(vindta:511:3) = 'ABS'
045000130116     C***                EVAL      VABTIC='B'
045100130116     C***                ENDIF
045200130116     C***                IF        %subst(vindta:511:3) = 'CON'
045300130116     C***                EVAL      VABTIC=*blanks
045400130116     C***                ENDIF
045500130116     C***                IF        %subst(vindta:511:3) = 'ACM'
045600130116     C***                EVAL      VABTIC='CM'
045700130116     C***                ENDIF
045800130116     C***                IF        %subst(vindta:511:3) = 'ACS'
045900130116     C***                EVAL      VABTIC='CB'
046000130116     C***                ENDIF
046100130116     C***                IF        %subst(vindta:511:3) = 'VAR'
046200130116     C***                EVAL      VABTIC=*blanks
046300130116     C***                ENDIF
046400111031     C* CAP
046500111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
046600111031     C                   EXSR      CHKNUM
046700111031     C                   IF        PiInt=*on
046800111031     C                   Z-ADD     PiVal         Num5_0
046900111031     C                   MOVEL(P)  Num5_0        VABCAD
047000111031     C                   ELSE
047100111031     C                   ADD       1             errore
047200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047300111031     C                             + ' ' + 'VABCAD'
047400111031     C                   ENDIF
047500111031     C* Reperisco la provincia dal CAP e dalla località
047600111031     C                   IF        VABCAD <> *blanks AND
047700111031     C                             VABPRD  = *blanks
047800111031     C                   CLEAR                   TISI95DS
047900111031     C                   EVAL      I95TCN = '3'
048000111031     C                   Z-ADD     datcor        I95DAT
048100111031     C                   EVAL      I95CAP = VABCAD
048200111031     C                   EVAL      I95LOC = VABLOD
048300111031     C                   EVAL      I95NAR = VABNZD
048400111031     C                   CALL      'TISI95R'
048500111031     C                   PARM                    TISI95DS
048600111031     C                   EVAL      VABPRD = O95PRV
048700111031     C                   ENDIF
048800111031     C* NCL
048900111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
049000111031     C                   EXSR      CHKNUM
049100111031     C                   IF        PiInt=*on
049200111031     C                   SELECT
049300111031     C                   WHEN      wNCL = *blanks
049400111031     C                   Z-ADD     PiVal         VABNCL
049500111031     C                   WHEN      wNCL = 'A'
049600111031     C                   ADD       PiVal         VABNCL
049700111031     C                   WHEN      wNCL = '1'
049800111031     C                   ADD       1             VABNCL
049900111031     C                   ENDSL
050000111031     C                   ELSE
050100111031     C                   ADD       1             errore
050200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300111031     C                             + ' ' + 'VABNCL'
050400111031     C                   ENDIF
050500111031     C* PKB
050600120403     C* il peso è in grammi
050700111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
050800111031     C                   EXSR      CHKNUM
050900111031     C                   IF        PiNum=*on
051000120403     C                   EVAL      PiVal=PiVal/1000
051100111031     C                   Z-ADD     PiVal         VABPKB
051200130128     C* se il peso risultante è 0
051300130128     C                   IF        VABPKB = 0
051400130128     C* lo imposto a 0,1
051500130128     C                   EVAL      VABPKB = 0,1
051600130128     C                   ENDIF
051700111031     C                   ELSE
051800111031     C                   ADD       1             errore
051900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052000111031     C                             + ' ' + 'VABPKB'
052100111031     C                   ENDIF
052200111031     C* RMN
052300121109     C                   EVAL      PiStr=%trim(%subst(vindta:46:14))
052400120403     C*** elenco delle posizioni utilizzate nel tempo
052500121109     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
052600121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
052700120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
052800120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
052900111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
053000111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
053100111031     C                   EXSR      CHKNUM
053200111031     C                   IF        PiInt=*on
053300111031     C                   Z-ADD     PiVal         VABRMN
053400111031     C                   ELSE
053500111031     C                   ADD       1             errore
053600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053700111031     C                             + ' ' + 'VABRMN'
053800111031     C                   ENDIF
053900111031     C* CAS
054000111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
054100111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
054200111031     C                             %trim(%subst(vindta:502:9))<>*zeros
054300111031     C                   EVAL      VABVCA = 'EUR'
054400111031     C                   EVAL      FlgCAS = '1'
054500111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
054600111031     C                   EXSR      CHKNUM
054700111031     C                   IF        PiNum=*on
054800111031     C                   Z-ADD     PiVal         VABCAS
054900111031     C                   ELSE
055000111031     C                   ADD       1             errore
055100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055200111031     C                             + ' ' + 'VABCAS'
055300111031     C                   ENDIF
055400111031     C                   ENDIF
055500111031     C* IAS
055600111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
055700111031     C                   EVAL      VABVAS = 'EUR'
055800111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
055900111031     C                   EXSR      CHKNUM
056000111031     C                   IF        PiNum=*on
056100111031     C                   Z-ADD     PiVal         VABIAS
056200111031     C                   ELSE
056300111031     C                   ADD       1             errore
056400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056500111031     C                             + ' ' + 'VABIAS'
056600111031     C                   ENDIF
056700111031     C                   ENDIF
056800111031     C*
056900111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
057000111031     C                   IF        FlgCAS <> '0'
057100111031     C                   IF        VABCBO = '1'
057200111031     C                   EVAL      VABCBO = '4'
057300111031     C                   ELSE
057400111031     C                   EVAL      VABCBO = '6'
057500111031     C                   ENDIF
057600111031     C                   ENDIF
057700111031     C*
057800111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057900111031     C                   EXSR      CHKIMPDIV
058000111031     C*
058100111031     C* Ebbene...
058200111031     C                   ADD       1             §CTRMO
058300111031     C                   IF        errore <> *zeros
058400111031     C                   ADD       1             §CTRNO
058500111031     C                   EVAL      recko = vindta
058600111031     C                   EVAL      vinflg = '2'
058700111031     C                   ELSE
058800111031     C                   ADD       1             §CTROKVB
058900111031     C                   ENDIF
059000111031     C*
059100111031     C                   ENDSR
059200111031
059300111031
059400111031     C*----------------------------------------------------*
059500111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059600111031     C*----------------------------------------------------*
059700111031     C     CHKIMPDIV     BEGSR
059800111031     C*
059900111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060000111031     C                   Z-ADD     *zeros        wrkDec            9 9
060100111031     C*
060200111031     C* Come prima cosa effettuo considerazioni sulla divisa
060300111031     C                   IF        vabIAS > *zeros
060400111031     C                   IF        vabVAS <> 'EUR'
060500111031     C                   EVAL      vabVAS =  'ITL'
060600111031     C                   ENDIF
060700111031     C                   ENDIF
060800111031     C*
060900111031     C                   IF        vabCAS > *zeros
061000111031     C                   IF        vabVCA <> 'EUR'
061100111031     C                   EVAL      vabVCA =  'ITL'
061200111031     C                   ENDIF
061300130207     C***                SELECT
061400130207     C***                WHEN      VABCAS < 500
061500130207     C***                EVAL      VABTIC = *blank
061600130207     C***                WHEN      VABCAS >= 500
061700130116     C                   EVAL      VABTIC = 'BM'
061800130207     C***                ENDSL
061900111031     C                   ENDIF
062000111031     C*
062100111031     C                   IF        vabVMD > *zeros
062200111031     C                   IF        vabVAD <> 'EUR'
062300111031     C                   EVAL      vabVAD =  'ITL'
062400111031     C                   ENDIF
062500111031     C                   ENDIF
062600111031     C*
062700111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062800111031     C                   Z-ADD     vabIAS        wrkDec
062900111031     C                   IF        wrkDec > *zeros
063000111031     C                   IF        vabVAS = 'ITL'
063100111031     C                   EVAL      vabIAS = *zeros
063200111031     C                   ENDIF
063300111031     C                   ENDIF
063400111031     C*
063500111031     C* Stabilisco se il contrasegno ha decimali valorizzati
063600111031     C                   Z-ADD     vabCAS        wrkDec
063700111031     C                   IF        wrkDec > *zeros
063800111031     C                   IF        vabVCA = 'ITL'
063900111031     C                   EVAL      vabCAS = *zeros
064000111031     C                   ENDIF
064100111031     C                   ENDIF
064200111031     C*
064300111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064400111031     C                   Z-ADD     vabVMD        wrkDec
064500111031     C                   IF        wrkDec > *zeros
064600111031     C                   IF        vabVAD = 'ITL'
064700111031     C                   EVAL      vabVMD = *zeros
064800111031     C                   ENDIF
064900111031     C                   ENDIF
065000111031     C*
065100111031     C                   ENDSR
065200111031     C***
065300111031
065400111031
065500111031     C*----------------------------------------------------*
065600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
065700111031     C*----------------------------------------------------*
065800111031     C     EXEVATA       BEGSR
065900111031     C*
066000111031     C***                EVAL      VATTRC='A'
066100111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
066200111031     C***                exsr      wrivat                                       => scarico VAT
066300111031     C*
066400111031     C                   ENDSR
066500111031     C*----------------------------------------------------*
066600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
066700111031     C*----------------------------------------------------*
066800111031     C     EXEVATB       BEGSR
066900111031     C*
067000121217     C***                EVAL      VATTRC='B'
067100121217     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
067200121217     C***                exsr      wrivat                                       => scarico VAT
067300111031     C*
067400111031     C                   ENDSR
067500111031     C*----------------------------------------------------*
067600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
067700111031     C*----------------------------------------------------*
067800111031     C     EXEVATE       BEGSR
067900111031     C*
068000111031     C                   EVAL      VATTRC='E'
068100111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
068200111031     C                   exsr      wrivat                                       => scarico VAT
068300111031     C*
068400111031     C                   ENDSR
068500120402     C*----------------------------------------------------*
068600120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
068700120402     C*----------------------------------------------------*
068800120402     C     EXEVATI       BEGSR
068900120402     C*
069000120402     C                   EVAL      VATTRC='I'
069100120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
069200120402     C                   exsr      wrivat                                       => scarico VAT
069300120402     C*
069400120402     C                   EVAL      VATTRC='J'
069500120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
069600120402     C                   exsr      wrivat                                       => scarico VAT
069700120402     C*
069800120402     C                   ENDSR
069900111031     C*----------------------------------------------------*
070000111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
070100111031     C*----------------------------------------------------*
070200111031     C     WRIVAT        BEGSR
070300111031     C*
070400111031     C* Scrivo solo se valorizzato qualcosa
070500111031     C                   IF        VATNOT <> *blanks
070600111031     C                   WRITE     FIVAT000
070700111031     C                   ENDIF
070800111031     C*
070900111031     C                   ENDSR
071000020904
071100020904
071200010202     C*----------------------------------------------------*
071300100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071400010202     C*----------------------------------------------------*
071500100525     C     PREVASX       BEGSR
071600100525     C*
071700021113     C* Compongo il nome del membro da dare al FIVATWWR
071800010202     C                   eval      parmbr = vlrhdl
071900010202     C                   movel     'M'           parmbr
072000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072100010202     C                   eval      paropz = '1'
072200100525     C*
072300010202     C* Effettuo la chiamata al CLLE preposto
072400100525     C  N51              call(e)   'TITVVTC'
072500010202     C                   parm                    parccm
072600010202     C                   parm                    parmbr
072700010202     C                   parm                    paropz
072800100525     C   51              call(e)   'TITVVBC'
072900100525     C                   parm                    parccm
073000100525     C                   parm                    parmbr
073100100525     C                   parm                    paropz
073200100525     C*
073300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073400010202     C                   if        %error
073500010202     C                   movel     '1'           chkcall
073600010202     C                   else
073700010202     C                   movel     '0'           chkcall
073800010202     C                   endif
073900010202     C*
074000010202     C                   ENDSR
074100000801     C*----------------------------------------------------*
074200000801     C*  CONTROLLO NUMERICITA' CAMPI
074300000801     C*----------------------------------------------------*
074400000801     C     CHKNUM        BEGSR
074500081113     C*
074600081113     C                   IF        PiDecChr = *blanks
074700110408     C                   EVAL      PiDecChr = '.'
074800081113     C                   ENDIF
074900091223     C*
075000081113     C                   callp(e)  UBISNUM_Check(PiStr
075100081113     C                                          :PiDecChr
075200081113     C                                          :PiVal
075300081113     C                                          :PiNum
075400081113     C                                          :PiInt)
075500081113     C*
075600000801     C                   IF        %error
075700000801     C                   EVAL      PiInt=*off
075800000801     C                   ENDIF
075900000801     C*
076000000801     C                   ENDSR
076100000801     C***
076200011113
076300011113
076400000801
076500000801
076600990920      /TITLE Invio dei dati al punto operativo.
076700010202     C     invio         BEGSR
076800990920     C*
076900021113     C* 1° invio FIVAT
077000010201     C                   reset                   dscmz
077100110411     C                   move      vatfgs        cmzdst
077200021113     C                   eval      cmzfld = 'FIVATWWR'
077300010201     C                   eval      cmzmbd = vlrhdl
077400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077500021009     C***                if        prmfir = *blanks
077600021113     C                   eval      cmzfla = 'FIVAT00F'
077700021113     C                   eval      cmzmba = 'FIVAT00F'
077800021009     C***                else
077900021009     C***                eval      cmzfla = prmfir
078000021009     C***                eval      cmzmba = prmfir
078100021009     C***                endif
078200010201     C                   eval      cmznrr = *zeros
078300020305     C                   move      §ctrokvt      cmznrr
078400021018     C                   eval      cmzlba = vlrfl1
078500010201     C                   call(e)   'TIS711C'
078600010201     C                   parm                    dscmz
078700010201     C                   parm      *blanks       esito
078800010205     C                   if        %error
078900010205     C                             or cmzerr = '1'
079000010205     C                             or esito  = '1'
079100010205     C                   eval      wrkesito = '3'
079200010205     C                   else
079300010201     C*
079400021113     C* 2° invio FIVAB
079500100525     C                   if        *in51 = *off
079600010201     C                   reset                   dscmz
079700110411     C                   move      vabfgs        cmzdst
079800010201     C                   eval      cmzfld = vlrfou
079900010201     C                   eval      cmzmbd = vlrhdl
080000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080100021009     C***                if        prmfir = *blanks
080200021113     C                   eval      cmzfla = 'FIVAB00F'
080300021113     C                   eval      cmzmba = 'FIVAB00F'
080400021009     C***                else
080500021009     C***                eval      cmzfla = prmfir
080600021009     C***                eval      cmzmba = prmfir
080700021009     C***                endif
080800010201     C                   eval      cmznrr = *zeros
080900010201     C                   move      §ctrokvb      cmznrr
081000021018     C                   eval      cmzlba = vlrfl1
081100010201     C                   call(e)   'TIS711C'
081200010201     C                   parm                    dscmz
081300010201     C                   parm      *blanks       esito
081400010201     C                   if        %error
081500010201     C                             or cmzerr = '1'
081600010201     C                             or esito  = '1'
081700010201     C                   eval      wrkesito = '3'
081800010201     C                   endif
081900100525     C                   endif
082000010205     C                   endif
082100990920     C*
082200000613     C                   ENDSR
082300000613     C***
082400070411
082500110627     C     *pssr         BEGSR
082600070411     C*
082700110627     C                   if        %open(tivin00r)
082800110627     C                   close     tivin00r
082900110627     C                   endif
083000110627     C                   if        %open(fivabwwr)
083100110627     C                   close     fivabwwr
083200110627     C                   endif
083300110627     C                   if        %open(fivatwwr)
083400110627     C                   close     fivatwwr
083500110627     C                   endif
083600110627     C*
083700110627     C* Effettuo la chiamata al CLLE preposto
083800110627     C  N51              call(e)   'TITVVTC'
083900110627     C                   parm                    parccm
084000110627     C                   parm                    parmbr
084100110627     C                   parm      '2'           paropz
084200110627     C   51              call(e)   'TITVVBC'
084300110627     C                   parm                    parccm
084400110627     C                   parm                    parmbr
084500110627     C                   parm      '2'           paropz
084600110627     C*
084700110627     C                   eval      wrkesito = '2'
084800110627     C*
084900110627     C                   seton                                        LR
085000110627     C*
085100110627     C                   ENDSR     '*CANCL'
085200070411     C***
085300070411
085400990910
085500000613     C     *inzsr        BEGSR
085600990910     C*
085700990910     C     *entry        plist
085800990920     C                   parm                    tivlrds
085900990921     C                   parm      wrkesito      esito
086000000724     C                   parm                    prmlit
086100000710     C                   parm                    prmfir
086200000613     C*
086300000830     C* CALCOLA LA DATA CORRENTE
086400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
086500091223     C                   eval      datcor = %dec(%date() : *ISO)
086600000830     C*
086700000613     C                   ENDSR
086800000613     C***
