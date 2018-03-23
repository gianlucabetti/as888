000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300150430
000400130116      * PARTICOLARITA':
000500130207      * Valorizzare il campo VABTIC sempre a BM a prescindere dal importo contrassegno
000600150407      * 07/04/15 in caso di importo contrassegno valorizzato, VABTIC = *blank
000700150430      * 30/04/15 VABTIC lo si determina da pos. 511x3: se ABS->VABTIC=BB, se CON->VABTIC=SC
000800150430      *          quindi non è più valido che se c/assegno valorizzato VABTIC=*blank
000900130128      * se il peso risultante è 0 , lo imposto a 0,1
001000130116
001100020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001200100412     FFIVABwwr  O    E             DISK    usropn
001300021113     FFIVATwwr  O    E             DISK    usropn
001400990908
001500000801     D*----------------------------------------------------
001600000801     D* DICHIARAZIOINE VARIABILI DI WRK
001700000801     D*----------------------------------------------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100990920     D tivlrds       e ds                  extname(tivlr00f)
002200070730     D tisi95ds      e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800000621     D recko           s            150    INZ(*blanks)
002900011113     D depcmd          s            150    INZ(*blanks)
003000111031     D depspe          s             25    INZ(*blanks)
003100111031     D curspe          s             25    INZ(*blanks)
003200010202     D parccm          s              8    INZ(*blanks)
003300010202     D parmbr          s             10    INZ(*blanks)
003400010202     D paropz          s              1    INZ(*blanks)
003500010202     D chkcall         s              1    INZ(*blanks)
003600080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003700091223     D Num5_0          s              5  0
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300100729     D  VINDTA                     2048
004400100729     D  VINFLG                        1
004500111031     D  VINSPE                       25
004600100729     D  VINRRN                        8  0
004700020725     D*
004800081113
004900091223     D*------------------
005000091223     D* DS REPERIMENTO NUMERATORE
005100091223     D*------------------
005200100309     D trul33ds      e ds                  inz
005300091223     D*------------------
005400091223     D* DS ARCHITETTURA
005500091223     D*------------------
005600091223     D kpjba         e ds                  inz
005700091223
005800101119
005900101119     D*-------------------
006000101119     D* COSTANTI
006100101119     D*-------------------
006200101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006300101119     D                                     èéù')
006400101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006500101119     D                                     EEU')
006600081113
006700081113     D*------------------
006800081113     D* LINKING A DEFINIZIONI ESTERNE
006900081113     D*------------------
007000081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007100090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007200081113
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008800070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008900000613     C*
009000010202     C* Effettuo la chiamata al CLLE preposto
009100100525     C  N51              call(e)   'TITVVTC'
009200100525     C                   parm                    parccm
009300100525     C                   parm                    parmbr
009400100525     C                   parm      '2'           paropz
009500100525     C   51              call(e)   'TITVVBC'
009600100525     C                   parm                    parccm
009700100525     C                   parm                    parmbr
009800100525     C                   parm      '2'           paropz
009900070730     C*
010000070730     C* Effettuo lancio TISI95 solo x chiusura
010100070730     C                   CLEAR                   TISI95DS
010200070730     C                   EVAL      I95TLA = 'C'
010300070730     C                   CALL      'TISI95R'
010400070730     C                   PARM                    TISI95DS
010500000616     C*
010600000801     C
010700010201     C                   seton                                        LR
010800990908
010900000801
011000910830     C*--------------------------------------------------------
011100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
011200910830     C*--------------------------------------------------------
011300070530     C     RWFILE        BEGSR
011400990910     C*
011500990914     C                   if        not %open(tivin00r)
011600990908     C                   open      tivin00r
011700990914     C                   endif
011800070530     C*
011900100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
012000100525     C                   exsr      prevasx
012100010201     C*
012200010202     C                   if        chkcall = '0'
012300010202     C*
012400100525     C                   if        not %open(fivabwwr)
012500100525     C                   open      fivabwwr
012600100525     C                   endif
012700100525     C*
012800021113     C                   if        not %open(fivatwwr)
012900021113     C                   open      fivatwwr
013000010201     C                   endif
013100080117     C*
013200080117     C                   EXSR      INZVAR
013300080117     C                   EXSR      DEFCAM
013400990910     C*
013500010201     C                   clear                   §CTROKVB          5 0
013600020305     C                   clear                   §CTROKVT          5 0
013700000801     C                   clear                   §CTRMO            5 0
013800000801     C                   clear                   §CTRNO            5 0
013900990910     C*
014000020725     C/EXEC SQL
014100020725     C+ declare C1 cursor for select
014200111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
014300060223     C+ from tivin00r
014400111020     C+ order by sped
014500020725     C+ for read only
014600020725     C/END-EXEC
014700020725     C
014800020725     C/EXEC SQL
014900020725     C+ open C1
015000020725     C/END-EXEC
015100020725     C
015200020725     C/EXEC SQL
015300070530     C+ Fetch C1 into :INPUT_DS
015400020725     C/END-EXEC
015500020725     C*
015600020725     C                   dow       sqlcod = *zeros
015700990913     C*
015800100310     C                   if        vindta > *blanks
015900000613     C                   add       1             rrnum
016000000801     C*
016100020725     C                   if        vinflg = *blanks
016200020725     C                             or vinflg = '0'
016300020725     C                             or vinflg = '2'
016400000801     C*
016500020725     C                   clear                   x_vinmsg
016600020725     C                   eval      x_vinflg = '1'
016700101119     C*
016800101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016900101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
017000010305     C*
017100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017200111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
017300010305     C*
017400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017500080923     C                   if        *in50 = *off
017600101119     C                   exsr      inzvar
017700101119     C                   exsr      defcam
017800071003     C                   exsr      impvab
017900100525     C                   exsr      wrivab
018000111031     C                   exsr      exevata                                      => carico VAT
018100111031     C                   exsr      exevatb                                      => carico VAT
018200111031     C   51              exsr      exevate                                      => carico VAT
018300120911     C***                exsr      exevati                                      => carico VAT
018400071003     C                   else
018500080923     C*
018600071009     C                   if        wDISK = 'D'
018700091223     C                   exsr      repNSP
018800071009     C                   exsr      impvab
018900100412     C                   exsr      wrivab
019000111031     C                   exsr      exevata                                      => carico VAT
019100111031     C                   exsr      exevatb                                      => carico VAT
019200111031     C                   exsr      exevate                                      => carico VAT
019300120911     C***                exsr      exevati                                      => carico VAT
019400071009     C                   else
019500071009     C*
019600010305     C                   if        depspe = *blanks                             => 1° giro
019700010305     C                   eval      depspe = curspe                              => memorizz. spediz
019800080117     C                   clear                   tivinds
019900091223     C                   exsr      repNSP
020000020305     C                   exsr      impvab
020100111031     C                   exsr      exevata                                      => carico VAT
020200111031     C                   exsr      exevatb                                      => carico VAT
020300111031     C   50              exsr      exevate                                      => carico VAT
020400120911     C***                exsr      exevati                                      => carico VAT
020500010305     C                   else
020600020725     C                   if        depspe <> curspe                             => rottura di spediz
020700010305     C                   eval      depspe = curspe                              => memorizz. spediz
020800100412     C                   exsr      wrivab
020900080117     C                   clear                   tivinds
021000091223     C                   exsr      repNSP
021100020305     C                   exsr      impvab
021200111031     C                   exsr      exevata                                      => carico VAT
021300111031     C                   exsr      exevatb                                      => carico VAT
021400111031     C   50              exsr      exevate                                      => carico VAT
021500120911     C***                exsr      exevati                                      => carico VAT
021600020305     C                   else                                                   => x stessa spediz
021700100401     C*
021800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021900100928     C                   select
022000100928     C                   when      wDUPREC = ' '
022100020305     C                   exsr      impvab
022200111031     C                   exsr      exevata                                      => carico VAT
022300111031     C                   exsr      exevatb                                      => carico VAT
022400111031     C   50              exsr      exevate                                      => carico VAT
022500120911     C***                exsr      exevati                                      => carico VAT
022600100928     C                   when      wDUPREC = 'C'
022700111031     C   50              exsr      exevate                                      => carico VAT
022800100928     C                   endsl
022900010305     C                   endif
023000010305     C                   endif
023100010305     C                   endif
023200071003     C                   endif
023300110201     C*
023400071009     C                   endif
023500000905     C*
023600000905     C                   else
023700020725     C                   eval      x_vinflg = '1'
023800000905     C                   endif
023900000905     C*
024000020725     C     VINRRN        chain     tivin000
024100020725     C                   if        %found(tivin00r)
024200020725     C                   eval      y_vinflg = x_vinflg
024300020725     C                   eval      y_vinmsg = x_vinmsg
024400020725     C                   update    tivin000
024500020725     C                   endif
024600020725     C*
024700020725     C/EXEC SQL
024800070530     C+ Fetch C1 into :INPUT_DS
024900020725     C/END-EXEC
025000020725     C*
025100020725     C                   enddo
025200020725     C*
025300020725     C/EXEC SQL
025400020725     C+ close C1
025500020725     C/END-EXEC
025600000905     C*
025700020305     C* Scarico i VAB rimasti "in sospeso"
025800071009     C                   if        wDISK = 'C'
025900100412     C                   exsr      wrivab
026000071009     C                   endif
026100010202     C*
026200010202     C                   endif
026300990910
026400990910     C* Se non ci sono record con errori ...
026500000710     C                   if        §ctrno = 0
026600990910     C* ... restituisco esito OK.
026700990921     C                   eval      wrkesito = '0'
026800990910     C                   else
026900100525     C                   if        §ctrokvb > 0 OR
027000100525     C                             §ctrokvt > 0
027100990921     C                   eval      wrkesito = '1'
027200000710     C                   else
027300000710     C                   eval      wrkesito = '2'
027400990910     C                   endif
027500000710     C                   endif
027600990910     C*
027700990914     C                   if        %open(tivin00r)
027800990908     C                   close     tivin00r
027900990914     C                   endif
028000021113     C                   if        %open(fivabwwr)
028100021113     C                   close     fivabwwr
028200990914     C                   endif
028300021113     C                   if        %open(fivatwwr)
028400021113     C                   close     fivatwwr
028500010201     C                   endif
028600990910     C*
028700100525     C                   if        §ctrokvb > 0 OR
028800100525     C                             §ctrokvt > 0
028900000724     C                             and vlrpoi <> *zeros
029000010202     C                   exsr      invio
029100990920     C                   endif
029200990920     C*
029300910830     C                   ENDSR
029400000613     C***
029500100412
029600100412
029700010305
029800010305     C*----------------------------------------------------*
029900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030000010305     C*----------------------------------------------------*
030100020305     C     WRIVAB        BEGSR
030200100412     C*
030300100412     C* Gestisco l'eventuale rottura x numero spedizione
030400100412     C                   if        VABRMA = *blanks
030500100412     C                   movel(P)  VABRMN        VABRMA
030600100412     C                   endif
030700100412     C*
030800100525     C                   if        *in51 = *off and *in31 = *off
030900100525     C                   write     fivab000                                     => scarico il VAB
031000100525     C                   endif
031100010305     C*
031200010305     C                   ENDSR
031300990920
031400000801     C*----------------------------------------------------*
031500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031600000801     C*----------------------------------------------------*
031700010201     C     INZVAR        BEGSR
031800000801     C*
031900010201     C                   Z-ADD     *zeros        Num5_0
032000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032100020725     C                   MOVEL     '0'           FlgCAS            1
032200000801     C*
032300000801     C                   ENDSR
032400000801     C*----------------------------------------------------*
032500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032600000801     C*----------------------------------------------------*
032700020904     C     DEFCAM        BEGSR
032800080923     C*
032900100928     C                   SETOFF                                       505160
033000000801     C*
033100021113     C                   CLEAR                   FIVAB000
033200021113     C                   CLEAR                   FIVAT000
033300070730     C* Imposto i valori di default...
033400121109     C                   EVAL      VABCCM = 1661771
033500121109     C                   EVAL      VATCCM = 1661771
033600121109     C                   EVAL      VABLNP = 166
033700121109     C                   EVAL      VATLNP = 166
033800120910     C                   EVAL      VABCTR = 000
033900100525     C                   EVAL      VABCBO = '1'
034000070222     C* ... e poi verifico se sono stati passati come parametri
034100070222     C                   IF        vlrppt > *blanks
034200100525     C*
034300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034500070222     C                   EXSR      CHKNUM
034600070222     C                   IF        PiInt=*on
034700070222     C                   Z-ADD     PiVal         VABCCM
034800070222     C                   Z-ADD     PiVal         VATCCM
034900070222     C                   ENDIF
035000070222     C                   ENDIF
035100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035300070222     C                   EXSR      CHKNUM
035400070222     C                   IF        PiInt=*on
035500070222     C                   Z-ADD     PiVal         VABLNP
035600070222     C                   Z-ADD     PiVal         VATLNP
035700070222     C                   ENDIF
035800070222     C                   ENDIF
035900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036100070222     C                   EXSR      CHKNUM
036200070222     C                   IF        PiInt=*on
036300070222     C                   Z-ADD     PiVal         VABCTR
036400070222     C                   ENDIF
036500070928     C                   ENDIF
036600071009     C                   MOVEL     *blanks       wDISK             1
036700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036800071009     C                   IF        wDISK <> *blanks
036900070928     C                   SETON                                        50
037000070222     C                   ENDIF
037100100525     C                   IF        wDISK  = 'T'
037200100525     C                   SETOFF                                       50
037300100525     C                   SETON                                        51
037400100525     C                   ENDIF
037500100401     C                   MOVEL     *blanks       wDUPREC           1
037600100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037700100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037800100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037900100525     C                   ENDIF
038000111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
038100111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
038200100525     C*
038300070222     C                   ENDIF
038400071009     C*
038500071009     C   50              EVAL      VABCTM = '7Q'
038600000801     C*
038700000801     C                   ENDSR
038800091223     C*----------------------------------------------------*
038900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039000091223     C*----------------------------------------------------*
039100091223     C     REPNSP        BEGSR
039200091223     C*
039300091223     C                   EXSR      INZVAR
039400091223     C                   EXSR      DEFCAM
039500100928     C*
039600100928     C                   SETON                                        60
039700091223     C*
039800091223     C* NSP => Stacco un numeratore da AZNUM
039900091223     C                   clear                   TRUL33DS
040000091223     C                   eval      I33OPE = *zeros
040100091223     C                   eval      I33CNU = 302
040200091223     C                   eval      I33NUM = 1
040300091223     C                   movel     TRUL33DS      KPJBU
040400091223     C                   call      'TRUL33R'
040500091223     C                   parm                    KPJBA
040600091223     C                   movel     KPJBU         TRUL33DS
040700091223     C                   if        O33ERR = *zeros
040800091223     C                   z-add     O33NRF        VABNSP
040900091223     C                   z-add     O33NRF        VATNSP
041000091223     C                   else
041100091223     C                   SETON                                        31
041200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300091223     C                             + ' ' + 'VABNSP VATNSP'
041400091223     C                   endif
041500091223     C*
041600091223     C                   ENDSR
041700111031     C*----------------------------------------------------*
041800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041900111031     C*----------------------------------------------------*
042000111031     C     IMPVAB        BEGSR
042100111031     C*
042200111031     C                   Z-ADD     *zeros        errore            1 0
042300111031     C                   MOVEL     datcor        VABAAS
042400111031     C                   MOVEL     datcor        VATAAS
042500111031     C                   MOVE      datcor        VABMGS
042600111031     C                   MOVE(P)   vlrpoi        VABFGS
042700111031     C                   MOVE(P)   vlrpoi        VATFGS
042800111031     C*
042900120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
043000111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043100111031     C     '@':'A'       XLATE     VABRSD        VABRSD
043200111031     C* ==
043300111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
043400111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043500111031     C                                    %trim(%subst(vindta:341:30))+' '+
043600111031     C                                    %trim(%subst(vindta:371:5)))
043700111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043800111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043900111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
044000121217     C***                EVAL      VABRMA=%trim(%subst(vindta:46:25))
044100121217     C                   EVAL      VABRMA=%trim(%subst(vindta:376:15))
044200111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
044300111031     C                   IF        %subst(vindta:43:1) = 'N'
044400111031     C                   EVAL      VABCBO='1'
044500111031     C                   ENDIF
044600111031     C                   IF        %subst(vindta:43:1) = 'P'
044700111031     C                   EVAL      VABCBO='2'
044800111031     C                   ENDIF
044900130116     C***                IF        %subst(vindta:511:3) = 'ABM'
045000130116     C***                EVAL      VABTIC='BM'
045100130116     C***                ENDIF
045200130116     C***                IF        %subst(vindta:511:3) = 'ABS'
045300130116     C***                EVAL      VABTIC='B'
045400130116     C***                ENDIF
045500130116     C***                IF        %subst(vindta:511:3) = 'CON'
045600130116     C***                EVAL      VABTIC=*blanks
045700130116     C***                ENDIF
045800130116     C***                IF        %subst(vindta:511:3) = 'ACM'
045900130116     C***                EVAL      VABTIC='CM'
046000130116     C***                ENDIF
046100130116     C***                IF        %subst(vindta:511:3) = 'ACS'
046200130116     C***                EVAL      VABTIC='CB'
046300130116     C***                ENDIF
046400130116     C***                IF        %subst(vindta:511:3) = 'VAR'
046500130116     C***                EVAL      VABTIC=*blanks
046600130116     C***                ENDIF
046700150430     C                   IF        %subst(vindta:511:3) = 'ABS'
046800150430     C                   EVAL      VABTIC='BB'
046900150430     C                   ENDIF
047000150430     C                   IF        %subst(vindta:511:3) = 'CON'
047100150430     C                   EVAL      VABTIC='SC'
047200150430     C                   ENDIF
047300111031     C* CAP
047400111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
047500111031     C                   EXSR      CHKNUM
047600111031     C                   IF        PiInt=*on
047700111031     C                   Z-ADD     PiVal         Num5_0
047800111031     C                   MOVEL(P)  Num5_0        VABCAD
047900111031     C                   ELSE
048000111031     C                   ADD       1             errore
048100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048200111031     C                             + ' ' + 'VABCAD'
048300111031     C                   ENDIF
048400111031     C* Reperisco la provincia dal CAP e dalla località
048500111031     C                   IF        VABCAD <> *blanks AND
048600111031     C                             VABPRD  = *blanks
048700111031     C                   CLEAR                   TISI95DS
048800111031     C                   EVAL      I95TCN = '3'
048900111031     C                   Z-ADD     datcor        I95DAT
049000111031     C                   EVAL      I95CAP = VABCAD
049100111031     C                   EVAL      I95LOC = VABLOD
049200111031     C                   EVAL      I95NAR = VABNZD
049300111031     C                   CALL      'TISI95R'
049400111031     C                   PARM                    TISI95DS
049500111031     C                   EVAL      VABPRD = O95PRV
049600111031     C                   ENDIF
049700111031     C* NCL
049800111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
049900111031     C                   EXSR      CHKNUM
050000111031     C                   IF        PiInt=*on
050100111031     C                   SELECT
050200111031     C                   WHEN      wNCL = *blanks
050300111031     C                   Z-ADD     PiVal         VABNCL
050400111031     C                   WHEN      wNCL = 'A'
050500111031     C                   ADD       PiVal         VABNCL
050600111031     C                   WHEN      wNCL = '1'
050700111031     C                   ADD       1             VABNCL
050800111031     C                   ENDSL
050900111031     C                   ELSE
051000111031     C                   ADD       1             errore
051100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200111031     C                             + ' ' + 'VABNCL'
051300111031     C                   ENDIF
051400111031     C* PKB
051500120403     C* il peso è in grammi
051600111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
051700111031     C                   EXSR      CHKNUM
051800111031     C                   IF        PiNum=*on
051900120403     C                   EVAL      PiVal=PiVal/1000
052000111031     C                   Z-ADD     PiVal         VABPKB
052100130128     C* se il peso risultante è 0
052200130128     C                   IF        VABPKB = 0
052300130128     C* lo imposto a 0,1
052400130128     C                   EVAL      VABPKB = 0,1
052500130128     C                   ENDIF
052600111031     C                   ELSE
052700111031     C                   ADD       1             errore
052800111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900111031     C                             + ' ' + 'VABPKB'
053000111031     C                   ENDIF
053100111031     C* RMN
053200121109     C                   EVAL      PiStr=%trim(%subst(vindta:46:14))
053300120403     C*** elenco delle posizioni utilizzate nel tempo
053400121109     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
053500121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
053600120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
053700120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
053800111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
053900111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
054000111031     C                   EXSR      CHKNUM
054100111031     C                   IF        PiInt=*on
054200111031     C                   Z-ADD     PiVal         VABRMN
054300111031     C                   ELSE
054400111031     C                   ADD       1             errore
054500111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600111031     C                             + ' ' + 'VABRMN'
054700111031     C                   ENDIF
054800111031     C* CAS
054900111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
055000111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
055100111031     C                             %trim(%subst(vindta:502:9))<>*zeros
055200111031     C                   EVAL      VABVCA = 'EUR'
055300111031     C                   EVAL      FlgCAS = '1'
055400111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
055500111031     C                   EXSR      CHKNUM
055600111031     C                   IF        PiNum=*on
055700111031     C                   Z-ADD     PiVal         VABCAS
055800111031     C                   ELSE
055900111031     C                   ADD       1             errore
056000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056100111031     C                             + ' ' + 'VABCAS'
056200111031     C                   ENDIF
056300111031     C                   ENDIF
056400111031     C* IAS
056500111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
056600111031     C                   EVAL      VABVAS = 'EUR'
056700111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
056800111031     C                   EXSR      CHKNUM
056900111031     C                   IF        PiNum=*on
057000111031     C                   Z-ADD     PiVal         VABIAS
057100111031     C                   ELSE
057200111031     C                   ADD       1             errore
057300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057400111031     C                             + ' ' + 'VABIAS'
057500111031     C                   ENDIF
057600111031     C                   ENDIF
057700111031     C*
057800111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
057900111031     C                   IF        FlgCAS <> '0'
058000111031     C                   IF        VABCBO = '1'
058100111031     C                   EVAL      VABCBO = '4'
058200111031     C                   ELSE
058300111031     C                   EVAL      VABCBO = '6'
058400111031     C                   ENDIF
058500111031     C                   ENDIF
058600111031     C*
058700111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058800111031     C                   EXSR      CHKIMPDIV
058900111031     C*
059000111031     C* Ebbene...
059100111031     C                   ADD       1             §CTRMO
059200111031     C                   IF        errore <> *zeros
059300111031     C                   ADD       1             §CTRNO
059400111031     C                   EVAL      recko = vindta
059500111031     C                   EVAL      vinflg = '2'
059600111031     C                   ELSE
059700111031     C                   ADD       1             §CTROKVB
059800111031     C                   ENDIF
059900111031     C*
060000111031     C                   ENDSR
060100111031
060200111031
060300111031     C*----------------------------------------------------*
060400111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060500111031     C*----------------------------------------------------*
060600111031     C     CHKIMPDIV     BEGSR
060700111031     C*
060800111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060900111031     C                   Z-ADD     *zeros        wrkDec            9 9
061000111031     C*
061100111031     C* Come prima cosa effettuo considerazioni sulla divisa
061200111031     C                   IF        vabIAS > *zeros
061300111031     C                   IF        vabVAS <> 'EUR'
061400111031     C                   EVAL      vabVAS =  'ITL'
061500111031     C                   ENDIF
061600111031     C                   ENDIF
061700111031     C*
061800111031     C                   IF        vabCAS > *zeros
061900111031     C                   IF        vabVCA <> 'EUR'
062000111031     C                   EVAL      vabVCA =  'ITL'
062100111031     C                   ENDIF
062200130207     C***                SELECT
062300130207     C***                WHEN      VABCAS < 500
062400130207     C***                EVAL      VABTIC = *blank
062500130207     C***                WHEN      VABCAS >= 500
062600150430     C***                EVAL      VABTIC = *blank
062700130207     C***                ENDSL
062800111031     C                   ENDIF
062900111031     C*
063000111031     C                   IF        vabVMD > *zeros
063100111031     C                   IF        vabVAD <> 'EUR'
063200111031     C                   EVAL      vabVAD =  'ITL'
063300111031     C                   ENDIF
063400111031     C                   ENDIF
063500111031     C*
063600111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063700111031     C                   Z-ADD     vabIAS        wrkDec
063800111031     C                   IF        wrkDec > *zeros
063900111031     C                   IF        vabVAS = 'ITL'
064000111031     C                   EVAL      vabIAS = *zeros
064100111031     C                   ENDIF
064200111031     C                   ENDIF
064300111031     C*
064400111031     C* Stabilisco se il contrasegno ha decimali valorizzati
064500111031     C                   Z-ADD     vabCAS        wrkDec
064600111031     C                   IF        wrkDec > *zeros
064700111031     C                   IF        vabVCA = 'ITL'
064800111031     C                   EVAL      vabCAS = *zeros
064900111031     C                   ENDIF
065000111031     C                   ENDIF
065100111031     C*
065200111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065300111031     C                   Z-ADD     vabVMD        wrkDec
065400111031     C                   IF        wrkDec > *zeros
065500111031     C                   IF        vabVAD = 'ITL'
065600111031     C                   EVAL      vabVMD = *zeros
065700111031     C                   ENDIF
065800111031     C                   ENDIF
065900111031     C*
066000111031     C                   ENDSR
066100111031     C***
066200111031
066300111031
066400111031     C*----------------------------------------------------*
066500111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
066600111031     C*----------------------------------------------------*
066700111031     C     EXEVATA       BEGSR
066800111031     C*
066900111031     C***                EVAL      VATTRC='A'
067000111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
067100111031     C***                exsr      wrivat                                       => scarico VAT
067200111031     C*
067300111031     C                   ENDSR
067400111031     C*----------------------------------------------------*
067500111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
067600111031     C*----------------------------------------------------*
067700111031     C     EXEVATB       BEGSR
067800111031     C*
067900121217     C***                EVAL      VATTRC='B'
068000121217     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
068100121217     C***                exsr      wrivat                                       => scarico VAT
068200111031     C*
068300111031     C                   ENDSR
068400111031     C*----------------------------------------------------*
068500111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
068600111031     C*----------------------------------------------------*
068700111031     C     EXEVATE       BEGSR
068800111031     C*
068900111031     C                   EVAL      VATTRC='E'
069000111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
069100111031     C                   exsr      wrivat                                       => scarico VAT
069200111031     C*
069300111031     C                   ENDSR
069400120402     C*----------------------------------------------------*
069500120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
069600120402     C*----------------------------------------------------*
069700120402     C     EXEVATI       BEGSR
069800120402     C*
069900120402     C                   EVAL      VATTRC='I'
070000120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
070100120402     C                   exsr      wrivat                                       => scarico VAT
070200120402     C*
070300120402     C                   EVAL      VATTRC='J'
070400120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
070500120402     C                   exsr      wrivat                                       => scarico VAT
070600120402     C*
070700120402     C                   ENDSR
070800111031     C*----------------------------------------------------*
070900111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
071000111031     C*----------------------------------------------------*
071100111031     C     WRIVAT        BEGSR
071200111031     C*
071300111031     C* Scrivo solo se valorizzato qualcosa
071400111031     C                   IF        VATNOT <> *blanks
071500111031     C                   WRITE     FIVAT000
071600111031     C                   ENDIF
071700111031     C*
071800111031     C                   ENDSR
071900020904
072000020904
072100010202     C*----------------------------------------------------*
072200100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
072300010202     C*----------------------------------------------------*
072400100525     C     PREVASX       BEGSR
072500100525     C*
072600021113     C* Compongo il nome del membro da dare al FIVATWWR
072700010202     C                   eval      parmbr = vlrhdl
072800010202     C                   movel     'M'           parmbr
072900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
073000010202     C                   eval      paropz = '1'
073100100525     C*
073200010202     C* Effettuo la chiamata al CLLE preposto
073300100525     C  N51              call(e)   'TITVVTC'
073400010202     C                   parm                    parccm
073500010202     C                   parm                    parmbr
073600010202     C                   parm                    paropz
073700100525     C   51              call(e)   'TITVVBC'
073800100525     C                   parm                    parccm
073900100525     C                   parm                    parmbr
074000100525     C                   parm                    paropz
074100100525     C*
074200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
074300010202     C                   if        %error
074400010202     C                   movel     '1'           chkcall
074500010202     C                   else
074600010202     C                   movel     '0'           chkcall
074700010202     C                   endif
074800010202     C*
074900010202     C                   ENDSR
075000000801     C*----------------------------------------------------*
075100000801     C*  CONTROLLO NUMERICITA' CAMPI
075200000801     C*----------------------------------------------------*
075300000801     C     CHKNUM        BEGSR
075400081113     C*
075500081113     C                   IF        PiDecChr = *blanks
075600110408     C                   EVAL      PiDecChr = '.'
075700081113     C                   ENDIF
075800091223     C*
075900081113     C                   callp(e)  UBISNUM_Check(PiStr
076000081113     C                                          :PiDecChr
076100081113     C                                          :PiVal
076200081113     C                                          :PiNum
076300081113     C                                          :PiInt)
076400081113     C*
076500000801     C                   IF        %error
076600000801     C                   EVAL      PiInt=*off
076700000801     C                   ENDIF
076800000801     C*
076900000801     C                   ENDSR
077000000801     C***
077100011113
077200011113
077300000801
077400000801
077500990920      /TITLE Invio dei dati al punto operativo.
077600010202     C     invio         BEGSR
077700990920     C*
077800021113     C* 1° invio FIVAT
077900010201     C                   reset                   dscmz
078000110411     C                   move      vatfgs        cmzdst
078100021113     C                   eval      cmzfld = 'FIVATWWR'
078200010201     C                   eval      cmzmbd = vlrhdl
078300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078400021009     C***                if        prmfir = *blanks
078500021113     C                   eval      cmzfla = 'FIVAT00F'
078600021113     C                   eval      cmzmba = 'FIVAT00F'
078700021009     C***                else
078800021009     C***                eval      cmzfla = prmfir
078900021009     C***                eval      cmzmba = prmfir
079000021009     C***                endif
079100010201     C                   eval      cmznrr = *zeros
079200020305     C                   move      §ctrokvt      cmznrr
079300021018     C                   eval      cmzlba = vlrfl1
079400010201     C                   call(e)   'TIS711C'
079500010201     C                   parm                    dscmz
079600010201     C                   parm      *blanks       esito
079700010205     C                   if        %error
079800010205     C                             or cmzerr = '1'
079900010205     C                             or esito  = '1'
080000010205     C                   eval      wrkesito = '3'
080100010205     C                   else
080200010201     C*
080300021113     C* 2° invio FIVAB
080400100525     C                   if        *in51 = *off
080500010201     C                   reset                   dscmz
080600110411     C                   move      vabfgs        cmzdst
080700010201     C                   eval      cmzfld = vlrfou
080800010201     C                   eval      cmzmbd = vlrhdl
080900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081000021009     C***                if        prmfir = *blanks
081100021113     C                   eval      cmzfla = 'FIVAB00F'
081200021113     C                   eval      cmzmba = 'FIVAB00F'
081300021009     C***                else
081400021009     C***                eval      cmzfla = prmfir
081500021009     C***                eval      cmzmba = prmfir
081600021009     C***                endif
081700010201     C                   eval      cmznrr = *zeros
081800010201     C                   move      §ctrokvb      cmznrr
081900021018     C                   eval      cmzlba = vlrfl1
082000010201     C                   call(e)   'TIS711C'
082100010201     C                   parm                    dscmz
082200010201     C                   parm      *blanks       esito
082300010201     C                   if        %error
082400010201     C                             or cmzerr = '1'
082500010201     C                             or esito  = '1'
082600010201     C                   eval      wrkesito = '3'
082700010201     C                   endif
082800100525     C                   endif
082900010205     C                   endif
083000990920     C*
083100000613     C                   ENDSR
083200000613     C***
083300070411
083400110627     C     *pssr         BEGSR
083500070411     C*
083600110627     C                   if        %open(tivin00r)
083700110627     C                   close     tivin00r
083800110627     C                   endif
083900110627     C                   if        %open(fivabwwr)
084000110627     C                   close     fivabwwr
084100110627     C                   endif
084200110627     C                   if        %open(fivatwwr)
084300110627     C                   close     fivatwwr
084400110627     C                   endif
084500110627     C*
084600110627     C* Effettuo la chiamata al CLLE preposto
084700110627     C  N51              call(e)   'TITVVTC'
084800110627     C                   parm                    parccm
084900110627     C                   parm                    parmbr
085000110627     C                   parm      '2'           paropz
085100110627     C   51              call(e)   'TITVVBC'
085200110627     C                   parm                    parccm
085300110627     C                   parm                    parmbr
085400110627     C                   parm      '2'           paropz
085500110627     C*
085600110627     C                   eval      wrkesito = '2'
085700110627     C*
085800110627     C                   seton                                        LR
085900110627     C*
086000110627     C                   ENDSR     '*CANCL'
086100070411     C***
086200070411
086300990910
086400000613     C     *inzsr        BEGSR
086500990910     C*
086600990910     C     *entry        plist
086700990920     C                   parm                    tivlrds
086800990921     C                   parm      wrkesito      esito
086900000724     C                   parm                    prmlit
087000000710     C                   parm                    prmfir
087100000613     C*
087200000830     C* CALCOLA LA DATA CORRENTE
087300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087400091223     C                   eval      datcor = %dec(%date() : *ISO)
087500000830     C*
087600000613     C                   ENDSR
087700000613     C***
