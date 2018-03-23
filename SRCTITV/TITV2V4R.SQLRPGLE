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
002100000621     D recko           s            150    INZ(*blanks)
002200011113     D depcmd          s            150    INZ(*blanks)
002300160822     D depspe          s             10    INZ(*blanks)
002400160822     D curspe          s             10    INZ(*blanks)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000091223     D Num5_0          s              5  0
003100020725
003200020725     D*------------------
003300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003400020725     D*------------------
003500070530     D INPUT_DS        DS                  INZ
003600100729     D  VINDTA                     2048
003700100729     D  VINFLG                        1
003800160822     D  VINSPE                       10
003900100729     D  VINRRN                        8  0
004000020725     D*
004100081113
004200091223     D*------------------
004300091223     D* DS REPERIMENTO NUMERATORE
004400091223     D*------------------
004500100309     D trul33ds      e ds                  inz
004600091223     D*------------------
004700091223     D* DS ARCHITETTURA
004800091223     D*------------------
004900091223     D kpjba         e ds                  inz
005000091223
005100101119
005200101119     D*-------------------
005300101119     D* COSTANTI
005400101119     D*-------------------
005500101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005600101119     D                                     èéù')
005700101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005800101119     D                                     EEU')
005900081113
006000081113     D*------------------
006100081113     D* LINKING A DEFINIZIONI ESTERNE
006200081113     D*------------------
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006500081113
006600990908
006700010201
006800010201
006900080117     C*
007000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100080117     C
007200080117     C/EXEC SQL
007300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400080117     C/END-EXEC
007500080117     C
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000613     C*
008000100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400100525     C  N51              call(e)   'TITVVTC'
008500100525     C                   parm                    parccm
008600100525     C                   parm                    parmbr
008700100525     C                   parm      '2'           paropz
008800100525     C   51              call(e)   'TITVVBC'
008900100525     C                   parm                    parccm
009000100525     C                   parm                    parmbr
009100100525     C                   parm      '2'           paropz
009200070730     C*
009300070730     C* Effettuo lancio TISI95 solo x chiusura
009400070730     C                   CLEAR                   TISI95DS
009500070730     C                   EVAL      I95TLA = 'C'
009600070730     C                   CALL      'TISI95R'
009700070730     C                   PARM                    TISI95DS
009800000616     C*
009900000801     C
010000010201     C                   seton                                        LR
010100990908
010200000801
010300910830     C*--------------------------------------------------------
010400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010500910830     C*--------------------------------------------------------
010600070530     C     RWFILE        BEGSR
010700990910     C*
010800990914     C                   if        not %open(tivin00r)
010900990908     C                   open      tivin00r
011000990914     C                   endif
011100070530     C*
011200100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011300100525     C                   exsr      prevasx
011400010201     C*
011500010202     C                   if        chkcall = '0'
011600010202     C*
011700100525     C                   if        not %open(fivabwwr)
011800100525     C                   open      fivabwwr
011900100525     C                   endif
012000100525     C*
012100021113     C                   if        not %open(fivatwwr)
012200021113     C                   open      fivatwwr
012300010201     C                   endif
012400080117     C*
012500080117     C                   EXSR      INZVAR
012600080117     C                   EXSR      DEFCAM
012700990910     C*
012800010201     C                   clear                   §CTROKVB          5 0
012900020305     C                   clear                   §CTROKVT          5 0
013000000801     C                   clear                   §CTRMO            5 0
013100000801     C                   clear                   §CTRNO            5 0
013200990910     C*
013300020725     C/EXEC SQL
013400020725     C+ declare C1 cursor for select
013500160822     C+ vindta, vinflg, substr(vindta, 46, 10) as sped, rrn(tivin00r)
013600060223     C+ from tivin00r
013700111020     C+ order by sped
013800020725     C+ for read only
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200020725     C+ open C1
014300020725     C/END-EXEC
014400020725     C
014500020725     C/EXEC SQL
014600070530     C+ Fetch C1 into :INPUT_DS
014700020725     C/END-EXEC
014800020725     C*
014900020725     C                   dow       sqlcod = *zeros
015000990913     C*
015100100310     C                   if        vindta > *blanks
015200000613     C                   add       1             rrnum
015300000801     C*
015400020725     C                   if        vinflg = *blanks
015500020725     C                             or vinflg = '0'
015600020725     C                             or vinflg = '2'
015700000801     C*
015800020725     C                   clear                   x_vinmsg
015900020725     C                   eval      x_vinflg = '1'
016000101119     C*
016100101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016200101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016300010305     C*
016400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016500160822     C                   EVAL      curspe=%trim(%subst(vindta:46:10))
016600010305     C*
016700100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016800080923     C                   if        *in50 = *off
016900101119     C                   exsr      inzvar
017000101119     C                   exsr      defcam
017100071003     C                   exsr      impvab
017200100525     C                   exsr      wrivab
017300111031     C                   exsr      exevata                                      => carico VAT
017400111031     C                   exsr      exevatb                                      => carico VAT
017500111031     C   51              exsr      exevate                                      => carico VAT
017600071003     C                   else
017700080923     C*
017800071009     C                   if        wDISK = 'D'
017900091223     C                   exsr      repNSP
018000071009     C                   exsr      impvab
018100100412     C                   exsr      wrivab
018200111031     C                   exsr      exevata                                      => carico VAT
018300111031     C                   exsr      exevatb                                      => carico VAT
018400111031     C                   exsr      exevate                                      => carico VAT
018500071009     C                   else
018600071009     C*
018700010305     C                   if        depspe = *blanks                             => 1° giro
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900080117     C                   clear                   tivinds
019000091223     C                   exsr      repNSP
019100020305     C                   exsr      impvab
019200111031     C                   exsr      exevata                                      => carico VAT
019300111031     C                   exsr      exevatb                                      => carico VAT
019400111031     C   50              exsr      exevate                                      => carico VAT
019500010305     C                   else
019600020725     C                   if        depspe <> curspe                             => rottura di spediz
019700010305     C                   eval      depspe = curspe                              => memorizz. spediz
019800100412     C                   exsr      wrivab
019900080117     C                   clear                   tivinds
020000091223     C                   exsr      repNSP
020100020305     C                   exsr      impvab
020200111031     C                   exsr      exevata                                      => carico VAT
020300111031     C                   exsr      exevatb                                      => carico VAT
020400111031     C   50              exsr      exevate                                      => carico VAT
020500020305     C                   else                                                   => x stessa spediz
020600100401     C*
020700100401     C* Se nn richiesta esclusione spedizioni "duplicate"
020800100928     C                   select
020900100928     C                   when      wDUPREC = ' '
021000020305     C                   exsr      impvab
021100111031     C                   exsr      exevata                                      => carico VAT
021200111031     C                   exsr      exevatb                                      => carico VAT
021300111031     C   50              exsr      exevate                                      => carico VAT
021400100928     C                   when      wDUPREC = 'C'
021500111031     C   50              exsr      exevate                                      => carico VAT
021600100928     C                   endsl
021700010305     C                   endif
021800010305     C                   endif
021900010305     C                   endif
022000071003     C                   endif
022100110201     C*
022200071009     C                   endif
022300000905     C*
022400000905     C                   else
022500020725     C                   eval      x_vinflg = '1'
022600000905     C                   endif
022700000905     C*
022800020725     C     VINRRN        chain     tivin000
022900020725     C                   if        %found(tivin00r)
023000020725     C                   eval      y_vinflg = x_vinflg
023100020725     C                   eval      y_vinmsg = x_vinmsg
023200020725     C                   update    tivin000
023300020725     C                   endif
023400020725     C*
023500020725     C/EXEC SQL
023600070530     C+ Fetch C1 into :INPUT_DS
023700020725     C/END-EXEC
023800020725     C*
023900020725     C                   enddo
024000020725     C*
024100020725     C/EXEC SQL
024200020725     C+ close C1
024300020725     C/END-EXEC
024400000905     C*
024500020305     C* Scarico i VAB rimasti "in sospeso"
024600071009     C                   if        wDISK = 'C'
024700100412     C                   exsr      wrivab
024800071009     C                   endif
024900010202     C*
025000010202     C                   endif
025100990910
025200990910     C* Se non ci sono record con errori ...
025300000710     C                   if        §ctrno = 0
025400990910     C* ... restituisco esito OK.
025500990921     C                   eval      wrkesito = '0'
025600990910     C                   else
025700100525     C                   if        §ctrokvb > 0 OR
025800100525     C                             §ctrokvt > 0
025900990921     C                   eval      wrkesito = '1'
026000000710     C                   else
026100000710     C                   eval      wrkesito = '2'
026200990910     C                   endif
026300000710     C                   endif
026400990910     C*
026500990914     C                   if        %open(tivin00r)
026600990908     C                   close     tivin00r
026700990914     C                   endif
026800021113     C                   if        %open(fivabwwr)
026900021113     C                   close     fivabwwr
027000990914     C                   endif
027100021113     C                   if        %open(fivatwwr)
027200021113     C                   close     fivatwwr
027300010201     C                   endif
027400990910     C*
027500100525     C                   if        §ctrokvb > 0 OR
027600100525     C                             §ctrokvt > 0
027700000724     C                             and vlrpoi <> *zeros
027800010202     C                   exsr      invio
027900990920     C                   endif
028000990920     C*
028100910830     C                   ENDSR
028200000613     C***
028300100412
028400100412
028500010305
028600010305     C*----------------------------------------------------*
028700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028800010305     C*----------------------------------------------------*
028900020305     C     WRIVAB        BEGSR
029000100412     C*
029100100412     C* Gestisco l'eventuale rottura x numero spedizione
029200100412     C                   if        VABRMA = *blanks
029300100412     C                   movel(P)  VABRMN        VABRMA
029400100412     C                   endif
029500100412     C*
029600100525     C                   if        *in51 = *off and *in31 = *off
029700100525     C                   write     fivab000                                     => scarico il VAB
029800100525     C                   endif
029900010305     C*
030000010305     C                   ENDSR
030100990920
030200000801     C*----------------------------------------------------*
030300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030400000801     C*----------------------------------------------------*
030500010201     C     INZVAR        BEGSR
030600000801     C*
030700010201     C                   Z-ADD     *zeros        Num5_0
030800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030900020725     C                   MOVEL     '0'           FlgCAS            1
031000000801     C*
031100000801     C                   ENDSR
031200000801     C*----------------------------------------------------*
031300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031400000801     C*----------------------------------------------------*
031500020904     C     DEFCAM        BEGSR
031600080923     C*
031700100928     C                   SETOFF                                       505160
031800000801     C*
031900021113     C                   CLEAR                   FIVAB000
032000021113     C                   CLEAR                   FIVAT000
032100070730     C* Imposto i valori di default...
032200160803     C                   EVAL      VABCCM = 2230486
032300160803     C                   EVAL      VATCCM = 2230486
032400160803     C                   EVAL      VABLNP = 223
032500160803     C                   EVAL      VATLNP = 223
032600160803     C                   EVAL      VABCTR = 300
032700100525     C                   EVAL      VABCBO = '1'
032800070222     C* ... e poi verifico se sono stati passati come parametri
032900070222     C                   IF        vlrppt > *blanks
033000100525     C*
033100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033300070222     C                   EXSR      CHKNUM
033400070222     C                   IF        PiInt=*on
033500070222     C                   Z-ADD     PiVal         VABCCM
033600070222     C                   Z-ADD     PiVal         VATCCM
033700070222     C                   ENDIF
033800070222     C                   ENDIF
033900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034100070222     C                   EXSR      CHKNUM
034200070222     C                   IF        PiInt=*on
034300070222     C                   Z-ADD     PiVal         VABLNP
034400070222     C                   Z-ADD     PiVal         VATLNP
034500070222     C                   ENDIF
034600070222     C                   ENDIF
034700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABCTR
035200070222     C                   ENDIF
035300070928     C                   ENDIF
035400071009     C                   MOVEL     *blanks       wDISK             1
035500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035600071009     C                   IF        wDISK <> *blanks
035700070928     C                   SETON                                        50
035800070222     C                   ENDIF
035900100525     C                   IF        wDISK  = 'T'
036000100525     C                   SETOFF                                       50
036100100525     C                   SETON                                        51
036200100525     C                   ENDIF
036300100401     C                   MOVEL     *blanks       wDUPREC           1
036400100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036500100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036600100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036700100525     C                   ENDIF
036800111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
036900111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
037000100525     C*
037100070222     C                   ENDIF
037200071009     C*
037300071009     C   50              EVAL      VABCTM = '7Q'
037400000801     C*
037500000801     C                   ENDSR
037600091223     C*----------------------------------------------------*
037700091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
037800091223     C*----------------------------------------------------*
037900091223     C     REPNSP        BEGSR
038000091223     C*
038100091223     C                   EXSR      INZVAR
038200091223     C                   EXSR      DEFCAM
038300100928     C*
038400100928     C                   SETON                                        60
038500091223     C*
038600091223     C* NSP => Stacco un numeratore da AZNUM
038700091223     C                   clear                   TRUL33DS
038800091223     C                   eval      I33OPE = *zeros
038900091223     C                   eval      I33CNU = 302
039000091223     C                   eval      I33NUM = 1
039100091223     C                   movel     TRUL33DS      KPJBU
039200091223     C                   call      'TRUL33R'
039300091223     C                   parm                    KPJBA
039400091223     C                   movel     KPJBU         TRUL33DS
039500091223     C                   if        O33ERR = *zeros
039600091223     C                   z-add     O33NRF        VABNSP
039700091223     C                   z-add     O33NRF        VATNSP
039800091223     C                   else
039900091223     C                   SETON                                        31
040000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100091223     C                             + ' ' + 'VABNSP VATNSP'
040200091223     C                   endif
040300091223     C*
040400091223     C                   ENDSR
040500111031     C*----------------------------------------------------*
040600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040700111031     C*----------------------------------------------------*
040800111031     C     IMPVAB        BEGSR
040900111031     C*
041000111031     C                   Z-ADD     *zeros        errore            1 0
041100111031     C                   MOVEL     datcor        VABAAS
041200111031     C                   MOVEL     datcor        VATAAS
041300111031     C                   MOVE      datcor        VABMGS
041400111031     C                   MOVE(P)   vlrpoi        VABFGS
041500111031     C                   MOVE(P)   vlrpoi        VATFGS
041600111031     C*
041700111031     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
041800111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041900111031     C     '@':'A'       XLATE     VABRSD        VABRSD
042000111031     C* ==
042100111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
042200111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
042300111031     C                                    %trim(%subst(vindta:341:30))+' '+
042400111031     C                                    %trim(%subst(vindta:371:5)))
042500111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
042600160803     C***                EVAL      VABPRD=%trim(%subst(vindta:430:2))
042700160822     C                   EVAL      VABRMA=%trim(%subst(vindta:46:10))
042800160822     C***                EVAL      VABRMA=%trim(%subst(vindta:46:25))
042900111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
043000111031     C                   IF        %subst(vindta:43:1) = 'N'
043100111031     C                   EVAL      VABCBO='1'
043200111031     C                   ENDIF
043300111031     C                   IF        %subst(vindta:43:1) = 'P'
043400111031     C                   EVAL      VABCBO='2'
043500111031     C                   ENDIF
043600111031     C                   IF        %subst(vindta:511:3) = 'ABM'
043700111031     C                   EVAL      VABTIC='BM'
043800111031     C                   ENDIF
043900111031     C                   IF        %subst(vindta:511:3) = 'ABS'
044000111031     C                   EVAL      VABTIC='B'
044100111031     C                   ENDIF
044200111031     C                   IF        %subst(vindta:511:3) = 'CON'
044300111031     C                   EVAL      VABTIC=*blanks
044400111031     C                   ENDIF
044500111031     C                   IF        %subst(vindta:511:3) = 'ACM'
044600111031     C                   EVAL      VABTIC='CM'
044700111031     C                   ENDIF
044800111031     C                   IF        %subst(vindta:511:3) = 'ACS'
044900111031     C                   EVAL      VABTIC='CB'
045000111031     C                   ENDIF
045100111031     C                   IF        %subst(vindta:511:3) = 'VAR'
045200111031     C                   EVAL      VABTIC=*blanks
045300111031     C                   ENDIF
045400111031     C* CAP
045500111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
045600111031     C                   EXSR      CHKNUM
045700111031     C                   IF        PiInt=*on
045800111031     C                   Z-ADD     PiVal         Num5_0
045900111031     C                   MOVEL(P)  Num5_0        VABCAD
046000111031     C                   ELSE
046100111031     C                   ADD       1             errore
046200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300111031     C                             + ' ' + 'VABCAD'
046400111031     C                   ENDIF
046500111031     C* Reperisco la provincia dal CAP e dalla località
046600111031     C                   IF        VABCAD <> *blanks AND
046700111031     C                             VABPRD  = *blanks
046800111031     C                   CLEAR                   TISI95DS
046900111031     C                   EVAL      I95TCN = '3'
047000111031     C                   Z-ADD     datcor        I95DAT
047100111031     C                   EVAL      I95CAP = VABCAD
047200111031     C                   EVAL      I95LOC = VABLOD
047300111031     C                   EVAL      I95NAR = VABNZD
047400111031     C                   CALL      'TISI95R'
047500111031     C                   PARM                    TISI95DS
047600111031     C                   EVAL      VABPRD = O95PRV
047700111031     C                   ENDIF
047800111031     C* NCL
047900111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
048000111031     C                   EXSR      CHKNUM
048100111031     C                   IF        PiInt=*on
048200111031     C                   SELECT
048300111031     C                   WHEN      wNCL = *blanks
048400111031     C                   Z-ADD     PiVal         VABNCL
048500111031     C                   WHEN      wNCL = 'A'
048600111031     C                   ADD       PiVal         VABNCL
048700111031     C                   WHEN      wNCL = '1'
048800111031     C                   ADD       1             VABNCL
048900111031     C                   ENDSL
049000111031     C                   ELSE
049100111031     C                   ADD       1             errore
049200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300111031     C                             + ' ' + 'VABNCL'
049400111031     C                   ENDIF
049500111031     C* PKB
049600111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
049700111031     C                   EXSR      CHKNUM
049800111031     C                   IF        PiNum=*on
049900160803     C                   EVAL      PiVal=PiVal/1000
050000160803     C                   ADD       PiVal         VABPKB
050100111031     C                   ELSE
050200111031     C                   ADD       1             errore
050300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400111031     C                             + ' ' + 'VABPKB'
050500111031     C                   ENDIF
050600111031     C* RMN
050700160822     C                   EVAL      PiStr=%trim(%subst(vindta:46:10))
050800160822     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
050900160803     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
051000160803     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
051100111031     C                   EXSR      CHKNUM
051200111031     C                   IF        PiInt=*on
051300111031     C                   Z-ADD     PiVal         VABRMN
051400111031     C                   ELSE
051500111031     C                   ADD       1             errore
051600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051700111031     C                             + ' ' + 'VABRMN'
051800111031     C                   ENDIF
051900111031     C* CAS
052000111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
052100111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
052200111031     C                             %trim(%subst(vindta:502:9))<>*zeros
052300111031     C                   EVAL      VABVCA = 'EUR'
052400111031     C                   EVAL      FlgCAS = '1'
052500111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
052600111031     C                   EXSR      CHKNUM
052700111031     C                   IF        PiNum=*on
052800111031     C                   Z-ADD     PiVal         VABCAS
052900111031     C                   ELSE
053000111031     C                   ADD       1             errore
053100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053200111031     C                             + ' ' + 'VABCAS'
053300111031     C                   ENDIF
053400111031     C                   ENDIF
053500111031     C* IAS
053600111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
053700111031     C                   EVAL      VABVAS = 'EUR'
053800111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
053900111031     C                   EXSR      CHKNUM
054000111031     C                   IF        PiNum=*on
054100111031     C                   Z-ADD     PiVal         VABIAS
054200111031     C                   ELSE
054300111031     C                   ADD       1             errore
054400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500111031     C                             + ' ' + 'VABIAS'
054600111031     C                   ENDIF
054700111031     C                   ENDIF
054800111031     C*
054900111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
055000111031     C                   IF        FlgCAS <> '0'
055100111031     C                   IF        VABCBO = '1'
055200111031     C                   EVAL      VABCBO = '4'
055300111031     C                   ELSE
055400111031     C                   EVAL      VABCBO = '6'
055500111031     C                   ENDIF
055600111031     C                   ENDIF
055700111031     C*
055800111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055900111031     C                   EXSR      CHKIMPDIV
056000111031     C*
056100111031     C* Ebbene...
056200111031     C                   ADD       1             §CTRMO
056300111031     C                   IF        errore <> *zeros
056400111031     C                   ADD       1             §CTRNO
056500111031     C                   EVAL      recko = vindta
056600111031     C                   EVAL      vinflg = '2'
056700111031     C                   ELSE
056800111031     C                   ADD       1             §CTROKVB
056900111031     C                   ENDIF
057000111031     C*
057100111031     C                   ENDSR
057200111031
057300111031
057400111031     C*----------------------------------------------------*
057500111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057600111031     C*----------------------------------------------------*
057700111031     C     CHKIMPDIV     BEGSR
057800111031     C*
057900111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058000111031     C                   Z-ADD     *zeros        wrkDec            9 9
058100111031     C*
058200111031     C* Come prima cosa effettuo considerazioni sulla divisa
058300111031     C                   IF        vabIAS > *zeros
058400111031     C                   IF        vabVAS <> 'EUR'
058500111031     C                   EVAL      vabVAS =  'ITL'
058600111031     C                   ENDIF
058700111031     C                   ENDIF
058800111031     C*
058900111031     C                   IF        vabCAS > *zeros
059000111031     C                   IF        vabVCA <> 'EUR'
059100111031     C                   EVAL      vabVCA =  'ITL'
059200111031     C                   ENDIF
059300111031     C                   ENDIF
059400111031     C*
059500111031     C                   IF        vabVMD > *zeros
059600111031     C                   IF        vabVAD <> 'EUR'
059700111031     C                   EVAL      vabVAD =  'ITL'
059800111031     C                   ENDIF
059900111031     C                   ENDIF
060000111031     C*
060100111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060200111031     C                   Z-ADD     vabIAS        wrkDec
060300111031     C                   IF        wrkDec > *zeros
060400111031     C                   IF        vabVAS = 'ITL'
060500111031     C                   EVAL      vabIAS = *zeros
060600111031     C                   ENDIF
060700111031     C                   ENDIF
060800111031     C*
060900111031     C* Stabilisco se il contrasegno ha decimali valorizzati
061000111031     C                   Z-ADD     vabCAS        wrkDec
061100111031     C                   IF        wrkDec > *zeros
061200111031     C                   IF        vabVCA = 'ITL'
061300111031     C                   EVAL      vabCAS = *zeros
061400111031     C                   ENDIF
061500111031     C                   ENDIF
061600111031     C*
061700111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061800111031     C                   Z-ADD     vabVMD        wrkDec
061900111031     C                   IF        wrkDec > *zeros
062000111031     C                   IF        vabVAD = 'ITL'
062100111031     C                   EVAL      vabVMD = *zeros
062200111031     C                   ENDIF
062300111031     C                   ENDIF
062400111031     C*
062500111031     C                   ENDSR
062600111031     C***
062700111031
062800111031
062900111031     C*----------------------------------------------------*
063000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
063100111031     C*----------------------------------------------------*
063200111031     C     EXEVATA       BEGSR
063300111031     C*
063400111031     C***                EVAL      VATTRC='A'
063500111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
063600111031     C***                exsr      wrivat                                       => scarico VAT
063700111031     C*
063800111031     C                   ENDSR
063900111031     C*----------------------------------------------------*
064000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
064100111031     C*----------------------------------------------------*
064200111031     C     EXEVATB       BEGSR
064300111031     C*
064400111031     C***                EVAL      VATTRC='B'
064500111031     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
064600111031     C***                exsr      wrivat                                       => scarico VAT
064700111031     C*
064800111031     C                   ENDSR
064900111031     C*----------------------------------------------------*
065000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
065100111031     C*----------------------------------------------------*
065200111031     C     EXEVATE       BEGSR
065300111031     C*
065400111031     C                   EVAL      VATTRC='E'
065500111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
065600111031     C                   exsr      wrivat                                       => scarico VAT
065700111031     C*
065800111031     C                   ENDSR
065900111031     C*----------------------------------------------------*
066000111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
066100111031     C*----------------------------------------------------*
066200111031     C     WRIVAT        BEGSR
066300111031     C*
066400111031     C* Scrivo solo se valorizzato qualcosa
066500111031     C                   IF        VATNOT <> *blanks
066600111031     C                   WRITE     FIVAT000
066700111031     C                   ENDIF
066800111031     C*
066900111031     C                   ENDSR
067000020904
067100020904
067200010202     C*----------------------------------------------------*
067300100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
067400010202     C*----------------------------------------------------*
067500100525     C     PREVASX       BEGSR
067600100525     C*
067700021113     C* Compongo il nome del membro da dare al FIVATWWR
067800010202     C                   eval      parmbr = vlrhdl
067900010202     C                   movel     'M'           parmbr
068000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068100010202     C                   eval      paropz = '1'
068200100525     C*
068300010202     C* Effettuo la chiamata al CLLE preposto
068400100525     C  N51              call(e)   'TITVVTC'
068500010202     C                   parm                    parccm
068600010202     C                   parm                    parmbr
068700010202     C                   parm                    paropz
068800100525     C   51              call(e)   'TITVVBC'
068900100525     C                   parm                    parccm
069000100525     C                   parm                    parmbr
069100100525     C                   parm                    paropz
069200100525     C*
069300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069400010202     C                   if        %error
069500010202     C                   movel     '1'           chkcall
069600010202     C                   else
069700010202     C                   movel     '0'           chkcall
069800010202     C                   endif
069900010202     C*
070000010202     C                   ENDSR
070100000801     C*----------------------------------------------------*
070200000801     C*  CONTROLLO NUMERICITA' CAMPI
070300000801     C*----------------------------------------------------*
070400000801     C     CHKNUM        BEGSR
070500081113     C*
070600081113     C                   IF        PiDecChr = *blanks
070700110408     C                   EVAL      PiDecChr = '.'
070800081113     C                   ENDIF
070900091223     C*
071000081113     C                   callp(e)  UBISNUM_Check(PiStr
071100081113     C                                          :PiDecChr
071200081113     C                                          :PiVal
071300081113     C                                          :PiNum
071400081113     C                                          :PiInt)
071500081113     C*
071600000801     C                   IF        %error
071700000801     C                   EVAL      PiInt=*off
071800000801     C                   ENDIF
071900000801     C*
072000000801     C                   ENDSR
072100000801     C***
072200011113
072300011113
072400000801
072500000801
072600990920      /TITLE Invio dei dati al punto operativo.
072700010202     C     invio         BEGSR
072800990920     C*
072900021113     C* 1° invio FIVAT
073000010201     C                   reset                   dscmz
073100110411     C                   move      vatfgs        cmzdst
073200021113     C                   eval      cmzfld = 'FIVATWWR'
073300010201     C                   eval      cmzmbd = vlrhdl
073400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073500021009     C***                if        prmfir = *blanks
073600021113     C                   eval      cmzfla = 'FIVAT00F'
073700021113     C                   eval      cmzmba = 'FIVAT00F'
073800021009     C***                else
073900021009     C***                eval      cmzfla = prmfir
074000021009     C***                eval      cmzmba = prmfir
074100021009     C***                endif
074200010201     C                   eval      cmznrr = *zeros
074300020305     C                   move      §ctrokvt      cmznrr
074400021018     C                   eval      cmzlba = vlrfl1
074500010201     C                   call(e)   'TIS711C'
074600010201     C                   parm                    dscmz
074700010201     C                   parm      *blanks       esito
074800010205     C                   if        %error
074900010205     C                             or cmzerr = '1'
075000010205     C                             or esito  = '1'
075100010205     C                   eval      wrkesito = '3'
075200010205     C                   else
075300010201     C*
075400021113     C* 2° invio FIVAB
075500100525     C                   if        *in51 = *off
075600010201     C                   reset                   dscmz
075700110411     C                   move      vabfgs        cmzdst
075800010201     C                   eval      cmzfld = vlrfou
075900010201     C                   eval      cmzmbd = vlrhdl
076000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076100021009     C***                if        prmfir = *blanks
076200021113     C                   eval      cmzfla = 'FIVAB00F'
076300021113     C                   eval      cmzmba = 'FIVAB00F'
076400021009     C***                else
076500021009     C***                eval      cmzfla = prmfir
076600021009     C***                eval      cmzmba = prmfir
076700021009     C***                endif
076800010201     C                   eval      cmznrr = *zeros
076900010201     C                   move      §ctrokvb      cmznrr
077000021018     C                   eval      cmzlba = vlrfl1
077100010201     C                   call(e)   'TIS711C'
077200010201     C                   parm                    dscmz
077300010201     C                   parm      *blanks       esito
077400010201     C                   if        %error
077500010201     C                             or cmzerr = '1'
077600010201     C                             or esito  = '1'
077700010201     C                   eval      wrkesito = '3'
077800010201     C                   endif
077900100525     C                   endif
078000010205     C                   endif
078100990920     C*
078200000613     C                   ENDSR
078300000613     C***
078400070411
078500110627     C     *pssr         BEGSR
078600070411     C*
078700110627     C                   if        %open(tivin00r)
078800110627     C                   close     tivin00r
078900110627     C                   endif
079000110627     C                   if        %open(fivabwwr)
079100110627     C                   close     fivabwwr
079200110627     C                   endif
079300110627     C                   if        %open(fivatwwr)
079400110627     C                   close     fivatwwr
079500110627     C                   endif
079600110627     C*
079700110627     C* Effettuo la chiamata al CLLE preposto
079800110627     C  N51              call(e)   'TITVVTC'
079900110627     C                   parm                    parccm
080000110627     C                   parm                    parmbr
080100110627     C                   parm      '2'           paropz
080200110627     C   51              call(e)   'TITVVBC'
080300110627     C                   parm                    parccm
080400110627     C                   parm                    parmbr
080500110627     C                   parm      '2'           paropz
080600110627     C*
080700110627     C                   eval      wrkesito = '2'
080800110627     C*
080900110627     C                   seton                                        LR
081000110627     C*
081100110627     C                   ENDSR     '*CANCL'
081200070411     C***
081300070411
081400990910
081500000613     C     *inzsr        BEGSR
081600990910     C*
081700990910     C     *entry        plist
081800990920     C                   parm                    tivlrds
081900990921     C                   parm      wrkesito      esito
082000000724     C                   parm                    prmlit
082100000710     C                   parm                    prmfir
082200000613     C*
082300000830     C* CALCOLA LA DATA CORRENTE
082400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082500091223     C                   eval      datcor = %dec(%date() : *ISO)
082600000830     C*
082700000613     C                   ENDSR
082800000613     C***
