000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002300111031     D depspe          s             25    INZ(*blanks)
002400111031     D curspe          s             25    INZ(*blanks)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000091223     D Num5_0          s              5  0
003100141010     D w70             s             70
003200020725
003300020725     D*------------------
003400020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500020725     D*------------------
003600070530     D INPUT_DS        DS                  INZ
003700100729     D  VINDTA                     2048
003800100729     D  VINFLG                        1
003900111031     D  VINSPE                       25
004000100729     D  VINRRN                        8  0
004100020725     D*
004200081113
004300091223     D*------------------
004400091223     D* DS REPERIMENTO NUMERATORE
004500091223     D*------------------
004600100309     D trul33ds      e ds                  inz
004700091223     D*------------------
004800091223     D* DS ARCHITETTURA
004900091223     D*------------------
005000091223     D kpjba         e ds                  inz
005100091223
005200101119
005300101119     D*-------------------
005400101119     D* COSTANTI
005500101119     D*-------------------
005600101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005700101119     D                                     èéù')
005800101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005900101119     D                                     EEU')
006000081113
006100081113     D*------------------
006200081113     D* LINKING A DEFINIZIONI ESTERNE
006300081113     D*------------------
006400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006600081113
006700990908
006800010201
006900010201
007000080117     C*
007100080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007200080117     C
007300080117     C/EXEC SQL
007400080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007500080117     C/END-EXEC
007600080117     C
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000613     C*
008100100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008300000613     C*
008400010202     C* Effettuo la chiamata al CLLE preposto
008500100525     C  N51              call(e)   'TITVVTC'
008600100525     C                   parm                    parccm
008700100525     C                   parm                    parmbr
008800100525     C                   parm      '2'           paropz
008900100525     C   51              call(e)   'TITVVBC'
009000100525     C                   parm                    parccm
009100100525     C                   parm                    parmbr
009200100525     C                   parm      '2'           paropz
009300070730     C*
009400070730     C* Effettuo lancio TISI95 solo x chiusura
009500070730     C                   CLEAR                   TISI95DS
009600070730     C                   EVAL      I95TLA = 'C'
009700070730     C                   CALL      'TISI95R'
009800070730     C                   PARM                    TISI95DS
009900000616     C*
010000000801     C
010100010201     C                   seton                                        LR
010200990908
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
013600111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
013700060223     C+ from tivin00r
013800111020     C+ order by sped
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
016400010305     C*
016500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016600111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
016700010305     C*
016800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016900080923     C                   if        *in50 = *off
017000101119     C                   exsr      inzvar
017100101119     C                   exsr      defcam
017200071003     C                   exsr      impvab
017300100525     C                   exsr      wrivab
017400111031     C                   exsr      exevata                                      => carico VAT
017500111031     C                   exsr      exevatb                                      => carico VAT
017600111031     C   51              exsr      exevate                                      => carico VAT
017700120402     C                   exsr      exevati                                      => carico VAT
017800150309     C                   exsr      exevats                                      => carico VAT
017900071003     C                   else
018000080923     C*
018100071009     C                   if        wDISK = 'D'
018200091223     C                   exsr      repNSP
018300071009     C                   exsr      impvab
018400100412     C                   exsr      wrivab
018500111031     C                   exsr      exevata                                      => carico VAT
018600111031     C                   exsr      exevatb                                      => carico VAT
018700111031     C                   exsr      exevate                                      => carico VAT
018800120402     C                   exsr      exevati                                      => carico VAT
018900150309     C                   exsr      exevats                                      => carico VAT
019000071009     C                   else
019100071009     C*
019200010305     C                   if        depspe = *blanks                             => 1° giro
019300010305     C                   eval      depspe = curspe                              => memorizz. spediz
019400080117     C                   clear                   tivinds
019500091223     C                   exsr      repNSP
019600020305     C                   exsr      impvab
019700111031     C                   exsr      exevata                                      => carico VAT
019800111031     C                   exsr      exevatb                                      => carico VAT
019900111031     C   50              exsr      exevate                                      => carico VAT
020000120402     C                   exsr      exevati                                      => carico VAT
020100150309     C                   exsr      exevats                                      => carico VAT
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
021200120402     C                   exsr      exevati                                      => carico VAT
021300150309     C                   exsr      exevats                                      => carico VAT
021400020305     C                   else                                                   => x stessa spediz
021500100401     C*
021600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021700100928     C                   select
021800100928     C                   when      wDUPREC = ' '
021900020305     C                   exsr      impvab
022000111031     C                   exsr      exevata                                      => carico VAT
022100111031     C                   exsr      exevatb                                      => carico VAT
022200111031     C   50              exsr      exevate                                      => carico VAT
022300120402     C                   exsr      exevati                                      => carico VAT
022400150309     C                   exsr      exevats                                      => carico VAT
022500100928     C                   when      wDUPREC = 'C'
022600111031     C   50              exsr      exevate                                      => carico VAT
022700100928     C                   endsl
022800010305     C                   endif
022900010305     C                   endif
023000010305     C                   endif
023100071003     C                   endif
023200110201     C*
023300071009     C                   endif
023400000905     C*
023500000905     C                   else
023600020725     C                   eval      x_vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900020725     C     VINRRN        chain     tivin000
024000020725     C                   if        %found(tivin00r)
024100020725     C                   eval      y_vinflg = x_vinflg
024200020725     C                   eval      y_vinmsg = x_vinmsg
024300020725     C                   update    tivin000
024400020725     C                   endif
024500020725     C*
024600020725     C/EXEC SQL
024700070530     C+ Fetch C1 into :INPUT_DS
024800020725     C/END-EXEC
024900020725     C*
025000020725     C                   enddo
025100020725     C*
025200020725     C/EXEC SQL
025300020725     C+ close C1
025400020725     C/END-EXEC
025500000905     C*
025600020305     C* Scarico i VAB rimasti "in sospeso"
025700071009     C                   if        wDISK = 'C'
025800100412     C                   exsr      wrivab
025900071009     C                   endif
026000010202     C*
026100010202     C                   endif
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800100525     C                   if        §ctrokvb > 0 OR
026900100525     C                             §ctrokvt > 0
027000990921     C                   eval      wrkesito = '1'
027100000710     C                   else
027200000710     C                   eval      wrkesito = '2'
027300990910     C                   endif
027400000710     C                   endif
027500990910     C*
027600990914     C                   if        %open(tivin00r)
027700990908     C                   close     tivin00r
027800990914     C                   endif
027900021113     C                   if        %open(fivabwwr)
028000021113     C                   close     fivabwwr
028100990914     C                   endif
028200021113     C                   if        %open(fivatwwr)
028300021113     C                   close     fivatwwr
028400010201     C                   endif
028500990910     C*
028600100525     C                   if        §ctrokvb > 0 OR
028700100525     C                             §ctrokvt > 0
028800000724     C                             and vlrpoi <> *zeros
028900010202     C                   exsr      invio
029000990920     C                   endif
029100990920     C*
029200910830     C                   ENDSR
029300000613     C***
029400100412
029500100412
029600010305
029700010305     C*----------------------------------------------------*
029800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029900010305     C*----------------------------------------------------*
030000020305     C     WRIVAB        BEGSR
030100100412     C*
030200100412     C* Gestisco l'eventuale rottura x numero spedizione
030300100412     C                   if        VABRMA = *blanks
030400100412     C                   movel(P)  VABRMN        VABRMA
030500100412     C                   endif
030600100412     C*
030700100525     C                   if        *in51 = *off and *in31 = *off
030800100525     C                   write     fivab000                                     => scarico il VAB
030900100525     C                   endif
031000010305     C*
031100010305     C                   ENDSR
031200990920
031300000801     C*----------------------------------------------------*
031400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031500000801     C*----------------------------------------------------*
031600010201     C     INZVAR        BEGSR
031700000801     C*
031800010201     C                   Z-ADD     *zeros        Num5_0
031900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032000020725     C                   MOVEL     '0'           FlgCAS            1
032100000801     C*
032200000801     C                   ENDSR
032300000801     C*----------------------------------------------------*
032400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032500000801     C*----------------------------------------------------*
032600020904     C     DEFCAM        BEGSR
032700080923     C*
032800100928     C                   SETOFF                                       505160
032900000801     C*
033000021113     C                   CLEAR                   FIVAB000
033100021113     C                   CLEAR                   FIVAT000
033200070730     C* Imposto i valori di default...
033300120403     C                   EVAL      VABCCM = 0262681
033400120403     C                   EVAL      VATCCM = 0262681
033500120402     C                   EVAL      VABLNP = 026
033600120402     C                   EVAL      VATLNP = 026
033700120402     C                   EVAL      VABCTR = 300
033800100525     C                   EVAL      VABCBO = '1'
033900070222     C* ... e poi verifico se sono stati passati come parametri
034000070222     C                   IF        vlrppt > *blanks
034100100525     C*
034200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034400070222     C                   EXSR      CHKNUM
034500070222     C                   IF        PiInt=*on
034600070222     C                   Z-ADD     PiVal         VABCCM
034700070222     C                   Z-ADD     PiVal         VATCCM
034800070222     C                   ENDIF
034900070222     C                   ENDIF
035000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035200070222     C                   EXSR      CHKNUM
035300070222     C                   IF        PiInt=*on
035400070222     C                   Z-ADD     PiVal         VABLNP
035500070222     C                   Z-ADD     PiVal         VATLNP
035600070222     C                   ENDIF
035700070222     C                   ENDIF
035800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036000070222     C                   EXSR      CHKNUM
036100070222     C                   IF        PiInt=*on
036200070222     C                   Z-ADD     PiVal         VABCTR
036300070222     C                   ENDIF
036400070928     C                   ENDIF
036500071009     C                   MOVEL     *blanks       wDISK             1
036600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036700071009     C                   IF        wDISK <> *blanks
036800070928     C                   SETON                                        50
036900070222     C                   ENDIF
037000100525     C                   IF        wDISK  = 'T'
037100100525     C                   SETOFF                                       50
037200100525     C                   SETON                                        51
037300100525     C                   ENDIF
037400100401     C                   MOVEL     *blanks       wDUPREC           1
037500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037800100525     C                   ENDIF
037900111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
038000111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
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
041600111031     C*----------------------------------------------------*
041700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041800111031     C*----------------------------------------------------*
041900111031     C     IMPVAB        BEGSR
042000111031     C*
042100111031     C                   Z-ADD     *zeros        errore            1 0
042200111031     C                   MOVEL     datcor        VABAAS
042300111031     C                   MOVEL     datcor        VATAAS
042400111031     C                   MOVE      datcor        VABMGS
042500111031     C                   MOVE(P)   vlrpoi        VABFGS
042600111031     C                   MOVE(P)   vlrpoi        VATFGS
042700111031     C*
042800120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042900111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043000111031     C     '@':'A'       XLATE     VABRSD        VABRSD
043100111031     C* ==
043200111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
043300111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043400111031     C                                    %trim(%subst(vindta:341:30))+' '+
043500111031     C                                    %trim(%subst(vindta:371:5)))
043600111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043700111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043800111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
043900111031     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
044000111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
044100131009     C                   EVAL      VABCAD=%trim(%subst(vindta:391:9))
044200111031     C                   IF        %subst(vindta:43:1) = 'N'
044300111031     C                   EVAL      VABCBO='1'
044400111031     C                   ENDIF
044500111031     C                   IF        %subst(vindta:43:1) = 'P'
044600111031     C                   EVAL      VABCBO='2'
044700111031     C                   ENDIF
044800111031     C                   IF        %subst(vindta:511:3) = 'ABM'
044900111031     C                   EVAL      VABTIC='BM'
045000111031     C                   ENDIF
045100111031     C                   IF        %subst(vindta:511:3) = 'ABS'
045200111031     C                   EVAL      VABTIC='B'
045300111031     C                   ENDIF
045400111031     C                   IF        %subst(vindta:511:3) = 'CON'
045500111031     C                   EVAL      VABTIC=*blanks
045600111031     C                   ENDIF
045700111031     C                   IF        %subst(vindta:511:3) = 'ACM'
045800111031     C                   EVAL      VABTIC='CM'
045900111031     C                   ENDIF
046000111031     C                   IF        %subst(vindta:511:3) = 'ACS'
046100111031     C                   EVAL      VABTIC='CB'
046200111031     C                   ENDIF
046300111031     C                   IF        %subst(vindta:511:3) = 'VAR'
046400111031     C                   EVAL      VABTIC=*blanks
046500111031     C                   ENDIF
046600130711     C*
046700111031     C* Reperisco la provincia dal CAP e dalla località
046800130711     C                   IF        VABNZD  = *blanks AND
046900130711     C                             VABCAD <> *blanks AND
047000131009     C                             VABLOD <> *blanks AND
047100111031     C                             VABPRD  = *blanks
047200111031     C                   CLEAR                   TISI95DS
047300111031     C                   EVAL      I95TCN = '3'
047400111031     C                   Z-ADD     datcor        I95DAT
047500111031     C                   EVAL      I95CAP = VABCAD
047600111031     C                   EVAL      I95LOC = VABLOD
047700111031     C                   EVAL      I95NAR = VABNZD
047800111031     C                   CALL      'TISI95R'
047900111031     C                   PARM                    TISI95DS
048000111031     C                   EVAL      VABPRD = O95PRV
048100111031     C                   ENDIF
048200111031     C* NCL
048300111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
048400111031     C                   EXSR      CHKNUM
048500111031     C                   IF        PiInt=*on
048600111031     C                   SELECT
048700111031     C                   WHEN      wNCL = *blanks
048800111031     C                   Z-ADD     PiVal         VABNCL
048900111031     C                   WHEN      wNCL = 'A'
049000111031     C                   ADD       PiVal         VABNCL
049100111031     C                   WHEN      wNCL = '1'
049200111031     C                   ADD       1             VABNCL
049300111031     C                   ENDSL
049400111031     C                   ELSE
049500111031     C                   ADD       1             errore
049600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700111031     C                             + ' ' + 'VABNCL'
049800111031     C                   ENDIF
049900111031     C* PKB
050000120403     C* il peso è in grammi
050100111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
050200111031     C                   EXSR      CHKNUM
050300111031     C                   IF        PiNum=*on
050400120403     C                   EVAL      PiVal=PiVal/1000
050500111031     C                   Z-ADD     PiVal         VABPKB
050600111031     C                   ELSE
050700111031     C                   ADD       1             errore
050800111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900111031     C                             + ' ' + 'VABPKB'
051000111031     C                   ENDIF
051100111031     C* RMN
051200120403     C*** elenco delle posizioni utilizzate nel tempo
051300111031     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
051400120312     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
051500120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
051600120402     C                   EVAL      PiStr=%trim(%subst(vindta:555:7))
051700111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
051800111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
051900111031     C                   EXSR      CHKNUM
052000111031     C                   IF        PiInt=*on
052100111031     C                   Z-ADD     PiVal         VABRMN
052200111031     C                   ELSE
052300111031     C                   ADD       1             errore
052400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500111031     C                             + ' ' + 'VABRMN'
052600111031     C                   ENDIF
052700111031     C* CAS
052800111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
052900111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
053000111031     C                             %trim(%subst(vindta:502:9))<>*zeros
053100111031     C                   EVAL      VABVCA = 'EUR'
053200111031     C                   EVAL      FlgCAS = '1'
053300111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
053400111031     C                   EXSR      CHKNUM
053500111031     C                   IF        PiNum=*on
053600111031     C                   Z-ADD     PiVal         VABCAS
053700111031     C                   ELSE
053800111031     C                   ADD       1             errore
053900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054000111031     C                             + ' ' + 'VABCAS'
054100111031     C                   ENDIF
054200111031     C                   ENDIF
054300111031     C* IAS
054400111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
054500111031     C                   EVAL      VABVAS = 'EUR'
054600111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
054700111031     C                   EXSR      CHKNUM
054800111031     C                   IF        PiNum=*on
054900111031     C                   Z-ADD     PiVal         VABIAS
055000111031     C                   ELSE
055100111031     C                   ADD       1             errore
055200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055300111031     C                             + ' ' + 'VABIAS'
055400111031     C                   ENDIF
055500111031     C                   ENDIF
055600111031     C*
055700111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
055800130517     C*
055900130517     C* se il traffico è DPD:
056000130517     C* - uso la tariffa appropriata
056100130517     C* - la provincia diventa nazione
056200130517     C* - la provincia viene svuotata
056300130517     C* - il trattamento merce è 7R
056400130517     C                   IF        %subst(vindta:615:1) = 'D'
056500130517     C                   EVAL      VABCTR=391
056600130517     C                   EVAL      VABNZD=VABPRD
056700130517     C                   EVAL      VABPRD=*blank
056800130517     C                   EVAL      VABCTM = '7R'
056900130517     C                   ENDIF
057000130517     C*
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
061400111031     C                   ENDIF
061500111031     C*
061600111031     C                   IF        vabVMD > *zeros
061700111031     C                   IF        vabVAD <> 'EUR'
061800111031     C                   EVAL      vabVAD =  'ITL'
061900111031     C                   ENDIF
062000111031     C                   ENDIF
062100111031     C*
062200111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062300111031     C                   Z-ADD     vabIAS        wrkDec
062400111031     C                   IF        wrkDec > *zeros
062500111031     C                   IF        vabVAS = 'ITL'
062600111031     C                   EVAL      vabIAS = *zeros
062700111031     C                   ENDIF
062800111031     C                   ENDIF
062900111031     C*
063000111031     C* Stabilisco se il contrasegno ha decimali valorizzati
063100111031     C                   Z-ADD     vabCAS        wrkDec
063200111031     C                   IF        wrkDec > *zeros
063300111031     C                   IF        vabVCA = 'ITL'
063400111031     C                   EVAL      vabCAS = *zeros
063500111031     C                   ENDIF
063600111031     C                   ENDIF
063700111031     C*
063800111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063900111031     C                   Z-ADD     vabVMD        wrkDec
064000111031     C                   IF        wrkDec > *zeros
064100111031     C                   IF        vabVAD = 'ITL'
064200111031     C                   EVAL      vabVMD = *zeros
064300111031     C                   ENDIF
064400111031     C                   ENDIF
064500111031     C*
064600111031     C                   ENDSR
064700111031     C***
064800111031
064900111031
065000111031     C*----------------------------------------------------*
065100111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
065200111031     C*----------------------------------------------------*
065300111031     C     EXEVATA       BEGSR
065400111031     C*
065500111031     C***                EVAL      VATTRC='A'
065600111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
065700111031     C***                exsr      wrivat                                       => scarico VAT
065800111031     C*
065900111031     C                   ENDSR
066000111031     C*----------------------------------------------------*
066100111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
066200111031     C*----------------------------------------------------*
066300111031     C     EXEVATB       BEGSR
066400111031     C*
066500120403     C                   EVAL      VATTRC='B'
066600120403     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
066700120403     C                   exsr      wrivat                                       => scarico VAT
066800111031     C*
066900111031     C                   ENDSR
067000141010     C*----------------------------------------------------*
067100141010     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
067200141010     C*----------------------------------------------------*
067300141010     C     EXEVATS       BEGSR
067400141010     C*
067500141010     C                   EVAL      VATTRC='S'
067600141010     C                   EVAL      VATNOT = %trim(%subst(vindta:713:14))
067700141010     C                   IF        VATNOT <> *blank
067800141010     C                   EVAL      %subst(VATNOT:17:1) = 'S'
067900141010     C                   ENDIF
068000141010     C                   exsr      wrivat                                       => scarico VAT
068100141010     C*
068200141010     C                   ENDSR
068300111031     C*----------------------------------------------------*
068400111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
068500111031     C*----------------------------------------------------*
068600111031     C     EXEVATE       BEGSR
068700111031     C*
068800111031     C                   EVAL      VATTRC='E'
068900111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
069000111031     C                   exsr      wrivat                                       => scarico VAT
069100111031     C*
069200111031     C                   ENDSR
069300120402     C*----------------------------------------------------*
069400120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
069500120402     C*----------------------------------------------------*
069600120402     C     EXEVATI       BEGSR
069700120402     C*
069800120402     C                   EVAL      VATTRC='I'
069900120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
070000120402     C                   exsr      wrivat                                       => scarico VAT
070100120402     C*
070200120402     C                   EVAL      VATTRC='J'
070300120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
070400120402     C                   exsr      wrivat                                       => scarico VAT
070500120402     C*
070600120402     C                   ENDSR
070700111031     C*----------------------------------------------------*
070800111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
070900111031     C*----------------------------------------------------*
071000111031     C     WRIVAT        BEGSR
071100111031     C*
071200111031     C* Scrivo solo se valorizzato qualcosa
071300111031     C                   IF        VATNOT <> *blanks
071400111031     C                   WRITE     FIVAT000
071500111031     C                   ENDIF
071600111031     C*
071700111031     C                   ENDSR
071800020904
071900020904
072000010202     C*----------------------------------------------------*
072100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
072200010202     C*----------------------------------------------------*
072300100525     C     PREVASX       BEGSR
072400100525     C*
072500021113     C* Compongo il nome del membro da dare al FIVATWWR
072600010202     C                   eval      parmbr = vlrhdl
072700010202     C                   movel     'M'           parmbr
072800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072900010202     C                   eval      paropz = '1'
073000100525     C*
073100010202     C* Effettuo la chiamata al CLLE preposto
073200100525     C  N51              call(e)   'TITVVTC'
073300010202     C                   parm                    parccm
073400010202     C                   parm                    parmbr
073500010202     C                   parm                    paropz
073600100525     C   51              call(e)   'TITVVBC'
073700100525     C                   parm                    parccm
073800100525     C                   parm                    parmbr
073900100525     C                   parm                    paropz
074000100525     C*
074100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
074200010202     C                   if        %error
074300010202     C                   movel     '1'           chkcall
074400010202     C                   else
074500010202     C                   movel     '0'           chkcall
074600010202     C                   endif
074700010202     C*
074800010202     C                   ENDSR
074900000801     C*----------------------------------------------------*
075000000801     C*  CONTROLLO NUMERICITA' CAMPI
075100000801     C*----------------------------------------------------*
075200000801     C     CHKNUM        BEGSR
075300081113     C*
075400081113     C                   IF        PiDecChr = *blanks
075500110408     C                   EVAL      PiDecChr = '.'
075600081113     C                   ENDIF
075700091223     C*
075800081113     C                   callp(e)  UBISNUM_Check(PiStr
075900081113     C                                          :PiDecChr
076000081113     C                                          :PiVal
076100081113     C                                          :PiNum
076200081113     C                                          :PiInt)
076300081113     C*
076400000801     C                   IF        %error
076500000801     C                   EVAL      PiInt=*off
076600000801     C                   ENDIF
076700000801     C*
076800000801     C                   ENDSR
076900000801     C***
077000011113
077100011113
077200000801
077300000801
077400990920      /TITLE Invio dei dati al punto operativo.
077500010202     C     invio         BEGSR
077600990920     C*
077700021113     C* 1° invio FIVAT
077800010201     C                   reset                   dscmz
077900110411     C                   move      vatfgs        cmzdst
078000021113     C                   eval      cmzfld = 'FIVATWWR'
078100010201     C                   eval      cmzmbd = vlrhdl
078200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078300021009     C***                if        prmfir = *blanks
078400021113     C                   eval      cmzfla = 'FIVAT00F'
078500021113     C                   eval      cmzmba = 'FIVAT00F'
078600021009     C***                else
078700021009     C***                eval      cmzfla = prmfir
078800021009     C***                eval      cmzmba = prmfir
078900021009     C***                endif
079000010201     C                   eval      cmznrr = *zeros
079100020305     C                   move      §ctrokvt      cmznrr
079200021018     C                   eval      cmzlba = vlrfl1
079300010201     C                   call(e)   'TIS711C'
079400010201     C                   parm                    dscmz
079500010201     C                   parm      *blanks       esito
079600010205     C                   if        %error
079700010205     C                             or cmzerr = '1'
079800010205     C                             or esito  = '1'
079900010205     C                   eval      wrkesito = '3'
080000010205     C                   else
080100010201     C*
080200021113     C* 2° invio FIVAB
080300100525     C                   if        *in51 = *off
080400010201     C                   reset                   dscmz
080500110411     C                   move      vabfgs        cmzdst
080600010201     C                   eval      cmzfld = vlrfou
080700010201     C                   eval      cmzmbd = vlrhdl
080800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080900021009     C***                if        prmfir = *blanks
081000021113     C                   eval      cmzfla = 'FIVAB00F'
081100021113     C                   eval      cmzmba = 'FIVAB00F'
081200021009     C***                else
081300021009     C***                eval      cmzfla = prmfir
081400021009     C***                eval      cmzmba = prmfir
081500021009     C***                endif
081600010201     C                   eval      cmznrr = *zeros
081700010201     C                   move      §ctrokvb      cmznrr
081800021018     C                   eval      cmzlba = vlrfl1
081900010201     C                   call(e)   'TIS711C'
082000010201     C                   parm                    dscmz
082100010201     C                   parm      *blanks       esito
082200010201     C                   if        %error
082300010201     C                             or cmzerr = '1'
082400010201     C                             or esito  = '1'
082500010201     C                   eval      wrkesito = '3'
082600010201     C                   endif
082700100525     C                   endif
082800010205     C                   endif
082900990920     C*
083000000613     C                   ENDSR
083100000613     C***
083200070411
083300110627     C     *pssr         BEGSR
083400070411     C*
083500110627     C                   if        %open(tivin00r)
083600110627     C                   close     tivin00r
083700110627     C                   endif
083800110627     C                   if        %open(fivabwwr)
083900110627     C                   close     fivabwwr
084000110627     C                   endif
084100110627     C                   if        %open(fivatwwr)
084200110627     C                   close     fivatwwr
084300110627     C                   endif
084400110627     C*
084500110627     C* Effettuo la chiamata al CLLE preposto
084600110627     C  N51              call(e)   'TITVVTC'
084700110627     C                   parm                    parccm
084800110627     C                   parm                    parmbr
084900110627     C                   parm      '2'           paropz
085000110627     C   51              call(e)   'TITVVBC'
085100110627     C                   parm                    parccm
085200110627     C                   parm                    parmbr
085300110627     C                   parm      '2'           paropz
085400110627     C*
085500110627     C                   eval      wrkesito = '2'
085600110627     C*
085700110627     C                   seton                                        LR
085800110627     C*
085900110627     C                   ENDSR     '*CANCL'
086000070411     C***
086100070411
086200990910
086300000613     C     *inzsr        BEGSR
086400990910     C*
086500990910     C     *entry        plist
086600990920     C                   parm                    tivlrds
086700990921     C                   parm      wrkesito      esito
086800000724     C                   parm                    prmlit
086900000710     C                   parm                    prmfir
087000000613     C*
087100000830     C* CALCOLA LA DATA CORRENTE
087200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087300091223     C                   eval      datcor = %dec(%date() : *ISO)
087400000830     C*
087500000613     C                   ENDSR
087600000613     C***
