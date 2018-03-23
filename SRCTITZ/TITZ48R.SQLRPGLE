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
003100020725
003200020725     D*------------------
003300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003400020725     D*------------------
003500070530     D INPUT_DS        DS                  INZ
003600100729     D  VINDTA                     2048
003700100729     D  VINFLG                        1
003800111031     D  VINSPE                       25
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
013500111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
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
016500111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
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
017600120911     C***                exsr      exevati                                      => carico VAT
017700071003     C                   else
017800080923     C*
017900071009     C                   if        wDISK = 'D'
018000091223     C                   exsr      repNSP
018100071009     C                   exsr      impvab
018200100412     C                   exsr      wrivab
018300111031     C                   exsr      exevata                                      => carico VAT
018400111031     C                   exsr      exevatb                                      => carico VAT
018500111031     C                   exsr      exevate                                      => carico VAT
018600120911     C***                exsr      exevati                                      => carico VAT
018700071009     C                   else
018800071009     C*
018900010305     C                   if        depspe = *blanks                             => 1° giro
019000010305     C                   eval      depspe = curspe                              => memorizz. spediz
019100080117     C                   clear                   tivinds
019200091223     C                   exsr      repNSP
019300020305     C                   exsr      impvab
019400111031     C                   exsr      exevata                                      => carico VAT
019500111031     C                   exsr      exevatb                                      => carico VAT
019600111031     C   50              exsr      exevate                                      => carico VAT
019700120911     C***                exsr      exevati                                      => carico VAT
019800010305     C                   else
019900020725     C                   if        depspe <> curspe                             => rottura di spediz
020000010305     C                   eval      depspe = curspe                              => memorizz. spediz
020100100412     C                   exsr      wrivab
020200080117     C                   clear                   tivinds
020300091223     C                   exsr      repNSP
020400020305     C                   exsr      impvab
020500111031     C                   exsr      exevata                                      => carico VAT
020600111031     C                   exsr      exevatb                                      => carico VAT
020700111031     C   50              exsr      exevate                                      => carico VAT
020800120911     C***                exsr      exevati                                      => carico VAT
020900020305     C                   else                                                   => x stessa spediz
021000100401     C*
021100100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021200100928     C                   select
021300100928     C                   when      wDUPREC = ' '
021400020305     C                   exsr      impvab
021500111031     C                   exsr      exevata                                      => carico VAT
021600111031     C                   exsr      exevatb                                      => carico VAT
021700111031     C   50              exsr      exevate                                      => carico VAT
021800120911     C***                exsr      exevati                                      => carico VAT
021900100928     C                   when      wDUPREC = 'C'
022000111031     C   50              exsr      exevate                                      => carico VAT
022100100928     C                   endsl
022200010305     C                   endif
022300010305     C                   endif
022400010305     C                   endif
022500071003     C                   endif
022600110201     C*
022700071009     C                   endif
022800000905     C*
022900000905     C                   else
023000020725     C                   eval      x_vinflg = '1'
023100000905     C                   endif
023200000905     C*
023300020725     C     VINRRN        chain     tivin000
023400020725     C                   if        %found(tivin00r)
023500020725     C                   eval      y_vinflg = x_vinflg
023600020725     C                   eval      y_vinmsg = x_vinmsg
023700020725     C                   update    tivin000
023800020725     C                   endif
023900020725     C*
024000020725     C/EXEC SQL
024100070530     C+ Fetch C1 into :INPUT_DS
024200020725     C/END-EXEC
024300020725     C*
024400020725     C                   enddo
024500020725     C*
024600020725     C/EXEC SQL
024700020725     C+ close C1
024800020725     C/END-EXEC
024900000905     C*
025000020305     C* Scarico i VAB rimasti "in sospeso"
025100071009     C                   if        wDISK = 'C'
025200100412     C                   exsr      wrivab
025300071009     C                   endif
025400010202     C*
025500010202     C                   endif
025600990910
025700990910     C* Se non ci sono record con errori ...
025800000710     C                   if        §ctrno = 0
025900990910     C* ... restituisco esito OK.
026000990921     C                   eval      wrkesito = '0'
026100990910     C                   else
026200100525     C                   if        §ctrokvb > 0 OR
026300100525     C                             §ctrokvt > 0
026400990921     C                   eval      wrkesito = '1'
026500000710     C                   else
026600000710     C                   eval      wrkesito = '2'
026700990910     C                   endif
026800000710     C                   endif
026900990910     C*
027000990914     C                   if        %open(tivin00r)
027100990908     C                   close     tivin00r
027200990914     C                   endif
027300021113     C                   if        %open(fivabwwr)
027400021113     C                   close     fivabwwr
027500990914     C                   endif
027600021113     C                   if        %open(fivatwwr)
027700021113     C                   close     fivatwwr
027800010201     C                   endif
027900990910     C*
028000100525     C                   if        §ctrokvb > 0 OR
028100100525     C                             §ctrokvt > 0
028200000724     C                             and vlrpoi <> *zeros
028300010202     C                   exsr      invio
028400990920     C                   endif
028500990920     C*
028600910830     C                   ENDSR
028700000613     C***
028800100412
028900100412
029000010305
029100010305     C*----------------------------------------------------*
029200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029300010305     C*----------------------------------------------------*
029400020305     C     WRIVAB        BEGSR
029500100412     C*
029600100412     C* Gestisco l'eventuale rottura x numero spedizione
029700100412     C                   if        VABRMA = *blanks
029800100412     C                   movel(P)  VABRMN        VABRMA
029900100412     C                   endif
030000100412     C*
030100100525     C                   if        *in51 = *off and *in31 = *off
030200100525     C                   write     fivab000                                     => scarico il VAB
030300100525     C                   endif
030400010305     C*
030500010305     C                   ENDSR
030600990920
030700000801     C*----------------------------------------------------*
030800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030900000801     C*----------------------------------------------------*
031000010201     C     INZVAR        BEGSR
031100000801     C*
031200010201     C                   Z-ADD     *zeros        Num5_0
031300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031400020725     C                   MOVEL     '0'           FlgCAS            1
031500000801     C*
031600000801     C                   ENDSR
031700000801     C*----------------------------------------------------*
031800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031900000801     C*----------------------------------------------------*
032000020904     C     DEFCAM        BEGSR
032100080923     C*
032200100928     C                   SETOFF                                       505160
032300000801     C*
032400021113     C                   CLEAR                   FIVAB000
032500021113     C                   CLEAR                   FIVAT000
032600070730     C* Imposto i valori di default...
032700120910     C                   EVAL      VABCCM = 0010354
032800120910     C                   EVAL      VATCCM = 0010354
032900120910     C                   EVAL      VABLNP = 001
033000120910     C                   EVAL      VATLNP = 001
033100120910     C                   EVAL      VABCTR = 000
033200100525     C                   EVAL      VABCBO = '1'
033300070222     C* ... e poi verifico se sono stati passati come parametri
033400070222     C                   IF        vlrppt > *blanks
033500100525     C*
033600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033800070222     C                   EXSR      CHKNUM
033900070222     C                   IF        PiInt=*on
034000070222     C                   Z-ADD     PiVal         VABCCM
034100070222     C                   Z-ADD     PiVal         VATCCM
034200070222     C                   ENDIF
034300070222     C                   ENDIF
034400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABLNP
034900070222     C                   Z-ADD     PiVal         VATLNP
035000070222     C                   ENDIF
035100070222     C                   ENDIF
035200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABCTR
035700070222     C                   ENDIF
035800070928     C                   ENDIF
035900071009     C                   MOVEL     *blanks       wDISK             1
036000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036100071009     C                   IF        wDISK <> *blanks
036200070928     C                   SETON                                        50
036300070222     C                   ENDIF
036400100525     C                   IF        wDISK  = 'T'
036500100525     C                   SETOFF                                       50
036600100525     C                   SETON                                        51
036700100525     C                   ENDIF
036800100401     C                   MOVEL     *blanks       wDUPREC           1
036900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037200100525     C                   ENDIF
037300111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
037400111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
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
041000111031     C*----------------------------------------------------*
041100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041200111031     C*----------------------------------------------------*
041300111031     C     IMPVAB        BEGSR
041400111031     C*
041500111031     C                   Z-ADD     *zeros        errore            1 0
041600111031     C                   MOVEL     datcor        VABAAS
041700111031     C                   MOVEL     datcor        VATAAS
041800111031     C                   MOVE      datcor        VABMGS
041900111031     C                   MOVE(P)   vlrpoi        VABFGS
042000111031     C                   MOVE(P)   vlrpoi        VATFGS
042100111031     C*
042200120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042300111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042400111031     C     '@':'A'       XLATE     VABRSD        VABRSD
042500111031     C* ==
042600111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
042700111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
042800111031     C                                    %trim(%subst(vindta:341:30))+' '+
042900111031     C                                    %trim(%subst(vindta:371:5)))
043000111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043100111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043200111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
043300111031     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
043400111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
043500111031     C                   IF        %subst(vindta:43:1) = 'N'
043600111031     C                   EVAL      VABCBO='1'
043700111031     C                   ENDIF
043800111031     C                   IF        %subst(vindta:43:1) = 'P'
043900111031     C                   EVAL      VABCBO='2'
044000111031     C                   ENDIF
044100111031     C                   IF        %subst(vindta:511:3) = 'ABM'
044200111031     C                   EVAL      VABTIC='BM'
044300111031     C                   ENDIF
044400111031     C                   IF        %subst(vindta:511:3) = 'ABS'
044500111031     C                   EVAL      VABTIC='B'
044600111031     C                   ENDIF
044700111031     C                   IF        %subst(vindta:511:3) = 'CON'
044800111031     C                   EVAL      VABTIC=*blanks
044900111031     C                   ENDIF
045000111031     C                   IF        %subst(vindta:511:3) = 'ACM'
045100111031     C                   EVAL      VABTIC='CM'
045200111031     C                   ENDIF
045300111031     C                   IF        %subst(vindta:511:3) = 'ACS'
045400111031     C                   EVAL      VABTIC='CB'
045500111031     C                   ENDIF
045600111031     C                   IF        %subst(vindta:511:3) = 'VAR'
045700111031     C                   EVAL      VABTIC=*blanks
045800111031     C                   ENDIF
045900111031     C* CAP
046000111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
046100111031     C                   EXSR      CHKNUM
046200111031     C                   IF        PiInt=*on
046300111031     C                   Z-ADD     PiVal         Num5_0
046400111031     C                   MOVEL(P)  Num5_0        VABCAD
046500111031     C                   ELSE
046600111031     C                   ADD       1             errore
046700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046800111031     C                             + ' ' + 'VABCAD'
046900111031     C                   ENDIF
047000111031     C* Reperisco la provincia dal CAP e dalla località
047100111031     C                   IF        VABCAD <> *blanks AND
047200111031     C                             VABPRD  = *blanks
047300111031     C                   CLEAR                   TISI95DS
047400111031     C                   EVAL      I95TCN = '3'
047500111031     C                   Z-ADD     datcor        I95DAT
047600111031     C                   EVAL      I95CAP = VABCAD
047700111031     C                   EVAL      I95LOC = VABLOD
047800111031     C                   EVAL      I95NAR = VABNZD
047900111031     C                   CALL      'TISI95R'
048000111031     C                   PARM                    TISI95DS
048100111031     C                   EVAL      VABPRD = O95PRV
048200111031     C                   ENDIF
048300111031     C* NCL
048400111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
048500111031     C                   EXSR      CHKNUM
048600111031     C                   IF        PiInt=*on
048700111031     C                   SELECT
048800111031     C                   WHEN      wNCL = *blanks
048900111031     C                   Z-ADD     PiVal         VABNCL
049000111031     C                   WHEN      wNCL = 'A'
049100111031     C                   ADD       PiVal         VABNCL
049200111031     C                   WHEN      wNCL = '1'
049300111031     C                   ADD       1             VABNCL
049400111031     C                   ENDSL
049500111031     C                   ELSE
049600111031     C                   ADD       1             errore
049700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800111031     C                             + ' ' + 'VABNCL'
049900111031     C                   ENDIF
050000111031     C* PKB
050100120403     C* il peso è in grammi
050200111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
050300111031     C                   EXSR      CHKNUM
050400111031     C                   IF        PiNum=*on
050500120403     C                   EVAL      PiVal=PiVal/1000
050600111031     C                   Z-ADD     PiVal         VABPKB
050700111031     C                   ELSE
050800111031     C                   ADD       1             errore
050900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051000111031     C                             + ' ' + 'VABPKB'
051100111031     C                   ENDIF
051200111031     C* RMN
051300120403     C*** elenco delle posizioni utilizzate nel tempo
051400111031     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
051500120911     C* l'unica non alfanumerica ma... è utile?
051600120911     C                   EVAL      PiStr=%trim(%subst(vindta:46+10:15))
051700120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
051800120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
051900111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
052000111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
052100111031     C                   EXSR      CHKNUM
052200111031     C                   IF        PiInt=*on
052300111031     C                   Z-ADD     PiVal         VABRMN
052400111031     C                   ELSE
052500111031     C                   ADD       1             errore
052600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052700111031     C                             + ' ' + 'VABRMN'
052800111031     C                   ENDIF
052900111031     C* CAS
053000111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
053100111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
053200111031     C                             %trim(%subst(vindta:502:9))<>*zeros
053300111031     C                   EVAL      VABVCA = 'EUR'
053400111031     C                   EVAL      FlgCAS = '1'
053500111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
053600111031     C                   EXSR      CHKNUM
053700111031     C                   IF        PiNum=*on
053800111031     C                   Z-ADD     PiVal         VABCAS
053900111031     C                   ELSE
054000111031     C                   ADD       1             errore
054100111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054200111031     C                             + ' ' + 'VABCAS'
054300111031     C                   ENDIF
054400111031     C                   ENDIF
054500111031     C* IAS
054600111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
054700111031     C                   EVAL      VABVAS = 'EUR'
054800111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
054900111031     C                   EXSR      CHKNUM
055000111031     C                   IF        PiNum=*on
055100111031     C                   Z-ADD     PiVal         VABIAS
055200111031     C                   ELSE
055300111031     C                   ADD       1             errore
055400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055500111031     C                             + ' ' + 'VABIAS'
055600111031     C                   ENDIF
055700111031     C                   ENDIF
055800111031     C*
055900111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
056000111031     C                   IF        FlgCAS <> '0'
056100111031     C                   IF        VABCBO = '1'
056200111031     C                   EVAL      VABCBO = '4'
056300111031     C                   ELSE
056400111031     C                   EVAL      VABCBO = '6'
056500111031     C                   ENDIF
056600111031     C                   ENDIF
056700111031     C*
056800111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
056900111031     C                   EXSR      CHKIMPDIV
057000111031     C*
057100111031     C* Ebbene...
057200111031     C                   ADD       1             §CTRMO
057300111031     C                   IF        errore <> *zeros
057400111031     C                   ADD       1             §CTRNO
057500111031     C                   EVAL      recko = vindta
057600111031     C                   EVAL      vinflg = '2'
057700111031     C                   ELSE
057800111031     C                   ADD       1             §CTROKVB
057900111031     C                   ENDIF
058000111031     C*
058100111031     C                   ENDSR
058200111031
058300111031
058400111031     C*----------------------------------------------------*
058500111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058600111031     C*----------------------------------------------------*
058700111031     C     CHKIMPDIV     BEGSR
058800111031     C*
058900111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059000111031     C                   Z-ADD     *zeros        wrkDec            9 9
059100111031     C*
059200111031     C* Come prima cosa effettuo considerazioni sulla divisa
059300111031     C                   IF        vabIAS > *zeros
059400111031     C                   IF        vabVAS <> 'EUR'
059500111031     C                   EVAL      vabVAS =  'ITL'
059600111031     C                   ENDIF
059700111031     C                   ENDIF
059800111031     C*
059900111031     C                   IF        vabCAS > *zeros
060000111031     C                   IF        vabVCA <> 'EUR'
060100111031     C                   EVAL      vabVCA =  'ITL'
060200111031     C                   ENDIF
060300111031     C                   ENDIF
060400111031     C*
060500111031     C                   IF        vabVMD > *zeros
060600111031     C                   IF        vabVAD <> 'EUR'
060700111031     C                   EVAL      vabVAD =  'ITL'
060800111031     C                   ENDIF
060900111031     C                   ENDIF
061000111031     C*
061100111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061200111031     C                   Z-ADD     vabIAS        wrkDec
061300111031     C                   IF        wrkDec > *zeros
061400111031     C                   IF        vabVAS = 'ITL'
061500111031     C                   EVAL      vabIAS = *zeros
061600111031     C                   ENDIF
061700111031     C                   ENDIF
061800111031     C*
061900111031     C* Stabilisco se il contrasegno ha decimali valorizzati
062000111031     C                   Z-ADD     vabCAS        wrkDec
062100111031     C                   IF        wrkDec > *zeros
062200111031     C                   IF        vabVCA = 'ITL'
062300111031     C                   EVAL      vabCAS = *zeros
062400111031     C                   ENDIF
062500111031     C                   ENDIF
062600111031     C*
062700111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062800111031     C                   Z-ADD     vabVMD        wrkDec
062900111031     C                   IF        wrkDec > *zeros
063000111031     C                   IF        vabVAD = 'ITL'
063100111031     C                   EVAL      vabVMD = *zeros
063200111031     C                   ENDIF
063300111031     C                   ENDIF
063400111031     C*
063500111031     C                   ENDSR
063600111031     C***
063700111031
063800111031
063900111031     C*----------------------------------------------------*
064000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
064100111031     C*----------------------------------------------------*
064200111031     C     EXEVATA       BEGSR
064300111031     C*
064400111031     C***                EVAL      VATTRC='A'
064500111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
064600111031     C***                exsr      wrivat                                       => scarico VAT
064700111031     C*
064800111031     C                   ENDSR
064900111031     C*----------------------------------------------------*
065000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
065100111031     C*----------------------------------------------------*
065200111031     C     EXEVATB       BEGSR
065300111031     C*
065400120403     C                   EVAL      VATTRC='B'
065500120403     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
065600120403     C                   exsr      wrivat                                       => scarico VAT
065700111031     C*
065800111031     C                   ENDSR
065900111031     C*----------------------------------------------------*
066000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
066100111031     C*----------------------------------------------------*
066200111031     C     EXEVATE       BEGSR
066300111031     C*
066400111031     C                   EVAL      VATTRC='E'
066500111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
066600111031     C                   exsr      wrivat                                       => scarico VAT
066700111031     C*
066800111031     C                   ENDSR
066900120402     C*----------------------------------------------------*
067000120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
067100120402     C*----------------------------------------------------*
067200120402     C     EXEVATI       BEGSR
067300120402     C*
067400120402     C                   EVAL      VATTRC='I'
067500120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
067600120402     C                   exsr      wrivat                                       => scarico VAT
067700120402     C*
067800120402     C                   EVAL      VATTRC='J'
067900120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
068000120402     C                   exsr      wrivat                                       => scarico VAT
068100120402     C*
068200120402     C                   ENDSR
068300111031     C*----------------------------------------------------*
068400111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
068500111031     C*----------------------------------------------------*
068600111031     C     WRIVAT        BEGSR
068700111031     C*
068800111031     C* Scrivo solo se valorizzato qualcosa
068900111031     C                   IF        VATNOT <> *blanks
069000111031     C                   WRITE     FIVAT000
069100111031     C                   ENDIF
069200111031     C*
069300111031     C                   ENDSR
069400020904
069500020904
069600010202     C*----------------------------------------------------*
069700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
069800010202     C*----------------------------------------------------*
069900100525     C     PREVASX       BEGSR
070000100525     C*
070100021113     C* Compongo il nome del membro da dare al FIVATWWR
070200010202     C                   eval      parmbr = vlrhdl
070300010202     C                   movel     'M'           parmbr
070400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070500010202     C                   eval      paropz = '1'
070600100525     C*
070700010202     C* Effettuo la chiamata al CLLE preposto
070800100525     C  N51              call(e)   'TITVVTC'
070900010202     C                   parm                    parccm
071000010202     C                   parm                    parmbr
071100010202     C                   parm                    paropz
071200100525     C   51              call(e)   'TITVVBC'
071300100525     C                   parm                    parccm
071400100525     C                   parm                    parmbr
071500100525     C                   parm                    paropz
071600100525     C*
071700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071800010202     C                   if        %error
071900010202     C                   movel     '1'           chkcall
072000010202     C                   else
072100010202     C                   movel     '0'           chkcall
072200010202     C                   endif
072300010202     C*
072400010202     C                   ENDSR
072500000801     C*----------------------------------------------------*
072600000801     C*  CONTROLLO NUMERICITA' CAMPI
072700000801     C*----------------------------------------------------*
072800000801     C     CHKNUM        BEGSR
072900081113     C*
073000081113     C                   IF        PiDecChr = *blanks
073100110408     C                   EVAL      PiDecChr = '.'
073200081113     C                   ENDIF
073300091223     C*
073400081113     C                   callp(e)  UBISNUM_Check(PiStr
073500081113     C                                          :PiDecChr
073600081113     C                                          :PiVal
073700081113     C                                          :PiNum
073800081113     C                                          :PiInt)
073900081113     C*
074000000801     C                   IF        %error
074100000801     C                   EVAL      PiInt=*off
074200000801     C                   ENDIF
074300000801     C*
074400000801     C                   ENDSR
074500000801     C***
074600011113
074700011113
074800000801
074900000801
075000990920      /TITLE Invio dei dati al punto operativo.
075100010202     C     invio         BEGSR
075200990920     C*
075300021113     C* 1° invio FIVAT
075400010201     C                   reset                   dscmz
075500110411     C                   move      vatfgs        cmzdst
075600021113     C                   eval      cmzfld = 'FIVATWWR'
075700010201     C                   eval      cmzmbd = vlrhdl
075800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075900021009     C***                if        prmfir = *blanks
076000021113     C                   eval      cmzfla = 'FIVAT00F'
076100021113     C                   eval      cmzmba = 'FIVAT00F'
076200021009     C***                else
076300021009     C***                eval      cmzfla = prmfir
076400021009     C***                eval      cmzmba = prmfir
076500021009     C***                endif
076600010201     C                   eval      cmznrr = *zeros
076700020305     C                   move      §ctrokvt      cmznrr
076800021018     C                   eval      cmzlba = vlrfl1
076900010201     C                   call(e)   'TIS711C'
077000010201     C                   parm                    dscmz
077100010201     C                   parm      *blanks       esito
077200010205     C                   if        %error
077300010205     C                             or cmzerr = '1'
077400010205     C                             or esito  = '1'
077500010205     C                   eval      wrkesito = '3'
077600010205     C                   else
077700010201     C*
077800021113     C* 2° invio FIVAB
077900100525     C                   if        *in51 = *off
078000010201     C                   reset                   dscmz
078100110411     C                   move      vabfgs        cmzdst
078200010201     C                   eval      cmzfld = vlrfou
078300010201     C                   eval      cmzmbd = vlrhdl
078400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078500021009     C***                if        prmfir = *blanks
078600021113     C                   eval      cmzfla = 'FIVAB00F'
078700021113     C                   eval      cmzmba = 'FIVAB00F'
078800021009     C***                else
078900021009     C***                eval      cmzfla = prmfir
079000021009     C***                eval      cmzmba = prmfir
079100021009     C***                endif
079200010201     C                   eval      cmznrr = *zeros
079300010201     C                   move      §ctrokvb      cmznrr
079400021018     C                   eval      cmzlba = vlrfl1
079500010201     C                   call(e)   'TIS711C'
079600010201     C                   parm                    dscmz
079700010201     C                   parm      *blanks       esito
079800010201     C                   if        %error
079900010201     C                             or cmzerr = '1'
080000010201     C                             or esito  = '1'
080100010201     C                   eval      wrkesito = '3'
080200010201     C                   endif
080300100525     C                   endif
080400010205     C                   endif
080500990920     C*
080600000613     C                   ENDSR
080700000613     C***
080800070411
080900110627     C     *pssr         BEGSR
081000070411     C*
081100110627     C                   if        %open(tivin00r)
081200110627     C                   close     tivin00r
081300110627     C                   endif
081400110627     C                   if        %open(fivabwwr)
081500110627     C                   close     fivabwwr
081600110627     C                   endif
081700110627     C                   if        %open(fivatwwr)
081800110627     C                   close     fivatwwr
081900110627     C                   endif
082000110627     C*
082100110627     C* Effettuo la chiamata al CLLE preposto
082200110627     C  N51              call(e)   'TITVVTC'
082300110627     C                   parm                    parccm
082400110627     C                   parm                    parmbr
082500110627     C                   parm      '2'           paropz
082600110627     C   51              call(e)   'TITVVBC'
082700110627     C                   parm                    parccm
082800110627     C                   parm                    parmbr
082900110627     C                   parm      '2'           paropz
083000110627     C*
083100110627     C                   eval      wrkesito = '2'
083200110627     C*
083300110627     C                   seton                                        LR
083400110627     C*
083500110627     C                   ENDSR     '*CANCL'
083600070411     C***
083700070411
083800990910
083900000613     C     *inzsr        BEGSR
084000990910     C*
084100990910     C     *entry        plist
084200990920     C                   parm                    tivlrds
084300990921     C                   parm      wrkesito      esito
084400000724     C                   parm                    prmlit
084500000710     C                   parm                    prmfir
084600000613     C*
084700000830     C* CALCOLA LA DATA CORRENTE
084800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084900091223     C                   eval      datcor = %dec(%date() : *ISO)
085000000830     C*
085100000613     C                   ENDSR
085200000613     C***
