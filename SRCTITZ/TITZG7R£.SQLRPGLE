000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400130116      * PARTICOLARITA':
000500130116
000600020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000700100412     FFIVABwwr  O    E             DISK    usropn
000800021113     FFIVATwwr  O    E             DISK    usropn
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700070730     D tisi95ds      e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300000621     D recko           s            150    INZ(*blanks)
002400011113     D depcmd          s            150    INZ(*blanks)
002500111031     D depspe          s             25    INZ(*blanks)
002600111031     D curspe          s             25    INZ(*blanks)
002700010202     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003200091223     D Num5_0          s              5  0
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700070530     D INPUT_DS        DS                  INZ
003800100729     D  VINDTA                     2048
003900100729     D  VINFLG                        1
004000111031     D  VINSPE                       25
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
010300990908
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
013700111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
013800060223     C+ from tivin00r
013900111020     C+ order by sped
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
016500010305     C*
016600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016700111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
016800010305     C*
016900100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017000080923     C                   if        *in50 = *off
017100101119     C                   exsr      inzvar
017200101119     C                   exsr      defcam
017300071003     C                   exsr      impvab
017400100525     C                   exsr      wrivab
017500111031     C                   exsr      exevata                                      => carico VAT
017600111031     C                   exsr      exevatb                                      => carico VAT
017700111031     C   51              exsr      exevate                                      => carico VAT
017800120911     C***                exsr      exevati                                      => carico VAT
017900071003     C                   else
018000080923     C*
018100071009     C                   if        wDISK = 'D'
018200091223     C                   exsr      repNSP
018300071009     C                   exsr      impvab
018400100412     C                   exsr      wrivab
018500111031     C                   exsr      exevata                                      => carico VAT
018600111031     C                   exsr      exevatb                                      => carico VAT
018700111031     C                   exsr      exevate                                      => carico VAT
018800120911     C***                exsr      exevati                                      => carico VAT
018900071009     C                   else
019000071009     C*
019100010305     C                   if        depspe = *blanks                             => 1° giro
019200010305     C                   eval      depspe = curspe                              => memorizz. spediz
019300080117     C                   clear                   tivinds
019400091223     C                   exsr      repNSP
019500020305     C                   exsr      impvab
019600111031     C                   exsr      exevata                                      => carico VAT
019700111031     C                   exsr      exevatb                                      => carico VAT
019800111031     C   50              exsr      exevate                                      => carico VAT
019900120911     C***                exsr      exevati                                      => carico VAT
020000010305     C                   else
020100020725     C                   if        depspe <> curspe                             => rottura di spediz
020200010305     C                   eval      depspe = curspe                              => memorizz. spediz
020300100412     C                   exsr      wrivab
020400080117     C                   clear                   tivinds
020500091223     C                   exsr      repNSP
020600020305     C                   exsr      impvab
020700111031     C                   exsr      exevata                                      => carico VAT
020800111031     C                   exsr      exevatb                                      => carico VAT
020900111031     C   50              exsr      exevate                                      => carico VAT
021000120911     C***                exsr      exevati                                      => carico VAT
021100020305     C                   else                                                   => x stessa spediz
021200100401     C*
021300100401     C* Se nn richiesta esclusione spedizioni "duplicate"
021400100928     C                   select
021500100928     C                   when      wDUPREC = ' '
021600020305     C                   exsr      impvab
021700111031     C                   exsr      exevata                                      => carico VAT
021800111031     C                   exsr      exevatb                                      => carico VAT
021900111031     C   50              exsr      exevate                                      => carico VAT
022000120911     C***                exsr      exevati                                      => carico VAT
022100100928     C                   when      wDUPREC = 'C'
022200111031     C   50              exsr      exevate                                      => carico VAT
022300100928     C                   endsl
022400010305     C                   endif
022500010305     C                   endif
022600010305     C                   endif
022700071003     C                   endif
022800110201     C*
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
032900140325     C                   EVAL      VABCCM = 0057449
033000140325     C                   EVAL      VATCCM = 0057449
033100140325     C                   EVAL      VABLNP = 005
033200140325     C                   EVAL      VATLNP = 005
033300140325     C                   EVAL      VABCTR = 001
033400140325     C                   EVAL      VABCTM = '7Q'
033500140325     C                   EVAL      VABCBO = '1'
033600070222     C* ... e poi verifico se sono stati passati come parametri
033700070222     C                   IF        vlrppt > *blanks
033800100525     C*
033900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034100070222     C                   EXSR      CHKNUM
034200070222     C                   IF        PiInt=*on
034300070222     C                   Z-ADD     PiVal         VABCCM
034400070222     C                   Z-ADD     PiVal         VATCCM
034500070222     C                   ENDIF
034600070222     C                   ENDIF
034700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABLNP
035200070222     C                   Z-ADD     PiVal         VATLNP
035300070222     C                   ENDIF
035400070222     C                   ENDIF
035500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABCTR
036000070222     C                   ENDIF
036100070928     C                   ENDIF
036200071009     C                   MOVEL     *blanks       wDISK             1
036300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036400071009     C                   IF        wDISK <> *blanks
036500070928     C                   SETON                                        50
036600070222     C                   ENDIF
036700100525     C                   IF        wDISK  = 'T'
036800100525     C                   SETOFF                                       50
036900100525     C                   SETON                                        51
037000100525     C                   ENDIF
037100100401     C                   MOVEL     *blanks       wDUPREC           1
037200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
037300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
037400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
037500100525     C                   ENDIF
037600111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
037700111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
037800100525     C*
037900070222     C                   ENDIF
038000071009     C*
038100071009     C   50              EVAL      VABCTM = '7Q'
038200000801     C*
038300000801     C                   ENDSR
038400091223     C*----------------------------------------------------*
038500091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038600091223     C*----------------------------------------------------*
038700091223     C     REPNSP        BEGSR
038800091223     C*
038900091223     C                   EXSR      INZVAR
039000091223     C                   EXSR      DEFCAM
039100100928     C*
039200100928     C                   SETON                                        60
039300091223     C*
039400091223     C* NSP => Stacco un numeratore da AZNUM
039500091223     C                   clear                   TRUL33DS
039600091223     C                   eval      I33OPE = *zeros
039700091223     C                   eval      I33CNU = 302
039800091223     C                   eval      I33NUM = 1
039900091223     C                   movel     TRUL33DS      KPJBU
040000091223     C                   call      'TRUL33R'
040100091223     C                   parm                    KPJBA
040200091223     C                   movel     KPJBU         TRUL33DS
040300091223     C                   if        O33ERR = *zeros
040400091223     C                   z-add     O33NRF        VABNSP
040500091223     C                   z-add     O33NRF        VATNSP
040600091223     C                   else
040700091223     C                   SETON                                        31
040800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040900091223     C                             + ' ' + 'VABNSP VATNSP'
041000091223     C                   endif
041100091223     C*
041200091223     C                   ENDSR
041300111031     C*----------------------------------------------------*
041400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041500111031     C*----------------------------------------------------*
041600111031     C     IMPVAB        BEGSR
041700111031     C*
041800111031     C                   Z-ADD     *zeros        errore            1 0
041900111031     C                   MOVEL     datcor        VABAAS
042000111031     C                   MOVEL     datcor        VATAAS
042100111031     C                   MOVE      datcor        VABMGS
042200111031     C                   MOVE(P)   vlrpoi        VABFGS
042300111031     C                   MOVE(P)   vlrpoi        VATFGS
042400111031     C*
042500120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042600111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042700111031     C     '@':'A'       XLATE     VABRSD        VABRSD
042800111031     C* ==
042900111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
043000140325     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:40)))
043100140325     C***                EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043200140325     C***                                 %trim(%subst(vindta:341:30))+' '+
043300140325     C***                                 %trim(%subst(vindta:371:5)))
043400111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043500111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043600140325     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
043700140325     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
043800130212     C***                EVAL      VABRMA=%trim(%subst(vindta:376:15))
043900111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
044000111031     C                   IF        %subst(vindta:43:1) = 'N'
044100111031     C                   EVAL      VABCBO='1'
044200111031     C                   ENDIF
044300111031     C                   IF        %subst(vindta:43:1) = 'P'
044400111031     C                   EVAL      VABCBO='2'
044500111031     C                   ENDIF
044600140325     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
044700140325     C                   EVAL      VABNZD=%trim(%subst(vindta:615:3))
044800111031     C* CAP
044900140325     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
045000111031     C                   EXSR      CHKNUM
045100111031     C                   IF        PiInt=*on
045200111031     C                   Z-ADD     PiVal         Num5_0
045300111031     C                   MOVEL(P)  Num5_0        VABCAD
045400111031     C                   ELSE
045500111031     C                   ADD       1             errore
045600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045700111031     C                             + ' ' + 'VABCAD'
045800111031     C                   ENDIF
045900111031     C* Reperisco la provincia dal CAP e dalla località
046000111031     C                   IF        VABCAD <> *blanks AND
046100111031     C                             VABPRD  = *blanks
046200111031     C                   CLEAR                   TISI95DS
046300111031     C                   EVAL      I95TCN = '3'
046400111031     C                   Z-ADD     datcor        I95DAT
046500111031     C                   EVAL      I95CAP = VABCAD
046600111031     C                   EVAL      I95LOC = VABLOD
046700111031     C                   EVAL      I95NAR = VABNZD
046800111031     C                   CALL      'TISI95R'
046900111031     C                   PARM                    TISI95DS
047000111031     C                   EVAL      VABPRD = O95PRV
047100111031     C                   ENDIF
047200111031     C* NCL
047300111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
047400111031     C                   EXSR      CHKNUM
047500111031     C                   IF        PiInt=*on
047600111031     C                   SELECT
047700111031     C                   WHEN      wNCL = *blanks
047800111031     C                   Z-ADD     PiVal         VABNCL
047900111031     C                   WHEN      wNCL = 'A'
048000111031     C                   ADD       PiVal         VABNCL
048100111031     C                   WHEN      wNCL = '1'
048200111031     C                   ADD       1             VABNCL
048300111031     C                   ENDSL
048400111031     C                   ELSE
048500111031     C                   ADD       1             errore
048600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700111031     C                             + ' ' + 'VABNCL'
048800111031     C                   ENDIF
048900111031     C* PKB
049000120403     C* il peso è in grammi
049100111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
049200111031     C                   EXSR      CHKNUM
049300111031     C                   IF        PiNum=*on
049400120403     C                   EVAL      PiVal=PiVal/1000
049500111031     C                   Z-ADD     PiVal         VABPKB
049600111031     C                   ELSE
049700111031     C                   ADD       1             errore
049800111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049900111031     C                             + ' ' + 'VABPKB'
050000111031     C                   ENDIF
050100111031     C* RMN
050200120403     C*** elenco delle posizioni utilizzate nel tempo
050300130212     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
050400121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
050500120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
050600120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
050700111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
050800111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
050900111031     C                   EXSR      CHKNUM
051000111031     C                   IF        PiInt=*on
051100111031     C                   Z-ADD     PiVal         VABRMN
051200111031     C                   ELSE
051300111031     C                   ADD       1             errore
051400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051500111031     C                             + ' ' + 'VABRMN'
051600111031     C                   ENDIF
051700111031     C* CAS
051800111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
051900130212     C                             %trim(%subst(vindta:502:9))<>'0000000.0' AND
052000111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
052100111031     C                             %trim(%subst(vindta:502:9))<>*zeros
052200111031     C                   EVAL      VABVCA = 'EUR'
052300111031     C                   EVAL      FlgCAS = '1'
052400111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
052500111031     C                   EXSR      CHKNUM
052600111031     C                   IF        PiNum=*on
052700111031     C                   Z-ADD     PiVal         VABCAS
052800111031     C                   ELSE
052900111031     C                   ADD       1             errore
053000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053100111031     C                             + ' ' + 'VABCAS'
053200111031     C                   ENDIF
053300111031     C                   ENDIF
053400111031     C* IAS
053500140325     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks and
053600140325     C                             %trim(%subst(vindta:544:10)) <> *zeros
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
063400140325     C                   EVAL      VATTRC='A'
063500140325     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
063600140325     C                   exsr      wrivat                                       => scarico VAT
063700111031     C*
063800111031     C                   ENDSR
063900111031     C*----------------------------------------------------*
064000111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
064100111031     C*----------------------------------------------------*
064200111031     C     EXEVATB       BEGSR
064300111031     C*
064400130212     C                   EVAL      VATTRC='B'
064500130212     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
064600130212     C                   exsr      wrivat                                       => scarico VAT
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
065900120402     C*----------------------------------------------------*
066000120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
066100140325     C* non valorizzati
066200140325     C*----------------------------------------------------*
066300120402     C     EXEVATI       BEGSR
066400120402     C*
066500120402     C                   EVAL      VATTRC='I'
066600120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
066700120402     C                   exsr      wrivat                                       => scarico VAT
066800120402     C*
066900120402     C                   EVAL      VATTRC='J'
067000120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
067100120402     C                   exsr      wrivat                                       => scarico VAT
067200120402     C*
067300120402     C                   ENDSR
067400111031     C*----------------------------------------------------*
067500111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
067600111031     C*----------------------------------------------------*
067700111031     C     WRIVAT        BEGSR
067800111031     C*
067900111031     C* Scrivo solo se valorizzato qualcosa
068000111031     C                   IF        VATNOT <> *blanks
068100111031     C                   WRITE     FIVAT000
068200111031     C                   ENDIF
068300111031     C*
068400111031     C                   ENDSR
068500020904
068600020904
068700010202     C*----------------------------------------------------*
068800100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
068900010202     C*----------------------------------------------------*
069000100525     C     PREVASX       BEGSR
069100100525     C*
069200021113     C* Compongo il nome del membro da dare al FIVATWWR
069300010202     C                   eval      parmbr = vlrhdl
069400010202     C                   movel     'M'           parmbr
069500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069600010202     C                   eval      paropz = '1'
069700100525     C*
069800010202     C* Effettuo la chiamata al CLLE preposto
069900100525     C  N51              call(e)   'TITVVTC'
070000010202     C                   parm                    parccm
070100010202     C                   parm                    parmbr
070200010202     C                   parm                    paropz
070300100525     C   51              call(e)   'TITVVBC'
070400100525     C                   parm                    parccm
070500100525     C                   parm                    parmbr
070600100525     C                   parm                    paropz
070700100525     C*
070800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070900010202     C                   if        %error
071000010202     C                   movel     '1'           chkcall
071100010202     C                   else
071200010202     C                   movel     '0'           chkcall
071300010202     C                   endif
071400010202     C*
071500010202     C                   ENDSR
071600000801     C*----------------------------------------------------*
071700000801     C*  CONTROLLO NUMERICITA' CAMPI
071800000801     C*----------------------------------------------------*
071900000801     C     CHKNUM        BEGSR
072000081113     C*
072100081113     C                   IF        PiDecChr = *blanks
072200110408     C                   EVAL      PiDecChr = '.'
072300081113     C                   ENDIF
072400091223     C*
072500081113     C                   callp(e)  UBISNUM_Check(PiStr
072600081113     C                                          :PiDecChr
072700081113     C                                          :PiVal
072800081113     C                                          :PiNum
072900081113     C                                          :PiInt)
073000081113     C*
073100000801     C                   IF        %error
073200000801     C                   EVAL      PiInt=*off
073300000801     C                   ENDIF
073400000801     C*
073500000801     C                   ENDSR
073600000801     C***
073700011113
073800011113
073900000801
074000000801
074100990920      /TITLE Invio dei dati al punto operativo.
074200010202     C     invio         BEGSR
074300990920     C*
074400021113     C* 1° invio FIVAT
074500010201     C                   reset                   dscmz
074600110411     C                   move      vatfgs        cmzdst
074700021113     C                   eval      cmzfld = 'FIVATWWR'
074800010201     C                   eval      cmzmbd = vlrhdl
074900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075000021009     C***                if        prmfir = *blanks
075100021113     C                   eval      cmzfla = 'FIVAT00F'
075200021113     C                   eval      cmzmba = 'FIVAT00F'
075300021009     C***                else
075400021009     C***                eval      cmzfla = prmfir
075500021009     C***                eval      cmzmba = prmfir
075600021009     C***                endif
075700010201     C                   eval      cmznrr = *zeros
075800020305     C                   move      §ctrokvt      cmznrr
075900021018     C                   eval      cmzlba = vlrfl1
076000010201     C                   call(e)   'TIS711C'
076100010201     C                   parm                    dscmz
076200010201     C                   parm      *blanks       esito
076300010205     C                   if        %error
076400010205     C                             or cmzerr = '1'
076500010205     C                             or esito  = '1'
076600010205     C                   eval      wrkesito = '3'
076700010205     C                   else
076800010201     C*
076900021113     C* 2° invio FIVAB
077000100525     C                   if        *in51 = *off
077100010201     C                   reset                   dscmz
077200110411     C                   move      vabfgs        cmzdst
077300010201     C                   eval      cmzfld = vlrfou
077400010201     C                   eval      cmzmbd = vlrhdl
077500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077600021009     C***                if        prmfir = *blanks
077700021113     C                   eval      cmzfla = 'FIVAB00F'
077800021113     C                   eval      cmzmba = 'FIVAB00F'
077900021009     C***                else
078000021009     C***                eval      cmzfla = prmfir
078100021009     C***                eval      cmzmba = prmfir
078200021009     C***                endif
078300010201     C                   eval      cmznrr = *zeros
078400010201     C                   move      §ctrokvb      cmznrr
078500021018     C                   eval      cmzlba = vlrfl1
078600010201     C                   call(e)   'TIS711C'
078700010201     C                   parm                    dscmz
078800010201     C                   parm      *blanks       esito
078900010201     C                   if        %error
079000010201     C                             or cmzerr = '1'
079100010201     C                             or esito  = '1'
079200010201     C                   eval      wrkesito = '3'
079300010201     C                   endif
079400100525     C                   endif
079500010205     C                   endif
079600990920     C*
079700000613     C                   ENDSR
079800000613     C***
079900070411
080000110627     C     *pssr         BEGSR
080100070411     C*
080200110627     C                   if        %open(tivin00r)
080300110627     C                   close     tivin00r
080400110627     C                   endif
080500110627     C                   if        %open(fivabwwr)
080600110627     C                   close     fivabwwr
080700110627     C                   endif
080800110627     C                   if        %open(fivatwwr)
080900110627     C                   close     fivatwwr
081000110627     C                   endif
081100110627     C*
081200110627     C* Effettuo la chiamata al CLLE preposto
081300110627     C  N51              call(e)   'TITVVTC'
081400110627     C                   parm                    parccm
081500110627     C                   parm                    parmbr
081600110627     C                   parm      '2'           paropz
081700110627     C   51              call(e)   'TITVVBC'
081800110627     C                   parm                    parccm
081900110627     C                   parm                    parmbr
082000110627     C                   parm      '2'           paropz
082100110627     C*
082200110627     C                   eval      wrkesito = '2'
082300110627     C*
082400110627     C                   seton                                        LR
082500110627     C*
082600110627     C                   ENDSR     '*CANCL'
082700070411     C***
082800070411
082900990910
083000000613     C     *inzsr        BEGSR
083100990910     C*
083200990910     C     *entry        plist
083300990920     C                   parm                    tivlrds
083400990921     C                   parm      wrkesito      esito
083500000724     C                   parm                    prmlit
083600000710     C                   parm                    prmfir
083700000613     C*
083800000830     C* CALCOLA LA DATA CORRENTE
083900091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084000091223     C                   eval      datcor = %dec(%date() : *ISO)
084100000830     C*
084200000613     C                   ENDSR
084300000613     C***
