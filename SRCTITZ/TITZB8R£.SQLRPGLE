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
032900130212     C                   EVAL      VABCCM = 0481746
033000130212     C                   EVAL      VATCCM = 0481746
033100130212     C                   EVAL      VABLNP = 048
033200130212     C                   EVAL      VATLNP = 048
033300120910     C                   EVAL      VABCTR = 000
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
037500111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
037600111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
037700100525     C*
037800070222     C                   ENDIF
037900071009     C*
038000071009     C   50              EVAL      VABCTM = '7Q'
038100000801     C*
038200000801     C                   ENDSR
038300091223     C*----------------------------------------------------*
038400091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
038500091223     C*----------------------------------------------------*
038600091223     C     REPNSP        BEGSR
038700091223     C*
038800091223     C                   EXSR      INZVAR
038900091223     C                   EXSR      DEFCAM
039000100928     C*
039100100928     C                   SETON                                        60
039200091223     C*
039300091223     C* NSP => Stacco un numeratore da AZNUM
039400091223     C                   clear                   TRUL33DS
039500091223     C                   eval      I33OPE = *zeros
039600091223     C                   eval      I33CNU = 302
039700091223     C                   eval      I33NUM = 1
039800091223     C                   movel     TRUL33DS      KPJBU
039900091223     C                   call      'TRUL33R'
040000091223     C                   parm                    KPJBA
040100091223     C                   movel     KPJBU         TRUL33DS
040200091223     C                   if        O33ERR = *zeros
040300091223     C                   z-add     O33NRF        VABNSP
040400091223     C                   z-add     O33NRF        VATNSP
040500091223     C                   else
040600091223     C                   SETON                                        31
040700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800091223     C                             + ' ' + 'VABNSP VATNSP'
040900091223     C                   endif
041000091223     C*
041100091223     C                   ENDSR
041200111031     C*----------------------------------------------------*
041300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041400111031     C*----------------------------------------------------*
041500111031     C     IMPVAB        BEGSR
041600111031     C*
041700111031     C                   Z-ADD     *zeros        errore            1 0
041800111031     C                   MOVEL     datcor        VABAAS
041900111031     C                   MOVEL     datcor        VATAAS
042000111031     C                   MOVE      datcor        VABMGS
042100111031     C                   MOVE(P)   vlrpoi        VABFGS
042200111031     C                   MOVE(P)   vlrpoi        VATFGS
042300111031     C*
042400120402     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
042500111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042600111031     C     '@':'A'       XLATE     VABRSD        VABRSD
042700111031     C* ==
042800111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
042900111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
043000111031     C                                    %trim(%subst(vindta:341:30))+' '+
043100111031     C                                    %trim(%subst(vindta:371:5)))
043200111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
043300111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
043400130212     C                   EVAL      VABRMA=%trim(%subst(vindta:46:10))
043500121217     C***                EVAL      VABRMA=%trim(%subst(vindta:46:25))
043600130212     C***                EVAL      VABRMA=%trim(%subst(vindta:376:15))
043700111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
043800111031     C                   IF        %subst(vindta:43:1) = 'N'
043900111031     C                   EVAL      VABCBO='1'
044000111031     C                   ENDIF
044100111031     C                   IF        %subst(vindta:43:1) = 'P'
044200111031     C                   EVAL      VABCBO='2'
044300111031     C                   ENDIF
044400130212     C                   IF        %subst(vindta:511:3) = 'ABM'
044500130212     C                   EVAL      VABTIC='BM'
044600130212     C                   ENDIF
044700130212     C                   IF        %subst(vindta:511:3) = 'ABS'
044800130212     C                   EVAL      VABTIC='B'
044900130212     C                   ENDIF
045000130212     C                   IF        %subst(vindta:511:3) = 'CON'
045100130212     C                   EVAL      VABTIC=*blanks
045200130212     C                   ENDIF
045300130212     C                   IF        %subst(vindta:511:3) = 'ACM'
045400130212     C                   EVAL      VABTIC='CM'
045500130212     C                   ENDIF
045600130212     C                   IF        %subst(vindta:511:3) = 'ACS'
045700130212     C                   EVAL      VABTIC='CB'
045800130212     C                   ENDIF
045900130212     C                   IF        %subst(vindta:511:3) = 'VAR'
046000130212     C                   EVAL      VABTIC=*blanks
046100130212     C                   ENDIF
046200111031     C* CAP
046300111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
046400111031     C                   EXSR      CHKNUM
046500111031     C                   IF        PiInt=*on
046600111031     C                   Z-ADD     PiVal         Num5_0
046700111031     C                   MOVEL(P)  Num5_0        VABCAD
046800111031     C                   ELSE
046900111031     C                   ADD       1             errore
047000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100111031     C                             + ' ' + 'VABCAD'
047200111031     C                   ENDIF
047300111031     C* Reperisco la provincia dal CAP e dalla località
047400111031     C                   IF        VABCAD <> *blanks AND
047500111031     C                             VABPRD  = *blanks
047600111031     C                   CLEAR                   TISI95DS
047700111031     C                   EVAL      I95TCN = '3'
047800111031     C                   Z-ADD     datcor        I95DAT
047900111031     C                   EVAL      I95CAP = VABCAD
048000111031     C                   EVAL      I95LOC = VABLOD
048100111031     C                   EVAL      I95NAR = VABNZD
048200111031     C                   CALL      'TISI95R'
048300111031     C                   PARM                    TISI95DS
048400111031     C                   EVAL      VABPRD = O95PRV
048500111031     C                   ENDIF
048600111031     C* NCL
048700111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
048800111031     C                   EXSR      CHKNUM
048900111031     C                   IF        PiInt=*on
049000111031     C                   SELECT
049100111031     C                   WHEN      wNCL = *blanks
049200111031     C                   Z-ADD     PiVal         VABNCL
049300111031     C                   WHEN      wNCL = 'A'
049400111031     C                   ADD       PiVal         VABNCL
049500111031     C                   WHEN      wNCL = '1'
049600111031     C                   ADD       1             VABNCL
049700111031     C                   ENDSL
049800111031     C                   ELSE
049900111031     C                   ADD       1             errore
050000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050100111031     C                             + ' ' + 'VABNCL'
050200111031     C                   ENDIF
050300111031     C* PKB
050400120403     C* il peso è in grammi
050500111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
050600111031     C                   EXSR      CHKNUM
050700111031     C                   IF        PiNum=*on
050800120403     C                   EVAL      PiVal=PiVal/1000
050900111031     C                   Z-ADD     PiVal         VABPKB
051000130128     C* se il peso risultante è 0
051100130128     C                   IF        VABPKB = 0
051200130128     C* lo imposto a 0,1
051300130128     C                   EVAL      VABPKB = 0,1
051400130128     C                   ENDIF
051500111031     C                   ELSE
051600111031     C                   ADD       1             errore
051700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800111031     C                             + ' ' + 'VABPKB'
051900111031     C                   ENDIF
052000111031     C* RMN
052100120403     C*** elenco delle posizioni utilizzate nel tempo
052200130212     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
052300121109     C***                EVAL      PiStr=%trim(%subst(vindta:46+10:15))
052400120402     C***                EVAL      PiStr=%trim(%subst(vindta:554:6))
052500120911     C***                EVAL      PiStr=%trim(%subst(vindta:555:7))
052600111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
052700111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
052800111031     C                   EXSR      CHKNUM
052900111031     C                   IF        PiInt=*on
053000111031     C                   Z-ADD     PiVal         VABRMN
053100111031     C                   ELSE
053200111031     C                   ADD       1             errore
053300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053400111031     C                             + ' ' + 'VABRMN'
053500111031     C                   ENDIF
053600111031     C* CAS
053700111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
053800130212     C                             %trim(%subst(vindta:502:9))<>'0000000.0' AND
053900111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
054000111031     C                             %trim(%subst(vindta:502:9))<>*zeros
054100111031     C                   EVAL      VABVCA = 'EUR'
054200111031     C                   EVAL      FlgCAS = '1'
054300111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
054400111031     C                   EXSR      CHKNUM
054500111031     C                   IF        PiNum=*on
054600111031     C                   Z-ADD     PiVal         VABCAS
054700111031     C                   ELSE
054800111031     C                   ADD       1             errore
054900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055000111031     C                             + ' ' + 'VABCAS'
055100111031     C                   ENDIF
055200111031     C                   ENDIF
055300111031     C* IAS
055400111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
055500111031     C                   EVAL      VABVAS = 'EUR'
055600111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
055700111031     C                   EXSR      CHKNUM
055800111031     C                   IF        PiNum=*on
055900111031     C                   Z-ADD     PiVal         VABIAS
056000111031     C                   ELSE
056100111031     C                   ADD       1             errore
056200111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056300111031     C                             + ' ' + 'VABIAS'
056400111031     C                   ENDIF
056500111031     C                   ENDIF
056600111031     C*
056700111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
056800111031     C                   IF        FlgCAS <> '0'
056900111031     C                   IF        VABCBO = '1'
057000111031     C                   EVAL      VABCBO = '4'
057100111031     C                   ELSE
057200111031     C                   EVAL      VABCBO = '6'
057300111031     C                   ENDIF
057400111031     C                   ENDIF
057500111031     C*
057600111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057700111031     C                   EXSR      CHKIMPDIV
057800111031     C*
057900111031     C* Ebbene...
058000111031     C                   ADD       1             §CTRMO
058100111031     C                   IF        errore <> *zeros
058200111031     C                   ADD       1             §CTRNO
058300111031     C                   EVAL      recko = vindta
058400111031     C                   EVAL      vinflg = '2'
058500111031     C                   ELSE
058600111031     C                   ADD       1             §CTROKVB
058700111031     C                   ENDIF
058800111031     C*
058900111031     C                   ENDSR
059000111031
059100111031
059200111031     C*----------------------------------------------------*
059300111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059400111031     C*----------------------------------------------------*
059500111031     C     CHKIMPDIV     BEGSR
059600111031     C*
059700111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059800111031     C                   Z-ADD     *zeros        wrkDec            9 9
059900111031     C*
060000111031     C* Come prima cosa effettuo considerazioni sulla divisa
060100111031     C                   IF        vabIAS > *zeros
060200111031     C                   IF        vabVAS <> 'EUR'
060300111031     C                   EVAL      vabVAS =  'ITL'
060400111031     C                   ENDIF
060500111031     C                   ENDIF
060600111031     C*
060700111031     C                   IF        vabCAS > *zeros
060800111031     C                   IF        vabVCA <> 'EUR'
060900111031     C                   EVAL      vabVCA =  'ITL'
061000111031     C                   ENDIF
061100111031     C                   ENDIF
061200111031     C*
061300111031     C                   IF        vabVMD > *zeros
061400111031     C                   IF        vabVAD <> 'EUR'
061500111031     C                   EVAL      vabVAD =  'ITL'
061600111031     C                   ENDIF
061700111031     C                   ENDIF
061800111031     C*
061900111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062000111031     C                   Z-ADD     vabIAS        wrkDec
062100111031     C                   IF        wrkDec > *zeros
062200111031     C                   IF        vabVAS = 'ITL'
062300111031     C                   EVAL      vabIAS = *zeros
062400111031     C                   ENDIF
062500111031     C                   ENDIF
062600111031     C*
062700111031     C* Stabilisco se il contrasegno ha decimali valorizzati
062800111031     C                   Z-ADD     vabCAS        wrkDec
062900111031     C                   IF        wrkDec > *zeros
063000111031     C                   IF        vabVCA = 'ITL'
063100111031     C                   EVAL      vabCAS = *zeros
063200111031     C                   ENDIF
063300111031     C                   ENDIF
063400111031     C*
063500111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063600111031     C                   Z-ADD     vabVMD        wrkDec
063700111031     C                   IF        wrkDec > *zeros
063800111031     C                   IF        vabVAD = 'ITL'
063900111031     C                   EVAL      vabVMD = *zeros
064000111031     C                   ENDIF
064100111031     C                   ENDIF
064200111031     C*
064300111031     C                   ENDSR
064400111031     C***
064500111031
064600111031
064700111031     C*----------------------------------------------------*
064800111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
064900111031     C*----------------------------------------------------*
065000111031     C     EXEVATA       BEGSR
065100111031     C*
065200111031     C***                EVAL      VATTRC='A'
065300111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
065400111031     C***                exsr      wrivat                                       => scarico VAT
065500111031     C*
065600111031     C                   ENDSR
065700111031     C*----------------------------------------------------*
065800111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
065900111031     C*----------------------------------------------------*
066000111031     C     EXEVATB       BEGSR
066100111031     C*
066200130212     C                   EVAL      VATTRC='B'
066300130212     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
066400130212     C                   exsr      wrivat                                       => scarico VAT
066500111031     C*
066600111031     C                   ENDSR
066700111031     C*----------------------------------------------------*
066800111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
066900111031     C*----------------------------------------------------*
067000111031     C     EXEVATE       BEGSR
067100111031     C*
067200111031     C                   EVAL      VATTRC='E'
067300111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
067400111031     C                   exsr      wrivat                                       => scarico VAT
067500111031     C*
067600111031     C                   ENDSR
067700120402     C*----------------------------------------------------*
067800120402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" E "J"
067900120402     C*----------------------------------------------------*
068000120402     C     EXEVATI       BEGSR
068100120402     C*
068200120402     C                   EVAL      VATTRC='I'
068300120402     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
068400120402     C                   exsr      wrivat                                       => scarico VAT
068500120402     C*
068600120402     C                   EVAL      VATTRC='J'
068700120403     C                   EVAL      VATNOT = %trim(%subst(vindta:698:15))
068800120402     C                   exsr      wrivat                                       => scarico VAT
068900120402     C*
069000120402     C                   ENDSR
069100111031     C*----------------------------------------------------*
069200111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
069300111031     C*----------------------------------------------------*
069400111031     C     WRIVAT        BEGSR
069500111031     C*
069600111031     C* Scrivo solo se valorizzato qualcosa
069700111031     C                   IF        VATNOT <> *blanks
069800111031     C                   WRITE     FIVAT000
069900111031     C                   ENDIF
070000111031     C*
070100111031     C                   ENDSR
070200020904
070300020904
070400010202     C*----------------------------------------------------*
070500100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
070600010202     C*----------------------------------------------------*
070700100525     C     PREVASX       BEGSR
070800100525     C*
070900021113     C* Compongo il nome del membro da dare al FIVATWWR
071000010202     C                   eval      parmbr = vlrhdl
071100010202     C                   movel     'M'           parmbr
071200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
071300010202     C                   eval      paropz = '1'
071400100525     C*
071500010202     C* Effettuo la chiamata al CLLE preposto
071600100525     C  N51              call(e)   'TITVVTC'
071700010202     C                   parm                    parccm
071800010202     C                   parm                    parmbr
071900010202     C                   parm                    paropz
072000100525     C   51              call(e)   'TITVVBC'
072100100525     C                   parm                    parccm
072200100525     C                   parm                    parmbr
072300100525     C                   parm                    paropz
072400100525     C*
072500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072600010202     C                   if        %error
072700010202     C                   movel     '1'           chkcall
072800010202     C                   else
072900010202     C                   movel     '0'           chkcall
073000010202     C                   endif
073100010202     C*
073200010202     C                   ENDSR
073300000801     C*----------------------------------------------------*
073400000801     C*  CONTROLLO NUMERICITA' CAMPI
073500000801     C*----------------------------------------------------*
073600000801     C     CHKNUM        BEGSR
073700081113     C*
073800081113     C                   IF        PiDecChr = *blanks
073900110408     C                   EVAL      PiDecChr = '.'
074000081113     C                   ENDIF
074100091223     C*
074200081113     C                   callp(e)  UBISNUM_Check(PiStr
074300081113     C                                          :PiDecChr
074400081113     C                                          :PiVal
074500081113     C                                          :PiNum
074600081113     C                                          :PiInt)
074700081113     C*
074800000801     C                   IF        %error
074900000801     C                   EVAL      PiInt=*off
075000000801     C                   ENDIF
075100000801     C*
075200000801     C                   ENDSR
075300000801     C***
075400011113
075500011113
075600000801
075700000801
075800990920      /TITLE Invio dei dati al punto operativo.
075900010202     C     invio         BEGSR
076000990920     C*
076100021113     C* 1° invio FIVAT
076200010201     C                   reset                   dscmz
076300110411     C                   move      vatfgs        cmzdst
076400021113     C                   eval      cmzfld = 'FIVATWWR'
076500010201     C                   eval      cmzmbd = vlrhdl
076600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076700021009     C***                if        prmfir = *blanks
076800021113     C                   eval      cmzfla = 'FIVAT00F'
076900021113     C                   eval      cmzmba = 'FIVAT00F'
077000021009     C***                else
077100021009     C***                eval      cmzfla = prmfir
077200021009     C***                eval      cmzmba = prmfir
077300021009     C***                endif
077400010201     C                   eval      cmznrr = *zeros
077500020305     C                   move      §ctrokvt      cmznrr
077600021018     C                   eval      cmzlba = vlrfl1
077700010201     C                   call(e)   'TIS711C'
077800010201     C                   parm                    dscmz
077900010201     C                   parm      *blanks       esito
078000010205     C                   if        %error
078100010205     C                             or cmzerr = '1'
078200010205     C                             or esito  = '1'
078300010205     C                   eval      wrkesito = '3'
078400010205     C                   else
078500010201     C*
078600021113     C* 2° invio FIVAB
078700100525     C                   if        *in51 = *off
078800010201     C                   reset                   dscmz
078900110411     C                   move      vabfgs        cmzdst
079000010201     C                   eval      cmzfld = vlrfou
079100010201     C                   eval      cmzmbd = vlrhdl
079200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079300021009     C***                if        prmfir = *blanks
079400021113     C                   eval      cmzfla = 'FIVAB00F'
079500021113     C                   eval      cmzmba = 'FIVAB00F'
079600021009     C***                else
079700021009     C***                eval      cmzfla = prmfir
079800021009     C***                eval      cmzmba = prmfir
079900021009     C***                endif
080000010201     C                   eval      cmznrr = *zeros
080100010201     C                   move      §ctrokvb      cmznrr
080200021018     C                   eval      cmzlba = vlrfl1
080300010201     C                   call(e)   'TIS711C'
080400010201     C                   parm                    dscmz
080500010201     C                   parm      *blanks       esito
080600010201     C                   if        %error
080700010201     C                             or cmzerr = '1'
080800010201     C                             or esito  = '1'
080900010201     C                   eval      wrkesito = '3'
081000010201     C                   endif
081100100525     C                   endif
081200010205     C                   endif
081300990920     C*
081400000613     C                   ENDSR
081500000613     C***
081600070411
081700110627     C     *pssr         BEGSR
081800070411     C*
081900110627     C                   if        %open(tivin00r)
082000110627     C                   close     tivin00r
082100110627     C                   endif
082200110627     C                   if        %open(fivabwwr)
082300110627     C                   close     fivabwwr
082400110627     C                   endif
082500110627     C                   if        %open(fivatwwr)
082600110627     C                   close     fivatwwr
082700110627     C                   endif
082800110627     C*
082900110627     C* Effettuo la chiamata al CLLE preposto
083000110627     C  N51              call(e)   'TITVVTC'
083100110627     C                   parm                    parccm
083200110627     C                   parm                    parmbr
083300110627     C                   parm      '2'           paropz
083400110627     C   51              call(e)   'TITVVBC'
083500110627     C                   parm                    parccm
083600110627     C                   parm                    parmbr
083700110627     C                   parm      '2'           paropz
083800110627     C*
083900110627     C                   eval      wrkesito = '2'
084000110627     C*
084100110627     C                   seton                                        LR
084200110627     C*
084300110627     C                   ENDSR     '*CANCL'
084400070411     C***
084500070411
084600990910
084700000613     C     *inzsr        BEGSR
084800990910     C*
084900990910     C     *entry        plist
085000990920     C                   parm                    tivlrds
085100990921     C                   parm      wrkesito      esito
085200000724     C                   parm                    prmlit
085300000710     C                   parm                    prmfir
085400000613     C*
085500000830     C* CALCOLA LA DATA CORRENTE
085600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085700091223     C                   eval      datcor = %dec(%date() : *ISO)
085800000830     C*
085900000613     C                   ENDSR
086000000613     C***
