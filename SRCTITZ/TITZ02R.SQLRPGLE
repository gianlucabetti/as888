000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021113     FFIVABwwr  O    E             DISK    usropn
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
002100111205     D depspe          s              6    INZ(*blanks)
002200111205     D curspe          s              6    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800070530     D Num5_0          s              5  0
002900000830
003000020725
003100020725     D*------------------
003200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300020725     D*------------------
003400070530     D INPUT_DS        DS                  INZ
003500101007     D  VINDTA                     2048
003600101007     D  VINFLG                        1
003700101007     D  VINSPE                       10
003800101007     D  VINRRN                        8  0
003900020725     D*
004000080923     D*------------------
004100080923     D* DS REPERIMENTO NUMERATORE
004200080923     D*------------------
004300080923     D trul33ds      e ds                  inz
004400080923     D*------------------
004500080923     D* DS ARCHITETTURA
004600080923     D*------------------
004700080923     D kpjba         e ds                  inz
004800080923     D*------------------
004900111205
005000111205
005100111205     D*-------------------
005200111205     D* COSTANTI
005300111205     D*-------------------
005400111205     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005500111205     D                                     èéù')
005600111205     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005700111205     D                                     EEU')
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
007900111228     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300050128     C                   call(e)   'TITVVTC'
008400010202     C                   parm                    parccm
008500010202     C                   parm                    parmbr
008600010202     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600990908
009700000801
009800910830     C*--------------------------------------------------------
009900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600021113     C                   if        not %open(fivabwwr)
010700021113     C                   open      fivabwwr
010800990914     C                   endif
010900070530     C*
011000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011100020305     C                   exsr      prevat
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500021113     C                   if        not %open(fivatwwr)
011600021113     C                   open      fivatwwr
011700010201     C                   endif
011800080117     C*
011900080117     C                   EXSR      INZVAR
012000080117     C                   EXSR      DEFCAM
012100990910     C*
012200010201     C                   clear                   §CTROKVB          5 0
012300020305     C                   clear                   §CTROKVT          5 0
012400000801     C                   clear                   §CTRMO            5 0
012500000801     C                   clear                   §CTRNO            5 0
012600990910     C*
012700020725     C/EXEC SQL
012800020725     C+ declare C1 cursor for select
012900111205     C+ vindta, vinflg, substr(vindta, 143, 6) as sped, rrn(tivin00r)
013000060223     C+ from tivin00r
013100060223     C+ order by sped
013200020725     C+ for read only
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600020725     C+ open C1
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000070530     C+ Fetch C1 into :INPUT_DS
014100020725     C/END-EXEC
014200020725     C*
014300020725     C                   dow       sqlcod = *zeros
014400990913     C*
014500020725     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700000801     C*
014800020725     C                   if        vinflg = *blanks
014900020725     C                             or vinflg = '0'
015000020725     C                             or vinflg = '2'
015100000801     C*
015200020725     C                   clear                   x_vinmsg
015300020725     C                   eval      x_vinflg = '1'
015400111205     C*
015500111205     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015600111205     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
015700010305     C*
015800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015900111205     C                   EVAL      curspe=%trim(%subst(vindta:143:6))
016000010305     C*
016100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016200080923     C                   if        *in50 = *off
016300111228     C                   exsr      inzvar
016400111228     C                   exsr      defcam
016500071003     C                   exsr      impvab
016600071003     C                   exsr      wrivab
016700071003     C                   else
016800080923     C*
016900071009     C                   if        wDISK = 'D'
017000111228     C                   exsr      inzvar
017100111228     C                   exsr      defcam
017200071009     C                   exsr      impvab
017300071009     C                   exsr      wrivab
017400071009     C                   exsr      wrivat_e                                     => carico VAT
017500071009     C                   else
017600071009     C*
017700010305     C                   if        depspe = *blanks                             => 1° giro
017800010305     C                   eval      depspe = curspe                              => memorizz. spediz
017900080117     C                   clear                   tivinds
018000111228     C                   exsr      inzvar
018100111228     C                   exsr      defcam
018200020305     C                   exsr      impvab
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400010305     C                   else
018500020725     C                   if        depspe <> curspe                             => rottura di spediz
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700070928     C                   exsr      wrivab
018800080117     C                   clear                   tivinds
018900111228     C                   exsr      inzvar
019000111228     C                   exsr      defcam
019100020305     C                   exsr      impvab
019200071003     C   50              exsr      wrivat_e                                     => carico VAT
019300020305     C                   else                                                   => x stessa spediz
019400111228     C                   exsr      impvab
019500071003     C   50              exsr      wrivat_e                                     => carico VAT
019600010305     C                   endif
019700010305     C                   endif
019800010305     C                   endif
019900071003     C                   endif
020000071009     C                   endif
020100000905     C*
020200000905     C                   else
020300020725     C                   eval      x_vinflg = '1'
020400000905     C                   endif
020500000905     C*
020600020725     C     VINRRN        chain     tivin000
020700020725     C                   if        %found(tivin00r)
020800020725     C                   eval      y_vinflg = x_vinflg
020900020725     C                   eval      y_vinmsg = x_vinmsg
021000020725     C                   update    tivin000
021100020725     C                   endif
021200020725     C*
021300020725     C/EXEC SQL
021400070530     C+ Fetch C1 into :INPUT_DS
021500020725     C/END-EXEC
021600020725     C*
021700020725     C                   enddo
021800020725     C*
021900020725     C/EXEC SQL
022000020725     C+ close C1
022100020725     C/END-EXEC
022200000905     C*
022300020305     C* Scarico i VAB rimasti "in sospeso"
022400071009     C                   if        wDISK = 'C'
022500071009     C                   exsr      wrivab
022600071009     C                   endif
022700010202     C*
022800010202     C                   endif
022900990910
023000990910     C* Se non ci sono record con errori ...
023100000710     C                   if        §ctrno = 0
023200990910     C* ... restituisco esito OK.
023300990921     C                   eval      wrkesito = '0'
023400990910     C                   else
023500010201     C                   if        §ctrokvb > 0
023600990921     C                   eval      wrkesito = '1'
023700000710     C                   else
023800000710     C                   eval      wrkesito = '2'
023900990910     C                   endif
024000000710     C                   endif
024100990910     C*
024200990914     C                   if        %open(tivin00r)
024300990908     C                   close     tivin00r
024400990914     C                   endif
024500021113     C                   if        %open(fivabwwr)
024600021113     C                   close     fivabwwr
024700990914     C                   endif
024800021113     C                   if        %open(fivatwwr)
024900021113     C                   close     fivatwwr
025000010201     C                   endif
025100990910     C*
025200010201     C                   if        §ctrokvb > 0
025300000724     C                             and vlrpoi <> *zeros
025400010202     C                   exsr      invio
025500990920     C                   endif
025600990920     C*
025700910830     C                   ENDSR
025800000613     C***
025900010305
026000010305     C*----------------------------------------------------*
026100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026200010305     C*----------------------------------------------------*
026300020305     C     WRIVAB        BEGSR
026400070730     C*
026500071003     C* Considerazioni finali
026600071003     C                   if        VABRMA = *blanks
026700071003     C                   movel(P)  VABRMN        VABRMA
026800071003     C                   endif
026900071003     C*
027000021113     C                   write     fivab000                                     => scarico il VAB
027100010305     C*
027200010305     C                   ENDSR
027300990920
027400000801     C*----------------------------------------------------*
027500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027600000801     C*----------------------------------------------------*
027700010201     C     INZVAR        BEGSR
027800000801     C*
027900010201     C                   Z-ADD     *zeros        Num5_0
028000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028100020725     C                   MOVEL     '0'           FlgCAS            1
028200000801     C*
028300000801     C                   ENDSR
028400000801     C*----------------------------------------------------*
028500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028600000801     C*----------------------------------------------------*
028700020904     C     DEFCAM        BEGSR
028800080923     C*
028900080923     C                   SETOFF                                       5051
029000000801     C*
029100021113     C                   CLEAR                   FIVAB000
029200021113     C                   CLEAR                   FIVAT000
029300070730     C* Imposto i valori di default...
029400111205     C                   EVAL      VABCCM = 0411185
029500111205     C                   EVAL      VATCCM = 0411185
029600111205     C                   EVAL      VABLNP = 041
029700111205     C                   EVAL      VATLNP = 041
029800101007     C                   EVAL      VABCTR = 000
029900070730     C                   EVAL      VABCBO = '1'
030000070222     C* ... e poi verifico se sono stati passati come parametri
030100070222     C                   IF        vlrppt > *blanks
030200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABCCM
030700070222     C                   Z-ADD     PiVal         VATCCM
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABLNP
031500070222     C                   Z-ADD     PiVal         VATLNP
031600070222     C                   ENDIF
031700070222     C                   ENDIF
031800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000070222     C                   EXSR      CHKNUM
032100070222     C                   IF        PiInt=*on
032200070222     C                   Z-ADD     PiVal         VABCTR
032300070222     C                   ENDIF
032400070928     C                   ENDIF
032500071009     C                   MOVEL     *blanks       wDISK             1
032600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032700071009     C                   IF        wDISK <> *blanks
032800070928     C                   SETON                                        50
032900070222     C                   ENDIF
033000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033100080923     C                   SETON                                        51
033200080923     C                   ENDIF
033300070222     C                   ENDIF
033400071009     C*
033500071009     C   50              EVAL      VABCTM = '7Q'
033600000801     C*
033700000801     C                   ENDSR
033800000801     C*----------------------------------------------------*
033900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034000000801     C*----------------------------------------------------*
034100010201     C     IMPVAB        BEGSR
034200070530     C*
034300070530     C                   SETOFF                                       3132
034400070928     C*
034500070928     C                   MOVE(P)   vlrpoi        VABFGS
034600070928     C                   MOVE(P)   vlrpoi        VATFGS
034700070928     C*
034800070928     C                   MOVEL     datcor        VABAAS
034900070928     C                   MOVEL     datcor        VATAAS
035000070928     C                   MOVE      datcor        VABMGS
035100111205     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035400070928     C* ==
035500111205     C                   EVAL      VABIND=%trim(%subst(vindta:37:30))
035600111205     C                   EVAL      VABLOD=%trim(%subst(vindta:68:30))
035700111205     C                   EVAL      VABCAD=%trim(%subst(vindta:99:6))
035800111205     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
035900111205     C                   EVAL      VABRMA=%trim(%subst(vindta:113:6))
036000080923     C* RMN
036100111205     C                   EVAL      PiStr=%trim(%subst(vindta:143:6))
036200080923     C                   EXSR      CHKNUM
036300080923     C                   IF        PiInt=*on
036400080923     C                   Z-ADD     PiVal         VABRMN
036500080923     C                   ELSE
036600080923     C                   SETON                                        32
036700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036800080923     C                             + ' ' + 'VABRMN'
036900080923     C                   ENDIF
037000080923     C* NSP
037100080923     C                   IF        *in51 = *on
037200080923     C* NSP => Stacco un numeratore da AZNUM
037300080923     C                   clear                   TRUL33DS
037400080923     C                   eval      I33OPE = *zeros
037500080923     C                   eval      I33CNU = 302
037600080923     C                   eval      I33NUM = 1
037700080923     C                   movel     TRUL33DS      KPJBU
037800080923     C                   call      'TRUL33R'
037900080923     C                   parm                    KPJBA
038000080923     C                   movel     KPJBU         TRUL33DS
038100080923     C                   if        O33ERR = *zeros
038200080923     C                   z-add     O33NRF        VABNSP
038300080923     C                   z-add     O33NRF        VATNSP
038400080923     C                   else
038500080923     C                   SETON                                        31
038600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038700080923     C                             + ' ' + 'VABNSP VATNSP'
038800080923     C                   endif
038900080923     C                   ELSE
039000111205     C                   EVAL      PiStr=%trim(%subst(vindta:143:6))
039100080923     C                   EXSR      CHKNUM
039200080923     C                   IF        PiInt=*on
039300080923     C                   Z-ADD     PiVal         VABNSP
039400080923     C                   Z-ADD     PiVal         VATNSP
039500080923     C                   ELSE
039600080923     C                   SETON                                        31
039700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039800080923     C                             + ' ' + 'VABNSP VATNSP'
039900080923     C                   ENDIF
040000080923     C                   ENDIF
040100071003     C* NCL
040200111205     C                   ADD       1             VABNCL
040300071003     C* PKB
040400111205     C                   EVAL      PiStr=%trim(%subst(vindta:131:11))
040500070928     C                   EXSR      CHKNUM
040600070928     C                   IF        PiNum=*on
040700111205     C                   add(h)    PiVal         VABPKB
040800070928     C                   ELSE
040900070928     C                   SETON                                        32
041000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041100070928     C                             + ' ' + 'VABPKB'
041200070928     C                   ENDIF
041300111205     C* VLB
041400111205     C* espresso in cm cubi con 4 decimali, prendo solo l'intero e divido per 1 milione
041500111205     C                   EVAL      PiStr=%trim(%subst(vindta:184:14))
041600111205     C                   EXSR      CHKNUM
041700111205     C                   IF        PiNum=*on
041800111205     C                   EVAL      PiVal=PiVal/1000000
041900111205     C                   ADD(H)    PiVal         VABVLB
042000111205     C                   ELSE
042100111205     C                   SETON                                        32
042200111205     C                   Z-ADD     *zeros        VABVLB
042300111205     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400111205     C                             + ' ' + 'VABVLB'
042500111205     C                   ENDIF
042600111205     C*
042700111205     C* Se provincia nn valorizzata la reperisco
042800111205     C* tramite TISI95R a seconda dei dati d instradamento presenti
042900111205     C                   IF        VABNZD  = *blanks AND
043000111205     C                             VABPRD  = *blanks AND
043100111205     C                             VABCAD <> *blanks
043200111205     C                   CLEAR                   TISI95DS
043300111205     C                   EVAL      I95TCN = '3'
043400111205     C                   Z-ADD     datcor        I95DAT
043500111205     C                   EVAL      I95NAR = VABNZD
043600111205     C                   EVAL      I95CAP = VABCAD
043700111205     C                   EVAL      I95LOC = VABLOD
043800111205     C                   CALL      'TISI95R'
043900111205     C                   PARM                    TISI95DS
044000111205     C                   EVAL      VABPRD = O95PRV
044100111205     C                   ENDIF
044200070730     C*
044300070730     C* Considerazioni finali su CBO/CAS
044400070730     C                   IF        FlgCAS = '1'
044500070730     C                   IF        VABCBO = '1'
044600070730     C                   EVAL      VABCBO = '4'
044700070730     C                   ENDIF
044800070730     C                   IF        VABCBO = '2'
044900070730     C                   EVAL      VABCBO = '6'
045000070730     C                   ENDIF
045100070730     C                   ENDIF
045200010202     C*
045300000801     C* Ebbene...
045400000801     C                   ADD       1             §CTRMO
045500070530     C                   IF        *in31 <> *zeros OR
045600070530     C                             *in32 <> *zeros
045700000801     C                   ADD       1             §CTRNO
045800020725     C                   EVAL      x_vinflg = '2'
045900000801     C                   ELSE
046000010201     C                   ADD       1             §CTROKVB
046100000801     C                   ENDIF
046200000801     C*
046300000801     C                   ENDSR
046400071003     C*----------------------------------------------------*
046500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
046600071003     C*----------------------------------------------------*
046700071003     C     WRIVAT_E      BEGSR
046800090204     C*
046900090204     C                   eval      vatAAS = vabAAS
047000090204     C                   eval      vatLNP = vabLNP
047100090204     C                   eval      vatNRS = vabNRS
047200090204     C                   eval      vatNSP = vabNSP
047300071003     C*
047400101007     C* Valorizzo il buffer di scrittura del FIVAT
047500071003     C* - TIPO RECORD "E"
047600111205     C                   if        %subst(vindta:150:10) <> *blanks
047700071003     C                   eval      VATTRC = 'E'
047800111205     C                   eval      VATNOT = %trim(%subst(vindta:150:10))
047900071003     C                   write     FIVAT000
048000111205     C                   endif
048100071003     C*
048200071003     C                   ENDSR
048300020904
048400020904
048500010202     C*----------------------------------------------------*
048600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048700010202     C*----------------------------------------------------*
048800020305     C     PREVAT        BEGSR
048900010202     C*
049000021113     C* Compongo il nome del membro da dare al FIVATWWR
049100010202     C                   eval      parmbr = vlrhdl
049200010202     C                   movel     'M'           parmbr
049300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
049400010202     C                   eval      paropz = '1'
049500010202     C* Effettuo la chiamata al CLLE preposto
049600050128     C                   call(e)   'TITVVTC'
049700010202     C                   parm                    parccm
049800010202     C                   parm                    parmbr
049900010202     C                   parm                    paropz
050000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050100010202     C                   if        %error
050200010202     C                   movel     '1'           chkcall
050300010202     C                   else
050400010202     C                   movel     '0'           chkcall
050500010202     C                   endif
050600010202     C*
050700010202     C                   ENDSR
050800000801     C*----------------------------------------------------*
050900000801     C*  CONTROLLO NUMERICITA' CAMPI
051000000801     C*----------------------------------------------------*
051100000801     C     CHKNUM        BEGSR
051200081113     C*
051300081113     C                   IF        PiDecChr = *blanks
051400110223     C                   EVAL      PiDecChr = ','
051500081113     C                   ENDIF
051600081113     C*
051700081113     C                   callp(e)  UBISNUM_Check(PiStr
051800081113     C                                          :PiDecChr
051900081113     C                                          :PiVal
052000081113     C                                          :PiNum
052100081113     C                                          :PiInt)
052200081113     C*
052300000801     C                   IF        %error
052400000801     C                   EVAL      PiInt=*off
052500000801     C                   ENDIF
052600000801     C*
052700000801     C                   ENDSR
052800000801     C***
052900000801
053000011113
053100000801
053200000801
053300990920      /TITLE Invio dei dati al punto operativo.
053400010202     C     invio         BEGSR
053500990920     C*
053600021113     C* 1° invio FIVAT
053700010201     C                   reset                   dscmz
053800010201     C                   move      vlrpoi        cmzdst
053900021113     C                   eval      cmzfld = 'FIVATWWR'
054000010201     C                   eval      cmzmbd = vlrhdl
054100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054200021009     C***                if        prmfir = *blanks
054300021113     C                   eval      cmzfla = 'FIVAT00F'
054400021113     C                   eval      cmzmba = 'FIVAT00F'
054500021009     C***                else
054600021009     C***                eval      cmzfla = prmfir
054700021009     C***                eval      cmzmba = prmfir
054800021009     C***                endif
054900010201     C                   eval      cmznrr = *zeros
055000020305     C                   move      §ctrokvt      cmznrr
055100021018     C                   eval      cmzlba = vlrfl1
055200010201     C                   call(e)   'TIS711C'
055300010201     C                   parm                    dscmz
055400010201     C                   parm      *blanks       esito
055500010205     C                   if        %error
055600010205     C                             or cmzerr = '1'
055700010205     C                             or esito  = '1'
055800010205     C                   eval      wrkesito = '3'
055900010205     C                   else
056000010201     C*
056100021113     C* 2° invio FIVAB
056200010201     C                   reset                   dscmz
056300010201     C                   move      vlrpoi        cmzdst
056400010201     C                   eval      cmzfld = vlrfou
056500010201     C                   eval      cmzmbd = vlrhdl
056600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700021009     C***                if        prmfir = *blanks
056800021113     C                   eval      cmzfla = 'FIVAB00F'
056900021113     C                   eval      cmzmba = 'FIVAB00F'
057000021009     C***                else
057100021009     C***                eval      cmzfla = prmfir
057200021009     C***                eval      cmzmba = prmfir
057300021009     C***                endif
057400010201     C                   eval      cmznrr = *zeros
057500010201     C                   move      §ctrokvb      cmznrr
057600021018     C                   eval      cmzlba = vlrfl1
057700010201     C                   call(e)   'TIS711C'
057800010201     C                   parm                    dscmz
057900010201     C                   parm      *blanks       esito
058000010201     C                   if        %error
058100010201     C                             or cmzerr = '1'
058200010201     C                             or esito  = '1'
058300010201     C                   eval      wrkesito = '3'
058400010201     C                   endif
058500010205     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800000613     C***
058900070411
059000070411     C     *pssr         BEGSR
059100070411     C*
059200070411     C                   if        %open(tivin00r)
059300070411     C                   close     tivin00r
059400070411     C                   endif
059500070411     C                   if        %open(fivabwwr)
059600070411     C                   close     fivabwwr
059700070411     C                   endif
059800070411     C                   if        %open(fivatwwr)
059900070411     C                   close     fivatwwr
060000070411     C                   endif
060100070411     C*
060200070411     C* Effettuo la chiamata al CLLE preposto
060300070411     C                   call(e)   'TITVVTC'
060400070411     C                   parm                    parccm
060500070411     C                   parm                    parmbr
060600070411     C                   parm      '2'           paropz
060700070411     C*
060800070411     C                   eval      wrkesito = '2'
060900070411     C*
061000070411     C                   seton                                        LR
061100070411     C*
061200070411     C                   ENDSR     '*CANCL'
061300070411     C***
061400070411
061500990910
061600000613     C     *inzsr        BEGSR
061700990910     C*
061800990910     C     *entry        plist
061900990920     C                   parm                    tivlrds
062000990921     C                   parm      wrkesito      esito
062100000724     C                   parm                    prmlit
062200000710     C                   parm                    prmfir
062300000613     C*
062400000830     C* CALCOLA LA DATA CORRENTE
062500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
062600100324     C                   eval      datcor = %dec(%date() : *ISO)
062700000830     C*
062800000613     C                   ENDSR
