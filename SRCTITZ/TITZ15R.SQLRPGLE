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
002100111227     D depspe          s              7    INZ(*blanks)
002200111227     D curspe          s              7    INZ(*blanks)
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
012900120227     C+ vindta, vinflg, substr(vindta, 193, 4) as sped, rrn(tivin00r)
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
014700111228
014800000801     C*
014900020725     C                   if        vinflg = *blanks
015000020725     C                             or vinflg = '0'
015100020725     C                             or vinflg = '2'
015200000801     C*
015300020725     C                   clear                   x_vinmsg
015400020725     C                   eval      x_vinflg = '1'
015500111205     C*
015600111205     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
015700111205     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
015800010305     C*
015900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016000120227     C                   EVAL      curspe=%trim(%subst(vindta:193:4))
016100010305     C*
016200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016300080923     C                   if        *in50 = *off
016400111228     C                   exsr      inzvar
016500111228     C                   exsr      defcam
016600071003     C                   exsr      impvab
016700071003     C                   exsr      wrivab
016800111228     C                   exsr      wrivat_b                                     => carico VAT
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200111228     C                   exsr      inzvar
017300111228     C                   exsr      defcam
017400071009     C                   exsr      impvab
017500071009     C                   exsr      wrivab
017600111227     C                   exsr      wrivat_b                                     => carico VAT
017700071009     C                   exsr      wrivat_e                                     => carico VAT
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300111228     C                   exsr      inzvar
018400111228     C                   exsr      defcam
018500020305     C                   exsr      impvab
018600111227     C   50              exsr      wrivat_b                                     => carico VAT
018700071003     C   50              exsr      wrivat_e                                     => carico VAT
018800010305     C                   else
018900020725     C                   if        depspe <> curspe                             => rottura di spediz
019000010305     C                   eval      depspe = curspe                              => memorizz. spediz
019100070928     C                   exsr      wrivab
019200080117     C                   clear                   tivinds
019300111228     C                   exsr      inzvar
019400111228     C                   exsr      defcam
019500020305     C                   exsr      impvab
019600111227     C   50              exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800020305     C                   else                                                   => x stessa spediz
019900111228     C                   exsr      impvab
020000111227     C   50              exsr      wrivat_b                                     => carico VAT
020100071003     C   50              exsr      wrivat_e                                     => carico VAT
020200010305     C                   endif
020300010305     C                   endif
020400010305     C                   endif
020500071003     C                   endif
020600071009     C                   endif
020700000905     C*
020800000905     C                   else
020900020725     C                   eval      x_vinflg = '1'
021000000905     C                   endif
021100000905     C*
021200020725     C     VINRRN        chain     tivin000
021300020725     C                   if        %found(tivin00r)
021400020725     C                   eval      y_vinflg = x_vinflg
021500020725     C                   eval      y_vinmsg = x_vinmsg
021600020725     C                   update    tivin000
021700020725     C                   endif
021800020725     C*
021900020725     C/EXEC SQL
022000070530     C+ Fetch C1 into :INPUT_DS
022100020725     C/END-EXEC
022200020725     C*
022300020725     C                   enddo
022400020725     C*
022500020725     C/EXEC SQL
022600020725     C+ close C1
022700020725     C/END-EXEC
022800000905     C*
022900020305     C* Scarico i VAB rimasti "in sospeso"
023000071009     C                   if        wDISK = 'C'
023100071009     C                   exsr      wrivab
023200071009     C                   endif
023300010202     C*
023400010202     C                   endif
023500990910
023600990910     C* Se non ci sono record con errori ...
023700000710     C                   if        §ctrno = 0
023800990910     C* ... restituisco esito OK.
023900990921     C                   eval      wrkesito = '0'
024000990910     C                   else
024100010201     C                   if        §ctrokvb > 0
024200990921     C                   eval      wrkesito = '1'
024300000710     C                   else
024400000710     C                   eval      wrkesito = '2'
024500990910     C                   endif
024600000710     C                   endif
024700990910     C*
024800990914     C                   if        %open(tivin00r)
024900990908     C                   close     tivin00r
025000990914     C                   endif
025100021113     C                   if        %open(fivabwwr)
025200021113     C                   close     fivabwwr
025300990914     C                   endif
025400021113     C                   if        %open(fivatwwr)
025500021113     C                   close     fivatwwr
025600010201     C                   endif
025700990910     C*
025800010201     C                   if        §ctrokvb > 0
025900000724     C                             and vlrpoi <> *zeros
026000010202     C                   exsr      invio
026100990920     C                   endif
026200990920     C*
026300910830     C                   ENDSR
026400000613     C***
026500010305
026600010305     C*----------------------------------------------------*
026700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026800010305     C*----------------------------------------------------*
026900020305     C     WRIVAB        BEGSR
027000070730     C*
027100071003     C* Considerazioni finali
027200071003     C                   if        VABRMA = *blanks
027300071003     C                   movel(P)  VABRMN        VABRMA
027400071003     C                   endif
027500071003     C*
027600021113     C                   write     fivab000                                     => scarico il VAB
027700010305     C*
027800010305     C                   ENDSR
027900990920
028000000801     C*----------------------------------------------------*
028100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028200000801     C*----------------------------------------------------*
028300010201     C     INZVAR        BEGSR
028400000801     C*
028500010201     C                   Z-ADD     *zeros        Num5_0
028600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028700020725     C                   MOVEL     '0'           FlgCAS            1
028800000801     C*
028900000801     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029200000801     C*----------------------------------------------------*
029300020904     C     DEFCAM        BEGSR
029400080923     C*
029500080923     C                   SETOFF                                       5051
029600000801     C*
029700021113     C                   CLEAR                   FIVAB000
029800021113     C                   CLEAR                   FIVAT000
029900070730     C* Imposto i valori di default...
030000120227     C                   EVAL      VABCCM = 0490523
030100120227     C                   EVAL      VATCCM = 0490523
030200120227     C                   EVAL      VABLNP = 049
030300120227     C                   EVAL      VATLNP = 049
030400120301     C                   EVAL      VABCTR = 001
030500070730     C                   EVAL      VABCBO = '1'
030600111227     C                   EVAL      VABTSP = 'C'
030700070222     C* ... e poi verifico se sono stati passati come parametri
030800070222     C                   IF        vlrppt > *blanks
030900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031100070222     C                   EXSR      CHKNUM
031200070222     C                   IF        PiInt=*on
031300070222     C                   Z-ADD     PiVal         VABCCM
031400070222     C                   Z-ADD     PiVal         VATCCM
031500070222     C                   ENDIF
031600070222     C                   ENDIF
031700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900070222     C                   EXSR      CHKNUM
032000070222     C                   IF        PiInt=*on
032100070222     C                   Z-ADD     PiVal         VABLNP
032200070222     C                   Z-ADD     PiVal         VATLNP
032300070222     C                   ENDIF
032400070222     C                   ENDIF
032500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700070222     C                   EXSR      CHKNUM
032800070222     C                   IF        PiInt=*on
032900070222     C                   Z-ADD     PiVal         VABCTR
033000070222     C                   ENDIF
033100070928     C                   ENDIF
033200071009     C                   MOVEL     *blanks       wDISK             1
033300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033400071009     C                   IF        wDISK <> *blanks
033500070928     C                   SETON                                        50
033600070222     C                   ENDIF
033700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033800080923     C                   SETON                                        51
033900080923     C                   ENDIF
034000070222     C                   ENDIF
034100071009     C*
034200071009     C   50              EVAL      VABCTM = '7Q'
034300000801     C*
034400000801     C                   ENDSR
034500000801     C*----------------------------------------------------*
034600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034700000801     C*----------------------------------------------------*
034800010201     C     IMPVAB        BEGSR
034900070530     C*
035000070530     C                   SETOFF                                       3132
035100070928     C*
035200070928     C                   MOVE(P)   vlrpoi        VABFGS
035300070928     C                   MOVE(P)   vlrpoi        VATFGS
035400070928     C*
035500070928     C                   MOVEL     datcor        VABAAS
035600070928     C                   MOVEL     datcor        VATAAS
035700070928     C                   MOVE      datcor        VABMGS
035800120227     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
035900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036100070928     C* ==
036200120227     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
036300120227     C                   EVAL      VABLOD=%trim(%subst(vindta:112:25))
036400120227     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
036500120227     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036600120227     C                   IF        %trim(%subst(vindta:145:2)) <> 'IT'
036700120227     C                   EVAL      VABNZD=%trim(%subst(vindta:145:2))
036800120227     C                   ENDIF
036900080923     C* RMN
037000120227     C                   EVAL      PiStr=%trim(%subst(vindta:193:4))
037100080923     C                   EXSR      CHKNUM
037200080923     C                   IF        PiInt=*on
037300080923     C                   Z-ADD     PiVal         VABRMN
037400080923     C                   ELSE
037500080923     C                   SETON                                        32
037600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037700080923     C                             + ' ' + 'VABRMN'
037800080923     C                   ENDIF
037900080923     C* NSP
038000080923     C                   IF        *in51 = *on
038100080923     C* NSP => Stacco un numeratore da AZNUM
038200080923     C                   clear                   TRUL33DS
038300080923     C                   eval      I33OPE = *zeros
038400080923     C                   eval      I33CNU = 302
038500080923     C                   eval      I33NUM = 1
038600080923     C                   movel     TRUL33DS      KPJBU
038700080923     C                   call      'TRUL33R'
038800080923     C                   parm                    KPJBA
038900080923     C                   movel     KPJBU         TRUL33DS
039000080923     C                   if        O33ERR = *zeros
039100080923     C                   z-add     O33NRF        VABNSP
039200080923     C                   z-add     O33NRF        VATNSP
039300080923     C                   else
039400080923     C                   SETON                                        31
039500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600080923     C                             + ' ' + 'VABNSP VATNSP'
039700080923     C                   endif
039800080923     C                   ELSE
039900120227     C                   EVAL      PiStr=%trim(%subst(vindta:193:4))
040000080923     C                   EXSR      CHKNUM
040100080923     C                   IF        PiInt=*on
040200080923     C                   Z-ADD     PiVal         VABNSP
040300080923     C                   Z-ADD     PiVal         VATNSP
040400080923     C                   ELSE
040500080923     C                   SETON                                        31
040600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040700080923     C                             + ' ' + 'VABNSP VATNSP'
040800080923     C                   ENDIF
040900080923     C                   ENDIF
041000071003     C* NCL
041100120227     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
041200120227     C                   EXSR      CHKNUM
041300120227     C                   IF        PiNum=*on
041400120227     C                   Z-ADD     PiVal         VABNCL
041500120227     C                   ELSE
041600120227     C                   SETON                                        32
041700120227     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041800120227     C                             + ' ' + 'VABNCL'
041900120227     C                   ENDIF
042000071003     C* PKB
042100120227     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
042200070928     C                   EXSR      CHKNUM
042300070928     C                   IF        PiNum=*on
042400120227     C                   Z-ADD     PiVal         VABPKB
042500070928     C                   ELSE
042600070928     C                   SETON                                        32
042700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042800070928     C                             + ' ' + 'VABPKB'
042900070928     C                   ENDIF
043000111227     C* CAS
043100120227     C                   IF        %subst(vindta:280:12) <> *blanks
043200111227     C                   EVAL      FlgCAS = '1'
043300111228     C                   EVAL      VABvca = 'EUR'
043400120227     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
043500111227     C                   EXSR      CHKNUM
043600111227     C                   IF        PiNum=*on
043700111227     C                   Z-ADD     PiVal         VABCAS
043800111227     C                   ELSE
043900111227     C                   SETON                                        32
044000111227     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044100111227     C                             + ' ' + 'VABCAS'
044200111227     C                   ENDIF
044300111227     C                   ENDIF
044400111205     C*
044500111205     C* Se provincia nn valorizzata la reperisco
044600111205     C* tramite TISI95R a seconda dei dati d instradamento presenti
044700111205     C                   IF        VABNZD  = *blanks AND
044800111205     C                             VABPRD  = *blanks AND
044900111205     C                             VABCAD <> *blanks
045000111205     C                   CLEAR                   TISI95DS
045100111205     C                   EVAL      I95TCN = '3'
045200111205     C                   Z-ADD     datcor        I95DAT
045300111205     C                   EVAL      I95NAR = VABNZD
045400111205     C                   EVAL      I95CAP = VABCAD
045500111205     C                   EVAL      I95LOC = VABLOD
045600111205     C                   CALL      'TISI95R'
045700111205     C                   PARM                    TISI95DS
045800111205     C                   EVAL      VABPRD = O95PRV
045900111205     C                   ENDIF
046000070730     C*
046100070730     C* Considerazioni finali su CBO/CAS
046200070730     C                   IF        FlgCAS = '1'
046300070730     C                   IF        VABCBO = '1'
046400070730     C                   EVAL      VABCBO = '4'
046500070730     C                   ENDIF
046600070730     C                   IF        VABCBO = '2'
046700070730     C                   EVAL      VABCBO = '6'
046800070730     C                   ENDIF
046900070730     C                   ENDIF
047000010202     C*
047100000801     C* Ebbene...
047200000801     C                   ADD       1             §CTRMO
047300070530     C                   IF        *in31 <> *zeros OR
047400070530     C                             *in32 <> *zeros
047500000801     C                   ADD       1             §CTRNO
047600020725     C                   EVAL      x_vinflg = '2'
047700000801     C                   ELSE
047800010201     C                   ADD       1             §CTROKVB
047900000801     C                   ENDIF
048000000801     C*
048100000801     C                   ENDSR
048200111227     C*----------------------------------------------------*
048300111227     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
048400111227     C*----------------------------------------------------*
048500111227     C     WRIVAT_B      BEGSR
048600111227     C*
048700111227     C                   eval      vatAAS = vabAAS
048800111227     C                   eval      vatLNP = vabLNP
048900111227     C                   eval      vatNRS = vabNRS
049000111227     C                   eval      vatNSP = vabNSP
049100111227     C*
049200111227     C* Valorizzo il buffer di scrittura del FIVAT
049300111227     C* - TIPO RECORD "B"
049400111227     C                   if        %subst(vindta:374:10) <> *blanks
049500111227     C                   eval      VATTRC = 'B'
049600111227     C                   eval      VATNOT = %trim(%subst(vindta:374:10))
049700111227     C                   write     FIVAT000
049800111227     C                   endif
049900111227     C*
050000111227     C                   ENDSR
050100111227
050200071003     C*----------------------------------------------------*
050300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
050400071003     C*----------------------------------------------------*
050500071003     C     WRIVAT_E      BEGSR
050600090204     C*
050700090204     C                   eval      vatAAS = vabAAS
050800090204     C                   eval      vatLNP = vabLNP
050900090204     C                   eval      vatNRS = vabNRS
051000090204     C                   eval      vatNSP = vabNSP
051100071003     C*
051200101007     C* Valorizzo il buffer di scrittura del FIVAT
051300071003     C* - TIPO RECORD "E"
051400120103     C                   if        %subst(vindta:920:21) <> *blanks
051500071003     C                   eval      VATTRC = 'E'
051600120103     C                   eval      VATNOT = %trim(%subst(vindta:920:21))
051700071003     C                   write     FIVAT000
051800111205     C                   endif
051900071003     C*
052000071003     C                   ENDSR
052100020904
052200010202     C*----------------------------------------------------*
052300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052400010202     C*----------------------------------------------------*
052500020305     C     PREVAT        BEGSR
052600010202     C*
052700021113     C* Compongo il nome del membro da dare al FIVATWWR
052800010202     C                   eval      parmbr = vlrhdl
052900010202     C                   movel     'M'           parmbr
053000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053100010202     C                   eval      paropz = '1'
053200010202     C* Effettuo la chiamata al CLLE preposto
053300050128     C                   call(e)   'TITVVTC'
053400010202     C                   parm                    parccm
053500010202     C                   parm                    parmbr
053600010202     C                   parm                    paropz
053700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053800010202     C                   if        %error
053900010202     C                   movel     '1'           chkcall
054000010202     C                   else
054100010202     C                   movel     '0'           chkcall
054200010202     C                   endif
054300010202     C*
054400010202     C                   ENDSR
054500000801     C*----------------------------------------------------*
054600000801     C*  CONTROLLO NUMERICITA' CAMPI
054700000801     C*----------------------------------------------------*
054800000801     C     CHKNUM        BEGSR
054900081113     C*
055000081113     C                   IF        PiDecChr = *blanks
055100120227     C                   EVAL      PiDecChr = '.'
055200081113     C                   ENDIF
055300081113     C*
055400081113     C                   callp(e)  UBISNUM_Check(PiStr
055500081113     C                                          :PiDecChr
055600081113     C                                          :PiVal
055700081113     C                                          :PiNum
055800081113     C                                          :PiInt)
055900081113     C*
056000000801     C                   IF        %error
056100000801     C                   EVAL      PiInt=*off
056200000801     C                   ENDIF
056300000801     C*
056400000801     C                   ENDSR
056500000801     C***
056600000801
056700011113
056800000801
056900000801
057000990920      /TITLE Invio dei dati al punto operativo.
057100010202     C     invio         BEGSR
057200990920     C*
057300021113     C* 1° invio FIVAT
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600021113     C                   eval      cmzfld = 'FIVATWWR'
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021113     C                   eval      cmzfla = 'FIVAT00F'
058100021113     C                   eval      cmzmba = 'FIVAT00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700020305     C                   move      §ctrokvt      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010205     C                   if        %error
059300010205     C                             or cmzerr = '1'
059400010205     C                             or esito  = '1'
059500010205     C                   eval      wrkesito = '3'
059600010205     C                   else
059700010201     C*
059800021113     C* 2° invio FIVAB
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100010201     C                   eval      cmzfld = vlrfou
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500021113     C                   eval      cmzfla = 'FIVAB00F'
060600021113     C                   eval      cmzmba = 'FIVAB00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200010201     C                   move      §ctrokvb      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010201     C                   if        %error
061800010201     C                             or cmzerr = '1'
061900010201     C                             or esito  = '1'
062000010201     C                   eval      wrkesito = '3'
062100010201     C                   endif
062200010205     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500000613     C***
062600070411
062700070411     C     *pssr         BEGSR
062800070411     C*
062900070411     C                   if        %open(tivin00r)
063000070411     C                   close     tivin00r
063100070411     C                   endif
063200070411     C                   if        %open(fivabwwr)
063300070411     C                   close     fivabwwr
063400070411     C                   endif
063500070411     C                   if        %open(fivatwwr)
063600070411     C                   close     fivatwwr
063700070411     C                   endif
063800070411     C*
063900070411     C* Effettuo la chiamata al CLLE preposto
064000070411     C                   call(e)   'TITVVTC'
064100070411     C                   parm                    parccm
064200070411     C                   parm                    parmbr
064300070411     C                   parm      '2'           paropz
064400070411     C*
064500070411     C                   eval      wrkesito = '2'
064600070411     C*
064700070411     C                   seton                                        LR
064800070411     C*
064900070411     C                   ENDSR     '*CANCL'
065000070411     C***
065100070411
065200990910
065300000613     C     *inzsr        BEGSR
065400990910     C*
065500990910     C     *entry        plist
065600990920     C                   parm                    tivlrds
065700990921     C                   parm      wrkesito      esito
065800000724     C                   parm                    prmlit
065900000710     C                   parm                    prmfir
066000000613     C*
066100000830     C* CALCOLA LA DATA CORRENTE
066200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066300100324     C                   eval      datcor = %dec(%date() : *ISO)
066400000830     C*
066500000613     C                   ENDSR
