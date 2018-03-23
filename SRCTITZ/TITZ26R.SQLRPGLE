000100100331      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120307
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500100331     FEDIVABwr  O    E             DISK    usropn
000600100331     FEDIVATwr  O    E             DISK    usropn
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
002100120420     D depspe          s              7    INZ(*blanks)
002200120420     D curspe          s              7    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900020725
003000020725     D*------------------
003100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003200020725     D*------------------
003300070530     D INPUT_DS        DS                  INZ
003400120411     D  VINDTA                     2048
003500120411     D  VINFLG                        1
003600120420     D  VINSPE                        7
003700120411     D  VINRRN                        8  0
003800020725     D*
003900081113
004000091223     D*------------------
004100091223     D* DS REPERIMENTO NUMERATORE
004200091223     D*------------------
004300100309     D trul33ds      e ds                  inz
004400091223     D*------------------
004500091223     D* DS ARCHITETTURA
004600091223     D*------------------
004700091223     D kpjba         e ds                  inz
004800091223
004900081113
005000120411     D*-------------------
005100120411     D* COSTANTI
005200120411     D*-------------------
005300120411     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005400120411     D                                     èéù')
005500120411     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
005600120411     D                                     EEU')
005700120411
005800081113     D*------------------
005900081113     D* LINKING A DEFINIZIONI ESTERNE
006000081113     D*------------------
006100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006200090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113
006400990908
006500010201
006600010201
006700080117     C*
006800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006900080117     C
007000080117     C/EXEC SQL
007100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007200080117     C/END-EXEC
007300080117     C
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000613     C*
007800120411     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
007900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008000000613     C*
008100120411     C* Effettuo la chiamata al CLLE preposto
008200120411     C  N51              call(e)   'TITVEVTC'
008300120411     C                   parm                    parccm
008400120411     C                   parm                    parmbr
008500120411     C                   parm      '2'           paropz
008600120411     C   51              call(e)   'TITVEVBC'
008700120411     C                   parm                    parccm
008800120411     C                   parm                    parmbr
008900120411     C                   parm      '2'           paropz
009000070730     C*
009100070730     C* Effettuo lancio TISI95 solo x chiusura
009200070730     C                   CLEAR                   TISI95DS
009300070730     C                   EVAL      I95TLA = 'C'
009400070730     C                   CALL      'TISI95R'
009500070730     C                   PARM                    TISI95DS
009600000616     C*
009700000801     C
009800010201     C                   seton                                        LR
009900990908
010000000801
010100910830     C*--------------------------------------------------------
010200100331     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
010300910830     C*--------------------------------------------------------
010400070530     C     RWFILE        BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900070530     C*
011000100331     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011100120412     C                   exsr      prevasx
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400120412     C*
011500120412     C                   if        not %open(edivabwr)
011600120412     C                   open      edivabwr
011700120412     C                   endif
011800010202     C*
011900100331     C                   if        not %open(edivatwr)
012000100331     C                   open      edivatwr
012100010201     C                   endif
012200080117     C*
012300080117     C                   EXSR      INZVAR
012400080117     C                   EXSR      DEFCAM
012500990910     C*
012600010201     C                   clear                   §CTROKVB          5 0
012700020305     C                   clear                   §CTROKVT          5 0
012800000801     C                   clear                   §CTRMO            5 0
012900000801     C                   clear                   §CTRNO            5 0
013000990910     C*
013100120420     C* considero solo i rcd che iniziano con A1
013200120420     C*
013300020725     C/EXEC SQL
013400020725     C+ declare C1 cursor for select
013500120420     C+ vindta, vinflg, substr(vindta, 14, 7) as sped, rrn(tivin00r)
013600060223     C+ from tivin00r
013700120420     C+ where substr(vindta, 1, 2) = 'A1'
013800060223     C+ order by sped
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
016100120411     C*
016200120411     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016300120411     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016400010305     C*
016500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016600120420     C                   EVAL      curspe=%trim(%subst(vindta:14:7))
016700010305     C*
016800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016900080923     C                   if        *in50 = *off
017000120411     C                   exsr      inzvar
017100120411     C                   exsr      defcam
017200071003     C                   exsr      impvab
017300071003     C                   exsr      wrivab
017400100127     C                   exsr      wrivat_a                                     => carico VAT
017500071003     C                   exsr      wrivat_b                                     => carico VAT
017600120411     C   51              exsr      wrivat_e                                     => carico VAT
017700120420     C                   exsr      wrivat_i                                     => carico VAT
017800120420     C                   exsr      wrivat_j                                     => carico VAT
017900071003     C                   else
018000080923     C*
018100071009     C                   if        wDISK = 'D'
018200091223     C                   exsr      repNSP
018300071009     C                   exsr      impvab
018400071009     C                   exsr      wrivab
018500100127     C                   exsr      wrivat_a                                     => carico VAT
018600071009     C                   exsr      wrivat_b                                     => carico VAT
018700071009     C                   exsr      wrivat_e                                     => carico VAT
018800120420     C                   exsr      wrivat_i                                     => carico VAT
018900120420     C                   exsr      wrivat_j                                     => carico VAT
019000071009     C                   else
019100071009     C*
019200010305     C                   if        depspe = *blanks                             => 1° giro
019300010305     C                   eval      depspe = curspe                              => memorizz. spediz
019400080117     C                   clear                   tivinds
019500091223     C                   exsr      repNSP
019600020305     C                   exsr      impvab
019700100127     C                   exsr      wrivat_a                                     => carico VAT
019800071003     C                   exsr      wrivat_b                                     => carico VAT
019900071003     C   50              exsr      wrivat_e                                     => carico VAT
020000120420     C                   exsr      wrivat_i                                     => carico VAT
020100120420     C                   exsr      wrivat_j                                     => carico VAT
020200010305     C                   else
020300020725     C                   if        depspe <> curspe                             => rottura di spediz
020400010305     C                   eval      depspe = curspe                              => memorizz. spediz
020500070928     C                   exsr      wrivab
020600080117     C                   clear                   tivinds
020700091223     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900100127     C                   exsr      wrivat_a                                     => carico VAT
021000071003     C                   exsr      wrivat_b                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200120420     C                   exsr      wrivat_i                                     => carico VAT
021300120420     C                   exsr      wrivat_j                                     => carico VAT
021400020305     C                   else                                                   => x stessa spediz
021500120411     C*
021600120411     C* Se nn richiesta esclusione spedizioni "duplicate"
021700120411     C                   select
021800120411     C                   when      wDUPREC = ' '
021900120411     C                   exsr      impvab
022000120411     C                   exsr      wrivat_a                                     => carico VAT
022100120411     C                   exsr      wrivat_b                                     => carico VAT
022200120411     C   50              exsr      wrivat_e                                     => carico VAT
022300120420     C                   exsr      wrivat_i                                     => carico VAT
022400120420     C                   exsr      wrivat_j                                     => carico VAT
022500120411     C                   when      wDUPREC = 'C'
022600120411     C   50              exsr      wrivat_e                                     => carico VAT
022700120411     C                   endsl
022800010305     C                   endif
022900010305     C                   endif
023000010305     C                   endif
023100071003     C                   endif
023200120411     C*
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
025800071009     C                   exsr      wrivab
025900071009     C                   endif
026000010202     C*
026100010202     C                   endif
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800120412     C                   if        §ctrokvb > 0 OR
026900120412     C                             §ctrokvt > 0
027000990921     C                   eval      wrkesito = '1'
027100000710     C                   else
027200000710     C                   eval      wrkesito = '2'
027300990910     C                   endif
027400000710     C                   endif
027500990910     C*
027600990914     C                   if        %open(tivin00r)
027700990908     C                   close     tivin00r
027800990914     C                   endif
027900100331     C                   if        %open(edivabwr)
028000100331     C                   close     edivabwr
028100990914     C                   endif
028200100331     C                   if        %open(edivatwr)
028300100331     C                   close     edivatwr
028400010201     C                   endif
028500990910     C*
028600120412     C                   if        (§ctrokvb > 0 OR
028700120412     C                              §ctrokvt > 0    )
028800000724     C                             and vlrpoi <> *zeros
028900010202     C                   exsr      invio
029000990920     C                   endif
029100990920     C*
029200910830     C                   ENDSR
029300000613     C***
029400010305
029500010305     C*----------------------------------------------------*
029600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029700010305     C*----------------------------------------------------*
029800020305     C     WRIVAB        BEGSR
029900070730     C*
030000071003     C* Considerazioni finali
030100071003     C                   if        VABRMA = *blanks
030200071003     C                   movel(P)  VABRMN        VABRMA
030300071003     C                   endif
030400100331     C*
030500100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030600100331     C                   EVAL      VABCMR = %char(datcor)+'-'+%char(oracor)
030700100331     C                   EVAL      VABDCM = datcor
030800100331     C                   EVAL      VABDTS = datcor
030900100331     C                   EVAL      VABHMS = oracor
031000100331     C                   EVAL      VABCNT = 1
031100071003     C*
031200120411     C                   if        *in51 = *off and *in31 = *off
031300100331     C                   write     edivab00                                     => scarico il VAB
031400120411     C                   endif
031500010305     C*
031600010305     C                   ENDSR
031700990920
031800000801     C*----------------------------------------------------*
031900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032000000801     C*----------------------------------------------------*
032100010201     C     INZVAR        BEGSR
032200000801     C*
032300010201     C                   Z-ADD     *zeros        Num5_0
032400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032500020725     C                   MOVEL     '0'           FlgCAS            1
032600000801     C*
032700000801     C                   ENDSR
032800000801     C*----------------------------------------------------*
032900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033000000801     C*----------------------------------------------------*
033100020904     C     DEFCAM        BEGSR
033200080923     C*
033300120411     C                   SETOFF                                       505160
033400000801     C*
033500100331     C                   CLEAR                   EDIVAB00
033600100331     C                   CLEAR                   EDIVAT00
033700070730     C* Imposto i valori di default...
033800120420     C                   EVAL      VABCCM = 0896180
033900120420     C                   EVAL      VATCCM = 0896180
034000120420     C                   EVAL      VABLNP = 089
034100120420     C                   EVAL      VATLNP = 089
034200120420     C                   EVAL      VABCTR = 000
034300120411     C                   EVAL      VABCBO = '1'
034400070222     C* ... e poi verifico se sono stati passati come parametri
034500070222     C                   IF        vlrppt > *blanks
034600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABCCM
035100070222     C                   Z-ADD     PiVal         VATCCM
035200070222     C                   ENDIF
035300070222     C                   ENDIF
035400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035600070222     C                   EXSR      CHKNUM
035700070222     C                   IF        PiInt=*on
035800070222     C                   Z-ADD     PiVal         VABLNP
035900070222     C                   Z-ADD     PiVal         VATLNP
036000070222     C                   ENDIF
036100070222     C                   ENDIF
036200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036400070222     C                   EXSR      CHKNUM
036500070222     C                   IF        PiInt=*on
036600070222     C                   Z-ADD     PiVal         VABCTR
036700070222     C                   ENDIF
036800070928     C                   ENDIF
036900071009     C                   MOVEL     *blanks       wDISK             1
037000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037100071009     C                   IF        wDISK <> *blanks
037200070928     C                   SETON                                        50
037300070222     C                   ENDIF
037400120411     C                   IF        wDISK  = 'T'
037500120411     C                   SETOFF                                       50
037600120411     C                   SETON                                        51
037700120411     C                   ENDIF
037800120411     C                   MOVEL     *blanks       wDUPREC           1
037900120411     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
038000120411     C                   IF        %subst(vlrppt:16:2) <> *blanks
038100120411     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038200120411     C                   ENDIF
038300120411     C*
038400070222     C                   ENDIF
038500071009     C*
038600071009     C   50              EVAL      VABCTM = '7Q'
038700000801     C*
038800000801     C                   ENDSR
038900091223     C*----------------------------------------------------*
039000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039100091223     C*----------------------------------------------------*
039200091223     C     REPNSP        BEGSR
039300091223     C*
039400091223     C                   EXSR      INZVAR
039500091223     C                   EXSR      DEFCAM
039600120411     C*
039700120411     C                   SETON                                        60
039800091223     C*
039900091223     C* NSP => Stacco un numeratore da AZNUM
040000091223     C                   clear                   TRUL33DS
040100091223     C                   eval      I33OPE = *zeros
040200091223     C                   eval      I33CNU = 302
040300091223     C                   eval      I33NUM = 1
040400091223     C                   movel     TRUL33DS      KPJBU
040500091223     C                   call      'TRUL33R'
040600091223     C                   parm                    KPJBA
040700091223     C                   movel     KPJBU         TRUL33DS
040800091223     C                   if        O33ERR = *zeros
040900091223     C                   z-add     O33NRF        VABNSP
041000091223     C                   z-add     O33NRF        VATNSP
041100091223     C                   else
041200091223     C                   SETON                                        31
041300091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041400091223     C                             + ' ' + 'VABNSP VATNSP'
041500091223     C                   endif
041600091223     C*
041700091223     C                   ENDSR
041800000801     C*----------------------------------------------------*
041900100331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
042000000801     C*----------------------------------------------------*
042100010201     C     IMPVAB        BEGSR
042200070530     C*
042300070530     C                   SETOFF                                       3132
042400070928     C*
042500070928     C                   MOVE(P)   vlrpoi        VABFGS
042600070928     C                   MOVE(P)   vlrpoi        VATFGS
042700070928     C*
042800070928     C                   MOVEL     datcor        VABAAS
042900070928     C                   MOVEL     datcor        VATAAS
043000070928     C                   MOVE      datcor        VABMGS
043100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
043300070928     C* ==
043400120420     C                   EVAL      VABIND=%trim(%subst(vindta:119:30))
043500120420     C                   EVAL      VABLOD=%trim(%subst(vindta:215:26))
043600120420     C                   EVAL      VABCAD=%trim(%subst(vindta:243:5))
043700120420     C                   EVAL      VABRMA=%trim(%subst(vindta:6:15))
043800120420     C                   EVAL      VABRSD=%trim(%subst(vindta:59:28))
043900120420     C                   EVAL      VABRD2=%trim(%subst(vindta:151:30))
044000120516     C                   EVAL      VABRMO=%trim(%subst(vindta:653:25))
044100120516     C                   EVAL      VABCMO=%trim(%subst(vindta:837:5))
044200120516     C* il cliente ci passa la sua sigla di nazione che non sappiamo se ISO, ma siccome sarà
044300120516     C* ipoteticamente sempre Francia, prendiamo il primo carattere di questa sigla, che sarà
044400120516     C* appunto F
044500120516     C                   EVAL      VABNMO=%trim(%subst(vindta:835:1))
044600120411     C*
044700120411     C* Reperimento campi NUMERICI
044800120411     C*
044900120411     C* NSP
045000120411     C                   IF        *IN51 = *off
045100120420     C                   EVAL      PiStr=%subst(vindta:14:7)
045200120411     C                   EXSR      CHKNUM
045300120411     C                   IF        PiInt=*on
045400120411     C  N60              Z-ADD     PiVal         VABNSP
045500120411     C  N60              Z-ADD     PiVal         VATNSP
045600120420     C**N60              Z-ADD     PiVal         VABRMN
045700120411     C                   ELSE
045800120411     C                   SETON                                        32
045900120420     C***                Z-ADD     1             VABRMN
046000120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100120411     C                             + ' ' + 'VABNSP VATNSP'
046200120411     C                   ENDIF
046300120411     C*
046400120411     C                   ELSE
046500120411     C*
046600120420     C                   EVAL      PiStr=%subst(vindta:14:7)
046700120411     C                   EXSR      CHKNUM
046800120411     C                   IF        PiInt=*on
046900120411     C  N60              Z-ADD     PiVal         VATNSP
047000120411     C                   ELSE
047100120411     C                   SETON                                        32
047200120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047300120411     C                             + ' ' + 'VATNSP'
047400120411     C                   ENDIF
047500120411     C                   ENDIF
047600120411     C* RMN
047700120420     C                   EVAL      PiStr=%subst(vindta:6:15)
047800120411     C                   EXSR      CHKNUM
047900120411     C                   IF        PiInt=*on
048000120411     C                   Z-ADD     PiVal         VABRMN
048100120411     C                   ELSE
048200120411     C  N51              SETON                                        32
048300120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
048400120411     C                             + ' ' + 'VABRMN'
048500120411     C                   ENDIF
048600120411     C* NCL
048700120420     C                   EVAL      PiStr=%subst(vindta:21:2)
048800120411     C                   EXSR      CHKNUM
048900120411     C                   IF        PiInt=*on
049000120411     C                   Z-ADD     PiVal         VABNCL
049100120411     C                   ELSE
049200120411     C  N51              SETON                                        32
049300120411     C  N51              Z-ADD     *zeros        VABNCL
049400120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049500120411     C                             + ' ' + 'VABNCL'
049600120411     C                   ENDIF
049700120411     C* PKB
049800120420     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
049900120411     C                   EXSR      CHKNUM
050000120411     C                   IF        PiNum=*on
050100120411     C* il peso ci arriva in grammi
050200120411-->  C                   EVAL(H)   PiVal=PiVal/1000
050300120411     C                   Z-ADD(H)  PiVal         VABPKB
050400120411     C                   ELSE
050500120411     C  N51              SETON                                        32
050600120411     C  N51              Z-ADD     *zeros        VABPKB
050700120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050800120411     C                             + ' ' + 'VABPKB'
050900120411     C                   ENDIF
051000100310     C*
051100100310     C* Se provincia nn valorizzata la reperisco
051200100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
051300120412     C                   IF        VABNZD  = *blanks AND
051400120412     C                             VABPRD  = *blanks AND
051500120412     C                             VABCAD <> *blanks
051600100310     C                   CLEAR                   TISI95DS
051700100310     C                   EVAL      I95TCN = '3'
051800100310     C                   Z-ADD     datcor        I95DAT
051900100310     C                   EVAL      I95NAR = VABNZD
052000100310     C                   EVAL      I95CAP = VABCAD
052100100310     C                   EVAL      I95LOC = VABLOD
052200100310     C                   CALL      'TISI95R'
052300100310     C                   PARM                    TISI95DS
052400100310     C                   EVAL      VABPRD = O95PRV
052500100310     C                   ENDIF
052600070730     C*
052700070730     C* Considerazioni finali su CBO/CAS
052800070730     C                   IF        FlgCAS = '1'
052900070730     C                   IF        VABCBO = '1'
053000070730     C                   EVAL      VABCBO = '4'
053100070730     C                   ENDIF
053200070730     C                   IF        VABCBO = '2'
053300070730     C                   EVAL      VABCBO = '6'
053400070730     C                   ENDIF
053500070730     C                   ENDIF
053600010202     C*
053700000801     C* Ebbene...
053800000801     C                   ADD       1             §CTRMO
053900070530     C                   IF        *in31 <> *zeros OR
054000070530     C                             *in32 <> *zeros
054100000801     C                   ADD       1             §CTRNO
054200020725     C                   EVAL      x_vinflg = '2'
054300000801     C                   ELSE
054400010201     C                   ADD       1             §CTROKVB
054500000801     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800100127     C*----------------------------------------------------*
054900100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
055000100127     C*----------------------------------------------------*
055100100127     C     WRIVAT_A      BEGSR
055200100127     C*
055300100331     C* Valorizzo l buffer di scrittura del EDIVAT
055400100127     C* - TIPO RECORD "A"
055500100331     C***                eval      VATTRC = 'A'
055600100331     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
055700100331     C***                exsr      wrivat
055800100127     C*
055900100127     C                   ENDSR
056000010201     C*----------------------------------------------------*
056100100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
056200010201     C*----------------------------------------------------*
056300071003     C     WRIVAT_B      BEGSR
056400010201     C*
056500100331     C* Valorizzo l buffer di scrittura del EDIVAT
056600070928     C* - TIPO RECORD "B"
056700120420     C                   eval      VATTRC = 'B'
056800120420     C                   eval      VATNOT = %trim(%subst(vindta:253:20))
056900120420     C                   exsr      wrivat
057000010201     C*
057100010201     C                   ENDSR
057200120411     C*----------------------------------------------------*
057300120411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
057400120411     C*----------------------------------------------------*
057500120411     C     WRIVAT_E      BEGSR
057600120411     C*
057700120411     C* Valorizzo l buffer di scrittura del FIVAT
057800120411     C* - TIPO RECORD "E"
057900120411     C*
058000120411     C                   eval      VATTRC = 'E'
058100120516     C                   eval      VATNOT = %trim(%subst(vindta:11:10))
058200120420     C                   exsr      wrivat
058300120420     C***
058400120420     C**Sviluppati i "CHI SONO" in relazione al numero colli
058500120420     C***
058600120420     C**Inizializzo progressivo collo su totale colli della spedizione
058700120420     C***                Z-ADD     1             wNumColloDA       2 0
058800120420     C***                MOVEL     *blanks       wNumColloCor      2
058900120420     C***                MOVEL     *blanks       wWhoIs            6
059000120420     C***                eval      wWhoIs = %trim(%subst(vindta:5:6))
059100120420     C***
059200120420     C***                DOW       wNumColloDA <= VABNCL
059300120420     C***                MOVEL(P)  wNumColloDA   wNumColloCor
059400120420     C***                EVAL      VATNOT =
059500120420     C***                             %subst(%char(%dec(%date() : *ISO)):3:2) +
059600120420     C***                             wNumColloCor + wWhoIs
059700120420     C***                exsr      wrivat
059800120420     C***                add       1             §CTROKVT
059900120420     C***
060000120420     C***                EVAL      wNumColloDA = wNumColloDA + 1
060100120420     C***                ENDDO
060200120411     C*
060300120411     C                   ENDSR
060400120420     C*----------------------------------------------------*
060500120420     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "I"
060600120420     C*----------------------------------------------------*
060700120420     C     WRIVAT_I      BEGSR
060800120420     C*
060900120420     C* Valorizzo l buffer di scrittura del EDIVAT
061000120420     C* - TIPO RECORD "I"
061100120420     C                   if        %trim(%subst(vindta:293:35)) <> *blank
061200120420     C                   eval      VATTRC = 'I'
061300120420     C                   eval      VATNOT = %trim(%subst(vindta:293:35))
061400120420     C                   exsr      wrivat
061500120420     C                   endif
061600120420     C*
061700120420     C                   ENDSR
061800120420     C*----------------------------------------------------*
061900120420     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "J"
062000120420     C*----------------------------------------------------*
062100120420     C     WRIVAT_J      BEGSR
062200120420     C*
062300120420     C* Valorizzo l buffer di scrittura del EDIVAT
062400120420     C* - TIPO RECORD "J"
062500120420     C                   if        %trim(%subst(vindta:318:35)) <> *blank
062600120420     C                   eval      VATTRC = 'J'
062700120420     C                   eval      VATNOT = %trim(%subst(vindta:318:35))
062800120420     C                   exsr      wrivat
062900120420     C                   endif
063000120420     C*
063100120420     C                   ENDSR
063200100127     C*----------------------------------------------------*
063300100331     C*  SCARICO BUFFER EDIVAT
063400100127     C*----------------------------------------------------*
063500100127     C     WRIVAT        BEGSR
063600100127     C*
063700100127     C                   if        vatNOT <> *blanks
063800100331     C*
063900100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064000100331     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
064100100331     C                   EVAL      VATCNT = 1
064200100331     C*
064300100127     C                   ADD       1             §CTROKVT
064400100331     C                   write     EDIVAT00
064500100127     C                   endif
064600100127     C*
064700100127     C                   ENDSR
064800020904
064900020904
065000010202     C*----------------------------------------------------*
065100100331     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
065200010202     C*----------------------------------------------------*
065300120412     C     PREVASX       BEGSR
065400010202     C*
065500100331     C* Compongo il nome del membro da dare al EDIVATWR
065600010202     C                   eval      parmbr = vlrhdl
065700010202     C                   movel     'M'           parmbr
065800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065900010202     C                   eval      paropz = '1'
066000010202     C* Effettuo la chiamata al CLLE preposto
066100120411     C  N51              call(e)   'TITVEVTC'
066200010202     C                   parm                    parccm
066300010202     C                   parm                    parmbr
066400010202     C                   parm                    paropz
066500120411     C   51              call(e)   'TITVEVBC'
066600120411     C                   parm                    parccm
066700120411     C                   parm                    parmbr
066800120411     C                   parm                    paropz
066900120411     C*
067000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067100010202     C                   if        %error
067200010202     C                   movel     '1'           chkcall
067300010202     C                   else
067400010202     C                   movel     '0'           chkcall
067500010202     C                   endif
067600010202     C*
067700010202     C                   ENDSR
067800000801     C*----------------------------------------------------*
067900000801     C*  CONTROLLO NUMERICITA' CAMPI
068000000801     C*----------------------------------------------------*
068100000801     C     CHKNUM        BEGSR
068200081113     C*
068300081113     C                   IF        PiDecChr = *blanks
068400120412     C                   EVAL      PiDecChr = '.'
068500081113     C                   ENDIF
068600091223     C*
068700081113     C                   callp(e)  UBISNUM_Check(PiStr
068800081113     C                                          :PiDecChr
068900081113     C                                          :PiVal
069000081113     C                                          :PiNum
069100081113     C                                          :PiInt)
069200081113     C*
069300000801     C                   IF        %error
069400000801     C                   EVAL      PiInt=*off
069500000801     C                   ENDIF
069600000801     C*
069700000801     C                   ENDSR
069800000801     C***
069900000801
070000990920      /TITLE Invio dei dati al punto operativo.
070100010202     C     invio         BEGSR
070200990920     C*
070300120412     C* 1° invio EDIVAT
070400010201     C                   reset                   dscmz
070500120412     C                   move      vatfgs        cmzdst
070600100331     C                   eval      cmzfld = 'EDIVATWR'
070700010201     C                   eval      cmzmbd = vlrhdl
070800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070900021009     C***                if        prmfir = *blanks
071000100331     C                   eval      cmzfla = 'EDIVAT0F'
071100100331     C                   eval      cmzmba = 'EDIVAT0F'
071200021009     C***                else
071300021009     C***                eval      cmzfla = prmfir
071400021009     C***                eval      cmzmba = prmfir
071500021009     C***                endif
071600010201     C                   eval      cmznrr = *zeros
071700020305     C                   move      §ctrokvt      cmznrr
071800021018     C                   eval      cmzlba = vlrfl1
071900010201     C                   call(e)   'TIS711C'
072000010201     C                   parm                    dscmz
072100010201     C                   parm      *blanks       esito
072200010205     C                   if        %error
072300010205     C                             or cmzerr = '1'
072400010205     C                             or esito  = '1'
072500010205     C                   eval      wrkesito = '3'
072600010205     C                   else
072700010201     C*
072800100331     C* 2° invio EDIVAB
072900120412     C                   if        *in51 = *off
073000010201     C                   reset                   dscmz
073100120412     C                   move      vabfgs        cmzdst
073200010201     C                   eval      cmzfld = vlrfou
073300010201     C                   eval      cmzmbd = vlrhdl
073400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073500021009     C***                if        prmfir = *blanks
073600100331     C                   eval      cmzfla = 'EDIVAB0F'
073700100331     C                   eval      cmzmba = 'EDIVAB0F'
073800021009     C***                else
073900021009     C***                eval      cmzfla = prmfir
074000021009     C***                eval      cmzmba = prmfir
074100021009     C***                endif
074200010201     C                   eval      cmznrr = *zeros
074300010201     C                   move      §ctrokvb      cmznrr
074400021018     C                   eval      cmzlba = vlrfl1
074500010201     C                   call(e)   'TIS711C'
074600010201     C                   parm                    dscmz
074700010201     C                   parm      *blanks       esito
074800010201     C                   if        %error
074900010201     C                             or cmzerr = '1'
075000010201     C                             or esito  = '1'
075100010201     C                   eval      wrkesito = '3'
075200010201     C                   endif
075300010205     C                   endif
075400120412     C                   endif
075500990920     C*
075600000613     C                   ENDSR
075700000613     C***
075800070411
075900070411     C     *pssr         BEGSR
076000070411     C*
076100070411     C                   if        %open(tivin00r)
076200070411     C                   close     tivin00r
076300070411     C                   endif
076400100331     C                   if        %open(edivabwr)
076500100331     C                   close     edivabwr
076600070411     C                   endif
076700100331     C                   if        %open(edivatwr)
076800100331     C                   close     edivatwr
076900070411     C                   endif
077000070411     C*
077100070411     C* Effettuo la chiamata al CLLE preposto
077200120411     C  N51              call(e)   'TITVEVTC'
077300120411     C                   parm                    parccm
077400120411     C                   parm                    parmbr
077500120411     C                   parm      '2'           paropz
077600120411     C   51              call(e)   'TITVEVBC'
077700120411     C                   parm                    parccm
077800120411     C                   parm                    parmbr
077900120411     C                   parm      '2'           paropz
078000070411     C*
078100070411     C                   eval      wrkesito = '2'
078200070411     C*
078300070411     C                   seton                                        LR
078400070411     C*
078500070411     C                   ENDSR     '*CANCL'
078600070411     C***
078700070411
078800990910
078900000613     C     *inzsr        BEGSR
079000990910     C*
079100990910     C     *entry        plist
079200990920     C                   parm                    tivlrds
079300990921     C                   parm      wrkesito      esito
079400000724     C                   parm                    prmlit
079500000710     C                   parm                    prmfir
079600000613     C*
079700000830     C* CALCOLA LA DATA CORRENTE
079800100331     C                   time                    wn14             14 0
079900100331     C                   movel     wn14          oracor            6 0          *ORA
080000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080100091223     C                   eval      datcor = %dec(%date() : *ISO)
080200000830     C*
080300000613     C                   ENDSR
080400000613     C***
