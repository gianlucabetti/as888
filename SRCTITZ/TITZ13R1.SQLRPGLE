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
002100120411     D depspe          s              6    INZ(*blanks)
002200120411     D curspe          s              6    INZ(*blanks)
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
003600120411     D  VINSPE                        6
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
013100020725     C/EXEC SQL
013200020725     C+ declare C1 cursor for select
013300120411     C+ vindta, vinflg, substr(vindta, 188, 6) as sped, rrn(tivin00r)
013400060223     C+ from tivin00r
013500060223     C+ order by sped
013600020725     C+ for read only
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000020725     C+ open C1
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400070530     C+ Fetch C1 into :INPUT_DS
014500020725     C/END-EXEC
014600020725     C*
014700020725     C                   dow       sqlcod = *zeros
014800990913     C*
014900100310     C                   if        vindta > *blanks
015000000613     C                   add       1             rrnum
015100000801     C*
015200020725     C                   if        vinflg = *blanks
015300020725     C                             or vinflg = '0'
015400020725     C                             or vinflg = '2'
015500000801     C*
015600020725     C                   clear                   x_vinmsg
015700020725     C                   eval      x_vinflg = '1'
015800120411     C*
015900120411     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016000120411     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016100010305     C*
016200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016300120411     C                   EVAL      curspe=%trim(%subst(vindta:188:6))
016400010305     C*
016500071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016600080923     C                   if        *in50 = *off
016700120411     C                   exsr      inzvar
016800120411     C                   exsr      defcam
016900071003     C                   exsr      impvab
017000071003     C                   exsr      wrivab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300120411     C   51              exsr      wrivat_e                                     => carico VAT
017400071003     C                   else
017500080923     C*
017600071009     C                   if        wDISK = 'D'
017700091223     C                   exsr      repNSP
017800071009     C                   exsr      impvab
017900071009     C                   exsr      wrivab
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071009     C                   exsr      wrivat_b                                     => carico VAT
018200071009     C                   exsr      wrivat_e                                     => carico VAT
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800091223     C                   exsr      repNSP
018900020305     C                   exsr      impvab
019000100127     C                   exsr      wrivat_a                                     => carico VAT
019100071003     C                   exsr      wrivat_b                                     => carico VAT
019200071003     C   50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   else
019400020725     C                   if        depspe <> curspe                             => rottura di spediz
019500010305     C                   eval      depspe = curspe                              => memorizz. spediz
019600070928     C                   exsr      wrivab
019700080117     C                   clear                   tivinds
019800091223     C                   exsr      repNSP
019900020305     C                   exsr      impvab
020000100127     C                   exsr      wrivat_a                                     => carico VAT
020100071003     C                   exsr      wrivat_b                                     => carico VAT
020200071003     C   50              exsr      wrivat_e                                     => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400120411     C*
020500120411     C* Se nn richiesta esclusione spedizioni "duplicate"
020600120411     C                   select
020700120411     C                   when      wDUPREC = ' '
020800120411     C                   exsr      impvab
020900120411     C                   exsr      wrivat_a                                     => carico VAT
021000120411     C                   exsr      wrivat_b                                     => carico VAT
021100120411     C   50              exsr      wrivat_e                                     => carico VAT
021200120411     C                   when      wDUPREC = 'C'
021300120411     C   50              exsr      wrivat_e                                     => carico VAT
021400120411     C                   endsl
021500010305     C                   endif
021600010305     C                   endif
021700010305     C                   endif
021800071003     C                   endif
021900120411     C*
022000071009     C                   endif
022100000905     C*
022200000905     C                   else
022300020725     C                   eval      x_vinflg = '1'
022400000905     C                   endif
022500000905     C*
022600020725     C     VINRRN        chain     tivin000
022700020725     C                   if        %found(tivin00r)
022800020725     C                   eval      y_vinflg = x_vinflg
022900020725     C                   eval      y_vinmsg = x_vinmsg
023000020725     C                   update    tivin000
023100020725     C                   endif
023200020725     C*
023300020725     C/EXEC SQL
023400070530     C+ Fetch C1 into :INPUT_DS
023500020725     C/END-EXEC
023600020725     C*
023700020725     C                   enddo
023800020725     C*
023900020725     C/EXEC SQL
024000020725     C+ close C1
024100020725     C/END-EXEC
024200000905     C*
024300020305     C* Scarico i VAB rimasti "in sospeso"
024400071009     C                   if        wDISK = 'C'
024500071009     C                   exsr      wrivab
024600071009     C                   endif
024700010202     C*
024800010202     C                   endif
024900990910
025000990910     C* Se non ci sono record con errori ...
025100000710     C                   if        §ctrno = 0
025200990910     C* ... restituisco esito OK.
025300990921     C                   eval      wrkesito = '0'
025400990910     C                   else
025500120412     C                   if        §ctrokvb > 0 OR
025600120412     C                             §ctrokvt > 0
025700990921     C                   eval      wrkesito = '1'
025800000710     C                   else
025900000710     C                   eval      wrkesito = '2'
026000990910     C                   endif
026100000710     C                   endif
026200990910     C*
026300990914     C                   if        %open(tivin00r)
026400990908     C                   close     tivin00r
026500990914     C                   endif
026600100331     C                   if        %open(edivabwr)
026700100331     C                   close     edivabwr
026800990914     C                   endif
026900100331     C                   if        %open(edivatwr)
027000100331     C                   close     edivatwr
027100010201     C                   endif
027200990910     C*
027300120412     C                   if        (§ctrokvb > 0 OR
027400120412     C                              §ctrokvt > 0    )
027500000724     C                             and vlrpoi <> *zeros
027600010202     C                   exsr      invio
027700990920     C                   endif
027800990920     C*
027900910830     C                   ENDSR
028000000613     C***
028100010305
028200010305     C*----------------------------------------------------*
028300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028400010305     C*----------------------------------------------------*
028500020305     C     WRIVAB        BEGSR
028600070730     C*
028700071003     C* Considerazioni finali
028800071003     C                   if        VABRMA = *blanks
028900071003     C                   movel(P)  VABRMN        VABRMA
029000071003     C                   endif
029100100331     C*
029200100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
029300100331     C                   EVAL      VABCMR = %char(datcor)+'-'+%char(oracor)
029400100331     C                   EVAL      VABDCM = datcor
029500100331     C                   EVAL      VABDTS = datcor
029600100331     C                   EVAL      VABHMS = oracor
029700100331     C                   EVAL      VABCNT = 1
029800071003     C*
029900120411     C                   if        *in51 = *off and *in31 = *off
030000100331     C                   write     edivab00                                     => scarico il VAB
030100120411     C                   endif
030200010305     C*
030300010305     C                   ENDSR
030400990920
030500000801     C*----------------------------------------------------*
030600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700000801     C*----------------------------------------------------*
030800010201     C     INZVAR        BEGSR
030900000801     C*
031000010201     C                   Z-ADD     *zeros        Num5_0
031100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031200020725     C                   MOVEL     '0'           FlgCAS            1
031300000801     C*
031400000801     C                   ENDSR
031500000801     C*----------------------------------------------------*
031600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
031700000801     C*----------------------------------------------------*
031800020904     C     DEFCAM        BEGSR
031900080923     C*
032000120411     C                   SETOFF                                       505160
032100000801     C*
032200100331     C                   CLEAR                   EDIVAB00
032300100331     C                   CLEAR                   EDIVAT00
032400070730     C* Imposto i valori di default...
032500120411     C                   EVAL      VABCCM = 0435529
032600120411     C                   EVAL      VATCCM = 0435529
032700120411     C                   EVAL      VABLNP = 043
032800120411     C                   EVAL      VATLNP = 043
032900120411     C                   EVAL      VABCTR = 301
033000120502     C                   EVAL      VABCBO = '2'
033100070222     C* ... e poi verifico se sono stati passati come parametri
033200070222     C                   IF        vlrppt > *blanks
033300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033500070222     C                   EXSR      CHKNUM
033600070222     C                   IF        PiInt=*on
033700070222     C                   Z-ADD     PiVal         VABCCM
033800070222     C                   Z-ADD     PiVal         VATCCM
033900070222     C                   ENDIF
034000070222     C                   ENDIF
034100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABLNP
034600070222     C                   Z-ADD     PiVal         VATLNP
034700070222     C                   ENDIF
034800070222     C                   ENDIF
034900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABCTR
035400070222     C                   ENDIF
035500070928     C                   ENDIF
035600071009     C                   MOVEL     *blanks       wDISK             1
035700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035800071009     C                   IF        wDISK <> *blanks
035900070928     C                   SETON                                        50
036000070222     C                   ENDIF
036100120411     C                   IF        wDISK  = 'T'
036200120411     C                   SETOFF                                       50
036300120411     C                   SETON                                        51
036400120411     C                   ENDIF
036500120411     C                   MOVEL     *blanks       wDUPREC           1
036600120411     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036700120411     C                   IF        %subst(vlrppt:16:2) <> *blanks
036800120411     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036900120411     C                   ENDIF
037000120411     C*
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
038300120411     C*
038400120411     C                   SETON                                        60
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
040500000801     C*----------------------------------------------------*
040600100331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
040700000801     C*----------------------------------------------------*
040800010201     C     IMPVAB        BEGSR
040900070530     C*
041000070530     C                   SETOFF                                       3132
041100070928     C*
041200070928     C                   MOVE(P)   vlrpoi        VABFGS
041300070928     C                   MOVE(P)   vlrpoi        VATFGS
041400070928     C*
041500070928     C                   MOVEL     datcor        VABAAS
041600070928     C                   MOVEL     datcor        VATAAS
041700070928     C                   MOVE      datcor        VABMGS
041800120411     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
041900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
042100070928     C* ==
042200120411     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
042300120411     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
042400120411     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
042500120411     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
042600120411     C                   EVAL      VABRMA=%trim(%subst(vindta:311:14))
042700120411     C                   EVAL      VABNOT=%trim(%subst(vindta:196:35))
042800120411     C                   EVAL      VABNT2=%trim(%subst(vindta:231:35))
042900120411     C*
043000120411     C* Reperimento campi NUMERICI
043100120411     C*
043200120411     C* NSP
043300120411     C                   IF        *IN51 = *off
043400120411     C                   EVAL      PiStr=%subst(vindta:188:6)
043500120411     C                   EXSR      CHKNUM
043600120411     C                   IF        PiInt=*on
043700120411     C  N60              Z-ADD     PiVal         VABNSP
043800120411     C  N60              Z-ADD     PiVal         VATNSP
043900120411     C  N60              Z-ADD     PiVal         VABRMN
044000120411     C                   ELSE
044100120411     C                   SETON                                        32
044200120411     C                   Z-ADD     1             VABRMN
044300120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400120411     C                             + ' ' + 'VABNSP VATNSP'
044500120411     C                   ENDIF
044600120411     C*
044700120411     C                   ELSE
044800120411     C*
044900120411     C                   EVAL      PiStr=%subst(vindta:188:6)
045000120411     C                   EXSR      CHKNUM
045100120411     C                   IF        PiInt=*on
045200120411     C  N60              Z-ADD     PiVal         VATNSP
045300120411     C                   ELSE
045400120411     C                   SETON                                        32
045500120411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600120411     C                             + ' ' + 'VATNSP'
045700120411     C                   ENDIF
045800120411     C                   ENDIF
045900120411     C* RMN
046000120411     C                   EVAL      PiStr=%subst(vindta:188:6)
046100120411     C                   EXSR      CHKNUM
046200120411     C                   IF        PiInt=*on
046300120411     C                   Z-ADD     PiVal         VABRMN
046400120411     C                   ELSE
046500120411     C  N51              SETON                                        32
046600120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
046700120411     C                             + ' ' + 'VABRMN'
046800120411     C                   ENDIF
046900120411     C* NCL
047000120411     C                   EVAL      PiStr=%subst(vindta:164:5)
047100120411     C                   EXSR      CHKNUM
047200120411     C                   IF        PiInt=*on
047300120411     C                   Z-ADD     PiVal         VABNCL
047400120411     C                   ELSE
047500120411     C  N51              SETON                                        32
047600120411     C  N51              Z-ADD     *zeros        VABNCL
047700120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
047800120411     C                             + ' ' + 'VABNCL'
047900120411     C                   ENDIF
048000120411     C* PKB
048100120411     C                   EVAL      PiStr=%trim(%subst(vindta:169:8))
048200120411     C                   EXSR      CHKNUM
048300120411     C                   IF        PiNum=*on
048400120411     C* il peso ci arriva in grammi
048500120411-->  C                   EVAL(H)   PiVal=PiVal/1000
048600120411     C                   Z-ADD(H)  PiVal         VABPKB
048700120411     C                   ELSE
048800120411     C  N51              SETON                                        32
048900120411     C  N51              Z-ADD     *zeros        VABPKB
049000120411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049100120411     C                             + ' ' + 'VABPKB'
049200120411     C                   ENDIF
049300100310     C*
049400100310     C* Se provincia nn valorizzata la reperisco
049500100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
049600120412     C                   IF        VABNZD  = *blanks AND
049700120412     C                             VABPRD  = *blanks AND
049800120412     C                             VABCAD <> *blanks
049900100310     C                   CLEAR                   TISI95DS
050000100310     C                   EVAL      I95TCN = '3'
050100100310     C                   Z-ADD     datcor        I95DAT
050200100310     C                   EVAL      I95NAR = VABNZD
050300100310     C                   EVAL      I95CAP = VABCAD
050400100310     C                   EVAL      I95LOC = VABLOD
050500100310     C                   CALL      'TISI95R'
050600100310     C                   PARM                    TISI95DS
050700100310     C                   EVAL      VABPRD = O95PRV
050800100310     C                   ENDIF
050900070730     C*
051000070730     C* Considerazioni finali su CBO/CAS
051100070730     C                   IF        FlgCAS = '1'
051200070730     C                   IF        VABCBO = '1'
051300070730     C                   EVAL      VABCBO = '4'
051400070730     C                   ENDIF
051500070730     C                   IF        VABCBO = '2'
051600070730     C                   EVAL      VABCBO = '6'
051700070730     C                   ENDIF
051800070730     C                   ENDIF
051900010202     C*
052000000801     C* Ebbene...
052100000801     C                   ADD       1             §CTRMO
052200070530     C                   IF        *in31 <> *zeros OR
052300070530     C                             *in32 <> *zeros
052400000801     C                   ADD       1             §CTRNO
052500020725     C                   EVAL      x_vinflg = '2'
052600000801     C                   ELSE
052700010201     C                   ADD       1             §CTROKVB
052800000801     C                   ENDIF
052900000801     C*
053000000801     C                   ENDSR
053100100127     C*----------------------------------------------------*
053200100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
053300100127     C*----------------------------------------------------*
053400100127     C     WRIVAT_A      BEGSR
053500100127     C*
053600100331     C* Valorizzo l buffer di scrittura del EDIVAT
053700100127     C* - TIPO RECORD "A"
053800100331     C***                eval      VATTRC = 'A'
053900100331     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
054000100331     C***                exsr      wrivat
054100100127     C*
054200100127     C                   ENDSR
054300010201     C*----------------------------------------------------*
054400100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
054500010201     C*----------------------------------------------------*
054600071003     C     WRIVAT_B      BEGSR
054700010201     C*
054800100331     C* Valorizzo l buffer di scrittura del EDIVAT
054900070928     C* - TIPO RECORD "B"
055000100331     C***                eval      VATTRC = 'B'
055100100331     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
055200100331     C***                exsr      wrivat
055300010201     C*
055400010201     C                   ENDSR
055500120411     C*----------------------------------------------------*
055600120411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
055700120411     C*----------------------------------------------------*
055800120411     C     WRIVAT_E      BEGSR
055900120411     C*
056000120411     C* Valorizzo l buffer di scrittura del FIVAT
056100120411     C* - TIPO RECORD "E"
056200120411     C*
056300120411     C                   eval      VATTRC = 'E'
056400120629     C                   eval      VATNOT = %trim(%subst(vindta:325:14))
056500120629     C                   exsr      wrivat
056600120411     C*
056700120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
056800120411     C*
056900120411     C* Inizializzo progressivo collo su totale colli della spedizione
057000120629     C***                Z-ADD     1             wNumColloDA       2 0
057100120629     C***                MOVEL     *blanks       wNumColloCor      2
057200120629     C***                MOVEL     *blanks       wWhoIs            6
057300120629     C***                eval      wWhoIs = %trim(%subst(vindta:325:14))
057400120411     C*
057500120629     C***                DOW       wNumColloDA <= VABNCL
057600120629     C***                MOVEL(P)  wNumColloDA   wNumColloCor
057700120629     C***                EVAL      VATNOT =
057800120629     C***                             %subst(%char(%dec(%date() : *ISO)):3:2) +
057900120629     C***                             wNumColloCor + wWhoIs
058000120629     C***                exsr      wrivat
058100120629     C***                add       1             §CTROKVT
058200120411     C*
058300120629     C***                EVAL      wNumColloDA = wNumColloDA + 1
058400120629     C***                ENDDO
058500120411     C*
058600120411     C                   ENDSR
058700100127     C*----------------------------------------------------*
058800100331     C*  SCARICO BUFFER EDIVAT
058900100127     C*----------------------------------------------------*
059000100127     C     WRIVAT        BEGSR
059100100127     C*
059200100127     C                   if        vatNOT <> *blanks
059300100331     C*
059400100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
059500100331     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
059600100331     C                   EVAL      VATCNT = 1
059700100331     C*
059800100127     C                   ADD       1             §CTROKVT
059900100331     C                   write     EDIVAT00
060000100127     C                   endif
060100100127     C*
060200100127     C                   ENDSR
060300020904
060400020904
060500010202     C*----------------------------------------------------*
060600100331     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
060700010202     C*----------------------------------------------------*
060800120412     C     PREVASX       BEGSR
060900010202     C*
061000100331     C* Compongo il nome del membro da dare al EDIVATWR
061100010202     C                   eval      parmbr = vlrhdl
061200010202     C                   movel     'M'           parmbr
061300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061400010202     C                   eval      paropz = '1'
061500010202     C* Effettuo la chiamata al CLLE preposto
061600120411     C  N51              call(e)   'TITVEVTC'
061700010202     C                   parm                    parccm
061800010202     C                   parm                    parmbr
061900010202     C                   parm                    paropz
062000120411     C   51              call(e)   'TITVEVBC'
062100120411     C                   parm                    parccm
062200120411     C                   parm                    parmbr
062300120411     C                   parm                    paropz
062400120411     C*
062500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062600010202     C                   if        %error
062700010202     C                   movel     '1'           chkcall
062800010202     C                   else
062900010202     C                   movel     '0'           chkcall
063000010202     C                   endif
063100010202     C*
063200010202     C                   ENDSR
063300000801     C*----------------------------------------------------*
063400000801     C*  CONTROLLO NUMERICITA' CAMPI
063500000801     C*----------------------------------------------------*
063600000801     C     CHKNUM        BEGSR
063700081113     C*
063800081113     C                   IF        PiDecChr = *blanks
063900120412     C                   EVAL      PiDecChr = '.'
064000081113     C                   ENDIF
064100091223     C*
064200081113     C                   callp(e)  UBISNUM_Check(PiStr
064300081113     C                                          :PiDecChr
064400081113     C                                          :PiVal
064500081113     C                                          :PiNum
064600081113     C                                          :PiInt)
064700081113     C*
064800000801     C                   IF        %error
064900000801     C                   EVAL      PiInt=*off
065000000801     C                   ENDIF
065100000801     C*
065200000801     C                   ENDSR
065300000801     C***
065400000801
065500990920      /TITLE Invio dei dati al punto operativo.
065600010202     C     invio         BEGSR
065700990920     C*
065800120412     C* 1° invio EDIVAT
065900010201     C                   reset                   dscmz
066000120412     C                   move      vatfgs        cmzdst
066100100331     C                   eval      cmzfld = 'EDIVATWR'
066200010201     C                   eval      cmzmbd = vlrhdl
066300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400021009     C***                if        prmfir = *blanks
066500100331     C                   eval      cmzfla = 'EDIVAT0F'
066600100331     C                   eval      cmzmba = 'EDIVAT0F'
066700021009     C***                else
066800021009     C***                eval      cmzfla = prmfir
066900021009     C***                eval      cmzmba = prmfir
067000021009     C***                endif
067100010201     C                   eval      cmznrr = *zeros
067200020305     C                   move      §ctrokvt      cmznrr
067300021018     C                   eval      cmzlba = vlrfl1
067400010201     C                   call(e)   'TIS711C'
067500010201     C                   parm                    dscmz
067600010201     C                   parm      *blanks       esito
067700010205     C                   if        %error
067800010205     C                             or cmzerr = '1'
067900010205     C                             or esito  = '1'
068000010205     C                   eval      wrkesito = '3'
068100010205     C                   else
068200010201     C*
068300100331     C* 2° invio EDIVAB
068400120412     C                   if        *in51 = *off
068500010201     C                   reset                   dscmz
068600120412     C                   move      vabfgs        cmzdst
068700010201     C                   eval      cmzfld = vlrfou
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100100331     C                   eval      cmzfla = 'EDIVAB0F'
069200100331     C                   eval      cmzmba = 'EDIVAB0F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800010201     C                   move      §ctrokvb      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010201     C                   if        %error
070400010201     C                             or cmzerr = '1'
070500010201     C                             or esito  = '1'
070600010201     C                   eval      wrkesito = '3'
070700010201     C                   endif
070800010205     C                   endif
070900120412     C                   endif
071000990920     C*
071100000613     C                   ENDSR
071200000613     C***
071300070411
071400070411     C     *pssr         BEGSR
071500070411     C*
071600070411     C                   if        %open(tivin00r)
071700070411     C                   close     tivin00r
071800070411     C                   endif
071900100331     C                   if        %open(edivabwr)
072000100331     C                   close     edivabwr
072100070411     C                   endif
072200100331     C                   if        %open(edivatwr)
072300100331     C                   close     edivatwr
072400070411     C                   endif
072500070411     C*
072600070411     C* Effettuo la chiamata al CLLE preposto
072700120411     C  N51              call(e)   'TITVEVTC'
072800120411     C                   parm                    parccm
072900120411     C                   parm                    parmbr
073000120411     C                   parm      '2'           paropz
073100120411     C   51              call(e)   'TITVEVBC'
073200120411     C                   parm                    parccm
073300120411     C                   parm                    parmbr
073400120411     C                   parm      '2'           paropz
073500070411     C*
073600070411     C                   eval      wrkesito = '2'
073700070411     C*
073800070411     C                   seton                                        LR
073900070411     C*
074000070411     C                   ENDSR     '*CANCL'
074100070411     C***
074200070411
074300990910
074400000613     C     *inzsr        BEGSR
074500990910     C*
074600990910     C     *entry        plist
074700990920     C                   parm                    tivlrds
074800990921     C                   parm      wrkesito      esito
074900000724     C                   parm                    prmlit
075000000710     C                   parm                    prmfir
075100000613     C*
075200000830     C* CALCOLA LA DATA CORRENTE
075300100331     C                   time                    wn14             14 0
075400100331     C                   movel     wn14          oracor            6 0          *ORA
075500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075600091223     C                   eval      datcor = %dec(%date() : *ISO)
075700000830     C*
075800000613     C                   ENDSR
075900000613     C***
