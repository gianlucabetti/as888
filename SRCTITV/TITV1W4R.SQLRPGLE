000100100331      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002100100310     D depspe          s             10    INZ(*blanks)
002200100310     D curspe          s             10    INZ(*blanks)
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
003400020725     D  VINDTA                 1   2048
003500020725     D  VINFLG              2049   2049
003600020725     D  VINSPE              2050   2059
003700020725     D  VINRRN              2060   2067  0
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
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005400090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005500081113
005600990908
005700010201
005800010201
005900080117     C*
006000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100080117     C
006200080117     C/EXEC SQL
006300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400080117     C/END-EXEC
006500080117     C
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000613     C*
007000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007100000613     C*
007200010202     C* Effettuo la chiamata al CLLE preposto
007300100331     C                   call(e)   'TITVEVTC'
007400010202     C                   parm                    parccm
007500010202     C                   parm                    parmbr
007600010202     C                   parm      '2'           paropz
007700070730     C*
007800070730     C* Effettuo lancio TISI95 solo x chiusura
007900070730     C                   CLEAR                   TISI95DS
008000070730     C                   EVAL      I95TLA = 'C'
008100070730     C                   CALL      'TISI95R'
008200070730     C                   PARM                    TISI95DS
008300000616     C*
008400000801     C
008500010201     C                   seton                                        LR
008600990908
008700000801
008800910830     C*--------------------------------------------------------
008900100331     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009000910830     C*--------------------------------------------------------
009100070530     C     RWFILE        BEGSR
009200990910     C*
009300990914     C                   if        not %open(tivin00r)
009400990908     C                   open      tivin00r
009500990914     C                   endif
009600100331     C                   if        not %open(edivabwr)
009700100331     C                   open      edivabwr
009800990914     C                   endif
009900070530     C*
010000100331     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010100020305     C                   exsr      prevat
010200010201     C*
010300010202     C                   if        chkcall = '0'
010400010202     C*
010500100331     C                   if        not %open(edivatwr)
010600100331     C                   open      edivatwr
010700010201     C                   endif
010800080117     C*
010900080117     C                   EXSR      INZVAR
011000080117     C                   EXSR      DEFCAM
011100990910     C*
011200010201     C                   clear                   §CTROKVB          5 0
011300020305     C                   clear                   §CTROKVT          5 0
011400000801     C                   clear                   §CTRMO            5 0
011500000801     C                   clear                   §CTRNO            5 0
011600990910     C*
011700020725     C/EXEC SQL
011800020725     C+ declare C1 cursor for select
011900100331     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
012000060223     C+ from tivin00r
012100060223     C+ order by sped
012200020725     C+ for read only
012300020725     C/END-EXEC
012400020725     C
012500020725     C/EXEC SQL
012600020725     C+ open C1
012700020725     C/END-EXEC
012800020725     C
012900020725     C/EXEC SQL
013000070530     C+ Fetch C1 into :INPUT_DS
013100020725     C/END-EXEC
013200020725     C*
013300020725     C                   dow       sqlcod = *zeros
013400990913     C*
013500100310     C                   if        vindta > *blanks
013600000613     C                   add       1             rrnum
013700000801     C*
013800020725     C                   if        vinflg = *blanks
013900020725     C                             or vinflg = '0'
014000020725     C                             or vinflg = '2'
014100000801     C*
014200020725     C                   clear                   x_vinmsg
014300020725     C                   eval      x_vinflg = '1'
014400010305     C*
014500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014600100331     C                   EVAL      curspe=%trim(%subst(vindta:108:10))
014700010305     C*
014800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014900080923     C                   if        *in50 = *off
015000091223     C                   exsr      repNSP
015100071003     C                   exsr      impvab
015200071003     C                   exsr      wrivab
015300100127     C                   exsr      wrivat_a                                     => carico VAT
015400071003     C                   exsr      wrivat_b                                     => carico VAT
015500071003     C                   else
015600080923     C*
015700071009     C                   if        wDISK = 'D'
015800091223     C                   exsr      repNSP
015900071009     C                   exsr      impvab
016000071009     C                   exsr      wrivab
016100100127     C                   exsr      wrivat_a                                     => carico VAT
016200071009     C                   exsr      wrivat_b                                     => carico VAT
016300071009     C                   exsr      wrivat_e                                     => carico VAT
016400071009     C                   else
016500071009     C*
016600010305     C                   if        depspe = *blanks                             => 1° giro
016700010305     C                   eval      depspe = curspe                              => memorizz. spediz
016800080117     C                   clear                   tivinds
016900091223     C                   exsr      repNSP
017000020305     C                   exsr      impvab
017100100127     C                   exsr      wrivat_a                                     => carico VAT
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300071003     C   50              exsr      wrivat_e                                     => carico VAT
017400010305     C                   else
017500020725     C                   if        depspe <> curspe                             => rottura di spediz
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700070928     C                   exsr      wrivab
017800080117     C                   clear                   tivinds
017900091223     C                   exsr      repNSP
018000020305     C                   exsr      impvab
018100100127     C                   exsr      wrivat_a                                     => carico VAT
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400020305     C                   else                                                   => x stessa spediz
018500020305     C                   exsr      impvab
018600100127     C                   exsr      wrivat_a                                     => carico VAT
018700071003     C                   exsr      wrivat_b                                     => carico VAT
018800071003     C   50              exsr      wrivat_e                                     => carico VAT
018900010305     C                   endif
019000010305     C                   endif
019100010305     C                   endif
019200071003     C                   endif
019300071009     C                   endif
019400000905     C*
019500000905     C                   else
019600020725     C                   eval      x_vinflg = '1'
019700000905     C                   endif
019800000905     C*
019900020725     C     VINRRN        chain     tivin000
020000020725     C                   if        %found(tivin00r)
020100020725     C                   eval      y_vinflg = x_vinflg
020200020725     C                   eval      y_vinmsg = x_vinmsg
020300020725     C                   update    tivin000
020400020725     C                   endif
020500020725     C*
020600020725     C/EXEC SQL
020700070530     C+ Fetch C1 into :INPUT_DS
020800020725     C/END-EXEC
020900020725     C*
021000020725     C                   enddo
021100020725     C*
021200020725     C/EXEC SQL
021300020725     C+ close C1
021400020725     C/END-EXEC
021500000905     C*
021600020305     C* Scarico i VAB rimasti "in sospeso"
021700071009     C                   if        wDISK = 'C'
021800071009     C                   exsr      wrivab
021900071009     C                   endif
022000010202     C*
022100010202     C                   endif
022200990910
022300990910     C* Se non ci sono record con errori ...
022400000710     C                   if        §ctrno = 0
022500990910     C* ... restituisco esito OK.
022600990921     C                   eval      wrkesito = '0'
022700990910     C                   else
022800010201     C                   if        §ctrokvb > 0
022900990921     C                   eval      wrkesito = '1'
023000000710     C                   else
023100000710     C                   eval      wrkesito = '2'
023200990910     C                   endif
023300000710     C                   endif
023400990910     C*
023500990914     C                   if        %open(tivin00r)
023600990908     C                   close     tivin00r
023700990914     C                   endif
023800100331     C                   if        %open(edivabwr)
023900100331     C                   close     edivabwr
024000990914     C                   endif
024100100331     C                   if        %open(edivatwr)
024200100331     C                   close     edivatwr
024300010201     C                   endif
024400990910     C*
024500010201     C                   if        §ctrokvb > 0
024600000724     C                             and vlrpoi <> *zeros
024700010202     C                   exsr      invio
024800990920     C                   endif
024900990920     C*
025000910830     C                   ENDSR
025100000613     C***
025200010305
025300010305     C*----------------------------------------------------*
025400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025500010305     C*----------------------------------------------------*
025600020305     C     WRIVAB        BEGSR
025700070730     C*
025800071003     C* Considerazioni finali
025900071003     C                   if        VABRMA = *blanks
026000071003     C                   movel(P)  VABRMN        VABRMA
026100071003     C                   endif
026200100331     C*
026300100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
026400100331     C                   EVAL      VABCMR = %char(datcor)+'-'+%char(oracor)
026500100331     C                   EVAL      VABDCM = datcor
026600100331     C                   EVAL      VABDTS = datcor
026700100331     C                   EVAL      VABHMS = oracor
026800100331     C                   EVAL      VABCNT = 1
026900071003     C*
027000100331     C                   write     edivab00                                     => scarico il VAB
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
028900090310     C                   SETOFF                                       50
029000000801     C*
029100100331     C                   CLEAR                   EDIVAB00
029200100331     C                   CLEAR                   EDIVAT00
029300070730     C* Imposto i valori di default...
029400100331     C                   EVAL      VABCCM = 2491035
029500100331     C                   EVAL      VATCCM = 2491035
029600121108     C                   EVAL      VABLNP = 005
029700121108     C                   EVAL      VATLNP = 005
029800100429     C                   EVAL      VABCTR = 300
029900070730     C                   EVAL      VABCBO = '1'
030000100309     C                   EVAL      VABVCA = 'EUR'
030100100331     C                   EVAL      VABVAS = 'EUR'
030200070222     C* ... e poi verifico se sono stati passati come parametri
030300070222     C                   IF        vlrppt > *blanks
030400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABCCM
030900070222     C                   Z-ADD     PiVal         VATCCM
031000070222     C                   ENDIF
031100070222     C                   ENDIF
031200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABLNP
031700070222     C                   Z-ADD     PiVal         VATLNP
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABCTR
032500070222     C                   ENDIF
032600070928     C                   ENDIF
032700071009     C                   MOVEL     *blanks       wDISK             1
032800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032900071009     C                   IF        wDISK <> *blanks
033000070928     C                   SETON                                        50
033100070222     C                   ENDIF
033200070222     C                   ENDIF
033300071009     C*
033400071009     C   50              EVAL      VABCTM = '7Q'
033500000801     C*
033600000801     C                   ENDSR
033700091223     C*----------------------------------------------------*
033800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033900091223     C*----------------------------------------------------*
034000091223     C     REPNSP        BEGSR
034100091223     C*
034200091223     C                   EXSR      INZVAR
034300091223     C                   EXSR      DEFCAM
034400091223     C*
034500091223     C* NSP => Stacco un numeratore da AZNUM
034600091223     C                   clear                   TRUL33DS
034700091223     C                   eval      I33OPE = *zeros
034800091223     C                   eval      I33CNU = 302
034900091223     C                   eval      I33NUM = 1
035000091223     C                   movel     TRUL33DS      KPJBU
035100091223     C                   call      'TRUL33R'
035200091223     C                   parm                    KPJBA
035300091223     C                   movel     KPJBU         TRUL33DS
035400091223     C                   if        O33ERR = *zeros
035500091223     C                   z-add     O33NRF        VABNSP
035600091223     C                   z-add     O33NRF        VATNSP
035700091223     C                   else
035800091223     C                   SETON                                        31
035900091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036000091223     C                             + ' ' + 'VABNSP VATNSP'
036100091223     C                   endif
036200091223     C*
036300091223     C                   ENDSR
036400000801     C*----------------------------------------------------*
036500100331     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
036600000801     C*----------------------------------------------------*
036700010201     C     IMPVAB        BEGSR
036800070530     C*
036900070530     C                   SETOFF                                       3132
037000070928     C*
037100070928     C                   MOVE(P)   vlrpoi        VABFGS
037200070928     C                   MOVE(P)   vlrpoi        VATFGS
037300070928     C*
037400070928     C                   MOVEL     datcor        VABAAS
037500070928     C                   MOVEL     datcor        VATAAS
037600070928     C                   MOVE      datcor        VABMGS
037700100331     C                   EVAL      VABRSD=%trim(%subst(vindta:01:35))
037800070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037900070928     C     '@':'A'       XLATE     VABRSD        VABRSD
038000070928     C* ==
038100100331     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
038200100331     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
038300100331     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
038400100331     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
038500100331     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
038600100331     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
038700141003     C                   IF        %subst(vindta:137:1) = 'P'
038800141003     C                   EVAL      VABTSP='E'
038900141003     C                   ENDIF
039000100331     C* CBO
039100100331     C                   IF        %subst(vindta:207:1) = 'A'
039200100331     C                   EVAL      VABCBO = '2'
039300100331     C                   ELSE
039400100331     C                   EVAL      VABCBO = '1'
039500100310     C                   ENDIF
039600100309     C* NCL
039700100331     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
039800100309     C                   EXSR      CHKNUM
039900100309     C                   IF        PiInt=*on
040000100309     C                   Z-ADD     PiVal         VABNCL
040100100309     C                   ELSE
040200100309     C                   SETON                                        32
040300100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400100309     C                             + ' ' + 'VABNCL'
040500100309     C                   ENDIF
040600100309     C* PKB
040700100331     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
040800100309     C                   EXSR      CHKNUM
040900100309     C                   IF        PiNum=*on
041000100309     C                   Z-ADD     PiVal         VABPKB
041100100309     C                   ELSE
041200100309     C                   SETON                                        32
041300100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041400100309     C                             + ' ' + 'VABPKB'
041500100309     C                   ENDIF
041600071003     C* CAD
041700100331     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
041800070928     C                   EXSR      CHKNUM
041900070928     C                   IF        PiInt=*on
042000070928     C                   Z-ADD     PiVal         Num5_0
042100070928     C                   MOVEL     Num5_0        VABCAD
042200070928     C                   ELSE
042300070928     C                   SETON                                        32
042400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042500070928     C                             + ' ' + 'VABCAD'
042600070928     C                   ENDIF
042700080923     C* RMN
042800100331     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
042900080923     C                   EXSR      CHKNUM
043000080923     C                   IF        PiInt=*on
043100080923     C                   Z-ADD     PiVal         VABRMN
043200080923     C                   ELSE
043300080923     C                   SETON                                        32
043400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500080923     C                             + ' ' + 'VABRMN'
043600080923     C                   ENDIF
043700100331     C* CAS
043800141003     C                   IF        %trim(%subst(vindta:138:9))<>*blanks
043900100331     C                             AND
044000141003     C                             %trim(%subst(vindta:138:9))<>*zeros
044100100331     C                             AND
044200141003     C                             %trim(%subst(vindta:138:9))<>'0,0'
044300100331     C                             AND
044400141003     C                             %trim(%subst(vindta:138:9))<>'0.0'
044500100331     C                             AND
044600141003     C                             %trim(%subst(vindta:138:9))<>'000000,00'
044700100331     C                             AND
044800141003     C                             %trim(%subst(vindta:138:9))<>'000000.00'
044900100331     C                   MOVEL     '1'           FlgCAS
045000141003     C                   EVAL      PiStr=%trim(%subst(vindta:138:9))
045100100331     C                   EXSR      CHKNUM
045200100331     C                   IF        PiNum=*on
045300100331     C                   Z-ADD     PiVal         VABCAS
045400100331     C                   ELSE
045500100331     C                   SETON                                        32
045600100331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045700100331     C                             + ' ' + 'VABCAS'
045800100331     C                   ENDIF
045900100331     C                   ENDIF
046000100331     C* IAS
046100100331     C                   IF        %trim(%subst(vindta:223:11))<>*blanks
046200100331     C                             AND
046300100331     C                             %trim(%subst(vindta:223:11))<>*zeros
046400100331     C                             AND
046500100331     C                             %trim(%subst(vindta:223:11))<>'0,0'
046600100331     C                             AND
046700100331     C                             %trim(%subst(vindta:223:11))<>'0.0'
046800100331     C                             AND
046900100331     C                             %trim(%subst(vindta:223:11))<>'0000000,00'
047000100331     C                             AND
047100100331     C                             %trim(%subst(vindta:223:11))<>'0000000.00'
047200100331     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
047300100331     C                   EXSR      CHKNUM
047400100331     C                   IF        PiNum=*on
047500100331     C                   Z-ADD     PiVal         VABIAS
047600100331     C                   ELSE
047700100331     C                   SETON                                        32
047800100331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047900100331     C                             + ' ' + 'VABIAS'
048000100331     C                   ENDIF
048100100331     C                   ENDIF
048200100310     C*
048300100310     C* Se provincia nn valorizzata la reperisco
048400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
048500100310     C                   IF        VABNZD = *blanks AND
048600100310     C                             VABPRD = *blanks
048700100310     C                   CLEAR                   TISI95DS
048800100310     C                   EVAL      I95TCN = '3'
048900100310     C                   Z-ADD     datcor        I95DAT
049000100310     C                   EVAL      I95NAR = VABNZD
049100100310     C                   EVAL      I95CAP = VABCAD
049200100310     C                   EVAL      I95LOC = VABLOD
049300100310     C                   CALL      'TISI95R'
049400100310     C                   PARM                    TISI95DS
049500100310     C                   EVAL      VABPRD = O95PRV
049600100310     C                   ENDIF
049700070730     C*
049800070730     C* Considerazioni finali su CBO/CAS
049900070730     C                   IF        FlgCAS = '1'
050000070730     C                   IF        VABCBO = '1'
050100070730     C                   EVAL      VABCBO = '4'
050200070730     C                   ENDIF
050300070730     C                   IF        VABCBO = '2'
050400070730     C                   EVAL      VABCBO = '6'
050500070730     C                   ENDIF
050600070730     C                   ENDIF
050700020305     C*
050800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050900011113     C                   EXSR      CHKIMPDIV
051000010202     C*
051100000801     C* Ebbene...
051200000801     C                   ADD       1             §CTRMO
051300070530     C                   IF        *in31 <> *zeros OR
051400070530     C                             *in32 <> *zeros
051500000801     C                   ADD       1             §CTRNO
051600020725     C                   EVAL      x_vinflg = '2'
051700000801     C                   ELSE
051800010201     C                   ADD       1             §CTROKVB
051900000801     C                   ENDIF
052000000801     C*
052100000801     C                   ENDSR
052200100127     C*----------------------------------------------------*
052300100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
052400100127     C*----------------------------------------------------*
052500100127     C     WRIVAT_A      BEGSR
052600100127     C*
052700100331     C* Valorizzo l buffer di scrittura del EDIVAT
052800100127     C* - TIPO RECORD "A"
052900100331     C***                eval      VATTRC = 'A'
053000100331     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
053100100331     C***                exsr      wrivat
053200100127     C*
053300100127     C                   ENDSR
053400010201     C*----------------------------------------------------*
053500100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
053600010201     C*----------------------------------------------------*
053700071003     C     WRIVAT_B      BEGSR
053800010201     C*
053900100331     C* Valorizzo l buffer di scrittura del EDIVAT
054000070928     C* - TIPO RECORD "B"
054100100331     C***                eval      VATTRC = 'B'
054200100331     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
054300100331     C***                exsr      wrivat
054400010201     C*
054500010201     C                   ENDSR
054600071003     C*----------------------------------------------------*
054700100331     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
054800071003     C*----------------------------------------------------*
054900071003     C     WRIVAT_E      BEGSR
055000071003     C*
055100100331     C* Valorizzo l buffer di scrittura del EDIVAT
055200071003     C* - TIPO RECORD "E"
055300100310     C                   eval      VATTRC = 'E'
055400100331     C*
055500100331     C* X ogni collo indicato in testata
055600100331     C                   z-add     1             wColli            5 0
055700100331     C                   dow       wColli <= VABNCL
055800100331     C*
055900100331     C* Valorizzo il buffer di scrittura del FIVAT
056000100331     C                   move(p)   wColli        wColliAlfa        2
056100100331     C                   eval      VATNOT = %trim(VABRMA)+ wColliAlfa
056200100331     C*
056300100331     C                   exsr      wrivat                                       => scarico VAT
056400100331     C*
056500100331     C                   add       1             wColli
056600100331     C                   enddo
056700071003     C*
056800071003     C                   ENDSR
056900100127     C*----------------------------------------------------*
057000100331     C*  SCARICO BUFFER EDIVAT
057100100127     C*----------------------------------------------------*
057200100127     C     WRIVAT        BEGSR
057300100127     C*
057400100127     C                   if        vatNOT <> *blanks
057500100331     C*
057600100331     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
057700100331     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
057800100331     C                   EVAL      VATCNT = 1
057900100331     C*
058000100127     C                   ADD       1             §CTROKVT
058100100331     C                   write     EDIVAT00
058200100127     C                   endif
058300100127     C*
058400100127     C                   ENDSR
058500020904
058600020904
058700010202     C*----------------------------------------------------*
058800100331     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
058900010202     C*----------------------------------------------------*
059000020305     C     PREVAT        BEGSR
059100010202     C*
059200100331     C* Compongo il nome del membro da dare al EDIVATWR
059300010202     C                   eval      parmbr = vlrhdl
059400010202     C                   movel     'M'           parmbr
059500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
059600010202     C                   eval      paropz = '1'
059700010202     C* Effettuo la chiamata al CLLE preposto
059800100331     C                   call(e)   'TITVEVTC'
059900010202     C                   parm                    parccm
060000010202     C                   parm                    parmbr
060100010202     C                   parm                    paropz
060200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060300010202     C                   if        %error
060400010202     C                   movel     '1'           chkcall
060500010202     C                   else
060600010202     C                   movel     '0'           chkcall
060700010202     C                   endif
060800010202     C*
060900010202     C                   ENDSR
061000000801     C*----------------------------------------------------*
061100000801     C*  CONTROLLO NUMERICITA' CAMPI
061200000801     C*----------------------------------------------------*
061300000801     C     CHKNUM        BEGSR
061400081113     C*
061500081113     C                   IF        PiDecChr = *blanks
061600091223     C                   EVAL      PiDecChr = ','
061700081113     C                   ENDIF
061800091223     C*
061900081113     C                   callp(e)  UBISNUM_Check(PiStr
062000081113     C                                          :PiDecChr
062100081113     C                                          :PiVal
062200081113     C                                          :PiNum
062300081113     C                                          :PiInt)
062400081113     C*
062500000801     C                   IF        %error
062600000801     C                   EVAL      PiInt=*off
062700000801     C                   ENDIF
062800000801     C*
062900000801     C                   ENDSR
063000000801     C***
063100000801
063200011113
063300011113     C*----------------------------------------------------*
063400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063500011113     C*----------------------------------------------------*
063600011113     C     CHKIMPDIV     BEGSR
063700011113     C*
063800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063900011113     C                   Z-ADD     *zeros        wrkDec            9 9
064000011113     C*
064100011113     C* Come prima cosa effettuo considerazioni sulla divisa
064200011113     C                   IF        vabIAS > *zeros
064300011113     C                   IF        vabVAS <> 'EUR'
064400011113     C                   EVAL      vabVAS =  'ITL'
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C                   IF        vabCAS > *zeros
064900011113     C                   IF        vabVCA <> 'EUR'
065000011113     C                   EVAL      vabVCA =  'ITL'
065100011113     C                   ENDIF
065200011113     C                   ENDIF
065300011113     C*
065400011113     C                   IF        vabVMD > *zeros
065500020305     C                   IF        vabVAD <> 'EUR'
065600011113     C                   EVAL      vabVAD =  'ITL'
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066100011113     C                   Z-ADD     vabIAS        wrkDec
066200011113     C                   IF        wrkDec > *zeros
066300011113     C                   IF        vabVAS = 'ITL'
066400011113     C                   EVAL      vabIAS = *zeros
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066900011113     C                   Z-ADD     vabCAS        wrkDec
067000011113     C                   IF        wrkDec > *zeros
067100011113     C                   IF        vabVCA = 'ITL'
067200011113     C                   EVAL      vabCAS = *zeros
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067700011113     C                   Z-ADD     vabVMD        wrkDec
067800011113     C                   IF        wrkDec > *zeros
067900011113     C                   IF        vabVAD = 'ITL'
068000011113     C                   EVAL      vabVMD = *zeros
068100011113     C                   ENDIF
068200011113     C                   ENDIF
068300011113     C*
068400011113     C                   ENDSR
068500011113     C***
068600011113
068700011113
068800000801
068900000801
069000990920      /TITLE Invio dei dati al punto operativo.
069100010202     C     invio         BEGSR
069200990920     C*
069300100331     C* 1° invio EDIVAT
069400010201     C                   reset                   dscmz
069500010201     C                   move      vlrpoi        cmzdst
069600100331     C                   eval      cmzfld = 'EDIVATWR'
069700010201     C                   eval      cmzmbd = vlrhdl
069800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069900021009     C***                if        prmfir = *blanks
070000100331     C                   eval      cmzfla = 'EDIVAT0F'
070100100331     C                   eval      cmzmba = 'EDIVAT0F'
070200021009     C***                else
070300021009     C***                eval      cmzfla = prmfir
070400021009     C***                eval      cmzmba = prmfir
070500021009     C***                endif
070600010201     C                   eval      cmznrr = *zeros
070700020305     C                   move      §ctrokvt      cmznrr
070800021018     C                   eval      cmzlba = vlrfl1
070900010201     C                   call(e)   'TIS711C'
071000010201     C                   parm                    dscmz
071100010201     C                   parm      *blanks       esito
071200010205     C                   if        %error
071300010205     C                             or cmzerr = '1'
071400010205     C                             or esito  = '1'
071500010205     C                   eval      wrkesito = '3'
071600010205     C                   else
071700010201     C*
071800100331     C* 2° invio EDIVAB
071900010201     C                   reset                   dscmz
072000010201     C                   move      vlrpoi        cmzdst
072100010201     C                   eval      cmzfld = vlrfou
072200010201     C                   eval      cmzmbd = vlrhdl
072300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072400021009     C***                if        prmfir = *blanks
072500100331     C                   eval      cmzfla = 'EDIVAB0F'
072600100331     C                   eval      cmzmba = 'EDIVAB0F'
072700021009     C***                else
072800021009     C***                eval      cmzfla = prmfir
072900021009     C***                eval      cmzmba = prmfir
073000021009     C***                endif
073100010201     C                   eval      cmznrr = *zeros
073200010201     C                   move      §ctrokvb      cmznrr
073300021018     C                   eval      cmzlba = vlrfl1
073400010201     C                   call(e)   'TIS711C'
073500010201     C                   parm                    dscmz
073600010201     C                   parm      *blanks       esito
073700010201     C                   if        %error
073800010201     C                             or cmzerr = '1'
073900010201     C                             or esito  = '1'
074000010201     C                   eval      wrkesito = '3'
074100010201     C                   endif
074200010205     C                   endif
074300990920     C*
074400000613     C                   ENDSR
074500000613     C***
074600070411
074700070411     C     *pssr         BEGSR
074800070411     C*
074900070411     C                   if        %open(tivin00r)
075000070411     C                   close     tivin00r
075100070411     C                   endif
075200100331     C                   if        %open(edivabwr)
075300100331     C                   close     edivabwr
075400070411     C                   endif
075500100331     C                   if        %open(edivatwr)
075600100331     C                   close     edivatwr
075700070411     C                   endif
075800070411     C*
075900070411     C* Effettuo la chiamata al CLLE preposto
076000100331     C                   call(e)   'TITVEVTC'
076100070411     C                   parm                    parccm
076200070411     C                   parm                    parmbr
076300070411     C                   parm      '2'           paropz
076400070411     C*
076500070411     C                   eval      wrkesito = '2'
076600070411     C*
076700070411     C                   seton                                        LR
076800070411     C*
076900070411     C                   ENDSR     '*CANCL'
077000070411     C***
077100070411
077200990910
077300000613     C     *inzsr        BEGSR
077400990910     C*
077500990910     C     *entry        plist
077600990920     C                   parm                    tivlrds
077700990921     C                   parm      wrkesito      esito
077800000724     C                   parm                    prmlit
077900000710     C                   parm                    prmfir
078000000613     C*
078100000830     C* CALCOLA LA DATA CORRENTE
078200100331     C                   time                    wn14             14 0
078300100331     C                   movel     wn14          oracor            6 0          *ORA
078400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078500091223     C                   eval      datcor = %dec(%date() : *ISO)
078600000830     C*
078700000613     C                   ENDSR
078800000613     C***
