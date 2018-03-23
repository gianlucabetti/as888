000100100713      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300130115
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500100713     FEDIVABwr  O    E             DISK    usropn
000600100713     FEDIVATwr  O    E             DISK    usropn
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
002100100713     D depspe          s             10    INZ(*blanks)
002200100713     D curspe          s             10    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900100713     D wNomeFile       s             30    INZ(*blanks)
003000100713
003100100713     D*------------------
003200100713     D* Costanti
003300100713     D*------------------
003400100713     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003500100713     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003600020725
003700020725     D*------------------
003800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003900020725     D*------------------
004000070530     D INPUT_DS        DS                  INZ
004100020725     D  VINDTA                 1   2048
004200020725     D  VINFLG              2049   2049
004300020725     D  VINSPE              2050   2059
004400020725     D  VINRRN              2060   2067  0
004500020725     D*
004600091223
004700081113
004800081113     D*------------------
004900081113     D* LINKING A DEFINIZIONI ESTERNE
005000081113     D*------------------
005100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005200090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005300081113
005400990908
005500010201
005600010201
005700080117     C*
005800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005900080117     C
006000080117     C/EXEC SQL
006100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006200080117     C/END-EXEC
006300080117     C
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000613     C*
006800100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
006900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007000000613     C*
007100010202     C* Effettuo la chiamata al CLLE preposto
007200100714     C  N51              call(e)   'TITVEVTC'
007300100525     C                   parm                    parccm
007400100525     C                   parm                    parmbr
007500100525     C                   parm      '2'           paropz
007600100714     C   51              call(e)   'TITVEVBC'
007700100525     C                   parm                    parccm
007800100525     C                   parm                    parmbr
007900100525     C                   parm      '2'           paropz
008000070730     C*
008100070730     C* Effettuo lancio TISI95 solo x chiusura
008200070730     C                   CLEAR                   TISI95DS
008300070730     C                   EVAL      I95TLA = 'C'
008400070730     C                   CALL      'TISI95R'
008500070730     C                   PARM                    TISI95DS
008600000616     C*
008700000801     C
008800010201     C                   seton                                        LR
008900990908
009000000801
009100910830     C*--------------------------------------------------------
009200100713     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009300910830     C*--------------------------------------------------------
009400070530     C     RWFILE        BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900070530     C*
010000100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
010100100525     C                   exsr      prevasx
010200010201     C*
010300010202     C                   if        chkcall = '0'
010400010202     C*
010500100713     C                   if        not %open(edivabwr)
010600100713     C                   open      edivabwr
010700100525     C                   endif
010800100525     C*
010900100713     C                   if        not %open(edivatwr)
011000100713     C                   open      edivatwr
011100010201     C                   endif
011200080117     C*
011300100714     C                   exsr      inzvar
011400100714     C                   exsr      defcam
011500990910     C*
011600010201     C                   clear                   §CTROKVB          5 0
011700020305     C                   clear                   §CTROKVT          5 0
011800000801     C                   clear                   §CTRMO            5 0
011900000801     C                   clear                   §CTRNO            5 0
012000990910     C*
012100020725     C/EXEC SQL
012200020725     C+ declare C1 cursor for select
012300100713     C+ vindta, vinflg, substr(vindta, 20, 10) as sped, rrn(tivin00r)
012400060223     C+ from tivin00r
012500060223     C+ order by sped
012600020725     C+ for read only
012700020725     C/END-EXEC
012800020725     C
012900020725     C/EXEC SQL
013000020725     C+ open C1
013100020725     C/END-EXEC
013200020725     C
013300020725     C/EXEC SQL
013400070530     C+ Fetch C1 into :INPUT_DS
013500020725     C/END-EXEC
013600020725     C*
013700020725     C                   dow       sqlcod = *zeros
013800990913     C*
013900100310     C                   if        vindta > *blanks
014000000613     C                   add       1             rrnum
014100000801     C*
014200020725     C                   if        vinflg = *blanks
014300020725     C                             or vinflg = '0'
014400020725     C                             or vinflg = '2'
014500000801     C*
014600020725     C                   clear                   x_vinmsg
014700020725     C                   eval      x_vinflg = '1'
014800010305     C*
014900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015000100713     C                   EVAL      curspe=%trim(%subst(vindta:20:10))
015100010305     C*
015200100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400071003     C                   exsr      impvab
015500100525     C                   exsr      wrivab
015600100127     C                   exsr      wrivat_a                                     => carico VAT
015700071003     C                   exsr      wrivat_b                                     => carico VAT
015800100525     C   51              exsr      wrivat_e                                     => carico VAT
015900071003     C                   else
016000080923     C*
016100071009     C                   if        wDISK = 'D'
016200100714     C                   exsr      inzvar
016300100714     C                   exsr      defcam
016400071009     C                   exsr      impvab
016500100412     C                   exsr      wrivab
016600100127     C                   exsr      wrivat_a                                     => carico VAT
016700071009     C                   exsr      wrivat_b                                     => carico VAT
016800071009     C                   exsr      wrivat_e                                     => carico VAT
016900071009     C                   else
017000071009     C*
017100010305     C                   if        depspe = *blanks                             => 1° giro
017200010305     C                   eval      depspe = curspe                              => memorizz. spediz
017300080117     C                   clear                   tivinds
017400100714     C                   exsr      inzvar
017500100714     C                   exsr      defcam
017600020305     C                   exsr      impvab
017700100127     C                   exsr      wrivat_a                                     => carico VAT
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300100412     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500100714     C                   exsr      inzvar
018600100714     C                   exsr      defcam
018700020305     C                   exsr      impvab
018800100127     C                   exsr      wrivat_a                                     => carico VAT
018900071003     C                   exsr      wrivat_b                                     => carico VAT
019000071003     C   50              exsr      wrivat_e                                     => carico VAT
019100020305     C                   else                                                   => x stessa spediz
019200100401     C*
019300100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019400100412     C                   if        wDUPREC <> 'N'
019500020305     C                   exsr      impvab
019600100127     C                   exsr      wrivat_a                                     => carico VAT
019700071003     C                   exsr      wrivat_b                                     => carico VAT
019800071003     C   50              exsr      wrivat_e                                     => carico VAT
019900100401     C                   endif
020000010305     C                   endif
020100010305     C                   endif
020200010305     C                   endif
020300071003     C                   endif
020400071009     C                   endif
020500000905     C*
020600000905     C                   else
020700020725     C                   eval      x_vinflg = '1'
020800000905     C                   endif
020900000905     C*
021000020725     C     VINRRN        chain     tivin000
021100020725     C                   if        %found(tivin00r)
021200020725     C                   eval      y_vinflg = x_vinflg
021300020725     C                   eval      y_vinmsg = x_vinmsg
021400020725     C                   update    tivin000
021500020725     C                   endif
021600020725     C*
021700020725     C/EXEC SQL
021800070530     C+ Fetch C1 into :INPUT_DS
021900020725     C/END-EXEC
022000020725     C*
022100020725     C                   enddo
022200020725     C*
022300020725     C/EXEC SQL
022400020725     C+ close C1
022500020725     C/END-EXEC
022600000905     C*
022700020305     C* Scarico i VAB rimasti "in sospeso"
022800071009     C                   if        wDISK = 'C'
022900100412     C                   exsr      wrivab
023000071009     C                   endif
023100010202     C*
023200010202     C                   endif
023300990910
023400990910     C* Se non ci sono record con errori ...
023500000710     C                   if        §ctrno = 0
023600990910     C* ... restituisco esito OK.
023700990921     C                   eval      wrkesito = '0'
023800990910     C                   else
023900100525     C                   if        §ctrokvb > 0 OR
024000100525     C                             §ctrokvt > 0
024100990921     C                   eval      wrkesito = '1'
024200000710     C                   else
024300000710     C                   eval      wrkesito = '2'
024400990910     C                   endif
024500000710     C                   endif
024600990910     C*
024700990914     C                   if        %open(tivin00r)
024800990908     C                   close     tivin00r
024900990914     C                   endif
025000100713     C                   if        %open(edivabwr)
025100100713     C                   close     edivabwr
025200990914     C                   endif
025300100713     C                   if        %open(edivatwr)
025400100713     C                   close     edivatwr
025500010201     C                   endif
025600990910     C*
025700100525     C                   if        §ctrokvb > 0 OR
025800100525     C                             §ctrokvt > 0
025900000724     C                             and vlrpoi <> *zeros
026000010202     C                   exsr      invio
026100990920     C                   endif
026200990920     C*
026300910830     C                   ENDSR
026400000613     C***
026500100412
026600100412
026700010305
026800010305     C*----------------------------------------------------*
026900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027000010305     C*----------------------------------------------------*
027100020305     C     WRIVAB        BEGSR
027200100713     C*
027300100713     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
027400100714     C                   EVAL      VABCMR = %char(VABCCM)
027500100713     C                   EVAL      VABDCM = DATCOR
027600100713     C                   EVAL      VABDTS = DATCOR
027700100713     C                   EVAL      VABHMS = ORACOR
027800100713     C                   EVAL      VABCNT = 1
027900100412     C*
028000100412     C* Gestisco l'eventuale rottura x numero spedizione
028100100412     C                   if        VABRMA = *blanks
028200100412     C                   movel(P)  VABRMN        VABRMA
028300100412     C                   endif
028400100412     C*
028500100525     C                   if        *in51 = *off and *in31 = *off
028600100713     C                   write     edivab00                                     => scarico il VAB
028700100525     C                   endif
028800010305     C*
028900010305     C                   ENDSR
029000990920
029100000801     C*----------------------------------------------------*
029200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300000801     C*----------------------------------------------------*
029400010201     C     INZVAR        BEGSR
029500000801     C*
029600010201     C                   Z-ADD     *zeros        Num5_0
029700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029800020725     C                   MOVEL     '0'           FlgCAS            1
029900100713     C                   MOVEL     *blanks       wNOTE            70
030000000801     C*
030100000801     C                   ENDSR
030200000801     C*----------------------------------------------------*
030300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030400000801     C*----------------------------------------------------*
030500020904     C     DEFCAM        BEGSR
030600080923     C*
030700100525     C                   SETOFF                                       5051
030800000801     C*
030900100713     C                   CLEAR                   EDIVAB00
031000100713     C                   CLEAR                   EDIVAT00
031100100713     C* Imposto i valori di default...
031200100713     C                   EVAL      VABCCM = 2220929
031300100713     C                   EVAL      VABLNP = 222
031400100713     C                   EVAL      VABCTR = 001
031500100713     C                   EVAL      VABTSP = 'C'
031600100713     C                   EVAL      VABCBO = '1'
031700070222     C* ... e poi verifico se sono stati passati come parametri
031800070222     C                   IF        vlrppt > *blanks
031900100525     C*
032000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABCCM
032500070222     C                   Z-ADD     PiVal         VATCCM
032600070222     C                   ENDIF
032700070222     C                   ENDIF
032800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033000070222     C                   EXSR      CHKNUM
033100070222     C                   IF        PiInt=*on
033200070222     C                   Z-ADD     PiVal         VABLNP
033300070222     C                   Z-ADD     PiVal         VATLNP
033400070222     C                   ENDIF
033500070222     C                   ENDIF
033600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
033700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033800070222     C                   EXSR      CHKNUM
033900070222     C                   IF        PiInt=*on
034000070222     C                   Z-ADD     PiVal         VABCTR
034100070222     C                   ENDIF
034200070928     C                   ENDIF
034300071009     C                   MOVEL     *blanks       wDISK             1
034400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
034500071009     C                   IF        wDISK <> *blanks
034600070928     C                   SETON                                        50
034700070222     C                   ENDIF
034800100525     C                   IF        wDISK  = 'T'
034900100525     C                   SETOFF                                       50
035000100525     C                   SETON                                        51
035100100525     C                   ENDIF
035200100401     C                   MOVEL     *blanks       wDUPREC           1
035300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
035400100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
035500100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
035600100525     C                   ENDIF
035700100525     C*
035800070222     C                   ENDIF
035900071009     C*
036000071009     C   50              EVAL      VABCTM = '7Q'
036100000801     C*
036200000801     C                   ENDSR
036300000801     C*----------------------------------------------------*
036400100713     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
036500000801     C*----------------------------------------------------*
036600010201     C     IMPVAB        BEGSR
036700070530     C*
036800070530     C                   SETOFF                                       3132
036900070928     C*
037000070928     C                   MOVE(P)   vlrpoi        VABFGS
037100070928     C*
037200070928     C                   MOVEL     datcor        VABAAS
037300070928     C                   MOVE      datcor        VABMGS
037400100713     C*
037500100713     C* Porto tutto il buffer d input in maiuscolo
037600100713     C     minu:maiu     XLATE     vindta        vindta
037700100713     C*
037800100713     C* Reperimento campi ALFA
037900100713     C                   EVAL      VABRSD=%trim(%subst(vindta:74:35))
038000100714     C                   EVAL      VABIND=%trim(%subst(vindta:109:40))+' '+
038100100715     C                                    %trim(%subst(vindta:624:40))
038200100713     C                   EVAL      VABLOD=%trim(%subst(vindta:149:40))
038300100713     C                   EVAL      VABPRD=%trim(%subst(vindta:199:2))
038400100713     C***                EVAL      VABRMA=%trim(%subst(vindta:20:10))
038500100713     C                   EVAL      VABRMA=%trim(%subst(vindta:38:10))
038600100713     C                   IF        %trim(%subst(vindta:598:4)) = 'ZESP'
038700100713     C                   EVAL      VABTSP='E'
038800100713     C                   ENDIF
038900100713     C                   IF        %trim(%subst(vindta:235:1)) = 'F'
039000100713     C                   EVAL      VABCBO='1'
039100100713     C                   ENDIF
039200100713     C                   IF        %trim(%subst(vindta:235:1)) = 'A'
039300100713     C                   EVAL      VABCBO='2'
039400100713     C                   ENDIF
039500100713     C                   IF        %trim(%subst(vindta:225:10)) = 'TASSATIVA'
039600100713     C                   EVAL      VABTCR=*blanks
039700100713     C                   ENDIF
039800100713     C                   IF        %trim(%subst(vindta:225:10)) = 'ENTRO IL'
039900100713     C                   EVAL      VABTCR='P'
040000100713     C                   ENDIF
040100100713     C                   IF        %trim(%subst(vindta:225:10)) = 'DOPO IL'
040200100713     C                   EVAL      VABTCR='D'
040300100713     C                   ENDIF
040400100713     C                   EVAL      wNOTE=%trim(%subst(vindta:247:75))+
040500100713     C                                   %trim(%subst(vindta:322:75))+
040600100713     C                                   %trim(%subst(vindta:397:75))+
040700100713     C                                   %trim(%subst(vindta:472:75))
040800100713     C                   EVAL      VABNOT=%subst(wNOTE:01:35)
040900100713     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
041000100713     C*
041100100713     C* Considerazioni sulla ragione sociale del destinatario da indicare
041200100713     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041300100713     C     '@':'A'       XLATE     VABRSD        VABRSD
041400100713     C* ==
041500100713     C*
041600100713     C* Reperimento campi NUMERICI
041700100713     C* NSP
041800100715     C                   EVAL      PiStr=%trim(%subst(vindta:38:10))
041900100713     C                   EXSR      CHKNUM
042000100713     C                   IF        PiInt=*on
042100100713     C                   Z-ADD     PiVal         VABNSP
042200100713     C                   ELSE
042300100713     C                   SETON                                        31
042400100713     C                   Z-ADD     *zeros        VABNSP
042500100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042600100713     C                             + ' ' + 'VABNSP'
042700100713     C                   ENDIF
042800100713     C* RMN
042900100713     C***                EVAL      PiStr=%trim(%subst(vindta:38:10))
043000100713     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
043100100713     C                   EXSR      CHKNUM
043200100713     C                   IF        PiInt=*on
043300100713     C                   Z-ADD     PiVal         VABRMN
043400100713     C                   ELSE
043500100713     C                   SETON                                        31
043600100713     C                   Z-ADD     1             VABRMN
043700100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043800100713     C                             + ' ' + 'VABRMN'
043900100713     C                   ENDIF
044000100713     C* CAD
044100100713     C                   EVAL      PiStr=%trim(%subst(vindta:189:10))
044200100713     C                   EXSR      CHKNUM
044300100713     C                   IF        PiInt=*on
044400100713     C                   Z-ADD     PiVal         Num5_0
044500100713     C                   MOVEL(p)  Num5_0        VABCAD
044600100713     C                   ELSE
044700100713     C                   SETON                                        32
044800100713     C                   EVAL      VABCAD = *zeros
044900100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045000100713     C                             + ' ' + 'VABCAD'
045100100713     C                   ENDIF
045200100713     C* Reperisco la provincia dal CAP e dalla località
045300100713     C                   IF        VABCAD <> *blanks AND
045400100713     C                             VABPRD  = *blanks
045500100713     C                   CLEAR                   TISI95DS
045600100713     C                   EVAL      I95TCN = '3'
045700100713     C                   Z-ADD     datcor        I95DAT
045800100713     C                   EVAL      I95CAP = VABCAD
045900100713     C                   EVAL      I95LOC = VABLOD
046000100713     C                   EVAL      I95NAR = VABNZD
046100100713     C                   CALL      'TISI95R'
046200100713     C                   PARM                    TISI95DS
046300100713     C                   EVAL      VABPRD = O95PRV
046400100713     C                   ENDIF
046500100713     C* DCR
046600100714     C                   IF        %subst(vindta:217:8) <> *blanks
046700100713     C                   EVAL      PiStr=%trim(%subst(vindta:217:8))
046800100713     C                   EXSR      CHKNUM
046900100713     C                   IF        PiInt=*on
047000100713     C                   Z-ADD     PiVal         VABDCR
047100100713     C                   ELSE
047200100713     C                   SETON                                        32
047300100713     C                   Z-ADD     *zeros        VABDCR
047400100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500100713     C                             + ' ' + 'VABDCR'
047600100713     C                   ENDIF
047700100714     C                   ENDIF
047800100713     C* NCL
047900100713     C                   Z-ADD     *zeros        wNCL_01           5 0
048000100713     C                   Z-ADD     *zeros        wNCL_02           5 0
048100100713     C                   Z-ADD     *zeros        wNCL_03           5 0
048200100713     C                   Z-ADD     *zeros        wNCL_04           5 0
048300100713     C*
048400100713     C                   EVAL      PiStr=%trim(%subst(vindta:547:6))
048500100713     C                   EXSR      CHKNUM
048600100713     C                   IF        PiInt=*on
048700100713     C                   Z-ADD     PiVal         wNCL_01
048800100713     C                   ELSE
048900100713     C                   SETON                                        32
049000100713     C                   Z-ADD     *zeros        wNCL_01
049100100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200100713     C                             + ' ' + 'VABNCL'
049300100713     C                   ENDIF
049400100713     C*
049500100713     C                   EVAL      PiStr=%trim(%subst(vindta:553:6))
049600100713     C                   EXSR      CHKNUM
049700100713     C                   IF        PiInt=*on
049800100713     C                   Z-ADD     PiVal         wNCL_02
049900100713     C                   ELSE
050000100713     C                   SETON                                        32
050100100713     C                   Z-ADD     *zeros        wNCL_02
050200100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050300100713     C                             + ' ' + 'VABNCL'
050400100713     C                   ENDIF
050500100713     C*
050600100713     C                   EVAL      PiStr=%trim(%subst(vindta:559:6))
050700100713     C                   EXSR      CHKNUM
050800100713     C                   IF        PiInt=*on
050900100713     C                   Z-ADD     PiVal         wNCL_03
051000100713     C                   ELSE
051100100713     C                   SETON                                        32
051200100713     C                   Z-ADD     *zeros        wNCL_03
051300100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051400100713     C                             + ' ' + 'VABNCL'
051500100713     C                   ENDIF
051600100713     C*
051700100713     C                   EVAL      PiStr=%trim(%subst(vindta:565:6))
051800100713     C                   EXSR      CHKNUM
051900100713     C                   IF        PiInt=*on
052000100713     C                   Z-ADD     PiVal         wNCL_04
052100100713     C                   ELSE
052200100713     C                   SETON                                        32
052300100713     C                   Z-ADD     *zeros        wNCL_04
052400100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500100713     C                             + ' ' + 'VABNCL'
052600100713     C                   ENDIF
052700100713     C*
052800100713     C                   EVAL      VABNCL = VABNCL +
052900100713     C                                      wNCL_01+wNCL_02+wNCL_03+wNCL_04
053000100713     C* PKB
053100100713     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
053200100713     C                   EXSR      CHKNUM
053300100713     C                   IF        PiNum=*on
053400100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
053500100713     C                   EVAL      VABPKB = VABPKB + PiVal
053600100713     C                   ELSE
053700100713     C                   SETON                                        32
053800100713     C                   Z-ADD     *zeros        VABPKB
053900100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054000100713     C                             + ' ' + 'VABPKB'
054100100713     C                   ENDIF
054200100713     C* CAS
054300100713     C                   IF        %trim(%subst(vindta:236:11))<>*all'0' AND
054400100713     C                             %trim(%subst(vindta:236:11))<>*blanks
054500100713     C                   EVAL      FlgCAS = '1'
054600100713     C                   EVAL      VABVCA='EUR'
054700100713     C                   EVAL      PiStr=%trim(%subst(vindta:236:11))
054800100713     C                   EXSR      CHKNUM
054900100713     C                   IF        PiNum=*on
055000100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
055100100713     C                   EVAL      VABCAS = VABCAS + PiVal
055200100713     C                   ELSE
055300100713     C                   SETON                                        32
055400100713     C                   Z-ADD     *zeros        VABCAS
055500100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600100713     C                             + ' ' + 'VABCAS'
055700100713     C                   ENDIF
055800100713     C                   ENDIF
055900070730     C*
056000070730     C* Considerazioni finali su CBO/CAS
056100100629     C                   IF        FlgCAS = '1'
056200100629     C                   IF        VABCBO = '1'
056300100629     C                   EVAL      VABCBO = '4'
056400100629     C                   ENDIF
056500100629     C                   IF        VABCBO = '2'
056600100629     C                   EVAL      VABCBO = '6'
056700100629     C                   ENDIF
056800100629     C                   ENDIF
056900010202     C*
057000000801     C* Ebbene...
057100000801     C                   ADD       1             §CTRMO
057200070530     C                   IF        *in31 <> *zeros OR
057300070530     C                             *in32 <> *zeros
057400000801     C                   ADD       1             §CTRNO
057500020725     C                   EVAL      x_vinflg = '2'
057600000801     C                   ELSE
057700010201     C                   ADD       1             §CTROKVB
057800000801     C                   ENDIF
057900000801     C*
058000000801     C                   ENDSR
058100100127     C*----------------------------------------------------*
058200100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
058300100127     C*----------------------------------------------------*
058400100127     C     WRIVAT_A      BEGSR
058500100127     C*
058600100713     C* Valorizzo l buffer di scrittura del EDIVAT
058700100127     C* - TIPO RECORD "A"
058800100412     C***                eval      VATTRC = 'A'
058900100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
059000100412     C***                exsr      wrivat
059100100127     C*
059200100127     C                   ENDSR
059300010201     C*----------------------------------------------------*
059400100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
059500010201     C*----------------------------------------------------*
059600071003     C     WRIVAT_B      BEGSR
059700010201     C*
059800100713     C* Valorizzo l buffer di scrittura del EDIVAT
059900070928     C* - TIPO RECORD "B"
060000100412     C***                eval      VATTRC = 'B'
060100100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
060200100412     C***                exsr      wrivat
060300010201     C*
060400010201     C                   ENDSR
060500071003     C*----------------------------------------------------*
060600100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
060700071003     C*----------------------------------------------------*
060800071003     C     WRIVAT_E      BEGSR
060900071003     C*
061000100713     C* Valorizzo l buffer di scrittura del EDIVAT
061100071003     C* - TIPO RECORD "E"
061200100629     C***                eval      VATTRC = 'E'
061300100629     C***                eval      VATNOT = %trim(%subst(vindta:14:6))
061400100629     C***                exsr      wrivat
061500071003     C*
061600071003     C                   ENDSR
061700100127     C*----------------------------------------------------*
061800100713     C*  SCARICO BUFFER EDIVAT
061900100127     C*----------------------------------------------------*
062000100127     C     WRIVAT        BEGSR
062100100127     C*
062200100127     C                   if        vatNOT <> *blanks
062300100127     C                   ADD       1             §CTROKVT
062400100713     C                   write     EDIVAT00
062500100127     C                   endif
062600100127     C*
062700100127     C                   ENDSR
062800020904
062900020904
063000010202     C*----------------------------------------------------*
063100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
063200010202     C*----------------------------------------------------*
063300100525     C     PREVASX       BEGSR
063400100525     C*
063500100713     C* Compongo il nome del membro da dare al EDIVATWR
063600010202     C                   eval      parmbr = vlrhdl
063700010202     C                   movel     'M'           parmbr
063800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063900010202     C                   eval      paropz = '1'
064000100525     C*
064100010202     C* Effettuo la chiamata al CLLE preposto
064200100714     C  N51              call(e)   'TITVEVTC'
064300010202     C                   parm                    parccm
064400010202     C                   parm                    parmbr
064500010202     C                   parm                    paropz
064600100714     C   51              call(e)   'TITVEVBC'
064700100525     C                   parm                    parccm
064800100525     C                   parm                    parmbr
064900100525     C                   parm                    paropz
065000100525     C*
065100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065200010202     C                   if        %error
065300010202     C                   movel     '1'           chkcall
065400010202     C                   else
065500010202     C                   movel     '0'           chkcall
065600010202     C                   endif
065700010202     C*
065800010202     C                   ENDSR
065900000801     C*----------------------------------------------------*
066000000801     C*  CONTROLLO NUMERICITA' CAMPI
066100000801     C*----------------------------------------------------*
066200000801     C     CHKNUM        BEGSR
066300081113     C*
066400081113     C                   IF        PiDecChr = *blanks
066500100503     C                   EVAL      PiDecChr = '.'
066600081113     C                   ENDIF
066700091223     C*
066800081113     C                   callp(e)  UBISNUM_Check(PiStr
066900081113     C                                          :PiDecChr
067000081113     C                                          :PiVal
067100081113     C                                          :PiNum
067200081113     C                                          :PiInt)
067300081113     C*
067400000801     C                   IF        %error
067500000801     C                   EVAL      PiInt=*off
067600000801     C                   ENDIF
067700000801     C*
067800000801     C                   ENDSR
067900000801     C***
068000011113
068100011113
068200000801
068300000801
068400990920      /TITLE Invio dei dati al punto operativo.
068500010202     C     invio         BEGSR
068600990920     C*
068700100713     C* 1° invio EDIVAT
068800010201     C                   reset                   dscmz
068900010201     C                   move      vlrpoi        cmzdst
069000100713     C                   eval      cmzfld = 'EDIVATWR'
069100010201     C                   eval      cmzmbd = vlrhdl
069200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069300021009     C***                if        prmfir = *blanks
069400100713     C                   eval      cmzfla = 'EDIVAT0F'
069500100713     C                   eval      cmzmba = 'EDIVAT0F'
069600021009     C***                else
069700021009     C***                eval      cmzfla = prmfir
069800021009     C***                eval      cmzmba = prmfir
069900021009     C***                endif
070000010201     C                   eval      cmznrr = *zeros
070100020305     C                   move      §ctrokvt      cmznrr
070200021018     C                   eval      cmzlba = vlrfl1
070300010201     C                   call(e)   'TIS711C'
070400010201     C                   parm                    dscmz
070500010201     C                   parm      *blanks       esito
070600010205     C                   if        %error
070700010205     C                             or cmzerr = '1'
070800010205     C                             or esito  = '1'
070900010205     C                   eval      wrkesito = '3'
071000010205     C                   else
071100010201     C*
071200100713     C* 2° invio EDIVAB
071300100525     C                   if        *in51 = *off
071400010201     C                   reset                   dscmz
071500010201     C                   move      vlrpoi        cmzdst
071600010201     C                   eval      cmzfld = vlrfou
071700010201     C                   eval      cmzmbd = vlrhdl
071800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071900021009     C***                if        prmfir = *blanks
072000100713     C                   eval      cmzfla = 'EDIVAB0F'
072100100713     C                   eval      cmzmba = 'EDIVAB0F'
072200021009     C***                else
072300021009     C***                eval      cmzfla = prmfir
072400021009     C***                eval      cmzmba = prmfir
072500021009     C***                endif
072600010201     C                   eval      cmznrr = *zeros
072700010201     C                   move      §ctrokvb      cmznrr
072800021018     C                   eval      cmzlba = vlrfl1
072900010201     C                   call(e)   'TIS711C'
073000010201     C                   parm                    dscmz
073100010201     C                   parm      *blanks       esito
073200010201     C                   if        %error
073300010201     C                             or cmzerr = '1'
073400010201     C                             or esito  = '1'
073500010201     C                   eval      wrkesito = '3'
073600010201     C                   endif
073700100525     C                   endif
073800010205     C                   endif
073900990920     C*
074000000613     C                   ENDSR
074100000613     C***
074200070411
074300070411     C     *pssr         BEGSR
074400070411     C*
074500070411     C                   if        %open(tivin00r)
074600070411     C                   close     tivin00r
074700070411     C                   endif
074800100713     C                   if        %open(edivabwr)
074900100713     C                   close     edivabwr
075000070411     C                   endif
075100100713     C                   if        %open(edivatwr)
075200100713     C                   close     edivatwr
075300070411     C                   endif
075400070411     C*
075500070411     C* Effettuo la chiamata al CLLE preposto
075600100714     C  N51              call(e)   'TITVEVTC'
075700100525     C                   parm                    parccm
075800100525     C                   parm                    parmbr
075900100525     C                   parm      '2'           paropz
076000100714     C   51              call(e)   'TITVEVBC'
076100100525     C                   parm                    parccm
076200100525     C                   parm                    parmbr
076300100525     C                   parm      '2'           paropz
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
078000100713     C*
078100100713     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
078200100713     C                   move(p)   vlrMSG        wNomeFile
078300000613     C*
078400000830     C* CALCOLA LA DATA CORRENTE
078500100713     C                   time                    wn14             14 0
078600100713     C                   movel     wn14          oracor            6 0          *ORA
078700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078800091223     C                   eval      datcor = %dec(%date() : *ISO)
078900000830     C*
079000000613     C                   ENDSR
