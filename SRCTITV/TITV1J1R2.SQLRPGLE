000100100713      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
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
015500161201     C                   exsr      wrivat_p                                     => carico VAT
015600100525     C                   exsr      wrivab
015700100127     C                   exsr      wrivat_a                                     => carico VAT
015800071003     C                   exsr      wrivat_b                                     => carico VAT
015900100525     C   51              exsr      wrivat_e                                     => carico VAT
016000071003     C                   else
016100080923     C*
016200071009     C                   if        wDISK = 'D'
016300100714     C                   exsr      inzvar
016400100714     C                   exsr      defcam
016500071009     C                   exsr      impvab
016600161201     C                   exsr      wrivat_p                                     => carico VAT
016700100412     C                   exsr      wrivab
016800100127     C                   exsr      wrivat_a                                     => carico VAT
016900071009     C                   exsr      wrivat_b                                     => carico VAT
017000071009     C                   exsr      wrivat_e                                     => carico VAT
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600100714     C                   exsr      inzvar
017700100714     C                   exsr      defcam
017800020305     C                   exsr      impvab
017900161201     C                   exsr      wrivat_p                                     => carico VAT
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071003     C                   exsr      wrivat_b                                     => carico VAT
018200071003     C   50              exsr      wrivat_e                                     => carico VAT
018300010305     C                   else
018400020725     C                   if        depspe <> curspe                             => rottura di spediz
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600100412     C                   exsr      wrivab
018700080117     C                   clear                   tivinds
018800100714     C                   exsr      inzvar
018900100714     C                   exsr      defcam
019000020305     C                   exsr      impvab
019100161201     C                   exsr      wrivat_p                                     => carico VAT
019200100127     C                   exsr      wrivat_a                                     => carico VAT
019300071003     C                   exsr      wrivat_b                                     => carico VAT
019400071003     C   50              exsr      wrivat_e                                     => carico VAT
019500020305     C                   else                                                   => x stessa spediz
019600100401     C*
019700100401     C* Se nn richiesta esclusione spedizioni "duplicate"
019800100412     C                   if        wDUPREC <> 'N'
019900020305     C                   exsr      impvab
020000161201     C                   exsr      wrivat_p                                     => carico VAT
020100100127     C                   exsr      wrivat_a                                     => carico VAT
020200071003     C                   exsr      wrivat_b                                     => carico VAT
020300071003     C   50              exsr      wrivat_e                                     => carico VAT
020400100401     C                   endif
020500010305     C                   endif
020600010305     C                   endif
020700010305     C                   endif
020800071003     C                   endif
020900071009     C                   endif
021000000905     C*
021100000905     C                   else
021200020725     C                   eval      x_vinflg = '1'
021300000905     C                   endif
021400000905     C*
021500020725     C     VINRRN        chain     tivin000
021600020725     C                   if        %found(tivin00r)
021700020725     C                   eval      y_vinflg = x_vinflg
021800020725     C                   eval      y_vinmsg = x_vinmsg
021900020725     C                   update    tivin000
022000020725     C                   endif
022100020725     C*
022200020725     C/EXEC SQL
022300070530     C+ Fetch C1 into :INPUT_DS
022400020725     C/END-EXEC
022500020725     C*
022600020725     C                   enddo
022700020725     C*
022800020725     C/EXEC SQL
022900020725     C+ close C1
023000020725     C/END-EXEC
023100000905     C*
023200020305     C* Scarico i VAB rimasti "in sospeso"
023300071009     C                   if        wDISK = 'C'
023400100412     C                   exsr      wrivab
023500071009     C                   endif
023600010202     C*
023700010202     C                   endif
023800990910
023900990910     C* Se non ci sono record con errori ...
024000000710     C                   if        §ctrno = 0
024100990910     C* ... restituisco esito OK.
024200990921     C                   eval      wrkesito = '0'
024300990910     C                   else
024400100525     C                   if        §ctrokvb > 0 OR
024500100525     C                             §ctrokvt > 0
024600990921     C                   eval      wrkesito = '1'
024700000710     C                   else
024800000710     C                   eval      wrkesito = '2'
024900990910     C                   endif
025000000710     C                   endif
025100990910     C*
025200990914     C                   if        %open(tivin00r)
025300990908     C                   close     tivin00r
025400990914     C                   endif
025500100713     C                   if        %open(edivabwr)
025600100713     C                   close     edivabwr
025700990914     C                   endif
025800100713     C                   if        %open(edivatwr)
025900100713     C                   close     edivatwr
026000010201     C                   endif
026100990910     C*
026200100525     C                   if        §ctrokvb > 0 OR
026300100525     C                             §ctrokvt > 0
026400000724     C                             and vlrpoi <> *zeros
026500010202     C                   exsr      invio
026600990920     C                   endif
026700990920     C*
026800910830     C                   ENDSR
026900000613     C***
027000100412
027100100412
027200010305
027300010305     C*----------------------------------------------------*
027400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027500010305     C*----------------------------------------------------*
027600020305     C     WRIVAB        BEGSR
027700100713     C*
027800100713     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
027900100713     C                   eval      VABCMR = %subst(wNomeFile:10:05)
028000100713     C                   EVAL      VABDCM = DATCOR
028100100713     C                   EVAL      VABDTS = DATCOR
028200100713     C                   EVAL      VABHMS = ORACOR
028300100713     C                   EVAL      VABCNT = 1
028400100412     C*
028500100412     C* Gestisco l'eventuale rottura x numero spedizione
028600100412     C                   if        VABRMA = *blanks
028700100412     C                   movel(P)  VABRMN        VABRMA
028800100412     C                   endif
028900100412     C*
029000100525     C                   if        *in51 = *off and *in31 = *off
029100100713     C                   write     edivab00                                     => scarico il VAB
029200100525     C                   endif
029300010305     C*
029400010305     C                   ENDSR
029500990920
029600000801     C*----------------------------------------------------*
029700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029800000801     C*----------------------------------------------------*
029900010201     C     INZVAR        BEGSR
030000000801     C*
030100010201     C                   Z-ADD     *zeros        Num5_0
030200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030300020725     C                   MOVEL     '0'           FlgCAS            1
030400100713     C                   MOVEL     *blanks       wNOTE            70
030500000801     C*
030600000801     C                   ENDSR
030700000801     C*----------------------------------------------------*
030800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030900000801     C*----------------------------------------------------*
031000020904     C     DEFCAM        BEGSR
031100080923     C*
031200100525     C                   SETOFF                                       5051
031300000801     C*
031400100713     C                   CLEAR                   EDIVAB00
031500100713     C                   CLEAR                   EDIVAT00
031600100713     C* Imposto i valori di default...
031700100713     C                   EVAL      VABCCM = 2220929
031800100713     C                   EVAL      VABLNP = 222
031900100713     C                   EVAL      VABCTR = 001
032000100713     C                   EVAL      VABTSP = 'C'
032100100713     C                   EVAL      VABCBO = '1'
032200070222     C* ... e poi verifico se sono stati passati come parametri
032300070222     C                   IF        vlrppt > *blanks
032400100525     C*
032500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
032600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032700070222     C                   EXSR      CHKNUM
032800070222     C                   IF        PiInt=*on
032900070222     C                   Z-ADD     PiVal         VABCCM
033000070222     C                   Z-ADD     PiVal         VATCCM
033100070222     C                   ENDIF
033200070222     C                   ENDIF
033300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
033400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033500070222     C                   EXSR      CHKNUM
033600070222     C                   IF        PiInt=*on
033700070222     C                   Z-ADD     PiVal         VABLNP
033800070222     C                   Z-ADD     PiVal         VATLNP
033900070222     C                   ENDIF
034000070222     C                   ENDIF
034100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
034200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034300070222     C                   EXSR      CHKNUM
034400070222     C                   IF        PiInt=*on
034500070222     C                   Z-ADD     PiVal         VABCTR
034600070222     C                   ENDIF
034700070928     C                   ENDIF
034800071009     C                   MOVEL     *blanks       wDISK             1
034900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
035000071009     C                   IF        wDISK <> *blanks
035100070928     C                   SETON                                        50
035200070222     C                   ENDIF
035300100525     C                   IF        wDISK  = 'T'
035400100525     C                   SETOFF                                       50
035500100525     C                   SETON                                        51
035600100525     C                   ENDIF
035700100921     C   50              EVAL      VABCTM = '7Q'
035800100401     C                   MOVEL     *blanks       wDUPREC           1
035900100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
036000100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
036100100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
036200100525     C                   ENDIF
036300100525     C*
036400070222     C                   ENDIF
036500000801     C*
036600000801     C                   ENDSR
036700000801     C*----------------------------------------------------*
036800100713     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
036900000801     C*----------------------------------------------------*
037000010201     C     IMPVAB        BEGSR
037100070530     C*
037200070530     C                   SETOFF                                       3132
037300070928     C*
037400070928     C                   MOVE(P)   vlrpoi        VABFGS
037500070928     C*
037600070928     C                   MOVEL     datcor        VABAAS
037700070928     C                   MOVE      datcor        VABMGS
037800100713     C*
037900100713     C* Porto tutto il buffer d input in maiuscolo
038000100713     C     minu:maiu     XLATE     vindta        vindta
038100100713     C*
038200100713     C* Reperimento campi ALFA
038300100713     C                   EVAL      VABRSD=%trim(%subst(vindta:74:35))
038400100714     C                   EVAL      VABIND=%trim(%subst(vindta:109:40))+' '+
038500100714     C                                    %trim(%subst(vindta:634:30))
038600100713     C                   EVAL      VABLOD=%trim(%subst(vindta:149:40))
038700100713     C                   EVAL      VABPRD=%trim(%subst(vindta:199:2))
038800100713     C***                EVAL      VABRMA=%trim(%subst(vindta:20:10))
038900100713     C                   EVAL      VABRMA=%trim(%subst(vindta:38:10))
039000100713     C                   IF        %trim(%subst(vindta:598:4)) = 'ZESP'
039100100713     C                   EVAL      VABTSP='E'
039200100713     C                   ENDIF
039300100713     C                   IF        %trim(%subst(vindta:235:1)) = 'F'
039400100713     C                   EVAL      VABCBO='1'
039500100713     C                   ENDIF
039600100713     C                   IF        %trim(%subst(vindta:235:1)) = 'A'
039700100713     C                   EVAL      VABCBO='2'
039800100713     C                   ENDIF
039900100713     C                   IF        %trim(%subst(vindta:225:10)) = 'TASSATIVA'
040000100713     C                   EVAL      VABTCR=*blanks
040100100713     C                   ENDIF
040200100713     C                   IF        %trim(%subst(vindta:225:10)) = 'ENTRO IL'
040300100713     C                   EVAL      VABTCR='P'
040400100713     C                   ENDIF
040500100713     C                   IF        %trim(%subst(vindta:225:10)) = 'DOPO IL'
040600100713     C                   EVAL      VABTCR='D'
040700100713     C                   ENDIF
040800100713     C                   EVAL      wNOTE=%trim(%subst(vindta:247:75))+
040900100713     C                                   %trim(%subst(vindta:322:75))+
041000100713     C                                   %trim(%subst(vindta:397:75))+
041100100713     C                                   %trim(%subst(vindta:472:75))
041200100713     C                   EVAL      VABNOT=%subst(wNOTE:01:35)
041300100713     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
041400100713     C*
041500100713     C* Considerazioni sulla ragione sociale del destinatario da indicare
041600100713     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041700100713     C     '@':'A'       XLATE     VABRSD        VABRSD
041800100713     C* ==
041900100713     C*
042000100713     C* Reperimento campi NUMERICI
042100100713     C* NSP
042200100713     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
042300100713     C                   EXSR      CHKNUM
042400100713     C                   IF        PiInt=*on
042500100713     C                   Z-ADD     PiVal         VABNSP
042600100713     C                   ELSE
042700100713     C                   SETON                                        31
042800100713     C                   Z-ADD     *zeros        VABNSP
042900100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043000100713     C                             + ' ' + 'VABNSP'
043100100713     C                   ENDIF
043200100713     C* RMN
043300100713     C***                EVAL      PiStr=%trim(%subst(vindta:38:10))
043400100713     C                   EVAL      PiStr=%trim(%subst(vindta:20:10))
043500100713     C                   EXSR      CHKNUM
043600100713     C                   IF        PiInt=*on
043700100713     C                   Z-ADD     PiVal         VABRMN
043800100713     C                   ELSE
043900100713     C                   SETON                                        31
044000100713     C                   Z-ADD     1             VABRMN
044100100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200100713     C                             + ' ' + 'VABRMN'
044300100713     C                   ENDIF
044400100713     C* CAD
044500100713     C                   EVAL      PiStr=%trim(%subst(vindta:189:10))
044600100713     C                   EXSR      CHKNUM
044700100713     C                   IF        PiInt=*on
044800100713     C                   Z-ADD     PiVal         Num5_0
044900100713     C                   MOVEL(p)  Num5_0        VABCAD
045000100713     C                   ELSE
045100100713     C                   SETON                                        32
045200100713     C                   EVAL      VABCAD = *zeros
045300100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045400100713     C                             + ' ' + 'VABCAD'
045500100713     C                   ENDIF
045600100713     C* Reperisco la provincia dal CAP e dalla località
045700100713     C                   IF        VABCAD <> *blanks AND
045800100713     C                             VABPRD  = *blanks
045900100713     C                   CLEAR                   TISI95DS
046000100713     C                   EVAL      I95TCN = '3'
046100100713     C                   Z-ADD     datcor        I95DAT
046200100713     C                   EVAL      I95CAP = VABCAD
046300100713     C                   EVAL      I95LOC = VABLOD
046400100713     C                   EVAL      I95NAR = VABNZD
046500100713     C                   CALL      'TISI95R'
046600100713     C                   PARM                    TISI95DS
046700100713     C                   EVAL      VABPRD = O95PRV
046800100713     C                   ENDIF
046900100713     C* DCR
047000100714     C                   IF        %subst(vindta:217:8) <> *blanks
047100100713     C                   EVAL      PiStr=%trim(%subst(vindta:217:8))
047200100713     C                   EXSR      CHKNUM
047300100713     C                   IF        PiInt=*on
047400100713     C                   Z-ADD     PiVal         VABDCR
047500100713     C                   ELSE
047600100713     C                   SETON                                        32
047700100713     C                   Z-ADD     *zeros        VABDCR
047800100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047900100713     C                             + ' ' + 'VABDCR'
048000100713     C                   ENDIF
048100100714     C                   ENDIF
048200100713     C* NCL
048300100713     C                   Z-ADD     *zeros        wNCL_01           5 0
048400100713     C                   Z-ADD     *zeros        wNCL_02           5 0
048500100713     C                   Z-ADD     *zeros        wNCL_03           5 0
048600100713     C                   Z-ADD     *zeros        wNCL_04           5 0
048700100713     C*
048800100713     C                   EVAL      PiStr=%trim(%subst(vindta:547:6))
048900100713     C                   EXSR      CHKNUM
049000100713     C                   IF        PiInt=*on
049100100713     C                   Z-ADD     PiVal         wNCL_01
049200100713     C                   ELSE
049300100713     C                   SETON                                        32
049400100713     C                   Z-ADD     *zeros        wNCL_01
049500100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600100713     C                             + ' ' + 'VABNCL'
049700100713     C                   ENDIF
049800100713     C*
049900100713     C                   EVAL      PiStr=%trim(%subst(vindta:553:6))
050000100713     C                   EXSR      CHKNUM
050100100713     C                   IF        PiInt=*on
050200100713     C                   Z-ADD     PiVal         wNCL_02
050300100713     C                   ELSE
050400100713     C                   SETON                                        32
050500100713     C                   Z-ADD     *zeros        wNCL_02
050600100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700100713     C                             + ' ' + 'VABNCL'
050800100713     C                   ENDIF
050900100713     C*
051000100713     C                   EVAL      PiStr=%trim(%subst(vindta:559:6))
051100100713     C                   EXSR      CHKNUM
051200100713     C                   IF        PiInt=*on
051300100713     C                   Z-ADD     PiVal         wNCL_03
051400100713     C                   ELSE
051500100713     C                   SETON                                        32
051600100713     C                   Z-ADD     *zeros        wNCL_03
051700100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800100713     C                             + ' ' + 'VABNCL'
051900100713     C                   ENDIF
052000100713     C*
052100100713     C                   EVAL      PiStr=%trim(%subst(vindta:565:6))
052200100713     C                   EXSR      CHKNUM
052300100713     C                   IF        PiInt=*on
052400100713     C                   Z-ADD     PiVal         wNCL_04
052500100713     C                   ELSE
052600100713     C                   SETON                                        32
052700100713     C                   Z-ADD     *zeros        wNCL_04
052800100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900100713     C                             + ' ' + 'VABNCL'
053000100713     C                   ENDIF
053100100713     C*
053200100713     C                   EVAL      VABNCL = VABNCL +
053300100713     C                                      wNCL_01+wNCL_02+wNCL_03+wNCL_04
053400100713     C* PKB
053500100713     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
053600100713     C                   EXSR      CHKNUM
053700100713     C                   IF        PiNum=*on
053800100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
053900100713     C                   EVAL      VABPKB = VABPKB + PiVal
054000100713     C                   ELSE
054100100713     C                   SETON                                        32
054200100713     C                   Z-ADD     *zeros        VABPKB
054300100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400100713     C                             + ' ' + 'VABPKB'
054500100713     C                   ENDIF
054600100713     C* CAS
054700100713     C                   IF        %trim(%subst(vindta:236:11))<>*all'0' AND
054800100713     C                             %trim(%subst(vindta:236:11))<>*blanks
054900100713     C                   EVAL      FlgCAS = '1'
055000100713     C                   EVAL      VABVCA='EUR'
055100100713     C                   EVAL      PiStr=%trim(%subst(vindta:236:11))
055200100713     C                   EXSR      CHKNUM
055300100713     C                   IF        PiNum=*on
055400100713     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
055500100713     C                   EVAL      VABCAS = VABCAS + PiVal
055600100713     C                   ELSE
055700100713     C                   SETON                                        32
055800100713     C                   Z-ADD     *zeros        VABCAS
055900100713     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056000100713     C                             + ' ' + 'VABCAS'
056100100713     C                   ENDIF
056200100713     C                   ENDIF
056300070730     C*
056400070730     C* Considerazioni finali su CBO/CAS
056500100629     C                   IF        FlgCAS = '1'
056600100629     C                   IF        VABCBO = '1'
056700100629     C                   EVAL      VABCBO = '4'
056800100629     C                   ENDIF
056900100629     C                   IF        VABCBO = '2'
057000100629     C                   EVAL      VABCBO = '6'
057100100629     C                   ENDIF
057200100629     C                   ENDIF
057300010202     C*
057400000801     C* Ebbene...
057500000801     C                   ADD       1             §CTRMO
057600070530     C                   IF        *in31 <> *zeros OR
057700070530     C                             *in32 <> *zeros
057800000801     C                   ADD       1             §CTRNO
057900020725     C                   EVAL      x_vinflg = '2'
058000000801     C                   ELSE
058100010201     C                   ADD       1             §CTROKVB
058200000801     C                   ENDIF
058300000801     C*
058400000801     C                   ENDSR
058500100127     C*----------------------------------------------------*
058600100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "A"
058700100127     C*----------------------------------------------------*
058800100127     C     WRIVAT_A      BEGSR
058900100127     C*
059000100713     C* Valorizzo l buffer di scrittura del EDIVAT
059100100127     C* - TIPO RECORD "A"
059200100412     C***                eval      VATTRC = 'A'
059300100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
059400100412     C***                exsr      wrivat
059500100127     C*
059600100127     C                   ENDSR
059700010201     C*----------------------------------------------------*
059800100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
059900010201     C*----------------------------------------------------*
060000071003     C     WRIVAT_B      BEGSR
060100010201     C*
060200100713     C* Valorizzo l buffer di scrittura del EDIVAT
060300070928     C* - TIPO RECORD "B"
060400100412     C***                eval      VATTRC = 'B'
060500100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
060600100412     C***                exsr      wrivat
060700010201     C*
060800010201     C                   ENDSR
060900071003     C*----------------------------------------------------*
061000100713     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
061100071003     C*----------------------------------------------------*
061200071003     C     WRIVAT_E      BEGSR
061300071003     C*
061400100713     C* Valorizzo l buffer di scrittura del EDIVAT
061500071003     C* - TIPO RECORD "E"
061600100629     C***                eval      VATTRC = 'E'
061700100629     C***                eval      VATNOT = %trim(%subst(vindta:14:6))
061800100629     C***                exsr      wrivat
061900071003     C*
062000071003     C                   ENDSR
062100161201     C*----------------------------------------------------*
062200161201     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "P"
062300161201     C*----------------------------------------------------*
062400161201     C     WRIVAT_P      BEGSR
062500161201     C*
062600161201     C* Valorizzo l buffer di scrittura del EDIVAT
062700161201     C* - TIPO RECORD "P"
062800161201     C                   eval      VATTRC = 'P'
062900170328     C***                select
063000170328     C***                when      %trim(%subst(vindta:674:10)) <> *blanks
063100170328     C***                eval      VATNOT = %trim(%subst(vindta:20:10))+ '_' +
063200170328     C***                                   %trim(%subst(vindta:674:10))+'.PDF'
063300170328     C***                other
063400161201     C                   eval      VATNOT = %trim(%subst(vindta:20:10))+'.PDF'
063500170328     C***                endsl
063600161201     C                   eval      %subst(VATNOT:34:2) = '10'
063700161201     C                   exsr      wrivat
063800161201     C*
063900161201     C                   ENDSR
064000100127     C*----------------------------------------------------*
064100100713     C*  SCARICO BUFFER EDIVAT
064200100127     C*----------------------------------------------------*
064300100127     C     WRIVAT        BEGSR
064400100127     C*
064500100127     C                   if        vatNOT <> *blanks
064600161205     C*
064700161205     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
064800161205     C                   eval      vatCMR = %subst(wNomeFile:10:05)
064900161205     C                   eval      vatCNT = 1
065000161205     C*
065100161205     C* VALORIZZO CAMPI CHIAVE CON TESTATA BOLLE
065200161205     C                   eval      vatFGS = vabFGS
065300161205     C                   eval      vatCCM = vabCCM
065400161205     C                   eval      vatAAS = vabAAS
065500161205     C                   eval      vatLNP = vabLNP
065600161205     C                   eval      vatNRS = vabNRS
065700161205     C                   eval      vatNSP = vabNSP
065800161205     C*
065900161205     C* VALORIZZO CAMPI DATI
066000100713     C                   write     EDIVAT00
066100161205     C                   add       1             §CTROKVT
066200161205     C*
066300100127     C                   endif
066400100127     C*
066500100127     C                   ENDSR
066600020904
066700020904
066800010202     C*----------------------------------------------------*
066900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
067000010202     C*----------------------------------------------------*
067100100525     C     PREVASX       BEGSR
067200100525     C*
067300100713     C* Compongo il nome del membro da dare al EDIVATWR
067400010202     C                   eval      parmbr = vlrhdl
067500010202     C                   movel     'M'           parmbr
067600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
067700010202     C                   eval      paropz = '1'
067800100525     C*
067900010202     C* Effettuo la chiamata al CLLE preposto
068000100714     C  N51              call(e)   'TITVEVTC'
068100010202     C                   parm                    parccm
068200010202     C                   parm                    parmbr
068300010202     C                   parm                    paropz
068400100714     C   51              call(e)   'TITVEVBC'
068500100525     C                   parm                    parccm
068600100525     C                   parm                    parmbr
068700100525     C                   parm                    paropz
068800100525     C*
068900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069000010202     C                   if        %error
069100010202     C                   movel     '1'           chkcall
069200010202     C                   else
069300010202     C                   movel     '0'           chkcall
069400010202     C                   endif
069500010202     C*
069600010202     C                   ENDSR
069700000801     C*----------------------------------------------------*
069800000801     C*  CONTROLLO NUMERICITA' CAMPI
069900000801     C*----------------------------------------------------*
070000000801     C     CHKNUM        BEGSR
070100081113     C*
070200081113     C                   IF        PiDecChr = *blanks
070300100503     C                   EVAL      PiDecChr = '.'
070400081113     C                   ENDIF
070500091223     C*
070600081113     C                   callp(e)  UBISNUM_Check(PiStr
070700081113     C                                          :PiDecChr
070800081113     C                                          :PiVal
070900081113     C                                          :PiNum
071000081113     C                                          :PiInt)
071100081113     C*
071200000801     C                   IF        %error
071300000801     C                   EVAL      PiInt=*off
071400000801     C                   ENDIF
071500000801     C*
071600000801     C                   ENDSR
071700000801     C***
071800011113
071900011113
072000000801
072100000801
072200990920      /TITLE Invio dei dati al punto operativo.
072300010202     C     invio         BEGSR
072400990920     C*
072500100713     C* 1° invio EDIVAT
072600010201     C                   reset                   dscmz
072700010201     C                   move      vlrpoi        cmzdst
072800100713     C                   eval      cmzfld = 'EDIVATWR'
072900010201     C                   eval      cmzmbd = vlrhdl
073000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073100021009     C***                if        prmfir = *blanks
073200100713     C                   eval      cmzfla = 'EDIVAT0F'
073300100713     C                   eval      cmzmba = 'EDIVAT0F'
073400021009     C***                else
073500021009     C***                eval      cmzfla = prmfir
073600021009     C***                eval      cmzmba = prmfir
073700021009     C***                endif
073800010201     C                   eval      cmznrr = *zeros
073900020305     C                   move      §ctrokvt      cmznrr
074000021018     C                   eval      cmzlba = vlrfl1
074100010201     C                   call(e)   'TIS711C'
074200010201     C                   parm                    dscmz
074300010201     C                   parm      *blanks       esito
074400010205     C                   if        %error
074500010205     C                             or cmzerr = '1'
074600010205     C                             or esito  = '1'
074700010205     C                   eval      wrkesito = '3'
074800010205     C                   else
074900010201     C*
075000100713     C* 2° invio EDIVAB
075100100525     C                   if        *in51 = *off
075200010201     C                   reset                   dscmz
075300010201     C                   move      vlrpoi        cmzdst
075400010201     C                   eval      cmzfld = vlrfou
075500010201     C                   eval      cmzmbd = vlrhdl
075600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075700021009     C***                if        prmfir = *blanks
075800100713     C                   eval      cmzfla = 'EDIVAB0F'
075900100713     C                   eval      cmzmba = 'EDIVAB0F'
076000021009     C***                else
076100021009     C***                eval      cmzfla = prmfir
076200021009     C***                eval      cmzmba = prmfir
076300021009     C***                endif
076400010201     C                   eval      cmznrr = *zeros
076500010201     C                   move      §ctrokvb      cmznrr
076600021018     C                   eval      cmzlba = vlrfl1
076700010201     C                   call(e)   'TIS711C'
076800010201     C                   parm                    dscmz
076900010201     C                   parm      *blanks       esito
077000010201     C                   if        %error
077100010201     C                             or cmzerr = '1'
077200010201     C                             or esito  = '1'
077300010201     C                   eval      wrkesito = '3'
077400010201     C                   endif
077500100525     C                   endif
077600010205     C                   endif
077700990920     C*
077800000613     C                   ENDSR
077900000613     C***
078000070411
078100070411     C     *pssr         BEGSR
078200070411     C*
078300070411     C                   if        %open(tivin00r)
078400070411     C                   close     tivin00r
078500070411     C                   endif
078600100713     C                   if        %open(edivabwr)
078700100713     C                   close     edivabwr
078800070411     C                   endif
078900100713     C                   if        %open(edivatwr)
079000100713     C                   close     edivatwr
079100070411     C                   endif
079200070411     C*
079300070411     C* Effettuo la chiamata al CLLE preposto
079400100714     C  N51              call(e)   'TITVEVTC'
079500100525     C                   parm                    parccm
079600100525     C                   parm                    parmbr
079700100525     C                   parm      '2'           paropz
079800100714     C   51              call(e)   'TITVEVBC'
079900100525     C                   parm                    parccm
080000100525     C                   parm                    parmbr
080100100525     C                   parm      '2'           paropz
080200070411     C*
080300070411     C                   eval      wrkesito = '2'
080400070411     C*
080500070411     C                   seton                                        LR
080600070411     C*
080700070411     C                   ENDSR     '*CANCL'
080800070411     C***
080900070411
081000990910
081100000613     C     *inzsr        BEGSR
081200990910     C*
081300990910     C     *entry        plist
081400990920     C                   parm                    tivlrds
081500990921     C                   parm      wrkesito      esito
081600000724     C                   parm                    prmlit
081700000710     C                   parm                    prmfir
081800100713     C*
081900100713     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
082000100713     C                   move(p)   vlrMSG        wNomeFile
082100000613     C*
082200000830     C* CALCOLA LA DATA CORRENTE
082300100713     C                   time                    wn14             14 0
082400100713     C                   movel     wn14          oracor            6 0          *ORA
082500091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082600091223     C                   eval      datcor = %dec(%date() : *ISO)
082700000830     C*
082800000613     C                   ENDSR
082900000613     C***
