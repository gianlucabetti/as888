000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130117     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400100607     Ftabel00f  if   e           k disk
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070730     D tisi95ds      e ds
001700100607     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300100607     D depspe          s              8    INZ(*blanks)
002400100607     D curspe          s              8    INZ(*blanks)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000091223     D Num5_0          s              5  0
003100100607
003200100607     D*------------
003300100607     D* GESTIONE NAZIONI ESTERE
003400100607     D*------------
003500100607     D wNAZ            S              3    INZ
003600100607     D jNAZ            S              5  0 INZ
003700100607     D skNAZISO        S              3    DIM(1000)
003800100607     D skNAZBAR        S              3    DIM(1000)
003900100607
004000100607
004100100607     D*------------------
004200100607     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
004300100607     D*------------------
004400110616     D InStringa       S          65535A   VARYING                              (stringa input)
004500100607     D InSepara        S             10A                                        (separatore)
004600100607     D InTypeOut       S              1A                                        (tipo output)
004700110617     D wSkParam        S          65535A                                        (output)
004800100607     D OutErrore       S              1A                                        (flag errore)
004900100607     D OutMsg          S             80A                                        (messaggio errore)
005000100607     D SkSplitCSV_6    S            512    DIM(128)
005100020725
005200020725     D*------------------
005300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005400020725     D*------------------
005500070530     D INPUT_DS        DS                  INZ
005600020725     D  VINDTA                 1   2048
005700020725     D  VINFLG              2049   2049
005800020725     D  VINSPE              2050   2059
005900020725     D  VINRRN              2060   2067  0
006000020725     D*
006100081113
006200091223     D*------------------
006300091223     D* DS REPERIMENTO NUMERATORE
006400091223     D*------------------
006500100309     D trul33ds      e ds                  inz
006600091223     D*------------------
006700091223     D* DS ARCHITETTURA
006800091223     D*------------------
006900091223     D kpjba         e ds                  inz
007000091223
007100081113
007200081113     D*------------------
007300081113     D* LINKING A DEFINIZIONI ESTERNE
007400081113     D*------------------
007500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700081113
007800990908
007900010201
008000010201
008100080117     C*
008200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008300080117     C
008400080117     C/EXEC SQL
008500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008600080117     C/END-EXEC
008700100607     C*
008800100607     C* Carico le tabelle
008900100607     C                   exsr      cartab
009000100607     C*
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400000613     C*
009500100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
009600070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900100525     C  N51              call(e)   'TITVVTC'
010000100525     C                   parm                    parccm
010100100525     C                   parm                    parmbr
010200100525     C                   parm      '2'           paropz
010300100525     C   51              call(e)   'TITVVBC'
010400100525     C                   parm                    parccm
010500100525     C                   parm                    parmbr
010600100525     C                   parm      '2'           paropz
010700070730     C*
010800070730     C* Effettuo lancio TISI95 solo x chiusura
010900070730     C                   CLEAR                   TISI95DS
011000070730     C                   EVAL      I95TLA = 'C'
011100070730     C                   CALL      'TISI95R'
011200070730     C                   PARM                    TISI95DS
011300100607     C*
011400100607     C* Effettuo lancio XSPLIT solo x chiusura
011500100607     C                   CALL(e)   'XSPLIT2'
011600110616     C                   PARM      ''            InStringa
011700100607     C                   PARM                    InSepara
011800100607     C                   PARM                    InTypeOut
011900110616     C                   PARM      ''            wSkParam
012000100607     C                   PARM                    OutErrore
012100100607     C                   PARM                    OutMsg
012200000616     C*
012300000801     C
012400010201     C                   seton                                        LR
012500990908
012600000801
012700910830     C*--------------------------------------------------------
012800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
012900910830     C*--------------------------------------------------------
013000070530     C     RWFILE        BEGSR
013100990910     C*
013200990914     C                   if        not %open(tivin00r)
013300990908     C                   open      tivin00r
013400990914     C                   endif
013500070530     C*
013600100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
013700100525     C                   exsr      prevasx
013800010201     C*
013900010202     C                   if        chkcall = '0'
014000010202     C*
014100100525     C                   if        not %open(fivabwwr)
014200100525     C                   open      fivabwwr
014300100525     C                   endif
014400100525     C*
014500021113     C                   if        not %open(fivatwwr)
014600021113     C                   open      fivatwwr
014700010201     C                   endif
014800080117     C*
014900080117     C                   EXSR      INZVAR
015000080117     C                   EXSR      DEFCAM
015100990910     C*
015200010201     C                   clear                   §CTROKVB          5 0
015300020305     C                   clear                   §CTROKVT          5 0
015400000801     C                   clear                   §CTRMO            5 0
015500000801     C                   clear                   §CTRNO            5 0
015600990910     C*
015700020725     C/EXEC SQL
015800020725     C+ declare C1 cursor for select
015900100607     C+ vindta, vinflg, substr(vindta, 16, 8) as sped, rrn(tivin00r)
016000060223     C+ from tivin00r
016100060223     C+ order by sped
016200020725     C+ for read only
016300020725     C/END-EXEC
016400020725     C
016500020725     C/EXEC SQL
016600020725     C+ open C1
016700020725     C/END-EXEC
016800020725     C
016900020725     C/EXEC SQL
017000070530     C+ Fetch C1 into :INPUT_DS
017100020725     C/END-EXEC
017200020725     C*
017300020725     C                   dow       sqlcod = *zeros
017400990913     C*
017500100310     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700000801     C*
017800020725     C                   if        vinflg = *blanks
017900020725     C                             or vinflg = '0'
018000020725     C                             or vinflg = '2'
018100000801     C*
018200020725     C                   clear                   x_vinmsg
018300020725     C                   eval      x_vinflg = '1'
018400010305     C*
018500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018600100607     C                   EVAL      curspe=%trim(%subst(vindta:16:8))
018700010305     C*
018800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
018900080923     C                   if        *in50 = *off
019000071003     C                   exsr      impvab
019100100525     C                   exsr      wrivab
019200100127     C                   exsr      wrivat_a                                     => carico VAT
019300071003     C                   exsr      wrivat_b                                     => carico VAT
019400100525     C   51              exsr      wrivat_e                                     => carico VAT
019500071003     C                   else
019600080923     C*
019700071009     C                   if        wDISK = 'D'
019800091223     C                   exsr      repNSP
019900071009     C                   exsr      impvab
020000100412     C                   exsr      wrivab
020100100127     C                   exsr      wrivat_a                                     => carico VAT
020200071009     C                   exsr      wrivat_b                                     => carico VAT
020300071009     C                   exsr      wrivat_e                                     => carico VAT
020400071009     C                   else
020500071009     C*
020600010305     C                   if        depspe = *blanks                             => 1° giro
020700010305     C                   eval      depspe = curspe                              => memorizz. spediz
020800080117     C                   clear                   tivinds
020900091223     C                   exsr      repNSP
021000020305     C                   exsr      impvab
021100100127     C                   exsr      wrivat_a                                     => carico VAT
021200071003     C                   exsr      wrivat_b                                     => carico VAT
021300071003     C   50              exsr      wrivat_e                                     => carico VAT
021400010305     C                   else
021500020725     C                   if        depspe <> curspe                             => rottura di spediz
021600010305     C                   eval      depspe = curspe                              => memorizz. spediz
021700100412     C                   exsr      wrivab
021800080117     C                   clear                   tivinds
021900091223     C                   exsr      repNSP
022000020305     C                   exsr      impvab
022100100127     C                   exsr      wrivat_a                                     => carico VAT
022200071003     C                   exsr      wrivat_b                                     => carico VAT
022300071003     C   50              exsr      wrivat_e                                     => carico VAT
022400020305     C                   else                                                   => x stessa spediz
022500100401     C*
022600100401     C* Se nn richiesta esclusione spedizioni "duplicate"
022700100412     C                   if        wDUPREC <> 'N'
022800020305     C                   exsr      impvab
022900100127     C                   exsr      wrivat_a                                     => carico VAT
023000071003     C                   exsr      wrivat_b                                     => carico VAT
023100071003     C   50              exsr      wrivat_e                                     => carico VAT
023200100401     C                   endif
023300010305     C                   endif
023400010305     C                   endif
023500010305     C                   endif
023600071003     C                   endif
023700071009     C                   endif
023800000905     C*
023900000905     C                   else
024000020725     C                   eval      x_vinflg = '1'
024100000905     C                   endif
024200000905     C*
024300020725     C     VINRRN        chain     tivin000
024400020725     C                   if        %found(tivin00r)
024500020725     C                   eval      y_vinflg = x_vinflg
024600020725     C                   eval      y_vinmsg = x_vinmsg
024700020725     C                   update    tivin000
024800020725     C                   endif
024900020725     C*
025000020725     C/EXEC SQL
025100070530     C+ Fetch C1 into :INPUT_DS
025200020725     C/END-EXEC
025300020725     C*
025400020725     C                   enddo
025500020725     C*
025600020725     C/EXEC SQL
025700020725     C+ close C1
025800020725     C/END-EXEC
025900000905     C*
026000020305     C* Scarico i VAB rimasti "in sospeso"
026100071009     C                   if        wDISK = 'C'
026200100412     C                   exsr      wrivab
026300071009     C                   endif
026400010202     C*
026500010202     C                   endif
026600990910
026700990910     C* Se non ci sono record con errori ...
026800000710     C                   if        §ctrno = 0
026900990910     C* ... restituisco esito OK.
027000990921     C                   eval      wrkesito = '0'
027100990910     C                   else
027200100525     C                   if        §ctrokvb > 0 OR
027300100525     C                             §ctrokvt > 0
027400990921     C                   eval      wrkesito = '1'
027500000710     C                   else
027600000710     C                   eval      wrkesito = '2'
027700990910     C                   endif
027800000710     C                   endif
027900990910     C*
028000990914     C                   if        %open(tivin00r)
028100990908     C                   close     tivin00r
028200990914     C                   endif
028300021113     C                   if        %open(fivabwwr)
028400021113     C                   close     fivabwwr
028500990914     C                   endif
028600021113     C                   if        %open(fivatwwr)
028700021113     C                   close     fivatwwr
028800010201     C                   endif
028900990910     C*
029000100525     C                   if        §ctrokvb > 0 OR
029100100525     C                             §ctrokvt > 0
029200000724     C                             and vlrpoi <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800100412
029900100412
030000010305
030100010305     C*----------------------------------------------------*
030200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030300010305     C*----------------------------------------------------*
030400020305     C     WRIVAB        BEGSR
030500100412     C*
030600100412     C* Gestisco l'eventuale rottura x numero spedizione
030700100412     C                   if        VABRMA = *blanks
030800100412     C                   movel(P)  VABRMN        VABRMA
030900100412     C                   endif
031000100412     C*
031100100525     C                   if        *in51 = *off and *in31 = *off
031200100525     C                   write     fivab000                                     => scarico il VAB
031300100525     C                   endif
031400010305     C*
031500010305     C                   ENDSR
031600990920
031700000801     C*----------------------------------------------------*
031800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900000801     C*----------------------------------------------------*
032000010201     C     INZVAR        BEGSR
032100000801     C*
032200010201     C                   Z-ADD     *zeros        Num5_0
032300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032400020725     C                   MOVEL     '0'           FlgCAS            1
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032900000801     C*----------------------------------------------------*
033000020904     C     DEFCAM        BEGSR
033100080923     C*
033200100525     C                   SETOFF                                       5051
033300000801     C*
033400021113     C                   CLEAR                   FIVAB000
033500021113     C                   CLEAR                   FIVAT000
033600100607     C                   CLEAR                   SkSplitCSV_6
033700070730     C* Imposto i valori di default...
033800100607     C                   EVAL      VABCCM = 0661313
033900100607     C                   EVAL      VATCCM = 0661313
034000100607     C                   EVAL      VABLNP = 066
034100100607     C                   EVAL      VATLNP = 066
034200100525     C                   EVAL      VABCTR = 000
034300100525     C                   EVAL      VABCBO = '1'
034400070222     C* ... e poi verifico se sono stati passati come parametri
034500070222     C                   IF        vlrppt > *blanks
034600100525     C*
034700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABCCM
035200070222     C                   Z-ADD     PiVal         VATCCM
035300070222     C                   ENDIF
035400070222     C                   ENDIF
035500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABLNP
036000070222     C                   Z-ADD     PiVal         VATLNP
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABCTR
036800070222     C                   ENDIF
036900070928     C                   ENDIF
037000071009     C                   MOVEL     *blanks       wDISK             1
037100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037200071009     C                   IF        wDISK <> *blanks
037300070928     C                   SETON                                        50
037400070222     C                   ENDIF
037500100525     C                   IF        wDISK  = 'T'
037600100525     C                   SETOFF                                       50
037700100525     C                   SETON                                        51
037800100525     C                   ENDIF
037900100401     C                   MOVEL     *blanks       wDUPREC           1
038000100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
038100100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
038200100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038300100525     C                   ENDIF
038400100525     C*
038500070222     C                   ENDIF
038600071009     C*
038700071009     C   50              EVAL      VABCTM = '7Q'
038800000801     C*
038900000801     C                   ENDSR
039000091223     C*----------------------------------------------------*
039100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039200091223     C*----------------------------------------------------*
039300091223     C     REPNSP        BEGSR
039400091223     C*
039500091223     C                   EXSR      INZVAR
039600091223     C                   EXSR      DEFCAM
039700091223     C*
039800091223     C* NSP => Stacco un numeratore da AZNUM
039900091223     C                   clear                   TRUL33DS
040000091223     C                   eval      I33OPE = *zeros
040100091223     C                   eval      I33CNU = 302
040200091223     C                   eval      I33NUM = 1
040300091223     C                   movel     TRUL33DS      KPJBU
040400091223     C                   call      'TRUL33R'
040500091223     C                   parm                    KPJBA
040600091223     C                   movel     KPJBU         TRUL33DS
040700091223     C                   if        O33ERR = *zeros
040800091223     C                   z-add     O33NRF        VABNSP
040900091223     C                   z-add     O33NRF        VATNSP
041000091223     C                   else
041100091223     C                   SETON                                        31
041200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041300091223     C                             + ' ' + 'VABNSP VATNSP'
041400091223     C                   endif
041500091223     C*
041600091223     C                   ENDSR
041700000801     C*----------------------------------------------------*
041800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041900000801     C*----------------------------------------------------*
042000010201     C     IMPVAB        BEGSR
042100070530     C*
042200070530     C                   SETOFF                                       3132
042300070928     C*
042400070928     C                   MOVE(P)   vlrpoi        VABFGS
042500070928     C                   MOVE(P)   vlrpoi        VATFGS
042600070928     C*
042700070928     C                   MOVEL     datcor        VABAAS
042800070928     C                   MOVEL     datcor        VATAAS
042900070928     C                   MOVE      datcor        VABMGS
043000100607     C                   EVAL      VABRSD=%trim(%subst(vindta:34:40))
043100100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043200100503     C     '@':'A'       XLATE     VABRSD        VABRSD
043300100503     C* ==
043400100607     C                   EVAL      VABRD2=%trim(%subst(vindta:74:40))
043500100607     C                   EVAL      VABIND=%trim(%subst(vindta:114:40))+
043600100607     C                                    %trim(%subst(vindta:154:40))+
043700100607     C                                    %trim(%subst(vindta:194:40))
043800100607     C                   EVAL      VABLOD=%trim(%subst(vindta:244:40))
043900100607     C***                EVAL      VABPRD=%trim(%subst(vindta:314:5))
044000100607     C                   EVAL      VABRMA=%trim(%subst(vindta:16:8))
044100100607     C                   MOVEL     *blanks       wNOTE            70
044200100607     C                   EVAL      wNOTE =%trim(%subst(vindta:384:70))+
044300100607     C                                    %trim(%subst(vindta:454:70))
044400100607     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
044500100607     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
044600100607     C*
044700100607     C* NZD
044800100607     C                   EVAL      wNAZ=%trim(%subst(vindta:283:45))
044900100607     C* ...passo dal codice ISO a quello "Bartolini"
045000100607     C                   Z-ADD     1             jNAZ
045100100607     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         55
045200100607     C                   IF        %found
045300100607     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
045400100607     C                   ENDIF
045500100923     C*
045600100923     C* CCM
045700100923     C                   IF        %subst(vindta:524:23) <> *blanks
045800100923     C                   EVAL      PiStr=%trim(%subst(vindta:524:23))
045900100923     C                   EXSR      CHKNUM
046000100923     C                   IF        PiInt=*on
046100100923     C                   Z-ADD     PiVal         VABCCM
046200100923     C                   Z-ADD     PiVal         VATCCM
046300100923     C                   ELSE
046400100923     C                   SETON                                        31
046500100923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046600100923     C                             + ' ' + 'VABCCM VATCCM'
046700100923     C                   ENDIF
046800100923     C                   ENDIF
046900100520     C*
047000100525     C* NSP
047100100525     C                   IF        *in50 = *off
047200100607     C                   EVAL      PiStr=%trim(%subst(vindta:16:8))
047300100525     C                   EXSR      CHKNUM
047400100525     C                   IF        PiInt=*on
047500100525     C                   Z-ADD     PiVal         VABNSP
047600100525     C                   Z-ADD     PiVal         VATNSP
047700100525     C                   ELSE
047800100525     C                   SETON                                        31
047900100525     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000100525     C                             + ' ' + 'VABNSP VATNSP'
048100100525     C                   ENDIF
048200100525     C                   ENDIF
048300100525     C* RMN
048400100607     C                   EVAL      PiStr=%trim(%subst(vindta:16:8))
048500100607     C                   EXSR      CHKNUM
048600100607     C                   IF        PiInt=*on
048700100607     C                   Z-ADD     PiVal         VABRMN
048800100607     C                   ELSE
048900100607     C                   SETON                                        32
049000100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100100607     C                             + ' ' + 'VABRMN'
049200100607     C                   ENDIF
049300100503     C* CAD
049400100607     C                   EVAL      PiStr=%trim(%subst(vindta:234:10))
049500100607     C                   EXSR      CHKNUM
049600100607     C                   IF        PiInt=*on
049700100607     C                   Z-ADD     PiVal         Num5_0
049800100607     C                   MOVEL     Num5_0        VABCAD
049900100607     C                   ELSE
050000100607     C                   SETON                                        32
050100100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200100607     C                             + ' ' + 'VABCAD'
050300100607     C                   ENDIF
050400100503     C* NCL
050500100607     C                   EVAL      PiStr=%trim(%subst(vindta:327:5))
050600100607     C                   EXSR      CHKNUM
050700100607     C                   IF        PiInt=*on
050800100607     C                   Z-ADD     PiVal         VABNCL
050900100607     C                   ELSE
051000100607     C                   SETON                                        32
051100100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200100607     C                             + ' ' + 'VABNCL'
051300100607     C                   ENDIF
051400100503     C* PKB
051500100607     C                   EVAL      PiStr=%trim(%subst(vindta:332:7))
051600100607     C                   EXSR      CHKNUM
051700100607     C                   IF        PiNum=*on
051800100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
051900100607     C                   Z-ADD     PiVal         VABPKB
052000100607     C                   ELSE
052100100607     C                   SETON                                        32
052200100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052300100607     C                             + ' ' + 'VABPKB'
052400100607     C                   ENDIF
052500100607     C* VLB
052600100607     C                   EVAL      PiStr=%trim(%subst(vindta:339:7))
052700100607     C                   EXSR      CHKNUM
052800100607     C                   IF        PiNum=*on
052900100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
053000100607     C                   Z-ADD     PiVal         VABVLB
053100100607     C                   ELSE
053200100607     C                   SETON                                        32
053300100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053400100607     C                             + ' ' + 'VABVLB'
053500100607     C                   ENDIF
053600100503     C* CAS
053700100607     C                   IF        %trim(%subst(vindta:346:9))<>*blanks
053800100607     C                             AND
053900100607     C                             %trim(%subst(vindta:346:9))<>*zeros
054000100607     C                   MOVEL     '1'           FlgCAS
054100100607     C                   EVAL      PiStr=%trim(%subst(vindta:346:9))
054200100607     C                   EXSR      CHKNUM
054300100607     C                   IF        PiNum=*on
054400100607     C                   EVAL      PiVal=PiVal / 100                            * gestisco 2 dec.
054500100607     C                   Z-ADD     PiVal         VABCAS
054600100607     C                   ELSE
054700100607     C                   SETON                                        32
054800100607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054900100607     C                             + ' ' + 'VABCAS'
055000100607     C                   ENDIF
055100100607     C                   ENDIF
055200100310     C*
055300100310     C* Se provincia nn valorizzata la reperisco
055400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
055500100607     C                   IF        VABNZD  = *blanks AND
055600100607     C                             VABPRD  = *blanks AND
055700100607     C                             VABCAD <> *blanks
055800100607     C                   CLEAR                   TISI95DS
055900100607     C                   EVAL      I95TCN = '3'
056000100607     C                   Z-ADD     datcor        I95DAT
056100100607     C                   EVAL      I95NAR = VABNZD
056200100607     C                   EVAL      I95CAP = VABCAD
056300100607     C                   EVAL      I95LOC = VABLOD
056400100607     C                   CALL      'TISI95R'
056500100607     C                   PARM                    TISI95DS
056600100607     C                   EVAL      VABPRD = O95PRV
056700100607     C                   ENDIF
056800070730     C*
056900070730     C* Considerazioni finali su CBO/CAS
057000100525     C***                IF        FlgCAS = '1'
057100100525     C***                IF        VABCBO = '1'
057200100525     C***                EVAL      VABCBO = '4'
057300100525     C***                ENDIF
057400100525     C***                IF        VABCBO = '2'
057500100525     C***                EVAL      VABCBO = '6'
057600100525     C***                ENDIF
057700100525     C***                ENDIF
057800010202     C*
057900000801     C* Ebbene...
058000000801     C                   ADD       1             §CTRMO
058100070530     C                   IF        *in31 <> *zeros OR
058200070530     C                             *in32 <> *zeros
058300000801     C                   ADD       1             §CTRNO
058400020725     C                   EVAL      x_vinflg = '2'
058500000801     C                   ELSE
058600010201     C                   ADD       1             §CTROKVB
058700000801     C                   ENDIF
058800000801     C*
058900000801     C                   ENDSR
059000100127     C*----------------------------------------------------*
059100100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059200100127     C*----------------------------------------------------*
059300100127     C     WRIVAT_A      BEGSR
059400100127     C*
059500100127     C* Valorizzo l buffer di scrittura del FIVAT
059600100127     C* - TIPO RECORD "A"
059700100412     C***                eval      VATTRC = 'A'
059800100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
059900100412     C***                exsr      wrivat
060000100127     C*
060100100127     C                   ENDSR
060200010201     C*----------------------------------------------------*
060300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060400010201     C*----------------------------------------------------*
060500071003     C     WRIVAT_B      BEGSR
060600010201     C*
060700021113     C* Valorizzo l buffer di scrittura del FIVAT
060800070928     C* - TIPO RECORD "B"
060900100412     C***                eval      VATTRC = 'B'
061000100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
061100100412     C***                exsr      wrivat
061200010201     C*
061300010201     C                   ENDSR
061400071003     C*----------------------------------------------------*
061500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
061600071003     C*----------------------------------------------------*
061700071003     C     WRIVAT_E      BEGSR
061800071003     C*
061900071003     C* Valorizzo l buffer di scrittura del FIVAT
062000071003     C* - TIPO RECORD "E"
062100100607     C                   eval      VATTRC = 'E'
062200100607     C                   eval      InStringa = %trim(%subst(vindta:547:1000))
062300100607     C*
062400100607     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
062500100607     C                   CALL      'XSPLIT2'
062600100607     C                   PARM                    InStringa
062700100607     C                   PARM      ';'           InSepara
062800100607     C                   PARM      '6'           InTypeOut
062900110616     C                   PARM      ''            wSkParam
063000100607     C                   PARM                    OutErrore
063100100607     C                   PARM                    OutMsg
063200100607     C                   MOVEA     wSkParam      SkSplitCSV_6
063300100607     C*
063400100607     C* Valorizzo la schiera delle intestazioni colonne (elementi)
063500100607     C                   z-add     1             i                 4 0
063600100607     C                   dow       i <= %elem(SkSplitCSV_6)
063700100607     C                   if        SkSplitCSV_6(i) = *blanks
063800100607     C                   leave
063900100607     C                   else
064000100607     C                   eval      vatnot = SkSplitCSV_6(i)
064100100607     C                   exsr      wrivat
064200100607     C                   endif
064300100607     C                   add       1             i
064400100607     C                   enddo
064500071003     C*
064600071003     C                   ENDSR
064700100127     C*----------------------------------------------------*
064800100127     C*  SCARICO BUFFER FIVAT
064900100127     C*----------------------------------------------------*
065000100127     C     WRIVAT        BEGSR
065100100127     C*
065200100127     C                   if        vatNOT <> *blanks
065300100127     C                   ADD       1             §CTROKVT
065400100127     C                   write     FIVAT000
065500100127     C                   endif
065600100127     C*
065700100127     C                   ENDSR
065800020904
065900020904
066000010202     C*----------------------------------------------------*
066100100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
066200010202     C*----------------------------------------------------*
066300100525     C     PREVASX       BEGSR
066400100525     C*
066500021113     C* Compongo il nome del membro da dare al FIVATWWR
066600010202     C                   eval      parmbr = vlrhdl
066700010202     C                   movel     'M'           parmbr
066800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066900010202     C                   eval      paropz = '1'
067000100525     C*
067100010202     C* Effettuo la chiamata al CLLE preposto
067200100525     C  N51              call(e)   'TITVVTC'
067300010202     C                   parm                    parccm
067400010202     C                   parm                    parmbr
067500010202     C                   parm                    paropz
067600100525     C   51              call(e)   'TITVVBC'
067700100525     C                   parm                    parccm
067800100525     C                   parm                    parmbr
067900100525     C                   parm                    paropz
068000100525     C*
068100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
068200010202     C                   if        %error
068300010202     C                   movel     '1'           chkcall
068400010202     C                   else
068500010202     C                   movel     '0'           chkcall
068600010202     C                   endif
068700010202     C*
068800010202     C                   ENDSR
068900000801     C*----------------------------------------------------*
069000000801     C*  CONTROLLO NUMERICITA' CAMPI
069100000801     C*----------------------------------------------------*
069200000801     C     CHKNUM        BEGSR
069300081113     C*
069400081113     C                   IF        PiDecChr = *blanks
069500100503     C                   EVAL      PiDecChr = '.'
069600081113     C                   ENDIF
069700091223     C*
069800081113     C                   callp(e)  UBISNUM_Check(PiStr
069900081113     C                                          :PiDecChr
070000081113     C                                          :PiVal
070100081113     C                                          :PiNum
070200081113     C                                          :PiInt)
070300081113     C*
070400000801     C                   IF        %error
070500000801     C                   EVAL      PiInt=*off
070600000801     C                   ENDIF
070700000801     C*
070800000801     C                   ENDSR
070900000801     C***
071000100607
071100100607
071200100607     C*--------------------------------------------------------
071300100607     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
071400100607     C*--------------------------------------------------------
071500100607     C     CARTAB        BEGSR
071600100607     C*
071700100607     C* TABELLA '15' - NAZIONI
071800100607     C                   clear                   skNAZISO
071900100607     C                   clear                   skNAZBAR
072000100607     C                   eval      tblKUT = 1
072100100607     C                   eval      tblCOD = '15'
072200100607     C     KEYtabP       setll     tabel00f
072300100607     C     KEYtabP       reade     tabel00f
072400100607     C                   dow       not %eof(tabel00f)
072500100607     C                   if        tblFLG = *blanks
072600100607     C                   movel(p)  tblUNI        ds15
072700100607     C                   add       1             jNAZ
072800100607     C                   eval      skNAZBAR(jNAZ) = tblKEY
072900100607     C                   eval      skNAZISO(jNAZ) = §15COD
073000100607     C                   endif
073100100607     C     KEYtabP       reade     tabel00f
073200100607     C                   enddo
073300100607     C*
073400100607     C                   ENDSR
073500100607     C***
073600011113
073700011113
073800000801
073900000801
074000990920      /TITLE Invio dei dati al punto operativo.
074100010202     C     invio         BEGSR
074200990920     C*
074300021113     C* 1° invio FIVAT
074400010201     C                   reset                   dscmz
074500010201     C                   move      vlrpoi        cmzdst
074600021113     C                   eval      cmzfld = 'FIVATWWR'
074700010201     C                   eval      cmzmbd = vlrhdl
074800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074900021009     C***                if        prmfir = *blanks
075000021113     C                   eval      cmzfla = 'FIVAT00F'
075100021113     C                   eval      cmzmba = 'FIVAT00F'
075200021009     C***                else
075300021009     C***                eval      cmzfla = prmfir
075400021009     C***                eval      cmzmba = prmfir
075500021009     C***                endif
075600010201     C                   eval      cmznrr = *zeros
075700020305     C                   move      §ctrokvt      cmznrr
075800021018     C                   eval      cmzlba = vlrfl1
075900010201     C                   call(e)   'TIS711C'
076000010201     C                   parm                    dscmz
076100010201     C                   parm      *blanks       esito
076200010205     C                   if        %error
076300010205     C                             or cmzerr = '1'
076400010205     C                             or esito  = '1'
076500010205     C                   eval      wrkesito = '3'
076600010205     C                   else
076700010201     C*
076800021113     C* 2° invio FIVAB
076900100525     C                   if        *in51 = *off
077000010201     C                   reset                   dscmz
077100010201     C                   move      vlrpoi        cmzdst
077200010201     C                   eval      cmzfld = vlrfou
077300010201     C                   eval      cmzmbd = vlrhdl
077400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077500021009     C***                if        prmfir = *blanks
077600021113     C                   eval      cmzfla = 'FIVAB00F'
077700021113     C                   eval      cmzmba = 'FIVAB00F'
077800021009     C***                else
077900021009     C***                eval      cmzfla = prmfir
078000021009     C***                eval      cmzmba = prmfir
078100021009     C***                endif
078200010201     C                   eval      cmznrr = *zeros
078300010201     C                   move      §ctrokvb      cmznrr
078400021018     C                   eval      cmzlba = vlrfl1
078500010201     C                   call(e)   'TIS711C'
078600010201     C                   parm                    dscmz
078700010201     C                   parm      *blanks       esito
078800010201     C                   if        %error
078900010201     C                             or cmzerr = '1'
079000010201     C                             or esito  = '1'
079100010201     C                   eval      wrkesito = '3'
079200010201     C                   endif
079300100525     C                   endif
079400010205     C                   endif
079500990920     C*
079600000613     C                   ENDSR
079700000613     C***
079800070411
079900070411     C     *pssr         BEGSR
080000070411     C*
080100070411     C                   if        %open(tivin00r)
080200070411     C                   close     tivin00r
080300070411     C                   endif
080400070411     C                   if        %open(fivabwwr)
080500070411     C                   close     fivabwwr
080600070411     C                   endif
080700070411     C                   if        %open(fivatwwr)
080800070411     C                   close     fivatwwr
080900070411     C                   endif
081000070411     C*
081100070411     C* Effettuo la chiamata al CLLE preposto
081200100525     C  N51              call(e)   'TITVVTC'
081300100525     C                   parm                    parccm
081400100525     C                   parm                    parmbr
081500100525     C                   parm      '2'           paropz
081600100525     C   51              call(e)   'TITVVBC'
081700100525     C                   parm                    parccm
081800100525     C                   parm                    parmbr
081900100525     C                   parm      '2'           paropz
082000070411     C*
082100070411     C                   eval      wrkesito = '2'
082200070411     C*
082300070411     C                   seton                                        LR
082400070411     C*
082500070411     C                   ENDSR     '*CANCL'
082600070411     C***
082700070411
082800990910
082900000613     C     *inzsr        BEGSR
083000990910     C*
083100990910     C     *entry        plist
083200990920     C                   parm                    tivlrds
083300990921     C                   parm      wrkesito      esito
083400000724     C                   parm                    prmlit
083500000710     C                   parm                    prmfir
083600100607     C*
083700100607     C* Chiave su TABEL00F - parziale
083800100607     C     KEYtabP       KLIST
083900100607     C                   KFLD                    tblKUT
084000100607     C                   KFLD                    tblCOD
084100000613     C*
084200000830     C* CALCOLA LA DATA CORRENTE
084300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084400091223     C                   eval      datcor = %dec(%date() : *ISO)
084500000830     C*
084600000613     C                   ENDSR
084700000613     C***
