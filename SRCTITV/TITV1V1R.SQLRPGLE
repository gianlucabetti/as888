000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
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
002100100127     D depspe          s             80    INZ(*blanks)
002200100127     D curspe          s             80    INZ(*blanks)
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
004300091223     D trul33ds      e ds                  inz
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
007300050128     C                   call(e)   'TITVVTC'
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
008900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009000910830     C*--------------------------------------------------------
009100070530     C     RWFILE        BEGSR
009200990910     C*
009300990914     C                   if        not %open(tivin00r)
009400990908     C                   open      tivin00r
009500990914     C                   endif
009600021113     C                   if        not %open(fivabwwr)
009700021113     C                   open      fivabwwr
009800990914     C                   endif
009900070530     C*
010000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010100020305     C                   exsr      prevat
010200010201     C*
010300010202     C                   if        chkcall = '0'
010400010202     C*
010500021113     C                   if        not %open(fivatwwr)
010600021113     C                   open      fivatwwr
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
011900100222     C+ vindta, vinflg, substr(vindta, 17, 12) as sped, rrn(tivin00r)
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
013500020725     C                   if        vindta > *blanks
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
014600100222     C                   EVAL      curspe=%trim(%subst(vindta:17:12))
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
023800021113     C                   if        %open(fivabwwr)
023900021113     C                   close     fivabwwr
024000990914     C                   endif
024100021113     C                   if        %open(fivatwwr)
024200021113     C                   close     fivatwwr
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
026200071003     C*
026300021113     C                   write     fivab000                                     => scarico il VAB
026400010305     C*
026500010305     C                   ENDSR
026600990920
026700000801     C*----------------------------------------------------*
026800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026900000801     C*----------------------------------------------------*
027000010201     C     INZVAR        BEGSR
027100000801     C*
027200010201     C                   Z-ADD     *zeros        Num5_0
027300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027400020725     C                   MOVEL     '0'           FlgCAS            1
027500000801     C*
027600000801     C                   ENDSR
027700000801     C*----------------------------------------------------*
027800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027900000801     C*----------------------------------------------------*
028000020904     C     DEFCAM        BEGSR
028100080923     C*
028200090310     C                   SETOFF                                       50
028300000801     C*
028400021113     C                   CLEAR                   FIVAB000
028500021113     C                   CLEAR                   FIVAT000
028600070730     C* Imposto i valori di default...
028700100222     C                   EVAL      VABCCM = 0895142
028800100222     C                   EVAL      VATCCM = 0895142
028900100222     C                   EVAL      VABLNP = 089
029000100222     C                   EVAL      VATLNP = 089
029100100222     C                   EVAL      VABCTR = 300
029200070730     C                   EVAL      VABCBO = '1'
029300100222     C                   EVAL      VABCTM = '7Q'
029400100222     C                   EVAL      VABTSP = 'D'
029500100222     C                   EVAL      VABNCL = 1
029600100222     C                   EVAL      VABPKB = 1
029700100222     C                   EVAL      VABNAS = 'VARIE'
029800100315     C                   EVAL      VABNOT ='RICHIESTA VARIAZIONE INDIRIZZO E'
029900100315     C                   EVAL      VABNT2 ='FERMO DEP AUTORIZZATO - NO GIACENZA'
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
033000070222     C                   ENDIF
033100071009     C*
033200071009     C   50              EVAL      VABCTM = '7Q'
033300000801     C*
033400000801     C                   ENDSR
033500091223     C*----------------------------------------------------*
033600091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033700091223     C*----------------------------------------------------*
033800091223     C     REPNSP        BEGSR
033900091223     C*
034000091223     C                   EXSR      INZVAR
034100091223     C                   EXSR      DEFCAM
034200091223     C*
034300091223     C* NSP => Stacco un numeratore da AZNUM
034400091223     C                   clear                   TRUL33DS
034500091223     C                   eval      I33OPE = *zeros
034600091223     C                   eval      I33CNU = 302
034700091223     C                   eval      I33NUM = 1
034800091223     C                   movel     TRUL33DS      KPJBU
034900091223     C                   call      'TRUL33R'
035000091223     C                   parm                    KPJBA
035100091223     C                   movel     KPJBU         TRUL33DS
035200091223     C                   if        O33ERR = *zeros
035300091223     C                   z-add     O33NRF        VABNSP
035400091223     C                   z-add     O33NRF        VATNSP
035500091223     C                   else
035600091223     C                   SETON                                        31
035700091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035800091223     C                             + ' ' + 'VABNSP VATNSP'
035900091223     C                   endif
036000091223     C*
036100091223     C                   ENDSR
036200000801     C*----------------------------------------------------*
036300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036400000801     C*----------------------------------------------------*
036500010201     C     IMPVAB        BEGSR
036600070530     C*
036700070530     C                   SETOFF                                       3132
036800070928     C*
036900070928     C                   MOVE(P)   vlrpoi        VABFGS
037000070928     C                   MOVE(P)   vlrpoi        VATFGS
037100070928     C*
037200070928     C                   MOVEL     datcor        VABAAS
037300070928     C                   MOVEL     datcor        VATAAS
037400070928     C                   MOVE      datcor        VABMGS
037500100222     C                   EVAL      VABRSD=%trim(%subst(vindta:29:30))
037600070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037700070928     C     '@':'A'       XLATE     VABRSD        VABRSD
037800070928     C* ==
037900100222     C                   EVAL      VABIND=%trim(%subst(vindta:59:30))
038000100224     C                   MOVEL     *blanks       wLOD             35
038100100224     C                   EVAL      wLOD=%trim(%subst(vindta:94:30))
038200100224     C                   IF        %len(%trim(wLOD)) > 2
038300100224     C                   EVAL      VABLOD=%trim(%subst(%trim(wLOD):1:
038400100224     C                                    %len(%trim(wLOD))-3))
038500100224     C                   EVAL      VABPRD=%trim(%subst(%trim(wLOD):
038600100224     C                                    %len(%trim(wLOD))-2+1:2))
038700100222     C                   ELSE
038800100224     C                   EVAL      VABLOD=wLOD
038900100222     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
039000100222     C                   ENDIF
039100100222     C                   EVAL      VABRMA=%trim(%subst(vindta:17:12))
039200071003     C* CAD
039300100222     C                   EVAL      PiStr=%trim(%subst(vindta:89:5))
039400070928     C                   EXSR      CHKNUM
039500070928     C                   IF        PiInt=*on
039600070928     C                   Z-ADD     PiVal         Num5_0
039700070928     C                   MOVEL     Num5_0        VABCAD
039800070928     C                   ELSE
039900070928     C                   SETON                                        32
040000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100070928     C                             + ' ' + 'VABCAD'
040200070928     C                   ENDIF
040300080923     C* RMN
040400100222     C                   EVAL      PiStr=%trim(%subst(vindta:17:12))
040500080923     C                   EXSR      CHKNUM
040600080923     C                   IF        PiInt=*on
040700080923     C                   Z-ADD     PiVal         VABRMN
040800080923     C                   ELSE
040900080923     C                   SETON                                        32
041000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041100080923     C                             + ' ' + 'VABRMN'
041200080923     C                   ENDIF
041300070730     C*
041400070730     C* Considerazioni finali su CBO/CAS
041500070730     C                   IF        FlgCAS = '1'
041600070730     C                   IF        VABCBO = '1'
041700070730     C                   EVAL      VABCBO = '4'
041800070730     C                   ENDIF
041900070730     C                   IF        VABCBO = '2'
042000070730     C                   EVAL      VABCBO = '6'
042100070730     C                   ENDIF
042200070730     C                   ENDIF
042300020305     C*
042400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042500011113     C                   EXSR      CHKIMPDIV
042600010202     C*
042700000801     C* Ebbene...
042800000801     C                   ADD       1             §CTRMO
042900070530     C                   IF        *in31 <> *zeros OR
043000070530     C                             *in32 <> *zeros
043100000801     C                   ADD       1             §CTRNO
043200020725     C                   EVAL      x_vinflg = '2'
043300000801     C                   ELSE
043400010201     C                   ADD       1             §CTROKVB
043500000801     C                   ENDIF
043600000801     C*
043700000801     C                   ENDSR
043800100127     C*----------------------------------------------------*
043900100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
044000100127     C*----------------------------------------------------*
044100100127     C     WRIVAT_A      BEGSR
044200100127     C*
044300100127     C* Valorizzo l buffer di scrittura del FIVAT
044400100127     C* - TIPO RECORD "A"
044500100127     C***                eval      VATTRC = 'A'
044600100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
044700100127     C***                exsr      wrivat
044800100127     C*
044900100127     C                   ENDSR
045000010201     C*----------------------------------------------------*
045100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
045200010201     C*----------------------------------------------------*
045300071003     C     WRIVAT_B      BEGSR
045400010201     C*
045500021113     C* Valorizzo l buffer di scrittura del FIVAT
045600070928     C* - TIPO RECORD "B"
045700100127     C***                eval      VATTRC = 'B'
045800100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
045900100127     C***                exsr      wrivat
046000010201     C*
046100010201     C                   ENDSR
046200071003     C*----------------------------------------------------*
046300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
046400071003     C*----------------------------------------------------*
046500071003     C     WRIVAT_E      BEGSR
046600071003     C*
046700071003     C* Valorizzo l buffer di scrittura del FIVAT
046800071003     C* - TIPO RECORD "E"
046900100222     C                   eval      VATTRC = 'E'
047000100222     C                   eval      VATNOT = %trim(%subst(vindta:17:12))
047100100222     C                   exsr      wrivat
047200071003     C*
047300071003     C                   ENDSR
047400100127     C*----------------------------------------------------*
047500100127     C*  SCARICO BUFFER FIVAT
047600100127     C*----------------------------------------------------*
047700100127     C     WRIVAT        BEGSR
047800100127     C*
047900100127     C                   if        vatNOT <> *blanks
048000100127     C                   ADD       1             §CTROKVT
048100100127     C                   write     FIVAT000
048200100127     C                   endif
048300100127     C*
048400100127     C                   ENDSR
048500020904
048600020904
048700010202     C*----------------------------------------------------*
048800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048900010202     C*----------------------------------------------------*
049000020305     C     PREVAT        BEGSR
049100010202     C*
049200021113     C* Compongo il nome del membro da dare al FIVATWWR
049300010202     C                   eval      parmbr = vlrhdl
049400010202     C                   movel     'M'           parmbr
049500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
049600010202     C                   eval      paropz = '1'
049700010202     C* Effettuo la chiamata al CLLE preposto
049800050128     C                   call(e)   'TITVVTC'
049900010202     C                   parm                    parccm
050000010202     C                   parm                    parmbr
050100010202     C                   parm                    paropz
050200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050300010202     C                   if        %error
050400010202     C                   movel     '1'           chkcall
050500010202     C                   else
050600010202     C                   movel     '0'           chkcall
050700010202     C                   endif
050800010202     C*
050900010202     C                   ENDSR
051000000801     C*----------------------------------------------------*
051100000801     C*  CONTROLLO NUMERICITA' CAMPI
051200000801     C*----------------------------------------------------*
051300000801     C     CHKNUM        BEGSR
051400081113     C*
051500081113     C                   IF        PiDecChr = *blanks
051600091223     C                   EVAL      PiDecChr = ','
051700081113     C                   ENDIF
051800091223     C*
051900081113     C                   callp(e)  UBISNUM_Check(PiStr
052000081113     C                                          :PiDecChr
052100081113     C                                          :PiVal
052200081113     C                                          :PiNum
052300081113     C                                          :PiInt)
052400081113     C*
052500000801     C                   IF        %error
052600000801     C                   EVAL      PiInt=*off
052700000801     C                   ENDIF
052800000801     C*
052900000801     C                   ENDSR
053000000801     C***
053100000801
053200011113
053300011113     C*----------------------------------------------------*
053400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053500011113     C*----------------------------------------------------*
053600011113     C     CHKIMPDIV     BEGSR
053700011113     C*
053800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053900011113     C                   Z-ADD     *zeros        wrkDec            9 9
054000011113     C*
054100011113     C* Come prima cosa effettuo considerazioni sulla divisa
054200011113     C                   IF        vabIAS > *zeros
054300011113     C                   IF        vabVAS <> 'EUR'
054400011113     C                   EVAL      vabVAS =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabCAS > *zeros
054900011113     C                   IF        vabVCA <> 'EUR'
055000011113     C                   EVAL      vabVCA =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C                   IF        vabVMD > *zeros
055500020305     C                   IF        vabVAD <> 'EUR'
055600011113     C                   EVAL      vabVAD =  'ITL'
055700011113     C                   ENDIF
055800011113     C                   ENDIF
055900011113     C*
056000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056100011113     C                   Z-ADD     vabIAS        wrkDec
056200011113     C                   IF        wrkDec > *zeros
056300011113     C                   IF        vabVAS = 'ITL'
056400011113     C                   EVAL      vabIAS = *zeros
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056900011113     C                   Z-ADD     vabCAS        wrkDec
057000011113     C                   IF        wrkDec > *zeros
057100011113     C                   IF        vabVCA = 'ITL'
057200011113     C                   EVAL      vabCAS = *zeros
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057700011113     C                   Z-ADD     vabVMD        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVAD = 'ITL'
058000011113     C                   EVAL      vabVMD = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C                   ENDSR
058500011113     C***
058600011113
058700011113
058800000801
058900000801
059000990920      /TITLE Invio dei dati al punto operativo.
059100010202     C     invio         BEGSR
059200990920     C*
059300021113     C* 1° invio FIVAT
059400010201     C                   reset                   dscmz
059500010201     C                   move      vlrpoi        cmzdst
059600021113     C                   eval      cmzfld = 'FIVATWWR'
059700010201     C                   eval      cmzmbd = vlrhdl
059800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059900021009     C***                if        prmfir = *blanks
060000021113     C                   eval      cmzfla = 'FIVAT00F'
060100021113     C                   eval      cmzmba = 'FIVAT00F'
060200021009     C***                else
060300021009     C***                eval      cmzfla = prmfir
060400021009     C***                eval      cmzmba = prmfir
060500021009     C***                endif
060600010201     C                   eval      cmznrr = *zeros
060700020305     C                   move      §ctrokvt      cmznrr
060800021018     C                   eval      cmzlba = vlrfl1
060900010201     C                   call(e)   'TIS711C'
061000010201     C                   parm                    dscmz
061100010201     C                   parm      *blanks       esito
061200010205     C                   if        %error
061300010205     C                             or cmzerr = '1'
061400010205     C                             or esito  = '1'
061500010205     C                   eval      wrkesito = '3'
061600010205     C                   else
061700010201     C*
061800021113     C* 2° invio FIVAB
061900010201     C                   reset                   dscmz
062000010201     C                   move      vlrpoi        cmzdst
062100010201     C                   eval      cmzfld = vlrfou
062200010201     C                   eval      cmzmbd = vlrhdl
062300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062400021009     C***                if        prmfir = *blanks
062500021113     C                   eval      cmzfla = 'FIVAB00F'
062600021113     C                   eval      cmzmba = 'FIVAB00F'
062700021009     C***                else
062800021009     C***                eval      cmzfla = prmfir
062900021009     C***                eval      cmzmba = prmfir
063000021009     C***                endif
063100010201     C                   eval      cmznrr = *zeros
063200010201     C                   move      §ctrokvb      cmznrr
063300021018     C                   eval      cmzlba = vlrfl1
063400010201     C                   call(e)   'TIS711C'
063500010201     C                   parm                    dscmz
063600010201     C                   parm      *blanks       esito
063700010201     C                   if        %error
063800010201     C                             or cmzerr = '1'
063900010201     C                             or esito  = '1'
064000010201     C                   eval      wrkesito = '3'
064100010201     C                   endif
064200010205     C                   endif
064300990920     C*
064400000613     C                   ENDSR
064500000613     C***
064600070411
064700070411     C     *pssr         BEGSR
064800070411     C*
064900070411     C                   if        %open(tivin00r)
065000070411     C                   close     tivin00r
065100070411     C                   endif
065200070411     C                   if        %open(fivabwwr)
065300070411     C                   close     fivabwwr
065400070411     C                   endif
065500070411     C                   if        %open(fivatwwr)
065600070411     C                   close     fivatwwr
065700070411     C                   endif
065800070411     C*
065900070411     C* Effettuo la chiamata al CLLE preposto
066000070411     C                   call(e)   'TITVVTC'
066100070411     C                   parm                    parccm
066200070411     C                   parm                    parmbr
066300070411     C                   parm      '2'           paropz
066400070411     C*
066500070411     C                   eval      wrkesito = '2'
066600070411     C*
066700070411     C                   seton                                        LR
066800070411     C*
066900070411     C                   ENDSR     '*CANCL'
067000070411     C***
067100070411
067200990910
067300000613     C     *inzsr        BEGSR
067400990910     C*
067500990910     C     *entry        plist
067600990920     C                   parm                    tivlrds
067700990921     C                   parm      wrkesito      esito
067800000724     C                   parm                    prmlit
067900000710     C                   parm                    prmfir
068000000613     C*
068100000830     C* CALCOLA LA DATA CORRENTE
068200091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068300091223     C                   eval      datcor = %dec(%date() : *ISO)
068400000830     C*
068500000613     C                   ENDSR
068600000613     C***
