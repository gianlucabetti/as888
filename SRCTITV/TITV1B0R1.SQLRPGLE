000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
002100100922     D depspe          s             11    INZ(*blanks)
002200100922     D curspe          s             11    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800070530     D Num5_0          s              5  0
002900000830
003000000830     D*------------------
003100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200000830     D*------------------
003300000830     D WLBDA8          DS                  INZ
003400000830     D  G08DAT                 1      8  0
003500000830     D  G08INV                 9     16  0
003600000830     D  G08ERR                17     17
003700000830     D  G08TGI                18     22  0
003800000830     D*
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400020725     D  VINDTA                 1   2048
004500020725     D  VINFLG              2049   2049
004600020725     D  VINSPE              2050   2059
004700020725     D  VINRRN              2060   2067  0
004800090112     D*------------------
004900090112     D* DS REPERIMENTO NUMERATORE
005000090112     D*------------------
005100090112     D trul33ds      e ds                  inz
005200090112     D*------------------
005300090112     D* DS ARCHITETTURA
005400090112     D*------------------
005500090112     D kpjba         e ds                  inz
005600990908
005700010201
005800010201
005900071120     C*
006000071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100071120     C
006200071120     C/EXEC SQL
006300071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400071120     C/END-EXEC
006500071120     C*
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
010800990910     C*
010900010201     C                   clear                   §CTROKVB          5 0
011000020305     C                   clear                   §CTROKVT          5 0
011100000801     C                   clear                   §CTRMO            5 0
011200000801     C                   clear                   §CTRNO            5 0
011300990910     C*
011400020725     C/EXEC SQL
011500020725     C+ declare C1 cursor for select
011600100922     C+ vindta, vinflg, substr(vindta, 351, 11) as sped, rrn(tivin00r)
011700060223     C+ from tivin00r
011800060223     C+ order by sped
011900020725     C+ for read only
012000020725     C/END-EXEC
012100020725     C
012200020725     C/EXEC SQL
012300020725     C+ open C1
012400020725     C/END-EXEC
012500020725     C
012600020725     C/EXEC SQL
012700070530     C+ Fetch C1 into :INPUT_DS
012800020725     C/END-EXEC
012900020725     C*
013000020725     C                   dow       sqlcod = *zeros
013100990913     C*
013200020725     C                   if        vindta > *blanks
013300000613     C                   add       1             rrnum
013400000801     C*
013500020725     C                   if        vinflg = *blanks
013600020725     C                             or vinflg = '0'
013700020725     C                             or vinflg = '2'
013800000801     C*
013900020725     C                   clear                   x_vinmsg
014000020725     C                   eval      x_vinflg = '1'
014100010305     C*
014200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014300100922     C                   EVAL      PiStr=%trim(%subst(vindta:351:11))
014400020305     C                   MOVEL(p)  PiStr         curspe
014500010305     C*
014600010305     C                   if        depspe = *blanks                             => 1° giro
014700010305     C                   eval      depspe = curspe                              => memorizz. spediz
014800020725     C                   clear                   tivinds
014900090112     C                   exsr      repNSP
015000020305     C                   exsr      impvab
015100070928     C   50              exsr      wrivat                                       => carico VAT
015200010305     C                   else
015300020725     C                   if        depspe <> curspe                             => rottura di spediz
015400010305     C                   eval      depspe = curspe                              => memorizz. spediz
015500070928     C                   exsr      wrivab
015600020725     C                   clear                   tivinds
015700090112     C                   exsr      repNSP
015800020305     C                   exsr      impvab
015900070928     C   50              exsr      wrivat                                       => carico VAT
016000020305     C                   else                                                   => x stessa spediz
016100020305     C                   exsr      impvab
016200070928     C   50              exsr      wrivat                                       => carico VAT
016300010305     C                   endif
016400010305     C                   endif
016500010305     C                   endif
016600000905     C*
016700000905     C                   else
016800020725     C                   eval      x_vinflg = '1'
016900000905     C                   endif
017000000905     C*
017100020725     C     VINRRN        chain     tivin000
017200020725     C                   if        %found(tivin00r)
017300020725     C                   eval      y_vinflg = x_vinflg
017400020725     C                   eval      y_vinmsg = x_vinmsg
017500020725     C                   update    tivin000
017600020725     C                   endif
017700020725     C*
017800020725     C/EXEC SQL
017900070530     C+ Fetch C1 into :INPUT_DS
018000020725     C/END-EXEC
018100020725     C*
018200020725     C                   enddo
018300020725     C*
018400020725     C/EXEC SQL
018500020725     C+ close C1
018600020725     C/END-EXEC
018700000905     C*
018800020305     C* Scarico i VAB rimasti "in sospeso"
018900020305     C                   exsr      wrivab
019000010202     C*
019100010202     C                   endif
019200990910
019300990910     C* Se non ci sono record con errori ...
019400000710     C                   if        §ctrno = 0
019500990910     C* ... restituisco esito OK.
019600990921     C                   eval      wrkesito = '0'
019700990910     C                   else
019800010201     C                   if        §ctrokvb > 0
019900990921     C                   eval      wrkesito = '1'
020000000710     C                   else
020100000710     C                   eval      wrkesito = '2'
020200990910     C                   endif
020300000710     C                   endif
020400990910     C*
020500990914     C                   if        %open(tivin00r)
020600990908     C                   close     tivin00r
020700990914     C                   endif
020800021113     C                   if        %open(fivabwwr)
020900021113     C                   close     fivabwwr
021000990914     C                   endif
021100021113     C                   if        %open(fivatwwr)
021200021113     C                   close     fivatwwr
021300010201     C                   endif
021400990910     C*
021500010201     C                   if        §ctrokvb > 0
021600000724     C                             and vlrpoi <> *zeros
021700010202     C                   exsr      invio
021800990920     C                   endif
021900990920     C*
022000910830     C                   ENDSR
022100000613     C***
022200010305
022300010305     C*----------------------------------------------------*
022400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022500010305     C*----------------------------------------------------*
022600020305     C     WRIVAB        BEGSR
022700070730     C*
022800021113     C                   write     fivab000                                     => scarico il VAB
022900010305     C*
023000010305     C                   ENDSR
023100990920
023200000801     C*----------------------------------------------------*
023300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023400000801     C*----------------------------------------------------*
023500010201     C     INZVAR        BEGSR
023600000801     C*
023700010201     C                   Z-ADD     *zeros        Num5_0
023800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023900020725     C                   MOVEL     '0'           FlgCAS            1
024000000801     C*
024100000801     C                   ENDSR
024200000801     C*----------------------------------------------------*
024300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024400000801     C*----------------------------------------------------*
024500020904     C     DEFCAM        BEGSR
024600000801     C*
024700021113     C                   CLEAR                   FIVAB000
024800021113     C                   CLEAR                   FIVAT000
024900070730     C* Imposto i valori di default...
025000100922     C                   EVAL      VABCCM = 0064286
025100100922     C                   EVAL      VATCCM = 0064286
025200071002     C                   EVAL      VABLNP = 006
025300071002     C                   EVAL      VATLNP = 006
025400070928     C                   EVAL      VABCTR = 000
025500070730     C                   EVAL      VABCBO = '1'
025600070928     C                   EVAL      VABCTM = '7Q'
025700070222     C* ... e poi verifico se sono stati passati come parametri
025800070222     C                   IF        vlrppt > *blanks
025900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
026000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026100070222     C                   EXSR      CHKNUM
026200070222     C                   IF        PiInt=*on
026300070222     C                   Z-ADD     PiVal         VABCCM
026400070222     C                   Z-ADD     PiVal         VATCCM
026500070222     C                   ENDIF
026600070222     C                   ENDIF
026700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
026800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026900070222     C                   EXSR      CHKNUM
027000070222     C                   IF        PiInt=*on
027100070222     C                   Z-ADD     PiVal         VABLNP
027200070222     C                   Z-ADD     PiVal         VATLNP
027300070222     C                   ENDIF
027400070222     C                   ENDIF
027500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
027600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027700070222     C                   EXSR      CHKNUM
027800070222     C                   IF        PiInt=*on
027900070222     C                   Z-ADD     PiVal         VABCTR
028000070222     C                   ENDIF
028100070928     C                   ENDIF
028200070928     C                   IF        %subst(vlrppt:14:1) = 'C'
028300070928     C                   SETON                                        50
028400070928     C                   ELSE
028500070928     C                   SETOFF                                       50
028600070222     C                   ENDIF
028700070222     C                   ENDIF
028800000801     C*
028900000801     C                   ENDSR
029000090112     C*----------------------------------------------------*
029100090112     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
029200090112     C*----------------------------------------------------*
029300090112     C     REPNSP        BEGSR
029400090112     C*
029500090112     C                   EXSR      INZVAR
029600090112     C                   EXSR      DEFCAM
029700090112     C*
029800090112     C* NSP => Stacco un numeratore da AZNUM
029900090112     C                   clear                   TRUL33DS
030000090112     C                   eval      I33OPE = *zeros
030100090112     C                   eval      I33CNU = 302
030200090112     C                   eval      I33NUM = 1
030300090112     C                   movel     TRUL33DS      KPJBU
030400090112     C                   call      'TRUL33R'
030500090112     C                   parm                    KPJBA
030600090112     C                   movel     KPJBU         TRUL33DS
030700090112     C                   if        O33ERR = *zeros
030800090112     C                   z-add     O33NRF        VABNSP
030900090112     C                   z-add     O33NRF        VATNSP
031000090112     C                   else
031100090112     C                   SETON                                        31
031200090112     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
031300090112     C                             + ' ' + 'VABNSP VATNSP'
031400090112     C                   endif
031500090112     C*
031600090112     C                   ENDSR
031700000801     C*----------------------------------------------------*
031800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031900000801     C*----------------------------------------------------*
032000010201     C     IMPVAB        BEGSR
032100070530     C*
032200070530     C                   SETOFF                                       3132
032300070928     C*
032400070928     C                   MOVE(P)   vlrpoi        VABFGS
032500070928     C                   MOVE(P)   vlrpoi        VATFGS
032600070928     C*
032700070928     C                   MOVEL     datcor        VABAAS
032800070928     C                   MOVEL     datcor        VATAAS
032900070928     C                   MOVE      datcor        VABMGS
033000100922     C                   EVAL      VABRSD=%trim(%subst(vindta:159:40))
033100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
033300070928     C* ==
033400100922     C                   EVAL      VABIND=%trim(%subst(vindta:199:35))
033500100922     C                   EVAL      VABLOD=%trim(%subst(vindta:261:30))
033600100922     C                   EVAL      VABCAD=%trim(%subst(vindta:252:9))
033700100922     C                   EVAL      VABPRD=%trim(%subst(vindta:291:2))
033800101129     C***                EVAL      VABRMA=%trim(%subst(vindta:351:11))
033900101129     C                   EVAL      VABRMA=%trim(%subst(vindta:529:15))
034000100922     C                   EVAL      VABVCA=%trim(%subst(vindta:376:3))
034100100922     C                   EVAL      VABNOT=%trim(%subst(vindta:389:35))
034200100922     C                   EVAL      VABNT2=%trim(%subst(vindta:389+35:70-35))
034300090112     C* RMN
034400100922     C                   EVAL      PiStr=%trim(%subst(vindta:295:6))
034500090112     C                   EXSR      CHKNUM
034600090112     C                   IF        PiInt=*on
034700090112     C                   Z-ADD     PiVal         VABRMN
034800090112     C                   ELSE
034900090112     C                   SETON                                        31
035000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035100090112     C                             + ' ' + 'VABRMN'
035200070928     C                   ENDIF
035300071003     C* NCL
035400100922     C                   EVAL      PiStr=%trim(%subst(vindta:311:5))
035500070928     C                   EXSR      CHKNUM
035600070928     C                   IF        PiInt=*on
035700070928     C                   Z-ADD     PiVal         VABNCL
035800070928     C                   ELSE
035900070928     C                   SETON                                        32
036000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036100070928     C                             + ' ' + 'VABNCL'
036200070928     C                   ENDIF
036300071003     C* PKB
036400100922     C                   EVAL      PiStr=%trim(%subst(vindta:316:10))
036500070928     C                   EXSR      CHKNUM
036600070928     C                   IF        PiNum=*on
036700070928     C                   Z-ADD     PiVal         VABPKB
036800070928     C                   ELSE
036900070928     C                   SETON                                        32
037000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037100070928     C                             + ' ' + 'VABPKB'
037200070928     C                   ENDIF
037300071003     C* CAS
037400100922     C                   IF        %trim(%subst(vindta:379:10))<>*blanks AND
037500100922     C                             %trim(%subst(vindta:379:10))<>*zeros  AND
037600100922     C                             %trim(%subst(vindta:379:10))<>'0.00'  AND
037700100922     C                             %trim(%subst(vindta:379:10))<>'0000000.00'
037800070928     C                   MOVEL     '1'           FlgCAS
037900100922     C                   EVAL      PiStr=%trim(%subst(vindta:379:10))
038000070928     C                   EXSR      CHKNUM
038100070928     C                   IF        PiNum=*on
038200071120     C                   EVAL      VABTIC = 'BM'
038300070928     C                   Z-ADD     PiVal         VABCAS
038400070928     C                   ELSE
038500070928     C                   SETON                                        32
038600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038700070928     C                             + ' ' + 'VABCAS'
038800070928     C                   ENDIF
038900070928     C                   ENDIF
039000070730     C*
039100070730     C* Considerazioni finali su CBO/CAS
039200070730     C                   IF        FlgCAS = '1'
039300070730     C                   IF        VABCBO = '1'
039400070730     C                   EVAL      VABCBO = '4'
039500070730     C                   ENDIF
039600070730     C                   IF        VABCBO = '2'
039700070730     C                   EVAL      VABCBO = '6'
039800070730     C                   ENDIF
039900070730     C                   ENDIF
040000020305     C*
040100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040200011113     C                   EXSR      CHKIMPDIV
040300010202     C*
040400000801     C* Ebbene...
040500000801     C                   ADD       1             §CTRMO
040600070530     C                   IF        *in31 <> *zeros OR
040700070530     C                             *in32 <> *zeros
040800000801     C                   ADD       1             §CTRNO
040900020725     C                   EVAL      x_vinflg = '2'
041000000801     C                   ELSE
041100010201     C                   ADD       1             §CTROKVB
041200000801     C                   ENDIF
041300000801     C*
041400000801     C                   ENDSR
041500010201     C*----------------------------------------------------*
041600021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041700010201     C*----------------------------------------------------*
041800020305     C     WRIVAT        BEGSR
041900010201     C*
042000021113     C* Valorizzo l buffer di scrittura del FIVAT
042100070928     C* - TIPO RECORD "B"
042200100922     C                   if        %subst(vindta:234:18) <> *blanks
042300070928     C                   eval      VATTRC = 'B'
042400100922     C                   eval      VATNOT = %trim(%subst(vindta:234:18))
042500021113     C                   write     FIVAT000
042600020725     C                   endif
042700070928     C* - TIPO RECORD "E"
042800100922     C                   if        %subst(vindta:362:14) <> *blanks
042900070928     C                   eval      VATTRC = 'E'
043000100922     C                   eval      VATNOT = %trim(%subst(vindta:362:14))
043100070928     C                   write     FIVAT000
043200070928     C                   endif
043300010201     C*
043400010201     C                   ENDSR
043500020904
043600020904
043700010202     C*----------------------------------------------------*
043800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900010202     C*----------------------------------------------------*
044000020305     C     PREVAT        BEGSR
044100010202     C*
044200021113     C* Compongo il nome del membro da dare al FIVATWWR
044300010202     C                   eval      parmbr = vlrhdl
044400010202     C                   movel     'M'           parmbr
044500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
044600010202     C                   eval      paropz = '1'
044700010202     C* Effettuo la chiamata al CLLE preposto
044800050128     C                   call(e)   'TITVVTC'
044900010202     C                   parm                    parccm
045000010202     C                   parm                    parmbr
045100010202     C                   parm                    paropz
045200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300010202     C                   if        %error
045400010202     C                   movel     '1'           chkcall
045500010202     C                   else
045600010202     C                   movel     '0'           chkcall
045700010202     C                   endif
045800010202     C*
045900010202     C                   ENDSR
046000000801     C*----------------------------------------------------*
046100000801     C*  CONTROLLO NUMERICITA' CAMPI
046200000801     C*----------------------------------------------------*
046300000801     C     CHKNUM        BEGSR
046400000801     C*
046500000801     C                   call(e)   'ISNUMERIC'
046600000801     C                   PARM                    PiStr            30
046700100922     C                   PARM      '.'           PiDecChr          1
046800000801     C                   PARM      *ZEROS        PiVal            30 9
046900000801     C                   PARM      '0'           PiInt             1
047000000801     C                   PARM      '0'           PiNum             1
047100000801     C                   IF        %error
047200000801     C                   EVAL      PiInt=*off
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600000801     C***
047700000801
047800011113
047900011113     C*----------------------------------------------------*
048000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048100011113     C*----------------------------------------------------*
048200011113     C     CHKIMPDIV     BEGSR
048300011113     C*
048400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048500011113     C                   Z-ADD     *zeros        wrkDec            9 9
048600011113     C*
048700011113     C* Come prima cosa effettuo considerazioni sulla divisa
048800011113     C                   IF        vabIAS > *zeros
048900011113     C                   IF        vabVAS <> 'EUR'
049000011113     C                   EVAL      vabVAS =  'ITL'
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C                   IF        vabCAS > *zeros
049500011113     C                   IF        vabVCA <> 'EUR'
049600011113     C                   EVAL      vabVCA =  'ITL'
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C                   IF        vabVMD > *zeros
050100020305     C                   IF        vabVAD <> 'EUR'
050200011113     C                   EVAL      vabVAD =  'ITL'
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050700011113     C                   Z-ADD     vabIAS        wrkDec
050800011113     C                   IF        wrkDec > *zeros
050900011113     C                   IF        vabVAS = 'ITL'
051000011113     C                   EVAL      vabIAS = *zeros
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051500011113     C                   Z-ADD     vabCAS        wrkDec
051600011113     C                   IF        wrkDec > *zeros
051700011113     C                   IF        vabVCA = 'ITL'
051800011113     C                   EVAL      vabCAS = *zeros
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052300011113     C                   Z-ADD     vabVMD        wrkDec
052400011113     C                   IF        wrkDec > *zeros
052500011113     C                   IF        vabVAD = 'ITL'
052600011113     C                   EVAL      vabVMD = *zeros
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C                   ENDSR
053100011113     C***
053200011113
053300011113
053400000801
053500000801
053600990920      /TITLE Invio dei dati al punto operativo.
053700010202     C     invio         BEGSR
053800990920     C*
053900021113     C* 1° invio FIVAT
054000010201     C                   reset                   dscmz
054100010201     C                   move      vlrpoi        cmzdst
054200021113     C                   eval      cmzfld = 'FIVATWWR'
054300010201     C                   eval      cmzmbd = vlrhdl
054400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054500021009     C***                if        prmfir = *blanks
054600021113     C                   eval      cmzfla = 'FIVAT00F'
054700021113     C                   eval      cmzmba = 'FIVAT00F'
054800021009     C***                else
054900021009     C***                eval      cmzfla = prmfir
055000021009     C***                eval      cmzmba = prmfir
055100021009     C***                endif
055200010201     C                   eval      cmznrr = *zeros
055300020305     C                   move      §ctrokvt      cmznrr
055400021018     C                   eval      cmzlba = vlrfl1
055500010201     C                   call(e)   'TIS711C'
055600010201     C                   parm                    dscmz
055700010201     C                   parm      *blanks       esito
055800010205     C                   if        %error
055900010205     C                             or cmzerr = '1'
056000010205     C                             or esito  = '1'
056100010205     C                   eval      wrkesito = '3'
056200010205     C                   else
056300010201     C*
056400021113     C* 2° invio FIVAB
056500010201     C                   reset                   dscmz
056600010201     C                   move      vlrpoi        cmzdst
056700010201     C                   eval      cmzfld = vlrfou
056800010201     C                   eval      cmzmbd = vlrhdl
056900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057000021009     C***                if        prmfir = *blanks
057100021113     C                   eval      cmzfla = 'FIVAB00F'
057200021113     C                   eval      cmzmba = 'FIVAB00F'
057300021009     C***                else
057400021009     C***                eval      cmzfla = prmfir
057500021009     C***                eval      cmzmba = prmfir
057600021009     C***                endif
057700010201     C                   eval      cmznrr = *zeros
057800010201     C                   move      §ctrokvb      cmznrr
057900021018     C                   eval      cmzlba = vlrfl1
058000010201     C                   call(e)   'TIS711C'
058100010201     C                   parm                    dscmz
058200010201     C                   parm      *blanks       esito
058300010201     C                   if        %error
058400010201     C                             or cmzerr = '1'
058500010201     C                             or esito  = '1'
058600010201     C                   eval      wrkesito = '3'
058700010201     C                   endif
058800010205     C                   endif
058900990920     C*
059000000613     C                   ENDSR
059100000613     C***
059200070411
059300070411     C     *pssr         BEGSR
059400070411     C*
059500070411     C                   if        %open(tivin00r)
059600070411     C                   close     tivin00r
059700070411     C                   endif
059800070411     C                   if        %open(fivabwwr)
059900070411     C                   close     fivabwwr
060000070411     C                   endif
060100070411     C                   if        %open(fivatwwr)
060200070411     C                   close     fivatwwr
060300070411     C                   endif
060400070411     C*
060500070411     C* Effettuo la chiamata al CLLE preposto
060600070411     C                   call(e)   'TITVVTC'
060700070411     C                   parm                    parccm
060800070411     C                   parm                    parmbr
060900070411     C                   parm      '2'           paropz
061000070411     C*
061100070411     C                   eval      wrkesito = '2'
061200070411     C*
061300070411     C                   seton                                        LR
061400070411     C*
061500070411     C                   ENDSR     '*CANCL'
061600070411     C***
061700070411
061800990910
061900000613     C     *inzsr        BEGSR
062000990910     C*
062100990910     C     *entry        plist
062200990920     C                   parm                    tivlrds
062300990921     C                   parm      wrkesito      esito
062400000724     C                   parm                    prmlit
062500000710     C                   parm                    prmfir
062600000613     C*
062700000830     C* CALCOLA LA DATA CORRENTE
062800000830     C                   time                    wn14             14 0
062900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063000000830     C                   z-add     wn8           g08dat
063100000830     C                   z-add     *zeros        g08inv
063200000830     C                   movel     '0'           g08err
063300000830     C                   call      'XSRDA8'
063400000830     C                   parm                    wlbda8
063500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063600000830     C*
063700000613     C                   ENDSR
063800000613     C***
