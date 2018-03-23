000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130115     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
011900140307     C+ vindta, vinflg, substr(vindta, 164, 8) as sped, rrn(tivin00r)
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
014600140307     C                   EVAL      curspe=%trim(%subst(vindta:164:8))
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
028700100309     C                   EVAL      VABCCM = 1520005
028800100309     C                   EVAL      VATCCM = 1520005
028900100309     C                   EVAL      VABLNP = 152
029000100309     C                   EVAL      VATLNP = 152
029100100331     C                   EVAL      VABCTR = 500
029200070730     C                   EVAL      VABCBO = '1'
029300100309     C                   EVAL      VABVCA = 'EUR'
029400070222     C* ... e poi verifico se sono stati passati come parametri
029500070222     C                   IF        vlrppt > *blanks
029600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029800070222     C                   EXSR      CHKNUM
029900070222     C                   IF        PiInt=*on
030000070222     C                   Z-ADD     PiVal         VABCCM
030100070222     C                   Z-ADD     PiVal         VATCCM
030200070222     C                   ENDIF
030300070222     C                   ENDIF
030400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABLNP
030900070222     C                   Z-ADD     PiVal         VATLNP
031000070222     C                   ENDIF
031100070222     C                   ENDIF
031200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABCTR
031700070222     C                   ENDIF
031800070928     C                   ENDIF
031900071009     C                   MOVEL     *blanks       wDISK             1
032000071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032100071009     C                   IF        wDISK <> *blanks
032200070928     C                   SETON                                        50
032300070222     C                   ENDIF
032400070222     C                   ENDIF
032500071009     C*
032600071009     C   50              EVAL      VABCTM = '7Q'
032700000801     C*
032800000801     C                   ENDSR
032900091223     C*----------------------------------------------------*
033000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
033100091223     C*----------------------------------------------------*
033200091223     C     REPNSP        BEGSR
033300091223     C*
033400091223     C                   EXSR      INZVAR
033500091223     C                   EXSR      DEFCAM
033600091223     C*
033700091223     C* NSP => Stacco un numeratore da AZNUM
033800091223     C                   clear                   TRUL33DS
033900091223     C                   eval      I33OPE = *zeros
034000091223     C                   eval      I33CNU = 302
034100091223     C                   eval      I33NUM = 1
034200091223     C                   movel     TRUL33DS      KPJBU
034300091223     C                   call      'TRUL33R'
034400091223     C                   parm                    KPJBA
034500091223     C                   movel     KPJBU         TRUL33DS
034600091223     C                   if        O33ERR = *zeros
034700091223     C                   z-add     O33NRF        VABNSP
034800091223     C                   z-add     O33NRF        VATNSP
034900091223     C                   else
035000091223     C                   SETON                                        31
035100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035200091223     C                             + ' ' + 'VABNSP VATNSP'
035300091223     C                   endif
035400091223     C*
035500091223     C                   ENDSR
035600000801     C*----------------------------------------------------*
035700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035800000801     C*----------------------------------------------------*
035900010201     C     IMPVAB        BEGSR
036000070530     C*
036100070530     C                   SETOFF                                       3132
036200070928     C*
036300070928     C                   MOVE(P)   vlrpoi        VABFGS
036400070928     C                   MOVE(P)   vlrpoi        VATFGS
036500070928     C*
036600070928     C                   MOVEL     datcor        VABAAS
036700070928     C                   MOVEL     datcor        VATAAS
036800070928     C                   MOVE      datcor        VABMGS
036900100309     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
037000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
037200070928     C* ==
037300100309     C                   EVAL      VABRD2=%trim(%subst(vindta:36:35))
037400100309     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
037500100309     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
037600100309     C                   EVAL      VABPRD=%trim(%subst(vindta:150:2))
037700140307     C                   EVAL      VABRMA=%trim(%subst(vindta:164:8))
037800100309     C* CBO
037900100309     C                   SELECT
038000140307     C                   WHEN      %trim(%subst(vindta:172:50))
038100100309     C                             = 'FRANCO'
038200100309     C                   EVAL      VABCBO='1'
038300140307     C                   WHEN      %trim(%subst(vindta:172:50))
038400100309     C                             = 'FRANCO ADD.FATT.'
038500100309     C                   EVAL      VABCBO='1'
038600140307     C                   WHEN      %trim(%subst(vindta:172:50))
038700100309     C                             = 'ASSEGNATO'
038800100309     C                   EVAL      VABCBO='2'
038900100309     C                   ENDSL
039000100309     C* TIC
039100100309     C                   SELECT
039200140307     C                   WHEN      %trim(%subst(vindta:272:50))
039300100309     C                             = 'Assegno c.c. intestato al mittente'
039400100429     C                   EVAL      VABTIC='BM'
039500140307     C                   WHEN      %trim(%subst(vindta:272:50))
039600100309     C                             = 'Contanti'
039700100309     C                   EVAL      VABTIC=*blanks
039800140307     C                   WHEN      %trim(%subst(vindta:272:50))
039900100309     C                             = 'Assegno Circolare'
040000100429     C                   EVAL      VABTIC='CM'
040100161102     C                   WHEN      %trim(%subst(vindta:272:50))
040200161102     C                             = 'TM'
040300161102     C                   EVAL      VABTIC='TM'
040400100429     C                   OTHER
040500100429     C                   EVAL      VABTIC=*blanks
040600100309     C                   ENDSL
040700100309     C* FFD
040800100309     C                   SELECT
040900140307     C                   WHEN      %trim(%subst(vindta:322:50))
041000100309     C                             = 'SI'
041100100309     C                   EVAL      VABFFD='S'
041200100309     C                   ENDSL
041300100309     C* TSP
041400100309     C                   SELECT
041500140307     C                   WHEN      %trim(%subst(vindta:372:50))
041600100309     C                             = 'SI'
041700100309     C                   EVAL      VABTSP='E'
041800100309     C                   ENDSL
041900100622     C* TC1
042000100622     C                   SELECT
042100140307     C                   WHEN      %trim(%subst(vindta:622:1))
042200100622     C                             = 'A'
042300100622     C                   EVAL      VABTC1='A'
042400100622     C                   ENDSL
042500100309     C* NOT/NT2
042600100309     C                   MOVEL     *blanks       wNOTE            70
042700140307     C                   EVAL      wNOTE=%trim(%subst(vindta:422:200))
042800100309     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
042900100309     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
043000100309     C* NCL
043100100309     C                   EVAL      PiStr=%trim(%subst(vindta:152:5))
043200100309     C                   EXSR      CHKNUM
043300100309     C                   IF        PiInt=*on
043400100309     C                   Z-ADD     PiVal         VABNCL
043500100309     C                   ELSE
043600100309     C                   SETON                                        32
043700100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043800100309     C                             + ' ' + 'VABNCL'
043900100309     C                   ENDIF
044000100309     C* PKB
044100100309     C                   EVAL      PiStr=%trim(%subst(vindta:157:7))
044200100309     C                   EXSR      CHKNUM
044300100309     C                   IF        PiNum=*on
044400100309     C                   Z-ADD     PiVal         VABPKB
044500100309     C                   ELSE
044600100309     C                   SETON                                        32
044700100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044800100309     C                             + ' ' + 'VABPKB'
044900100309     C                   ENDIF
045000100309     C* CAS
045100140307     C                   IF        %subst(vindta:222:50) <> *blanks AND
045200140307     C                             %subst(vindta:222:50) <> *zeros
045300100309     C                   MOVEL     '1'           FlgCAS
045400140307     C                   EVAL      PiStr=%trim(%subst(vindta:222:50))
045500100309     C                   EXSR      CHKNUM
045600100309     C                   IF        PiNum=*on
045700100309     C                   Z-ADD     PiVal         VABCAS
045800100309     C                   ELSE
045900100309     C                   SETON                                        32
046000100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100100309     C                             + ' ' + 'VABCAS'
046200100309     C                   ENDIF
046300100309     C                   ENDIF
046400170316     C* IAS
046500170316     C                   IF        %subst(vindta:628:50) <> *blanks AND
046600170316     C                             %subst(vindta:628:50) <> *zeros
046700170316     C                   EVAL      PiStr=%trim(%subst(vindta:628:50))
046800170316     C                   EXSR      CHKNUM
046900170316     C                   IF        PiNum=*on
047000170316     C                   Z-ADD     PiVal         VABIAS
047100170316     C                   EVAL      VABVAS = 'EUR'
047200170316     C                   ELSE
047300170316     C                   SETON                                        32
047400170316     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500170316     C                             + ' ' + 'VABIAS'
047600170316     C                   ENDIF
047700170316     C                   ENDIF
047800071003     C* CAD
047900100309     C                   EVAL      PiStr=%trim(%subst(vindta:106:9))
048000070928     C                   EXSR      CHKNUM
048100070928     C                   IF        PiInt=*on
048200070928     C                   Z-ADD     PiVal         Num5_0
048300070928     C                   MOVEL     Num5_0        VABCAD
048400070928     C                   ELSE
048500070928     C                   SETON                                        32
048600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700070928     C                             + ' ' + 'VABCAD'
048800070928     C                   ENDIF
048900080923     C* RMN
049000140307     C                   EVAL      PiStr=%trim(%subst(vindta:164:8))
049100080923     C                   EXSR      CHKNUM
049200080923     C                   IF        PiInt=*on
049300080923     C                   Z-ADD     PiVal         VABRMN
049400080923     C                   ELSE
049500080923     C                   SETON                                        32
049600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700080923     C                             + ' ' + 'VABRMN'
049800080923     C                   ENDIF
049900100622     C* QFT
050000140307     C                   EVAL      PiStr=%trim(%subst(vindta:623:5))
050100100622     C                   EXSR      CHKNUM
050200100622     C                   IF        PiNum=*on
050300100622     C                   Z-ADD     PiVal         VABQFT
050400100622     C                   ELSE
050500100622     C                   SETON                                        32
050600100622     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700100622     C                             + ' ' + 'VABQFT'
050800100622     C                   ENDIF
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
051900020305     C*
052000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052100011113     C                   EXSR      CHKIMPDIV
052200010202     C*
052300000801     C* Ebbene...
052400000801     C                   ADD       1             §CTRMO
052500070530     C                   IF        *in31 <> *zeros OR
052600070530     C                             *in32 <> *zeros
052700000801     C                   ADD       1             §CTRNO
052800020725     C                   EVAL      x_vinflg = '2'
052900000801     C                   ELSE
053000010201     C                   ADD       1             §CTROKVB
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400100127     C*----------------------------------------------------*
053500100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
053600100127     C*----------------------------------------------------*
053700100127     C     WRIVAT_A      BEGSR
053800100127     C*
053900100127     C* Valorizzo l buffer di scrittura del FIVAT
054000100127     C* - TIPO RECORD "A"
054100100127     C***                eval      VATTRC = 'A'
054200100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
054300100127     C***                exsr      wrivat
054400100127     C*
054500100127     C                   ENDSR
054600010201     C*----------------------------------------------------*
054700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054800010201     C*----------------------------------------------------*
054900071003     C     WRIVAT_B      BEGSR
055000010201     C*
055100021113     C* Valorizzo l buffer di scrittura del FIVAT
055200070928     C* - TIPO RECORD "B"
055300100127     C***                eval      VATTRC = 'B'
055400100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
055500100127     C***                exsr      wrivat
055600010201     C*
055700010201     C                   ENDSR
055800071003     C*----------------------------------------------------*
055900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
056000071003     C*----------------------------------------------------*
056100071003     C     WRIVAT_E      BEGSR
056200071003     C*
056300071003     C* Valorizzo l buffer di scrittura del FIVAT
056400071003     C* - TIPO RECORD "E"
056500100309     C***                eval      VATTRC = 'E'
056600100309     C***                eval      VATNOT = %trim(%subst(vindta:17:12))
056700100309     C***                exsr      wrivat
056800071003     C*
056900071003     C                   ENDSR
057000100127     C*----------------------------------------------------*
057100100127     C*  SCARICO BUFFER FIVAT
057200100127     C*----------------------------------------------------*
057300100127     C     WRIVAT        BEGSR
057400100127     C*
057500100127     C                   if        vatNOT <> *blanks
057600100127     C                   ADD       1             §CTROKVT
057700100127     C                   write     FIVAT000
057800100127     C                   endif
057900100127     C*
058000100127     C                   ENDSR
058100020904
058200020904
058300010202     C*----------------------------------------------------*
058400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058500010202     C*----------------------------------------------------*
058600020305     C     PREVAT        BEGSR
058700010202     C*
058800021113     C* Compongo il nome del membro da dare al FIVATWWR
058900010202     C                   eval      parmbr = vlrhdl
059000010202     C                   movel     'M'           parmbr
059100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
059200010202     C                   eval      paropz = '1'
059300010202     C* Effettuo la chiamata al CLLE preposto
059400050128     C                   call(e)   'TITVVTC'
059500010202     C                   parm                    parccm
059600010202     C                   parm                    parmbr
059700010202     C                   parm                    paropz
059800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059900010202     C                   if        %error
060000010202     C                   movel     '1'           chkcall
060100010202     C                   else
060200010202     C                   movel     '0'           chkcall
060300010202     C                   endif
060400010202     C*
060500010202     C                   ENDSR
060600000801     C*----------------------------------------------------*
060700000801     C*  CONTROLLO NUMERICITA' CAMPI
060800000801     C*----------------------------------------------------*
060900000801     C     CHKNUM        BEGSR
061000081113     C*
061100081113     C                   IF        PiDecChr = *blanks
061200091223     C                   EVAL      PiDecChr = ','
061300081113     C                   ENDIF
061400091223     C*
061500081113     C                   callp(e)  UBISNUM_Check(PiStr
061600081113     C                                          :PiDecChr
061700081113     C                                          :PiVal
061800081113     C                                          :PiNum
061900081113     C                                          :PiInt)
062000081113     C*
062100000801     C                   IF        %error
062200000801     C                   EVAL      PiInt=*off
062300000801     C                   ENDIF
062400000801     C*
062500000801     C                   ENDSR
062600000801     C***
062700000801
062800011113
062900011113     C*----------------------------------------------------*
063000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063100011113     C*----------------------------------------------------*
063200011113     C     CHKIMPDIV     BEGSR
063300011113     C*
063400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063500011113     C                   Z-ADD     *zeros        wrkDec            9 9
063600011113     C*
063700011113     C* Come prima cosa effettuo considerazioni sulla divisa
063800011113     C                   IF        vabIAS > *zeros
063900011113     C                   IF        vabVAS <> 'EUR'
064000011113     C                   EVAL      vabVAS =  'ITL'
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C                   IF        vabCAS > *zeros
064500011113     C                   IF        vabVCA <> 'EUR'
064600011113     C                   EVAL      vabVCA =  'ITL'
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C                   IF        vabVMD > *zeros
065100020305     C                   IF        vabVAD <> 'EUR'
065200011113     C                   EVAL      vabVAD =  'ITL'
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065700011113     C                   Z-ADD     vabIAS        wrkDec
065800011113     C                   IF        wrkDec > *zeros
065900011113     C                   IF        vabVAS = 'ITL'
066000011113     C                   EVAL      vabIAS = *zeros
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066500011113     C                   Z-ADD     vabCAS        wrkDec
066600011113     C                   IF        wrkDec > *zeros
066700011113     C                   IF        vabVCA = 'ITL'
066800011113     C                   EVAL      vabCAS = *zeros
066900011113     C                   ENDIF
067000011113     C                   ENDIF
067100011113     C*
067200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067300011113     C                   Z-ADD     vabVMD        wrkDec
067400011113     C                   IF        wrkDec > *zeros
067500011113     C                   IF        vabVAD = 'ITL'
067600011113     C                   EVAL      vabVMD = *zeros
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C                   ENDSR
068100011113     C***
068200011113
068300011113
068400000801
068500000801
068600990920      /TITLE Invio dei dati al punto operativo.
068700010202     C     invio         BEGSR
068800990920     C*
068900021113     C* 1° invio FIVAT
069000010201     C                   reset                   dscmz
069100010201     C                   move      vlrpoi        cmzdst
069200021113     C                   eval      cmzfld = 'FIVATWWR'
069300010201     C                   eval      cmzmbd = vlrhdl
069400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069500021009     C***                if        prmfir = *blanks
069600021113     C                   eval      cmzfla = 'FIVAT00F'
069700021113     C                   eval      cmzmba = 'FIVAT00F'
069800021009     C***                else
069900021009     C***                eval      cmzfla = prmfir
070000021009     C***                eval      cmzmba = prmfir
070100021009     C***                endif
070200010201     C                   eval      cmznrr = *zeros
070300020305     C                   move      §ctrokvt      cmznrr
070400021018     C                   eval      cmzlba = vlrfl1
070500010201     C                   call(e)   'TIS711C'
070600010201     C                   parm                    dscmz
070700010201     C                   parm      *blanks       esito
070800010205     C                   if        %error
070900010205     C                             or cmzerr = '1'
071000010205     C                             or esito  = '1'
071100010205     C                   eval      wrkesito = '3'
071200010205     C                   else
071300010201     C*
071400021113     C* 2° invio FIVAB
071500010201     C                   reset                   dscmz
071600010201     C                   move      vlrpoi        cmzdst
071700010201     C                   eval      cmzfld = vlrfou
071800010201     C                   eval      cmzmbd = vlrhdl
071900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000021009     C***                if        prmfir = *blanks
072100021113     C                   eval      cmzfla = 'FIVAB00F'
072200021113     C                   eval      cmzmba = 'FIVAB00F'
072300021009     C***                else
072400021009     C***                eval      cmzfla = prmfir
072500021009     C***                eval      cmzmba = prmfir
072600021009     C***                endif
072700010201     C                   eval      cmznrr = *zeros
072800010201     C                   move      §ctrokvb      cmznrr
072900021018     C                   eval      cmzlba = vlrfl1
073000010201     C                   call(e)   'TIS711C'
073100010201     C                   parm                    dscmz
073200010201     C                   parm      *blanks       esito
073300010201     C                   if        %error
073400010201     C                             or cmzerr = '1'
073500010201     C                             or esito  = '1'
073600010201     C                   eval      wrkesito = '3'
073700010201     C                   endif
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
074800070411     C                   if        %open(fivabwwr)
074900070411     C                   close     fivabwwr
075000070411     C                   endif
075100070411     C                   if        %open(fivatwwr)
075200070411     C                   close     fivatwwr
075300070411     C                   endif
075400070411     C*
075500070411     C* Effettuo la chiamata al CLLE preposto
075600070411     C                   call(e)   'TITVVTC'
075700070411     C                   parm                    parccm
075800070411     C                   parm                    parmbr
075900070411     C                   parm      '2'           paropz
076000070411     C*
076100070411     C                   eval      wrkesito = '2'
076200070411     C*
076300070411     C                   seton                                        LR
076400070411     C*
076500070411     C                   ENDSR     '*CANCL'
076600070411     C***
076700070411
076800990910
076900000613     C     *inzsr        BEGSR
077000990910     C*
077100990910     C     *entry        plist
077200990920     C                   parm                    tivlrds
077300990921     C                   parm      wrkesito      esito
077400000724     C                   parm                    prmlit
077500000710     C                   parm                    prmfir
077600000613     C*
077700000830     C* CALCOLA LA DATA CORRENTE
077800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077900091223     C                   eval      datcor = %dec(%date() : *ISO)
078000000830     C*
078100000613     C                   ENDSR
078200000613     C***
