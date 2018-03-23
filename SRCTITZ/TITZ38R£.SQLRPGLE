000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
002100110622     D depspe          s             10    INZ(*blanks)
002200110622     D curspe          s             10    INZ(*blanks)
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
003700110524     D  VINSPE                        8
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
004900081113
005000081113     D*------------------
005100081113     D* LINKING A DEFINIZIONI ESTERNE
005200081113     D*------------------
005300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
008600000801
008700910830     C*--------------------------------------------------------
008800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008900910830     C*--------------------------------------------------------
009000070530     C     RWFILE        BEGSR
009100990910     C*
009200990914     C                   if        not %open(tivin00r)
009300990908     C                   open      tivin00r
009400990914     C                   endif
009500021113     C                   if        not %open(fivabwwr)
009600021113     C                   open      fivabwwr
009700990914     C                   endif
009800070530     C*
009900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010000020305     C                   exsr      prevat
010100010201     C*
010200010202     C                   if        chkcall = '0'
010300010202     C*
010400021113     C                   if        not %open(fivatwwr)
010500021113     C                   open      fivatwwr
010600010201     C                   endif
010700080117     C*
010800080117     C                   EXSR      INZVAR
010900080117     C                   EXSR      DEFCAM
011000990910     C*
011100010201     C                   clear                   §CTROKVB          5 0
011200020305     C                   clear                   §CTROKVT          5 0
011300000801     C                   clear                   §CTRMO            5 0
011400000801     C                   clear                   §CTRNO            5 0
011500990910     C*
011600020725     C/EXEC SQL
011700020725     C+ declare C1 cursor for select
011800110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
011900060223     C+ from tivin00r
012000060223     C+ order by sped
012100020725     C+ for read only
012200020725     C/END-EXEC
012300020725     C
012400020725     C/EXEC SQL
012500020725     C+ open C1
012600020725     C/END-EXEC
012700020725     C
012800020725     C/EXEC SQL
012900070530     C+ Fetch C1 into :INPUT_DS
013000020725     C/END-EXEC
013100020725     C*
013200020725     C                   dow       sqlcod = *zeros
013300990913     C*
013400020725     C                   if        vindta > *blanks
013500000613     C                   add       1             rrnum
013600000801     C*
013700020725     C                   if        vinflg = *blanks
013800020725     C                             or vinflg = '0'
013900020725     C                             or vinflg = '2'
014000000801     C*
014100020725     C                   clear                   x_vinmsg
014200020725     C                   eval      x_vinflg = '1'
014300010305     C*
014400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014500110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
014600020305     C                   MOVEL(p)  PiStr         curspe
014700010305     C*
014800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
014900080923     C                   if        *in50 = *off
015000071003     C                   exsr      impvab
015100071003     C                   exsr      wrivab
015200071003     C                   exsr      wrivat_b                                     => carico VAT
015300071003     C                   else
015400080923     C*
015500071009     C                   if        wDISK = 'D'
015600071009     C                   exsr      impvab
015700071009     C                   exsr      wrivab
015800071009     C                   exsr      wrivat_b                                     => carico VAT
015900071009     C                   exsr      wrivat_e                                     => carico VAT
016000071009     C                   else
016100071009     C*
016200010305     C                   if        depspe = *blanks                             => 1° giro
016300010305     C                   eval      depspe = curspe                              => memorizz. spediz
016400080117     C                   clear                   tivinds
016500020305     C                   exsr      impvab
016600071003     C                   exsr      wrivat_b                                     => carico VAT
016700071003     C   50              exsr      wrivat_e                                     => carico VAT
016800010305     C                   else
016900020725     C                   if        depspe <> curspe                             => rottura di spediz
017000010305     C                   eval      depspe = curspe                              => memorizz. spediz
017100070928     C                   exsr      wrivab
017200080117     C                   clear                   tivinds
017300020305     C                   exsr      impvab
017400071003     C                   exsr      wrivat_b                                     => carico VAT
017500071003     C   50              exsr      wrivat_e                                     => carico VAT
017600020305     C                   else                                                   => x stessa spediz
017700090204     C***                exsr      impvab
017800071003     C                   exsr      wrivat_b                                     => carico VAT
017900071003     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   endif
018100010305     C                   endif
018200010305     C                   endif
018300071003     C                   endif
018400071009     C                   endif
018500000905     C*
018600000905     C                   else
018700020725     C                   eval      x_vinflg = '1'
018800000905     C                   endif
018900000905     C*
019000020725     C     VINRRN        chain     tivin000
019100020725     C                   if        %found(tivin00r)
019200020725     C                   eval      y_vinflg = x_vinflg
019300020725     C                   eval      y_vinmsg = x_vinmsg
019400020725     C                   update    tivin000
019500020725     C                   endif
019600020725     C*
019700020725     C/EXEC SQL
019800070530     C+ Fetch C1 into :INPUT_DS
019900020725     C/END-EXEC
020000020725     C*
020100020725     C                   enddo
020200020725     C*
020300020725     C/EXEC SQL
020400020725     C+ close C1
020500020725     C/END-EXEC
020600000905     C*
020700020305     C* Scarico i VAB rimasti "in sospeso"
020800071009     C                   if        wDISK = 'C'
020900071009     C                   exsr      wrivab
021000071009     C                   endif
021100010202     C*
021200010202     C                   endif
021300990910
021400990910     C* Se non ci sono record con errori ...
021500000710     C                   if        §ctrno = 0
021600990910     C* ... restituisco esito OK.
021700990921     C                   eval      wrkesito = '0'
021800990910     C                   else
021900010201     C                   if        §ctrokvb > 0
022000990921     C                   eval      wrkesito = '1'
022100000710     C                   else
022200000710     C                   eval      wrkesito = '2'
022300990910     C                   endif
022400000710     C                   endif
022500990910     C*
022600990914     C                   if        %open(tivin00r)
022700990908     C                   close     tivin00r
022800990914     C                   endif
022900021113     C                   if        %open(fivabwwr)
023000021113     C                   close     fivabwwr
023100990914     C                   endif
023200021113     C                   if        %open(fivatwwr)
023300021113     C                   close     fivatwwr
023400010201     C                   endif
023500990910     C*
023600010201     C                   if        §ctrokvb > 0
023700000724     C                             and vlrpoi <> *zeros
023800010202     C                   exsr      invio
023900990920     C                   endif
024000990920     C*
024100910830     C                   ENDSR
024200000613     C***
024300010305
024400010305     C*----------------------------------------------------*
024500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024600010305     C*----------------------------------------------------*
024700020305     C     WRIVAB        BEGSR
024800070730     C*
024900071003     C* Considerazioni finali
025000071003     C                   if        VABRMA = *blanks
025100071003     C                   movel(P)  VABRMN        VABRMA
025200071003     C                   endif
025300071003     C*
025400021113     C                   write     fivab000                                     => scarico il VAB
025500010305     C*
025600010305     C                   ENDSR
025700990920
025800000801     C*----------------------------------------------------*
025900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026000000801     C*----------------------------------------------------*
026100010201     C     INZVAR        BEGSR
026200000801     C*
026300010201     C                   Z-ADD     *zeros        Num5_0
026400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026500020725     C                   MOVEL     '0'           FlgCAS            1
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027000000801     C*----------------------------------------------------*
027100020904     C     DEFCAM        BEGSR
027200080923     C*
027300080923     C                   SETOFF                                       5051
027400000801     C*
027500021113     C                   CLEAR                   FIVAB000
027600021113     C                   CLEAR                   FIVAT000
027700070730     C* Imposto i valori di default...
027800120628     C                   EVAL      VABCCM = 0016105
027900120628     C                   EVAL      VATCCM = 0016105
028000120628     C                   EVAL      VABLNP = 001
028100120628     C                   EVAL      VATLNP = 001
028200120628     C                   EVAL      VABCTR = 000
028300120628     C                   EVAL      VABCTM = '7Q'
028400070730     C                   EVAL      VABCBO = '1'
028500110520     C                   EVAL      VABTSP = 'C'
028600070222     C* ... e poi verifico se sono stati passati come parametri
028700070222     C                   IF        vlrppt > *blanks
028800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
028900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029000070222     C                   EXSR      CHKNUM
029100070222     C                   IF        PiInt=*on
029200070222     C                   Z-ADD     PiVal         VABCCM
029300070222     C                   Z-ADD     PiVal         VATCCM
029400070222     C                   ENDIF
029500070222     C                   ENDIF
029600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
029700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800070222     C                   EXSR      CHKNUM
029900070222     C                   IF        PiInt=*on
030000070222     C                   Z-ADD     PiVal         VABLNP
030100070222     C                   Z-ADD     PiVal         VATLNP
030200070222     C                   ENDIF
030300070222     C                   ENDIF
030400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABCTR
030900070222     C                   ENDIF
031000070928     C                   ENDIF
031100071009     C                   MOVEL     *blanks       wDISK             1
031200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031300071009     C                   IF        wDISK <> *blanks
031400070928     C                   SETON                                        50
031500070222     C                   ENDIF
031600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
031700080923     C                   SETON                                        51
031800080923     C                   ENDIF
031900070222     C                   ENDIF
032000071009     C*
032100071009     C   50              EVAL      VABCTM = '7Q'
032200000801     C*
032300000801     C                   ENDSR
032400000801     C*----------------------------------------------------*
032500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032600000801     C*----------------------------------------------------*
032700010201     C     IMPVAB        BEGSR
032800070530     C*
032900070530     C                   SETOFF                                       3132
033000070928     C*
033100070928     C                   EXSR      INZVAR
033200070928     C                   EXSR      DEFCAM
033300070928     C*
033400070928     C                   MOVE(P)   vlrpoi        VABFGS
033500070928     C                   MOVE(P)   vlrpoi        VATFGS
033600070928     C*
033700070928     C                   MOVEL     datcor        VABAAS
033800070928     C                   MOVEL     datcor        VATAAS
033900070928     C                   MOVE      datcor        VABMGS
034000070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034300070928     C* ==
034400070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
034500070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034600070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
034700110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
034800110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
034900070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035000080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
035100070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035200070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035300070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
035400120628     C                   EVAL      VABNZD=%trim(%subst(vindta:412:3))
035500120717     C*
035600120717     C* Filiali di partenza e Codici tariffa
035700120717     C*
035800120628     C* se nazione è Italia (blank) metto valori di default
035900120628     C                   EVAL      VABLNP = 001
036000120628     C                   EVAL      VATLNP = 001
036100120628     C                   EVAL      VABFGS = 001
036200120719     C                   EVAL      VATFGS = 001
036300120719     C                   IF        VABNZD = *blank
036400120628     C                   EVAL      VABCTR = 000
036500130305     C                   IF        %trim(%subst(vindta:12:1)) = 'T'
036600130305     C                   EVAL      VABTSP='E'
036700130305     C                   ENDIF
036800130305     C                   IF        %trim(%subst(vindta:16:1)) = '1'
036900130305     C                   EVAL      VABFFD='S'
037000130305     C                   ENDIF
037100130305     C                   IF        %trim(%subst(vindta:16:1)) = '2'
037200130305     C                   EVAL      VABTC1='A'
037300130305     C                   ENDIF
037400120628     C* se nazione estera uso altri valori
037500120719     C                   ELSE
037600120719     C***                EVAL      VABLNP = 043
037700120719     C***                EVAL      VATLNP = 043
037800120719     C***                EVAL      VABFGS = 043
037900120719     C***                EVAL      VATFGS = 043
038000120628     C                   EVAL      VABCTR = 152
038100120719     C                   EVAL      VABPRD = *blanks
038200120628     C                   ENDIF
038300100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
038400100324     C***                EVAL      VABCBO='1'
038500100324     C***                ENDIF
038600100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
038700100324     C***                EVAL      VABCBO='2'
038800100324     C***                ENDIF
038900100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
039000100324     C***                EVAL      VABTSP='E'
039100100324     C***                ENDIF
039200100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
039300100324     C***                EVAL      VABFFD='S'
039400100324     C***                ENDIF
039500100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
039600100324     C***                EVAL      VABTC1='A'
039700100324     C***                ENDIF
039800071003     C* CAD
039900120716     C* siccome le spedizioni riguardano l'estero, considero il CAP non un numerico ma una stringa
040000120716     C* di 5
040100120716     C                   EVAL      VABCAD=%subst(vindta:107:5)
040200120716     C***                EVAL      PiStr=%trim(%subst(vindta:107:5))
040300120716     C***                EXSR      CHKNUM
040400120716     C***                IF        PiInt=*on
040500120716     C***                Z-ADD     PiVal         Num5_0
040600120716     C***                MOVEL     Num5_0        VABCAD
040700120716     C***                ELSE
040800120716     C***                SETON                                        32
040900120716     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
041000120716     C***                          + ' ' + 'VABCAD'
041100120716     C***                ENDIF
041200080923     C* RMN
041300110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041400110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
041500110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
041600101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
041700130109     C     '/':'0'       XLATE     PiStr         PiStr
041800080923     C                   EXSR      CHKNUM
041900080923     C                   IF        PiInt=*on
042000080923     C                   Z-ADD     PiVal         VABRMN
042100080923     C                   ELSE
042200080923     C                   SETON                                        32
042300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400080923     C                             + ' ' + 'VABRMN'
042500080923     C                   ENDIF
042600080923     C* NSP
042700080923     C                   IF        *in51 = *on
042800080923     C* NSP => Stacco un numeratore da AZNUM
042900080923     C                   clear                   TRUL33DS
043000080923     C                   eval      I33OPE = *zeros
043100080923     C                   eval      I33CNU = 302
043200080923     C                   eval      I33NUM = 1
043300080923     C                   movel     TRUL33DS      KPJBU
043400080923     C                   call      'TRUL33R'
043500080923     C                   parm                    KPJBA
043600080923     C                   movel     KPJBU         TRUL33DS
043700080923     C                   if        O33ERR = *zeros
043800080923     C                   z-add     O33NRF        VABNSP
043900080923     C                   z-add     O33NRF        VATNSP
044000080923     C                   else
044100080923     C                   SETON                                        31
044200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044300080923     C                             + ' ' + 'VABNSP VATNSP'
044400080923     C                   endif
044500080923     C                   ELSE
044600110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
044700110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
044800110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
044900080923     C                   EXSR      CHKNUM
045000080923     C                   IF        PiInt=*on
045100080923     C                   Z-ADD     PiVal         VABNSP
045200080923     C                   Z-ADD     PiVal         VATNSP
045300080923     C                   ELSE
045400080923     C                   SETON                                        31
045500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600080923     C                             + ' ' + 'VABNSP VATNSP'
045700080923     C                   ENDIF
045800080923     C                   ENDIF
045900071003     C* NCL
046000070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
046100070928     C                   EXSR      CHKNUM
046200070928     C                   IF        PiInt=*on
046300070928     C                   Z-ADD     PiVal         VABNCL
046400070928     C                   ELSE
046500070928     C                   SETON                                        32
046600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700070928     C                             + ' ' + 'VABNCL'
046800070928     C                   ENDIF
046900071003     C* PKB
047000110520     C                   IF        %subst(vindta:170:8) <> *blanks
047100070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
047200070928     C                   EXSR      CHKNUM
047300070928     C                   IF        PiNum=*on
047400070928     C                   Z-ADD     PiVal         VABPKB
047500070928     C                   ELSE
047600070928     C                   SETON                                        32
047700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047800070928     C                             + ' ' + 'VABPKB'
047900070928     C                   ENDIF
048000110520     C                   ELSE
048100110520     C                   IF        VABCTR = 100
048200110520     C                   EVAL      VABPKB = 18 * VABNCL
048300110520     C                   ELSE
048400110520     C                   EVAL      VABPKB = 10 * VABNCL
048500110520     C                   ENDIF
048600110520     C                   ENDIF
048700080923     C* CAS
048800080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
048900080923     C                             AND
049000080923     C                             %trim(%subst(vindta:280:12))<>*zeros
049100080923     C                             AND
049200080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
049300080923     C                             AND
049400080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
049500080923     C                             AND
049600080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
049700070928     C                   MOVEL     '1'           FlgCAS
049800070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
049900070928     C                   EXSR      CHKNUM
050000070928     C                   IF        PiNum=*on
050100070928     C                   Z-ADD     PiVal         VABCAS
050200070928     C                   ELSE
050300070928     C                   SETON                                        32
050400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050500070928     C                             + ' ' + 'VABCAS'
050600070928     C                   ENDIF
050700070928     C                   ENDIF
050800071003     C* IAS
050900080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
051000080923     C                             AND
051100080923     C                             %trim(%subst(vindta:295:12))<>*zeros
051200080923     C                             AND
051300080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
051400080923     C                             AND
051500080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
051600080923     C                             AND
051700080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
051800080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
051900080923     C                   EXSR      CHKNUM
052000080923     C                   IF        PiNum=*on
052100080923     C                   Z-ADD     PiVal         VABIAS
052200080923     C                   ELSE
052300080923     C                   SETON                                        32
052400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500080923     C                             + ' ' + 'VABIAS'
052600080923     C                   ENDIF
052700071003     C                   ENDIF
052800120719     C*
052900120719     C* Verifica condizioni particolari sulla nazione di destinazione
053000120719     C*
053100120719     C                   IF        VABNZD = 'P'
053200120719     C                   EVAL      VABRSD = 'NACEX P/C PRINK'
053300120719     C                   EVAL      VABRD2 = 'RECOGERAN SEUR INTERNATIONAL'
053400120719     C                   EVAL      VABIND = 'CAL FERNANDO 18'
053500120719     C                   EVAL      VABCAD = '08820'
053600120719     C                   EVAL      VABLOD = 'EL PRAT DE LLOBREGAT'
053700120719     C                   EVAL      VABNZD = 'E'
053800120719     C                   ENDIF
053900070730     C*
054000070730     C* Considerazioni finali su CBO/CAS
054100070730     C                   IF        FlgCAS = '1'
054200070730     C                   IF        VABCBO = '1'
054300070730     C                   EVAL      VABCBO = '4'
054400070730     C                   ENDIF
054500070730     C                   IF        VABCBO = '2'
054600070730     C                   EVAL      VABCBO = '6'
054700070730     C                   ENDIF
054800070730     C                   ENDIF
054900020305     C*
055000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055100011113     C                   EXSR      CHKIMPDIV
055200010202     C*
055300000801     C* Ebbene...
055400000801     C                   ADD       1             §CTRMO
055500070530     C                   IF        *in31 <> *zeros OR
055600070530     C                             *in32 <> *zeros
055700000801     C                   ADD       1             §CTRNO
055800020725     C                   EVAL      x_vinflg = '2'
055900000801     C                   ELSE
056000010201     C                   ADD       1             §CTROKVB
056100000801     C                   ENDIF
056200000801     C*
056300000801     C                   ENDSR
056400010201     C*----------------------------------------------------*
056500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056600010201     C*----------------------------------------------------*
056700071003     C     WRIVAT_B      BEGSR
056800090204     C*
056900090204     C                   eval      vatAAS = vabAAS
057000090204     C                   eval      vatLNP = vabLNP
057100090204     C                   eval      vatNRS = vabNRS
057200090204     C                   eval      vatNSP = vabNSP
057300010201     C*
057400101007     C* Valorizzo il buffer di scrittura del FIVAT
057500070928     C* - TIPO RECORD "B"
057600070928     C                   if        %subst(vindta:92:15) <> *blanks
057700070928     C                   eval      VATTRC = 'B'
057800070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
057900021113     C                   write     FIVAT000
058000020725     C                   endif
058100010201     C*
058200010201     C                   ENDSR
058300071003     C*----------------------------------------------------*
058400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058500071003     C*----------------------------------------------------*
058600071003     C     WRIVAT_E      BEGSR
058700090204     C*
058800090204     C                   eval      vatAAS = vabAAS
058900090204     C                   eval      vatLNP = vabLNP
059000090204     C                   eval      vatNRS = vabNRS
059100090204     C                   eval      vatNSP = vabNSP
059200071003     C*
059300101007     C* Valorizzo il buffer di scrittura del FIVAT
059400071003     C* - TIPO RECORD "E"
059500071003     C                   if        %subst(vindta:331:26) <> *blanks
059600071003     C                   eval      VATTRC = 'E'
059700071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
059800071003     C                   write     FIVAT000
059900071003     C                   endif
060000071003     C*
060100071003     C                   ENDSR
060200020904
060300020904
060400010202     C*----------------------------------------------------*
060500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060600010202     C*----------------------------------------------------*
060700020305     C     PREVAT        BEGSR
060800010202     C*
060900021113     C* Compongo il nome del membro da dare al FIVATWWR
061000010202     C                   eval      parmbr = vlrhdl
061100010202     C                   movel     'M'           parmbr
061200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061300010202     C                   eval      paropz = '1'
061400010202     C* Effettuo la chiamata al CLLE preposto
061500050128     C                   call(e)   'TITVVTC'
061600010202     C                   parm                    parccm
061700010202     C                   parm                    parmbr
061800010202     C                   parm                    paropz
061900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062000010202     C                   if        %error
062100010202     C                   movel     '1'           chkcall
062200010202     C                   else
062300010202     C                   movel     '0'           chkcall
062400010202     C                   endif
062500010202     C*
062600010202     C                   ENDSR
062700000801     C*----------------------------------------------------*
062800000801     C*  CONTROLLO NUMERICITA' CAMPI
062900000801     C*----------------------------------------------------*
063000000801     C     CHKNUM        BEGSR
063100081113     C*
063200081113     C                   IF        PiDecChr = *blanks
063300110524     C                   EVAL      PiDecChr = '.'
063400081113     C                   ENDIF
063500081113     C*
063600081113     C                   callp(e)  UBISNUM_Check(PiStr
063700081113     C                                          :PiDecChr
063800081113     C                                          :PiVal
063900081113     C                                          :PiNum
064000081113     C                                          :PiInt)
064100081113     C*
064200000801     C                   IF        %error
064300000801     C                   EVAL      PiInt=*off
064400000801     C                   ENDIF
064500000801     C*
064600000801     C                   ENDSR
064700000801     C***
064800000801
064900011113
065000011113     C*----------------------------------------------------*
065100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065200011113     C*----------------------------------------------------*
065300011113     C     CHKIMPDIV     BEGSR
065400011113     C*
065500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065600011113     C                   Z-ADD     *zeros        wrkDec            9 9
065700011113     C*
065800011113     C* Come prima cosa effettuo considerazioni sulla divisa
065900011113     C                   IF        vabIAS > *zeros
066000011113     C                   IF        vabVAS <> 'EUR'
066100011113     C                   EVAL      vabVAS =  'ITL'
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C                   IF        vabCAS > *zeros
066600011113     C                   IF        vabVCA <> 'EUR'
066700011113     C                   EVAL      vabVCA =  'ITL'
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C                   IF        vabVMD > *zeros
067200020305     C                   IF        vabVAD <> 'EUR'
067300011113     C                   EVAL      vabVAD =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067800011113     C                   Z-ADD     vabIAS        wrkDec
067900011113     C                   IF        wrkDec > *zeros
068000011113     C                   IF        vabVAS = 'ITL'
068100011113     C                   EVAL      vabIAS = *zeros
068200011113     C                   ENDIF
068300011113     C                   ENDIF
068400011113     C*
068500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068600011113     C                   Z-ADD     vabCAS        wrkDec
068700011113     C                   IF        wrkDec > *zeros
068800011113     C                   IF        vabVCA = 'ITL'
068900011113     C                   EVAL      vabCAS = *zeros
069000011113     C                   ENDIF
069100011113     C                   ENDIF
069200011113     C*
069300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069400011113     C                   Z-ADD     vabVMD        wrkDec
069500011113     C                   IF        wrkDec > *zeros
069600011113     C                   IF        vabVAD = 'ITL'
069700011113     C                   EVAL      vabVMD = *zeros
069800011113     C                   ENDIF
069900011113     C                   ENDIF
070000011113     C*
070100011113     C                   ENDSR
070200011113     C***
070300011113
070400011113
070500000801
070600000801
070700990920      /TITLE Invio dei dati al punto operativo.
070800010202     C     invio         BEGSR
070900990920     C*
071000021113     C* 1° invio FIVAT
071100010201     C                   reset                   dscmz
071200010201     C                   move      vlrpoi        cmzdst
071300021113     C                   eval      cmzfld = 'FIVATWWR'
071400010201     C                   eval      cmzmbd = vlrhdl
071500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071600021009     C***                if        prmfir = *blanks
071700021113     C                   eval      cmzfla = 'FIVAT00F'
071800021113     C                   eval      cmzmba = 'FIVAT00F'
071900021009     C***                else
072000021009     C***                eval      cmzfla = prmfir
072100021009     C***                eval      cmzmba = prmfir
072200021009     C***                endif
072300010201     C                   eval      cmznrr = *zeros
072400020305     C                   move      §ctrokvt      cmznrr
072500021018     C                   eval      cmzlba = vlrfl1
072600010201     C                   call(e)   'TIS711C'
072700010201     C                   parm                    dscmz
072800010201     C                   parm      *blanks       esito
072900010205     C                   if        %error
073000010205     C                             or cmzerr = '1'
073100010205     C                             or esito  = '1'
073200010205     C                   eval      wrkesito = '3'
073300010205     C                   else
073400010201     C*
073500021113     C* 2° invio FIVAB
073600010201     C                   reset                   dscmz
073700010201     C                   move      vlrpoi        cmzdst
073800010201     C                   eval      cmzfld = vlrfou
073900010201     C                   eval      cmzmbd = vlrhdl
074000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074100021009     C***                if        prmfir = *blanks
074200021113     C                   eval      cmzfla = 'FIVAB00F'
074300021113     C                   eval      cmzmba = 'FIVAB00F'
074400021009     C***                else
074500021009     C***                eval      cmzfla = prmfir
074600021009     C***                eval      cmzmba = prmfir
074700021009     C***                endif
074800010201     C                   eval      cmznrr = *zeros
074900010201     C                   move      §ctrokvb      cmznrr
075000021018     C                   eval      cmzlba = vlrfl1
075100010201     C                   call(e)   'TIS711C'
075200010201     C                   parm                    dscmz
075300010201     C                   parm      *blanks       esito
075400010201     C                   if        %error
075500010201     C                             or cmzerr = '1'
075600010201     C                             or esito  = '1'
075700010201     C                   eval      wrkesito = '3'
075800010201     C                   endif
075900010205     C                   endif
076000990920     C*
076100000613     C                   ENDSR
076200000613     C***
076300070411
076400070411     C     *pssr         BEGSR
076500070411     C*
076600070411     C                   if        %open(tivin00r)
076700070411     C                   close     tivin00r
076800070411     C                   endif
076900070411     C                   if        %open(fivabwwr)
077000070411     C                   close     fivabwwr
077100070411     C                   endif
077200070411     C                   if        %open(fivatwwr)
077300070411     C                   close     fivatwwr
077400070411     C                   endif
077500070411     C*
077600070411     C* Effettuo la chiamata al CLLE preposto
077700070411     C                   call(e)   'TITVVTC'
077800070411     C                   parm                    parccm
077900070411     C                   parm                    parmbr
078000070411     C                   parm      '2'           paropz
078100070411     C*
078200070411     C                   eval      wrkesito = '2'
078300070411     C*
078400070411     C                   seton                                        LR
078500070411     C*
078600070411     C                   ENDSR     '*CANCL'
078700070411     C***
078800070411
078900990910
079000000613     C     *inzsr        BEGSR
079100990910     C*
079200990910     C     *entry        plist
079300990920     C                   parm                    tivlrds
079400990921     C                   parm      wrkesito      esito
079500000724     C                   parm                    prmlit
079600000710     C                   parm                    prmfir
079700000613     C*
079800000830     C* CALCOLA LA DATA CORRENTE
079900100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080000100324     C                   eval      datcor = %dec(%date() : *ISO)
080100000830     C*
080200000613     C                   ENDSR
080300000613     C***
