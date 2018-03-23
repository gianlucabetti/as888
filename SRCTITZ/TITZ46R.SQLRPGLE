000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120906      *
000300120906      * PARTICOLARITA':
000400120906      * Il tracciato è del tutto simile a quello TNT ma non vengono scritti i "CHI SONO" per cui
000500120906      * non viene dichiarato tale, anche se sull'anagrafica si fa conto sia un disk C.
000600120906      * Quindi la routine wrivat_e viene asteriscata e VABCTM viene lasciato vuoto.
000700120906      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900990908
001000020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001100021113     FFIVABwwr  O    E             DISK    usropn
001200021113     FFIVATwwr  O    E             DISK    usropn
001300990908
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100070730     D tisi95ds      e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700110622     D depspe          s             10    INZ(*blanks)
002800110622     D curspe          s             10    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500000830
003600020725
003700020725     D*------------------
003800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003900020725     D*------------------
004000070530     D INPUT_DS        DS                  INZ
004100101007     D  VINDTA                     2048
004200101007     D  VINFLG                        1
004300110524     D  VINSPE                        8
004400101007     D  VINRRN                        8  0
004500020725     D*
004600080923     D*------------------
004700080923     D* DS REPERIMENTO NUMERATORE
004800080923     D*------------------
004900080923     D trul33ds      e ds                  inz
005000080923     D*------------------
005100080923     D* DS ARCHITETTURA
005200080923     D*------------------
005300080923     D kpjba         e ds                  inz
005400080923     D*------------------
005500081113
005600081113     D*------------------
005700081113     D* LINKING A DEFINIZIONI ESTERNE
005800081113     D*------------------
005900100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006000081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006100081113
006200990908
006300010201
006400010201
006500080117     C*
006600080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006700080117     C
006800080117     C/EXEC SQL
006900080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007000080117     C/END-EXEC
007100080117     C
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500000613     C*
007600070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007700000613     C*
007800010202     C* Effettuo la chiamata al CLLE preposto
007900050128     C                   call(e)   'TITVVTC'
008000010202     C                   parm                    parccm
008100010202     C                   parm                    parmbr
008200010202     C                   parm      '2'           paropz
008300070730     C*
008400070730     C* Effettuo lancio TISI95 solo x chiusura
008500070730     C                   CLEAR                   TISI95DS
008600070730     C                   EVAL      I95TLA = 'C'
008700070730     C                   CALL      'TISI95R'
008800070730     C                   PARM                    TISI95DS
008900000616     C*
009000000801     C
009100010201     C                   seton                                        LR
009200000801
009300910830     C*--------------------------------------------------------
009400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009500910830     C*--------------------------------------------------------
009600070530     C     RWFILE        BEGSR
009700990910     C*
009800990914     C                   if        not %open(tivin00r)
009900990908     C                   open      tivin00r
010000990914     C                   endif
010100021113     C                   if        not %open(fivabwwr)
010200021113     C                   open      fivabwwr
010300990914     C                   endif
010400070530     C*
010500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010600020305     C                   exsr      prevat
010700010201     C*
010800010202     C                   if        chkcall = '0'
010900010202     C*
011000021113     C                   if        not %open(fivatwwr)
011100021113     C                   open      fivatwwr
011200010201     C                   endif
011300080117     C*
011400080117     C                   EXSR      INZVAR
011500080117     C                   EXSR      DEFCAM
011600990910     C*
011700010201     C                   clear                   §CTROKVB          5 0
011800020305     C                   clear                   §CTROKVT          5 0
011900000801     C                   clear                   §CTRMO            5 0
012000000801     C                   clear                   §CTRNO            5 0
012100990910     C*
012200020725     C/EXEC SQL
012300020725     C+ declare C1 cursor for select
012400110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
012500060223     C+ from tivin00r
012600060223     C+ order by sped
012700020725     C+ for read only
012800020725     C/END-EXEC
012900020725     C
013000020725     C/EXEC SQL
013100020725     C+ open C1
013200020725     C/END-EXEC
013300020725     C
013400020725     C/EXEC SQL
013500070530     C+ Fetch C1 into :INPUT_DS
013600020725     C/END-EXEC
013700020725     C*
013800020725     C                   dow       sqlcod = *zeros
013900990913     C*
014000020725     C                   if        vindta > *blanks
014100000613     C                   add       1             rrnum
014200000801     C*
014300020725     C                   if        vinflg = *blanks
014400020725     C                             or vinflg = '0'
014500020725     C                             or vinflg = '2'
014600000801     C*
014700020725     C                   clear                   x_vinmsg
014800020725     C                   eval      x_vinflg = '1'
014900010305     C*
015000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015100110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015200020305     C                   MOVEL(p)  PiStr         curspe
015300010305     C*
015400071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015500080923     C                   if        *in50 = *off
015600071003     C                   exsr      impvab
015700071003     C                   exsr      wrivab
015800071003     C                   exsr      wrivat_b                                     => carico VAT
015900071003     C                   else
016000080923     C*
016100071009     C                   if        wDISK = 'D'
016200071009     C                   exsr      impvab
016300071009     C                   exsr      wrivab
016400071009     C                   exsr      wrivat_b                                     => carico VAT
016500120906     C***                exsr      wrivat_e                                     => carico VAT
016600071009     C                   else
016700071009     C*
016800010305     C                   if        depspe = *blanks                             => 1° giro
016900010305     C                   eval      depspe = curspe                              => memorizz. spediz
017000080117     C                   clear                   tivinds
017100020305     C                   exsr      impvab
017200071003     C                   exsr      wrivat_b                                     => carico VAT
017300120906     C***50              exsr      wrivat_e                                     => carico VAT
017400010305     C                   else
017500020725     C                   if        depspe <> curspe                             => rottura di spediz
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700070928     C                   exsr      wrivab
017800080117     C                   clear                   tivinds
017900020305     C                   exsr      impvab
018000071003     C                   exsr      wrivat_b                                     => carico VAT
018100120906     C***50              exsr      wrivat_e                                     => carico VAT
018200020305     C                   else                                                   => x stessa spediz
018300090204     C***                exsr      impvab
018400071003     C                   exsr      wrivat_b                                     => carico VAT
018500120906     C***50              exsr      wrivat_e                                     => carico VAT
018600010305     C                   endif
018700010305     C                   endif
018800010305     C                   endif
018900071003     C                   endif
019000071009     C                   endif
019100000905     C*
019200000905     C                   else
019300020725     C                   eval      x_vinflg = '1'
019400000905     C                   endif
019500000905     C*
019600020725     C     VINRRN        chain     tivin000
019700020725     C                   if        %found(tivin00r)
019800020725     C                   eval      y_vinflg = x_vinflg
019900020725     C                   eval      y_vinmsg = x_vinmsg
020000020725     C                   update    tivin000
020100020725     C                   endif
020200020725     C*
020300020725     C/EXEC SQL
020400070530     C+ Fetch C1 into :INPUT_DS
020500020725     C/END-EXEC
020600020725     C*
020700020725     C                   enddo
020800020725     C*
020900020725     C/EXEC SQL
021000020725     C+ close C1
021100020725     C/END-EXEC
021200000905     C*
021300020305     C* Scarico i VAB rimasti "in sospeso"
021400071009     C                   if        wDISK = 'C'
021500071009     C                   exsr      wrivab
021600071009     C                   endif
021700010202     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500021113     C                   if        %open(fivabwwr)
023600021113     C                   close     fivabwwr
023700990914     C                   endif
023800021113     C                   if        %open(fivatwwr)
023900021113     C                   close     fivatwwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900010305
025000010305     C*----------------------------------------------------*
025100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025200010305     C*----------------------------------------------------*
025300020305     C     WRIVAB        BEGSR
025400070730     C*
025500071003     C* Considerazioni finali
025600071003     C                   if        VABRMA = *blanks
025700071003     C                   movel(P)  VABRMN        VABRMA
025800071003     C                   endif
025900071003     C*
026000021113     C                   write     fivab000                                     => scarico il VAB
026100010305     C*
026200010305     C                   ENDSR
026300990920
026400000801     C*----------------------------------------------------*
026500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600000801     C*----------------------------------------------------*
026700010201     C     INZVAR        BEGSR
026800000801     C*
026900010201     C                   Z-ADD     *zeros        Num5_0
027000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027100020725     C                   MOVEL     '0'           FlgCAS            1
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700020904     C     DEFCAM        BEGSR
027800080923     C*
027900080923     C                   SETOFF                                       5051
028000000801     C*
028100021113     C                   CLEAR                   FIVAB000
028200021113     C                   CLEAR                   FIVAT000
028300070730     C* Imposto i valori di default...
028400120906     C                   EVAL      VABCCM = 0410586
028500120906     C                   EVAL      VATCCM = 0410586
028600120906     C                   EVAL      VABLNP = 041
028700120906     C                   EVAL      VATLNP = 041
028800120628     C                   EVAL      VABCTR = 000
028900120906     C***                EVAL      VABCTM = '7Q'
029000070730     C                   EVAL      VABCBO = '1'
029100110520     C                   EVAL      VABTSP = 'C'
029200070222     C* ... e poi verifico se sono stati passati come parametri
029300070222     C                   IF        vlrppt > *blanks
029400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABCCM
029900070222     C                   Z-ADD     PiVal         VATCCM
030000070222     C                   ENDIF
030100070222     C                   ENDIF
030200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABLNP
030700070222     C                   Z-ADD     PiVal         VATLNP
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABCTR
031500070222     C                   ENDIF
031600070928     C                   ENDIF
031700071009     C                   MOVEL     *blanks       wDISK             1
031800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031900071009     C                   IF        wDISK <> *blanks
032000070928     C                   SETON                                        50
032100070222     C                   ENDIF
032200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032300080923     C                   SETON                                        51
032400080923     C                   ENDIF
032500070222     C                   ENDIF
032600071009     C*
032700120906     C***50              EVAL      VABCTM = '7Q'
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200000801     C*----------------------------------------------------*
033300010201     C     IMPVAB        BEGSR
033400070530     C*
033500070530     C                   SETOFF                                       3132
033600070928     C*
033700070928     C                   EXSR      INZVAR
033800070928     C                   EXSR      DEFCAM
033900070928     C*
034000070928     C                   MOVE(P)   vlrpoi        VABFGS
034100070928     C                   MOVE(P)   vlrpoi        VATFGS
034200070928     C*
034300070928     C                   MOVEL     datcor        VABAAS
034400070928     C                   MOVEL     datcor        VATAAS
034500070928     C                   MOVE      datcor        VABMGS
034600070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034900070928     C* ==
035000070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035100070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035200070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035300110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035400110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
035500070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035600080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
035700070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035800070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035900070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
036000120628     C                   EVAL      VABNZD=%trim(%subst(vindta:412:3))
036100120717     C*
036200120717     C* Filiali di partenza e Codici tariffa
036300120717     C*
036400120628     C* se nazione è Italia (blank) metto valori di default
036500130108     C                   EVAL      VABLNP = 041
036600130108     C                   EVAL      VATLNP = 041
036700130108     C                   EVAL      VABFGS = 041
036800130108     C                   EVAL      VATFGS = 041
036900120719     C                   IF        VABNZD = *blank
037000120628     C                   EVAL      VABCTR = 000
037100120628     C* se nazione estera uso altri valori
037200120719     C                   ELSE
037300120719     C***                EVAL      VABLNP = 043
037400120719     C***                EVAL      VATLNP = 043
037500120719     C***                EVAL      VABFGS = 043
037600120719     C***                EVAL      VATFGS = 043
037700120628     C                   EVAL      VABCTR = 152
037800120719     C                   EVAL      VABPRD = *blanks
037900120628     C                   ENDIF
038000100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
038100100324     C***                EVAL      VABCBO='1'
038200100324     C***                ENDIF
038300100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
038400100324     C***                EVAL      VABCBO='2'
038500100324     C***                ENDIF
038600100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
038700100324     C***                EVAL      VABTSP='E'
038800100324     C***                ENDIF
038900100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
039000100324     C***                EVAL      VABFFD='S'
039100100324     C***                ENDIF
039200100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
039300100324     C***                EVAL      VABTC1='A'
039400100324     C***                ENDIF
039500071003     C* CAD
039600120716     C* siccome le spedizioni riguardano l'estero, considero il CAP non un numerico ma una stringa
039700120716     C* di 5
039800120716     C                   EVAL      VABCAD=%subst(vindta:107:5)
039900120716     C***                EVAL      PiStr=%trim(%subst(vindta:107:5))
040000120716     C***                EXSR      CHKNUM
040100120716     C***                IF        PiInt=*on
040200120716     C***                Z-ADD     PiVal         Num5_0
040300120716     C***                MOVEL     Num5_0        VABCAD
040400120716     C***                ELSE
040500120716     C***                SETON                                        32
040600120716     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
040700120716     C***                          + ' ' + 'VABCAD'
040800120716     C***                ENDIF
040900080923     C* RMN
041000110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041100110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
041200110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
041300101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
041400080923     C                   EXSR      CHKNUM
041500080923     C                   IF        PiInt=*on
041600080923     C                   Z-ADD     PiVal         VABRMN
041700080923     C                   ELSE
041800080923     C                   SETON                                        32
041900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000080923     C                             + ' ' + 'VABRMN'
042100080923     C                   ENDIF
042200080923     C* NSP
042300080923     C                   IF        *in51 = *on
042400080923     C* NSP => Stacco un numeratore da AZNUM
042500080923     C                   clear                   TRUL33DS
042600080923     C                   eval      I33OPE = *zeros
042700080923     C                   eval      I33CNU = 302
042800080923     C                   eval      I33NUM = 1
042900080923     C                   movel     TRUL33DS      KPJBU
043000080923     C                   call      'TRUL33R'
043100080923     C                   parm                    KPJBA
043200080923     C                   movel     KPJBU         TRUL33DS
043300080923     C                   if        O33ERR = *zeros
043400080923     C                   z-add     O33NRF        VABNSP
043500080923     C                   z-add     O33NRF        VATNSP
043600080923     C                   else
043700080923     C                   SETON                                        31
043800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043900080923     C                             + ' ' + 'VABNSP VATNSP'
044000080923     C                   endif
044100080923     C                   ELSE
044200110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
044300110622     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
044400110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
044500080923     C                   EXSR      CHKNUM
044600080923     C                   IF        PiInt=*on
044700080923     C                   Z-ADD     PiVal         VABNSP
044800080923     C                   Z-ADD     PiVal         VATNSP
044900080923     C                   ELSE
045000080923     C                   SETON                                        31
045100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045200080923     C                             + ' ' + 'VABNSP VATNSP'
045300080923     C                   ENDIF
045400080923     C                   ENDIF
045500071003     C* NCL
045600070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
045700070928     C                   EXSR      CHKNUM
045800070928     C                   IF        PiInt=*on
045900070928     C                   Z-ADD     PiVal         VABNCL
046000070928     C                   ELSE
046100070928     C                   SETON                                        32
046200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300070928     C                             + ' ' + 'VABNCL'
046400070928     C                   ENDIF
046500071003     C* PKB
046600110520     C                   IF        %subst(vindta:170:8) <> *blanks
046700070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
046800070928     C                   EXSR      CHKNUM
046900070928     C                   IF        PiNum=*on
047000070928     C                   Z-ADD     PiVal         VABPKB
047100070928     C                   ELSE
047200070928     C                   SETON                                        32
047300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400070928     C                             + ' ' + 'VABPKB'
047500070928     C                   ENDIF
047600110520     C                   ELSE
047700110520     C                   IF        VABCTR = 100
047800110520     C                   EVAL      VABPKB = 18 * VABNCL
047900110520     C                   ELSE
048000110520     C                   EVAL      VABPKB = 10 * VABNCL
048100110520     C                   ENDIF
048200110520     C                   ENDIF
048300080923     C* CAS
048400080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
048500080923     C                             AND
048600080923     C                             %trim(%subst(vindta:280:12))<>*zeros
048700080923     C                             AND
048800080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
048900080923     C                             AND
049000080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
049100080923     C                             AND
049200080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
049300070928     C                   MOVEL     '1'           FlgCAS
049400070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
049500070928     C                   EXSR      CHKNUM
049600070928     C                   IF        PiNum=*on
049700070928     C                   Z-ADD     PiVal         VABCAS
049800070928     C                   ELSE
049900070928     C                   SETON                                        32
050000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050100070928     C                             + ' ' + 'VABCAS'
050200070928     C                   ENDIF
050300070928     C                   ENDIF
050400071003     C* IAS
050500080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
050600080923     C                             AND
050700080923     C                             %trim(%subst(vindta:295:12))<>*zeros
050800080923     C                             AND
050900080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
051000080923     C                             AND
051100080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
051200080923     C                             AND
051300080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
051400080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
051500080923     C                   EXSR      CHKNUM
051600080923     C                   IF        PiNum=*on
051700080923     C                   Z-ADD     PiVal         VABIAS
051800080923     C                   ELSE
051900080923     C                   SETON                                        32
052000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052100080923     C                             + ' ' + 'VABIAS'
052200080923     C                   ENDIF
052300071003     C                   ENDIF
052400120719     C*
052500120719     C* Verifica condizioni particolari sulla nazione di destinazione
052600120719     C*
052700120906     C*  nessuno
052800070730     C*
052900070730     C* Considerazioni finali su CBO/CAS
053000070730     C                   IF        FlgCAS = '1'
053100070730     C                   IF        VABCBO = '1'
053200070730     C                   EVAL      VABCBO = '4'
053300070730     C                   ENDIF
053400070730     C                   IF        VABCBO = '2'
053500070730     C                   EVAL      VABCBO = '6'
053600070730     C                   ENDIF
053700070730     C                   ENDIF
053800020305     C*
053900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054000011113     C                   EXSR      CHKIMPDIV
054100010202     C*
054200000801     C* Ebbene...
054300000801     C                   ADD       1             §CTRMO
054400070530     C                   IF        *in31 <> *zeros OR
054500070530     C                             *in32 <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700020725     C                   EVAL      x_vinflg = '2'
054800000801     C                   ELSE
054900010201     C                   ADD       1             §CTROKVB
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300010201     C*----------------------------------------------------*
055400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
055500010201     C*----------------------------------------------------*
055600071003     C     WRIVAT_B      BEGSR
055700090204     C*
055800090204     C                   eval      vatAAS = vabAAS
055900090204     C                   eval      vatLNP = vabLNP
056000090204     C                   eval      vatNRS = vabNRS
056100090204     C                   eval      vatNSP = vabNSP
056200010201     C*
056300101007     C* Valorizzo il buffer di scrittura del FIVAT
056400070928     C* - TIPO RECORD "B"
056500070928     C                   if        %subst(vindta:92:15) <> *blanks
056600070928     C                   eval      VATTRC = 'B'
056700070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
056800021113     C                   write     FIVAT000
056900020725     C                   endif
057000010201     C*
057100010201     C                   ENDSR
057200071003     C*----------------------------------------------------*
057300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057400071003     C*----------------------------------------------------*
057500071003     C     WRIVAT_E      BEGSR
057600090204     C*
057700090204     C                   eval      vatAAS = vabAAS
057800090204     C                   eval      vatLNP = vabLNP
057900090204     C                   eval      vatNRS = vabNRS
058000090204     C                   eval      vatNSP = vabNSP
058100071003     C*
058200101007     C* Valorizzo il buffer di scrittura del FIVAT
058300071003     C* - TIPO RECORD "E"
058400071003     C                   if        %subst(vindta:331:26) <> *blanks
058500071003     C                   eval      VATTRC = 'E'
058600071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
058700071003     C                   write     FIVAT000
058800071003     C                   endif
058900071003     C*
059000071003     C                   ENDSR
059100020904
059200020904
059300010202     C*----------------------------------------------------*
059400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
059500010202     C*----------------------------------------------------*
059600020305     C     PREVAT        BEGSR
059700010202     C*
059800021113     C* Compongo il nome del membro da dare al FIVATWWR
059900010202     C                   eval      parmbr = vlrhdl
060000010202     C                   movel     'M'           parmbr
060100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
060200010202     C                   eval      paropz = '1'
060300010202     C* Effettuo la chiamata al CLLE preposto
060400050128     C                   call(e)   'TITVVTC'
060500010202     C                   parm                    parccm
060600010202     C                   parm                    parmbr
060700010202     C                   parm                    paropz
060800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060900010202     C                   if        %error
061000010202     C                   movel     '1'           chkcall
061100010202     C                   else
061200010202     C                   movel     '0'           chkcall
061300010202     C                   endif
061400010202     C*
061500010202     C                   ENDSR
061600000801     C*----------------------------------------------------*
061700000801     C*  CONTROLLO NUMERICITA' CAMPI
061800000801     C*----------------------------------------------------*
061900000801     C     CHKNUM        BEGSR
062000081113     C*
062100081113     C                   IF        PiDecChr = *blanks
062200110524     C                   EVAL      PiDecChr = '.'
062300081113     C                   ENDIF
062400081113     C*
062500081113     C                   callp(e)  UBISNUM_Check(PiStr
062600081113     C                                          :PiDecChr
062700081113     C                                          :PiVal
062800081113     C                                          :PiNum
062900081113     C                                          :PiInt)
063000081113     C*
063100000801     C                   IF        %error
063200000801     C                   EVAL      PiInt=*off
063300000801     C                   ENDIF
063400000801     C*
063500000801     C                   ENDSR
063600000801     C***
063700000801
063800011113
063900011113     C*----------------------------------------------------*
064000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064100011113     C*----------------------------------------------------*
064200011113     C     CHKIMPDIV     BEGSR
064300011113     C*
064400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
064500011113     C                   Z-ADD     *zeros        wrkDec            9 9
064600011113     C*
064700011113     C* Come prima cosa effettuo considerazioni sulla divisa
064800011113     C                   IF        vabIAS > *zeros
064900011113     C                   IF        vabVAS <> 'EUR'
065000011113     C                   EVAL      vabVAS =  'ITL'
065100011113     C                   ENDIF
065200011113     C                   ENDIF
065300011113     C*
065400011113     C                   IF        vabCAS > *zeros
065500011113     C                   IF        vabVCA <> 'EUR'
065600011113     C                   EVAL      vabVCA =  'ITL'
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C                   IF        vabVMD > *zeros
066100020305     C                   IF        vabVAD <> 'EUR'
066200011113     C                   EVAL      vabVAD =  'ITL'
066300011113     C                   ENDIF
066400011113     C                   ENDIF
066500011113     C*
066600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066700011113     C                   Z-ADD     vabIAS        wrkDec
066800011113     C                   IF        wrkDec > *zeros
066900011113     C                   IF        vabVAS = 'ITL'
067000011113     C                   EVAL      vabIAS = *zeros
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
067500011113     C                   Z-ADD     vabCAS        wrkDec
067600011113     C                   IF        wrkDec > *zeros
067700011113     C                   IF        vabVCA = 'ITL'
067800011113     C                   EVAL      vabCAS = *zeros
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068300011113     C                   Z-ADD     vabVMD        wrkDec
068400011113     C                   IF        wrkDec > *zeros
068500011113     C                   IF        vabVAD = 'ITL'
068600011113     C                   EVAL      vabVMD = *zeros
068700011113     C                   ENDIF
068800011113     C                   ENDIF
068900011113     C*
069000011113     C                   ENDSR
069100011113     C***
069200011113
069300011113
069400000801
069500000801
069600990920      /TITLE Invio dei dati al punto operativo.
069700010202     C     invio         BEGSR
069800990920     C*
069900021113     C* 1° invio FIVAT
070000010201     C                   reset                   dscmz
070100010201     C                   move      vlrpoi        cmzdst
070200021113     C                   eval      cmzfld = 'FIVATWWR'
070300010201     C                   eval      cmzmbd = vlrhdl
070400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070500021009     C***                if        prmfir = *blanks
070600021113     C                   eval      cmzfla = 'FIVAT00F'
070700021113     C                   eval      cmzmba = 'FIVAT00F'
070800021009     C***                else
070900021009     C***                eval      cmzfla = prmfir
071000021009     C***                eval      cmzmba = prmfir
071100021009     C***                endif
071200010201     C                   eval      cmznrr = *zeros
071300020305     C                   move      §ctrokvt      cmznrr
071400021018     C                   eval      cmzlba = vlrfl1
071500010201     C                   call(e)   'TIS711C'
071600010201     C                   parm                    dscmz
071700010201     C                   parm      *blanks       esito
071800010205     C                   if        %error
071900010205     C                             or cmzerr = '1'
072000010205     C                             or esito  = '1'
072100010205     C                   eval      wrkesito = '3'
072200010205     C                   else
072300010201     C*
072400021113     C* 2° invio FIVAB
072500010201     C                   reset                   dscmz
072600010201     C                   move      vlrpoi        cmzdst
072700010201     C                   eval      cmzfld = vlrfou
072800010201     C                   eval      cmzmbd = vlrhdl
072900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073000021009     C***                if        prmfir = *blanks
073100021113     C                   eval      cmzfla = 'FIVAB00F'
073200021113     C                   eval      cmzmba = 'FIVAB00F'
073300021009     C***                else
073400021009     C***                eval      cmzfla = prmfir
073500021009     C***                eval      cmzmba = prmfir
073600021009     C***                endif
073700010201     C                   eval      cmznrr = *zeros
073800010201     C                   move      §ctrokvb      cmznrr
073900021018     C                   eval      cmzlba = vlrfl1
074000010201     C                   call(e)   'TIS711C'
074100010201     C                   parm                    dscmz
074200010201     C                   parm      *blanks       esito
074300010201     C                   if        %error
074400010201     C                             or cmzerr = '1'
074500010201     C                             or esito  = '1'
074600010201     C                   eval      wrkesito = '3'
074700010201     C                   endif
074800010205     C                   endif
074900990920     C*
075000000613     C                   ENDSR
075100000613     C***
075200070411
075300070411     C     *pssr         BEGSR
075400070411     C*
075500070411     C                   if        %open(tivin00r)
075600070411     C                   close     tivin00r
075700070411     C                   endif
075800070411     C                   if        %open(fivabwwr)
075900070411     C                   close     fivabwwr
076000070411     C                   endif
076100070411     C                   if        %open(fivatwwr)
076200070411     C                   close     fivatwwr
076300070411     C                   endif
076400070411     C*
076500070411     C* Effettuo la chiamata al CLLE preposto
076600070411     C                   call(e)   'TITVVTC'
076700070411     C                   parm                    parccm
076800070411     C                   parm                    parmbr
076900070411     C                   parm      '2'           paropz
077000070411     C*
077100070411     C                   eval      wrkesito = '2'
077200070411     C*
077300070411     C                   seton                                        LR
077400070411     C*
077500070411     C                   ENDSR     '*CANCL'
077600070411     C***
077700070411
077800990910
077900000613     C     *inzsr        BEGSR
078000990910     C*
078100990910     C     *entry        plist
078200990920     C                   parm                    tivlrds
078300990921     C                   parm      wrkesito      esito
078400000724     C                   parm                    prmlit
078500000710     C                   parm                    prmfir
078600000613     C*
078700000830     C* CALCOLA LA DATA CORRENTE
078800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078900100324     C                   eval      datcor = %dec(%date() : *ISO)
079000000830     C*
079100000613     C                   ENDSR
079200000613     C***
