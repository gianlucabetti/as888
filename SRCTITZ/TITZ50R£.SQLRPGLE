000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200160211      *
000300121001      * PARTICOLARITA':
000400121011      * Anche in caso di spedizione in contrassegno lasciare il campo VABCBO = 1 per spedizione.
000500140306      * Gli indirizzi email possono essere più d'uno divisi da ";"
000600140306      * si prende solo il primo
000700121001      *
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
002700121011     D depspe          s              6    INZ(*blanks)
002800121011     D curspe          s              6    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500121011     D w70             s             70
003600140306     D wPos            s              3  0
003700151014     D wColloPrg       s              3  0
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600121011     D  VINSPE                        6
004700101007     D  VINRRN                        8  0
004800020725     D*
004900080923     D*------------------
005000080923     D* DS REPERIMENTO NUMERATORE
005100080923     D*------------------
005200080923     D trul33ds      e ds                  inz
005300080923     D*------------------
005400080923     D* DS ARCHITETTURA
005500080923     D*------------------
005600080923     D kpjba         e ds                  inz
005700080923     D*------------------
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400081113
006500990908
006600010201
006700010201
006800080117     C*
006900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000080117     C
007100080117     C/EXEC SQL
007200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300080117     C/END-EXEC
007400080117     C
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000613     C*
007900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008000000613     C*
008100010202     C* Effettuo la chiamata al CLLE preposto
008200050128     C                   call(e)   'TITVVTC'
008300010202     C                   parm                    parccm
008400010202     C                   parm                    parmbr
008500010202     C                   parm      '2'           paropz
008600070730     C*
008700070730     C* Effettuo lancio TISI95 solo x chiusura
008800070730     C                   CLEAR                   TISI95DS
008900070730     C                   EVAL      I95TLA = 'C'
009000070730     C                   CALL      'TISI95R'
009100070730     C                   PARM                    TISI95DS
009200000616     C*
009300000801     C
009400010201     C                   seton                                        LR
009500000801
009600910830     C*--------------------------------------------------------
009700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009800910830     C*--------------------------------------------------------
009900070530     C     RWFILE        BEGSR
010000990910     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400021113     C                   if        not %open(fivabwwr)
010500021113     C                   open      fivabwwr
010600990914     C                   endif
010700070530     C*
010800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010900020305     C                   exsr      prevat
011000010201     C*
011100010202     C                   if        chkcall = '0'
011200010202     C*
011300021113     C                   if        not %open(fivatwwr)
011400021113     C                   open      fivatwwr
011500010201     C                   endif
011600080117     C*
011700080117     C                   EXSR      INZVAR
011800080117     C                   EXSR      DEFCAM
011900990910     C*
012000010201     C                   clear                   §CTROKVB          5 0
012100020305     C                   clear                   §CTROKVT          5 0
012200000801     C                   clear                   §CTRMO            5 0
012300000801     C                   clear                   §CTRNO            5 0
012400990910     C*
012500020725     C/EXEC SQL
012600020725     C+ declare C1 cursor for select
012700121011     C+ vindta, vinflg, substr(vindta, 3, 6) as sped, rrn(tivin00r)
012800060223     C+ from tivin00r
012900060223     C+ order by sped
013000020725     C+ for read only
013100020725     C/END-EXEC
013200020725     C
013300020725     C/EXEC SQL
013400020725     C+ open C1
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800070530     C+ Fetch C1 into :INPUT_DS
013900020725     C/END-EXEC
014000020725     C*
014100020725     C                   dow       sqlcod = *zeros
014200990913     C*
014300020725     C                   if        vindta > *blanks
014400000613     C                   add       1             rrnum
014500000801     C*
014600020725     C                   if        vinflg = *blanks
014700020725     C                             or vinflg = '0'
014800020725     C                             or vinflg = '2'
014900000801     C*
015000020725     C                   clear                   x_vinmsg
015100020725     C                   eval      x_vinflg = '1'
015200010305     C*
015300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015400121011     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
015500020305     C                   MOVEL(p)  PiStr         curspe
015600010305     C*
015700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015800080923     C                   if        *in50 = *off
015900071003     C                   exsr      impvab
016000071003     C                   exsr      wrivab
016100121122     C                   exsr      wrivat_b                                     => carico VAT
016200140304     C                   exsr      wrivat_i                                     => carico VAT
016300071003     C                   else
016400080923     C*
016500071009     C                   if        wDISK = 'D'
016600071009     C                   exsr      impvab
016700071009     C                   exsr      wrivab
016800121122     C                   exsr      wrivat_b                                     => carico VAT
016900140304     C                   exsr      wrivat_i                                     => carico VAT
017000151014     C                   exsr      wrivat_e                                     => carico VAT
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600020305     C                   exsr      impvab
017700121122     C                   exsr      wrivat_b                                     => carico VAT
017800140304     C                   exsr      wrivat_i                                     => carico VAT
017900151014     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300070928     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500020305     C                   exsr      impvab
018600121122     C                   exsr      wrivat_b                                     => carico VAT
018700140304     C                   exsr      wrivat_i                                     => carico VAT
018800151014     C   50              exsr      wrivat_e                                     => carico VAT
018900020305     C                   else                                                   => x stessa spediz
019000090204     C***                exsr      impvab
019100121122     C***                exsr      wrivat_b                                     => carico VAT
019200151014     C   50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   endif
019400010305     C                   endif
019500010305     C                   endif
019600071003     C                   endif
019700071009     C                   endif
019800000905     C*
019900000905     C                   else
020000020725     C                   eval      x_vinflg = '1'
020100000905     C                   endif
020200000905     C*
020300020725     C     VINRRN        chain     tivin000
020400020725     C                   if        %found(tivin00r)
020500020725     C                   eval      y_vinflg = x_vinflg
020600020725     C                   eval      y_vinmsg = x_vinmsg
020700020725     C                   update    tivin000
020800020725     C                   endif
020900020725     C*
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   enddo
021500020725     C*
021600020725     C/EXEC SQL
021700020725     C+ close C1
021800020725     C/END-EXEC
021900000905     C*
022000020305     C* Scarico i VAB rimasti "in sospeso"
022100071009     C                   if        wDISK = 'C'
022200071009     C                   exsr      wrivab
022300071009     C                   endif
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021113     C                   if        %open(fivabwwr)
024300021113     C                   close     fivabwwr
024400990914     C                   endif
024500021113     C                   if        %open(fivatwwr)
024600021113     C                   close     fivatwwr
024700010201     C                   endif
024800990910     C*
024900010201     C                   if        §ctrokvb > 0
025000000724     C                             and vlrpoi <> *zeros
025100010202     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600010305
025700010305     C*----------------------------------------------------*
025800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025900010305     C*----------------------------------------------------*
026000020305     C     WRIVAB        BEGSR
026100070730     C*
026200071003     C* Considerazioni finali
026300071003     C                   if        VABRMA = *blanks
026400071003     C                   movel(P)  VABRMN        VABRMA
026500071003     C                   endif
026600071003     C*
026700021113     C                   write     fivab000                                     => scarico il VAB
026800010305     C*
026900010305     C                   ENDSR
027000990920
027100000801     C*----------------------------------------------------*
027200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027300000801     C*----------------------------------------------------*
027400010201     C     INZVAR        BEGSR
027500000801     C*
027600010201     C                   Z-ADD     *zeros        Num5_0
027700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027800020725     C                   MOVEL     '0'           FlgCAS            1
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028300000801     C*----------------------------------------------------*
028400020904     C     DEFCAM        BEGSR
028500080923     C*
028600080923     C                   SETOFF                                       5051
028700000801     C*
028800021113     C                   CLEAR                   FIVAB000
028900021113     C                   CLEAR                   FIVAT000
029000070730     C* Imposto i valori di default...
029100121011     C                   EVAL      VABCCM = 0321551
029200121011     C                   EVAL      VATCCM = 0321551
029300121011     C                   EVAL      VABLNP = 032
029400121011     C                   EVAL      VATLNP = 032
029500120628     C                   EVAL      VABCTR = 000
029600121011     C                   EVAL      VABTSP = 'C'
029700121011     C                   EVAL      VABCBO = '1'
029800070222     C* ... e poi verifico se sono stati passati come parametri
029900070222     C                   IF        vlrppt > *blanks
030000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030200070222     C                   EXSR      CHKNUM
030300070222     C                   IF        PiInt=*on
030400070222     C                   Z-ADD     PiVal         VABCCM
030500070222     C                   Z-ADD     PiVal         VATCCM
030600070222     C                   ENDIF
030700070222     C                   ENDIF
030800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031000070222     C                   EXSR      CHKNUM
031100070222     C                   IF        PiInt=*on
031200070222     C                   Z-ADD     PiVal         VABLNP
031300070222     C                   Z-ADD     PiVal         VATLNP
031400070222     C                   ENDIF
031500070222     C                   ENDIF
031600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031800070222     C                   EXSR      CHKNUM
031900070222     C                   IF        PiInt=*on
032000070222     C                   Z-ADD     PiVal         VABCTR
032100070222     C                   ENDIF
032200070928     C                   ENDIF
032300071009     C                   MOVEL     *blanks       wDISK             1
032400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032500071009     C                   IF        wDISK <> *blanks
032600070928     C                   SETON                                        50
032700070222     C                   ENDIF
032800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032900080923     C                   SETON                                        51
033000080923     C                   ENDIF
033100070222     C                   ENDIF
033200071009     C*
033300071009     C   50              EVAL      VABCTM = '7Q'
033400000801     C*
033500000801     C                   ENDSR
033600000801     C*----------------------------------------------------*
033700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800000801     C*----------------------------------------------------*
033900010201     C     IMPVAB        BEGSR
034000070530     C*
034100070530     C                   SETOFF                                       3132
034200070928     C*
034300070928     C                   EXSR      INZVAR
034400070928     C                   EXSR      DEFCAM
034500070928     C*
034600070928     C                   MOVE(P)   vlrpoi        VABFGS
034700070928     C                   MOVE(P)   vlrpoi        VATFGS
034800070928     C*
034900070928     C                   MOVEL     datcor        VABAAS
035000070928     C                   MOVEL     datcor        VATAAS
035100070928     C                   MOVE      datcor        VABMGS
035200121011     C                   EVAL      VABRSD=%trim(%subst(vindta:191:40))
035300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035500070928     C* ==
035600121011     C                   EVAL      VABIND=%trim(%subst(vindta:231:35))
035700121011     C                   EVAL      VABLOD=%trim(%subst(vindta:266:30))
035800121011     C                   EVAL      VABPRD=%trim(%subst(vindta:296:2))
035900121011     C                   EVAL      VABRMA=%trim(%subst(vindta:330:6))
036000160211     C                                    + '-' +
036100160211     C                                    %trim(%subst(vindta:3:6))
036200121122     C                   EVAL      VABNOT=%trim(%subst(vindta:436:30))
036300121122     C                   EVAL      VABNT2='AL '+%trim(%subst(vindta:498:02))+
036400121122     C                                     '/'+ %trim(%subst(vindta:496:02))+
036500121122     C                                     '/'+ %trim(%subst(vindta:492:04))+
036600121122     C                                      ' '+%trim(%subst(vindta:396:40))
036700120717     C*
036800071003     C* CAD
036900121011     C                   EVAL      PiStr=%trim(%subst(vindta:298:5))
037000121001     C                   EXSR      CHKNUM
037100121001     C                   IF        PiInt=*on
037200121001     C                   Z-ADD     PiVal         Num5_0
037300121001     C                   MOVEL     Num5_0        VABCAD
037400121001     C                   ELSE
037500121001     C                   SETON                                        32
037600121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037700121001     C                             + ' ' + 'VABCAD'
037800121001     C                   ENDIF
037900080923     C* RMN
038000121011     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
038100121001     C                   EXSR      CHKNUM
038200121001     C                   IF        PiInt=*on
038300121001     C                   Z-ADD     PiVal         VABRMN
038400121001     C                   ELSE
038500121001     C                   SETON                                        32
038600121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038700121001     C                             + ' ' + 'VABRMN'
038800121001     C                   ENDIF
038900080923     C* NSP
039000080923     C                   IF        *in51 = *on
039100080923     C* NSP => Stacco un numeratore da AZNUM
039200080923     C                   clear                   TRUL33DS
039300080923     C                   eval      I33OPE = *zeros
039400080923     C                   eval      I33CNU = 302
039500080923     C                   eval      I33NUM = 1
039600080923     C                   movel     TRUL33DS      KPJBU
039700080923     C                   call      'TRUL33R'
039800080923     C                   parm                    KPJBA
039900080923     C                   movel     KPJBU         TRUL33DS
040000080923     C                   if        O33ERR = *zeros
040100080923     C                   z-add     O33NRF        VABNSP
040200080923     C                   z-add     O33NRF        VATNSP
040300080923     C                   else
040400080923     C                   SETON                                        31
040500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040600080923     C                             + ' ' + 'VABNSP VATNSP'
040700080923     C                   endif
040800080923     C                   ELSE
040900121011     C*** non è indicata la posizione di questo dato sul file
041000121011     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
041100121011     C***                EXSR      CHKNUM
041200121011     C***                IF        PiInt=*on
041300121011     C***                Z-ADD     PiVal         VABNSP
041400121011     C***                Z-ADD     PiVal         VATNSP
041500121011     C***                ELSE
041600121011     C***                SETON                                        31
041700121011     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
041800121011     C***                          + ' ' + 'VABNSP VATNSP'
041900121011     C***                ENDIF
042000080923     C                   ENDIF
042100071003     C* NCL
042200121011     C                   EVAL      PiStr=%trim(%subst(vindta:303:4))
042300070928     C                   EXSR      CHKNUM
042400070928     C                   IF        PiInt=*on
042500070928     C                   Z-ADD     PiVal         VABNCL
042600070928     C                   ELSE
042700070928     C                   SETON                                        32
042800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042900070928     C                             + ' ' + 'VABNCL'
043000070928     C                   ENDIF
043100071003     C* PKB
043200121011     C                   EVAL      PiStr=%trim(%subst(vindta:307:8))
043300070928     C                   EXSR      CHKNUM
043400070928     C                   IF        PiNum=*on
043500121011     C* il dato ha 3 decimali
043600121011     C                   EVAL(H)   VABPKB = PiVal / 1000
043700070928     C                   ELSE
043800070928     C                   SETON                                        32
043900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044000070928     C                             + ' ' + 'VABPKB'
044100070928     C                   ENDIF
044200121029     C* VLB
044300121029     C                   EVAL      PiStr=%trim(%subst(vindta:469:5))
044400121029     C                   EXSR      CHKNUM
044500121029     C                   IF        PiNum=*on
044600121029     C* il dato ha 3 decimali
044700121029     C                   EVAL(H)   VABVLB = PiVal / 1000
044800121029     C                   ELSE
044900121029     C                   SETON                                        32
045000121029     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100121029     C                             + ' ' + 'VABVLB'
045200121029     C                   ENDIF
045300080923     C* CAS
045400121011     C* l'importo contrassegno ha 2 decimali e viene scritto senza separatore decimale
045500121011     C                   IF        %trim(%subst(vindta:315:15))<>*blanks
045600080923     C                             AND
045700121011     C                             %trim(%subst(vindta:315:15))<>*zeros
045800121011     C                             AND
045900121011     C                             %trim(%subst(vindta:315:15))<>
046000121011     C                              '              0'
046100121011     C                   EVAL      VABVCA = 'EUR'
046200070928     C                   MOVEL     '1'           FlgCAS
046300121011     C                   EVAL      PiStr=%trim(%subst(vindta:315:15))
046400070928     C                   EXSR      CHKNUM
046500070928     C                   IF        PiNum=*on
046600121011     C                   EVAL(H)   VABCAS = PiVal/100
046700070928     C                   ELSE
046800070928     C                   SETON                                        32
046900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047000070928     C                             + ' ' + 'VABCAS'
047100070928     C                   ENDIF
047200070928     C                   ENDIF
047300070730     C*
047400121106     C* Considerazioni finali su CAS
047500121106     C*
047600121106      * se c/assegno impostato
047700121106     C                   IF        VABCAS > 0
047800121122     C                   EVAL      VABCBO = '4'
047900121106      * metto BM come tipo incasso
048000121106     C                   EVAL      VABTIC = 'BM'
048100121106      * metto come tipo consegna particolare l'appuntamento
048200161207     C***                EVAL      VABTC1 = 'A'
048300121106     C                   ENDIF
048400020305     C*
048500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048600011113     C                   EXSR      CHKIMPDIV
048700010202     C*
048800000801     C* Ebbene...
048900000801     C                   ADD       1             §CTRMO
049000070530     C                   IF        *in31 <> *zeros OR
049100070530     C                             *in32 <> *zeros
049200000801     C                   ADD       1             §CTRNO
049300020725     C                   EVAL      x_vinflg = '2'
049400000801     C                   ELSE
049500010201     C                   ADD       1             §CTROKVB
049600000801     C                   ENDIF
049700000801     C*
049800000801     C                   ENDSR
049900010201     C*----------------------------------------------------*
050000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
050100010201     C*----------------------------------------------------*
050200071003     C     WRIVAT_B      BEGSR
050300090204     C*
050400121122     C                   eval      vatCCM = vabCCM
050500090204     C                   eval      vatAAS = vabAAS
050600090204     C                   eval      vatLNP = vabLNP
050700090204     C                   eval      vatNRS = vabNRS
050800090204     C                   eval      vatNSP = vabNSP
050900010201     C*
051000101007     C* Valorizzo il buffer di scrittura del FIVAT
051100070928     C* - TIPO RECORD "B"
051200121121     C                   eval      VATNOT = %trim(%subst(vindta:474:17))
051300121121     C                   if        VATNOT <> *blanks
051400070928     C                   eval      VATTRC = 'B'
051500021113     C                   write     FIVAT000
051600020725     C                   endif
051700010201     C*
051800010201     C                   ENDSR
051900140304     C*----------------------------------------------------*
052000140304     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
052100140304     C*----------------------------------------------------*
052200140304     C     WRIVAT_I      BEGSR
052300140306     C*
052400140306     C                   eval      vatCCM = vabCCM
052500140306     C                   eval      vatAAS = vabAAS
052600140306     C                   eval      vatLNP = vabLNP
052700140306     C                   eval      vatNRS = vabNRS
052800140306     C                   eval      vatNSP = vabNSP
052900140304     C*
053000140306     C* gli indirizzi email possono essere più d'uno divisi da ";"
053100140306     C* si prende solo il primo
053200140306     C                   eval      w70 = %trim(%subst(vindta:561:70))
053300140306     C                   EVAL      wPos=%scan(';':w70)
053400140306     C                   SELECT
053500140306     C*
053600140306     C* se il primo ";" darebbe un indirizzo lungo al massimo 35 char, creo solo il TRC = I
053700140306     C                   WHEN      wPos <> 0
053800140306     C                             and wPos < 37
053900140304     C* Valorizzo il buffer di scrittura del FIVAT
054000140304     C* - TIPO RECORD "I"
054100140306     C                   eval      VATNOT = %trim(%subst(vindta:561:wPos-1))
054200140304     C                   if        VATNOT <> *blanks
054300140304     C                   eval      VATTRC = 'I'
054400140304     C                   write     FIVAT000
054500140304     C                   endif
054600140306     C*
054700140306     C* se il primo ";" darebbe un indirizzo lungo più di 35 char, creo i TRC = I e J
054800140306     C                   WHEN      wPos <> 0
054900140306     C                             and wPos >= 37
055000140306     C* Valorizzo il buffer di scrittura del FIVAT
055100140306     C* - TIPO RECORD "I"
055200140306     C                   eval      VATNOT = %trim(%subst(vindta:561:35))
055300140306     C                   if        VATNOT <> *blanks
055400140306     C                   eval      VATTRC = 'I'
055500140306     C                   write     FIVAT000
055600140306     C                   endif
055700140306     C* - TIPO RECORD "J"
055800140306     C                   eval      VATNOT = %trim(%subst(vindta:561+35:wPos-36))
055900140306     C                   if        VATNOT <> *blanks
056000140306     C                   eval      VATTRC = 'J'
056100140306     C                   write     FIVAT000
056200140306     C                   endif
056300140306     C*
056400140306     C* se non trovo il primo ";" all'interno dei 70 char, creo i TRC = I e J
056500140306     C* sapendo che l'indirizzo non è completo
056600140306     C* oppure che è vuoto
056700140306     C* oppure che ce n'è uno solo e manca il ";"
056800140306     C                   OTHER
056900140306     C* Valorizzo il buffer di scrittura del FIVAT
057000140306     C* - TIPO RECORD "I"
057100140306     C                   eval      VATNOT = %trim(%subst(vindta:561:35))
057200140306     C                   if        VATNOT <> *blanks
057300140306     C                   eval      VATTRC = 'I'
057400140306     C                   write     FIVAT000
057500140306     C                   endif
057600140306     C* - TIPO RECORD "J"
057700140306     C                   eval      VATNOT = %trim(%subst(vindta:561+35:35))
057800140306     C                   if        VATNOT <> *blanks
057900140306     C                   eval      VATTRC = 'J'
058000140306     C                   write     FIVAT000
058100140306     C                   endif
058200140306     C*
058300140306     C                   ENDSL
058400140306     C*
058500140304     C                   ENDSR
058600071003     C*----------------------------------------------------*
058700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058800071003     C*----------------------------------------------------*
058900071003     C     WRIVAT_E      BEGSR
059000090204     C*
059100090204     C                   eval      vatAAS = vabAAS
059200090204     C                   eval      vatLNP = vabLNP
059300090204     C                   eval      vatNRS = vabNRS
059400090204     C                   eval      vatNSP = vabNSP
059500151014     C*
059600151014     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli in questo modo;:
059700160211     C* VABRMA (?) + <progressivo collo> (3) + VABNCL (3)
059800151014     C*
059900151014     C                   eval      VATTRC = 'E'
060000151014     C*
060100151014     C     1             DO        VABNCL        wColloPrg
060200160211     C                   EVAL      VATNOT =
060300160211     C                                      %trim(%subst(vindta:330:6)) +
060400151014     C                                      %editc(wColloPrg:'X') +
060500151014     C                                      %editc(%dec(VABNCL:3:0):'X')
060600071003     C*
060700071003     C                   write     FIVAT000
060800151014     C                   ENDDO
060900071003     C*
061000071003     C                   ENDSR
061100020904
061200020904
061300010202     C*----------------------------------------------------*
061400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061500010202     C*----------------------------------------------------*
061600020305     C     PREVAT        BEGSR
061700010202     C*
061800021113     C* Compongo il nome del membro da dare al FIVATWWR
061900010202     C                   eval      parmbr = vlrhdl
062000010202     C                   movel     'M'           parmbr
062100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062200010202     C                   eval      paropz = '1'
062300010202     C* Effettuo la chiamata al CLLE preposto
062400050128     C                   call(e)   'TITVVTC'
062500010202     C                   parm                    parccm
062600010202     C                   parm                    parmbr
062700010202     C                   parm                    paropz
062800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062900010202     C                   if        %error
063000010202     C                   movel     '1'           chkcall
063100010202     C                   else
063200010202     C                   movel     '0'           chkcall
063300010202     C                   endif
063400010202     C*
063500010202     C                   ENDSR
063600000801     C*----------------------------------------------------*
063700000801     C*  CONTROLLO NUMERICITA' CAMPI
063800000801     C*----------------------------------------------------*
063900000801     C     CHKNUM        BEGSR
064000081113     C*
064100081113     C                   IF        PiDecChr = *blanks
064200110524     C                   EVAL      PiDecChr = '.'
064300081113     C                   ENDIF
064400081113     C*
064500081113     C                   callp(e)  UBISNUM_Check(PiStr
064600081113     C                                          :PiDecChr
064700081113     C                                          :PiVal
064800081113     C                                          :PiNum
064900081113     C                                          :PiInt)
065000081113     C*
065100000801     C                   IF        %error
065200000801     C                   EVAL      PiInt=*off
065300000801     C                   ENDIF
065400000801     C*
065500000801     C                   ENDSR
065600000801     C***
065700000801
065800011113
065900011113     C*----------------------------------------------------*
066000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066100011113     C*----------------------------------------------------*
066200011113     C     CHKIMPDIV     BEGSR
066300011113     C*
066400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066500011113     C                   Z-ADD     *zeros        wrkDec            9 9
066600011113     C*
066700011113     C* Come prima cosa effettuo considerazioni sulla divisa
066800011113     C                   IF        vabIAS > *zeros
066900011113     C                   IF        vabVAS <> 'EUR'
067000011113     C                   EVAL      vabVAS =  'ITL'
067100011113     C                   ENDIF
067200011113     C                   ENDIF
067300011113     C*
067400011113     C                   IF        vabCAS > *zeros
067500011113     C                   IF        vabVCA <> 'EUR'
067600011113     C                   EVAL      vabVCA =  'ITL'
067700011113     C                   ENDIF
067800011113     C                   ENDIF
067900011113     C*
068000011113     C                   IF        vabVMD > *zeros
068100020305     C                   IF        vabVAD <> 'EUR'
068200011113     C                   EVAL      vabVAD =  'ITL'
068300011113     C                   ENDIF
068400011113     C                   ENDIF
068500011113     C*
068600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068700011113     C                   Z-ADD     vabIAS        wrkDec
068800011113     C                   IF        wrkDec > *zeros
068900011113     C                   IF        vabVAS = 'ITL'
069000011113     C                   EVAL      vabIAS = *zeros
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069500011113     C                   Z-ADD     vabCAS        wrkDec
069600011113     C                   IF        wrkDec > *zeros
069700011113     C                   IF        vabVCA = 'ITL'
069800011113     C                   EVAL      vabCAS = *zeros
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070300011113     C                   Z-ADD     vabVMD        wrkDec
070400011113     C                   IF        wrkDec > *zeros
070500011113     C                   IF        vabVAD = 'ITL'
070600011113     C                   EVAL      vabVMD = *zeros
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C                   ENDSR
071100011113     C***
071200011113
071300011113
071400000801
071500000801
071600990920      /TITLE Invio dei dati al punto operativo.
071700010202     C     invio         BEGSR
071800990920     C*
071900021113     C* 1° invio FIVAT
072000010201     C                   reset                   dscmz
072100010201     C                   move      vlrpoi        cmzdst
072200021113     C                   eval      cmzfld = 'FIVATWWR'
072300010201     C                   eval      cmzmbd = vlrhdl
072400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072500021009     C***                if        prmfir = *blanks
072600021113     C                   eval      cmzfla = 'FIVAT00F'
072700021113     C                   eval      cmzmba = 'FIVAT00F'
072800021009     C***                else
072900021009     C***                eval      cmzfla = prmfir
073000021009     C***                eval      cmzmba = prmfir
073100021009     C***                endif
073200010201     C                   eval      cmznrr = *zeros
073300020305     C                   move      §ctrokvt      cmznrr
073400021018     C                   eval      cmzlba = vlrfl1
073500010201     C                   call(e)   'TIS711C'
073600010201     C                   parm                    dscmz
073700010201     C                   parm      *blanks       esito
073800010205     C                   if        %error
073900010205     C                             or cmzerr = '1'
074000010205     C                             or esito  = '1'
074100010205     C                   eval      wrkesito = '3'
074200010205     C                   else
074300010201     C*
074400021113     C* 2° invio FIVAB
074500010201     C                   reset                   dscmz
074600010201     C                   move      vlrpoi        cmzdst
074700010201     C                   eval      cmzfld = vlrfou
074800010201     C                   eval      cmzmbd = vlrhdl
074900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075000021009     C***                if        prmfir = *blanks
075100021113     C                   eval      cmzfla = 'FIVAB00F'
075200021113     C                   eval      cmzmba = 'FIVAB00F'
075300021009     C***                else
075400021009     C***                eval      cmzfla = prmfir
075500021009     C***                eval      cmzmba = prmfir
075600021009     C***                endif
075700010201     C                   eval      cmznrr = *zeros
075800010201     C                   move      §ctrokvb      cmznrr
075900021018     C                   eval      cmzlba = vlrfl1
076000010201     C                   call(e)   'TIS711C'
076100010201     C                   parm                    dscmz
076200010201     C                   parm      *blanks       esito
076300010201     C                   if        %error
076400010201     C                             or cmzerr = '1'
076500010201     C                             or esito  = '1'
076600010201     C                   eval      wrkesito = '3'
076700010201     C                   endif
076800010205     C                   endif
076900990920     C*
077000000613     C                   ENDSR
077100000613     C***
077200070411
077300070411     C     *pssr         BEGSR
077400070411     C*
077500070411     C                   if        %open(tivin00r)
077600070411     C                   close     tivin00r
077700070411     C                   endif
077800070411     C                   if        %open(fivabwwr)
077900070411     C                   close     fivabwwr
078000070411     C                   endif
078100070411     C                   if        %open(fivatwwr)
078200070411     C                   close     fivatwwr
078300070411     C                   endif
078400070411     C*
078500070411     C* Effettuo la chiamata al CLLE preposto
078600070411     C                   call(e)   'TITVVTC'
078700070411     C                   parm                    parccm
078800070411     C                   parm                    parmbr
078900070411     C                   parm      '2'           paropz
079000070411     C*
079100070411     C                   eval      wrkesito = '2'
079200070411     C*
079300070411     C                   seton                                        LR
079400070411     C*
079500070411     C                   ENDSR     '*CANCL'
079600070411     C***
079700070411
079800990910
079900000613     C     *inzsr        BEGSR
080000990910     C*
080100990910     C     *entry        plist
080200990920     C                   parm                    tivlrds
080300990921     C                   parm      wrkesito      esito
080400000724     C                   parm                    prmlit
080500000710     C                   parm                    prmfir
080600000613     C*
080700000830     C* CALCOLA LA DATA CORRENTE
080800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080900100324     C                   eval      datcor = %dec(%date() : *ISO)
081000000830     C*
081100000613     C                   ENDSR
081200000613     C***
