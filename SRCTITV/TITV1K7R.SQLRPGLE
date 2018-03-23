000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081203     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002100081203     D depspe          s              9    INZ(*blanks)
002200081203     D curspe          s              9    INZ(*blanks)
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
004400081203     D  VINDTA                     2048
004500081203     D  VINFLG                        1
004600081203     D  VINSPE                        9
004700081203     D  VINRRN                        8  0
004800020725     D*
004900081203
005000081203
005100081203     D*------------------
005200081203     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005300081203     D*------------------
005400081203     D PiStr           S            100A   INZ
005500081203     D PiDecChr        S              1A   INZ
005600081203     D PiVal           S             63S30 INZ
005700081203     D PiInt           S               N   INZ
005800081203     D PiNum           S               N   INZ
005900081203
006000081203     D*------------------
006100081203     D* LINKING A DEFINIZIONI ESTERNE
006200081203     D*------------------
006300081203     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400081203
006500081203
006600990908
006700010201
006800071120     C*
006900071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000071120     C
007100071120     C/EXEC SQL
007200071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300071120     C/END-EXEC
007400071120     C*
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
009500990908
009600000801
009700910830     C*--------------------------------------------------------
009800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009900910830     C*--------------------------------------------------------
010000070530     C     RWFILE        BEGSR
010100990910     C*
010200990914     C                   if        not %open(tivin00r)
010300990908     C                   open      tivin00r
010400990914     C                   endif
010500021113     C                   if        not %open(fivabwwr)
010600021113     C                   open      fivabwwr
010700990914     C                   endif
010800070530     C*
010900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000020305     C                   exsr      prevat
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400021113     C                   if        not %open(fivatwwr)
011500021113     C                   open      fivatwwr
011600010201     C                   endif
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200990910     C*
012300020725     C/EXEC SQL
012400020725     C+ declare C1 cursor for select
012500081203     C+ vindta, vinflg, substr(vindta, 123, 9) as sped, rrn(tivin00r)
012600060223     C+ from tivin00r
012700060223     C+ order by sped
012800020725     C+ for read only
012900020725     C/END-EXEC
013000020725     C
013100020725     C/EXEC SQL
013200020725     C+ open C1
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600070530     C+ Fetch C1 into :INPUT_DS
013700020725     C/END-EXEC
013800020725     C*
013900020725     C                   dow       sqlcod = *zeros
014000990913     C*
014100020725     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400020725     C                   if        vinflg = *blanks
014500020725     C                             or vinflg = '0'
014600020725     C                             or vinflg = '2'
014700000801     C*
014800020725     C                   clear                   x_vinmsg
014900020725     C                   eval      x_vinflg = '1'
015000010305     C*
015100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200081203     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
015300020305     C                   MOVEL(p)  PiStr         curspe
015400010305     C*
015500010305     C                   if        depspe = *blanks                             => 1° giro
015600010305     C                   eval      depspe = curspe                              => memorizz. spediz
015700020725     C                   clear                   tivinds
015800020725     C                   exsr      inzvar
015900020725     C                   exsr      defcam
016000020305     C                   exsr      impvab
016100081203     C                   exsr      wrivate                                      => carico VAT
016200010305     C                   else
016300020725     C                   if        depspe <> curspe                             => rottura di spediz
016400010305     C                   eval      depspe = curspe                              => memorizz. spediz
016500020305     C                   exsr      wrivab
016600020725     C                   clear                   tivinds
016700020725     C                   exsr      inzvar
016800020725     C                   exsr      defcam
016900020305     C                   exsr      impvab
017000081203     C                   exsr      wrivate                                      => carico VAT
017100020305     C                   else                                                   => x stessa spediz
017200020305     C                   exsr      impvab
017300081203     C                   exsr      wrivate                                      => carico VAT
017400010305     C                   endif
017500010305     C                   endif
017600010305     C                   endif
017700000905     C*
017800000905     C                   else
017900020725     C                   eval      x_vinflg = '1'
018000000905     C                   endif
018100000905     C*
018200020725     C     VINRRN        chain     tivin000
018300020725     C                   if        %found(tivin00r)
018400020725     C                   eval      y_vinflg = x_vinflg
018500020725     C                   eval      y_vinmsg = x_vinmsg
018600020725     C                   update    tivin000
018700020725     C                   endif
018800020725     C*
018900020725     C/EXEC SQL
019000070530     C+ Fetch C1 into :INPUT_DS
019100020725     C/END-EXEC
019200020725     C*
019300020725     C                   enddo
019400020725     C*
019500020725     C/EXEC SQL
019600020725     C+ close C1
019700020725     C/END-EXEC
019800000905     C*
019900020305     C* Scarico i VAB rimasti "in sospeso"
020000020305     C                   exsr      wrivab
020100010202     C*
020200010202     C                   endif
020300990910
020400990910     C* Se non ci sono record con errori ...
020500000710     C                   if        §ctrno = 0
020600990910     C* ... restituisco esito OK.
020700990921     C                   eval      wrkesito = '0'
020800990910     C                   else
020900010201     C                   if        §ctrokvb > 0
021000990921     C                   eval      wrkesito = '1'
021100000710     C                   else
021200000710     C                   eval      wrkesito = '2'
021300990910     C                   endif
021400000710     C                   endif
021500990910     C*
021600990914     C                   if        %open(tivin00r)
021700990908     C                   close     tivin00r
021800990914     C                   endif
021900021113     C                   if        %open(fivabwwr)
022000021113     C                   close     fivabwwr
022100990914     C                   endif
022200021113     C                   if        %open(fivatwwr)
022300021113     C                   close     fivatwwr
022400010201     C                   endif
022500990910     C*
022600010201     C                   if        §ctrokvb > 0
022700000724     C                             and vlrpoi <> *zeros
022800010202     C                   exsr      invio
022900990920     C                   endif
023000990920     C*
023100910830     C                   ENDSR
023200000613     C***
023300010305
023400010305     C*----------------------------------------------------*
023500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023600010305     C*----------------------------------------------------*
023700020305     C     WRIVAB        BEGSR
023800070730     C*
023900021113     C                   write     fivab000                                     => scarico il VAB
024000070530     C*
024100070530     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A e B
024200070530     C                   if        wVAT_A <> *blanks
024300070530     C                   eval      VATTRC = 'A'
024400070530     C                   eval      VATNOT = wVAT_A
024500070530     C                   write     FIVAT000
024600070530     C                   endif
024700070530     C*
024800070530     C                   if        wVAT_B <> *blanks
024900070530     C                   eval      VATTRC = 'B'
025000070530     C                   eval      VATNOT = wVAT_B
025100070530     C                   write     FIVAT000
025200070530     C                   endif
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100010201     C                   Z-ADD     *zeros        Num5_0
026200070530     C                   MOVEL     *blanks       wVAT_A           30
026300070530     C                   MOVEL     *blanks       wVAT_B           30
026400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026500020725     C                   MOVEL     '0'           FlgCAS            1
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027000000801     C*----------------------------------------------------*
027100020904     C     DEFCAM        BEGSR
027200000801     C*
027300021113     C                   CLEAR                   FIVAB000
027400021113     C                   CLEAR                   FIVAT000
027500070730     C* Imposto i valori di default...
027600081203     C                   EVAL      VABCCM = 0633192
027700081203     C                   EVAL      VATCCM = 0633192
027800081204     C                   EVAL      VABLNP = 255
027900081204     C                   EVAL      VATLNP = 255
028000081203     C                   EVAL      VABCTR = 000
028100070730     C                   EVAL      VABCBO = '1'
028200081203     C                   MOVEL(P)  '7Q'          VABCTM
028300070530     C                   MOVEL     '1'           VABCBO
028400070222     C* ... e poi verifico se sono stati passati come parametri
028500070222     C                   IF        vlrppt > *blanks
028600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
028700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028800070222     C                   EXSR      CHKNUM
028900070222     C                   IF        PiInt=*on
029000070222     C                   Z-ADD     PiVal         VABCCM
029100070222     C                   Z-ADD     PiVal         VATCCM
029200070222     C                   ENDIF
029300070222     C                   ENDIF
029400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABLNP
029900070222     C                   Z-ADD     PiVal         VATLNP
030000070222     C                   ENDIF
030100070222     C                   ENDIF
030200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABCTR
030700070222     C                   ENDIF
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000000801     C*
031100000801     C                   ENDSR
031200000801     C*----------------------------------------------------*
031300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031400000801     C*----------------------------------------------------*
031500010201     C     IMPVAB        BEGSR
031600070530     C*
031700070530     C                   SETOFF                                       3132
031800021113     C*
031900021113     C                   MOVE(P)   vlrpoi        VABFGS
032000021113     C                   MOVE(P)   vlrpoi        VATFGS
032100020725     C*
032200070530     C* Campi anno e mese/giorno
032300070530     C                   MOVEL     datcor        VABAAS
032400070530     C                   MOVEL     datcor        VATAAS
032500070530     C                   MOVE      datcor        VABMGS
032600070730     C*
032700070730     C* Reperimento campi ALFA
032800070730     C*
032900070730     C* Considerazioni sulla ragione sociale del destinatario da indicare
033000081203     C                   EVAL      VABRSD=%trim(%subst(vindta:25:30))
033100070730     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033200070730     C     '@':'A'       XLATE     VABRSD        VABRSD
033300070730     C* ==
033400081203     C                   EVAL      VABIND=%trim(%subst(vindta:55:30))
033500081203     C                   EVAL      VABLOD=%trim(%subst(vindta:90:20))
033600081203     C                   EVAL      VABPRD=%trim(%subst(vindta:110:2))
033700081203     C                   EVAL      VABRMA=%trim(%subst(vindta:114:8))
033800081203     C                   IF        %subst(vindta:140:1) = 'X'
033900081203     C                   EVAL      FlgCAS = '1'
034000081203     C                   ENDIF
034100070730     C*
034200070730     C* Reperimento campi NUMERICI
034300070730     C* NSP/RMN
034400081203     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
034500070730     C                   EXSR      CHKNUM
034600070730     C                   IF        PiInt=*on
034700070730     C                   Z-ADD     PiVal         VABNSP
034800070730     C                   Z-ADD     PiVal         VATNSP
034900070730     C                   Z-ADD     PiVal         VABRMN
035000070730     C                   ELSE
035100070730     C                   SETON                                        31
035200070730     C                   Z-ADD     *zeros        VABNSP
035300070730     C                   Z-ADD     *zeros        VATNSP
035400070730     C                   Z-ADD     1             VABRMN
035500070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035600070730     C                             + ' ' + 'VABNSP VATNSP VABRMN'
035700070730     C                   ENDIF
035800070730     C* CAD
035900081203     C                   EVAL      PiStr=%trim(%subst(vindta:85:5))
036000070730     C                   EXSR      CHKNUM
036100070730     C                   IF        PiInt=*on
036200070730     C                   Z-ADD     PiVal         Num5_0
036300070730     C                   MOVEL(p)  Num5_0        VABCAD
036400070730     C                   ELSE
036500070730     C                   SETON                                        32
036600070730     C                   EVAL      VABCAD = *zeros
036700070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036800070730     C                             + ' ' + 'VABCAD'
036900070730     C                   ENDIF
037000070730     C* Reperisco la provincia dal CAP e dalla località
037100070730     C                   IF        VABCAD <> *blanks AND
037200070730     C                             VABPRD  = *blanks
037300070730     C                   CLEAR                   TISI95DS
037400070730     C                   EVAL      I95TCN = '3'
037500070730     C                   Z-ADD     datcor        I95DAT
037600070730     C                   EVAL      I95CAP = VABCAD
037700070730     C                   EVAL      I95LOC = VABLOD
037800070730     C                   EVAL      I95NAR = VABNZD
037900070730     C                   CALL      'TISI95R'
038000070730     C                   PARM                    TISI95DS
038100070730     C                   EVAL      VABPRD = O95PRV
038200070730     C                   ENDIF
038300070730     C* NCL
038400081203     C                   EVAL      PiStr=%trim(%subst(vindta:16:3))
038500070730     C                   EXSR      CHKNUM
038600070730     C                   IF        PiInt=*on
038700081203     C                   Z-ADD     PiVal         VABNCL
038800070730     C                   ELSE
038900070730     C                   SETON                                        32
039000070730     C                   Z-ADD     *zeros        VABNCL
039100070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039200070730     C                             + ' ' + 'VABNCL'
039300070730     C                   ENDIF
039400070730     C* PKB
039500081203     C                   EVAL      PiStr=%trim(%subst(vindta:19:6))
039600070730     C                   EVAL      PiDecChr = '.'
039700070730     C                   EXSR      CHKNUM
039800070730     C                   IF        PiNum=*on
039900081203     C                   ADD       PiVal         VABPKB
040000070730     C                   ELSE
040100070730     C                   SETON                                        32
040200070730     C                   Z-ADD     *zeros        VABPKB
040300070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400070730     C                             + ' ' + 'VABPKB'
040500070730     C                   ENDIF
040600070730     C*
040700070730     C* Considerazioni finali su CBO/CAS
040800070730     C                   IF        FlgCAS = '1'
040900070730     C                   IF        VABCBO = '1'
041000070730     C                   EVAL      VABCBO = '4'
041100070730     C                   ENDIF
041200070730     C                   IF        VABCBO = '2'
041300070730     C                   EVAL      VABCBO = '6'
041400070730     C                   ENDIF
041500070730     C                   ENDIF
041600020305     C*
041700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041800011113     C                   EXSR      CHKIMPDIV
041900010202     C*
042000000801     C* Ebbene...
042100000801     C                   ADD       1             §CTRMO
042200070530     C                   IF        *in31 <> *zeros OR
042300070530     C                             *in32 <> *zeros
042400000801     C                   ADD       1             §CTRNO
042500020725     C                   EVAL      x_vinflg = '2'
042600000801     C                   ELSE
042700010201     C                   ADD       1             §CTROKVB
042800000801     C                   ENDIF
042900000801     C*
043000000801     C                   ENDSR
043100010201     C*----------------------------------------------------*
043200021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043300010201     C*----------------------------------------------------*
043400081203     C     WRIVATE       BEGSR
043500081203     C*
043600081204     C                   z-add     *zeros        wLenWhoAre        2 0
043700081204     C*
043800081204     C                   eval      VATNOT=%trim(%subst(vindta:141:10))
043900081203     C                   if        VATNOT <> *blanks
044000081204     C                   eval      wLenWhoAre = %len(%trim(VATNOT))
044100081204     C*
044200081204     C* Aggiungo costante prima del chi-sono x conformità etichetta
044300081204     C                   select
044400081204     C                   when      wLenWhoAre = 5
044500081204     C                   eval      VATNOT = 'M000' + %trim(VATNOT)
044600081204     C                   when      wLenWhoAre = 6
044700081204     C                   eval      VATNOT = 'M00' + %trim(VATNOT)
044800081204     C                   when      wLenWhoAre = 7
044900081204     C                   eval      VATNOT = 'M0' + %trim(VATNOT)
045000081204     C                   when      wLenWhoAre = 8
045100081204     C                   eval      VATNOT = 'M' + %trim(VATNOT)
045200081204     C                   other
045300081204     C                   endsl
045400081204     C*
045500081203     C                   eval      VATTRC = 'E'
045600081203     C                   write     FIVAT000
045700081203     C                   endif
045800010201     C*
045900010201     C                   ENDSR
046000020904
046100020904
046200010202     C*----------------------------------------------------*
046300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046400010202     C*----------------------------------------------------*
046500020305     C     PREVAT        BEGSR
046600010202     C*
046700021113     C* Compongo il nome del membro da dare al FIVATWWR
046800010202     C                   eval      parmbr = vlrhdl
046900010202     C                   movel     'M'           parmbr
047000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
047100010202     C                   eval      paropz = '1'
047200010202     C* Effettuo la chiamata al CLLE preposto
047300050128     C                   call(e)   'TITVVTC'
047400010202     C                   parm                    parccm
047500010202     C                   parm                    parmbr
047600010202     C                   parm                    paropz
047700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047800010202     C                   if        %error
047900010202     C                   movel     '1'           chkcall
048000010202     C                   else
048100010202     C                   movel     '0'           chkcall
048200010202     C                   endif
048300010202     C*
048400010202     C                   ENDSR
048500081203
048600081203
048700081203
048800081203     C*----------------------------------------------------*
048900081203     C*  CONTROLLO NUMERICITA' CAMPI
049000081203     C*----------------------------------------------------*
049100081203     C     CHKNUM        BEGSR
049200081203     C*
049300081203     C                   IF        PiDecChr = *blanks
049400081203     C                   EVAL      PiDecChr = '.'
049500081203     C                   ENDIF
049600081203     C*
049700081203     C                   callp(e)  UBISNUM_Check(PiStr
049800081203     C                                          :PiDecChr
049900081203     C                                          :PiVal
050000081203     C                                          :PiNum
050100081203     C                                          :PiInt)
050200081203     C*
050300081203     C                   IF        %error
050400081203     C                   EVAL      PiInt=*off
050500081203     C                   ENDIF
050600081203     C*
050700081203     C                   ENDSR
050800081203     C***
050900081203
051000081203
051100000801
051200011113
051300011113     C*----------------------------------------------------*
051400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051500011113     C*----------------------------------------------------*
051600011113     C     CHKIMPDIV     BEGSR
051700011113     C*
051800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051900011113     C                   Z-ADD     *zeros        wrkDec            9 9
052000011113     C*
052100011113     C* Come prima cosa effettuo considerazioni sulla divisa
052200011113     C                   IF        vabIAS > *zeros
052300011113     C                   IF        vabVAS <> 'EUR'
052400011113     C                   EVAL      vabVAS =  'ITL'
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   IF        vabCAS > *zeros
052900011113     C                   IF        vabVCA <> 'EUR'
053000011113     C                   EVAL      vabVCA =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C                   IF        vabVMD > *zeros
053500020305     C                   IF        vabVAD <> 'EUR'
053600011113     C                   EVAL      vabVAD =  'ITL'
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054100011113     C                   Z-ADD     vabIAS        wrkDec
054200011113     C                   IF        wrkDec > *zeros
054300011113     C                   IF        vabVAS = 'ITL'
054400011113     C                   EVAL      vabIAS = *zeros
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054900011113     C                   Z-ADD     vabCAS        wrkDec
055000011113     C                   IF        wrkDec > *zeros
055100011113     C                   IF        vabVCA = 'ITL'
055200011113     C                   EVAL      vabCAS = *zeros
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055700011113     C                   Z-ADD     vabVMD        wrkDec
055800011113     C                   IF        wrkDec > *zeros
055900011113     C                   IF        vabVAD = 'ITL'
056000011113     C                   EVAL      vabVMD = *zeros
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C                   ENDSR
056500011113     C***
056600011113
056700011113
056800000801
056900000801
057000990920      /TITLE Invio dei dati al punto operativo.
057100010202     C     invio         BEGSR
057200990920     C*
057300021113     C* 1° invio FIVAT
057400010201     C                   reset                   dscmz
057500010201     C                   move      vlrpoi        cmzdst
057600021113     C                   eval      cmzfld = 'FIVATWWR'
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021113     C                   eval      cmzfla = 'FIVAT00F'
058100021113     C                   eval      cmzmba = 'FIVAT00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700020305     C                   move      §ctrokvt      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010205     C                   if        %error
059300010205     C                             or cmzerr = '1'
059400010205     C                             or esito  = '1'
059500010205     C                   eval      wrkesito = '3'
059600010205     C                   else
059700010201     C*
059800021113     C* 2° invio FIVAB
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100010201     C                   eval      cmzfld = vlrfou
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500021113     C                   eval      cmzfla = 'FIVAB00F'
060600021113     C                   eval      cmzmba = 'FIVAB00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200010201     C                   move      §ctrokvb      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010201     C                   if        %error
061800010201     C                             or cmzerr = '1'
061900010201     C                             or esito  = '1'
062000010201     C                   eval      wrkesito = '3'
062100010201     C                   endif
062200010205     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500000613     C***
062600070411
062700070411     C     *pssr         BEGSR
062800070411     C*
062900070411     C                   if        %open(tivin00r)
063000070411     C                   close     tivin00r
063100070411     C                   endif
063200070411     C                   if        %open(fivabwwr)
063300070411     C                   close     fivabwwr
063400070411     C                   endif
063500070411     C                   if        %open(fivatwwr)
063600070411     C                   close     fivatwwr
063700070411     C                   endif
063800070411     C*
063900070411     C* Effettuo la chiamata al CLLE preposto
064000070411     C                   call(e)   'TITVVTC'
064100070411     C                   parm                    parccm
064200070411     C                   parm                    parmbr
064300070411     C                   parm      '2'           paropz
064400070411     C*
064500070411     C                   eval      wrkesito = '2'
064600070411     C*
064700070411     C                   seton                                        LR
064800070411     C*
064900070411     C                   ENDSR     '*CANCL'
065000070411     C***
065100070411
065200990910
065300000613     C     *inzsr        BEGSR
065400990910     C*
065500990910     C     *entry        plist
065600990920     C                   parm                    tivlrds
065700990921     C                   parm      wrkesito      esito
065800000724     C                   parm                    prmlit
065900000710     C                   parm                    prmfir
066000000613     C*
066100000830     C* CALCOLA LA DATA CORRENTE
066200000830     C                   time                    wn14             14 0
066300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066400000830     C                   z-add     wn8           g08dat
066500000830     C                   z-add     *zeros        g08inv
066600000830     C                   movel     '0'           g08err
066700000830     C                   call      'XSRDA8'
066800000830     C                   parm                    wlbda8
066900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067000000830     C*
067100000613     C                   ENDSR
067200000613     C***
