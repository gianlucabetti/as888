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
001500100125     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100100125     D depspe          s              6    INZ(*blanks)
002200100125     D curspe          s              6    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800091223     D Num5_0          s              5  0
002900120131     D wNumColloDA     s              5  0 INZ(*zeros)
003000100125     D wNumColloCor    s             10A   INZ(*blanks)
003100100125     D wLenCollo       s              2  0 INZ(*zeros)
003200020725
003300020725     D*------------------
003400020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500020725     D*------------------
003600070530     D INPUT_DS        DS                  INZ
003700020725     D  VINDTA                 1   2048
003800020725     D  VINFLG              2049   2049
003900020725     D  VINSPE              2050   2059
004000020725     D  VINRRN              2060   2067  0
004100020725     D*
004200081113
004300091223     D*------------------
004400091223     D* DS REPERIMENTO NUMERATORE
004500091223     D*------------------
004600091223     D trul33ds      e ds                  inz
004700091223     D*------------------
004800091223     D* DS ARCHITETTURA
004900091223     D*------------------
005000091223     D kpjba         e ds                  inz
005100091223
005200081113
005300081113     D*------------------
005400081113     D* LINKING A DEFINIZIONI ESTERNE
005500081113     D*------------------
005600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005800081113
005900990908
006000010201
006100010201
006200080117     C*
006300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400080117     C
006500080117     C/EXEC SQL
006600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700080117     C/END-EXEC
006800080117     C
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200100125     C*
007300100125     C                   EXSR      opeini
007400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007500000613     C*
007600010202     C* Effettuo la chiamata al CLLE preposto
007700050128     C                   call(e)   'TITVVTC'
007800010202     C                   parm                    parccm
007900010202     C                   parm                    parmbr
008000010202     C                   parm      '2'           paropz
008100070730     C*
008200070730     C* Effettuo lancio TISI95 solo x chiusura
008300070730     C                   CLEAR                   TISI95DS
008400070730     C                   EVAL      I95TLA = 'C'
008500070730     C                   CALL      'TISI95R'
008600070730     C                   PARM                    TISI95DS
008700000616     C*
008800000801     C
008900010201     C                   seton                                        LR
009000100125
009100100125
009200100125
009300100125      /TITLE Invio dei dati al punto operativo.
009400100125     C     opeini        BEGSR
009500100125     C*
009600100125     C* Inizializzo flag e contatori operativi
009700100125     C                   movel     '0'           flgGiro           1
009800100125     C                   movel     '0'           flgMulti          1
009900100125     C                   movel     '1'           flgStato          1
010000100125     C                   movel     '0'           flgOk             1
010100100125     C                   z-add     *zeros        cntNonEl         10 0
010200100125     C                   z-add     *zeros        depfil            3 0
010300100125     C                   z-add     *zeros        invfil            3 0
010400100125     C*
010500100125     C                   ENDSR
010600100125     C***
010700100125
010800100125
010900100125
011000100125     C*--------------------------------------------------------
011100100125     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200100125     C*--------------------------------------------------------
011300100125     C     PREELA        BEGSR
011400100125     C*
011500100125     C* SE OCCORRE SPEDIRE IN FILIALE
011600100125     C                   if        invfil <> *zeros and
011700100125     C                             flgGiro = '0'
011800100125     C*
011900100125     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000100125     C                   eval      flgGiro = '1'
012100100125     C*
012200100125     C                   endif
012300100125     C*
012400100125     C                   ENDSR
012500100125     C***
012600100125
012700100125
012800100125
012900100125      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
013000100125     C     repfil        BEGSR
013100100125     C*
013200100125     C                   if        invfil = *zeros and
013300100125     C                             depfil > *zeros and
013400100125     C                             (vinflg = *blanks or
013500100125     C                              vinflg = *zeros)
013600100125     C
013700100125     C                   eval      invfil = depfil
013800100125     C                   endif
013900100125     C*
014000100125     C                   if        depfil <> invfil and
014100100125     C                             invfil > *zeros
014200100125     C                   eval      flgMulti = '1'
014300100125     C                   if        vinflg = *blanks
014400100125     C                   add       1             cntNonEl
014500100125     C                   endif
014600100125     C                   endif
014700100125     C*
014800100125     C                   if        vinflg = '2'
014900100125     C                   eval      flgStato = '2'
015000100125     C                   endif
015100100125     C*
015200100125     C                   ENDSR
015300100125     C***
015400990908
015500000801
015600910830     C*--------------------------------------------------------
015700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015800910830     C*--------------------------------------------------------
015900070530     C     RWFILE        BEGSR
016000990910     C*
016100990914     C                   if        not %open(tivin00r)
016200990908     C                   open      tivin00r
016300990914     C                   endif
016400021113     C                   if        not %open(fivabwwr)
016500021113     C                   open      fivabwwr
016600990914     C                   endif
016700070530     C*
016800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016900020305     C                   exsr      prevat
017000010201     C*
017100010202     C                   if        chkcall = '0'
017200010202     C*
017300021113     C                   if        not %open(fivatwwr)
017400021113     C                   open      fivatwwr
017500010201     C                   endif
017600080117     C*
017700080117     C                   EXSR      INZVAR
017800080117     C                   EXSR      DEFCAM
017900990910     C*
018000010201     C                   clear                   §CTROKVB          5 0
018100020305     C                   clear                   §CTROKVT          5 0
018200000801     C                   clear                   §CTRMO            5 0
018300000801     C                   clear                   §CTRNO            5 0
018400990910     C*
018500020725     C/EXEC SQL
018600020725     C+ declare C1 cursor for select
018700100125     C+ vindta, vinflg, substr(vindta, 177, 6) as sped, rrn(tivin00r)
018800060223     C+ from tivin00r
018900060223     C+ order by sped
019000020725     C+ for read only
019100020725     C/END-EXEC
019200020725     C
019300020725     C/EXEC SQL
019400020725     C+ open C1
019500020725     C/END-EXEC
019600020725     C
019700020725     C/EXEC SQL
019800070530     C+ Fetch C1 into :INPUT_DS
019900020725     C/END-EXEC
020000020725     C*
020100020725     C                   dow       sqlcod = *zeros
020200990913     C*
020300020725     C                   if        vindta > *blanks
020400000613     C                   add       1             rrnum
020500000801     C*
020600020725     C                   if        vinflg = *blanks
020700020725     C                             or vinflg = '0'
020800020725     C                             or vinflg = '2'
020900000801     C*
021000020725     C                   clear                   x_vinmsg
021100020725     C                   eval      x_vinflg = '1'
021200010305     C*
021300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021400100125     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
021500020305     C                   MOVEL(p)  PiStr         curspe
021600010305     C*
021700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
021800100125     C                   if        *in50 = *off
021900091223     C                   exsr      repNSP
022000071003     C                   exsr      impvab
022100100125     C                   exsr      repfgs
022200071003     C                   exsr      wrivab
022300100125     C                   exsr      wrivat_a                                     => carico VAT
022400100125     C                   exsr      wrivat_b                                     => carico VAT
022500071003     C                   else
022600080923     C*
022700071009     C                   if        wDISK = 'D'
022800091223     C                   exsr      repNSP
022900071009     C                   exsr      impvab
023000100125     C                   exsr      repfgs
023100071009     C                   exsr      wrivab
023200100125     C                   exsr      wrivat_a                                     => carico VAT
023300071009     C                   exsr      wrivat_b                                     => carico VAT
023400071009     C                   exsr      wrivat_e                                     => carico VAT
023500071009     C                   else
023600071009     C*
023700010305     C                   if        depspe = *blanks                             => 1° giro
023800010305     C                   eval      depspe = curspe                              => memorizz. spediz
023900080117     C                   clear                   tivinds
024000091223     C                   exsr      repNSP
024100020305     C                   exsr      impvab
024200100125     C                   exsr      repfgs
024300100125     C                   exsr      wrivat_a                                     => carico VAT
024400071003     C                   exsr      wrivat_b                                     => carico VAT
024500071003     C   50              exsr      wrivat_e                                     => carico VAT
024600010305     C                   else
024700020725     C                   if        depspe <> curspe                             => rottura di spediz
024800010305     C                   eval      depspe = curspe                              => memorizz. spediz
024900070928     C                   exsr      wrivab
025000080117     C                   clear                   tivinds
025100091223     C                   exsr      repNSP
025200020305     C                   exsr      impvab
025300100125     C                   exsr      repfgs
025400100125     C                   exsr      wrivat_a                                     => carico VAT
025500071003     C                   exsr      wrivat_b                                     => carico VAT
025600071003     C   50              exsr      wrivat_e                                     => carico VAT
025700020305     C                   else                                                   => x stessa spediz
025800020305     C                   exsr      impvab
025900100125     C                   exsr      repfgs
026000100125     C                   exsr      wrivat_a                                     => carico VAT
026100071003     C                   exsr      wrivat_b                                     => carico VAT
026200071003     C   50              exsr      wrivat_e                                     => carico VAT
026300010305     C                   endif
026400010305     C                   endif
026500010305     C                   endif
026600071003     C                   endif
026700071009     C                   endif
026800000905     C*
026900000905     C                   else
027000020725     C                   eval      x_vinflg = '1'
027100000905     C                   endif
027200000905     C*
027300020725     C     VINRRN        chain     tivin000
027400020725     C                   if        %found(tivin00r)
027500020725     C                   eval      y_vinflg = x_vinflg
027600020725     C                   eval      y_vinmsg = x_vinmsg
027700020725     C                   update    tivin000
027800020725     C                   endif
027900020725     C*
028000020725     C/EXEC SQL
028100070530     C+ Fetch C1 into :INPUT_DS
028200020725     C/END-EXEC
028300020725     C*
028400020725     C                   enddo
028500020725     C*
028600020725     C/EXEC SQL
028700020725     C+ close C1
028800020725     C/END-EXEC
028900000905     C*
029000020305     C* Scarico i VAB rimasti "in sospeso"
029100071009     C                   if        wDISK = 'C'
029200071009     C                   exsr      wrivab
029300071009     C                   endif
029400010202     C*
029500010202     C                   endif
029600990910
029700990910     C* Se non ci sono record con errori ...
029800000710     C                   if        §ctrno = 0
029900990910     C* ... restituisco esito OK.
030000990921     C                   eval      wrkesito = '0'
030100990910     C                   else
030200010201     C                   if        §ctrokvb > 0
030300990921     C                   eval      wrkesito = '1'
030400000710     C                   else
030500000710     C                   eval      wrkesito = '2'
030600990910     C                   endif
030700000710     C                   endif
030800990910     C*
030900990914     C                   if        %open(tivin00r)
031000990908     C                   close     tivin00r
031100990914     C                   endif
031200021113     C                   if        %open(fivabwwr)
031300021113     C                   close     fivabwwr
031400990914     C                   endif
031500021113     C                   if        %open(fivatwwr)
031600021113     C                   close     fivatwwr
031700010201     C                   endif
031800990910     C*
031900010201     C                   if        §ctrokvb > 0
032000000724     C                             and vlrpoi <> *zeros
032100010202     C                   exsr      invio
032200990920     C                   endif
032300990920     C*
032400910830     C                   ENDSR
032500000613     C***
032600100125
032700100125
032800100125     C*----------------------------------------------------*
032900100125     C*  REPERIMENTO P.O. GESTIONE
033000100125     C*----------------------------------------------------*
033100100125     C     REPFGS        BEGSR
033200100125     C*
033300100125     C* Effettuo considerazioni x elaborazioni "multi-filiale"
033400100125     C                   eval      depfil = VABLNP
033500100125     C                   exsr      repfil
033600100125     C                   if        depfil = invfil
033700100125     C                   if        vlrpoi = 999
033800100125     C                   MOVE(P)   invfil        VABFGS
033900100125     C                   else
034000100125     C                   MOVE(P)   vlrpoi        VABFGS
034100100125     C                   endif
034200100125     C*
034300100125     C                   exsr      PREELA
034400100125     C*
034500100125     C                   endif
034600100125     C*
034700100125     C                   ENDSR
034800010305
034900010305     C*----------------------------------------------------*
035000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035100010305     C*----------------------------------------------------*
035200020305     C     WRIVAB        BEGSR
035300070730     C*
035400071003     C* Considerazioni finali
035500071003     C                   if        VABRMA = *blanks
035600071003     C                   movel(P)  VABRMN        VABRMA
035700071003     C                   endif
035800071003     C*
035900100125     C  N31              write     fivab000                                     => scarico il VAB
036000100125     C*
036100010305     C                   ENDSR
036200990920
036300000801     C*----------------------------------------------------*
036400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036500000801     C*----------------------------------------------------*
036600010201     C     INZVAR        BEGSR
036700000801     C*
036800010201     C                   Z-ADD     *zeros        Num5_0
036900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
037000020725     C                   MOVEL     '0'           FlgCAS            1
037100000801     C*
037200000801     C                   ENDSR
037300000801     C*----------------------------------------------------*
037400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037500000801     C*----------------------------------------------------*
037600020904     C     DEFCAM        BEGSR
037700080923     C*
037800090310     C                   SETOFF                                       50
037900000801     C*
038000021113     C                   CLEAR                   FIVAB000
038100021113     C                   CLEAR                   FIVAT000
038200070730     C* Imposto i valori di default...
038300100125     C                   EVAL      VABCCM = 0430946
038400100125     C                   EVAL      VATCCM = 0430946
038500100125     C                   EVAL      VABLNP = 043
038600100125     C                   EVAL      VATLNP = 043
038700100125     C                   EVAL      VABCTR = 000
038800100125     C                   EVAL      VABCBO = '1'
038900100125     C                   EVAL      VABCTM = *blanks
039000070222     C* ... e poi verifico se sono stati passati come parametri
039100070222     C                   IF        vlrppt > *blanks
039200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
039300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039400070222     C                   EXSR      CHKNUM
039500070222     C                   IF        PiInt=*on
039600070222     C                   Z-ADD     PiVal         VABCCM
039700070222     C                   Z-ADD     PiVal         VATCCM
039800070222     C                   ENDIF
039900070222     C                   ENDIF
040000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040200070222     C                   EXSR      CHKNUM
040300070222     C                   IF        PiInt=*on
040400070222     C                   Z-ADD     PiVal         VABLNP
040500070222     C                   Z-ADD     PiVal         VATLNP
040600070222     C                   ENDIF
040700070222     C                   ENDIF
040800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041000070222     C                   EXSR      CHKNUM
041100070222     C                   IF        PiInt=*on
041200070222     C                   Z-ADD     PiVal         VABCTR
041300070222     C                   ENDIF
041400070928     C                   ENDIF
041500071009     C                   MOVEL     *blanks       wDISK             1
041600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
041700071009     C                   IF        wDISK <> *blanks
041800070928     C                   SETON                                        50
041900070222     C                   ENDIF
042000100125     C                   IF        %subst(vlrppt:15:2) = *blanks
042100100125     C                   EVAL      wLenCollo = *zeros
042200100125     C                   ELSE
042300100125     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:2))
042400100125     C                   EXSR      CHKNUM
042500100125     C                   IF        PiInt=*on
042600100125     C                   Z-ADD     PiVal         wLenCollo
042700100125     C                   ENDIF
042800100125     C                   ENDIF
042900070222     C                   ENDIF
043000071009     C*
043100071009     C   50              EVAL      VABCTM = '7Q'
043200000801     C*
043300000801     C                   ENDSR
043400100125     C*----------------------------------------------------*
043500100125     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043600100125     C*----------------------------------------------------*
043700100125     C     IMPVAB        BEGSR
043800100125     C*
043900100125     C                   SETOFF                                       3132
044000100125     C*
044100100125     C* Reperimento campi ALFA
044200100125     C*
044300100125     C* Considerazioni sulla ragione sociale del destinatario da indicare
044400100125     C                   EVAL      VABRSD=%trim(%subst(vindta:5:40))
044500100125     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044600100125     C     '@':'A'       XLATE     VABRSD        VABRSD
044700100125     C* ==
044800100125     C                   EVAL      VABIND=%trim(%subst(vindta:45:35))
044900100125     C                   EVAL      VABRD2=' Tel.'+%trim(%subst(vindta:80:15))
045000100125     C                   EVAL      VABLOD=%trim(%subst(vindta:100:30))
045100100125     C                   EVAL      VABPRD=%trim(%subst(vindta:130:2))
045200120127     C                   EVAL      VABCAD=%trim(%subst(vindta:95:5))
045300100125     C                   EVAL      VABNZD=*blanks
045400100125     C                   EVAL      VABRMA=%trim(%subst(vindta:307:6))
045500100125     C                   EVAL      VABVCA='EUR'
045600121018     C                   EVAL      VABNOT=%trim(%subst(vindta:319:35))
045700121018     C                   EVAL      VABNT2=%trim(%subst(vindta:319+35:29))
045800100125     C*
045900100125     C                   MOVEL     DATCOR        VABAAS
046000100125     C                   MOVEL     DATCOR        VATAAS
046100100125     C                   MOVE      DATCOR        VABMGS
046200100125     C*
046300100125     C* Reperimento campi NUMERICI
046400100125     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
046500100125     C                   EXSR      CHKNUM
046600100125     C                   IF        PiInt=*on
046700100125     C***                Z-ADD     PiVal         VABNSP
046800100125     C***                Z-ADD     PiVal         VATNSP
046900100125     C                   Z-ADD     PiVal         VABRMN
047000100125     C                   ELSE
047100100125     C                   SETON                                        32
047200100125     C***                Z-ADD     *zeros        VABNSP
047300100125     C***                Z-ADD     *zeros        VATNSP
047400100125     C                   Z-ADD     *zeros        VABRMN
047500100125     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600100125     C                             + ' ' + 'VABRMN'
047700100125     C                   ENDIF
047800100125     C*
047900120127     C***                EVAL      PiStr=%trim(%subst(vindta:95:5))
048000120127     C***                EXSR      CHKNUM
048100120127     C***                IF        PiInt=*on
048200120127     C***                Z-ADD     PiVal         Num5_0
048300120127     C***                MOVEL(p)  Num5_0        VABCAD
048400120127     C***                ELSE
048500120127     C***                SETON                                        32
048600120127     C***                EVAL      VABCAD = *zeros
048700120127     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
048800120127     C***                          + ' ' + 'VABCAD'
048900120127     C***                ENDIF
049000100125     C*
049100100125     C* Reperisco la provincia dal CAP e dalla località
049200100125     C                   IF        VABCAD <> *blanks AND
049300100125     C                             VABPRD  = *blanks AND
049400100125     C                             VABNZD  = *blanks
049500100125     C                   CLEAR                   TISI95DS
049600100125     C                   EVAL      I95TCN = '3'
049700100125     C                   Z-ADD     datcor        I95DAT
049800100125     C                   EVAL      I95CAP = VABCAD
049900100125     C                   EVAL      I95LOC = VABLOD
050000100125     C                   CALL      'TISI95R'
050100100125     C                   PARM                    TISI95DS
050200100125     C                   EVAL      VABPRD = O95PRV
050300100125     C                   ENDIF
050400100125     C*
050500100125     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
050600100125     C                   EXSR      CHKNUM
050700100125     C                   IF        PiInt=*on
050800100125     C                   Z-ADD     PiVal         VABNCL
050900100125     C                   ELSE
051000100125     C                   SETON                                        32
051100100125     C                   Z-ADD     *zeros        VABNCL
051200100125     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300100125     C                             + ' ' + 'VABNCL'
051400100125     C                   ENDIF
051500100125     C*
051600100125     C                   EVAL      PiStr=%trim(%subst(vindta:158:8))
051700100125     C                   EVAL      PiDecChr = ','
051800100125     C                   EXSR      CHKNUM
051900100125     C                   IF        PiNum=*on
052000100125     C                   Z-ADD(H)  PiVal         VABPKB
052100100125     C                   ELSE
052200100125     C                   SETON                                        32
052300100125     C                   Z-ADD     *zeros        VABPKB
052400100125     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500100125     C                             + ' ' + 'VABPKB'
052600100125     C                   ENDIF
052700100125     C*
052800100125     C                   IF        %subst(vindta:268:15) <> *blanks AND
052900100125     C                             %subst(vindta:268:15) <> *all'0'
053000121126     C                   EVAL      VABTIC=%subst(vindta:386:2)
053100100125     C                   EVAL      PiStr=%trim(%subst(vindta:268:15))
053200100125     C* Detrmino che indipendentemente da giusto o sbagliato l'intenzione del cliente era d
053300100125     C* passare il contrassegno
053400100125     C                   MOVEL     '1'           FlgCAS
053500100125     C*
053600100125     C                   EXSR      CHKNUM
053700100125     C                   IF        PiNum=*on
053800100125     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
053900100125     C* riferisce ad un campo 15,3
054000100125     C***                EVAL(H)   VABCAS = PiVal/1000
054100100125     C                   EVAL      VABCAS = PiVal
054200100125     C                   ELSE
054300100125     C                   SETON                                        32
054400100125     C                   Z-ADD     *zeros        VABCAS
054500100125     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600100125     C                             + ' ' + 'VABCAS'
054700100125     C                   ENDIF
054800100125     C                   ENDIF
054900100125     C*
055000100125     C* Considerazioni finali su CBO/CAS
055100100125     C                   IF        FlgCAS = '1'
055200100125     C                   IF        VABCBO = '1'
055300100125     C                   EVAL      VABCBO = '4'
055400100125     C                   ENDIF
055500100125     C                   IF        VABCBO = '2'
055600100125     C                   EVAL      VABCBO = '6'
055700100125     C                   ENDIF
055800100125     C                   ENDIF
055900100125     C*
056000100125     C* Eseguo routine finale x considerazioni specifiche su importi/divise
056100100125     C                   EXSR      CHKIMPDIV
056200100125     C*
056300100125     C* Ebbene...
056400100125     C                   ADD       1             §CTRMO
056500100125     C                   IF        *in31 <> *zeros OR
056600100125     C                             *in32 <> *zeros
056700100125     C                   ADD       1             §CTRNO
056800100125     C                   EVAL      x_vinflg = '2'
056900100125     C                   ELSE
057000100125     C                   ADD       1             §CTROKVB
057100100125     C                   ENDIF
057200100125     C*
057300100125     C                   ENDSR
057400091223     C*----------------------------------------------------*
057500091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
057600091223     C*----------------------------------------------------*
057700091223     C     REPNSP        BEGSR
057800091223     C*
057900091223     C                   EXSR      INZVAR
058000091223     C                   EXSR      DEFCAM
058100091223     C*
058200091223     C* NSP => Stacco un numeratore da AZNUM
058300091223     C                   clear                   TRUL33DS
058400091223     C                   eval      I33OPE = *zeros
058500091223     C                   eval      I33CNU = 302
058600091223     C                   eval      I33NUM = 1
058700091223     C                   movel     TRUL33DS      KPJBU
058800091223     C                   call      'TRUL33R'
058900091223     C                   parm                    KPJBA
059000091223     C                   movel     KPJBU         TRUL33DS
059100091223     C                   if        O33ERR = *zeros
059200091223     C                   z-add     O33NRF        VABNSP
059300091223     C                   z-add     O33NRF        VATNSP
059400091223     C                   else
059500091223     C                   SETON                                        31
059600100125     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059700091223     C                             + ' ' + 'VABNSP VATNSP'
059800091223     C                   endif
059900091223     C*
060000091223     C                   ENDSR
060100010201     C*----------------------------------------------------*
060200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060300010201     C*----------------------------------------------------*
060400071003     C     WRIVAT_B      BEGSR
060500010201     C*
060600021113     C* Valorizzo l buffer di scrittura del FIVAT
060700070928     C* - TIPO RECORD "B"
060800100125     C***                if        %subst(vindta:92:15) <> *blanks
060900100125     C***                eval      VATTRC = 'B'
061000100125     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
061100100125     C***                exsr      wrivat
061200100125     C***                endif
061300010201     C*
061400010201     C                   ENDSR
061500100125     C*----------------------------------------------------*
061600100125     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061700100125     C*----------------------------------------------------*
061800100125     C     WRIVAT_A      BEGSR
061900100125     C*
062000100125     C* Valorizzo l buffer di scrittura del FIVAT
062100100125     C* - TIPO RECORD "A"
062200100125     C***                if        %subst(vindta:92:15) <> *blanks
062300100125     C***                eval      VATTRC = 'A'
062400100125     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
062500100125     C***                exsr      wrivat
062600100125     C***                endif
062700100125     C*
062800100125     C                   ENDSR
062900071003     C*----------------------------------------------------*
063000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063100071003     C*----------------------------------------------------*
063200071003     C     WRIVAT_E      BEGSR
063300100125     C*
063400100125     C                   eval      VATTRC = 'E'
063500100125     C*
063600100125     C* Gestisco le 2 modalità d scrittura "CHI SONO"
063700100125     C*
063800100125     C* ...1° modalità (classica) => "CHI SONO" esplicito
063900100125     C                   if        wLenCollo = *zeros
064000100125     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
064100100125     C                   exsr      wrivat
064200100125     C*
064300100125     C                   else
064400100125     C*
064500100125     C* ...2° modalità (automatica) => "CHI SONO" sviluppati in relazione al numero colli
064600100125     C*
064700100125     C* Inizializzo progressivo collo su totale colli della spedizione
064800100125     C                   Z-ADD     1             wNumColloDA
064900100125     C*
065000100125     C                   DOW       wNumColloDA <= VABNCL
065100100125     C                   MOVEL(P)  wNumColloDA   wNumColloCor
065200120127     C                   EVAL      VATNOT = %subst(vindta:313:6) +
065300100125     C                                  %subst(wNumColloCor:
065400100125     C                                  %len(%trim(wNumColloCor))-wLenCollo+1:
065500100125     C                                  wLenCollo)
065600100125     C                   EVAL      wNumColloDA = wNumColloDA + 1
065700100125     C                   exsr      wrivat
065800100125     C                   add       1             §CTROKVT
065900100125     C                   ENDDO
066000100125     C*
066100100125     C                   endif
066200071003     C*
066300071003     C                   ENDSR
066400100125     C*----------------------------------------------------*
066500100125     C*  SCARICO BUFFER RECORD FIVAT00F
066600100125     C*----------------------------------------------------*
066700100125     C     WRIVAT        BEGSR
066800100125     C*
066900100125     C                   eval      vatFGS = vabFGS
067000100125     C*
067100100125     C                   if        vatNOT <> *blanks
067200100125     C                   write     fivat000
067300100125     C                   endif
067400100125     C*
067500100125     C                   ENDSR
067600020904
067700020904
067800010202     C*----------------------------------------------------*
067900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
068000010202     C*----------------------------------------------------*
068100020305     C     PREVAT        BEGSR
068200010202     C*
068300021113     C* Compongo il nome del membro da dare al FIVATWWR
068400010202     C                   eval      parmbr = vlrhdl
068500010202     C                   movel     'M'           parmbr
068600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068700010202     C                   eval      paropz = '1'
068800010202     C* Effettuo la chiamata al CLLE preposto
068900050128     C                   call(e)   'TITVVTC'
069000010202     C                   parm                    parccm
069100010202     C                   parm                    parmbr
069200010202     C                   parm                    paropz
069300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069400010202     C                   if        %error
069500010202     C                   movel     '1'           chkcall
069600010202     C                   else
069700010202     C                   movel     '0'           chkcall
069800010202     C                   endif
069900010202     C*
070000010202     C                   ENDSR
070100000801     C*----------------------------------------------------*
070200000801     C*  CONTROLLO NUMERICITA' CAMPI
070300000801     C*----------------------------------------------------*
070400000801     C     CHKNUM        BEGSR
070500081113     C*
070600081113     C                   IF        PiDecChr = *blanks
070700091223     C                   EVAL      PiDecChr = ','
070800081113     C                   ENDIF
070900091223     C*
071000081113     C                   callp(e)  UBISNUM_Check(PiStr
071100081113     C                                          :PiDecChr
071200081113     C                                          :PiVal
071300081113     C                                          :PiNum
071400081113     C                                          :PiInt)
071500081113     C*
071600000801     C                   IF        %error
071700000801     C                   EVAL      PiInt=*off
071800000801     C                   ENDIF
071900000801     C*
072000000801     C                   ENDSR
072100000801     C***
072200000801
072300011113
072400011113     C*----------------------------------------------------*
072500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
072600011113     C*----------------------------------------------------*
072700011113     C     CHKIMPDIV     BEGSR
072800011113     C*
072900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
073000011113     C                   Z-ADD     *zeros        wrkDec            9 9
073100011113     C*
073200011113     C* Come prima cosa effettuo considerazioni sulla divisa
073300011113     C                   IF        vabIAS > *zeros
073400011113     C                   IF        vabVAS <> 'EUR'
073500011113     C                   EVAL      vabVAS =  'ITL'
073600011113     C                   ENDIF
073700011113     C                   ENDIF
073800011113     C*
073900011113     C                   IF        vabCAS > *zeros
074000011113     C                   IF        vabVCA <> 'EUR'
074100011113     C                   EVAL      vabVCA =  'ITL'
074200011113     C                   ENDIF
074300011113     C                   ENDIF
074400011113     C*
074500011113     C                   IF        vabVMD > *zeros
074600020305     C                   IF        vabVAD <> 'EUR'
074700011113     C                   EVAL      vabVAD =  'ITL'
074800011113     C                   ENDIF
074900011113     C                   ENDIF
075000011113     C*
075100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
075200011113     C                   Z-ADD     vabIAS        wrkDec
075300011113     C                   IF        wrkDec > *zeros
075400011113     C                   IF        vabVAS = 'ITL'
075500011113     C                   EVAL      vabIAS = *zeros
075600011113     C                   ENDIF
075700011113     C                   ENDIF
075800011113     C*
075900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
076000011113     C                   Z-ADD     vabCAS        wrkDec
076100011113     C                   IF        wrkDec > *zeros
076200011113     C                   IF        vabVCA = 'ITL'
076300011113     C                   EVAL      vabCAS = *zeros
076400011113     C                   ENDIF
076500011113     C                   ENDIF
076600011113     C*
076700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
076800011113     C                   Z-ADD     vabVMD        wrkDec
076900011113     C                   IF        wrkDec > *zeros
077000011113     C                   IF        vabVAD = 'ITL'
077100011113     C                   EVAL      vabVMD = *zeros
077200011113     C                   ENDIF
077300011113     C                   ENDIF
077400011113     C*
077500011113     C                   ENDSR
077600011113     C***
077700011113
077800011113
077900000801
078000000801
078100990920      /TITLE Invio dei dati al punto operativo.
078200010202     C     invio         BEGSR
078300990920     C*
078400021113     C* 1° invio FIVAT
078500010201     C                   reset                   dscmz
078600010201     C                   move      vlrpoi        cmzdst
078700021113     C                   eval      cmzfld = 'FIVATWWR'
078800010201     C                   eval      cmzmbd = vlrhdl
078900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079000021009     C***                if        prmfir = *blanks
079100021113     C                   eval      cmzfla = 'FIVAT00F'
079200021113     C                   eval      cmzmba = 'FIVAT00F'
079300021009     C***                else
079400021009     C***                eval      cmzfla = prmfir
079500021009     C***                eval      cmzmba = prmfir
079600021009     C***                endif
079700010201     C                   eval      cmznrr = *zeros
079800020305     C                   move      §ctrokvt      cmznrr
079900021018     C                   eval      cmzlba = vlrfl1
080000010201     C                   call(e)   'TIS711C'
080100010201     C                   parm                    dscmz
080200010201     C                   parm      *blanks       esito
080300010205     C                   if        %error
080400010205     C                             or cmzerr = '1'
080500010205     C                             or esito  = '1'
080600010205     C                   eval      wrkesito = '3'
080700010205     C                   else
080800010201     C*
080900021113     C* 2° invio FIVAB
081000010201     C                   reset                   dscmz
081100010201     C                   move      vlrpoi        cmzdst
081200010201     C                   eval      cmzfld = vlrfou
081300010201     C                   eval      cmzmbd = vlrhdl
081400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081500021009     C***                if        prmfir = *blanks
081600021113     C                   eval      cmzfla = 'FIVAB00F'
081700021113     C                   eval      cmzmba = 'FIVAB00F'
081800021009     C***                else
081900021009     C***                eval      cmzfla = prmfir
082000021009     C***                eval      cmzmba = prmfir
082100021009     C***                endif
082200010201     C                   eval      cmznrr = *zeros
082300010201     C                   move      §ctrokvb      cmznrr
082400021018     C                   eval      cmzlba = vlrfl1
082500010201     C                   call(e)   'TIS711C'
082600010201     C                   parm                    dscmz
082700010201     C                   parm      *blanks       esito
082800010201     C                   if        %error
082900010201     C                             or cmzerr = '1'
083000010201     C                             or esito  = '1'
083100010201     C                   eval      wrkesito = '3'
083200010201     C                   endif
083300010205     C                   endif
083400990920     C*
083500000613     C                   ENDSR
083600000613     C***
083700070411
083800120131     C     *pssr         BEGSR
083900070411     C*
084000120131     C                   if        %open(tivin00r)
084100120131     C                   close     tivin00r
084200120131     C                   endif
084300120131     C                   if        %open(fivabwwr)
084400120131     C                   close     fivabwwr
084500120131     C                   endif
084600120131     C                   if        %open(fivatwwr)
084700120131     C                   close     fivatwwr
084800120131     C                   endif
084900070411     C*
085000070411     C* Effettuo la chiamata al CLLE preposto
085100120131     C                   call(e)   'TITVVTC'
085200120131     C                   parm                    parccm
085300120131     C                   parm                    parmbr
085400120131     C                   parm      '2'           paropz
085500070411     C*
085600120131     C                   eval      wrkesito = '2'
085700070411     C*
085800120131     C                   seton                                        LR
085900070411     C*
086000120131     C                   ENDSR     '*CANCL'
086100070411     C***
086200070411
086300990910
086400000613     C     *inzsr        BEGSR
086500990910     C*
086600990910     C     *entry        plist
086700990920     C                   parm                    tivlrds
086800990921     C                   parm      wrkesito      esito
086900000724     C                   parm                    prmlit
087000000710     C                   parm                    prmfir
087100000613     C*
087200000830     C* CALCOLA LA DATA CORRENTE
087300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087400091223     C                   eval      datcor = %dec(%date() : *ISO)
087500000830     C*
087600000613     C                   ENDSR
087700000613     C***
