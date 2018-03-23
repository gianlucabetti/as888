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
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000110728     D depspe          s              7    INZ(*blanks)
002100010213     D depdat          s              8  0 INZ(*zeros)
002200010213     D depaas          s              4    INZ(*zeros)
002300110728     D curspe          s              7    INZ(*blanks)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002900000801
003000010213     D                 DS                  INZ
003100010213     D  aaaa                   1      4  0
003200010213     D  mmgg                   5      8  0
003300010213     D  mm                     5      6  0
003400010213     D  gg                     7      8  0
003500010213     D  datawrk                1      8  0
003600010213
003700000801     D  Num5_0         s              5  0
003800020904     D  SkChkDgt       s              1    DIM(35)
003900000830
004000000830     D*------------------
004100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200000830     D*------------------
004300000830     D WLBDA8          DS                  INZ
004400000830     D  G08DAT                 1      8  0
004500000830     D  G08INV                 9     16  0
004600000830     D  G08ERR                17     17
004700000830     D  G08TGI                18     22  0
004800000830     D*
004900020725
005000020725     D*------------------
005100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005200020725     D*------------------
005300110728     D PERLAGLPDS      DS                  INZ
005400110728     D  VINDTA                     2048
005500110728     D  VINFLG                        1
005600110728     D  VINSPE                        7
005700110728     D  VINRRN                        8  0
005800020725     D*
005900990908
006000010201
006100010201
006200120309     C*
006300120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400120309     C
006500120309     C/EXEC SQL
006600120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700120309     C/END-EXEC
006800000913     C                   reset                   rrnum
006900990921     C                   reset                   esito
007000990921     C                   reset                   wrkesito
007100000613     C*
007200020724     C                   EXSR      RWGLP                                        LETT/SCR. VAB
007300000613     C*
007400010202     C* Effettuo la chiamata al CLLE preposto
007500050128     C                   call(e)   'TITVVTC'
007600010202     C                   parm                    parccm
007700010202     C                   parm                    parmbr
007800010202     C                   parm      '2'           paropz
007900000616     C*
008000000801     C
008100010201     C                   seton                                        LR
008200990908
008300000801
008400910830     C*--------------------------------------------------------
008500021113     C* RWGLP    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008600910830     C*--------------------------------------------------------
008700020724     C     RWGLP         BEGSR
008800990910     C*
008900990914     C                   if        not %open(tivin00r)
009000990908     C                   open      tivin00r
009100990914     C                   endif
009200021113     C                   if        not %open(fivabwwr)
009300021113     C                   open      fivabwwr
009400990914     C                   endif
009500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009600020305     C                   exsr      prevat
009700010201     C*
009800010202     C                   if        chkcall = '0'
009900010202     C*
010000021113     C                   if        not %open(fivatwwr)
010100021113     C                   open      fivatwwr
010200010201     C                   endif
010300990910     C*
010400010201     C                   clear                   §CTROKVB          5 0
010500020305     C                   clear                   §CTROKVT          5 0
010600000801     C                   clear                   §CTRMO            5 0
010700000801     C                   clear                   §CTRNO            5 0
010800990910     C*
010900020725     C*
011000020725     C/EXEC SQL
011100020725     C+ declare C1 cursor for select
011200110728     C+ vindta, vinflg, substr(vindta, 51, 7) as sped, rrn(tivin00r)
011300060223     C+ from tivin00r
011400060223     C+ where substr(vindta, 262, 23) <> ' '
011500060223     C+ order by sped
011600020725     C+ for read only
011700020725     C/END-EXEC
011800020725     C
011900020725     C/EXEC SQL
012000020725     C+ open C1
012100020725     C/END-EXEC
012200020725     C
012300020725     C/EXEC SQL
012400020725     C+ Fetch C1 into :PERLAGLPDS
012500020725     C/END-EXEC
012600020725     C*
012700020725     C                   dow       sqlcod = *zeros
012800990913     C*
012900020725     C                   if        vindta > *blanks
013000000613     C                   add       1             rrnum
013100000801     C*
013200020725     C                   if        vinflg = *blanks
013300020725     C                             or vinflg = '0'
013400020725     C                             or vinflg = '2'
013500000801     C*
013600020725     C                   clear                   x_vinmsg
013700020725     C                   eval      x_vinflg = '1'
013800010305     C*
013900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014000110728     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
014100020305     C                   MOVEL(p)  PiStr         curspe
014200010305     C*
014300010305     C                   if        depspe = *blanks                             => 1° giro
014400010305     C                   eval      depspe = curspe                              => memorizz. spediz
014500020725     C                   clear                   tivinds
014600020725     C                   exsr      inzvar
014700020725     C                   exsr      defcam
014800020305     C                   exsr      impvab
014900020305     C                   exsr      wrivat                                       => carico VAT
015000010305     C                   else
015100020725     C                   if        depspe <> curspe                             => rottura di spediz
015200010305     C                   eval      depspe = curspe                              => memorizz. spediz
015300020305     C                   exsr      wrivab
015400020725     C                   clear                   tivinds
015500020725     C                   exsr      inzvar
015600020725     C                   exsr      defcam
015700020305     C                   exsr      impvab
015800020305     C                   exsr      wrivat                                       => carico VAT
015900020305     C                   else                                                   => x stessa spediz
016000020305     C                   exsr      impvab
016100020305     C                   exsr      wrivat                                       => carico VAT
016200010305     C                   endif
016300010305     C                   endif
016400010305     C                   endif
016500000905     C*
016600000905     C                   else
016700020725     C                   eval      x_vinflg = '1'
016800000905     C                   endif
016900000905     C*
017000020725     C     VINRRN        chain     tivin000
017100020725     C                   if        %found(tivin00r)
017200020725     C                   eval      y_vinflg = x_vinflg
017300020725     C                   eval      y_vinmsg = x_vinmsg
017400020725     C                   update    tivin000
017500020725     C                   endif
017600020725     C*
017700020725     C/EXEC SQL
017800020725     C+ Fetch C1 into :PERLAGLPDS
017900020725     C/END-EXEC
018000020725     C*
018100020725     C                   enddo
018200020725     C*
018300020725     C/EXEC SQL
018400020725     C+ close C1
018500020725     C/END-EXEC
018600000905     C*
018700020305     C* Scarico i VAB rimasti "in sospeso"
018800020305     C                   exsr      wrivab
018900010202     C*
019000010202     C                   endif
019100990910
019200990910     C* Se non ci sono record con errori ...
019300000710     C                   if        §ctrno = 0
019400990910     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990910     C                   else
019700010201     C                   if        §ctrokvb > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000000710     C                   eval      wrkesito = '2'
020100990910     C                   endif
020200000710     C                   endif
020300990910     C*
020400990914     C                   if        %open(tivin00r)
020500990908     C                   close     tivin00r
020600990914     C                   endif
020700021113     C                   if        %open(fivabwwr)
020800021113     C                   close     fivabwwr
020900990914     C                   endif
021000021113     C                   if        %open(fivatwwr)
021100021113     C                   close     fivatwwr
021200010201     C                   endif
021300990910     C*
021400010201     C                   if        §ctrokvb > 0
021500000724     C                             and vlrpoi <> *zeros
021600010202     C                   exsr      invio
021700990920     C                   endif
021800990920     C*
021900910830     C                   ENDSR
022000000613     C***
022100010305
022200010305     C*----------------------------------------------------*
022300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022400010305     C*----------------------------------------------------*
022500020305     C     WRIVAB        BEGSR
022600010305     C*
022700021113     C                   write     fivab000                                     => scarico il VAB
022800010305     C*
022900010305     C                   ENDSR
023000990920
023100000801     C*----------------------------------------------------*
023200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023300000801     C*----------------------------------------------------*
023400010201     C     INZVAR        BEGSR
023500000801     C*
023600010201     C                   Z-ADD     *zeros        Num5_0
023700020725     C                   MOVEL     *blanks       WrkAAS            4
023800020725     C                   MOVEL     *blanks       WrkMGS            4
023900020725     C                   Z-ADD     *zeros        CntDDT            7 0
024000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024100020725     C                   MOVEL     '0'           FlgCAS            1
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024600000801     C*----------------------------------------------------*
024700020904     C     DEFCAM        BEGSR
024800000801     C*
024900021113     C                   CLEAR                   FIVAB000
025000021113     C                   CLEAR                   FIVAT000
025100020801     C                   Z-ADD     0011658       VABCCM
025200020801     C                   Z-ADD     0011658       VATCCM
025300020725     C                   Z-ADD     001           VABLNP
025400020725     C                   Z-ADD     001           VATLNP
025500010201     C                   Z-ADD     000           VABCTR
025600020305     C                   MOVEL     '7Q'          VABCTM
025700070222     C                   MOVEL     'C'           VABTSP
025800020305     C                   MOVEL     'E'           VATTRC
025900070222     C* ... e poi verifico se sono stati passati come parametri
026000070222     C                   IF        vlrppt > *blanks
026100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
026200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026300070222     C                   EXSR      CHKNUM
026400070222     C                   IF        PiInt=*on
026500070222     C                   Z-ADD     PiVal         VABCCM
026600070222     C                   Z-ADD     PiVal         VATCCM
026700070222     C                   ENDIF
026800070222     C                   ENDIF
026900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
027000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027100070222     C                   EXSR      CHKNUM
027200070222     C                   IF        PiInt=*on
027300070222     C                   Z-ADD     PiVal         VABLNP
027400070222     C                   Z-ADD     PiVal         VATLNP
027500070222     C                   ENDIF
027600070222     C                   ENDIF
027700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
027800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027900070222     C                   EXSR      CHKNUM
028000070222     C                   IF        PiInt=*on
028100070222     C                   Z-ADD     PiVal         VABCTR
028200070222     C                   ENDIF
028300070222     C                   ENDIF
028400070222     C                   ENDIF
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028900000801     C*----------------------------------------------------*
029000010201     C     IMPVAB        BEGSR
029100010305     C*
029200000801     C                   Z-ADD     *zeros        errore            1 0
029300021113     C*
029400021113     C                   MOVE(P)   vlrpoi        VABFGS
029500021113     C                   MOVE(P)   vlrpoi        VATFGS
029600020725     C*
029700020725     C* Campi anno e mese/giorno
029800020725     C                   EVAL      WrkAAS=%subst(vindta:24:4)
029900020725     C                   MOVEL(P)  WrkAAS        VABAAS
030000020725     C                   MOVEL(P)  WrkAAS        VATAAS
030100020725     C                   EVAL      WrkMGS=%subst(vindta:29:2) +
030200020725     C                                    %subst(vindta:32:2)
030300020725     C                   MOVEL(P)  WrkMGS        VABMGS
030400020725     C*
030500020725     C* Campi relativi al destinatario
030600020725     C                   EVAL      VABRSD=%trim(%subst(vindta:81:35))
030700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030900020117     C* ==
031000020725     C                   EVAL      VABIND=%trim(%subst(vindta:116:35))
031100020725     C                   EVAL      VABLOD=%trim(%subst(vindta:151:35))
031200020725     C                   EVAL      VABNZD=%trim(%subst(vindta:196:4))
031300020725     C                   IF        %trim(VABNZD) = 'I'
031400020725     C                   EVAL      VABNZD = *blanks
031500020725     C                   ENDIF
031600020725     C                   EVAL      VABPRD=%trim(%subst(vindta:203:3))
031700020725     C                   EVAL      VABRMA=%trim(%subst(vindta:309:10))
031800050103     C                   EVAL      VABNT2=%trim(%subst(vindta:329:25))
031900020725     C*
032000020725     C* Altri campi alfa "costruiti"
032100020725     C                   EVAL      VABRMO=%subst(vindta:11:3) +
032200020725     C                                    %subst(vindta:14:7) +
032300020904     C                                    %subst(vindta:21:3) +
032400020904     C                                    %subst(vindta:312:7)
032500020725     C                   IF        %subst(vindta:306:1) = 'E'
032600020725     C                   EVAL      VABTSP='E'
032700020725     C                   ENDIF
032800020725     C                   EVAL      VABVCA=%trim(%subst(vindta:302:4))
032900020725     C*
033000020725     C* Effettuo il conteggio dei DDT x spedizione per indicare eventualmente nelle note
033100020725     C                   IF        %subst(vindta:307:1) = 'S'
033200020725     C                   ADD       1             CntDDT
033300020725     C                   ENDIF
033400020725     C                   IF        CntDDT > 1
033500020725     C                   EVAL      VABNOT = 'NUMERO DI DDT NELLA SPEDIZIONE: ' +
033600020725     C                                      %editc(CntDDT:'1')
033700020725     C                   ENDIF
033800050103     C*
033900050103     C* Effettuo considerazioni relative alle consegne particolari
034000050103     C                   IF        %subst(vindta:308:1) = 'G'
034100050103     C                   EVAL      VABTC1 = 'S'
034200050103     C                   ENDIF
034300050103     C                   IF        %subst(vindta:308:1) = 'P'
034400050103     C                   EVAL      VABTC1 = 'A'
034500050103     C                   ENDIF
034600050103     C                   IF        %subst(vindta:308:1) = 'S'
034700050103     C                   EVAL      VABTC1 = 'P'
034800050103     C                   ENDIF
034900020725     C*
035000020725     C* Campi numerici
035100020725     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
035200000801     C                   EXSR      CHKNUM
035300000801     C                   IF        PiInt=*on
035400000801     C                   Z-ADD     PiVal         Num5_0
035500010208     C                   MOVEL(p)  Num5_0        VABCAD
035600000801     C                   ELSE
035700000801     C                   ADD       1             errore
035800020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035900000801     C                             + ' ' + 'VABCAD'
036000000801     C                   ENDIF
036100020725     C*
036200020725     C                   EVAL      PiStr=%trim(%subst(vindta:235:7))
036300010201     C                   EXSR      CHKNUM
036400010201     C                   IF        PiInt=*on
036500020725     C                   ADD       PiVal         VABNCL
036600010201     C                   ELSE
036700010201     C                   ADD       1             errore
036800020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036900010201     C                             + ' ' + 'VABNCL'
037000010201     C                   ENDIF
037100020305     C*
037200020725     C                   EVAL      PiStr=%trim(%subst(vindta:251:11))
037300010201     C                   EXSR      CHKNUM
037400010201     C                   IF        PiNum=*on
037500020725     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
037600020725     C* riferisce ad un campo 11,3
037700020725     C                   EVAL(H)   VABPKB = VABPKB + (PiVal/1000)
037800010201     C                   ELSE
037900010201     C                   ADD       1             errore
038000020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038100010201     C                             + ' ' + 'VABPKB'
038200010201     C                   ENDIF
038300020305     C*
038400020725     C                   EVAL      PiStr=%trim(%subst(vindta:242:9))
038500010201     C                   EXSR      CHKNUM
038600010201     C                   IF        PiNum=*on
038700030305     C*
038800030305     C* Verifico (visti i precedenti) che il vol. ottenuto dal fle del cliente nn superi i 99,999 M3
038900030305     C                   IF        PiVal <= 99999
039000020725     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
039100020725     C* riferisce ad un campo 9,3
039200020725     C                   EVAL(H)   VABVLB = VABVLB + (PiVal/1000)
039300010201     C                   ELSE
039400010201     C                   ADD       1             errore
039500020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600010201     C                             + ' ' + 'VABVLB'
039700010202     C                   ENDIF
039800030305     C                   ELSE
039900030305     C                   ADD       1             errore
040000030305     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100030305     C                             + ' ' + 'VABVLB'
040200030305     C                   ENDIF
040300020305     C*
040400110728     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
040500010201     C                   EXSR      CHKNUM
040600010201     C                   IF        PiInt=*on
040700011115     C                   Z-ADD     PiVal         VABNSP
040800020305     C                   Z-ADD     PiVal         VATNSP
040900010201     C                   ELSE
041000010201     C                   ADD       1             errore
041100020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041200050112     C                             + ' ' + 'VABNSP VATNSP'
041300010201     C                   ENDIF
041400020904     C*
041500050112     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
041600050112     C                   EXSR      CHKNUM
041700050112     C                   IF        PiInt=*on
041800050112     C                   Z-ADD     PiVal         VABRMN
041900050112     C                   ELSE
042000050112     C                   ADD       1             errore
042100050112     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200050112     C                             + ' ' + 'VABRMN'
042300050112     C                   ENDIF
042400020725     C*
042500020725     C                   EVAL      PiStr=%trim(%subst(vindta:285:17))
042600020725     C* Detrmino se indipendentemente da giusto o sbagliato l'intenzione del cliente era d
042700020725     C* passare il contrassegno
042800020725     C                   IF        PiStr > *zeros
042900020725     C                   MOVEL     '1'           FlgCAS
043000020725     C                   ENDIF
043100020725     C*
043200020725     C                   EXSR      CHKNUM
043300020725     C                   IF        PiNum=*on
043400020725     C* Divido x 100000 perchè il campo di input non gestisce separatori decimali ma il valore si
043500020725     C* riferisce ad un campo 17,5
043600020725     C                   EVAL(H)   VABCAS = VABCAS + (PiVal/100000)
043700020725     C                   ELSE
043800020725     C                   SETON                                        32
043900020725     C                   Z-ADD     *zeros        VABCAS
044000020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044100020725     C                             + ' ' + 'VABCAS'
044200020725     C                   ENDIF
044300010205     C*
044400020725     C* Considerazioni sul contenuto di campi precedentemente valorizzati in relazione
044500020725     C* al tipo porto ed al contrassegno => x stabilire il tipo bolla
044600020725     C                   IF        %trim(%subst(vindta:233:2)) = '01' AND
044700020725     C                             FlgCAS = '0'
044800020904     C                   EVAL      VABCBO = '2'
044900020725     C                   ENDIF
045000020725     C                   IF        %trim(%subst(vindta:233:2)) = '01' AND
045100020725     C                             FlgCAS = '1'
045200020904     C                   EVAL      VABCBO = '6'
045300020725     C                   ENDIF
045400020725     C                   IF        %trim(%subst(vindta:233:2)) = '02' AND
045500020725     C                             FlgCAS = '0'
045600020904     C                   EVAL      VABCBO = '1'
045700020725     C                   ENDIF
045800020904     C                   IF        %trim(%subst(vindta:233:2)) = '02' AND
045900020725     C                             FlgCAS = '1'
046000020904     C                   EVAL      VABCBO = '4'
046100020725     C                   ENDIF
046200020725     C                   IF        %trim(%subst(vindta:233:2)) = '74'
046300020725     C                   EVAL      VABCBO = 'A'
046400020725     C                   ENDIF
046500020305     C*
046600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046700011113     C                   EXSR      CHKIMPDIV
046800010202     C*
046900000801     C* Ebbene...
047000000801     C                   ADD       1             §CTRMO
047100010201     C                   IF        errore <> *zeros
047200000801     C                   ADD       1             §CTRNO
047300020725     C                   EVAL      x_vinflg = '2'
047400000801     C                   ELSE
047500010201     C                   ADD       1             §CTROKVB
047600000801     C                   ENDIF
047700000801     C*
047800000801     C                   ENDSR
047900010201     C*----------------------------------------------------*
048000021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
048100010201     C*----------------------------------------------------*
048200020305     C     WRIVAT        BEGSR
048300010201     C*
048400021113     C* Valorizzo l buffer di scrittura del FIVAT
048500020725     C                   if        %subst(vindta:262:23) <> *blanks
048600020725     C                   eval      VATNOT = %trim(%subst(vindta:262:23))
048700040916     C* X alcuni barcode si rende necessario calcolare il check-digits (solo quelli numerici)
048800040916     C***                if        %subst(%trim(VATNOT):1:3) <> 'DAL'
048900040916     C                   eval      PiStr=%subst(VATNOT:1:3)
049000040916     C                   exsr      CHKNUM
049100040916     C                   if        PiInt=*on
049200020904     C                   exsr      CALCHKDGT
049300020904     C                   eval      VATNOT = %trim(VATNOT) + WrkChkDgtAlfa
049400020912     C                   endif
049500021113     C                   write     FIVAT000
049600020725     C                   else
049700020725     C                   eval      VABCTM = *blanks
049800020725     C                   endif
049900010201     C*
050000010201     C                   ENDSR
050100020904
050200020904
050300020904     C*--------------------------------------------------------
050400020904     C* CALCHKDGT - ROUTINE DI CALCOLO CHECK-DIGITS (MODULO 10)
050500020904     C*--------------------------------------------------------
050600020904     C     CALCHKDGT     BEGSR
050700020904     C*
050800020904     C                   CLEAR                   SkChkDgt
050900020904     C                   MOVEA     VATNOT        SkChkDgt
051000020904     C* Inizializzo variabili di wrk
051100020904     C                   Z-ADD     *zeros        WrkChkDgt         1 0
051200020904     C                   Z-ADD     *zeros        WrkChkDgtNum      1 0
051300020904     C                   MOVEL     *blanks       WrkChkDgtAlfa     1
051400020904     C                   Z-ADD     *zeros        SommaPari         5 0
051500020904     C                   Z-ADD     *zeros        SommaDispari      5 0
051600020904     C                   Z-ADD     *zeros        SommaTot          5 0
051700020904     C                   Z-ADD     *zeros        Risultato         3 1
051800020904     C*
051900020904     C* Effettuo il calcolo
052000020904     C     1             DO        35            I                 2 0
052100020904     C                   IF        SkChkDgt(I) <> *blanks
052200020904     C* Stabilisco se trattasi di cifra in posizione pari o dispari
052300020904     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
052400020904     C                   EVAL      Risultato = I/2
052500020904     C                   Z-ADD     Risultato     RestoDec          1 1
052600020904     C                   IF        RestoDec = *zeros                            * è pari
052700020904     C                   ADD       WrkChkDgt     SommaPari
052800020904     C                   ELSE                                                   * è dispari
052900020904     C                   ADD       WrkChkDgt     SommaDispari
053000020904     C                   ENDIF
053100020904     C                   ENDIF
053200020904     C                   ENDDO
053300020904     C*
053400020904     C                   EVAL      SommaTot = (SommaDispari * 3) + SommaPari
053500020904     C                   Z-ADD     *zeros        Multiplo10        5 0
053600020904     C                   DOW       Multiplo10 < SommaTot
053700020904     C                   EVAL      Multiplo10 = Multiplo10 + 10
053800020904     C                   ENDDO
053900020904     C*
054000020904     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
054100020904     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
054200020904     C*
054300020904     C                   ENDSR
054400020904     C***
054500020904
054600020904
054700010202     C*----------------------------------------------------*
054800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054900010202     C*----------------------------------------------------*
055000020305     C     PREVAT        BEGSR
055100010202     C*
055200021113     C* Compongo il nome del membro da dare al FIVATWWR
055300010202     C                   eval      parmbr = vlrhdl
055400010202     C                   movel     'M'           parmbr
055500020801     C                   eval      parccm = '00011658'
055600010202     C                   eval      paropz = '1'
055700010202     C* Effettuo la chiamata al CLLE preposto
055800050128     C                   call(e)   'TITVVTC'
055900010202     C                   parm                    parccm
056000010202     C                   parm                    parmbr
056100010202     C                   parm                    paropz
056200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056300010202     C                   if        %error
056400010202     C                   movel     '1'           chkcall
056500010202     C                   else
056600010202     C                   movel     '0'           chkcall
056700010202     C                   endif
056800010202     C*
056900010202     C                   ENDSR
057000000801     C*----------------------------------------------------*
057100000801     C*  CONTROLLO NUMERICITA' CAMPI
057200000801     C*----------------------------------------------------*
057300000801     C     CHKNUM        BEGSR
057400000801     C*
057500000801     C                   call(e)   'ISNUMERIC'
057600000801     C                   PARM                    PiStr            30
057700000801     C                   PARM      ','           PiDecChr          1
057800000801     C                   PARM      *ZEROS        PiVal            30 9
057900000801     C                   PARM      '0'           PiInt             1
058000000801     C                   PARM      '0'           PiNum             1
058100000801     C                   IF        %error
058200000801     C                   EVAL      PiInt=*off
058300000801     C                   ENDIF
058400000801     C*
058500000801     C                   ENDSR
058600000801     C***
058700000801
058800011113
058900011113     C*----------------------------------------------------*
059000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059100011113     C*----------------------------------------------------*
059200011113     C     CHKIMPDIV     BEGSR
059300011113     C*
059400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059500011113     C                   Z-ADD     *zeros        wrkDec            9 9
059600011113     C*
059700011113     C* Come prima cosa effettuo considerazioni sulla divisa
059800011113     C                   IF        vabIAS > *zeros
059900011113     C                   IF        vabVAS <> 'EUR'
060000011113     C                   EVAL      vabVAS =  'ITL'
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C                   IF        vabCAS > *zeros
060500011113     C                   IF        vabVCA <> 'EUR'
060600011113     C                   EVAL      vabVCA =  'ITL'
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C                   IF        vabVMD > *zeros
061100020305     C                   IF        vabVAD <> 'EUR'
061200011113     C                   EVAL      vabVAD =  'ITL'
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061700011113     C                   Z-ADD     vabIAS        wrkDec
061800011113     C                   IF        wrkDec > *zeros
061900011113     C                   IF        vabVAS = 'ITL'
062000011113     C                   EVAL      vabIAS = *zeros
062100011113     C                   ENDIF
062200011113     C                   ENDIF
062300011113     C*
062400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062500011113     C                   Z-ADD     vabCAS        wrkDec
062600011113     C                   IF        wrkDec > *zeros
062700011113     C                   IF        vabVCA = 'ITL'
062800011113     C                   EVAL      vabCAS = *zeros
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063300011113     C                   Z-ADD     vabVMD        wrkDec
063400011113     C                   IF        wrkDec > *zeros
063500011113     C                   IF        vabVAD = 'ITL'
063600011113     C                   EVAL      vabVMD = *zeros
063700011113     C                   ENDIF
063800011113     C                   ENDIF
063900011113     C*
064000011113     C                   ENDSR
064100011113     C***
064200011113
064300011113
064400000801
064500000801
064600990920      /TITLE Invio dei dati al punto operativo.
064700010202     C     invio         BEGSR
064800990920     C*
064900021113     C* 1° invio FIVAT
065000010201     C                   reset                   dscmz
065100010201     C                   move      vlrpoi        cmzdst
065200021113     C                   eval      cmzfld = 'FIVATWWR'
065300010201     C                   eval      cmzmbd = vlrhdl
065400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065500021009     C***                if        prmfir = *blanks
065600021113     C                   eval      cmzfla = 'FIVAT00F'
065700021113     C                   eval      cmzmba = 'FIVAT00F'
065800021009     C***                else
065900021009     C***                eval      cmzfla = prmfir
066000021009     C***                eval      cmzmba = prmfir
066100021009     C***                endif
066200010201     C                   eval      cmznrr = *zeros
066300020305     C                   move      §ctrokvt      cmznrr
066400021018     C                   eval      cmzlba = vlrfl1
066500010201     C                   call(e)   'TIS711C'
066600010201     C                   parm                    dscmz
066700010201     C                   parm      *blanks       esito
066800010205     C                   if        %error
066900010205     C                             or cmzerr = '1'
067000010205     C                             or esito  = '1'
067100010205     C                   eval      wrkesito = '3'
067200010205     C                   else
067300010201     C*
067400021113     C* 2° invio FIVAB
067500010201     C                   reset                   dscmz
067600010201     C                   move      vlrpoi        cmzdst
067700010201     C                   eval      cmzfld = vlrfou
067800010201     C                   eval      cmzmbd = vlrhdl
067900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068000021009     C***                if        prmfir = *blanks
068100021113     C                   eval      cmzfla = 'FIVAB00F'
068200021113     C                   eval      cmzmba = 'FIVAB00F'
068300021009     C***                else
068400021009     C***                eval      cmzfla = prmfir
068500021009     C***                eval      cmzmba = prmfir
068600021009     C***                endif
068700010201     C                   eval      cmznrr = *zeros
068800010201     C                   move      §ctrokvb      cmznrr
068900021018     C                   eval      cmzlba = vlrfl1
069000010201     C                   call(e)   'TIS711C'
069100010201     C                   parm                    dscmz
069200010201     C                   parm      *blanks       esito
069300010201     C                   if        %error
069400010201     C                             or cmzerr = '1'
069500010201     C                             or esito  = '1'
069600010201     C                   eval      wrkesito = '3'
069700010201     C                   endif
069800010205     C                   endif
069900990920     C*
070000000613     C                   ENDSR
070100000613     C***
070200070411
070300070411     C     *pssr         BEGSR
070400070411     C*
070500070411     C                   if        %open(tivin00r)
070600070411     C                   close     tivin00r
070700070411     C                   endif
070800070411     C                   if        %open(fivabwwr)
070900070411     C                   close     fivabwwr
071000070411     C                   endif
071100070411     C                   if        %open(fivatwwr)
071200070411     C                   close     fivatwwr
071300070411     C                   endif
071400070411     C*
071500070411     C* Effettuo la chiamata al CLLE preposto
071600070411     C                   call(e)   'TITVVTC'
071700070411     C                   parm                    parccm
071800070411     C                   parm                    parmbr
071900070411     C                   parm      '2'           paropz
072000070411     C*
072100070411     C                   eval      wrkesito = '2'
072200070411     C*
072300070411     C                   seton                                        LR
072400070411     C*
072500070411     C                   ENDSR     '*CANCL'
072600070411     C***
072700070411
072800990910
072900000613     C     *inzsr        BEGSR
073000990910     C*
073100990910     C     *entry        plist
073200990920     C                   parm                    tivlrds
073300990921     C                   parm      wrkesito      esito
073400000724     C                   parm                    prmlit
073500000710     C                   parm                    prmfir
073600000613     C*
073700000830     C* CALCOLA LA DATA CORRENTE
073800000830     C                   time                    wn14             14 0
073900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074000000830     C                   z-add     wn8           g08dat
074100000830     C                   z-add     *zeros        g08inv
074200000830     C                   movel     '0'           g08err
074300000830     C                   call      'XSRDA8'
074400000830     C                   parm                    wlbda8
074500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074600000830     C*
074700000613     C                   ENDSR
074800000613     C***
