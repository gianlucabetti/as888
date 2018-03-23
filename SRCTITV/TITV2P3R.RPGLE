000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
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
001500070719     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600041025     D*------------------
002700041025     D* DS REPERIMENTO NUMERATORE
002800041025     D*------------------
002900041025     D trul33ds      e ds                  inz
003000041025     D*------------------
003100041025     D* DS ARCHITETTURA
003200041025     D*------------------
003300041025     D kpjba         e ds                  inz
003400041025     D*------------------
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300070719     C*
004400070719     C* Esegue lancio TISI95R solo x chiusura
004500070719     C                   CLEAR                   TISI95DS
004600070719     C                   EVAL      I95TLA = 'C'
004700070719     C                   CALL      'TISI95R'
004800070719     C                   PARM                    TISI95DS
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500000616     C*
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700021113     C                   if        not %open(fivabwwr)
006800021113     C                   open      fivabwwr
006900990914     C                   endif
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300100729     C*
008400100729     C                   z-add     1             wGiro             1 0
008500040910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900040910     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000040910     C*
010100040910     C* Eseguo routine d traduzione
010200040910     C                   exsr      impvabvat
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300100722     C*
011400100722     C* Scarico i buffer testata ancora "in canna"
011500100722     C                   WRITE     FIVAB000
011600010202     C*
011700010202     C                   endif
011800990910
011900990910     C* Se non ci sono record con errori ...
012000000710     C                   if        §ctrno = 0
012100990910     C* ... restituisco esito OK.
012200990921     C                   eval      wrkesito = '0'
012300990910     C                   else
012400010201     C                   if        §ctrokvb > 0
012500990921     C                   eval      wrkesito = '1'
012600000710     C                   else
012700000710     C                   eval      wrkesito = '2'
012800990910     C                   endif
012900000710     C                   endif
013000990910     C*
013100990914     C                   if        %open(tivin00r)
013200990908     C                   close     tivin00r
013300990914     C                   endif
013400021113     C                   if        %open(fivabwwr)
013500021113     C                   close     fivabwwr
013600990914     C                   endif
013700021113     C                   if        %open(fivatwwr)
013800021113     C                   close     fivatwwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   if        §ctrokvb > 0
014200000724     C                             and vlrpoi <> *zeros
014300010202     C                   exsr      invio
014400990920     C                   endif
014500990920     C*
014600910830     C                   ENDSR
014700000613     C***
014800990920
014900000801     C*----------------------------------------------------*
015000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015100000801     C*----------------------------------------------------*
015200010201     C     INZVAR        BEGSR
015300000801     C*
015400040802     C                   Z-ADD     *zeros        Num5_0            5 0
015500040802     C                   MOVEL     '0'           FlgCAS            1
015600000801     C*
015700000801     C                   ENDSR
015800000801     C*----------------------------------------------------*
015900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016000000801     C*----------------------------------------------------*
016100000801     C     DEFCAM        BEGSR
016200000801     C*
016300021113     C                   CLEAR                   FIVAB000
016400040802     C                   CLEAR                   FIVAT000
016500020619     C* Imposto i valori di default...
016600130506     C                   Z-ADD     2494943       VABCCM
016700130506     C                   Z-ADD     2494943       VATCCM
016800130506     C                   Z-ADD     249           VABLNP
016900130506     C                   Z-ADD     249           VATLNP
017000130506     C                   Z-ADD     000           VABCTR
017100070531     C                   MOVEL     '7Q'          VABCTM
017200040823     C                   MOVEL     '1'           VABCBO
017300020619     C* ... e poi verifico se sono stati passati come parametri
017400020619     C                   IF        vlrppt > *blanks
017500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017700020619     C                   EXSR      CHKNUM
017800020619     C                   IF        PiInt=*on
017900020619     C                   Z-ADD     PiVal         VABCCM
018000020619     C                   Z-ADD     PiVal         VATCCM
018100020619     C                   ENDIF
018200040506     C                   ENDIF
018300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018500020619     C                   EXSR      CHKNUM
018600020619     C                   IF        PiInt=*on
018700020619     C                   Z-ADD     PiVal         VABLNP
018800020619     C                   Z-ADD     PiVal         VATLNP
018900040506     C                   ENDIF
019000020619     C                   ENDIF
019100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019300020619     C                   EXSR      CHKNUM
019400020619     C                   IF        PiInt=*on
019500020619     C                   Z-ADD     PiVal         VABCTR
019600040506     C                   ENDIF
019700020619     C                   ENDIF
019800060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
019900060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020000060202     C                   ENDIF
020100020619     C                   ENDIF
020200000801     C*
020300000801     C                   ENDSR
020400000801     C*----------------------------------------------------*
020500040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020600000801     C*----------------------------------------------------*
020700040910     C     IMPVABVAT     BEGSR
020800040910     C*
020900040910     C* Traduzione relativa ai tipi record del file del cliente
021000040910     C*
021100071210     C*
021200071210     C***
021300130506     C* ...tipo record 'CNI' (testata spedizione)
021400130506     C                   IF        %subst(vindta:1:3) = 'CNI'
021500100722     C*
021600100722     C* Se nn primo giro => scarico il buffer precedente
021700100722     C                   if        wGiro = 1
021800100722     C                   eval      wGiro = 2
021900100722     C                   else
022000100722     C                   WRITE     FIVAB000
022100100722     C                   endif
022200100722     C*
022300071210     C* Resetto indicatore d anomalia sul singolo record
022400071210     C                   eval      vinflg = '1'
022500130506     C*
022600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022700071210     C                   EXSR      INZVAR
022800071210     C                   EXSR      DEFCAM
022900071210     C*
023000071210     C                   Z-ADD     *zeros        errore            1 0
023100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023200071210     C                   MOVEL     datcor        VABAAS
023300071210     C                   MOVEL     datcor        VATAAS
023400071210     C                   MOVE      datcor        VABMGS
023500071210     C                   MOVE(P)   vlrpoi        VABFGS
023600071210     C                   MOVE(P)   vlrpoi        VATFGS
023700071210     C* ......VABNSP/VATNSP
023800071210     C* NSP => Stacco un numeratore da AZNUM
023900071210     C                   clear                   TRUL33DS
024000071210     C                   eval      I33OPE = *zeros
024100071210     C                   eval      I33CNU = 302
024200071210     C                   eval      I33NUM = 1
024300071210     C                   movel     TRUL33DS      KPJBU
024400071210     C                   call      'TRUL33R'
024500071210     C                   parm                    KPJBA
024600071210     C                   movel     KPJBU         TRUL33DS
024700071210     C                   if        O33ERR = *zeros
024800071210     C                   z-add     O33NRF        VABNSP
024900071210     C                   z-add     O33NRF        VATNSP
025000071210     C                   else
025100071210     C                   Z-ADD     1             errore
025200071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025300071210     C                             + ' ' + 'VABNSP VATNSP'
025400071210     C                   endif
025500060202     C* ......VABRMN
025600130506     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
025700060202     C                   EXSR      CHKNUM
025800060202     C                   IF        PiInt=*on
025900060202     C                   Z-ADD     PiVal         VABRMN
026000060202     C                   ELSE
026100060202     C                   ADD       1             errore
026200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
026300060202     C                             + ' ' + 'VABRMN'
026400060202     C                   ENDIF
026500110207     C* ......VABNCL
026600130506     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
026700100714     C                   EXSR      CHKNUM
026800100714     C                   IF        PiInt=*on
026900110211     C                   Z-ADD     PiVal         VABNCL
027000100714     C                   ELSE
027100100714     C                   ADD       1             errore
027200100714     C                   EVAL      vinmsg = %trimr(vinmsg)
027300100714     C                             + ' ' + 'VABNCL'
027400100714     C                   ENDIF
027500060202     C* ......VABPKB
027600130506     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
027700060202     C                   EXSR      CHKNUM
027800060202     C                   IF        PiNum=*on
027900130506     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
028000070726     C                   Z-ADD(H)  PiVal         VABPKB
028100060202     C                   ELSE
028200060202     C                   ADD       1             errore
028300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028400060202     C                             + ' ' + 'VABPKB'
028500060202     C                   ENDIF
028600110425     C* ......VABVLB
028700130506     C                   IF        %subst(vindta:78:12) <> *blanks AND
028800130506     C                             %subst(vindta:78:12) <> *zeros
028900130506     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
029000110425     C                   EXSR      CHKNUM
029100110425     C                   IF        PiNum=*on
029200130506     C                   EVAL      PiVal = PiVal / 100                          gest. 2 dec.
029300110425     C                   Z-ADD(H)  PiVal         VABVLB
029400110425     C                   ELSE
029500110425     C                   ADD       1             errore
029600110425     C                   EVAL      vinmsg = %trimr(vinmsg)
029700110425     C                             + ' ' + 'VABVLB'
029800110425     C                   ENDIF
029900110425     C                   ENDIF
030000110211     C* ......VABRMA
030100130506     C                   EVAL      VABRMA=%trim(%subst(vindta:9:35))
030200130506     C* ......VABCAS
030300130506     C                   IF        %trim(%subst(vindta:146:12)) <> *blanks AND
030400130506     C                             %trim(%subst(vindta:146:12)) <> *zeros
030500130506     C                   MOVEL     '1'           FlgCAS
030600130506     C                   EVAL      VABVCA = 'EUR'
030700130506     C                   EVAL      PiStr=%trim(%subst(vindta:146:12))
030800130506     C                   EXSR      CHKNUM
030900130506     C                   IF        PiNum=*on
031000130506     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
031100130506     C                   Z-ADD     PiVal         VABCAS
031200130506     C                   ELSE
031300130506     C                   ADD       1             errore
031400130506     C                   Z-ADD     *zeros        VABCAS
031500130506     C                   EVAL      vinmsg = %trimr(vinmsg)
031600130506     C                             + ' ' + 'VABCAS'
031700130506     C                   ENDIF
031800130506     C                   ENDIF
031900130506     C* ......VABIAS
032000130506     C                   IF        %trim(%subst(vindta:158:12)) <> *blanks AND
032100130506     C                             %trim(%subst(vindta:158:12)) <> *zeros
032200130506     C                   EVAL      VABVAS = 'EUR'
032300130506     C                   EVAL      PiStr=%trim(%subst(vindta:158:12))
032400130506     C                   EXSR      CHKNUM
032500130506     C                   IF        PiNum=*on
032600130506     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032700130506     C                   Z-ADD     PiVal         VABIAS
032800130506     C                   ELSE
032900130506     C                   ADD       1             errore
033000130506     C                   Z-ADD     *zeros        VABIAS
033100130506     C                   EVAL      vinmsg = %trimr(vinmsg)
033200130506     C                             + ' ' + 'VABIAS'
033300130506     C                   ENDIF
033400130506     C                   ENDIF
033500130506     C* ......VABDCR
033600130506     C                   IF        %subst(vindta:142:3) = '2'
033700130506     C                   EVAL      VABDCR = %dec(%subst(vindta:125:8):8:0)
033800130506     C                   ENDIF
033900130506     C*
034000130506     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034100130506     C                   IF        FlgCAS <> '0'
034200130506     C                   IF        VABCBO = '1'
034300130506     C                   EVAL      VABCBO = '4'
034400130506     C                   ELSE
034500130506     C                   EVAL      VABCBO = '6'
034600130506     C                   ENDIF
034700130506     C                   ENDIF
034800130506     C*
034900130506     C                   ENDIF
035000130506     C***
035100130506     C* ...tipo record 'NA2' - 'ST' (informazioni destinatario)
035200130506     C                   IF        %subst(vindta:1:3) = 'NA2' AND
035300130506     C                             %subst(vindta:4:3) = 'ST'
035400130506     C* Resetto indicatore d anomalia sul singolo record
035500130506     C                   eval      vinflg = '1'
035600130506     C*
035700060202     C* ......VABRSD
035800130506     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
035900130506     C* ......VABRD2
036000130506     C                   EVAL      VABRSD=%trim(%subst(vindta:77:35))
036100100714     C* ......VABIND
036200130506     C                   EVAL      VABIND=%trim(%subst(vindta:112:70))
036300060202     C* ......VABLOD
036400130506     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
036500130506     C* ......VABPRD
036600130506     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
036700060202     C* ......VABCAD
036800130506     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
036900130506     C* ......VABNZD
037000130506     C                   IF        %trim(%subst(vindta:243:3)) = 'IT'
037100130506     C                   EVAL      VABNZD = *blanks
037200130506     C                   ELSE
037300130506     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
037400130506     C                   ENDIF
037500130506     C*
037600070719     C* Reperisco la provincia dal CAP e dalla località
037700130506     C                   IF        VABNZD  = *blanks AND
037800130506     C                             VABPRD  = *blanks AND
037900070719     C                             VABCAD <> *blanks
038000070719     C                   CLEAR                   TISI95DS
038100070719     C                   EVAL      I95TCN = '3'
038200070719     C                   Z-ADD     datcor        I95DAT
038300070719     C                   EVAL      I95CAP = VABCAD
038400070719     C                   EVAL      I95LOC = VABLOD
038500070719     C                   CALL      'TISI95R'
038600070719     C                   PARM                    TISI95DS
038700070719     C                   EVAL      VABPRD = O95PRV
038800070719     C                   ENDIF
038900130506     C*
039000110425     C* VATNOT "B"
039100130506     C                   EVAL      VATNOT=%trim(%subst(vindta:406:40))
039200110425     C                   EVAL      VATTRC = 'B'
039300110425     C                   IF        VATNOT <> *blanks
039400110425     C                   ADD       1             §CTROKVT
039500110425     C                   WRITE     FIVAT000
039600110425     C                   ENDIF
039700130506     C* VATNOT "A"
039800130506     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
039900130506     C                   EVAL      VATTRC = 'A'
040000130506     C                   IF        VATNOT <> *blanks
040100130506     C                   ADD       1             §CTROKVT
040200130506     C                   WRITE     FIVAT000
040300130506     C                   ENDIF
040400130506     C* VATNOT "I"
040500130506     C***                EVAL      VATNOT=%trim(%subst(vindta:466:35))
040600130506     C***                EVAL      VATTRC = 'I'
040700130506     C***                IF        VATNOT <> *blanks
040800130506     C***                ADD       1             §CTROKVT
040900130506     C***                WRITE     FIVAT000
041000130506     C***                ENDIF
041100130506     C* VATNOT "J"
041200130506     C***                EVAL      VATNOT=%trim(%subst(vindta:466+35:35))
041300130506     C***                EVAL      VATTRC = 'J'
041400130506     C***                IF        VATNOT <> *blanks
041500130506     C***                ADD       1             §CTROKVT
041600130506     C***                WRITE     FIVAT000
041700130506     C***                ENDIF
041800130506     C*
041900130506     C                   ENDIF
042000130506     C***
042100130506     C* ...tipo record 'NA2' - 'CN' (informazioni destinatario - facoltative)
042200130506     C                   IF        %subst(vindta:1:3) = 'NA2' AND
042300130506     C                             %subst(vindta:4:3) = 'CN'
042400130506     C* Resetto indicatore d anomalia sul singolo record
042500130506     C                   eval      vinflg = '1'
042600130506     C*
042700130506     C* ......VABRSD
042800130506     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
042900130506     C* ......VABRD2
043000130506     C                   EVAL      VABRSD=%trim(%subst(vindta:77:35))
043100130506     C* ......VABIND
043200130506     C                   EVAL      VABIND=%trim(%subst(vindta:112:70))
043300130506     C* ......VABLOD
043400130506     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
043500130506     C* ......VABPRD
043600130506     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
043700130506     C* ......VABCAD
043800130506     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
043900130506     C* ......VABNZD
044000130506     C                   IF        %trim(%subst(vindta:243:3)) = 'IT'
044100130506     C                   EVAL      VABNZD = *blanks
044200130506     C                   ELSE
044300130506     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
044400130506     C                   ENDIF
044500130506     C*
044600130506     C* Reperisco la provincia dal CAP e dalla località
044700130506     C                   IF        VABNZD  = *blanks AND
044800130506     C                             VABPRD  = *blanks AND
044900130506     C                             VABCAD <> *blanks
045000130506     C                   CLEAR                   TISI95DS
045100130506     C                   EVAL      I95TCN = '3'
045200130506     C                   Z-ADD     datcor        I95DAT
045300130506     C                   EVAL      I95CAP = VABCAD
045400130506     C                   EVAL      I95LOC = VABLOD
045500130506     C                   CALL      'TISI95R'
045600130506     C                   PARM                    TISI95DS
045700130506     C                   EVAL      VABPRD = O95PRV
045800130506     C                   ENDIF
045900130506     C*
046000130506     C* VATNOT "B"
046100130506     C                   EVAL      VATNOT=%trim(%subst(vindta:406:40))
046200130506     C                   EVAL      VATTRC = 'B'
046300130506     C                   IF        VATNOT <> *blanks
046400130506     C                   ADD       1             §CTROKVT
046500130506     C                   WRITE     FIVAT000
046600130506     C                   ENDIF
046700130506     C* VATNOT "A"
046800130506     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
046900130506     C                   EVAL      VATTRC = 'A'
047000130506     C                   IF        VATNOT <> *blanks
047100130506     C                   ADD       1             §CTROKVT
047200130506     C                   WRITE     FIVAT000
047300130506     C                   ENDIF
047400130506     C* VATNOT "I"
047500130506     C***                EVAL      VATNOT=%trim(%subst(vindta:466:35))
047600130506     C***                EVAL      VATTRC = 'I'
047700130506     C***                IF        VATNOT <> *blanks
047800130506     C***                ADD       1             §CTROKVT
047900130506     C***                WRITE     FIVAT000
048000130506     C***                ENDIF
048100130506     C* VATNOT "J"
048200130506     C***                EVAL      VATNOT=%trim(%subst(vindta:466+35:35))
048300130506     C***                EVAL      VATTRC = 'J'
048400130506     C***                IF        VATNOT <> *blanks
048500130506     C***                ADD       1             §CTROKVT
048600130506     C***                WRITE     FIVAT000
048700130506     C***                ENDIF
048800130506     C*
048900130506     C                   ENDIF
049000040910     C***
049100130506     C* ...tipo record 'TSR' (note spedizione)
049200130506     C                   IF        %subst(vindta:1:3) = 'TSR'
049300070719     C* Resetto indicatore d anomalia sul singolo record
049400070719     C                   eval      vinflg = '1'
049500130506     C*
049600130506     C* ......VABNOT
049700130506     C                   EVAL      VABNOT=%trim(%subst(vindta:7:35))
049800130506     C* ......VABNT2
049900130506     C                   EVAL      VABNT2=%trim(%subst(vindta:7+35:35))
050000130506     C*
050100130506     C                   ENDIF
050200130506     C***
050300130506     C* ...tipo record 'PCI' (segnacolli)
050400130506     C                   IF        %subst(vindta:1:3) = 'PCI'
050500130506     C* Resetto indicatore d anomalia sul singolo record
050600130506     C                   eval      vinflg = '1'
050700130506     C*
050800130506     C                   EVAL      VATNOT=%trim(%subst(vindta:4:35))
050900130506     C                   EVAL      VATTRC = 'E'
051000130506     C                   IF        VATNOT <> *blanks
051100130506     C                   ADD       1             §CTROKVT
051200130506     C                   WRITE     FIVAT000
051300130506     C                   ENDIF
051400100722     C*
051500100722     C                   ENDIF
051600130506     C*
051700130506     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051800130506     C                   EXSR      CHKIMPDIV
051900010202     C*
052000000801     C* Ebbene...
052100000801     C                   ADD       1             §CTRMO
052200010201     C                   IF        errore <> *zeros
052300000801     C                   ADD       1             §CTRNO
052400000801     C                   EVAL      vinflg = '2'
052500000801     C                   ELSE
052600010201     C                   ADD       1             §CTROKVB
052700000801     C                   ENDIF
052800000801     C*
052900000801     C                   ENDSR
053000010202     C*----------------------------------------------------*
053100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053200010202     C*----------------------------------------------------*
053300020305     C     PREVAT        BEGSR
053400010202     C*
053500021113     C* Compongo il nome del membro da dare al FIVATWWR
053600010202     C                   eval      parmbr = vlrhdl
053700010202     C                   movel     'M'           parmbr
053800060113     C                   eval      parccm = vlrksc
053900010202     C                   eval      paropz = '1'
054000010202     C* Effettuo la chiamata al CLLE preposto
054100040506     C                   call(e)   'TITVVTC'
054200010202     C                   parm                    parccm
054300010202     C                   parm                    parmbr
054400010202     C                   parm                    paropz
054500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054600010202     C                   if        %error
054700010202     C                   movel     '1'           chkcall
054800010202     C                   else
054900010202     C                   movel     '0'           chkcall
055000010202     C                   endif
055100010202     C*
055200010202     C                   ENDSR
055300000801     C*----------------------------------------------------*
055400000801     C*  CONTROLLO NUMERICITA' CAMPI
055500000801     C*----------------------------------------------------*
055600000801     C     CHKNUM        BEGSR
055700000801     C*
055800000801     C                   call(e)   'ISNUMERIC'
055900000801     C                   PARM                    PiStr            30
056000040714     C                   PARM      ','           PiDecChr          1
056100000801     C                   PARM      *ZEROS        PiVal            30 9
056200000801     C                   PARM      '0'           PiInt             1
056300000801     C                   PARM      '0'           PiNum             1
056400000801     C                   IF        %error
056500000801     C                   EVAL      PiInt=*off
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900000801     C***
057000000801
057100011113
057200011113     C*----------------------------------------------------*
057300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057400011113     C*----------------------------------------------------*
057500011113     C     CHKIMPDIV     BEGSR
057600011113     C*
057700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057800011113     C                   Z-ADD     *zeros        wrkDec            9 9
057900011113     C*
058000011113     C* Come prima cosa effettuo considerazioni sulla divisa
058100011113     C                   IF        vabIAS > *zeros
058200011113     C                   IF        vabVAS <> 'EUR'
058300011113     C                   EVAL      vabVAS =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   IF        vabCAS > *zeros
058800011113     C                   IF        vabVCA <> 'EUR'
058900011113     C                   EVAL      vabVCA =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   IF        vabVMD > *zeros
059400020305     C                   IF        vabVAD <> 'EUR'
059500011113     C                   EVAL      vabVAD =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060000011113     C                   Z-ADD     vabIAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAS = 'ITL'
060300011113     C                   EVAL      vabIAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060800011113     C                   Z-ADD     vabCAS        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVCA = 'ITL'
061100011113     C                   EVAL      vabCAS = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061600011113     C                   Z-ADD     vabVMD        wrkDec
061700011113     C                   IF        wrkDec > *zeros
061800011113     C                   IF        vabVAD = 'ITL'
061900011113     C                   EVAL      vabVMD = *zeros
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   ENDSR
062400011113     C***
062500011113
062600011113
062700000801
062800000801
062900990920      /TITLE Invio dei dati al punto operativo.
063000010202     C     invio         BEGSR
063100990920     C*
063200021113     C* 1° invio FIVAT
063300010201     C                   reset                   dscmz
063400010201     C                   move      vlrpoi        cmzdst
063500021113     C                   eval      cmzfld = 'FIVATWWR'
063600010201     C                   eval      cmzmbd = vlrhdl
063700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063800021009     C***                if        prmfir = *blanks
063900021113     C                   eval      cmzfla = 'FIVAT00F'
064000021113     C                   eval      cmzmba = 'FIVAT00F'
064100021009     C***                else
064200021009     C***                eval      cmzfla = prmfir
064300021009     C***                eval      cmzmba = prmfir
064400021009     C***                endif
064500010201     C                   eval      cmznrr = *zeros
064600020305     C                   move      §ctrokvt      cmznrr
064700021018     C                   eval      cmzlba = vlrfl1
064800010201     C                   call(e)   'TIS711C'
064900010201     C                   parm                    dscmz
065000010201     C                   parm      *blanks       esito
065100010205     C                   if        %error
065200010205     C                             or cmzerr = '1'
065300010205     C                             or esito  = '1'
065400010205     C                   eval      wrkesito = '3'
065500010205     C                   else
065600010201     C*
065700021113     C* 2° invio FIVAB
065800010201     C                   reset                   dscmz
065900010201     C                   move      vlrpoi        cmzdst
066000010201     C                   eval      cmzfld = vlrfou
066100010201     C                   eval      cmzmbd = vlrhdl
066200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066300021009     C***                if        prmfir = *blanks
066400021113     C                   eval      cmzfla = 'FIVAB00F'
066500021113     C                   eval      cmzmba = 'FIVAB00F'
066600021009     C***                else
066700021009     C***                eval      cmzfla = prmfir
066800021009     C***                eval      cmzmba = prmfir
066900021009     C***                endif
067000010201     C                   eval      cmznrr = *zeros
067100010201     C                   move      §ctrokvb      cmznrr
067200021018     C                   eval      cmzlba = vlrfl1
067300010201     C                   call(e)   'TIS711C'
067400010201     C                   parm                    dscmz
067500010201     C                   parm      *blanks       esito
067600010201     C                   if        %error
067700010201     C                             or cmzerr = '1'
067800010201     C                             or esito  = '1'
067900010201     C                   eval      wrkesito = '3'
068000010201     C                   endif
068100010205     C                   endif
068200990920     C*
068300000613     C                   ENDSR
068400000613     C***
068500070411
068600070411     C     *pssr         BEGSR
068700070411     C*
068800070411     C                   if        %open(tivin00r)
068900070411     C                   close     tivin00r
069000070411     C                   endif
069100070411     C                   if        %open(fivabwwr)
069200070411     C                   close     fivabwwr
069300070411     C                   endif
069400070411     C                   if        %open(fivatwwr)
069500070411     C                   close     fivatwwr
069600070411     C                   endif
069700070411     C*
069800070411     C* Effettuo la chiamata al CLLE preposto
069900070411     C                   call(e)   'TITVVTC'
070000070411     C                   parm                    parccm
070100070411     C                   parm                    parmbr
070200070411     C                   parm      '2'           paropz
070300070411     C*
070400070411     C                   eval      wrkesito = '2'
070500070411     C*
070600070411     C                   seton                                        LR
070700070411     C*
070800070411     C                   ENDSR     '*CANCL'
070900070411     C***
071000070411
071100990910
071200000613     C     *inzsr        BEGSR
071300990910     C*
071400990910     C     *entry        plist
071500990920     C                   parm                    tivlrds
071600990921     C                   parm      wrkesito      esito
071700000724     C                   parm                    prmlit
071800000710     C                   parm                    prmfir
071900000613     C*
072000000830     C* CALCOLA LA DATA CORRENTE
072100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072200100722     C                   eval      datcor = %dec(%date() : *ISO)
072300000830     C*
072400000613     C                   ENDSR
072500000613     C***
