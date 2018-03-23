000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200170927     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300990908
000400171003     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060307     D tisi95ds      e ds
001700171003     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700150225     D curSped         s              7    INZ(*blanks)
002800150225     D depSped         s              7    INZ(*blanks)
002900171003     D wNAZ            s              2    INZ(*blanks)
003000171003     D VABCCMDPD       s                   INZ(*zeros)  LIKE(VABCCM)
003100171003     D VABLNPDPD       s                   INZ(*zeros)  LIKE(VABLNP)
003200171003     D VABCTRDPD       s                   INZ(*zeros)  LIKE(VABCTR)
003300171005     D VABCTMDPD       s                   INZ(*blanks) LIKE(VABCTM)
003400171005     D wCurRec         s              7  0 INZ(*zeros)
003500171005     D oracor          s              6  0 INZ(*zeros)
003600100118
003700100118     D*------------------
003800100118     D* DS REPERIMENTO NUMERATORE
003900100118     D*------------------
004000100118     D trul33ds      e ds                  inz
004100100118     D*------------------
004200100118     D* DS ARCHITETTURA
004300100118     D*------------------
004400100118     D kpjba         e ds                  inz
004500100118
004600171003     D*------------
004700171003     D jNAZ            s              5  0 INZ(*zeros)
004800171003     D skNAZISO        S              2    DIM(1000)
004900171003     D skNAZBAR        S              3    DIM(1000)
005000010201
005100010201
005200090127     C*
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000613     C*
005700171003     C                   exsr      cartab
005800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005900000613     C*
006000010202     C* Effettuo la chiamata al CLLE preposto
006100040506     C                   call(e)   'TITVVTC'
006200010202     C                   parm                    parccm
006300010202     C                   parm                    parmbr
006400010202     C                   parm      '2'           paropz
006500050201     C*
006600050201     C* Effettuo lancio TISI95 solo x chiusura
006700050201     C                   CLEAR                   TISI95DS
006800050201     C                   EVAL      I95TLA = 'C'
006900050201     C                   CALL      'TISI95R'
007000050201     C                   PARM                    TISI95DS
007100000616     C*
007200000801     C
007300010201     C                   seton                                        LR
007400990908
007500000801
007600910830     C*--------------------------------------------------------
007700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007800910830     C*--------------------------------------------------------
007900040526     C     RWFILE        BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400021113     C                   if        not %open(fivabwwr)
008500021113     C                   open      fivabwwr
008600990914     C                   endif
008700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008800020305     C                   exsr      prevat
008900010201     C*
009000010202     C                   if        chkcall = '0'
009100010202     C*
009200021113     C                   if        not %open(fivatwwr)
009300021113     C                   open      fivatwwr
009400010201     C                   endif
009500990910     C*
009600010201     C                   clear                   §CTROKVB          5 0
009700020305     C                   clear                   §CTROKVT          5 0
009800000801     C                   clear                   §CTRMO            5 0
009900000801     C                   clear                   §CTRNO            5 0
010000990910     C*
010100921023     C                   DO        *HIVAL
010200990913     C*
010300990915     C                   READ      tivin00r                               70
010400050627     C                   if        vindta > *blanks
010500000613     C                   add       1             rrnum
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400000801     C                   eval      vinflg = '1'
011500070402     C*
011600070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
011700171109     C***                eval      curSped = %subst(vindta:46+1:5-1)
011800171109     C                   eval      curSped = %subst(vindta:46+1:5)
011900171003     C                   if        curSped <> depSped OR
012000171003     C                             %subst(vindta:615:2)<>'IT'
012100070402     C                   clear                   fivab000
012200070402     C                   clear                   fivat000
012300050628     C*
012400060315     C                   exsr      impvab                                       => carico  VAB
012500090127     C*
012600060315     C                   exsr      wrivab                                       => scarico VAB
012700070402     C*
012800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
012900140304     C                   exsr      exevata
013000070402     C*
013100070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
013200070402     C                   exsr      exevatb
013300140226     C*
013400140226     C* Indirizzo e-mail
013500140226     C                   exsr      exevati
013600140226     C*
013700140226     C* Cellulare per SMS
013800140226     C                   exsr      exevats
013900070402     C*
014000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014100070402     C                   exsr      exevate
014200070402     C*
014300070402     C* Salvo il raggruppamento spedizione cliente corrente
014400070402     C                   eval      depSped = curSped
014500070402     C*
014600070402     C                   else
014700070402     C*
014800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014900070402     C                   exsr      exevate
015000070402     C                   endif
015100000905     C*
015200000905     C                   else
015300000905     C                   eval      vinflg = '1'
015400050628     C                   endif
015500000905     C                   endif
015600000905     C*
015700000905     C  N70              update    tivin000
015800000905     C*
015900991022     C  N70              ENDdo
016000010202     C*
016100010202     C                   endif
016200990910
016300990910     C* Se non ci sono record con errori ...
016400000710     C                   if        §ctrno = 0
016500990910     C* ... restituisco esito OK.
016600990921     C                   eval      wrkesito = '0'
016700990910     C                   else
016800010201     C                   if        §ctrokvb > 0
016900990921     C                   eval      wrkesito = '1'
017000000710     C                   else
017100000710     C                   eval      wrkesito = '2'
017200990910     C                   endif
017300000710     C                   endif
017400990910     C*
017500990914     C                   if        %open(tivin00r)
017600990908     C                   close     tivin00r
017700990914     C                   endif
017800021113     C                   if        %open(fivabwwr)
017900021113     C                   close     fivabwwr
018000990914     C                   endif
018100021113     C                   if        %open(fivatwwr)
018200021113     C                   close     fivatwwr
018300010201     C                   endif
018400990910     C*
018500010201     C                   if        §ctrokvb > 0
018600000724     C                             and vlrpoi <> *zeros
018700010202     C                   exsr      invio
018800990920     C                   endif
018900990920     C*
019000910830     C                   ENDSR
019100000613     C***
019200010305
019300010305     C*----------------------------------------------------*
019400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019500010305     C*----------------------------------------------------*
019600090127     C     WRIVAB        BEGSR
019700090127     C*
019800090127     C* Quindi scarico il buffer del file d testata
019900090127     C                   write     fivab000                                     => scarico il VAB
020000010305     C*
020100010305     C                   ENDSR
020200990920
020300000801     C*----------------------------------------------------*
020400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020500000801     C*----------------------------------------------------*
020600010201     C     INZVAR        BEGSR
020700000801     C*
020800040802     C                   Z-ADD     *zeros        Num5_0            5 0
020900040802     C                   MOVEL     '0'           FlgCAS            1
021000000801     C*
021100000801     C                   ENDSR
021200000801     C*----------------------------------------------------*
021300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021400000801     C*----------------------------------------------------*
021500000801     C     DEFCAM        BEGSR
021600070718     C*
021700070718     C* Imposto i valori di default...
021800170927     C                   Z-ADD     2103003       VABCCM
021900170927     C                   Z-ADD     2103003       VATCCM
022000170927     C                   Z-ADD     210           VABLNP
022100170927     C                   Z-ADD     210           VATLNP
022200170927     C                   Z-ADD     000           VABCTR
022300150225     C                   MOVEL     '1'           VABCBO
022400150225     C                   MOVEL     'C'           VABTSP
022500140224     C                   MOVEL     '7Q'          VABCTM
022600020619     C* ... e poi verifico se sono stati passati come parametri
022700020619     C                   IF        vlrppt > *blanks
022800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABCCM
023300020619     C                   Z-ADD     PiVal         VATCCM
023400020619     C                   ENDIF
023500040506     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABLNP
024100020619     C                   Z-ADD     PiVal         VATLNP
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABCTR
024900040506     C                   ENDIF
025000020619     C                   ENDIF
025100140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025200140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025300140224     C                   ENDIF
025400171003     C                   IF        %subst(vlrppt:18:7) <> *blanks
025500171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:7))
025600171003     C                   EXSR      CHKNUM
025700171003     C                   IF        PiInt=*on
025800171003     C                   Z-ADD     PiVal         VABCCMDPD
025900171003     C                   ENDIF
026000171003     C                   ENDIF
026100171003     C                   IF        %subst(vlrppt:25:3) <> *blanks
026200171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
026300171003     C                   EXSR      CHKNUM
026400171003     C                   IF        PiInt=*on
026500171003     C                   Z-ADD     PiVal         VABLNPDPD
026600171003     C                   ENDIF
026700171003     C                   ENDIF
026800171003     C                   IF        %subst(vlrppt:28:3) <> *blanks
026900171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:28:3))
027000171003     C                   EXSR      CHKNUM
027100171003     C                   IF        PiInt=*on
027200171003     C                   Z-ADD     PiVal         VABCTRDPD
027300171003     C                   ENDIF
027400171003     C                   ENDIF
027500171005     C                   IF        %subst(vlrppt:31:2) <> *blanks
027600171005     C                   EVAL      VABCTMDPD = %subst(vlrppt:31:2)
027700171005     C                   ENDIF
027800020619     C                   ENDIF
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028300000801     C*----------------------------------------------------*
028400040823     C     IMPVAB        BEGSR
028500040823     C*
028600020305     C                   EXSR      INZVAR
028700020305     C                   EXSR      DEFCAM
028800010305     C*
028900000801     C                   Z-ADD     *zeros        errore            1 0
029000000830     C                   MOVEL     datcor        VABAAS
029100020305     C                   MOVEL     datcor        VATAAS
029200040526     C                   MOVE      datcor        VABMGS
029300040823     C                   MOVE(P)   vlrpoi        VABFGS
029400040823     C                   MOVE(P)   vlrpoi        VATFGS
029500050628     C*
029600140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
029700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029900020117     C* ==
030000140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
030100070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
030200171109     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
030300171109     C***                EVAL      VABRMA=%trim(%subst(vindta:46:5))
030400171109     C                   EVAL      VABRMA=%trim(%subst(vindta:46:6))
030500171003     C* NZD
030600171003     C                   EVAL      VABNZD=%trim(%subst(vindta:615:2))
030700171003     C                   IF        VABNZD = 'IT'
030800171003     C                   MOVEL     '1'           wNTW              1
030900171003     C                   SETOFF                                       55
031000171003     C                   CLEAR                   VABNZD
031100171003     C                   ELSE
031200171003     C                   MOVEL     '2'           wNTW
031300171003     C                   SETON                                        55
031400171005     C                   ADD       1             wCurRec
031500171005     C                   EVAL      oracor = %dec(%time() : *iso)
031600171003     C                   EVAL      VABCCM = VABCCMDPD
031700171003     C                   EVAL      VABLNP = VABLNPDPD
031800171003     C                   EVAL      VABCTR = VABCTRDPD
031900171005     C                   EVAL      VABCTM = VABCTMDPD
032000171005     C                   EVAL      VABPRD = *blanks
032100171003     C                   EVAL      wNAZ = %trim(VABNZD)
032200171003     C                   Z-ADD     1             jNAZ
032300171003     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         13
032400171003     C                   IF        %found
032500171003     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
032600171003     C                   ENDIF
032700171003     C                   ENDIF
032800050628     C* CAP
032900140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
033000010201     C                   EXSR      CHKNUM
033100010201     C                   IF        PiInt=*on
033200140224     C                   Z-ADD     PiVal         Num5_0
033300040506     C                   MOVEL(P)  Num5_0        VABCAD
033400010201     C                   ELSE
033500040506     C                   ADD       1             errore
033600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033700040506     C                             + ' ' + 'VABCAD'
033800010201     C                   ENDIF
033900040506     C* Reperisco la provincia dal CAP e dalla località
034000040526     C                   IF        VABCAD <> *blanks AND
034100040526     C                             VABPRD  = *blanks
034200040506     C                   CLEAR                   TISI95DS
034300040506     C                   EVAL      I95TCN = '3'
034400040506     C                   Z-ADD     datcor        I95DAT
034500040506     C                   EVAL      I95CAP = VABCAD
034600040506     C                   EVAL      I95LOC = VABLOD
034700050627     C                   EVAL      I95NAR = VABNZD
034800040506     C                   CALL      'TISI95R'
034900040506     C                   PARM                    TISI95DS
035000040506     C                   EVAL      VABPRD = O95PRV
035100040506     C                   ENDIF
035200040506     C* PKB
035300150225     C                   EVAL      PiStr=%trim(%subst(vindta:435:5))
035400010201     C                   EXSR      CHKNUM
035500010201     C                   IF        PiNum=*on
035600150225     C                   EVAL      PiVal=PiVal/10                               kg con 1 decimale
035700060316     C                   Z-ADD     PiVal         VABPKB
035800010201     C                   ELSE
035900010201     C                   ADD       1             errore
036000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036100010201     C                             + ' ' + 'VABPKB'
036200010201     C                   ENDIF
036300150225     C* RMN
036400171109     C***                EVAL      PiStr=%trim(%subst(vindta:46+1:5-1))
036500171109     C                   EVAL      PiStr=%trim(%subst(vindta:46+1:5))
036600090511     C                   EXSR      CHKNUM
036700090511     C                   IF        PiInt=*on
036800090511     C                   Z-ADD     PiVal         VABRMN
036900090511     C                   ELSE
037000090511     C                   ADD       1             errore
037100090511     C                   EVAL      vinmsg = %trimr(vinmsg)
037200150225     C                             + ' ' + 'VABRMN'
037300090511     C                   ENDIF
037400150225     C* NSP
037500171005     C   55              EVAL      VABNSP=oracor+wCurRec
037600171005     C   55              EVAL      VATNSP=oracor+wCurRec
037700171109     C**N55              EVAL      PiStr=wNTW+%trim(%subst(vindta:46+1:5-1))
037800171109     C  N55              EVAL      PiStr=wNTW+%trim(%subst(vindta:46+1:5))
037900171005     C  N55              EXSR      CHKNUM
038000171005     C  N55              IF        PiInt=*on
038100150225     C                   Z-ADD     PiVal         VABNSP
038200150225     C                   Z-ADD     PiVal         VATNSP
038300150225     C                   ELSE
038400150225     C                   ADD       1             errore
038500150225     C                   EVAL      vinmsg = %trimr(vinmsg)
038600150225     C                             + ' ' + 'VABNSP VATNSP'
038700150225     C                   ENDIF
038800150225     C* NCL
038900150225     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
039000150225     C                   EXSR      CHKNUM
039100150225     C                   IF        PiInt=*on
039200171003     C  N55              Z-ADD     PiVal         VABNCL
039300171003     C   55              Z-ADD     1             VABNCL
039400150225     C                   ELSE
039500150225     C                   ADD       1             errore
039600150225     C                   EVAL      vinmsg = %trimr(vinmsg)
039700150225     C                             + ' ' + 'VABNCL'
039800150225     C                   ENDIF
039900060307     C* CAS
040000170927     C                   IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
040100170927     C                             %trim(%subst(vindta:502:9))<>'000000.00' AND
040200170927     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
040300170927     C                             %trim(%subst(vindta:502:9))<>*zeros
040400170927     C                   EVAL      VABVCA = 'EUR'
040500170927     C                   EVAL      FlgCAS = '1'
040600170927     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
040700170927     C                   EXSR      CHKNUM
040800170927     C                   IF        PiNum=*on
040900170927     C                   Z-ADD     PiVal         VABCAS
041000170927     C                   ELSE
041100170927     C                   ADD       1             errore
041200170927     C                   EVAL      vinmsg = %trimr(vinmsg)
041300170927     C                             + ' ' + 'VABCAS'
041400170927     C                   ENDIF
041500170927     C                   ENDIF
041600010205     C*
041700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041800040802     C                   IF        FlgCAS <> '0'
041900070102     C                   IF        VABCBO = '1'
042000010205     C                   EVAL      VABCBO = '4'
042100010205     C                   ELSE
042200070102     C                   EVAL      VABCBO = '6'
042300070102     C                   ENDIF
042400010205     C                   ENDIF
042500020305     C*
042600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042700011113     C                   EXSR      CHKIMPDIV
042800010202     C*
042900000801     C* Ebbene...
043000000801     C                   ADD       1             §CTRMO
043100010201     C                   IF        errore <> *zeros
043200000801     C                   ADD       1             §CTRNO
043300000801     C                   EVAL      vinflg = '2'
043400000801     C                   ELSE
043500010201     C                   ADD       1             §CTROKVB
043600000801     C                   ENDIF
043700000801     C*
043800000801     C                   ENDSR
043900070102     C*----------------------------------------------------*
044000070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
044100070102     C*----------------------------------------------------*
044200070102     C     EXEVATA       BEGSR
044300070102     C*
044400070102     C                   EXSR      INZVAR
044500070102     C                   EXSR      DEFCAM
044600070102     C*
044700070102     C                   EVAL      VATTRC='A'
044800150225     C                   EVAL      VATNOT = %trim(%subst(vindta:316:25))
044900070102     C                   exsr      wrivat                                       => scarico VAT
045000070102     C*
045100070102     C                   ENDSR
045200060307     C*----------------------------------------------------*
045300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045400060307     C*----------------------------------------------------*
045500060307     C     EXEVATB       BEGSR
045600060307     C*
045700060307     C                   EXSR      INZVAR
045800060307     C                   EXSR      DEFCAM
045900060307     C*
046000060307     C                   EVAL      VATTRC='B'
046100150225     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
046200060307     C                   exsr      wrivat                                       => scarico VAT
046300060307     C*
046400060307     C                   ENDSR
046500070402     C*----------------------------------------------------*
046600070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046700070402     C*----------------------------------------------------*
046800070402     C     EXEVATE       BEGSR
046900070402     C*
047000070402     C                   EXSR      INZVAR
047100070402     C                   EXSR      DEFCAM
047200070402     C*
047300070402     C                   EVAL      VATTRC='E'
047400150225     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
047500070402     C                   exsr      wrivat                                       => scarico VAT
047600070402     C*
047700070402     C                   ENDSR
047800140224     C*----------------------------------------------------*
047900140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
048000140224     C*----------------------------------------------------*
048100140224     C     EXEVATI       BEGSR
048200170927     C*
048300170927     C                   EXSR      INZVAR
048400170927     C                   EXSR      DEFCAM
048500170927     C
048600170927     C                   EVAL      VATTRC='I'
048700170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
048800170927     C                   exsr      wrivat                                       => scarico VAT
048900170927     C
049000170927     C                   EVAL      VATTRC='J'
049100170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
049200170927     C                   exsr      wrivat                                       => scarico VAT
049300170927     C*
049400140224     C                   ENDSR
049500140226     C*----------------------------------------------------*
049600140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
049700140226     C*----------------------------------------------------*
049800140226     C     EXEVATS       BEGSR
049900150225     C***
050000150225     C***                EXSR      INZVAR
050100150225     C***                EXSR      DEFCAM
050200150225     C***
050300150225     C***                EVAL      VATTRC='S'
050400150225     C***                EVAL      VATNOT=*blank
050500150225     C***                EVAL      %subst(VATNOT:1:14) =
050600150225     C***                           %trim(%subst(vindta:713:14))
050700150225     C***                IF        VATNOT <> *blank
050800150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
050900150225     C***                ENDIF
051000150225     C***                exsr      wrivat                                       => scarico VAT
051100150225     C***
051200140226     C                   ENDSR
051300010201     C*----------------------------------------------------*
051400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051500010201     C*----------------------------------------------------*
051600020305     C     WRIVAT        BEGSR
051700171005     C*
051800171005     C   55              EVAL      VATCCM = VABCCMDPD
051900171005     C   55              EVAL      VATLNP = VABLNPDPD
052000050628     C*
052100060223     C* Scrivo solo se valorizzato qualcosa
052200060223     C                   IF        VATNOT <> *blanks
052300040802     C                   WRITE     FIVAT000
052400060223     C                   ENDIF
052500010201     C*
052600010201     C                   ENDSR
052700010202     C*----------------------------------------------------*
052800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052900010202     C*----------------------------------------------------*
053000020305     C     PREVAT        BEGSR
053100010202     C*
053200021113     C* Compongo il nome del membro da dare al FIVATWWR
053300010202     C                   eval      parmbr = vlrhdl
053400010202     C                   movel     'M'           parmbr
053500050627     C                   eval      parccm = %subst(vlrKSC:2:7)
053600010202     C                   eval      paropz = '1'
053700010202     C* Effettuo la chiamata al CLLE preposto
053800040506     C                   call(e)   'TITVVTC'
053900010202     C                   parm                    parccm
054000010202     C                   parm                    parmbr
054100010202     C                   parm                    paropz
054200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054300010202     C                   if        %error
054400010202     C                   movel     '1'           chkcall
054500010202     C                   else
054600010202     C                   movel     '0'           chkcall
054700010202     C                   endif
054800010202     C*
054900010202     C                   ENDSR
055000000801     C*----------------------------------------------------*
055100000801     C*  CONTROLLO NUMERICITA' CAMPI
055200000801     C*----------------------------------------------------*
055300000801     C     CHKNUM        BEGSR
055400000801     C*
055500000801     C                   call(e)   'ISNUMERIC'
055600000801     C                   PARM                    PiStr            30
055700140226     C                   PARM      '.'           PiDecChr          1
055800000801     C                   PARM      *ZEROS        PiVal            30 9
055900000801     C                   PARM      '0'           PiInt             1
056000000801     C                   PARM      '0'           PiNum             1
056100000801     C                   IF        %error
056200000801     C                   EVAL      PiInt=*off
056300000801     C                   ENDIF
056400000801     C*
056500000801     C                   ENDSR
056600000801     C***
056700000801
056800011113
056900011113     C*----------------------------------------------------*
057000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057100011113     C*----------------------------------------------------*
057200011113     C     CHKIMPDIV     BEGSR
057300011113     C*
057400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057500011113     C                   Z-ADD     *zeros        wrkDec            9 9
057600011113     C*
057700011113     C* Come prima cosa effettuo considerazioni sulla divisa
057800011113     C                   IF        vabIAS > *zeros
057900011113     C                   IF        vabVAS <> 'EUR'
058000011113     C                   EVAL      vabVAS =  'ITL'
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C                   IF        vabCAS > *zeros
058500011113     C                   IF        vabVCA <> 'EUR'
058600011113     C                   EVAL      vabVCA =  'ITL'
058700011113     C                   ENDIF
058800011113     C                   ENDIF
058900011113     C*
059000011113     C                   IF        vabVMD > *zeros
059100020305     C                   IF        vabVAD <> 'EUR'
059200011113     C                   EVAL      vabVAD =  'ITL'
059300011113     C                   ENDIF
059400011113     C                   ENDIF
059500011113     C*
059600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059700011113     C                   Z-ADD     vabIAS        wrkDec
059800011113     C                   IF        wrkDec > *zeros
059900011113     C                   IF        vabVAS = 'ITL'
060000011113     C                   EVAL      vabIAS = *zeros
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060500011113     C                   Z-ADD     vabCAS        wrkDec
060600011113     C                   IF        wrkDec > *zeros
060700011113     C                   IF        vabVCA = 'ITL'
060800011113     C                   EVAL      vabCAS = *zeros
060900011113     C                   ENDIF
061000011113     C                   ENDIF
061100011113     C*
061200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061300011113     C                   Z-ADD     vabVMD        wrkDec
061400011113     C                   IF        wrkDec > *zeros
061500011113     C                   IF        vabVAD = 'ITL'
061600011113     C                   EVAL      vabVMD = *zeros
061700011113     C                   ENDIF
061800011113     C                   ENDIF
061900011113     C*
062000011113     C                   ENDSR
062100011113     C***
062200011113
062300011113
062400000801
062500000801
062600990920      /TITLE Invio dei dati al punto operativo.
062700010202     C     invio         BEGSR
062800990920     C*
062900021113     C* 1° invio FIVAT
063000010201     C                   reset                   dscmz
063100010201     C                   move      vlrpoi        cmzdst
063200021113     C                   eval      cmzfld = 'FIVATWWR'
063300010201     C                   eval      cmzmbd = vlrhdl
063400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063500021009     C***                if        prmfir = *blanks
063600021113     C                   eval      cmzfla = 'FIVAT00F'
063700021113     C                   eval      cmzmba = 'FIVAT00F'
063800021009     C***                else
063900021009     C***                eval      cmzfla = prmfir
064000021009     C***                eval      cmzmba = prmfir
064100021009     C***                endif
064200010201     C                   eval      cmznrr = *zeros
064300020305     C                   move      §ctrokvt      cmznrr
064400021018     C                   eval      cmzlba = vlrfl1
064500010201     C                   call(e)   'TIS711C'
064600010201     C                   parm                    dscmz
064700010201     C                   parm      *blanks       esito
064800010205     C                   if        %error
064900010205     C                             or cmzerr = '1'
065000010205     C                             or esito  = '1'
065100010205     C                   eval      wrkesito = '3'
065200010205     C                   else
065300010201     C*
065400021113     C* 2° invio FIVAB
065500010201     C                   reset                   dscmz
065600010201     C                   move      vlrpoi        cmzdst
065700010201     C                   eval      cmzfld = vlrfou
065800010201     C                   eval      cmzmbd = vlrhdl
065900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000021009     C***                if        prmfir = *blanks
066100021113     C                   eval      cmzfla = 'FIVAB00F'
066200021113     C                   eval      cmzmba = 'FIVAB00F'
066300021009     C***                else
066400021009     C***                eval      cmzfla = prmfir
066500021009     C***                eval      cmzmba = prmfir
066600021009     C***                endif
066700010201     C                   eval      cmznrr = *zeros
066800010201     C                   move      §ctrokvb      cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000010201     C                   call(e)   'TIS711C'
067100010201     C                   parm                    dscmz
067200010201     C                   parm      *blanks       esito
067300010201     C                   if        %error
067400010201     C                             or cmzerr = '1'
067500010201     C                             or esito  = '1'
067600010201     C                   eval      wrkesito = '3'
067700010201     C                   endif
067800010205     C                   endif
067900990920     C*
068000000613     C                   ENDSR
068100000613     C***
068200171003
068300171003
068400171003
068500171003     C*--------------------------------------------------------
068600171003     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
068700171003     C*--------------------------------------------------------
068800171003     C     CARTAB        BEGSR
068900171003     C*
069000171003     C* TABELLA '15' - NAZIONI
069100171003     C                   clear                   skNAZISO
069200171003     C                   clear                   skNAZBAR
069300171003     C                   eval      tblKUT = 1
069400171003     C                   eval      tblCOD = '15'
069500171003     C     KEYtabP       setll     tabel00f
069600171003     C     KEYtabP       reade     tabel00f
069700171003     C                   dow       not %eof(tabel00f)
069800171003     C                   if        tblFLG = *blanks
069900171003     C                   movel(p)  tblUNI        ds15
070000171003     C                   add       1             jNAZ
070100171003     C                   eval      skNAZBAR(jNAZ) = tblKEY
070200171005     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
070300171003     C                   endif
070400171003     C     KEYtabP       reade     tabel00f
070500171003     C                   enddo
070600171003     C*
070700171003     C                   ENDSR
070800171003     C***
070900171003
071000171003
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
072100100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072200100118     C                   eval      datcor = %dec(%date() : *ISO)
072300171003     C*
072400171003     C* Chiave su TABEL00F - parziale
072500171003     C     KEYtabP       KLIST
072600171003     C                   KFLD                    tblKUT
072700171003     C                   KFLD                    tblCOD
072800000830     C*
072900000613     C                   ENDSR
073000000613     C***
