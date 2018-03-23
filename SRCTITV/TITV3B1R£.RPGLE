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
011700170928     C                   eval      curSped = %subst(vindta:46+1:5-1)
011800171003     C                   if        curSped <> depSped OR
011900171003     C                             %subst(vindta:615:2)<>'IT'
012000070402     C                   clear                   fivab000
012100070402     C                   clear                   fivat000
012200050628     C*
012300060315     C                   exsr      impvab                                       => carico  VAB
012400090127     C*
012500060315     C                   exsr      wrivab                                       => scarico VAB
012600070402     C*
012700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
012800140304     C                   exsr      exevata
012900070402     C*
013000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
013100070402     C                   exsr      exevatb
013200140226     C*
013300140226     C* Indirizzo e-mail
013400140226     C                   exsr      exevati
013500140226     C*
013600140226     C* Cellulare per SMS
013700140226     C                   exsr      exevats
013800070402     C*
013900070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014000070402     C                   exsr      exevate
014100070402     C*
014200070402     C* Salvo il raggruppamento spedizione cliente corrente
014300070402     C                   eval      depSped = curSped
014400070402     C*
014500070402     C                   else
014600070402     C*
014700070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
014800070402     C                   exsr      exevate
014900070402     C                   endif
015000000905     C*
015100000905     C                   else
015200000905     C                   eval      vinflg = '1'
015300050628     C                   endif
015400000905     C                   endif
015500000905     C*
015600000905     C  N70              update    tivin000
015700000905     C*
015800991022     C  N70              ENDdo
015900010202     C*
016000010202     C                   endif
016100990910
016200990910     C* Se non ci sono record con errori ...
016300000710     C                   if        §ctrno = 0
016400990910     C* ... restituisco esito OK.
016500990921     C                   eval      wrkesito = '0'
016600990910     C                   else
016700010201     C                   if        §ctrokvb > 0
016800990921     C                   eval      wrkesito = '1'
016900000710     C                   else
017000000710     C                   eval      wrkesito = '2'
017100990910     C                   endif
017200000710     C                   endif
017300990910     C*
017400990914     C                   if        %open(tivin00r)
017500990908     C                   close     tivin00r
017600990914     C                   endif
017700021113     C                   if        %open(fivabwwr)
017800021113     C                   close     fivabwwr
017900990914     C                   endif
018000021113     C                   if        %open(fivatwwr)
018100021113     C                   close     fivatwwr
018200010201     C                   endif
018300990910     C*
018400010201     C                   if        §ctrokvb > 0
018500000724     C                             and vlrpoi <> *zeros
018600010202     C                   exsr      invio
018700990920     C                   endif
018800990920     C*
018900910830     C                   ENDSR
019000000613     C***
019100010305
019200010305     C*----------------------------------------------------*
019300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
019400010305     C*----------------------------------------------------*
019500090127     C     WRIVAB        BEGSR
019600090127     C*
019700090127     C* Quindi scarico il buffer del file d testata
019800090127     C                   write     fivab000                                     => scarico il VAB
019900010305     C*
020000010305     C                   ENDSR
020100990920
020200000801     C*----------------------------------------------------*
020300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020400000801     C*----------------------------------------------------*
020500010201     C     INZVAR        BEGSR
020600000801     C*
020700040802     C                   Z-ADD     *zeros        Num5_0            5 0
020800040802     C                   MOVEL     '0'           FlgCAS            1
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500070718     C*
021600070718     C* Imposto i valori di default...
021700170927     C                   Z-ADD     2103003       VABCCM
021800170927     C                   Z-ADD     2103003       VATCCM
021900170927     C                   Z-ADD     210           VABLNP
022000170927     C                   Z-ADD     210           VATLNP
022100170927     C                   Z-ADD     000           VABCTR
022200150225     C                   MOVEL     '1'           VABCBO
022300150225     C                   MOVEL     'C'           VABTSP
022400140224     C                   MOVEL     '7Q'          VABCTM
022500020619     C* ... e poi verifico se sono stati passati come parametri
022600020619     C                   IF        vlrppt > *blanks
022700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABCCM
023200020619     C                   Z-ADD     PiVal         VATCCM
023300020619     C                   ENDIF
023400040506     C                   ENDIF
023500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023700020619     C                   EXSR      CHKNUM
023800020619     C                   IF        PiInt=*on
023900020619     C                   Z-ADD     PiVal         VABLNP
024000020619     C                   Z-ADD     PiVal         VATLNP
024100040506     C                   ENDIF
024200020619     C                   ENDIF
024300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024500020619     C                   EXSR      CHKNUM
024600020619     C                   IF        PiInt=*on
024700020619     C                   Z-ADD     PiVal         VABCTR
024800040506     C                   ENDIF
024900020619     C                   ENDIF
025000140224     C                   IF        %subst(vlrppt:16:2) <> *blanks
025100140224     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025200140224     C                   ENDIF
025300171003     C                   IF        %subst(vlrppt:18:7) <> *blanks
025400171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:7))
025500171003     C                   EXSR      CHKNUM
025600171003     C                   IF        PiInt=*on
025700171003     C                   Z-ADD     PiVal         VABCCMDPD
025800171003     C                   ENDIF
025900171003     C                   ENDIF
026000171003     C                   IF        %subst(vlrppt:25:3) <> *blanks
026100171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
026200171003     C                   EXSR      CHKNUM
026300171003     C                   IF        PiInt=*on
026400171003     C                   Z-ADD     PiVal         VABLNPDPD
026500171003     C                   ENDIF
026600171003     C                   ENDIF
026700171003     C                   IF        %subst(vlrppt:28:3) <> *blanks
026800171003     C                   EVAL      PiStr=%trim(%subst(vlrppt:28:3))
026900171003     C                   EXSR      CHKNUM
027000171003     C                   IF        PiInt=*on
027100171003     C                   Z-ADD     PiVal         VABCTRDPD
027200171003     C                   ENDIF
027300171003     C                   ENDIF
027400171005     C                   IF        %subst(vlrppt:31:2) <> *blanks
027500171005     C                   EVAL      VABCTMDPD = %subst(vlrppt:31:2)
027600171005     C                   ENDIF
027700020619     C                   ENDIF
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028200000801     C*----------------------------------------------------*
028300040823     C     IMPVAB        BEGSR
028400040823     C*
028500020305     C                   EXSR      INZVAR
028600020305     C                   EXSR      DEFCAM
028700010305     C*
028800000801     C                   Z-ADD     *zeros        errore            1 0
028900000830     C                   MOVEL     datcor        VABAAS
029000020305     C                   MOVEL     datcor        VATAAS
029100040526     C                   MOVE      datcor        VABMGS
029200040823     C                   MOVE(P)   vlrpoi        VABFGS
029300040823     C                   MOVE(P)   vlrpoi        VATFGS
029400050628     C*
029500140221     C                   EVAL      VABRSD=%trim(%subst(vindta:276:35))
029600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029800020117     C* ==
029900140221     C                   EVAL      VABIND=%trim(%subst(vindta:341:35))
030000070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
030100070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
030200170928     C                   EVAL      VABRMA=%trim(%subst(vindta:46:5))
030300171003     C* NZD
030400171003     C                   EVAL      VABNZD=%trim(%subst(vindta:615:2))
030500171003     C                   IF        VABNZD = 'IT'
030600171003     C                   MOVEL     '1'           wNTW              1
030700171003     C                   SETOFF                                       55
030800171003     C                   CLEAR                   VABNZD
030900171003     C                   ELSE
031000171003     C                   MOVEL     '2'           wNTW
031100171003     C                   SETON                                        55
031200171005     C                   ADD       1             wCurRec
031300171005     C                   EVAL      oracor = %dec(%time() : *iso)
031400171003     C                   EVAL      VABCCM = VABCCMDPD
031500171003     C                   EVAL      VABLNP = VABLNPDPD
031600171003     C                   EVAL      VABCTR = VABCTRDPD
031700171005     C                   EVAL      VABCTM = VABCTMDPD
031800171005     C                   EVAL      VABPRD = *blanks
031900171003     C                   EVAL      wNAZ = %trim(VABNZD)
032000171003     C                   Z-ADD     1             jNAZ
032100171003     C     wNAZ          LOOKUP    skNAZISO(jNAZ)                         13
032200171003     C                   IF        %found
032300171003     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
032400171003     C                   ENDIF
032500171003     C                   ENDIF
032600050628     C* CAP
032700140224     C                   EVAL      PiStr=%trim(%subst(vindta:391:9))
032800010201     C                   EXSR      CHKNUM
032900010201     C                   IF        PiInt=*on
033000140224     C                   Z-ADD     PiVal         Num5_0
033100040506     C                   MOVEL(P)  Num5_0        VABCAD
033200010201     C                   ELSE
033300040506     C                   ADD       1             errore
033400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033500040506     C                             + ' ' + 'VABCAD'
033600010201     C                   ENDIF
033700040506     C* Reperisco la provincia dal CAP e dalla località
033800040526     C                   IF        VABCAD <> *blanks AND
033900040526     C                             VABPRD  = *blanks
034000040506     C                   CLEAR                   TISI95DS
034100040506     C                   EVAL      I95TCN = '3'
034200040506     C                   Z-ADD     datcor        I95DAT
034300040506     C                   EVAL      I95CAP = VABCAD
034400040506     C                   EVAL      I95LOC = VABLOD
034500050627     C                   EVAL      I95NAR = VABNZD
034600040506     C                   CALL      'TISI95R'
034700040506     C                   PARM                    TISI95DS
034800040506     C                   EVAL      VABPRD = O95PRV
034900040506     C                   ENDIF
035000040506     C* PKB
035100150225     C                   EVAL      PiStr=%trim(%subst(vindta:435:5))
035200010201     C                   EXSR      CHKNUM
035300010201     C                   IF        PiNum=*on
035400150225     C                   EVAL      PiVal=PiVal/10                               kg con 1 decimale
035500060316     C                   Z-ADD     PiVal         VABPKB
035600010201     C                   ELSE
035700010201     C                   ADD       1             errore
035800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035900010201     C                             + ' ' + 'VABPKB'
036000010201     C                   ENDIF
036100150225     C* RMN
036200170928     C                   EVAL      PiStr=%trim(%subst(vindta:46+1:5-1))
036300090511     C                   EXSR      CHKNUM
036400090511     C                   IF        PiInt=*on
036500090511     C                   Z-ADD     PiVal         VABRMN
036600090511     C                   ELSE
036700090511     C                   ADD       1             errore
036800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
036900150225     C                             + ' ' + 'VABRMN'
037000090511     C                   ENDIF
037100150225     C* NSP
037200171005     C   55              EVAL      VABNSP=oracor+wCurRec
037300171005     C   55              EVAL      VATNSP=oracor+wCurRec
037400171005     C  N55              EVAL      PiStr=wNTW+%trim(%subst(vindta:46+1:5-1))
037500171005     C  N55              EXSR      CHKNUM
037600171005     C  N55              IF        PiInt=*on
037700150225     C                   Z-ADD     PiVal         VABNSP
037800150225     C                   Z-ADD     PiVal         VATNSP
037900150225     C                   ELSE
038000150225     C                   ADD       1             errore
038100150225     C                   EVAL      vinmsg = %trimr(vinmsg)
038200150225     C                             + ' ' + 'VABNSP VATNSP'
038300150225     C                   ENDIF
038400150225     C* NCL
038500150225     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
038600150225     C                   EXSR      CHKNUM
038700150225     C                   IF        PiInt=*on
038800171003     C  N55              Z-ADD     PiVal         VABNCL
038900171003     C   55              Z-ADD     1             VABNCL
039000150225     C                   ELSE
039100150225     C                   ADD       1             errore
039200150225     C                   EVAL      vinmsg = %trimr(vinmsg)
039300150225     C                             + ' ' + 'VABNCL'
039400150225     C                   ENDIF
039500060307     C* CAS
039600170927     C                   IF        %trim(%subst(vindta:502:9))<>'000000,00' AND
039700170927     C                             %trim(%subst(vindta:502:9))<>'000000.00' AND
039800170927     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
039900170927     C                             %trim(%subst(vindta:502:9))<>*zeros
040000170927     C                   EVAL      VABVCA = 'EUR'
040100170927     C                   EVAL      FlgCAS = '1'
040200170927     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
040300170927     C                   EXSR      CHKNUM
040400170927     C                   IF        PiNum=*on
040500170927     C                   Z-ADD     PiVal         VABCAS
040600170927     C                   ELSE
040700170927     C                   ADD       1             errore
040800170927     C                   EVAL      vinmsg = %trimr(vinmsg)
040900170927     C                             + ' ' + 'VABCAS'
041000170927     C                   ENDIF
041100170927     C                   ENDIF
041200010205     C*
041300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041400040802     C                   IF        FlgCAS <> '0'
041500070102     C                   IF        VABCBO = '1'
041600010205     C                   EVAL      VABCBO = '4'
041700010205     C                   ELSE
041800070102     C                   EVAL      VABCBO = '6'
041900070102     C                   ENDIF
042000010205     C                   ENDIF
042100020305     C*
042200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300011113     C                   EXSR      CHKIMPDIV
042400010202     C*
042500000801     C* Ebbene...
042600000801     C                   ADD       1             §CTRMO
042700010201     C                   IF        errore <> *zeros
042800000801     C                   ADD       1             §CTRNO
042900000801     C                   EVAL      vinflg = '2'
043000000801     C                   ELSE
043100010201     C                   ADD       1             §CTROKVB
043200000801     C                   ENDIF
043300000801     C*
043400000801     C                   ENDSR
043500070102     C*----------------------------------------------------*
043600070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
043700070102     C*----------------------------------------------------*
043800070102     C     EXEVATA       BEGSR
043900070102     C*
044000070102     C                   EXSR      INZVAR
044100070102     C                   EXSR      DEFCAM
044200070102     C*
044300070102     C                   EVAL      VATTRC='A'
044400150225     C                   EVAL      VATNOT = %trim(%subst(vindta:316:25))
044500070102     C                   exsr      wrivat                                       => scarico VAT
044600070102     C*
044700070102     C                   ENDSR
044800060307     C*----------------------------------------------------*
044900060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
045000060307     C*----------------------------------------------------*
045100060307     C     EXEVATB       BEGSR
045200060307     C*
045300060307     C                   EXSR      INZVAR
045400060307     C                   EXSR      DEFCAM
045500060307     C*
045600060307     C                   EVAL      VATTRC='B'
045700150225     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
045800060307     C                   exsr      wrivat                                       => scarico VAT
045900060307     C*
046000060307     C                   ENDSR
046100070402     C*----------------------------------------------------*
046200070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
046300070402     C*----------------------------------------------------*
046400070402     C     EXEVATE       BEGSR
046500070402     C*
046600070402     C                   EXSR      INZVAR
046700070402     C                   EXSR      DEFCAM
046800070402     C*
046900070402     C                   EVAL      VATTRC='E'
047000150225     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
047100070402     C                   exsr      wrivat                                       => scarico VAT
047200070402     C*
047300070402     C                   ENDSR
047400140224     C*----------------------------------------------------*
047500140224     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I" e "J"
047600140224     C*----------------------------------------------------*
047700140224     C     EXEVATI       BEGSR
047800170927     C*
047900170927     C                   EXSR      INZVAR
048000170927     C                   EXSR      DEFCAM
048100170927     C
048200170927     C                   EVAL      VATTRC='I'
048300170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
048400170927     C                   exsr      wrivat                                       => scarico VAT
048500170927     C
048600170927     C                   EVAL      VATTRC='J'
048700170927     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:15))
048800170927     C                   exsr      wrivat                                       => scarico VAT
048900170927     C*
049000140224     C                   ENDSR
049100140226     C*----------------------------------------------------*
049200140226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
049300140226     C*----------------------------------------------------*
049400140226     C     EXEVATS       BEGSR
049500150225     C***
049600150225     C***                EXSR      INZVAR
049700150225     C***                EXSR      DEFCAM
049800150225     C***
049900150225     C***                EVAL      VATTRC='S'
050000150225     C***                EVAL      VATNOT=*blank
050100150225     C***                EVAL      %subst(VATNOT:1:14) =
050200150225     C***                           %trim(%subst(vindta:713:14))
050300150225     C***                IF        VATNOT <> *blank
050400150225     C***                EVAL      %subst(VATNOT:17:1) = 'S'
050500150225     C***                ENDIF
050600150225     C***                exsr      wrivat                                       => scarico VAT
050700150225     C***
050800140226     C                   ENDSR
050900010201     C*----------------------------------------------------*
051000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051100010201     C*----------------------------------------------------*
051200020305     C     WRIVAT        BEGSR
051300171005     C*
051400171005     C   55              EVAL      VATCCM = VABCCMDPD
051500171005     C   55              EVAL      VATLNP = VABLNPDPD
051600050628     C*
051700060223     C* Scrivo solo se valorizzato qualcosa
051800060223     C                   IF        VATNOT <> *blanks
051900040802     C                   WRITE     FIVAT000
052000060223     C                   ENDIF
052100010201     C*
052200010201     C                   ENDSR
052300010202     C*----------------------------------------------------*
052400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052500010202     C*----------------------------------------------------*
052600020305     C     PREVAT        BEGSR
052700010202     C*
052800021113     C* Compongo il nome del membro da dare al FIVATWWR
052900010202     C                   eval      parmbr = vlrhdl
053000010202     C                   movel     'M'           parmbr
053100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
053200010202     C                   eval      paropz = '1'
053300010202     C* Effettuo la chiamata al CLLE preposto
053400040506     C                   call(e)   'TITVVTC'
053500010202     C                   parm                    parccm
053600010202     C                   parm                    parmbr
053700010202     C                   parm                    paropz
053800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053900010202     C                   if        %error
054000010202     C                   movel     '1'           chkcall
054100010202     C                   else
054200010202     C                   movel     '0'           chkcall
054300010202     C                   endif
054400010202     C*
054500010202     C                   ENDSR
054600000801     C*----------------------------------------------------*
054700000801     C*  CONTROLLO NUMERICITA' CAMPI
054800000801     C*----------------------------------------------------*
054900000801     C     CHKNUM        BEGSR
055000000801     C*
055100000801     C                   call(e)   'ISNUMERIC'
055200000801     C                   PARM                    PiStr            30
055300140226     C                   PARM      '.'           PiDecChr          1
055400000801     C                   PARM      *ZEROS        PiVal            30 9
055500000801     C                   PARM      '0'           PiInt             1
055600000801     C                   PARM      '0'           PiNum             1
055700000801     C                   IF        %error
055800000801     C                   EVAL      PiInt=*off
055900000801     C                   ENDIF
056000000801     C*
056100000801     C                   ENDSR
056200000801     C***
056300000801
056400011113
056500011113     C*----------------------------------------------------*
056600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056700011113     C*----------------------------------------------------*
056800011113     C     CHKIMPDIV     BEGSR
056900011113     C*
057000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057100011113     C                   Z-ADD     *zeros        wrkDec            9 9
057200011113     C*
057300011113     C* Come prima cosa effettuo considerazioni sulla divisa
057400011113     C                   IF        vabIAS > *zeros
057500011113     C                   IF        vabVAS <> 'EUR'
057600011113     C                   EVAL      vabVAS =  'ITL'
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C                   IF        vabCAS > *zeros
058100011113     C                   IF        vabVCA <> 'EUR'
058200011113     C                   EVAL      vabVCA =  'ITL'
058300011113     C                   ENDIF
058400011113     C                   ENDIF
058500011113     C*
058600011113     C                   IF        vabVMD > *zeros
058700020305     C                   IF        vabVAD <> 'EUR'
058800011113     C                   EVAL      vabVAD =  'ITL'
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059300011113     C                   Z-ADD     vabIAS        wrkDec
059400011113     C                   IF        wrkDec > *zeros
059500011113     C                   IF        vabVAS = 'ITL'
059600011113     C                   EVAL      vabIAS = *zeros
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060100011113     C                   Z-ADD     vabCAS        wrkDec
060200011113     C                   IF        wrkDec > *zeros
060300011113     C                   IF        vabVCA = 'ITL'
060400011113     C                   EVAL      vabCAS = *zeros
060500011113     C                   ENDIF
060600011113     C                   ENDIF
060700011113     C*
060800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060900011113     C                   Z-ADD     vabVMD        wrkDec
061000011113     C                   IF        wrkDec > *zeros
061100011113     C                   IF        vabVAD = 'ITL'
061200011113     C                   EVAL      vabVMD = *zeros
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C                   ENDSR
061700011113     C***
061800011113
061900011113
062000000801
062100000801
062200990920      /TITLE Invio dei dati al punto operativo.
062300010202     C     invio         BEGSR
062400990920     C*
062500021113     C* 1° invio FIVAT
062600010201     C                   reset                   dscmz
062700010201     C                   move      vlrpoi        cmzdst
062800021113     C                   eval      cmzfld = 'FIVATWWR'
062900010201     C                   eval      cmzmbd = vlrhdl
063000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100021009     C***                if        prmfir = *blanks
063200021113     C                   eval      cmzfla = 'FIVAT00F'
063300021113     C                   eval      cmzmba = 'FIVAT00F'
063400021009     C***                else
063500021009     C***                eval      cmzfla = prmfir
063600021009     C***                eval      cmzmba = prmfir
063700021009     C***                endif
063800010201     C                   eval      cmznrr = *zeros
063900020305     C                   move      §ctrokvt      cmznrr
064000021018     C                   eval      cmzlba = vlrfl1
064100010201     C                   call(e)   'TIS711C'
064200010201     C                   parm                    dscmz
064300010201     C                   parm      *blanks       esito
064400010205     C                   if        %error
064500010205     C                             or cmzerr = '1'
064600010205     C                             or esito  = '1'
064700010205     C                   eval      wrkesito = '3'
064800010205     C                   else
064900010201     C*
065000021113     C* 2° invio FIVAB
065100010201     C                   reset                   dscmz
065200010201     C                   move      vlrpoi        cmzdst
065300010201     C                   eval      cmzfld = vlrfou
065400010201     C                   eval      cmzmbd = vlrhdl
065500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600021009     C***                if        prmfir = *blanks
065700021113     C                   eval      cmzfla = 'FIVAB00F'
065800021113     C                   eval      cmzmba = 'FIVAB00F'
065900021009     C***                else
066000021009     C***                eval      cmzfla = prmfir
066100021009     C***                eval      cmzmba = prmfir
066200021009     C***                endif
066300010201     C                   eval      cmznrr = *zeros
066400010201     C                   move      §ctrokvb      cmznrr
066500021018     C                   eval      cmzlba = vlrfl1
066600010201     C                   call(e)   'TIS711C'
066700010201     C                   parm                    dscmz
066800010201     C                   parm      *blanks       esito
066900010201     C                   if        %error
067000010201     C                             or cmzerr = '1'
067100010201     C                             or esito  = '1'
067200010201     C                   eval      wrkesito = '3'
067300010201     C                   endif
067400010205     C                   endif
067500990920     C*
067600000613     C                   ENDSR
067700000613     C***
067800171003
067900171003
068000171003
068100171003     C*--------------------------------------------------------
068200171003     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
068300171003     C*--------------------------------------------------------
068400171003     C     CARTAB        BEGSR
068500171003     C*
068600171003     C* TABELLA '15' - NAZIONI
068700171003     C                   clear                   skNAZISO
068800171003     C                   clear                   skNAZBAR
068900171003     C                   eval      tblKUT = 1
069000171003     C                   eval      tblCOD = '15'
069100171003     C     KEYtabP       setll     tabel00f
069200171003     C     KEYtabP       reade     tabel00f
069300171003     C                   dow       not %eof(tabel00f)
069400171003     C                   if        tblFLG = *blanks
069500171003     C                   movel(p)  tblUNI        ds15
069600171003     C                   add       1             jNAZ
069700171003     C                   eval      skNAZBAR(jNAZ) = tblKEY
069800171005     C                   eval      skNAZISO(jNAZ) = %trim(§15COD)
069900171003     C                   endif
070000171003     C     KEYtabP       reade     tabel00f
070100171003     C                   enddo
070200171003     C*
070300171003     C                   ENDSR
070400171003     C***
070500171003
070600171003
070700990910
070800000613     C     *inzsr        BEGSR
070900990910     C*
071000990910     C     *entry        plist
071100990920     C                   parm                    tivlrds
071200990921     C                   parm      wrkesito      esito
071300000724     C                   parm                    prmlit
071400000710     C                   parm                    prmfir
071500000613     C*
071600000830     C* CALCOLA LA DATA CORRENTE
071700100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071800100118     C                   eval      datcor = %dec(%date() : *ISO)
071900171003     C*
072000171003     C* Chiave su TABEL00F - parziale
072100171003     C     KEYtabP       KLIST
072200171003     C                   KFLD                    tblKUT
072300171003     C                   KFLD                    tblCOD
072400000830     C*
072500000613     C                   ENDSR
072600000613     C***
